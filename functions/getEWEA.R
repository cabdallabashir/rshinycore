# update_ewea("abdullahiaweis","tutka_sulsulaaye@123")
update_ewea <- function(user , pass , date_to_update){
  
  source("db/dbCreation.R")
  
  source("functions/value_to_label.R")
  source("functions/label_to_status.R")
  source("functions/label_to_value.R")
  

  
  
  ##### Retrieving submitted EWEA submitted data from ona via API and cleaning----
  get_data_ewea <- function(username, password ){
    
    # Make the API request
    response_ewea_data <- GET(url = "https://api.ona.io/api/v1/data/453775", authenticate(username, password))
    status <- status_code(response_ewea_data)
    if (status == 200) {
      ewea <- prettify(rawToChar(response_ewea_data$content))%>% fromJSON()%>%
        purrr::discard(is.list) %>%
        as.data.frame()
      
      names(ewea) <- gsub(".*/", "", names(ewea)) 
      
      ewea%<>%
        dplyr::mutate( ## format end time to date format. Reporting_date column has some inconsistencies, so don't use
          Reporting_date =  ymd(System_date),
          ## if the data was reported before 20th,it means the submission was for the past month as members start reporting on 20th of each month
          reporting_month = ifelse(day(Reporting_date)<20,
                                   format(Reporting_date %m-% months(1), "%Y-%m"),
                                   format(Reporting_date, "%Y-%m")),
          ## In some districts, multiple members work in, here we add the member suffix for distinction
          District = ifelse(District_Name == "Afgoye" | District_Name =="Galkaayo" | District_Name =="Dhusamareb",
                            paste0(District_Name," ",Member_name),
                            District_Name))%<>%
        # select columns for only data needed for analysis and visualization in dashboard
        select(reporting_month,Member_name,District,Rainfall_category,Catchment_status,water_point,Water_source_dried,
               River,flashfloods,Flashfloods_aff_hhs,crop_disease_occ,crop_disease_type,crop_affected_percent,
               Pasture_longevity,Cattle,Goat_sheep,Outbreak_deaths,Price_water,Price_maize,Price_sorghum,Price_rice,
               Goat_price,Labour_price,displaced_hh,HH_arrived,HH_migrated,coping,spent_days,locust_infest,crop_impact)%<>%
        # the data collection was fully harmonized from November 2019, any data before then should not be included in the dashboard
        filter(reporting_month>="2019-11")%<>%
        as.data.table()
      
      return(
        data.frame(
          status= 200,
          message = "success",
          ewea
        )
      )
    } else if (status == 401) {
      # Unauthorized - likely incorrect username/password
      return(
        data.frame(
          status= 401,
          message = "incorrect username/password",
          data = ""
        )
      )
    } else if (status >= 500) {
      # Server error
      warning("Server error: Try again later.")
      return(
        data.frame(
          status= 500,
          message = "Server error: Try again later.",
          data = ""
        )
      )
    } else {
      # Other errors
      warning()
      return(
        data.frame(
          status= status,
          message = paste("Error encountered. Status code:", status),
          data = ""
        )
      )
    }
    ## covert to dataframe                   
    
    
  }
  
  
  ewea_raw <- get_data_ewea(user, pass)
  
  if(ewea_raw$status[1] != "200"){
    return(
      ewea_raw$message[1]
    )
  }
  
  market_price <- ewea_raw %>%
    select(reporting_month,District,Price_water,Price_maize,Price_sorghum,Price_rice,Goat_price,Labour_price) %>%
    mutate_all(~replace(., is.na(.) | . == 'NA' | . == '', 0)) %>%
    mutate(reporting_month = as.yearmon(reporting_month))%>%
    summarise(
      Price_sorghum = round(mean(as.numeric(Price_sorghum),na.rm = T),2),
      Goat_price = round(mean(as.numeric(Goat_price),na.rm = T),2),
      Price_water = round(mean(as.numeric(Price_water),na.rm = T),2),
      Price_maize = round(mean(as.numeric(Price_maize),na.rm = T),2),
      Price_rice = round(mean(as.numeric(Price_rice),na.rm = T),2),
      Labour_price = round(mean(as.numeric(Labour_price),na.rm = T),2),
      .by =  c("reporting_month","District")
    ) %>% 
    rename(`Sorghum Prices`= Price_sorghum,
           `Goat prices`=  "Goat_price" ,
           `Water prices (20 litre jerrycan)` = Price_water,
           `Maize Prices` = Price_maize,
           `Rice prices` = Price_rice,
           `Labour prices` = Labour_price)%>%
    pivot_longer(
      cols = `Sorghum Prices` : `Labour prices` , 
      names_to = "variable",values_to = "value"
      
    ) %>%
    left_join(market_thresholds , by = c(c("District" = "Community"),"variable"),
              relationship = "many-to-many") %>%
    mutate(
      var_status = ifelse(
        variable == 'Labour prices',
        ifelse(
          status == 'Alarm' | status == 'Alert',
          value < From & value >= `Up to`,
          value > `Up to`
        )
        ,
        ifelse(
          status == 'Normal' | status == 'Alert',
          value > From & value <= `Up to`,
          value > From
        )
        
      ),
      status = ifelse(is.na(status), "Normal", status),
      From  = ifelse(is.na(From), 0, From),
      `Up to` = ifelse(is.na(`Up to`), 0, `Up to`),
      var_status =  ifelse(is.na(var_status), TRUE, var_status),
      indicator = "Market",
      other = "",
      number = ""
    ) %>%
    filter(var_status == TRUE | is.na(var_status)) %>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  
  
  #---------- rainfall
  
  rainfall <- ewea_raw%>%
    select(reporting_month,District,Rainfall_category)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    value_to_label("Rainfall_category")%>%
    label_to_status("Rainfall_category",status) %>%
    mutate(
      variable = "Rainfall",
      indicator = "Climate",
      other ="",
      number = "") %>%
    dplyr::rename(value = Rainfall_category) %>%
    select(-month) %>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  #------------------coping-----
  coping <- ewea_raw%>%
    select(reporting_month,District,coping)%>%
    mutate(
      reporting_month = as.yearmon(reporting_month)
    ) %>%
    separate_rows(coping, sep = " ",) %>%
    mutate(
      number = coping
    ) %>%
    dplyr::rename(coping_strategies = coping)  %>%
    value_to_label("coping_strategies") %>%
    mutate(
      other = coping_strategies
    ) %>%
    label_to_value("coping_strategies") %>%
    #mutate(variable = "coping_strategies",indicator = "coping") %>%
    rename(value = coping_strategies) %>%
    select(reporting_month,District,value,number,other)%>%
    group_by(reporting_month,District) %>%
    reframe(
      number = number[which.max(value)],
      other = other[which.max(value)],
      value = max(value),
    ) %>%
    label_to_status("coping_strategies",status) %>%
    mutate(variable = "coping_strategies",indicator = "Coping",
           value = other) %>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  
  #------------water source
  
  water_source <- ewea_raw%>%
    select(District,reporting_month,Water_source_dried)%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      number = Water_source_dried
    ) %>%
    value_to_label("Water_source_dried") %>%
    label_to_status("Water_source_dried",status) %>%
    mutate(
      variable = "Water_source_dried",
      other = "",
      indicator = "Climate"
    ) %>%
    dplyr::rename(value = Water_source_dried) %>%
    drop_na(value)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  #---------------------river_levels
  
  
  river_levels <-ewea_raw%>%
    select(District,reporting_month,River) %>%
    mutate(
      reporting_month = as.yearmon(reporting_month)
    )%>%
    value_to_label("River")%>%
    label_to_status("River",status) %>%
    mutate(
      variable = "River Levels",
      number = "",
      other = "",
      indicator = "Climate"
    )%>%
    dplyr::rename(value = River)%>%
    drop_na(status) %>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  #-----------flash floods
  
  
  flash_floods <- ewea_raw%>%
    select(reporting_month,District,flashfloods,Flashfloods_aff_hhs)%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      variable = "flashfloods",
      indicator = "Climate"
    )%>%
    ## change the values to labels using custom function
    value_to_label(.,"flashfloods")%>%
    dplyr::rename(other = Flashfloods_aff_hhs,
                  value = flashfloods)%>%
    label_to_status("flashfloods",status)%>%
    drop_na(value)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  ## Cleaning and harmonizing cattle, sheep/goat conditions indicator data----
  
  cattle.condition <- ewea_raw%>%
    select(reporting_month,District,Cattle)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    rename(`Cattle condition` = Cattle)%>%
    ## change the values to labels using custom function
    value_to_label("Cattle condition")%>%
    label_to_status("Cattle condition", status)%>%
    dplyr::rename(value = `Cattle condition`) %>%
    mutate(
      variable = "Cattle condition",
      indicator = "Livelihood",
      number = "",
      other = ""
    )%>%
    drop_na(value) %>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  goat.sheep.condition <- ewea_raw%>%
    select(reporting_month,District,Goat_sheep)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    rename(`Sheep/goat condition` = Goat_sheep)%>%
    ## change the values to labels using custom function
    value_to_label("Sheep/goat condition")%>%
    label_to_status("Sheep/goat condition", status)%>%
    dplyr::rename(value = `Sheep/goat condition`) %>%
    mutate(
      variable = "Sheep/goat condition",
      indicator = "Livelihood",
      number = "",
      other = ""
    )%>%
    drop_na(value) %>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  
  
  crop.diseases <- ewea_raw%>%
    select(reporting_month,District,crop_affected_percent,crop_disease_type,crop_disease_occ)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    dplyr::rename(value = crop_affected_percent,
                  other = crop_disease_type,
                  number = crop_disease_occ)%>%
    mutate(
      value = as.character(value),
      variable = "Crop diseases",
      indicator = "Livelihood")%>%
    label_to_status("Crop diseases",status) %>%
    
    drop_na(value)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  ## Cleaning and harmonizing pasture conditions indicator data----
  
  pasture <- ewea_raw%>%
    select(reporting_month,District,Pasture_longevity)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    ## change the values to labels using custom function
    value_to_label(.,"Pasture_longevity")%>%
    dplyr::rename(value = Pasture_longevity)%>%
    mutate(
      variable = "Pasture conditions",
      other = "",
      indicator = "Livelihood",
      number = "")%>%
    label_to_status("Pasture conditions",status) %>%
    drop_na(value)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  ## Cleaning and harmonizing migration indicator data----
  
  migration <- ewea_raw%>%
    select(reporting_month,District,HH_arrived,HH_migrated)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    rename(
      `Migration (Arrivals)` = HH_arrived,
      `Migration (departure)` = HH_migrated)%>%
    pivot_longer(cols = `Migration (Arrivals)`: `Migration (departure)`,names_to = "variable",values_to = "value")%>%
    mutate(status = "Normal",
           number = "",
           other = "",
           indicator = "Livelihood")%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  ## Cleaning and harmonizing presence of locust swarms indicator data
  
  locust <- ewea_raw%>%
    dplyr::select(reporting_month,District,spent_days,locust_infest)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    ## change the values to labels using custom function
    value_to_label(.,"locust_infest")%>%
    dplyr::rename(value = spent_days)%>%
    mutate(variable = "Locust infestation",
           status = case_when(
             as.numeric(value)== 0 ~ "Normal",
             as.numeric(value) <=2 & as.numeric(value) >=0 ~ "Alert",
             as.numeric(value) > 2 ~ "Alarm",
             .default = NA),
           number = "",
           other = "Presence of locust hoppers/swarms in the area",
           indicator = "Climate")%>%
    drop_na(status)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  ## Cleaning and harmonizing crop impact indicator data ----
  
  crop.impact<- ewea_raw%>%
    dplyr::select(reporting_month,District,crop_impact)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    ## change the values to labels using custom function
    value_to_label(.,"crop_impact")%>%
    dplyr::rename(value = crop_impact)%>%
    mutate(status = case_when(
      value == "No impact or little impact" ~ "Normal",
      value == "Less than a third of crops damaged by wilting/floods/pests" ~ "Alert",
      value == "more than a third of crops damaged by wilting/floods/pests" ~ "Alarm",
      .default = NA),
      number = "",
      other = "Estimated proportion of crops wilted, affected by pests or flooded  per hectare in the area",
      variable = "crop impact",
      indicator = "Livelihood")%>%
    drop_na(value)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  
  
  ## Cleaning and harmonizing HHs displaced due to conflict indicator data----
  
  conflict <- ewea_raw%>%
    dplyr::select(reporting_month,District,displaced_hh)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    dplyr::rename(value = displaced_hh)%>%
    mutate(variable = "Conflict",
           status = case_when(
             as.numeric(value) == 0 ~ "Normal",
             as.numeric(value) <=50 ~ "Alert",
             as.numeric(value) > 50~"Alarm",
             .default = NA),
           number = "",
           other = "Number of households displaced due to conflict events (armed/clan)",
           indicator = "Climate")%>%
    drop_na(value)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  ## Cleaning and harmonizing animal mortality indicator data----
  
  animal.mortality <- ewea_raw%>%
    dplyr::select(reporting_month,District,Outbreak_deaths)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    ## change the values to labels using custom function
    value_to_label(.,"Outbreak_deaths")%>%
    dplyr::rename(value = Outbreak_deaths)%>%
    mutate(variable = "Animal mortality",
           status = case_when(
             value == "Normal Animal mortality"~"Normal",
             value == "Higher than normal animal mortality"~"Alert",
             value == "Very high animal mortality/uncontrolled outbreak"~"Alarm",
             .default = NA),
           number = "",
           other = "",
           indicator = "Livelihood")%>%
    drop_na(value)%>%
    select(reporting_month,District,variable,value,status,indicator,number,other)
  
  
  
  ewea <-rbind(market_price,
               rainfall,
               coping,
               animal.mortality,
               cattle.condition,
               goat.sheep.condition,
               water_source,
               river_levels,
               flash_floods,
               crop.diseases,
               pasture,
               locust,
               crop.impact,
               conflict)
  
  ewea <-as.data.frame(ewea)%>%
    mutate(variable_type = case_when(
      indicator == "Climate" ~ "Shock occurence",
      .default = "Shock effects"
    ),
    district_name = word(District, 1))
  
  
  
  ewea <- readxl::read_excel("docs/FMS and districts.xlsx")%>%
    right_join(as.data.frame(ewea))
  
 
  
  shock.occurence <- ewea%>%
    filter(variable_type == "Shock occurence")%>%
    drop_na(status)%>%
    dplyr::summarise(
      count = n(),
      .by = c("reporting_month","FMS","district_name","status")
    ) %>%
    pivot_wider(names_from = status , values_from = count) %>%
    replace_na(list(Alarm = 0, Normal = 0 ,Alert = 0)) %>%
    mutate(
      shock.occurence = case_when(
        (Alarm>=1 | Alert >=1)~1,
        .default = 0
      ),
      classify = case_when(
        (shock.occurence==1 & Alarm>=5)~"Critical",
        (shock.occurence==1 & Alarm >=3 & Alarm<5)~"Severe",
        (shock.occurence==1 & Alarm <3)~"Moderate",
        .default =  "Normal")
    )
  
  
  
  shock.effect <- ewea%>%
    filter(variable_type == "Shock effects")%>%
    drop_na(status)%>%
    dplyr::summarise(
      count = n(),
      .by = c("reporting_month","FMS","district_name","status")
    ) %>%
    pivot_wider(names_from = status , values_from = count) %>%
    replace_na(list(Alarm = 0, Normal = 0 ,Alert = 0)) %>%
    mutate(
      shock.effect = case_when(
        (Alarm >=3 | Alarm >=2 & Alert >=3)~1,
        .default = 0)
    )
  
  red_flagging <- full_join(shock.occurence , shock.effect ,by = c("reporting_month","FMS","district_name")) %>%
    mutate(sum = shock.occurence+shock.effect,
           red_flagged = ifelse(sum==2,"Yes","No"),
           classify = ifelse(red_flagged == "No",NA,classify),
           Alarm = Alarm.x + Alarm.y,
           Alert = Alert.x + Alert.y,
           Normal= Normal.x + Normal.y ,
           District = district_name) %>%
    select(reporting_month , FMS , District , shock.occurence , shock.effect , red_flagged,classify,Alarm ,Normal ,Alert)
  
  ewea$reporting_month <- format(ewea$reporting_month, "%Y-%m")
  shock.occurence$reporting_month <-format(shock.occurence$reporting_month, "%Y-%m")
  shock.effect$reporting_month <-format(shock.effect$reporting_month, "%Y-%m")
  red_flagging$reporting_month <-format(red_flagging$reporting_month, "%Y-%m")
  
  createTable("ewea", ewea)
  createTable("shock.occurence", shock.occurence)
  createTable("shock.effect", shock.effect)
  createTable("red_flagging", red_flagging)
  createTable("market_threshold", market_thresholds)
  
  return(
    "success"
  )
  
}
