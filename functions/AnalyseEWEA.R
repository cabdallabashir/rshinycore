update_ewea("abdullahiaweis","tutka_sulsulaaye@123")
update_ewea <- function(user , pass , date_to_update){
  
  source("db/dbCreation.R")
  
  source("functions/value_to_label.R")
  source("functions/label_to_status.R")
  source("functions/label_to_value.R")
  

  ##### Retrieving submitted EWEA submitted data from ona via API and cleaning----
  get_data_ewea <- function(username, password ){
    
    required_columns <- c(
      
      "reporting_month","Member_name","District_Name","Community_name",
      "Rainfall_category",
      "Cumulative_rain",
      "temp",
      "Flash_flood",
      "Flash_flood_days",
      "Flashfloods_aff_hhs",
      "River_levels",
      "river_level_secondary",
      "awd_cases",
      "awd_cases_previous_week",
      "awd_cases_treated",
      "U5_mortality",
      "U5_screening",
      "locust_infest",
      "spent_days",
      "locust_effect",
      "conflict_presence",
      "conflict_intensity",
      "conflict_duration",
      "disputes_occ",
      "conflict_types",
      "conflict_affected_ppl",
      "displaced_hh",
      "HH_migrated",
      "Catchment_status",
      "water_point",
      "Water_source_dried",
      "crop_condition",
      "crop_disease_occ",
      "crop_disease_type",
      "crop_names_affected",
      "Pasture_condition",
      "Pasture_longevity",
      "Cattle",
      "Goat_sheep",
      "Camel",
      "Livestock_disease_prev",
      "Livestock_disease_types",
      "Livestock_affected",
      "Outbreak_deaths",
      "Price_water",
      "Price_maize",
      "Price_sorghum",
      "Price_rice",
      "Goat_price",
      "fuell_price",
      "Labour_price",
      "coping",
      "Residence_type",
      "crop_condition_stage",
      "crop_condition_causes"
    )
    
    # Make the API request
    response_ewea_data <- GET(url = "https://api.ona.io/api/v1/data/826805", authenticate(username, password))
    status <- status_code(response_ewea_data)
    if (status == 200) {
      ewea <- prettify(rawToChar(response_ewea_data$content))%>% fromJSON()%>%
        purrr::discard(is.list) %>%
        as.data.frame()
      
      names(ewea) <- gsub(".*/", "", names(ewea)) 

      ewea%<>%
        dplyr::mutate( 
          dataID = `_id`,
          submission_time = ymd_hms(`_submission_time`),
          ## format end time to date format. Reporting_date column has some inconsistencies, so don't use
          Reporting_date =  ymd(Reporting_date),
          ## if the data was reported before 20th,it means the submission was for the past month as members start reporting on 20th of each month
          reporting_month = ifelse(day(Reporting_date)<20,
                                   format(Reporting_date %m-% months(1), "%Y-%m"),
                                   format(Reporting_date, "%Y-%m"))
          
          ## In some districts, multiple members work in, here we add the member suffix for distinction
          # District = ifelse(District_Name == "Afgoye" | District_Name =="Galkaayo" | District_Name =="Dhusamareb",
          #                   paste0(District_Name," ",Member_name),
          #                   District_Name)
          )
      
      # Loop through each required column
      for (col in required_columns) {
        # Check if the column exists
        if (!col %in% names(ewea)) {
          # If it does not exist, add it with NA values
          ewea[[col]] <- NA
        }
      }
        # select columns for only data needed for analysis and visualization in dashboard
      ewea%<>%mutate(
        Member_name = trimws(Member_name),
        District_Name = trimws(District_Name),
        Residence_type = ifelse(is.na(Residence_type),"Rural",Residence_type)
      )%>%
        select(dataID,submission_time,Reporting_date,
          reporting_month,Member_name,District_Name,Community_name,Residence_type ,
          Rainfall_category,
          Cumulative_rain,
          temp,
          Flash_flood,
          Flash_flood_days,
          Flashfloods_aff_hhs,
          River_levels,
          river_level_secondary,
          awd_cases,
          awd_cases_previous_week,
          awd_cases_treated,
          U5_mortality,
          locust_infest,
          spent_days,
          locust_effect,
          conflict_presence,
          conflict_intensity,
          conflict_duration,
          disputes_occ,
          conflict_types,
          conflict_affected_ppl,
          displaced_hh,
          HH_arrived,
          HH_migrated,
          Catchment_status,
          water_point,
          Water_source_dried,
          crop_condition,
          crop_disease_occ,
          crop_disease_type,
          crop_names_affected,
          Pasture_condition,
          Pasture_longevity,
          Cattle,
          Goat_sheep,
          Camel,
          Livestock_disease_prev,
          Livestock_disease_types,
          Livestock_affected,
          Outbreak_deaths,
          Price_water,
          Price_maize,
          Price_sorghum,
          Price_rice,
          Goat_price,
          fuell_price,
          Labour_price,
          crop_condition_stage,
          crop_condition_causes,
          coping)%<>%as.data.table()
      
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
      warning("Error encountered. Status code:", status)
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
  
  
  ewea_raw <- get_data_ewea("abdullahiaweis","tutka_sulsulaaye@123")
  
  ewea_raw%<>%filter(
    !(Residence_type=="Rural" & Member_name %in% c("NRC","GREDO") & District_Name %in% c("Baidoa"))
  )
  
  ewea_raw%<>%filter(
    !(Residence_type=="Rural" & Member_name == "IRC_KAALO")
  )
  
  ewea_raw%<>%mutate(
    Member_name = case_when(
      Residence_type=="Urban" & Member_name == "NRC_GREDO" ~ "NRC",
      .default = Member_name
    ),
    Residence_type = case_when(
      District_Name=="Jariiban" & Member_name == "KAALO" ~ "Urban",
      .default = Residence_type
    ),
    
    reporting_month = case_when(
      dataID == "168983814" ~ "2025-03",
      dataID == "169628411" ~ "2025-03",
      dataID == "169639057" ~ "2025-03",
      .default = reporting_month
    )
  )
  
  ewea_raw%<>%
    mutate(
      District_Name = case_when(
        District_Name == "Galkacyo" & Member_name == "IRC" ~ "Galkacyo-South",
        District_Name == "Galkacyo" & Member_name == "KAALO" ~ "Galkacyo-North",
        .default = District_Name
      )
    )
 
  ewea_raw%<>%separate_rows(Community_name,sep = " ")
  
  ewea_raw%<>%arrange(desc(submission_time))%>%
    group_by(reporting_month,District_Name,Community_name,Residence_type) %>%
    slice(1) %>%
    ungroup() %>%
    as.data.frame()%>%
    filter(Community_name!=0)
  
  if(ewea_raw$status[1] != "200"){
    return(
      ewea_raw$message[1]
    )
  }
  
  
  
  market_thresholds_update <- read.csv("docs/ewea/ewea price threshold.csv")%>%
    dplyr::rename(`Upto` = Up.to)%>%
    mutate_all(~replace(., is.na(.) | . == 'NA' | . == '<NA>' | . == '' , 0)) %>%
    mutate(
      From = round(as.numeric(From),3) , 
      Upto = round(as.numeric(Upto),2),
      district = trimws(district),
      status = trimws(status),
      variable = trimws(variable)
      )

# 
#   ewea_raw%>%
#     select(reporting_month,Member_name,District_Name ,Residence_type,Community_name, starts_with("conflict"))%>%filter(District_Name == 'Belet_Hawa')%>%view()

  #---------- rainfall analysis
  
  rainfall <- ewea_raw%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,Rainfall_category,temp,Cumulative_rain)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    value_to_label("Rainfall_category")%>%
    label_to_status("Rainfall_category",status) %>%
    mutate(
      variable = "Rainfall",
      indicator = "Climate",
      other_info1 ="Cumulative_rain",
      content1 = Cumulative_rain,
      other_info2 = "",
      content2 = "") %>%
    dplyr::rename(value = Rainfall_category) %>%
    select(-month) %>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  #-----------flash floods analysis
  
  
  flash_floods <- ewea_raw%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,Flash_flood,Flash_flood_days,Flashfloods_aff_hhs)%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      variable = "Flash floods",
      indicator = "Climate",
      other_info1 = "# days above normal rainfall was received",
      content1 = Flash_flood_days,
      other_info2 = "# of HHs affected by the flash floods",
      content2 = Flashfloods_aff_hhs
    )%>%
    ## change the values to labels using custom function
    value_to_label(.,"Flash_flood")%>%
    dplyr::rename(value = Flash_flood)%>%
    label_to_status("Flash_flood",status)%>%
    drop_na(value)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  #---------------------river_levels analysis
  
  
  river_levels <-ewea_raw%>%
    select(reporting_month,Member_name,District_Name,Community_name,River_levels,
           river_level_secondary,Residence_type)%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      River_levels = ifelse(District_Name %in% reverine_districts , River_levels , 4)
    )%>%
    value_to_label("River_levels")%>%
    label_to_status("River_levels",status) %>%
    mutate(
      variable = "River Levels",
      indicator = "Climate",
      other_info1 = "river level secondary data",
      content1 = river_level_secondary,
      other_info2 = "",
      content2 = ""
    )%>%
    dplyr::rename(value = River_levels)%>%
    drop_na(status) %>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  
  #---------------------awd_cases analysis
  
  awd_cases <-ewea_raw%>%
    select(reporting_month,Member_name,District_Name,Community_name,awd_cases,awd_cases_previous_week, awd_cases_treated,
           U5_mortality,Residence_type) %>%
    mutate(
      awd_cases = abs(awd_cases),
      reporting_month = as.yearmon(reporting_month)
    )%>%
    mutate(
      other_info1 = "awd cases reported",
      content1 = awd_cases,
      awd_cases_previous_week = ifelse(is.na(awd_cases_previous_week),0,awd_cases_previous_week)
    )%>%
    value_to_label("awd_cases")%>%
    label_to_status("awd_cases",status) %>%
    mutate(
      variable = "AWD Cases",
      indicator = "Pests and Diseases",
      
      other_info2 = "awd cases previous weeks",
      content2 = awd_cases_previous_week
    )%>%
    dplyr::rename(value = awd_cases)%>%
    drop_na(status) %>%
    mutate(
      hold = value,
      value = paste(content1,"Cases"),
      content1 = hold
    )%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  ## -------------------------locust swarms analysis
  
  locust <- ewea_raw%>%
    dplyr::select(reporting_month,Member_name,District_Name,Community_name,locust_infest,spent_days,  locust_effect,Residence_type)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    ## change the values to labels using custom function
    value_to_label(.,"locust_infest")%>%
    label_to_status(.,"locust_infest",status)%>%
    dplyr::rename(value = spent_days)%>%
    mutate(
      value = ifelse(is.na(value), 0, value),
      variable = "Locust infestation",
      indicator = "Pests and Diseases",
      other_info1 = "",
      content1 = "",
      other_info2 = "locust effect",
      content2 = locust_effect)%>%
    drop_na(status)%>%
    mutate(
      content2 = case_when(
        content2 == "1" ~ "Severe",
        content2 == "2" ~ "Medium",
        content2 == "3" ~ "Low"
      ),
      value = paste("Locust spent",value,"days")
    )%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)

  
  
  #---------------------- Conflicts ---------
  
  conflict <- ewea_raw%>%
    dplyr::select(reporting_month,Member_name,District_Name,Community_name,conflict_presence,
                  conflict_intensity, conflict_duration,disputes_occ,
                  conflict_types, conflict_affected_ppl,Residence_type)%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      conflict_score = case_when(
        conflict_presence == 0 ~ 0,
        conflict_presence > 0 ~ ((as.numeric(conflict_presence) + as.numeric(conflict_intensity) + as.numeric(conflict_duration)) / 10)*100,
        .default = 0
      )  
    ) %>% 
    label_to_status(.,"conflict_score",status)%>%
    dplyr::rename(value = conflict_score)%>%
    mutate(
      conflict_presence = case_when(
        conflict_presence == 0 ~ "No conflict within the community and in neighbouring communities",
        conflict_presence == 1 ~ "Presence of conflict in neighbouring communities",
        conflict_presence == 2 ~ "Presence of conflict within the community",
        .default = conflict_presence
      ),
      conflict_intensity = case_when(
        conflict_intensity == 1 ~ "low",
        conflict_intensity == 2 ~ "Moderate",
        conflict_intensity == 3 ~ "High",
        .default = conflict_intensity
      ),
      conflict_duration = case_when(
        conflict_duration  == 1 ~ "still starting",
        conflict_duration  == 2 ~ "0-6 months",
        conflict_duration  == 3 ~ "7-12 months",
        conflict_duration  == 4 ~ "13-18 months",
        conflict_duration  == 5 ~ "More than 18 months",
        .default = conflict_duration 
      ),
      variable = "Armed/Clan Conflicts",
      indicator = "Non-Climate",
      other_info1 =  "Conflict Presence",
      content1 =  conflict_presence,
      other_info2 = "Conflict insensity",
      content2 = paste(paste(conflict_intensity) ,paste("Duration:",conflict_duration) ,sep = "<br>")
      )%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  
  #------------------- Population Disoplacement Analysis -----------

  pop_displacement <- ewea_raw%>%
    dplyr::select(reporting_month,Member_name,District_Name ,Community_name, HH_migrated,Residence_type
    )%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      HH_migrated = abs(HH_migrated)
      ) %>%
    ## change the values to labels using custom function
    label_to_status(.,"HH_migrated",status)%>%
    dplyr::rename(value = HH_migrated)%>%
    mutate(
      variable = "Households migrated",
      indicator = "Population Movements",
      other_info1 = "",
      content1 = "",
      other_info2 = "",
      content2 = "")%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  
  pop_displacement_arrived <- ewea_raw%>%
    dplyr::select(reporting_month,Member_name,District_Name,Community_name, HH_arrived,Residence_type
    )%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      HH_arrived = abs(HH_arrived)
    ) %>%
    ## change the values to labels using custom function
    label_to_status(.,"HH_arrived",status)%>%
    dplyr::rename(value = HH_arrived)%>%
    mutate(
      variable = "Households arrived",
      indicator = "Population Movements",
      other_info1 = "",
      content1 = "",
      other_info2 = "",
      content2 = "")%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  
  #------------------- Water Cachment Analysis -----------
  
  water_point <- ewea_raw%>%
    dplyr::select(reporting_month,Member_name,District_Name,Community_name, Catchment_status,water_point,Water_source_dried,Residence_type
    )%>%
    mutate(
      reporting_month = as.yearmon(reporting_month)
    ) %>%
    ## change the values to labels using custom function
    label_to_status(.,"Catchment_status",status)%>%
    value_to_label(.,"Catchment_status")%>%
    dplyr::rename(value = Catchment_status)%>%
    mutate(
      variable = "Primary Water source condition",
      indicator = "Livelihoods",
      other_info1 = "Nearest waterpoint(KM) ",
      content1 = water_point,
      other_info2 = "State",
      content2 = Water_source_dried)%>%
    drop_na(status)%>%
    mutate(
      content2 = case_when(
        content2 == "1" ~ "Water source is at risk of drying up",
        content2 == "2" ~ "Water source is not at risk of drying up",
        content2 == "3" ~ "Water source has already dried up",
        content2 == "98" ~ "Not Applicable",
        .default = content2
      )
    )%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
 
  
  #-------------------- Crop_condition analysis
  
  crop_condition <- ewea_raw%>%
    dplyr::select(reporting_month,Member_name,District_Name,Community_name, crop_condition,
                  crop_disease_occ,crop_disease_type, crop_names_affected,Residence_type,crop_condition_stage,
                  crop_condition_causes
    )%>%
    mutate(
      reporting_month = as.yearmon(reporting_month)
    ) %>%
    ## change the values to labels using custom function
    label_to_status(.,"crop_condition",status)%>%
    value_to_label(.,"crop_condition")%>%
    dplyr::rename(value = crop_condition)%>%
    mutate(
      crop_condition_stage = str_replace_all(crop_condition_stage,c("1" = "Crop Planting Stage", "2" = "Crop Growth Stage", "3" = "Crop Harvesting Stage")),
      crop_condition_causes =str_replace_all(crop_condition_causes,c("1" = "Floods", "2" = "Pests", "3" = "Diseases","4"="Drought","96"="Other")),

    )%>%
    mutate(
      variable = "Crop Condition",
      indicator = "Livelihoods",
      other_info1 = "Crop Desease Types",
      content1 = crop_disease_type,
      other_info2 = "Details",
      content2 = paste(paste0("Crops stage :",crop_condition_stage) , paste0("Crop Deseases:",crop_condition_causes),sep = "<br>"))%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)

  #-------------------- Pasture Condition analysis
  
  pasture_condition <- ewea_raw%>%
    dplyr::select(reporting_month,Member_name,District_Name,Community_name, Pasture_condition,
                  Pasture_longevity,Residence_type
    )%>%
    mutate(
      reporting_month = as.yearmon(reporting_month)
    ) %>%
    label_to_status(.,"Pasture_condition",status)%>%
    value_to_label(.,"Pasture_condition")%>%
    value_to_label(.,"Pasture_longevity")%>%
    dplyr::rename(value = Pasture_condition)%>%
    mutate(
      variable = "Pasture Condition",
      indicator = "Livelihoods",
      other_info1 = "Pasture longevity",
      content1 = Pasture_longevity,
      other_info2 = "",
      content2 = "")%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  
  #------------------Cattle  Analaysis
  
  cattle_condition <- ewea_raw%>%
    select(reporting_month,Member_name,District_Name ,Community_name, Cattle,Residence_type)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    label_to_status("Cattle", status)%>%
    value_to_label("Cattle")%>%
    dplyr::rename(value = Cattle) %>%
    mutate(
      variable = "Cattle condition",
      indicator = "Livelihoods",
      other_info1 = "",
      content1 = "",
      other_info2 = "",
      content2 = ""
    )%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  #------------------Camel Analaysis
  
  camel_condition <- ewea_raw%>%
    select(reporting_month,Member_name,District_Name,Community_name , Camel,Residence_type)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    label_to_status("Camel", status)%>%
    value_to_label("Camel")%>%
    dplyr::rename(value = Camel) %>%
    mutate(
      variable = "Camel condition",
      indicator = "Livelihoods",
      other_info1 = "",
      content1 = "",
      other_info2 = "",
      content2 = ""
    )%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  #------------------Goat_sheep Analaysis
  
  goat_sheep_condition <- ewea_raw%>%
    select(reporting_month,Member_name,District_Name ,Community_name, Goat_sheep,Residence_type)%>%
    mutate(reporting_month = as.yearmon(reporting_month)) %>%
    label_to_status("Goat_sheep", status)%>%
    value_to_label("Goat_sheep")%>%
    dplyr::rename(value = Goat_sheep) %>%
    mutate(
      variable = "Goat_sheep condition",
      indicator = "Livelihoods",
      other_info1 = "",
      content1 = "",
      other_info2 = "",
      content2 = ""
    )%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  
  #------------------Market Analysis
  
  # d <market_price%>%
  #   filter(reporting_month=="FEB 2025" & Member_name=="ACF" & District_Name=="Waajid")
  
  market_price <- ewea_raw %>%
    select(reporting_month,Member_name,District_Name,Community_name,Price_water, Price_maize,
           Price_sorghum,Price_rice,Goat_price,fuell_price,Labour_price,Residence_type) %>%
    mutate_all(~replace(., is.na(.) | . == 'NA' | . == '', 0)) %>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
      district_residence_type = paste(District_Name,Residence_type,sep = "-")
      )%>%
    summarise(
      Sorghum = round(mean(as.numeric(Price_sorghum),na.rm = T),2),
      Goat = round(mean(as.numeric(Goat_price),na.rm = T),2),
      Water = round(mean(as.numeric(Price_water),na.rm = T),2),
      Maize = round(mean(as.numeric(Price_maize),na.rm = T),2),
      Rice = round(mean(as.numeric(Price_rice),na.rm = T),2),
      Labour = round(mean(as.numeric(Labour_price),na.rm = T),2),
      Fuel = round(mean(as.numeric(fuell_price),na.rm = T),2),
      Member_name = first(Member_name),
      Residence_type = first(Residence_type),
      .by =  c("reporting_month","District_Name","district_residence_type","Community_name")
    )%>%
    pivot_longer(
      cols = Sorghum : Fuel , 
      names_to = "variable",values_to = "value"
      
    ) %>%
    left_join(market_thresholds_update , by = c(c("district_residence_type" = "district"),"variable"),
              relationship = "many-to-many") %>%
    mutate(
      var_status = ifelse(
        variable == 'Labour' | variable =='Goat',
        ifelse(
          status == 'Alert',
          value <= From & value >= Upto,
          ifelse(
            status == 'Normal',
            value >= Upto,
            value <= From
          )
        )
        ,
        ifelse(
          status == 'Alert',
          value >= From & value <= Upto,
          ifelse(
            status == 'Normal',
            value <= Upto,
            value >= From
          )
          
        )
        
      ),
      status = ifelse(is.na(status), "Normal", status),
      From  = ifelse(is.na(From), 0, From),
      Upto = ifelse(is.na(Upto), 0, Upto),
      var_status =  ifelse(is.na(var_status), TRUE, var_status),
      variable = variable,
      indicator = "Market",
      other_info1 = "From Price",
      content1 = From ,
      other_info2 = "Upto Price",
      content2 = Upto
    ) %>%
    filter(var_status == TRUE | is.na(var_status)) %>%
    # filter(Member_name == "NRC" & District_Name=="Dhusamareeb" & 
    #          reporting_month =="2024-11")%>% 
    # select(variable ,value ,  From , Upto,status , var_status)
    # group_by(reporting_month,Member_name,District_Name,variable) %>%
    # filter(n() > 1)%>%
    # summarise(
    #   number_of_submission = n()
    # )
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
  

  #------------------coping Strategy Analysis
  
  coping <- ewea_raw%>%
    select(reporting_month,Member_name,District_Name,Community_name,coping,Residence_type)%>%
    mutate(
      reporting_month = as.yearmon(reporting_month),
    ) %>%
    separate_rows(coping, sep = " ") %>%
    mutate(coping_strategies = coping , coping_codes = coping)%>%
    value_to_label("coping") %>%
    label_to_value("coping_strategies") %>%
    group_by(reporting_month ,Member_name ,District_Name,Community_name,Residence_type) %>%
    reframe(
      coping = paste(coping,collapse = ","),
      coping_code  = paste(coping_codes,collapse = ","),
      normal = sum(coping_strategies == 1, na.rm = TRUE),
      alert = sum(coping_strategies == 2, na.rm = TRUE),
      alarm = sum(coping_strategies == 3, na.rm = TRUE),
      count = n()
    )%>%
    select(-count)%>%
    mutate(
      value = coping_code,
      status = case_when(
        alarm == 0 & alert >= 0 & alert <= 2 ~ "Normal",
        alarm == 0 & alert > 2  ~ "Alert",
        alarm > 0  ~ "Alarm",
        .default = "Normal"
      )
    )%>%
    mutate(
      variable = "Coping Strategy",
      indicator = "Coping Strategies",
      other_info1 = "Alert Frequency,Alarm Frequency",
      content1 = paste(alert,alarm,sep = ","),
      other_info2 = "Coping",
      content2 = coping
    )%>%
    drop_na(status)%>%
    select(reporting_month,Member_name,District_Name,Community_name,Residence_type,indicator,variable,value,status,other_info1,content1,other_info2,content2)
    

  ewea <-rbind(
    rainfall,flash_floods,river_levels,awd_cases,locust,conflict,pop_displacement,pop_displacement_arrived,water_point,crop_condition,pasture_condition,
    cattle_condition,camel_condition,goat_sheep_condition,market_price,coping
  )
  
  ewea <-as.data.frame(ewea)%>%
    mutate(
      indicator_type = case_when(
      indicator %in% c("Climate","Pests and Diseases","Non-Climate") ~ "Shock occurence",
      .default = "Shock effects"
      ),
      District_Residency = paste(District_Name,Residence_type,sep = "-")
    )
  
  

  weighting_Scores <- readxl::read_excel("docs/ewea/ewea variable weighting.xlsx")%>%
    mutate_all(~replace(., is.na(.) | . == 'NA' | . == '<NA>' | . == '' , 0)) %>%
    mutate(
      variable_group_balancing = round(as.numeric(variable_group_balancing),1) , 
    )%>%
    as.data.frame()
  
  ewea <- weighting_Scores%>%right_join(as.data.frame(ewea),by = c("variable"))%>%
    mutate(
      status_code = case_when(
        status == 'Normal' ~ 0,
        status == 'Alert' ~ 1,
        status == 'Alarm' ~ 2,
        .default = 0
      ),
      variable_score = round(variable_weight * variable_group_balancing * status_code,1)
    )%>%
    as.data.frame()
  
 
    
  
  
  ewea <- readxl::read_excel("docs/ewea/ewea districts and fms.xlsx")%>%
    right_join(as.data.frame(ewea),by = c("District_Name","Residence_type"))%>%
    as.data.frame()%>%
    select(-Member_name.y)%>%
    dplyr::rename(Member_name = Member_name.x)
  
  
  
  ewea%<>%
    mutate(
      variable = case_when(
        variable == "Sorghum"~"Sorghum Price",
        variable == "Goat" ~ "Goat Price",
        variable == "Water" ~ "Water Price",
        variable == "Maize" ~ "Maize Price",
        variable == "Rice" ~ "Rice Price",
        variable == "Labour" ~ "Labour Price",
        variable == "Fuel" ~ "Fuel Price",
        .default = variable
      )
    )
  
  variable_scores <- ewea%>%
    drop_na(status)%>%
    dplyr::summarise(
      count = n(),
      variable_score = round(sum(variable_score,na.rm = TRUE),1),
      .by = c("reporting_month","Member_name","FMS","Region_name","District_Name","Community_name","Residence_type")
    )%>%select(-count)
  
 
  
  
  shock_occurence <- ewea%>%
    filter(indicator_type == "Shock occurence")%>%
    drop_na(status)%>%
    dplyr::summarise(
      count = n(),
      .by = c("reporting_month","Member_name","FMS","Region_name","District_Name","Community_name","Residence_type","status")
    ) %>%
    pivot_wider(names_from = status , values_from = count)
    
    for (status in c("Alarm","Alert","Normal")) {
      if (!(status %in% colnames(shock_occurence))) {
        shock_occurence[[status]] <- 0
      }
    }
  
    shock_occurence%<>%
      replace_na(list(Alarm = 0, Normal = 0 ,Alert = 0))%>%
      group_by(reporting_month,Member_name,FMS,Region_name,District_Name,Community_name,Residence_type)%>%
    reframe(
      Alarm = sum(Alarm),
      Alert =sum(Alert),
      Normal = sum(Normal)
    )%>%
    mutate(
      shock_occurence = case_when(
        (Alarm>=1 | Alert >=1)~1,
        .default = 0
      ),
      classify = case_when(
        (shock_occurence==1 & Alarm>=5)~"Critical",
        (shock_occurence==1 & Alarm >=3 & Alarm<5)~"Severe",
        (shock_occurence==1 & Alarm <3)~"Moderate",
        .default =  "Normal")
    )
 
  
  
  shock_effect <- ewea%>%
    filter(indicator_type == "Shock effects")%>%
    drop_na(status)%>%
    dplyr::summarise(
      count = n(),
      .by = c("reporting_month","Member_name","FMS","Region_name","District_Name","Community_name","Residence_type","status")
    ) %>%
    pivot_wider(names_from = status , values_from = count)
    
    for (status in c("Alarm","Alert","Normal")) {
      if (!(status %in% colnames(shock_effect))) {
        shock_effect[[status]] <- 0
      }
    }
  
  shock_effect%<>%
    replace_na(list(Alarm = 0, Normal = 0 ,Alert = 0))%>%
    group_by(reporting_month,Member_name,FMS,Region_name,District_Name,Community_name,Residence_type)%>%
    reframe(
      Alarm = sum(Alarm),
      Alert =sum(Alert),
      Normal = sum(Normal)
    )%>%
    mutate(
      shock_effect = case_when(
        (Alarm >=3 | (Alarm >=2 & Alert >=3))~1,
        .default = 0)
    )
 
  
  # red_flagging <- full_join(shock_occurence , shock_effect ,by = c("reporting_month","Member_name","FMS","Region_name","District_Name","Residence_type")) %>%
  #   mutate(sum = shock_occurence+shock_effect,
  #          red_flagged = ifelse(sum==2,"Yes","No"),
  #          classify = ifelse(red_flagged == "No",NA,classify),
  #          Alarm = Alarm.x + Alarm.y,
  #          Alert = Alert.x + Alert.y,
  #          Normal= Normal.x + Normal.y) %>%
  #   select(reporting_month,Member_name  , FMS ,Region_name, District_Name , Residence_type , shock_occurence , shock_effect , red_flagged,classify,Alarm ,Normal ,Alert)
  # 
  # red_flagging = red_flagging%>%left_join(variable_scores , by = c("reporting_month","Member_name","FMS","Region_name","District_Name","Residence_type"))%>%
  #   mutate(
  #     maximum_results = ifelse(District_Name %in% reverine_districts , 57.6, 55.2),
  #     variable_score_percentage = round((variable_score /maximum_results )*100,1),
  #     strategy = "old"
  #     )%>%select(-maximum_results)
  # 
  # red_flagging%>%as.data.frame()%>%write.xlsx("old red-flaging.xlsx")
  # red_flagging_new%>%as.data.frame()%>%write.xlsx("new red-flaging.xlsx")
  
  
  red_flagging_new <- full_join(shock_occurence , shock_effect ,by = c("reporting_month","Member_name","FMS","Region_name","District_Name","Community_name","Residence_type")) %>%
    mutate(
           Alarm = Alarm.x + Alarm.y,
           Alert = Alert.x + Alert.y,
           Normal= Normal.x + Normal.y
    )%>%
    select(reporting_month,Member_name  , FMS,Region_name , District_Name,Community_name,Residence_type , shock_occurence , shock_effect ,classify,Alarm ,Normal ,Alert)%>%
    left_join(variable_scores , by = c("reporting_month","Member_name","FMS","Region_name","District_Name","Community_name","Residence_type"))%>%
    mutate(
      maximum_results = ifelse(District_Name %in% reverine_districts , 57.6, 55.2),
      variable_score_percentage = round((variable_score / maximum_results )*100,1),
      strategy = "new",
      classify = case_when(
        variable_score_percentage <= 24 ~ "Normal",
        variable_score_percentage > 24 & variable_score_percentage <= 49 ~ "Alert",
        variable_score_percentage > 49 ~ "Alarm"
      ),
      red_flagged = ifelse(variable_score_percentage> 49,"Yes","No")
    )%>%select(-maximum_results)%>%as.data.frame()
  
  dominant_hazard <- ewea%>%
    group_by(reporting_month,Member_name,FMS,Region_name,District_Name,Community_name,Residence_type)%>%
    summarise(
      alarm_dominant_hazard = paste0(variable_desc[status == "Alarm"],'-Alarm', collapse = ","),
      alert_dominant_hazard = paste0(head(variable_desc[status == "Alert"], 3),"-Alert", collapse = ","),
      .groups = "drop"
    )%>%
    mutate(
      dominant_hazard = paste(alarm_dominant_hazard,alert_dominant_hazard, sep = ",")
    )%>%
    select(-alarm_dominant_hazard , -alert_dominant_hazard)
  
  red_flagging_new%<>%
    left_join(dominant_hazard)
  # red_flagging_new%>%
  #   filter(reporting_month=="2025-04" & Residence_type=="Rural")%>%select(District_Name)%>%distinct(District_Name)
 
  red_flagging = red_flagging_new
  
  
  # ewea%>%
  #   filter(Community_name == 'Shimbiroole_East' & reporting_month=="2025-08")%>%view()
  # 
  
  # red_flagging%>%
  #   filter(reporting_month=="2025-04" & Residence_type=="Rural")%>%select(District_Name)
  # ewea_raw%>%
  #   filter(reporting_month=="2025-04" & Residence_type=="Rural")%>%select(District_Name)
  
  # ewea%>%select(reporting_month , Member_name , District_Name,Community_name, Residence_type)%>%
  #   filter(Member_name == "IRC")
  
  ewea$reporting_month <- format(ewea$reporting_month, "%Y-%m")
  shock_occurence$reporting_month <-format(shock_occurence$reporting_month, "%Y-%m")
  shock_effect$reporting_month <-format(shock_effect$reporting_month, "%Y-%m")
  red_flagging$reporting_month <-format(red_flagging$reporting_month, "%Y-%m")
  
  
  
  postgresConnection <- dbConnect(RPostgres::Postgres(),
                                  dbname = "brcisShiny",
                                  host = "brcisproddb.postgres.database.azure.com", port = 5432,
                                  user = "brcisShiny", password = "brcisShiny@112233@",
                                  sslmode = 'require')
  

  
  
  createTable(postgresConnection,"ewea_community", ewea)
  createTable(postgresConnection,"shock_occurence_community", shock_occurence)
  createTable(postgresConnection,"shock_effect_community", shock_effect)
  createTable(postgresConnection,"red_flagging_community", red_flagging)
  createTable(postgresConnection,"market_threshold_community", market_thresholds_update)
  createTable(postgresConnection,"ewea_weighting_scores_community", weighting_Scores)
  
  
  dbDisconnect(postgresConnection)
  

  

  
  return(
    "success"
  )
  
}
