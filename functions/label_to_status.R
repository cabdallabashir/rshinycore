label_to_status <- function(data,variable,new_column){

  
    if(variable == "Rainfall_category"){
    data %<>%
      mutate(
        month = lubridate::month(reporting_month),
        {{new_column}} := case_when(
          ((month == 10 | month == 11 | month == 4| month == 5 | month == 6) & get(!!variable) == "No rain received") ~ "Alarm",
          ((month == 10 | month == 11 | month == 4| month == 5 | month == 6) & get(!!variable) == "Poor-Below Average Rainfall") ~ "Alert",
          .default = "Normal"
        )
      )
    return(
      data
    )
  }
  
  if(variable == "coping_strategies"){
    data %<>%
      mutate(
       
        {{new_column}} := case_when(
          value == 3 ~ "Alarm",
          value == 2 ~ "Alert",
          .default = "Normal"
        )
      )
    return(
      data
    )
  }
  
  if(variable == "Water_source_dried"){
    data %<>%
      mutate({{new_column}} := case_when(
        Water_source_dried == "No" ~ "Normal",
        Water_source_dried == "Yes" ~ "Alert",
        Water_source_dried == "It has already dried up."~ "Alarm",
        .default = "Normal"))
    return(
      data
    )
  }
  
  if(variable == "River_levels"){
      data %<>%
      mutate({{new_column}} := case_when(
        River_levels == "Normal for this time of the year" ~ "Normal",
        River_levels %in% c("Below Normal","Above Normal") ~ "Alert",
        River_levels %in% c("River Completely dry","River Overflowing","River bamk full") ~ "Alarm",
        .default = NA))
    return(
      data
    )
  }
  
  if(variable == "locust_infest"){
    data %<>%
      mutate({{new_column}} := case_when(
        locust_infest == "No" ~ "Normal",
        locust_infest == "Yes" & spent_days <= 2 ~ "Alert",
        locust_infest == "Yes" & spent_days > 2 ~ "Alarm",
        .default = NA))
    return(
      data
    )
  }
  
  if(variable == "Flash_flood"){
    data %<>% 
      mutate({{new_column}} := case_when(
      value == "No"~"Normal",
      value == "Yes" & (Flash_flood_days >2 & Flash_flood_days <= 4) ~"Alert",
      value == "Yes" & Flash_flood_days >4  ~"Alarm",
      .default =NA))
    return(
      data
    )
  }
  
  
  if(variable == "awd_cases"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) == "0 Case" ~ "Normal",
        get(!!variable) == "1 case" ~ "Alert",
        get(!!variable) == "Double the average number of cases from the previous two weeks"  ~ "Alarm",
        .default = "Alert"
      )) 
    return(data)
    
    
  }
  
  if(variable == "conflict_score"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) >=0 & get(!!variable) <= 33 ~ "Normal",
        get(!!variable) >=34 & get(!!variable) <= 66 ~ "Alert",
        get(!!variable) >=67  ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  if(variable == "HH_migrated"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) <=50  ~ "Normal",
        get(!!variable) > 50 & get(!!variable) <= 500  ~ "Alert",
        get(!!variable) >500  ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  if(variable == "HH_arrived"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) <=50   ~ "Normal",
        get(!!variable) > 50 & get(!!variable) <= 500  ~ "Alert",
        get(!!variable) >500  ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  if(variable == "Catchment_status"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) == 1  ~ "Normal",
        get(!!variable) == 2  ~ "Alert",
        get(!!variable) == 3  ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  if(variable == "crop_condition"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) %in% c(3,4)  ~ "Normal",
        get(!!variable)  %in% c(1,2)  ~ "Alert",
        get(!!variable) == 0   ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  if(variable == "Pasture_condition"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) == 1  ~ "Normal",
        get(!!variable) == 2  ~ "Alert",
        get(!!variable) == 3 & Pasture_longevity <= 2  ~ "Alarm" ,
        get(!!variable) == 3 & Pasture_longevity > 2  ~ "Alert" ,
        .default = NA
      )) 
    return(data)
    
  }
  
  if(variable == "Cattle"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) %in% c(3,4)  ~ "Normal",
        get(!!variable)  %in% c(1,2)  ~ "Alert",
        get(!!variable) %in% c(0,5)   ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
 
  if(variable == "Goat_sheep"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) %in% c(3,4)  ~ "Normal",
        get(!!variable)  %in% c(1,2)  ~ "Alert",
        get(!!variable) %in% c(0,5)   ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  if(variable == "Camel"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) %in% c(3,4)  ~ "Normal",
        get(!!variable)  %in% c(1,2)  ~ "Alert",
        get(!!variable) %in% c(0,5)   ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  coping_strategies <- data.frame(
    id = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
    strategy = c("Reducing Number of meals/Quantity of food eaten",
                "Selling household Assets/goods",
                "Purchasing food on credit or borrowed food",
                "Spending Savings",
                "Borrow Money",
                "Some household members migrating to a new area",
                "Fetch water from Distance water ponds (War)",
                "Selling productive assests or means of transport",
                "Consuming seed stocks that are to be saved for next season",
                "Withdrawing children from school",
                "Selling house of land",
                "Migration of entire household to a new area",
                "Minimize household water consumption by limiting water use to essential needs, (drinking and cooking)",
                "Wait for water trucking assistance from aid organizations"),
    type = c("alert","alert","alert","alert","alert","alert","alert",
             "alerm","alerm","alerm","alerm","alerm","alerm","alerm")
  )
  
  
  
  
  if(variable == "coping"){
    
    data%<>%
      mutate({{new_column}} := case_when(
        get(!!variable) %in% c(3,4)  ~ "Normal",
        get(!!variable)  %in% c(1,2)  ~ "Alert",
        get(!!variable) == 0   ~ "Alarm",
        .default = NA
      )) 
    return(data)
    
  }
  
  
}