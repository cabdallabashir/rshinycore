label_to_status <- function(data,variable,new_column){

  if(variable == "Rainfall_category"){
    data %<>%
      mutate(
        month = month(reporting_month),
        {{new_column}} := case_when(
          ((month == 10 | month == 11 | month == 4| month == 5 | month == 6) & get(!!variable) == "No rain received") ~ "Alarm",
          ((month == 10 | month == 11 | month == 4| month == 5 | month == 6) & get(!!variable) == "Poor/ below average") ~ "Alert",
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
  
  if(variable == "River"){
    data %<>%
      mutate({{new_column}} := case_when(
        River == "River level normal for this time of the season" ~ "Normal",
        River == "River level below normal for this time of the season" ~ "Alert",
        River == "River level above normal for this time of the season" ~ "Alert",
        River == "River is overflowing or at high risk of overflowing" ~ "Alarm",
        River == "River completely dry" ~ "Alarm",
        .default = NA))
    return(
      data
    )
  }
  
  if(variable == "flashfloods"){
    data %<>% 
      mutate({{new_column}} := case_when(
      value == "No"~"Normal",
      value == "Yes"~"Alarm",
      .default =NA),
      other = as.character(other),
      number = "",
      variable = "Flash floods")
    return(
      data
    )
  }
  
  if(variable == "Cattle condition"){
    data %<>% 
      mutate({{new_column}} := case_when(
        get(!!variable) == "Fat/heavy" ~ "Normal",
        get(!!variable) == "Average" ~ "Normal",
        get(!!variable) == "Weak" ~ "Alert",
        get(!!variable) == "Extremely weak" ~ "Alarm",
        .default =  NA))
    return(
      data
    )
  }
  
  if(variable == "Sheep/goat condition"){
    data %<>% 
      mutate({{new_column}} := case_when(
        get(!!variable) == "Fat/heavy" ~ "Normal",
        get(!!variable) == "Average"  ~ "Normal",
        get(!!variable) == "Weak"  ~ "Alert",
        get(!!variable) == "Extremely weak"  ~ "Alarm",
        .default =  NA))
    return(
      data
    )
  }
  
  if(variable == "Crop diseases"){
    data %<>% 
      mutate(value = as.numeric(value),
             {{new_column}} := case_when(value <=15 ~ "Normal",
                                value>15 & value<=30 ~ "Alert",
                                value>30 ~ "Alarm",
                                .default = NA))
    return(
      data
    )
  }
  
  if(variable == "Pasture conditions"){
    data %<>% 
      mutate({{new_column}} := case_when(
        (value == "For three months" | value == "For more than three months" | value == "For two months" )~"Normal",
        value == "For one month"~ "Alert",
        value == "No pasture remaining"~"Alarm", 
        .default = NA),
        )
    return(
      data
    )
  }
}