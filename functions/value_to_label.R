value_to_label <- function(data,variable){
  
  ## rainfall
  if(variable=="Rainfall_category"){
    
    data%<>%
      mutate({{variable}} :=  case_when(
        get(!!variable) == "1"	~ "Average-Nomal Rainfall",
        get(!!variable) == "2" ~	"Poor-Below Average Rainfall",
        get(!!variable) == "3"	~ "No rain received",
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if (variable=="coping"){

    
    data%<>%
      mutate({{variable}} :=  case_when(
        get(!!variable) == 1	~ "Reducing Number of meals/Quantity of food eaten",
        get(!!variable) == 3	~ "Purchasing food on credit or borrowed food", 
        get(!!variable) == 4	~ "Spending Savings",
        get(!!variable) == 5	~ "Borrow Money from saving groups",
        get(!!variable) == 8	~ "Selling Livestock including breeding animals or means of transport",
        get(!!variable) == 9	~ "Consuming seed stocks that are to be saved for next season",
        get(!!variable) == 10	~ "Withdrawing children from school",
        get(!!variable) == 11	~ "Selling house of land",
        get(!!variable) == 12	~ "Migration of entire household to a new area",
        get(!!variable) == 13	~ "Minimize household water consumption by limiting water use to essential needs, (drinking and cooking)",
        get(!!variable) == 14	~ "Wait for water trucking assistance from aid organizations",
        get(!!variable) == 15	~ "Engaging in alternative income generating activities",
        get(!!variable) == 16	~ "Seasonal livestock migration to find pasture and water",
        get(!!variable) == 17	~ "Temporary migration of family members to find work and send remittances home",
        get(!!variable) == 18	~ "Selling of non-essential assets or goods items (e.g.) furniture",
        get(!!variable) == 19	~ "Water conservation and rationing",
        get(!!variable) == 20	~ "Selling weak livestock first during a crisis to buy food",
        get(!!variable) == 21	~ "Taking high interest loans from traders",
        get(!!variable) == 96	~ "Other",
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if(variable == "Water_source_dried"){
    
    data%<>%
      mutate({{variable}} :=  case_when(
        get(!!variable) == "1"	~ "Yes",
        get(!!variable) == "2" ~	"No",
        get(!!variable) == "3"	~ "It has already dried up.",
        get(!!variable) == "98" ~	NA,
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if(variable == "River_levels"){

    
    data%<>%
      mutate({{variable}} :=  case_when(
        get(!!variable) == "1" ~ "Normal for this time of the year",
        get(!!variable) == "2" ~ "Below Normal", 
        get(!!variable) == "5" ~ "Above Normal", 
        get(!!variable) == "3" ~ "River Completely dry",
        get(!!variable) == "6" ~ "River Overflowing",
        get(!!variable) == "7" ~ "River bamk full",
        get(!!variable) == "4" ~ "Not Applicable",
        .default = NA
      ))
    
    return(data)
    
  }else if(variable == "Flash_flood" | variable == "locust_infest"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "Yes",
        get(!!variable) == "2" ~ "No",
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if(variable == "Catchment_status"){
    
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "More than half-full (75%) or full",
        get(!!variable) == "2" ~ "Half-full  ( 50%)",
        get(!!variable) == "3" ~ "Less than half-full (25%) /empty",
        get(!!variable) == "98" ~ NA,
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if(variable == "crop_condition"){
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "0" ~ "Failure",
        get(!!variable) == "1" ~ "Very poor",
        get(!!variable) == "2" ~ "Poor",
        get(!!variable)==  "3" ~ "Fair",
        get(!!variable) == "4" ~ "Very good",
        .default = get(!!variable)
      ))
    
    return(data)
  }else if(variable == "Pasture_longevity"){

    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "No pasture remaining",
        get(!!variable) == "2" ~ "For one month",
        get(!!variable) == "3" ~ "For two months",
        get(!!variable)== "4" ~ "For three months",
        get(!!variable)== "5" ~ "For more than three months",
        get(!!variable) == "98" ~ NA,
        .default = get(!!variable)
      ))
    return(data)
  }else if(variable == "Pasture_condition"){
   
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "Normal/Good replenishment",
        get(!!variable) == "2" ~ "Below normal",
        get(!!variable) == "3" ~ "No replenishment yet",
        .default = get(!!variable)
      ))  
    
  }else if(variable == "Cattle"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "0" ~ "Very poor",
        get(!!variable) == "1" ~ "poor",
        get(!!variable) == "2" ~ "Fair",
        get(!!variable)==  "3" ~ "good",
        get(!!variable) == "4" ~ "Very good",
        get(!!variable) == "5" ~ "Failure",
        .default = get(!!variable)
      )) 
    return(data)
    
    
  }else if(variable == "Goat_sheep"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "0" ~ "Very poor",
        get(!!variable) == "1" ~ "poor",
        get(!!variable) == "2" ~ "Fair",
        get(!!variable)==  "3" ~ "good",
        get(!!variable) == "4" ~ "Very good",
        get(!!variable) == "5" ~ "Failure",
        .default = get(!!variable)
      )) 
    return(data)
    
    
  }else if(variable == "Camel"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "0" ~ "Very poor",
        get(!!variable) == "1" ~ "poor",
        get(!!variable) == "2" ~ "Fair",
        get(!!variable)==  "3" ~ "good",
        get(!!variable) == "4" ~ "Very good",
        get(!!variable) == "5" ~ "Failure",
        .default = get(!!variable)
      )) 
    return(data)
    
    
  }else if(variable == "Outbreak_deaths"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "Normal Animal mortality",
        get(!!variable) == "2" ~ "Higher than normal animal mortality",
        get(!!variable) == "3" ~ "Very high animal mortality/uncontrolled outbreak",
        .default = get(!!variable)
      )) 
    return(data)
    
    
  }else if(variable == "awd_cases"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == 0 ~ "0 Case",
        get(!!variable) == 1 ~ "1 case",
        get(!!variable) >= 2*awd_cases_previous_week  ~ "Double the average number of cases from the previous two weeks",
        .default = paste(get(!!variable) ,"Cases reported")
      )) 
    return(data)
    
    
  }else{
    
    
    return(data)
  }
  
  
  
}
