value_to_label <- function(data,variable){
  
  ## rainfall
  if(variable=="Rainfall_category"){
    
    data%<>%
      mutate({{variable}} :=  case_when(
        get(!!variable) == "1"	~ "No rain received",
        get(!!variable) == "2" ~	"Poor/ below average",
        get(!!variable) == "3"	~ "Average/normal",
        get(!!variable) == "4" ~	"Good/ above average",
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if (variable=="coping_strategies"){
    
    data%<>%
      mutate({{variable}} :=  case_when(
        get(!!variable) == "1"	~ "Reducing number of meals/ quantity of food eaten",
        get(!!variable) == "2"	~ "Selling household assets/goods (non-capitals-seeds, animals)",
        get(!!variable) == "3"	~ "Purchasing food on credit or borrowed food", 
        get(!!variable) == "4"	~ "Spending Savings",
        get(!!variable) == "5"	~ "Borrowing money",
        get(!!variable) == "6"	~ "Selling productive capital assets or means of transport (sewing machine, wheelbarrow, bicycle, car, etc.)",
        get(!!variable) == "7"	~ "Consuming seed stocks that were to be held/saved for the next season",
        get(!!variable) == "8"	~ "Withdrawing children from school",
        get(!!variable) == "9"	~ "Selling house or land",
        get(!!variable) == "10"	~ "Selling last female animals",
        get(!!variable) == "11"	~ "Some household members migrating to a new area",
        get(!!variable) == "12"	~ "Entire households migrating to a new area",
        get(!!variable) == "96"	~ "Other",
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
    
  }else if(variable == "River"){
    
    data%<>%
      mutate({{variable}} :=  case_when(
        get(!!variable) == "1" ~ "River completely dry",
        get(!!variable) == "2" ~ "River level below normal for this time of the season", 
        get(!!variable) == "3" ~ "River level normal for this time of the season",
        get(!!variable) == "4" ~ "River level above normal for this time of the season",
        get(!!variable)== "5" ~ "River is overflowing or at high risk of overflowing",
        get(!!variable) == "6" ~ "Not applicable to this area",
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if(variable == "flashfloods" | variable == "locust_infest"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "Yes",
        get(!!variable) == "2" ~ "No",
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if(variable == "Cattle condition"){
    
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "Extremely weak",
        get(!!variable) == "2" ~ "Weak",
        get(!!variable) == "3" ~ "Average",
        get(!!variable)== "4" ~ "Fat/heavy",
        get(!!variable) == "98" ~ NA,
        .default = get(!!variable)
      ))
    
    return(data)
    
  }else if(variable == "Sheep/goat condition"){
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "Extremely weak",
        get(!!variable) == "2" ~ "Weak",
        get(!!variable) == "3" ~ "Average",
        get(!!variable)== "4" ~ "Fat/heavy",
        get(!!variable) == "98" ~ NA,
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
  }else if(variable == "crop_impact"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "No impact or little impact",
        get(!!variable) == "2" ~ "Less than a third of crops damaged by wilting/floods/pests",
        get(!!variable) == "3" ~ "more than a third of crops damaged by wilting/floods/pests",
        .default = get(!!variable)
      ))  
    
  }else if(variable == "Outbreak_deaths"){
    
    data%<>%
      mutate({{variable}} := case_when(
        get(!!variable) == "1" ~ "Normal Animal mortality",
        get(!!variable) == "2" ~ "Higher than normal animal mortality",
        get(!!variable) == "3" ~ "Very high animal mortality/uncontrolled outbreak",
        .default = get(!!variable)
      )) 
    return(data)
    
    
  }else{
    
    
    return(data)
  }
  
  
  
}
