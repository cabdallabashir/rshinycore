label_to_value <- function(data , variable){
  
  if (variable=="coping_strategies"){
    data %<>% 
      mutate(
        {{variable}} := case_when(
      coping_strategies == "Selling productive capital assets or means of transport (sewing machine, wheelbarrow, bicycle, car, etc.)"~3,
      coping_strategies == "Consuming seed stocks that were to be held/saved for the next season" ~ 3,
      coping_strategies ==  "Withdrawing children from school"~ 3,
      coping_strategies == "Selling house or land"~ 3,
      coping_strategies == "Selling last female animals"~ 3,
      coping_strategies == "Entire households migrating to a new area"~ 3,
      coping_strategies == "Reducing number of meals/ quantity of food eaten"~ 2,
      coping_strategies == "Selling household assets/goods (non-capitals-seeds, animals)"~ 2,
      coping_strategies == "Purchasing food on credit or borrowed food"~ 2,
      coping_strategies ==  "Spending Savings"~ 2,
      coping_strategies == "Borrowing money"~ 2,
      coping_strategies==  "Some household members migrating to a new area"~ 2,
      .default = 1)
    )
    return(
      data
    )
  }
  
}