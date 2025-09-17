label_to_value <- function(data , variable){
  
  if (variable=="coping_strategies"){
    data %<>% 
      mutate(
        {{variable}} := case_when(
      coping_strategies %in% c(15,16,17,18,19) ~1,
      coping_strategies %in% c(1,20,3,4,5,13) ~2,
      coping_strategies %in% c(8,9,10,11,12,14,21) ~3,
      .default = 1)
    )
    return(
      data
    )
  }
  
}