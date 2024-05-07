


target_vs_achievedUI <- function(id){
  ns <- NS(id)
  htmlTemplate("views/target_vs_achieved.html",
               
                 )
  
}

target_vs_achieved <- function(input,output,session,sharedValues){
  
  data_variables <- reactiveValues(
    baseline_targets_copy =NULL,
    fromDateControl = input$fromDateController,
    toDateControl = input$toDateController,
    variableCtrl = input$Segrigation
  )
  
  observe({
    req(input$fromDateController,input$toDateController,input$Segrigation)
    data_variables$fromDateControl = input$fromdatecontroller
    data_variables$toDateControl =input$todatecontroller
    data_variables$variableCtrl = input$Segrigation
    generateData(data_variables$fromDateControl, data_variables$toDateControl, data_variables,sharedValues)
  
    output$random_total_target <- renderUI({
     sum(data_variables$baseline_targets_copy$r_target)
    })
    
    output$random_total_achieved <- renderUI({
      sum(data_variables$baseline_targets_copy$r_achived)
      
    })
    
    output$snowball_total_target <- renderUI({
      sum(data_variables$baseline_targets_copy$s_target)
      
    })
    
    output$snowball_total_achieved <- renderUI({
      sum(data_variables$baseline_targets_copy$s_achived)
      
    })
    
    })
 
}