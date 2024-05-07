homeUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("homeUI"))
}

home <- function(input,output,session,sharedValues){
  output$homeUI <- renderUI({
    h1("this is home")
  })
}