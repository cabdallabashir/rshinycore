eweaHomeUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("eweaUI"))
}

eweaHome <- function(input,output,session,sharedValues){
  output$eweaUI <- renderUI({
    htmlTemplate("views/ewea.html")
  })
}