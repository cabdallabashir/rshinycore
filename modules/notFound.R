notFoundUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("notFoundUI"))
}

notFound <- function(input,output,session,sharedValues){
  output$notFoundUI <- renderUI({
    htmlTemplate("views/404.html")
  })
}