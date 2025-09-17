server <- function(input , output, session){
  vals <- reactiveValues(
    selected_tab_new = NULL,
    selected_tab_old = NULL
  )
  
  sharedValues <- NULL
  
  
  observeEvent(input$currentHash,{
    hashValue <- input$currentHash
    
    if(is.null(hashValue) || hashValue =="" ){
      hashValue = "baseline_home"
    }
    
    if(hashValue == "ittdash"){
      output$headerLayout <- renderUI({
        htmlTemplate("views/layout/header_itt.html")
      })
      
      output$footerLayout <- renderUI({
        htmlTemplate("views/layout/footer_itt.html")
      })
    }else{
      output$headerLayout <- renderUI({
        htmlTemplate("views/layout/header.html")
      })
      
      output$footerLayout <- renderUI({
        htmlTemplate("views/layout/footer.html")
      })
    }
    
   
    if(!(hashValue %in% allowedURL)){
      hashValue = "notFound"
    }
    source(paste0("modules/", hashValue, ".R"), local = FALSE)
    vals$selected_tab_new <- hashValue
   
    
    if(!is.null(vals$selected_tab_old)) {
      
      if (exists(vals$selected_tab_old, where = globalenv())) {
        rm(list = vals$selected_tab_old, pos = globalenv())
      }
      if (exists(paste0(vals$selected_tab_old, "UI"), where = globalenv())) {
        rm(list = paste0(vals$selected_tab_old, "UI"), pos = globalenv())
      }
      # rm(list = c(vals$selected_tab_old, paste0(vals$selected_tab_old, "UI")), pos = globalenv())
    }
    
    vals$selected_tab_old <- hashValue
  })
 
  observe({
    if (!is.null(vals$selected_tab_new)) {
      
     
      
      output$mainOutputLayout <- renderUI({
        rlang::as_function(paste0(vals$selected_tab_new, "UI"))(vals$selected_tab_new)
      })
      
      
      
      callModule(rlang::as_function(vals$selected_tab_new), id = vals$selected_tab_new,sharedValues=sharedValues)
    }
  })
}