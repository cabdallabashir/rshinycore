ewea <- readTable("ewea") %>% mutate(reporting_month = as.yearmon(reporting_month))


insert_line_breaks <- function(s) {
  s <- "This is a very long category"
  words <- unlist(strsplit(s, " "))
  for (i in seq(3, length(words), 3)) {
    words[i] <- paste0(words[i], "<br>")
  }
  paste(words, collapse = " ")
}

generateData <- function(from_date , to_date , districtControl,data_variables){
  data_variables$ewea <- readTable("ewea") %>% 
    filter((as.Date(as.yearmon(reporting_month)) >= as.Date(as.yearmon(from_date)) 
            & as.Date(as.yearmon(reporting_month)) <= as.Date(as.yearmon(to_date)))
           & district_name == as.character(districtControl)) %>%
    mutate(reporting_month = ifelse(
      !is.na(reporting_month) & reporting_month != "" , 
      as.yearmon(reporting_month),
      as.yearmon(NA)
    ))
    
    
}

can_convert_to_numeric <- function(vec) {
  tryCatch({
    as.numeric(vec)  # attempts to convert vec to numeric
    return(TRUE)
  }, warning = function(w) {
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}


districtUI <- function(id){
  ns <- NS(id)
  htmlTemplate("views/district.html",
               fromDateController =  selectInput(ns("fromdatecontroller"), 
                                                label = "From Date",
                                                choices = unique(sort(as.yearmon(ewea$reporting_month))),
                                                selected = min(as.yearmon(ewea$reporting_month))),
               toDateController =   selectInput(ns("todatecontroller"), 
                                                label = "To Date",
                                                choices = unique(sort(as.yearmon(ewea$reporting_month))),
                                                selected = max(as.yearmon(ewea$reporting_month))),
               districtDropDown = selectInput(ns("dropdownControllDIstrcit"), 
                                              label = "Select Variable",
                                              choices = unique(ewea$district_name)),
               # generateButton = actionButton(ns("generateData"),"Generate",class="btn btn-primary"),
              
               variableTypeAnalysis = DTOutput(ns("variableTypeAnalysis")) %>% withSpinner(),
               variableTypeAnalysisChart = plotlyOutput(ns("variableTypeAnalysisChart")) %>% withSpinner(),
               indicatorAnalysis = DTOutput(ns("indicatorAnalysis")) %>% withSpinner(),
               indicatorAnalysisChart = plotlyOutput(ns("indicatorAnalysisChart")) %>% withSpinner(),
               variableAnalysis =  DTOutput(ns("variableAnalysis")) %>% withSpinner(),
               variableAnalysisChart = plotlyOutput(ns("variableAnalysisChart")) %>% withSpinner(),
               )
  
}

district <- function(input,output,session,sharedValues){
  
  color_mapping <- setNames(c("#F99D1E", "#008BA8", "#ED7667"), c("Alert", "Normal", "Alarm"))
  get_color <- function(classify) {
    return(color_mapping[classify])
  }
  
  data_variables <- reactiveValues(
    ewea = NULL,
    fromDateControl = min(as.yearmon(ewea$reporting_month)),
    toDateControl = max(as.yearmon(ewea$reporting_month)),
    districtControl = first(unique(ewea$district_name))
  )
  
  # observeEvent(input$generateData, {
  #   
  #   req(input$fromdatecontroller,input$todatecontroller,input$dropdownControllDIstrcit)
  #   data_variables$fromDateControl = input$fromdatecontroller
  #   data_variables$toDateControl =input$todatecontroller
  #   data_variables$districtControl = input$dropdownControllDIstrcit
  #   
  # })
  
  
  observe({
    req(input$fromdatecontroller,input$todatecontroller,input$dropdownControllDIstrcit)
    data_variables$fromDateControl = input$fromdatecontroller
    data_variables$toDateControl =input$todatecontroller
    data_variables$districtControl = input$dropdownControllDIstrcit
    generateData(data_variables$fromDateControl, data_variables$toDateControl,data_variables$districtControl, data_variables)
    
    output$variableTypeAnalysis <- renderDT({
      summaryTable <-data_variables$ewea %>%
        group_by(variable_type , status) %>%
        reframe(
          count = n()
        ) %>%
        pivot_wider(names_from = status , values_from = count)
      
      datatable(
        summaryTable,
       
        options = list(
          pageLength = 20, 
          
          dom = 't'
          
        ))
      
     
    })
    
    output$variableTypeAnalysisChart <- renderPlotly({
      summaryTable <-data_variables$ewea %>%
        group_by(variable_type , status) %>%
        reframe(
          count = n()
        )
      
      ggplotly(
        ggplot(summaryTable, aes(x=variable_type, y=count)) +
          geom_bar(aes(fill = status,), stat = "identity", position = "dodge") +
          geom_text(aes(label = count, group = status),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25)+
          scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667"))+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      
    })
    
    output$indicatorAnalysis <- renderDT({
      summaryTable <-data_variables$ewea %>%
        group_by(indicator , status) %>%
        reframe(
          variable_type = first(variable_type),
          count = n()
        ) %>%
        pivot_wider(names_from = status , values_from = count)
      
      datatable(
        summaryTable,
        
        options = list(
          pageLength = 20, 
          
          dom = 't'
          
        ))
      
      
    })
    
    output$indicatorAnalysisChart <- renderPlotly({
      summaryTable <-data_variables$ewea %>%
        group_by(indicator , status) %>%
        reframe(
          count = n()
        )
      
      ggplotly(
        ggplot(summaryTable, aes(x=indicator, y=count)) +
          geom_bar(aes(fill = status,), stat = "identity", position = "dodge") +
          geom_text(aes(label = count, group = status),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25)+
          scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      
    })
    
    #--
    
    output$variableAnalysis <- renderDT({
      summaryTable <-data_variables$ewea %>%
        group_by(variable , status) %>%
        reframe(
          variable_type = first(variable_type),
          indicator = first(indicator),
          count = n()
        ) %>%
        pivot_wider(names_from = status , values_from = count)
      
      datatable(
        summaryTable,
        
        extensions = 'Buttons', 
        options = list(
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = data_variables$districtControl,
              title = data_variables$districtControl,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = data_variables$districtControl,
              title = data_variables$districtControl,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = data_variables$districtControl,
              title = data_variables$districtControl,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Red flagging',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = data_variables$districtControl,
              title = data_variables$districtControl,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ))
      
      
    })
    
    output$variableAnalysisChart <- renderPlotly({
      summaryTable <-data_variables$ewea %>%
        group_by(variable , status) %>%
        reframe(
          count = n()
        )
      
      ggplotly(
        ggplot(summaryTable, aes(x=variable, y=count)) +
          geom_bar(aes(fill = status,), stat = "identity", position = "dodge") +
          geom_text(aes(label = count, group = status),
                    position = position_dodge(width = 0.9),
                    vjust = -0.25)+
          scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      
    })
    
    
  })
  
  
  
}