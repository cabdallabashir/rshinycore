ewea <- readTable("ewea") %>% mutate(reporting_month = as.yearmon(reporting_month))


insert_line_breaks <- function(s) {
  s <- "This is a very long category"
  words <- unlist(strsplit(s, " "))
  for (i in seq(3, length(words), 3)) {
    words[i] <- paste0(words[i], "<br>")
  }
  paste(words, collapse = " ")
}

generateData <- function(from_date , to_date , variableCtrl,data_variables){
  data_variables$ewea <- readTable("ewea") %>% 
    filter((as.Date(as.yearmon(reporting_month)) >= as.Date(as.yearmon(from_date)) 
            & as.Date(as.yearmon(reporting_month)) <= as.Date(as.yearmon(to_date)))
           & variable == as.character(variableCtrl)) %>%
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


variableUI <- function(id){
  ns <- NS(id)
  htmlTemplate("views/variable.html",
               fromDateController =  selectInput(ns("fromdatecontroller"), 
                                                label = "From Date",
                                                choices = unique(sort(as.yearmon(ewea$reporting_month))),
                                                selected = min(as.yearmon(ewea$reporting_month))),
               toDateController =   selectInput(ns("todatecontroller"), 
                                                label = "To Date",
                                                choices = unique(sort(as.yearmon(ewea$reporting_month))),
                                                selected = max(as.yearmon(ewea$reporting_month))),
               variableController = selectInput(ns("variableController"), 
                                                label = "Select Variable",
                                                choices = unique(ewea$variable),
                                              ),
               # generateButton = actionButton(ns("generateData"),"Generate",class="btn btn-primary"),
               districtDropDown = selectInput(ns("dropdownControllDIstrcit"), 
                                              label = "Select Variable",
                                              choices = unique(ewea$district_name)),
               districtsbyvariable = plotlyOutput(ns("districtsbyvariable")) %>% withSpinner(),
               datebyvariable = plotlyOutput(ns("datebyvariable")) %>% withSpinner(),
               districtVariableAnalysis = plotlyOutput(ns("districtVariableAnalysis")) %>% withSpinner(),
               variableReportTable = DTOutput(ns("variableReportTable")) %>% withSpinner(),
               )
  
}

variable <- function(input,output,session,sharedValues){
  
  color_mapping <- setNames(c("#F99D1E", "#008BA8", "#ED7667"), c("Alert", "Normal", "Alarm"))
  get_color <- function(classify) {
    return(color_mapping[classify])
  }
  
  data_variables <- reactiveValues(
    ewea = NULL,
    fromDateControl = min(as.yearmon(ewea$reporting_month)),
    toDateControl = max(as.yearmon(ewea$reporting_month)),
    variableCtrl = first(unique(ewea$variable))
  )
  
  # observeEvent(input$generateData, {
  #   
  #   req(input$fromdatecontroller,input$todatecontroller,input$variableController)
  #   data_variables$fromDateControl = input$fromdatecontroller
  #   data_variables$toDateControl =input$todatecontroller
  #   data_variables$variableCtrl = input$variableController
  #   
  # })
  
  
  observe({
    req(input$fromdatecontroller,input$todatecontroller,input$variableController)
    data_variables$fromDateControl = input$fromdatecontroller
    data_variables$toDateControl =input$todatecontroller
    data_variables$variableCtrl = input$variableController
    generateData(data_variables$fromDateControl, data_variables$toDateControl,data_variables$variableCtrl, data_variables)
    
    output$districtsbyvariable <- renderPlotly({
      
    
      if(can_convert_to_numeric(data_variables$ewea$value)){
        
        summary <- data_variables$ewea %>%
          drop_na(value) %>% 
          group_by(district_name ,value,status ) %>%
          reframe(
            value = mean(as.numeric(value)),
            other = first(other),
            district = first(District),
            count = n()
          )
        
        totals <- summary %>%
          group_by(district_name, status) %>%
          summarise(total = round(mean(as.numeric(value)),2),
                    `status count` = sum(count))
        
        summary$hover_text <- with(summary, paste('District Name:', district_name, '<br>',
                                        'Count:', count, '<br>',
                                        paste('Avarage ',status, ":"), round(value,3), '<br>',
                                        'Status:', status))
        
        
        p <- ggplot(totals , aes(label =  `status count`)) +
          geom_line(aes(x = district_name, y = total, group = status), color = "#ED7667") +
          geom_point(aes(x = district_name, y = total, group = status), color = "#008BA8") +
          facet_grid(rows = vars(status),scales = "free") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          guides(fill = guide_legend(title = "")) +
          geom_text(data = totals, aes(x = district_name, y = total, label = total),
                    vjust = -0.5, position = position_dodge(width = 0.9),
                    size=2) +
          labs(
              title = paste("Avarage ",data_variables$variableCtrl," over the districts"),
              x = "",           # Replace with your x-axis label
              y = "Value"           # Replace with your y-axis label
            )
        
        
        ggplotly(p)
        
        
       
      
        
      }else{
          summary <- data_variables$ewea %>%
            drop_na(value) %>%
            group_by(district_name ,value ,status ) %>%
            reframe(
              other = first(other),
              district = first(District),
              count = n(),
            )
          
          totals <- summary %>%
            group_by(district_name, status) %>%
            summarise(total = sum(count))
          
          summary$hover_text <- with(summary, paste('District Name:', district_name, '<br>',
                                          'Count:', count, '<br>',
                                          'Value:', value, '<br>',
                                          'Status:', status))
         
          
          p <- ggplot(summary) +
            geom_bar( stat = "identity",aes(x = district_name,y = count, 
                                            fill= value , label = status)) +
            facet_grid(rows = vars(status),scales = "free") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            guides(fill = guide_legend(title = "")) +
            geom_text(data = totals, aes(x = district_name, y = total, label = total),
                      vjust = -0.5, position = position_dodge(width = 0.9),
                      size = 3)+
            labs(
              title = paste("District Level Analysis of  ",data_variables$variableCtrl),
              x = "",           # Replace with your x-axis label
              y = "Value"           # Replace with your y-axis label
            )
          # +
          #   geom_line(data = totals , aes(x = district_name, y = total, group = status), color = "blue")
          
          ggplotly(p)
        
      }
    })
    
    output$datebyvariable <- renderPlotly({
      
      
      if(can_convert_to_numeric(data_variables$ewea$value)){
        
        summary <- data_variables$ewea %>%
          drop_na(value) %>% 
          group_by(reporting_month ,value,status ) %>%
          reframe(
            value = mean(as.numeric(value)),
            other = first(other),
            district = first(District),
            count = n()
          )%>%arrange(as.yearmon(reporting_month))%>%
          mutate(reporting_month = as.character(as.yearmon(reporting_month)))
        
        summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
        
        
        totals <- summary %>%
          group_by(reporting_month, status) %>%
          summarise(total = round(mean(as.numeric(value)),2),
                    `status count` = sum(count))
        
        summary$hover_text <- with(summary, paste('District Name:', district, '<br>',
                                                  'Count:', count, '<br>',
                                                  paste('Avarage ',status, ":"), round(value,3), '<br>',
                                                  'Status:', status))
        
        
        p <- ggplot(totals) +
          geom_line(aes(x = reporting_month, y = total, group = status), color = "#ED7667") +
          geom_point(aes(x = reporting_month, y = total, group = status), color = "#008BA8") +
          facet_grid(rows = vars(status),scales = "free") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          guides(fill = guide_legend(title = "")) +
          geom_text(data = totals, aes(x = reporting_month, y = total, label = total), 
                    vjust = -0.5, position = position_dodge(width = 0.9),
                    size = 2) +
          labs(
            title = paste("Avarage",data_variables$variableCtrl,"over the districts"),
            x = "",           # Replace with your x-axis label
            y = "Value"           # Replace with your y-axis label
          )
        
        
        ggplotly(p)
        
        
        
        
        
      }else{
        summary <- data_variables$ewea %>%
          drop_na(value) %>%
          group_by(reporting_month ,value ,status ) %>%
          reframe(
            other = first(other),
            district = first(District),
            count = n(),
          )%>%arrange(as.yearmon(reporting_month))%>%
          mutate(reporting_month = as.character(as.yearmon(reporting_month)))
        
        summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
        
        totals <- summary %>%
          group_by(reporting_month, status) %>%
          summarise(total = sum(count))
        
        summary$hover_text <- with(summary, paste('District Name:', district, '<br>',
                                                  'Count:', count, '<br>',
                                                  'Value:', value, '<br>',
                                                  'Status:', status))
        
        
        p <- ggplot(summary) +
          geom_bar( stat = "identity",aes(x = reporting_month,y = count, 
                                          fill= value , label = status)) +
          facet_grid(rows = vars(status),scales = "free") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          guides(fill = guide_legend(title = "")) +
          geom_text(data = totals, aes(x = reporting_month, y = total, label = total), 
                    vjust = -0.5, position = position_dodge(width = 0.9),
                    size = 3) +
          labs(
            title = paste("Monthly Analysis of",data_variables$variableCtrl),
            x = "",           # Replace with your x-axis label
            y = "Value"           # Replace with your y-axis label
          )
        
        # +
        #   geom_line(data = totals , aes(x = reporting_month, y = total, group = status), color = "blue")
        
        ggplotly(p)
        
      }
      
      
      
      
      
      
    })
    
    output$variableReportTable <- renderDT(server = FALSE,{
      summary <- data_variables$ewea %>%
        mutate(reporting_month = as.character(as.yearmon(reporting_month))) %>%
        select(reporting_month,FMS ,District ,variable,value ,status ,indicator ,variable_type ,other )
      
      datatable(
        summary,
        extensions = 'Buttons', 
        options = list(
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = data_variables$variableCtrl,
              title = data_variables$variableCtrl,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = data_variables$variableCtrl,
              title = data_variables$variableCtrl,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = data_variables$variableCtrl,
              title = data_variables$variableCtrl,
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
              filename = data_variables$variableCtrl,
              title = data_variables$variableCtrl,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ),
        filter = 'top',class = "variableReportTableID")
    })
    
    
    

  })
  
  observeEvent(input$dropdownControllDIstrcit,{
    output$districtVariableAnalysis <- renderPlotly({
      if(can_convert_to_numeric(data_variables$ewea$value)){
        tryCatch({
        summary <- data_variables$ewea %>%
          filter(district_name == input$dropdownControllDIstrcit) %>%
          drop_na(value) %>%
          group_by(reporting_month ,status ) %>%
          reframe(
            value = as.numeric(value),
            other = first(other),
            district = first(District),
            count = n()
            
          )%>%
          arrange(as.yearmon(reporting_month))%>%
          mutate(reporting_month = as.character(as.yearmon(reporting_month)))%>%
          as.data.frame()
        
        title <- ifelse(is.na(first(summary$other)) || first(summary$other) == "" || nrow(summary) == 0 || ncol(summary) == 0,
                        data_variables$variableCtrl ,
                        sub(",","<br>",first(summary$other)))
        
      
        summary$hover_text <- with(summary, paste('District Name:', district, '<br>',
                                                  'Count:', count, '<br>',
                                                  'Status:', status))
       
        
          alarm <- min(
            as.numeric(summary[summary$status == "Alarm","value"])
          )
          
          alert <- min(
            as.numeric(summary[summary$status == "Alert","value"])
          )
          
          normal <- min(
            as.numeric(summary[summary$status == "Normal","value"])
          )
        
          # fig <- plot_ly(summary, x = ~reporting_month, y = ~value, type = 'scatter', mode = 'lines+markers+text',
          #                line = list(shape = "linear"),
          #                marker = list(size = 10,color = ~get_color(status)),
          #                text = ~value, textposition = "top center", # Add labels from the 'status' column
          #                hoverinfo = ~hover_text,
          #                fill = ~status) %>%
          #  
          #   layout(
          #     title = paste("Monthly ",title," at ", input$dropdownControllDIstrcit),
          #     
          #     xaxis = list(categoryorder = "array",
          #                  categoryarray = ~reporting_month)
          #   )
          #  
          # fig
          
          ggplotly(
                     ggplot(summary,aes(x = reporting_month, y = value)) +
                    
                     geom_line(aes(group = 1),color="#008BA8") + 
                     geom_point(aes(color = status)) + 
                     geom_text(aes(x = reporting_month, y = value, label = value),
                               vjust = -0.5, position = position_dodge(width = 0.9))+
                     theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          
                           plot.background = element_rect(fill = "white", colour = "white")) +
                     labs(
                       title = paste("Monthly ",title," at ", input$dropdownControllDIstrcit),
                       x = "",           # Replace with your x-axis label
                       y = "Value"           # Replace with your y-axis label
                     )
            )
        }, error = function(e) {
          message("Not able to visualize data")
        })
        
      }else{
      
        trunc <- function(s) {

            return(
              substr(s, 1, 20)
            )
        }
        tryCatch({
        summary <- data_variables$ewea %>%
          filter(district_name == input$dropdownControllDIstrcit) %>%
          drop_na(value) %>%
          group_by(reporting_month ,value ) %>%
          reframe(
            status = first(status),
            other = first(other),
            district = first(District),
            count = n()
          )%>%
          arrange(as.yearmon(reporting_month))%>%
          mutate(reporting_month = as.character(as.yearmon(reporting_month))
                 )%>%
          as.data.frame()
        
        summary$truncated <- sapply(summary$value , trunc)
        summary$hover_text <- with(summary, paste('District Name:', district, '<br>',
                                                  'Count:', count, '<br>',
                                                  'Status:', status))
        
        title <- sub("_"," ",data_variables$variableCtrl)
        
      
       
          p <- ggplot(summary, aes(x = reporting_month, y = truncated , label = value , text=hover_text)) +
            geom_tile(width = 0.8, height = 0.5 , aes(fill = status)) +
            geom_line(aes(x= reporting_month , y=truncated ,group =reporting_month) ,color = "#40419A") +
            labs(title = paste("Monthly ",title," at ", input$dropdownControllDIstrcit), x = "Month", y = "") +
            scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +  # Custom colors for each status
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # Adjust text angle for better readability
            labs(
              title = paste("Monthly ",title," at ", input$dropdownControllDIstrcit),
              x = "",           # Replace with your x-axis label
              y = "Value"           # Replace with your y-axis label
            )
          
          ggplotly(p)
        }, error = function(e) {
          message("Not able to visualize data")
        })
        
        
      }
    })
  })
  
  
}