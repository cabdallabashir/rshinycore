
ewea <- readTable("ewea") %>% mutate(reporting_month = as.yearmon(reporting_month))



generateData <- function(from_date , to_date,data_variables){
  

    data_variables$shocks <- readTable("shock.occurence") %>% mutate(reporting_month = as.yearmon(reporting_month)) %>%
      filter(as.Date(as.yearmon(reporting_month)) >= as.Date(as.yearmon(from_date)) & as.Date(as.yearmon(reporting_month)) <= as.Date(as.yearmon(to_date)))
    data_variables$shocks.count <-sum(data_variables$shocks$shock.occurence)
    data_variables$shocks.districts <-nrow(as.data.frame(unique(
      data_variables$shocks[data_variables$shocks$shock.occurence == 1,"district_name"]
    )))
    
    data_variables$shocks.effects <- readTable("shock.effect") %>% mutate(reporting_month = as.yearmon(reporting_month)) %>%
      filter(as.Date(as.yearmon(reporting_month)) >= as.Date(as.yearmon(from_date)) & as.Date(as.yearmon(reporting_month)) <= as.Date(as.yearmon(to_date)))
    data_variables$shocks.effects.count <-sum(data_variables$shocks.effects$shock.effect)
    data_variables$shocks.effects.districts <-nrow(as.data.frame(unique(
      data_variables$shocks.effects[data_variables$shocks.effects$shock.effect == 1,"district_name"]
    )))
    
    data_variables$red_flagged <- readTable("red_flagging") %>% mutate(reporting_month = as.yearmon(reporting_month)) %>%
      filter(as.Date(as.yearmon(reporting_month)) >= as.Date(as.yearmon(from_date)) & as.Date(as.yearmon(reporting_month)) <= as.Date(as.yearmon(to_date)))
    data_variables$red_flagged.count <-nrow(data_variables$red_flagged[data_variables$red_flagged$red_flagged == "Yes",])
    data_variables$red_flagged.districts <-nrow(as.data.frame(unique(
      data_variables$red_flagged[data_variables$red_flagged$red_flagged == "Yes","District"]
    )))
    
    data_variables$red_flagged_classify <- data_variables$red_flagged %>% 
      filter(red_flagged == "Yes") %>%
      select(classify , red_flagged ) %>%
      group_by(classify) %>%
      summarise(count = n())
    
    
    data_variables$red_flagged_districts_last_month_status <- data_variables$red_flagged %>% 
      filter(red_flagged == "Yes") %>%
      group_by(District) %>%
      summarise(
        last_clasify = classify[which.max(as.Date(reporting_month))],
        last_month = max(as.Date(reporting_month)),
        .groups = "rowwise"
      )
    
    
    data_variables$red_flagged_district_names_with_other_info <- data_variables$red_flagged %>% 
      filter(red_flagged == "Yes") %>%
      group_by(District,classify) %>%
      summarise(
        count  = n(),
        .groups = "rowwise"
      )%>%
      group_by(District) %>%
      summarise(
        total = sum(count),
        classification = paste(count ,classify , collapse = "," ),
        most_classify = classify[which.max(count)],
        most_clasify_count = max(count),
      ) %>% left_join(data_variables$red_flagged_districts_last_month_status) %>%
      left_join(somalia_districts) %>%sf::st_as_sf()
    
    
    
    
    
    
    data_variables$not_red_flagged.count <-nrow(data_variables$red_flagged[data_variables$red_flagged$red_flagged == "No",])
    data_variables$not_red_flagged.districts <-nrow(as.data.frame(unique(
      data_variables$red_flagged[data_variables$red_flagged$red_flagged == "No","District"]
    )))
    
    
    data_variables$indicator_total = data_variables$red_flagged %>%
      filter(red_flagged == "Yes") %>%
      summarise(Alarm = sum(Alarm),
                Alert = sum(Alert),
                Normal = sum(Normal))

  
  
}

summaryUI <- function(id){
  ns <- NS(id)
  htmlTemplate("views/summary.html",
               total_alarms = uiOutput(ns("total_alarms")) %>% withSpinner(size=0.5,proxy.height = "50px"),
               total_alerts = uiOutput(ns("total_alerts")) %>% withSpinner(size=0.5,proxy.height = "50px"),
               total_normal = uiOutput(ns("total_normal")) %>% withSpinner(size=0.5,proxy.height = "50px"),
               total_shocks = uiOutput(ns("shocks.count")) %>% withSpinner(size=0.5,proxy.height = "50px"),
               total_shock_effects = uiOutput(ns("shocks.effects.count")) %>% withSpinner(size=0.5,proxy.height = "50px"),
               total_districts_shock_occurence =uiOutput(ns("shocks.districts")),
               districts_effected_by_shock = uiOutput(ns("shocks.effects.districts")) ,
               red_flags_districts = uiOutput(ns("red_flagged.districts")) ,
               total_red_flags = uiOutput(ns("red_flagged.count")) %>% withSpinner(size=0.5,proxy.height = "50px"),
               not_red_flags_districts = uiOutput(ns("not_red_flagged.districts")) ,
               total_not_red_flags = uiOutput(ns("not_red_flagged.count")) %>% withSpinner(size=0.5,proxy.height = "50px"),
               RedFalggedDistrictMap = leafletOutput(ns("displayRedFlaggedMap")) %>% withSpinner(),
               displyclassifychart = plotlyOutput(ns("displyclassify")) %>% withSpinner(),
               redFlagBarChartSummary  = plotlyOutput(ns("redFlagBarChartSummary"))%>% withSpinner(),
               redFlagBarChartSummarybyMonth  = plotlyOutput(ns("redFlagBarChartSummarybyMonth"))%>% withSpinner(),
               displayRedFlagTable = DTOutput(ns("displayRedFlagTable"))%>% withSpinner(),
               fromDateController =  selectInput(ns("fromdatecontroller"), 
                                               label = "From Date",
                                              choices = unique(sort(as.yearmon(ewea$reporting_month))),
                                           selected = min(as.yearmon(ewea$reporting_month))),
               toDateController =   selectInput(ns("todatecontroller"), 
                                                label = "To Date",
                                                choices = unique(sort(as.yearmon(ewea$reporting_month))),
                                                selected = max(as.yearmon(ewea$reporting_month))),
               generateButton = actionButton(ns("generateData"),"Refresh",class="btn btn-primary"))
}

summary <- function(input,output,session,sharedValues){
  
  color_mapping <- setNames(c("#F99D1E", "#008BA8", "#ED7667"), c("Severe", "Critical", "Moderate"))
  get_color <- function(classify) {
    return(color_mapping[classify])
  }
  
  data_variables <- reactiveValues(
   
    shocks = NULL,
    shocks.count = NULL,
    shocks.districts = NULL,
    shocks.effects.districts = NULL,
    shocks.effects.count = NULL,
    shocks.effects = NULL,
    red_flagged.districts = NULL,
    red_flagged.count = NULL,
    red_flagged = NULL,
    red_flagged_district_names_with_other_info = NULL,
    red_flagged_districts_last_month_status = NULL,
    red_flagged_classify = NULL,
    not_red_flagged.districts = NULL,
    not_red_flagged.count = NULL,
    indicator_total = NULL,
    fromDateControl = min(as.yearmon(ewea$reporting_month)),
    toDateControl = max(as.yearmon(ewea$reporting_month))
  )
  
  observe({
    req(input$fromdatecontroller,input$todatecontroller)
    data_variables$fromDateControl = input$fromdatecontroller
    data_variables$toDateControl =input$todatecontroller
    generateData(data_variables$fromDateControl, data_variables$toDateControl, data_variables)
    
    output$shocks.count <- renderUI({
      
      format(data_variables$shocks.count , big.mark = ",", scientific = FALSE)
    })
    output$shocks.effects.count <- renderUI({
      format( data_variables$shocks.effects.count , big.mark = ",", scientific = FALSE)
    })
    output$shocks.effects.count <- renderUI({
     
      format( data_variables$shocks.effects.count , big.mark = ",", scientific = FALSE)
    })
    output$shocks.districts <- renderUI({
       paste("Unique Districts :",data_variables$shocks.districts, sep = " ")
    })
    output$shocks.effects.districts <- renderUI({
      paste("Unique Districts :",data_variables$shocks.effects.districts, sep = " ")
    })
    output$red_flagged.districts <- renderUI({
       
      paste("Unique Districts",data_variables$red_flagged.districts, sep = " ")
    })
    output$red_flagged.count <- renderUI({
      
      format(data_variables$red_flagged.count , big.mark = ",", scientific = FALSE)
    })
    output$not_red_flagged.count <- renderUI({
     
      format( data_variables$not_red_flagged.count , big.mark = ",", scientific = FALSE)
    })
    output$not_red_flagged.districts <- renderUI({
      
      paste("Unique Districts",data_variables$not_red_flagged.districts , sep = " ")
    })
    
    output$total_alarms <-renderUI({
      
      format(data_variables$indicator_total$Alarm, big.mark = ",", scientific = FALSE)
    })
    
    output$total_alerts <-renderUI({
      
      format(data_variables$indicator_total$Alert, big.mark = ",", scientific = FALSE)
    })
    
    output$total_normal <-renderUI({
      
      format(data_variables$indicator_total$Normal, big.mark = ",", scientific = FALSE)
    })
    

    output$displayRedFlaggedMap <- renderLeaflet({
     
      if(nrow(data_variables$red_flagged_district_names_with_other_info) > 0){
        data_variables$red_flagged_district_names_with_other_info$color <- sapply(data_variables$red_flagged_district_names_with_other_info$last_clasify, get_color)
        
        
        leaflet() %>%
          addTiles() %>%
          setView(lat=5.67, lng=46.189, zoom=5)%>%
          addPolygons(data = somalia,fillColor = "none") %>%
          addPolygons(data = somalia_districts , fillColor  = "white",
                      weight = 0.5, smoothFactor = 0.5,
                      opacity = 0.3, fillOpacity = 1,
                      popup = paste(somalia_districts$District ,  sep = "<br>",
                                    somalia_districts$admin1Name)) %>%
          addPolygons(data = data_variables$red_flagged_district_names_with_other_info , color = "#90718D", weight = 0.5, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = as.vector(data_variables$red_flagged_district_names_with_other_info$color),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = paste(sep = "<br>",
                                    paste("<b>Region: </b>",data_variables$red_flagged_district_names_with_other_info$admin1Name) ,  
                                    paste("<b>District: </b>",data_variables$red_flagged_district_names_with_other_info$District,"<hr>") ,
                                    paste("<b>Total Red-Flags: </b>",data_variables$red_flagged_district_names_with_other_info$total),
                                    paste("<b>Most Clasification: </b>",data_variables$red_flagged_district_names_with_other_info$most_classify),
                                    paste("<b>Last Red-Flag status: </b>",data_variables$red_flagged_district_names_with_other_info$last_clasify),
                                    paste("<b>Last month recorded: </b>",data_variables$red_flagged_district_names_with_other_info$last_month),
                                    paste("<b>Classification: </b>",data_variables$red_flagged_district_names_with_other_info$classification)) , 
                      label =~data_variables$red_flagged_district_names_with_other_info$District ,
                      group = data_variables$red_flagged_district_names_with_other_info$District) %>%
          addLayersControl(
            overlayGroups = data_variables$red_flagged_district_names_with_other_info$District,
            options = layersControlOptions(collapsed = TRUE),position = "bottomright")
      }else{
        leaflet() %>%
          addTiles() %>%
          setView(lat=5.67, lng=46.189, zoom=5)%>%
          addPolygons(data = somalia,fillColor = "none") %>%
          addPolygons(data = somalia_districts , fillColor  = "white",
                      weight = 0.5, smoothFactor = 0.5,
                      opacity = 0.3, fillOpacity = 1,
                      popup = paste(somalia_districts$District ,  sep = "<br>",
                                    somalia_districts$admin1Name)) %>%
          addControl(html = "<div style='background-color: white; padding: 5px; color=#008BA8'>No Red-flags recorded</div>", 
                     position = "topright" # You can change the position: topright, topleft, bottomright, bottomleft
                     )
      }
      
      
      
      
    })
    
    output$displyclassify <- renderPlotly({
      if(nrow(data_variables$red_flagged_classify) >0){
        plot_ly(type='pie', labels=data_variables$red_flagged_classify$classify, values=data_variables$red_flagged_classify$count,  
                textinfo='label+percent',
                insidetextorientation='radial',
                marker = list(colors = get_color(data_variables$red_flagged_classify$classify),
                              line = list(color = '#FFFFFF', width = 1))) %>% 
          layout(legend = list(x = 0.5, y = -0.1, orientation = "h")) 
      }else{
        plot_ly(type='pie', labels="No Red-Flag", values=100,  
                textinfo='label+percent',
                insidetextorientation='radial',
                marker = list(colors = "#008BA8",
                              line = list(color = '#FFFFFF', width = 1))) %>% 
          layout(legend = list(x = 0.5, y = -0.1, orientation = "h")) 
      }
     
      
    })
    
    output$redFlagBarChartSummary <- renderPlotly({
      summary <- data_variables$red_flagged %>%
        filter(red_flagged == "Yes") %>%
        group_by(District) %>%
        summarise(
          Alarm = sum(Alarm),
          Alert = sum(Alert),
          Normal = sum(Normal),
          count = n()
        )
      
      fig <- plot_ly(summary, x = ~District, y = ~count, type = 'bar', name = 'Red-Flags',
                     marker = list(color = '#008BA8'))%>% 
        add_trace(y = ~Alarm, name = 'Alarm',marker = list(color = '#ED7667'))%>% 
        add_trace(y = ~Alert, name = 'Alert',marker = list(color = '#F99D1E'))%>%
        layout(yaxis = list(title = 'Number of occurence'),
               xaxis = list(title = 'Districts'),
               barmode = 'group' , 
               xaxis = list(categoryorder = "array",
                            categoryarray = ~District),
               legend = list(orientation = "h", x = 0, y = 1.1, xanchor = 'left', yanchor = 'top'))
      
      fig
    })
    
    output$redFlagBarChartSummarybyMonth <- renderPlotly({
      summary <- data_variables$red_flagged %>%
        filter(red_flagged == "Yes") %>%
        group_by(reporting_month) %>%
        summarise(
          Alarm = sum(Alarm),
          Alert = sum(Alert),
          Normal = sum(Normal),
          count = n()
        ) %>%
        arrange(as.yearmon(reporting_month))%>%
        mutate(reporting_month = as.character(reporting_month))
      
      fig <- plot_ly(summary, x = ~reporting_month, y = ~count, type = 'bar', name = 'Red-Flags',
                     marker = list(color = '#008BA8'))%>% 
        add_trace(y = ~Alarm, name = 'Alarm',marker = list(color = '#ED7667'))%>% 
        add_trace(y = ~Alert, name = 'Alert',marker = list(color = '#F99D1E'))%>%
        layout(yaxis = list(title = 'Number of occurence'),
               xaxis = list(title = 'Months'),
               barmode = 'group' , 
                            xaxis = list(categoryorder = "array",
                                         categoryarray = ~reporting_month),
               legend = list(orientation = "h", x = 0, y = 1.1, xanchor = 'left', yanchor = 'top'))
      
      fig
    })
    
    output$displayRedFlagTable <- renderDT(server = FALSE,{
      summary <- data_variables$red_flagged %>%
        filter(red_flagged == "Yes") %>%
        mutate(reporting_month = as.character(reporting_month)) %>%
        select(-c("shock.occurence","shock.effect","red_flagged"))
      
      datatable(
        summary,
        extensions = 'Buttons', 
        options = list(
        pageLength = 20, 
        
        dom = 'lBfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = "Red-flagging",
            title = "Red flagging",
            exportOptions = list(modifier = list(page = 'all'))
          ),
          list(
            extend = 'excel',
            filename = "Red-flagging",
            title = "Red flagging",
            exportOptions = list(modifier = list(page = 'all'))
          ),
          list(
            extend = 'pdf',
            filename = "Red-flagging",
            title = "Red flagging",
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
            filename = "Red-flagging",
            title = "Red flagging",
            exportOptions = list(modifier = list(page = 'all'))
          )
          
        )
        ),
        filter = 'top',class = "redFlagTbaleID")
    })
  })
  
  # observeEvent(input$generateData, {
  #   
  #   req(input$fromdatecontroller,input$todatecontroller)
  #   data_variables$fromDateControl = input$fromdatecontroller
  #   data_variables$toDateControl =input$todatecontroller
  #   
  # })

  
  
}