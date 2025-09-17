
districts_fms <- readxl::read_excel("docs/ewea/ewea districts and fms.xlsx")%>%
  as.data.frame()
segrationDropdown <- names(districts_fms[c("FMS",	"Region_name",	"District_Name","Member_name")])
# 
# length(unique(districts_fms$District_Name))
# length(unique(districts_fms$Member_name))


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



eweaUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/ewea.html",
                 explainStratgy = actionButton(
                   ns("explainStratgy"),
                   "Explain Stratgy",
                   class="btn btn-primary"
                 ),
                  messageBox = uiOutput(ns("messageBox")),
                  infoMessage= uiOutput(ns("infoMessage")),
                  ruralUrbanController = selectInput(
                     ns("ruralUrbanController"),
                     "Rural or Urban Areas",
                     choices = c("Rural","Urban")
                   ),
                 fromDateController = uiOutput(ns("fromDateControllerFILL")),
            
                 Segrigation =selectInput(
                   ns("Segrigation"),
                   "Summarise by",
                   choices = segrationDropdown
                 ),
                 Segrigation_element = uiOutput(ns("Segrigation_element")),
                 
                 total_red_flags = uiOutput(ns("total_red_flags")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 total_red_flags_text = uiOutput(ns("total_red_flags_text")),
                 RedFalggedDistrictMap = leafletOutput(ns("displayRedFlaggedMap")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 redFlagBarChartSummarybyMonth  = plotlyOutput(ns("redFlagBarChartSummarybyMonth"),height = "100%")%>% withSpinner(size=0.5,proxy.height = "50px"),
                 # displayRedFlagTable = DTOutput(ns("displayRedFlagTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 weightingScoreSummary = plotlyOutput(ns("weightingScoreSummary"),height = "100%")%>%withSpinner(size=0.5,proxy.height = "50px"),
                 variableController = uiOutput(ns("variableControllerFill"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 districtDropDown = uiOutput(ns("districtDropDownFill"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 districtsbyvariable = plotlyOutput(ns("districtsbyvariable"),height = "4500px")%>% withSpinner(size=0.5,proxy.height = "50px"),
                 datebyvariable = plotlyOutput(ns("datebyvariable")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 districtVariableAnalysis = plotlyOutput(ns("districtVariableAnalysis"),height = "100%") %>% withSpinner(size=0.5,proxy.height = "50px"),
                 variableReportTable = DTOutput(ns("variableReportTable")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 top3DistrictwithNormal= uiOutput(ns("top3DistrictwithNormal")),
                 districtsnotreported =DTOutput(ns("districtsnotreported")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 redFlagStatusByDistrict = plotlyOutput(ns("redFlagStatusByDistrict"),height = "100%") %>% withSpinner(size=0.5,proxy.height = "50px"),
                 whereWeWokMap = leafletOutput(ns("whereWeWokMap")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 downloadButtonThreshold = downloadButton(class ="btn btn-success",outputId = ns("downloadButtonThreshold"),label =  "Early Warning Indicators and Thresholds"),
                 alarmCasesPerVariable =plotlyOutput(ns("alarmCasesPerVariable"),height = "4500px")%>% withSpinner(size=0.5,proxy.height = "50px"),
                 redFlagBarChartSummarybyMonthTable=DTOutput(ns("redFlagBarChartSummarybyMonthTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 variableAnalysis = DTOutput(ns("variableAnalysis")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 riskClasificationChart=plotlyOutput(ns("riskClasificationChart"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 overallRiskClasification=plotlyOutput(ns("overallRiskClasification"),height = "500px")%>% withSpinner(size=0.5,proxy.height = "50px"),
                 summarisetheoverallclassification = uiOutput(ns("summarisetheoverallclassification")),
                 factorsContributingRedFlagging = DTOutput(ns("factorsContributingRedFlagging")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 factorsContributingRedFlaggingallVariables = DTOutput(ns("factorsContributingRedFlaggingallVariables")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 valuesOfIndicatorsForRedFlaggedDistricts = DTOutput(ns("valuesOfIndicatorsForRedFlaggedDistricts")) %>% withSpinner(size=0.5,proxy.height = "50px")
    )
  )
  
}



ewea <- function(input ,output , session,sharedValues){
  
  color_mapping <- setNames(c("#ED7667","#F99D1E","#00A65A"), c("Alarm" ,"Alert", "Normal"))

  get_color <- function(classify) {
    return(color_mapping[classify])
  }
  
  
  
  
  output$Segrigation_element <- renderUI({
    ns1 <- NS("ewea")
    
    
   
    selectInput(
      ns1("Segrigation_element_drill"),
      paste("Filter by",input$Segrigation),
      choices = c("all",unique(pull(districts_fms%>%filter(Residence_type == input$ruralUrbanController),all_of(!!sym(input$Segrigation))))),
      multiple = TRUE,
      selected = "all"
    )
  })
  
  global_vars <- reactiveValues(
    global_ewea = NULL,
    global_shock_occurence = NULL,
    global_shock_effect = NULL,
    global_red_flagging = NULL,
    global_market_threshold = NULL,
    global_ewea_weighting_scores = NULL
  )
  
  tryCatch({
    
    postgresConnection <- dbConnect(RPostgres::Postgres(),
                                    dbname = "brcisShiny",
                                    host = "brcisproddb.postgres.database.azure.com", port = 5432,
                                    user = "brcisShiny", password = "brcisShiny@112233@",
                                    sslmode = 'require')

    global_vars$global_ewea =  readTable(postgresConnection,"ewea")
    global_vars$global_shock_occurence = readTable(postgresConnection,"shock_occurence")
    global_vars$global_shock_effect = readTable(postgresConnection,"shock_effect")
    global_vars$global_red_flagging = readTable(postgresConnection,"red_flagging")
    global_vars$global_market_threshold = readTable(postgresConnection,"market_threshold")
    global_vars$global_ewea_weighting_scores = readTable(postgresConnection,"ewea_weighting_scores")
    
    
    dbDisconnect(postgresConnection)
    
  
    
    # output$toDateControllerFILL <- renderUI({
    #   ns1 <- NS("ewea")
    #   
    #   selectInput(
    #     ns1("toDateController"),
    #     paste("To Date"),
    #     choices = unique(sort(as.yearmon(global_vars$global_ewea$reporting_month))),
    #     selected = max(as.yearmon(global_vars$global_ewea$reporting_month))  # Selects the latest month
    #   )
    # })
    
  }, error = function(e) {
    output$messageBox <- renderUI({
      print(e$message)
      HTML(paste(sep = "",
                 '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bxs-message-square-x"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-white">Error</h6>
											<div class="text-white">Error on loading Data , please contact BRCIS CMU MEL for any support</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>' 
      ))
    })
    print(e$message)
  })
  
  
  observeEvent(input$ruralUrbanController,{
    sub_global <- global_vars$global_ewea%>%filter(Residence_type == input$ruralUrbanController) 
    
    output$fromDateControllerFILL <- renderUI({
      ns1 <- NS("ewea")
      
      selectInput(
        ns1("fromDateController"),
        paste("Date"),
        choices = unique(sort(as.yearmon(sub_global$reporting_month))),
        selected = max(as.yearmon(sub_global$reporting_month))  # Selects the latest month
      )
      
    })
  })
  
  observeEvent(input$Segrigation_element_drill, {
    if ("all" %in% input$Segrigation_element_drill && length(input$Segrigation_element_drill) > 1) {
      updateSelectInput(session, "Segrigation_element_drill",
                        selected = setdiff(input$Segrigation_element_drill, "all"))
    }
  })
  
  
  observe({
    tryCatch({
      
      if( is.null(nrow(global_vars$global_ewea)) || nrow(global_vars$global_ewea) == 0){
        return()
      }
      
     
      
      if( is.null(input$fromDateController) || 
          is.null(input$ruralUrbanController)   ||
          is.null(input$Segrigation) || 
          is.null(input$Segrigation_element_drill)
          ){
        return()
      }
      
     
      if ("all" %in% input$Segrigation_element_drill && length(input$Segrigation_element_drill) > 1) {
        return()
      }
     
      
      red_flagging<- global_vars$global_red_flagging%>%filter(Residence_type == input$ruralUrbanController) 
      main_ewea <- global_vars$global_ewea%>%filter(Residence_type == input$ruralUrbanController) 
      
      # Find missing districts per month
      districts_fms_sub <- districts_fms%>%filter(Residence_type == input$ruralUrbanController) 
      missing_districts_per_month <- main_ewea %>%
        group_by(reporting_month) %>%
        summarise(District_not_reported = list(setdiff(districts_fms_sub$District_Name, District_Name))) %>%
        unnest(cols = c(District_not_reported))%>%
        left_join(districts_fms_sub, by = c("District_not_reported" = "District_Name"))
      
      community_points <- somalia_districts_communities%>%filter(Residency == input$ruralUrbanController) 
     
      red_flagging_all <- red_flagging
      main_ewea_all <-main_ewea
     
      
      red_flagging%<>%mutate(reporting_month = as.yearmon(reporting_month)) %>%
        filter(as.Date(as.yearmon(reporting_month)) == as.Date(as.yearmon(input$fromDateController)))
    
      
      main_ewea%<>%mutate(reporting_month = as.yearmon(reporting_month)) %>%
        filter(as.Date(as.yearmon(reporting_month)) == as.Date(as.yearmon(input$fromDateController)))
      
      
      if(nrow(missing_districts_per_month)>0){
        missing_districts_per_month%<>%
          filter(as.Date(as.yearmon(reporting_month)) == as.Date(as.yearmon(input$fromDateController)))
        
        
      }
      
    
      red_flagging%<>%filter(strategy == "new")
      red_flagging_all%<>%filter(strategy == "new")

      
      
      if(!is.null((input$Segrigation)) & !is.null((input$Segrigation_element_drill))){
        if(!("all"  %in% input$Segrigation_element_drill)){
          

          
          red_flagging %<>% filter(
            !!sym(input$Segrigation) %in% input$Segrigation_element_drill
          )

          main_ewea %<>%filter(
            !!sym(input$Segrigation) %in%  input$Segrigation_element_drill
          )
         
          red_flagging_all %<>% filter(
            !!sym(input$Segrigation) %in% input$Segrigation_element_drill
          )
          
          main_ewea_all %<>% filter(
            !!sym(input$Segrigation) %in% input$Segrigation_element_drill
          )
          
          districts_fms_sub %<>% filter(
            !!sym(input$Segrigation) %in% input$Segrigation_element_drill
          )
          
          community_points <- community_points%>%
            rename(
              Member_name = member,
              District_Name = district,
              Region_name = region
            )%>%
            filter(
              !!sym(input$Segrigation) %in% input$Segrigation_element_drill
            )%>%
            rename(
              member = Member_name,
              district = District_Name,
              region = Region_name
            )
          
          
          
          
        }
        
      }
     
      output$districtDropDownFill <- renderUI({
        ns1 <- NS("ewea")
        
        selectInput(
          ns1("districtDropDown"),
          paste("Select District"),
          choices = unique(main_ewea$District_Name)
        )
      })
      
     
      
      output$variableControllerFill <- renderUI({
        ns1 <- NS("ewea")
        
        selectInput(
          ns1("variableController"),
          paste("Select Variable"),
          choices = unique(main_ewea$variable)
        )
      })
    
      output$total_red_flags <- renderUI({
        
        paste(red_flagging%>%filter(red_flagged =="Yes")%>%nrow(),"out of ",red_flagging%>%nrow())
        
      })
      
      output$total_red_flags_text <- renderUI({
        
        if(length(unique(red_flagging$reporting_month)) > 1){
          HTML(paste("<span class='text-primary font-14'>Between",min(as.yearmon(red_flagging$reporting_month)) ,"and",max(as.yearmon(red_flagging$reporting_month)),"</span>"))
        }else if(length(unique(red_flagging$reporting_month)) == 1){
          HTML(paste("<span class='text-primary font-14'>in",min(as.yearmon(red_flagging$reporting_month)),"</span>"))
          
        }else{
          HTML(paste("No Data is found"))
        }
        
      })
      
      
      
      output$whereWeWokMap <- renderLeaflet({
        
        whereWeWokMap <- districts_fms%>%
          filter(Residence_type == input$ruralUrbanController)%>%
          group_by(District_Name)%>%
          summarise(
            Region_name = first(Region_name),
            Member_name = paste(Member_name,collapse = ",")
          )%>%
          mutate(
            District_Name = case_when(
              District_Name == "Hudur" ~ "Xudur",
              District_Name == "El-berde" ~ "Ceel Barde",
              District_Name == "Rab_dhure" ~ "Rab Dhuure",
              District_Name == "Afgoye" ~ "Afgooye",
              District_Name == "Belet_Hawa" ~ "Belet Xaawo",
              District_Name == "Baardhere" ~ "Baardheere",
              District_Name == "Baidoa" ~ "Baydhaba",
              District_Name == "Wanla_Weyn" ~ "Wanla Weyn",
              District_Name == "Adado" ~ "Cadaado",
              District_Name == "Dhusamareeb" ~ "Dhuusamarreeb",
              District_Name == "Galkacyo-South" ~ "Gaalkacyo-South",
              District_Name == "Galkacyo-North" ~ "Gaalkacyo-North",
              District_Name == "Kismayo" ~ "Kismaayo",
              District_Name == "Afmadow" ~ "Afmadow",
              District_Name == "Belet_weyne" ~ "Belet Weyne",
              District_Name == "Jawhar" ~ "Jowhar",
              .default = District_Name
            )
          )%>%left_join(somalia_districts,by = c("District_Name"="DIST_NAME"))%>%sf::st_as_sf()
        
        d1 <- districts_fms%>%
          filter(Residence_type == input$ruralUrbanController)%>%
          pull(District_Name)
        
        sdc <- somalia_districts_communities%>% 
          filter(Residency == input$ruralUrbanController)%>%
          filter(
            district %in% d1
          )
        
        
        
        leaflet() %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%  # Satellite map tiles
          addTiles(group = "Normal Map") %>%  # Normal map tiles
          addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") %>%
          setView(lat=5.67, lng=46.189, zoom=5)%>%
          addPolygons(data = somalia,fillColor = "none",color = "black") %>%
          addPolygons(data = somalia_districts , fillColor = "white",
                      weight = 0.5, smoothFactor = 0.5,
                      opacity = 0.3, fillOpacity = 1,
                      color = "black",
                      popup = paste(somalia_districts$DIST_NAME ,  sep = "<br>",
                                    somalia_districts$REG_NAME)) %>%
          addPolygons(data = whereWeWokMap , color = "black",fillColor="#ED7667", weight = 0.5, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 1,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = FALSE),
                      popup = paste(sep = "<br>",
                                    paste("<b>Region: </b>",whereWeWokMap$Region_name) ,  
                                    paste("<b>District: </b>",whereWeWokMap$District_Name,"<hr>") ,
                                    paste("<b>Member(s): </b>",whereWeWokMap$Member_name,"<hr>") ),
                                    
                      label =~whereWeWokMap$District_Name ,
                      
                      group = whereWeWokMap$District_Name) %>%
          addCircleMarkers(
            data = sdc,
            lng = ~geopoint_longitude,
            lat = ~geopoint_latitude,
            label = ~community,
            popup = paste(sep = "<br>",
                          paste("<b>Region: </b>",sdc$region) ,  
                          paste("<b>District: </b>",sdc$district) ,
                          paste("<b>Cluster(s): </b>",sdc$cluster),
                          paste("<b>Community(s): </b>",sdc$community),
                          paste("<b>Member(s): </b>",sdc$member) ),
            radius = 2,             # Very small point radius
            color = "black",          # Or transparent for just fill
            fillColor = "black",        # Fill color of the point
            fillOpacity = 0.7,
            stroke = FALSE  
          )%>%
          addLegend(
            position = "bottomleft",
            colors = c("#fff","#ED7667","black"),
            labels = c("Out of scope districts","Districts we work","Communities"),
            title = "Where we work",
            opacity = 0.7
          )%>%
          addLayersControl(
            baseGroups = c("Satellite Map","Normal Map","CartoDB.Positron"),
            overlayGroups = whereWeWokMap$District_Name,
            options = layersControlOptions(collapsed = TRUE),position = "bottomright")
      })
      
      output$riskClasificationChart <- renderPlotly({
        cmap <- c("Alarm" = "#ED7667", "Alert" = "#F99D1E", "Normal" = "#00A65A")
        
        data <- red_flagging %>%
          group_by(classify)%>%
          summarise(
            count = n()
          )
        
        fig <- plot_ly(data, labels = ~classify, values = ~count, type = 'pie',
                       textinfo = "value+percent",  # Show category, count, and percentage
                       insidetextorientation = "auto",   # Auto-orient text inside the chart
                       marker = list(colors = unname(cmap[data$classify])))
        fig <- fig %>% layout(title =paste("District Risk Classification"),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              legend = list(
                                orientation = "h",  # Horizontal legend
                                x = 0.5,            # Centered horizontally
                                y = -0.2,           # Positioned below the chart
                                xanchor = "center"
                              ))
        
        fig
      })
  
     
      output$displayRedFlaggedMap <- renderLeaflet({
        
        red_flagged_district_names_with_other_info <- red_flagging %>% 
          group_by(District_Name) %>%
          reframe(
            count  = n(),
            Alarm = sum(Alarm),
            Alert = sum(Alert),
            Normal = sum(Normal),
            variable_score = sum(variable_score),
            maximum_results = ifelse(District_Name %in% reverine_districts , 57.6, 55.2),
            variable_score_percentage = round((variable_score / (maximum_results*count))*100,1),
            District_Name = first(District_Name),
            Member_name = first(Member_name),
            Member_risk = paste(paste(Member_name,"(",variable_score_percentage,"%)"))
            
          )%>%
          group_by(District_Name) %>%
          reframe(
            total_redflags = sum(count),
            Alarm = sum(Alarm),
            Alert = sum(Alert),
            Normal = sum(Normal),
            variable_score = sum(variable_score),
            maximum_results = ifelse(District_Name %in% reverine_districts , 57.6, 55.2),
            variable_score_percentage = round((variable_score / (maximum_results*total_redflags))*100,1),
            Member_name = paste(Member_name,collapse = ","),
            Member_risk = paste(Member_risk,collapse = ",")
          )%>%
          mutate(
            District_Name = case_when(
              District_Name == "Hudur" ~ "Xudur",
              District_Name == "El-berde" ~ "Ceel Barde",
              District_Name == "Rab_dhure" ~ "Rab Dhuure",
              District_Name == "Afgoye" ~ "Afgooye",
              District_Name == "Belet_Hawa" ~ "Belet Xaawo",
              District_Name == "Baardhere" ~ "Baardheere",
              District_Name == "Baidoa" ~ "Baydhaba",
              District_Name == "Wanla_Weyn" ~ "Wanla Weyn",
              District_Name == "Adado" ~ "Cadaado",
              District_Name == "Dhusamareeb" ~ "Dhuusamarreeb",
              District_Name == "Galkacyo-South" ~ "Gaalkacyo-South",
              District_Name == "Galkacyo-North" ~ "Gaalkacyo-North",
              District_Name == "Kismayo" ~ "Kismaayo",
              District_Name == "Afmadow" ~ "Afmadow",
              District_Name == "Belet_weyne" ~ "Belet Weyne",
              District_Name == "Jawhar" ~ "Jowhar",
              .default = District_Name
            ),  
            color = case_when(
              variable_score_percentage <= 24 ~ "Normal",
              variable_score_percentage > 24 & variable_score_percentage <= 49 ~ "Alert",
              variable_score_percentage > 49 ~ "Alarm",
              .default = "Normal"
            ),
            risk = case_when(
              variable_score_percentage <=24 ~  "Continuous monitoring and resilience programming",
              variable_score_percentage >24 & variable_score_percentage <=49 ~  "Continuous monitoring and launching early action",
              variable_score_percentage >49  ~  "Red Flagging, scale up early action, and application of the contingency fund (Crisis Modifier)",
              .default = "Continuous monitoring and resilience programming"
            )
          )
        
        
        red_flagged_district_names_with_other_info%<>%
          left_join(somalia_districts,by = c("District_Name"="DIST_NAME")) %>%sf::st_as_sf()
        
        
        
        not_reported_districts <- districts_fms_sub%<>%
          filter(!(District_Name %in% red_flagging$District_Name))%>%
          mutate(
            District_Name = case_when(
              District_Name == "Hudur" ~ "Xudur",
              District_Name == "El-berde" ~ "Ceel Barde",
              District_Name == "Rab_dhure" ~ "Rab Dhuure",
              District_Name == "Afgoye" ~ "Afgooye",
              District_Name == "Belet_Hawa" ~ "Belet Xaawo",
              District_Name == "Baardhere" ~ "Baardheere",
              District_Name == "Baidoa" ~ "Baydhaba",
              District_Name == "Wanla_Weyn" ~ "Wanla Weyn",
              District_Name == "Adado" ~ "Cadaado",
              District_Name == "Dhusamareeb" ~ "Dhuusamarreeb",
              District_Name == "Galkacyo-South" ~ "Gaalkacyo-South",
              District_Name == "Galkacyo-North" ~ "Gaalkacyo-North",
              District_Name == "Kismayo" ~ "Kismaayo",
              District_Name == "Afmadow" ~ "Afmadow",
              District_Name == "Belet_weyne" ~ "Belet Weyne",
              District_Name == "Jawhar" ~ "Jowhar",
              .default = District_Name
            )
          )%>%left_join(somalia_districts,by = c("District_Name"="DIST_NAME"))%>%sf::st_as_sf()
        

        not_red_flagged_district_names_with_other_info <- red_flagging %>% 
          filter(red_flagged == "No") %>%
          group_by(District_Name,classify) %>%
          reframe(
            Alarm = sum(Alarm),
            Alert = sum(Alert),
            Normal = sum(Normal),
            variable_score = sum(variable_score),
            District_Name = first(District_Name),
            Member_name = first(Member_name),
            count  = n(),
          )%>%
          group_by(District_Name) %>%
          reframe(
            count = sum(count),
            Alarm = sum(Alarm),
            Alert = sum(Alert),
            Normal = sum(Normal),
            variable_score = sum(variable_score),
            maximum_results = ifelse(District_Name %in% reverine_districts , 57.6, 55.2),
            variable_score_percentage = round((variable_score / (maximum_results*count))*100,1),
            Member_name = paste(Member_name,collapse = ",")
          )%>%
          mutate(
            District_Name = case_when(
              District_Name == "Hudur" ~ "Xudur",
              District_Name == "El-berde" ~ "Ceel Barde",
              District_Name == "Rab_dhure" ~ "Rab Dhuure",
              District_Name == "Afgoye" ~ "Afgooye",
              District_Name == "Belet_Hawa" ~ "Belet Xaawo",
              District_Name == "Baardhere" ~ "Baardheere",
              District_Name == "Baidoa" ~ "Baydhaba",
              District_Name == "Wanla_Weyn" ~ "Wanla Weyn",
              District_Name == "Adado" ~ "Cadaado",
              District_Name == "Dhusamareeb" ~ "Dhuusamarreeb",
              District_Name == "Galkacyo" ~ "Gaalkacyo",
              District_Name == "Kismayo" ~ "Kismaayo",
              District_Name == "Afmadow" ~ "Afmadow",
              District_Name == "Belet_weyne" ~ "Belet Weyne",
              District_Name == "Jawhar" ~ "Jowhar",
              .default = District_Name
            )
          )
        
        not_red_flagged_district_names_with_other_info%<>%
          left_join(somalia_districts,by = c("District_Name"="DIST_NAME")) %>%sf::st_as_sf()
        
        
        
       
        
        color_mapping <- setNames(c("#ED7667","#F99D1E","#00A65A"), c("Alarm" ,"Alert", "Normal"))
        
        get_color <- function(classify) {
          return(color_mapping[classify])
        }
        
        # d1 <- red_flagging %>% 
        #   group_by(District_Name)%>%
        #   reframe(
        #     count  = n()
        #   )%>%
        #   pull(District_Name)
        
        community_points <- community_points
        
        if(nrow(red_flagged_district_names_with_other_info) > 0){
          
          
          red_flagged_district_names_with_other_info$color <- sapply(red_flagged_district_names_with_other_info$color, get_color)
          
          if(nrow(not_red_flagged_district_names_with_other_info) > 0){
            leaflet() %>%
              addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%  # Satellite map tiles
              addTiles(group = "Normal Map") %>%  # Normal map tiles
              addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") %>%
              setView(lat=5.67, lng=46.189, zoom=5)%>%
              addPolygons(data = somalia,fillColor = "none",color = "black") %>%
              addPolygons(data = somalia_districts , fillColor = "white",
                          weight = 0.5, smoothFactor = 0.5,
                          opacity = 0.3, fillOpacity = 1,
                          color = "black",
                          popup = paste(somalia_districts$DIST_NAME ,  sep = "<br>",
                                        somalia_districts$REG_NAME)) %>%
              addPolygons(data = not_reported_districts , fillColor = "#40419A",
                          weight = 0.5, smoothFactor = 0.5,
                          opacity = 0.3, fillOpacity = 1,
                          color = "black",
                          popup = paste(not_reported_districts$District_Name ,  sep = "<br>",
                                        not_reported_districts$REG_NAME)) %>%
              addPolygons(data = red_flagged_district_names_with_other_info , color = "black",fillColor = as.vector(red_flagged_district_names_with_other_info$color) , weight = 0.5, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.9,
                          highlightOptions = highlightOptions(color = "white", weight = 2,
                                                              bringToFront = FALSE),
                          popup = paste(sep = "<br>",
                                        paste("<b>Region: </b>",red_flagged_district_names_with_other_info$REG_NAME) ,  
                                        paste("<b>District: </b>",red_flagged_district_names_with_other_info$District_Name,"<hr>") ,
                                        paste("<b>Member(s): </b>",red_flagged_district_names_with_other_info$Member_name,"<hr>") ,
                                        paste("<b>Overall Risk Score: </b>",paste0(red_flagged_district_names_with_other_info$variable_score_percentage)) ,
                                        paste("<b>Member Disaggregated Risk Score: </b>",paste0(red_flagged_district_names_with_other_info$Member_risk),"<hr>") ,
                                        paste("<b>Classification: </b>",red_flagged_district_names_with_other_info$risk)) , 
                          label =~red_flagged_district_names_with_other_info$District_Name ,
                          # labelOptions = labelOptions(noHide = TRUE) ,
                          group = red_flagged_district_names_with_other_info$District_Name) %>%
              addCircleMarkers(
                data = community_points,
                lng = ~geopoint_longitude,
                lat = ~geopoint_latitude,
                label = ~community,
                popup = paste(sep = "<br>",
                              paste("<b>Region: </b>",community_points$region) ,  
                              paste("<b>District: </b>",community_points$district) ,
                              paste("<b>Cluster(s): </b>",community_points$cluster),
                              paste("<b>Community(s): </b>",community_points$community),
                              paste("<b>Member(s): </b>",community_points$member) ),
                radius = 2,             # Very small point radius
                color = "black",          # Or transparent for just fill
                fillColor = "black",        # Fill color of the point
                fillOpacity = 0.7,
                stroke = FALSE  
              )%>%
              addLegend(
                position = "bottomleft",
                colors = c("#fff","#40419A","#ED7667","#F99D1E","#00A65A","black"),
                labels = c("Out of scope districts","Districts not Reported","Alarmed Districts (red-flagged)","Alerted Districts","Normal Districts","Communities"),
                title = "Risk Levels",
                opacity = 0.7
              )%>%
              addLayersControl(
                baseGroups = c("Satellite Map","Normal Map","CartoDB.Positron"),
                overlayGroups = red_flagged_district_names_with_other_info$District_Name,
                options = layersControlOptions(collapsed = FALSE),position = "bottomright")
          }else{
            leaflet() %>%
              addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%  # Satellite map tiles
              addTiles(group = "Normal Map") %>%  # Normal map tiles
              addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") %>%
              setView(lat=5.67, lng=46.189, zoom=5)%>%
              addPolygons(data = somalia,fillColor = "none",color = "black") %>%
              addPolygons(data = somalia_districts , fillColor = "white",
                          weight = 0.5, smoothFactor = 0.5,
                          opacity = 0.3, fillOpacity = 1,
                          color = "black",
                          popup = paste(somalia_districts$DIST_NAME ,  sep = "<br>",
                                        somalia_districts$REG_NAME)) %>%
              
              addPolygons(data = not_reported_districts , fillColor = "#40419A",
                          weight = 0.5, smoothFactor = 0.5,
                          opacity = 0.3, fillOpacity = 1,
                          color = "black",
                          popup = paste(not_reported_districts$District_Name ,  sep = "<br>",
                                        not_reported_districts$REG_NAME)) %>%
              addPolygons(data = red_flagged_district_names_with_other_info , color = "#ED7667",fillColor="#ED7667", weight = 0.5, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.9, # Add labels to polygons
                          # fillColor = as.vector(red_flagged_district_names_with_other_info$color),
                          highlightOptions = highlightOptions(color = "white", weight = 2,
                                                              bringToFront = FALSE),
                          popup = paste(sep = "<br>",
                                        paste("<b>Region: </b>",red_flagged_district_names_with_other_info$REG_NAME) ,  
                                        paste("<b>District: </b>",red_flagged_district_names_with_other_info$District_Name,"<hr>") ,
                                        paste("<b>Member(s): </b>",red_flagged_district_names_with_other_info$Member_name,"<hr>") ,
                                        paste("<b>Total Red-Flags: </b>",red_flagged_district_names_with_other_info$total_redflags),
                                        paste("<b>Total Alarms: </b>",red_flagged_district_names_with_other_info$Alarm),
                                        paste("<b>Total Alerts: </b>",red_flagged_district_names_with_other_info$Alert),
                                        paste("<b>Total Normal: </b>",red_flagged_district_names_with_other_info$Normal),
                                        paste("<b>Classification: </b>",red_flagged_district_names_with_other_info$classification)) , 
                          label =~red_flagged_district_names_with_other_info$District_Name ,
                          # labelOptions = labelOptions(noHide = TRUE) ,
                          group = red_flagged_district_names_with_other_info$District_Name) %>%
              addCircleMarkers(
                data = community_points,
                lng = ~geopoint_longitude,
                lat = ~geopoint_latitude,
                label = ~community,
                popup = paste(sep = "<br>",
                              paste("<b>Region: </b>",community_points$region) ,  
                              paste("<b>District: </b>",community_points$district) ,
                              paste("<b>Cluster(s): </b>",community_points$cluster),
                              paste("<b>Community(s): </b>",community_points$community),
                              paste("<b>Member(s): </b>",community_points$member) ),
                radius = 2,             # Very small point radius
                color = "black",          # Or transparent for just fill
                fillColor = "black",        # Fill color of the point
                fillOpacity = 0.7,
                stroke = FALSE  
              )%>%
              addLegend(
                position = "topleft",
                colors = c("#fff","#40419A","#00A65A","#F99D1E","#ED7667","black"),
                labels = c("Out of scope districts","Districts not Reported","Normal Districts","Alerted Districts","Red-flagged Districts","Communities"),
                title = "Risk Levels",
                opacity = 0.7
              )%>%
              addLayersControl(
                baseGroups = c("Satellite Map","Normal Map","CartoDB.Positron"),
                overlayGroups = red_flagged_district_names_with_other_info$District_Name,
                options = layersControlOptions(collapsed = FALSE),position = "bottomright")
          }
          
          
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
            addCircleMarkers(
              data = community_points,
              lng = ~geopoint_longitude,
              lat = ~geopoint_latitude,
              label = ~community,
              popup = paste(sep = "<br>",
                            paste("<b>Region: </b>",community_points$region) ,  
                            paste("<b>District: </b>",community_points$district) ,
                            paste("<b>Cluster(s): </b>",community_points$cluster),
                            paste("<b>Community(s): </b>",community_points$community),
                            paste("<b>Member(s): </b>",community_points$member) ),
              radius = 2,             # Very small point radius
              color = "black",          # Or transparent for just fill
              fillColor = "black",        # Fill color of the point
              fillOpacity = 0.7,
              stroke = FALSE  
            )%>%
            addControl(html = "<div style='background-color: white; padding: 5px; color=#00A65A'>No Red-flags recorded</div>", 
                       position = "topright" # You can change the position: topright, topleft, bottomright, bottomleft
            )
        }
        
      })
      
      
      output$factorsContributingRedFlagging <- renderDT(server = FALSE,{
        red_flagging %>%
          select(District_Name,reporting_month,red_flagged)%>%
          left_join(
            main_ewea%>%select(District_Name,reporting_month,variable_desc , value,status)
          )%>%
          select(District_Name,red_flagged,variable_desc,status)%>%
          pivot_wider(id_cols =District_Name:red_flagged , names_from = variable_desc ,values_from = status )%>%
          datatable(escape = FALSE, options = list(
            scrollX = TRUE,
            pageLength = 100,
            dom = 'Brt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # ✅ Also required,
            rowCallback = JS(
              "function(row, data, index) {",
              "  for (var i = 0; i < data.length; i++) {",
              "    var cellText = data[i] ? data[i].toString().toLowerCase() : '';",
              "    var cell = $('td:eq(' + i + ')', row);",
              "    if (cellText.includes('alarm')) {",
              "      cell.css({'background-color': 'red', 'color': 'white'});",
              "    } else if (cellText.includes('alert')) {",
              "      cell.css('background-color', 'orange');",
              "    } else if (cellText.includes('normal')) {",
              "      cell.css('background-color', 'lightgreen');",
              "    }",
              "  }",
              "}"
            )
          ))%>%
          formatStyle(
            'red_flagged',
            backgroundColor = styleEqual(c('Yes'),
                                         c("#dc3545"))
          )
          


      })
      
      output$valuesOfIndicatorsForRedFlaggedDistricts <- renderDT(server = FALSE,{
        red_flagging %>%
          filter(red_flagged=="Yes")%>%
          select(District_Name,reporting_month)%>%
          left_join(
            main_ewea%>%select(District_Name,reporting_month,variable_desc , value,status,content2,content1)
          )%>%
          mutate(
            badge_class = case_when(
              status == "Alarm" ~ "danger",
              status == "Alert" ~ "warning",
              status == "Normal" ~ "success",
              TRUE ~ "secondary"
            ),
            value = case_when(
              variable_desc == "Coping strategies" ~ paste0(
                "<ul>",
                "<span class='badge bg-", badge_class, "'>", status, "</span>",
                paste0("<li>", str_replace_all(content2, ",\\s*", "</li><li>")), "</li>",
                "</ul>"
              ),
              variable_desc == "AWD/sus Cholera Cases under 5" &  grepl("Double", content1) ~ paste(
                value ,content1, paste("(Previous Two week cases were ",content2,")"),sep = "<br>"
              ),
              TRUE ~ paste(
                sep = "<br>",
                paste0(
                  value, " <span class='badge bg-", badge_class, "'>", status, "</span>"
                ),
                ifelse(other_info1 != "", paste0(other_info1, ": ", content1), ""),
                ifelse(other_info2 != "", paste0(other_info2, ": ", content2), "")
              )
            ),
            
          )%>%
          select(District_Name,variable_desc,value)%>%
          pivot_wider(id_cols =variable_desc , names_from = District_Name ,values_from = value )%>%
          datatable(escape = FALSE,class = 'cell-border stripe', options = list(
            scrollX = TRUE,
            pageLength = 100,
            dom = 'Brt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')  # ✅ Also required
          ))


      })
     
      output$factorsContributingRedFlaggingallVariables <- renderDT(server = FALSE,{
       
          main_ewea%>%select(District_Name,variable_desc,status)%>%
          pivot_wider(id_cols =District_Name , names_from = variable_desc ,values_from = status )%>%
          datatable(escape = FALSE, options = list(
            scrollX = TRUE,
            pageLength = 100,
            dom = 'Brt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # ✅ Also required
            rowCallback = JS(
              "function(row, data, index) {",
              "  for (var i = 0; i < data.length; i++) {",
              "    var cellText = data[i] ? data[i].toString().toLowerCase() : '';",
              "    var cell = $('td:eq(' + i + ')', row);",
              "    if (cellText.includes('alarm')) {",
              "      cell.css({'background-color': 'red', 'color': 'white'});",
              "    } else if (cellText.includes('alert')) {",
              "      cell.css('background-color', 'orange');",
              "    } else if (cellText.includes('normal')) {",
              "      cell.css('background-color', 'lightgreen');",
              "    }",
              "  }",
              "}"
            )
          ))
        
        
      })
      
      
      output$redFlagStatusByDistrict <- renderPlotly({
        redFlagStatusByDistrict <- main_ewea%>%
          select(District_Name,status)%>%
          group_by(District_Name,status)%>%
          reframe(
            count = n()
          )
        
        # missing_district <- main_ewea_all %>%
        #   filter(!(District_Name %in% redFlagStatusByDistrict$District_Name))%>%
        #   mutate(
        #     status = "Normal",
        #     count = 0
        #   )
        # 
        # redFlagStatusByDistrict = rbind(redFlagStatusByDistrict,missing_district)
        
        ggplotly(
          ggplot(redFlagStatusByDistrict, aes(x = count, y = District_Name, fill = status)) +
            geom_col() +
            geom_text(aes(label = count), hjust = -0.2, size = 4) +
            facet_grid(cols = vars(status), scales = "free") +  # Facet by 'status'
            theme_minimal() +
            labs(title = paste("Indicator categorization per status for all district at", input$fromDateController), 
                 x = NULL, y = NULL) +
            theme(
              strip.text = element_text(face = "bold"),  # Make facet titles bold
              plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
            ) +
            scale_fill_manual(values = c("Alert" = "#F99D1E", "Normal" = "#00A65A", "Alarm" = "#ED7667")) +  # Apply custom colors
            xlim(0, max(redFlagStatusByDistrict$count) * 1.2)  # Extend x-axis to ensure text is visible
        )
  
      })
      
      output$districtsnotreported <- renderDT(server = FALSE,{
        datatable(
          missing_districts_per_month,
          extensions = 'Buttons', 
          options = list(
            scrollX = TRUE, 
            pageLength = 5,
            dom = 'lBfrtip', 
            
          
            buttons = list(
              list(
                extend = 'csv',
                filename = "Districts Not Reported",
                title = "Districts Not Reported",
                exportOptions = list(modifier = list(page = 'all'))
              ),
              list(
                extend = 'excel',
                filename = "Districts Not Reported",
                title = "Districts Not Reported",
                exportOptions = list(modifier = list(page = 'all'))
              ),
              list(
                extend = 'pdf',
                filename = "Districts Not Reported",
                title = "Districts Not Reported",
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
                filename = "Districts Not Reported",
                title = "Districts Not Reported",
                exportOptions = list(modifier = list(page = 'all'))
              )
              
            )
          ))
        
        
      })

      output$redFlagBarChartSummarybyMonth <- renderPlotly({
        
       if(nrow( red_flagging_all %>%
                filter(red_flagged == "Yes")) <=0){
         return()
       }
        
        summary <-red_flagging_all %>%
          filter(red_flagged == "Yes") %>%
          group_by(reporting_month) %>%
          summarise(
            Alarm = sum(Alarm),
            Alert = sum(Alert),
            Normal = sum(Normal),
            RedFlags  = n(),
          )%>% 
          select(reporting_month,RedFlags)%>%
          arrange((reporting_month))%>%
          mutate(reporting_month = as.character(as.yearmon(reporting_month)))
        
        summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
        
        
        max_value <- max(summary$RedFlags, na.rm = TRUE)
        x_range <- c(0, max_value * 1.1)  # Extend x-axis by 10%
        
        p <- ggplot(summary) +
          geom_line(aes(x = reporting_month, y = RedFlags, group = 1), color = "#ED7667") +
          geom_point(aes(x = reporting_month, y = RedFlags), color = "#00A65A", size = 3) +  # Corrected size
          geom_text(
            aes(x = reporting_month, y = RedFlags, label = RedFlags),
            nudge_y = 0.5,  # Push text above points
            size = 5,
            check_overlap = TRUE
          ) +
          scale_y_continuous(
            breaks = seq(0, max(summary$RedFlags, na.rm = TRUE), by = 1),  # Ensures only whole numbers
            labels = scales::number_format(accuracy = 1),  # Removes decimals
            expand = expansion(mult = c(0.05, 0.15))  # Maintain buffer space at the top
          ) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text.y = element_text(angle = 0, hjust = 0),  # Prevent text cutoff
            strip.placement = "outside"  # Keeps facet labels outside the plot
          ) +
          labs(
            title = "Red Flags Reported Per Month",
            x = NULL,
            y = NULL
          ) +
          scale_fill_brewer(palette = "Set2")  # Improve color distinction
        
        ggplotly(p)  # Convert ggplot to interactive plotly chart
        
        
      })
      
      output$redFlagBarChartSummarybyMonthTable <- renderDT({
        summary <-red_flagging_all %>%
          filter(red_flagged == "Yes") %>%
          group_by(reporting_month) %>%
          summarise(
            Alarm = sum(Alarm),
            Alert = sum(Alert),
            Normal = sum(Normal),
            RedFlags  = n(),
            distrcis = paste(District_Name,collapse = ", ")
          )%>% 
          select(reporting_month,RedFlags,distrcis,Alarm,Alert)%>%
          arrange((reporting_month))
        
        datatable(
          summary,
          extensions = 'Buttons', 
          options = list(
            scrollX = TRUE, 
            pageLength = 7,
            dom = 'rtip'
          ))
        
        
      })
      
      # output$displayRedFlagTable <- renderDT(server = FALSE,{
      #   summary <- red_flagging %>%
      #     mutate(
      #       reporting_month = as.character(reporting_month),
      #       shock_occurence = ifelse(shock_occurence == "1","Yes","No"),
      #       shock_effect = ifelse(shock_occurence == "1","Yes","No"),
      #       variable_score_percentage = paste(variable_score_percentage,"%")) %>%
      #     select(red_flagged,reporting_month,Member_name  , FMS,Region_name , District_Name, District_Name, shock_occurence , shock_effect , variable_score,variable_score_percentage ,classify,Alarm ,Normal ,Alert)
      #   
      #   datatable(
      #     summary,
      #     extensions = 'Buttons', 
      #     options = list(
      #       pageLength = 20, 
      #       
      #       dom = 'lBfrtip',
      #       buttons = list(
      #         list(
      #           extend = 'csv',
      #           filename = "Red-flagging",
      #           title = "Red flagging",
      #           exportOptions = list(modifier = list(page = 'all'))
      #         ),
      #         list(
      #           extend = 'excel',
      #           filename = "Red-flagging",
      #           title = "Red flagging",
      #           exportOptions = list(modifier = list(page = 'all'))
      #         ),
      #         list(
      #           extend = 'pdf',
      #           filename = "Red-flagging",
      #           title = "Red flagging",
      #           exportOptions = list(modifier = list(page = 'all')),
      #           customize = JS(
      #             "function(doc) {",
      #             "doc.content.splice(0, 0, {",
      #             "text: 'Red flagging',",
      #             "fontSize: 18,",
      #             "alignment: 'center'",
      #             "});",
      #             "}"
      #           )
      #         ),
      #         list(
      #           extend = 'print',
      #           filename = "Red-flagging",
      #           title = "Red flagging",
      #           exportOptions = list(modifier = list(page = 'all'))
      #         )
      #         
      #       )
      #     ),
      #     filter = 'top',class = "redFlagTbaleID")%>%
      #     formatStyle(
      #       'red_flagged',
      #       backgroundColor = styleEqual(c('Yes', 'No'), 
      #                                    c("#dc3545","#00B050"))
      #     )
      # })
      
    
      output$weightingScoreSummary <- renderPlotly({
        summary <- red_flagging %>%
          group_by(District_Name) %>%
          summarise(
            red_flagged = first(red_flagged),
            count = n(),
            variable_score = sum(variable_score),
            maximum_results = ifelse(District_Name %in% reverine_districts , 57.6, 55.2),
            variable_score_percentage = round((variable_score/(maximum_results*count))*100,1)
          )%>%select(-maximum_results)%>%
          arrange(desc(variable_score_percentage))
        

        
        summary$color <- ifelse(summary$variable_score_percentage <= 24, "#00A65A",  # Green
                                ifelse(summary$variable_score_percentage <= 49, "#F99D1E",  # Red
                                       "#ED7667"))  # Yellow
        
        # Create the bar chart with dynamic colors
        fig <- plot_ly(summary, 
                       x = ~District_Name, 
                       y = ~variable_score_percentage, 
                       type = 'bar', 
                       name = 'Variable Scoring Percentage', 
                       marker = list(color = ~color), # Apply dynamic colors
                       text = ~paste(variable_score_percentage, "%", sep = ""), 
                       textposition = 'outside') %>% 
          layout(
            title =paste("Risk Score at",input$fromDateController),
            xaxis = list(categoryorder = "total descending"), 
            yaxis = list(title = 'Variable Scoring Percentage'), 
            barmode = 'group', 
            legend = list(orientation = "h", x = 0, y = 1.1, xanchor = 'left', yanchor = 'top')
          )
        
        fig
        
        
      })
      
      output$overallRiskClasification<-renderPlotly({
        data <- red_flagging_all%>%
          group_by(District_Name,reporting_month,classify)%>%
          summarise(n=n())
        
        # Define color mapping
        status_colors <- c("Normal" = "#00A65A", "Alert" = "#F99D1E", "Alarm" = "#ED7667")
        
        height <- length(unique(data$reporting_month))* 200
        
        runjs(paste0("document.getElementById('ewea-overallRiskClasification').style.height = '",height,"px';"))
        # Plot
        ggplotly(ggplot(data, aes(x = District_Name, y = reporting_month, fill = classify)) +
                   geom_tile(color = "black") +  # Creates the table grid
                   geom_text(aes(label = classify), size = 3) +  # Display text inside the cells
                   scale_fill_manual(values = status_colors) +  # Apply custom colors
                   scale_x_discrete(position = "top") +  # Move x-axis labels to the top
                   theme_minimal() +
                   theme(
                     axis.text.x = element_text(size = 12, face = "bold", angle = 90, hjust = 0),  # Rotate labels for readability
                     axis.text.y = element_text(size = 12, face = "bold"),  # Rotate labels for readability
                     axis.title = element_blank(),  # Remove axis titles
                     panel.grid = element_blank(),  # Remove background grid
                     legend.position = "none"  # Hide legend (optional)
                   ))%>%
        layout(
          
          xaxis = list(side = "top")  # Move x-axis labels to the top
        )
      })
      
      output$summarisetheoverallclassification <-renderUI({
        data <- red_flagging_all%>%
          group_by(District_Name,reporting_month,classify)%>%
          summarise(n=n())
        data$reporting_month <- as.Date(paste0(data$reporting_month, "-01"), format = "%Y-%m-%d")  # Append "-01" and convert to Date
        data$reporting_month <- format(data$reporting_month, "%B %Y")
      
        HTML(paste("<span class='text-primary font-16'>The table below summarizes the trends in the overall classification for all the 19 districts over ",
              length(unique(data$reporting_month)),"months <br> ("
              ,paste(unique(as.character(data$reporting_month)), collapse = ", "),")</span>"))
      })
      
      output$districtVariableAnalysis <- renderPlotly({
        
        req(input$variableController,input$districtDropDown)
        ewea1 <- main_ewea_all%>%filter(variable == input$variableController &
                                      District_Name ==   input$districtDropDown )
        
        if(can_convert_to_numeric(ewea1$value)){
          tryCatch({
            summary <- ewea1 %>%
              drop_na(value) %>%
              group_by(reporting_month ,status ) %>%
              reframe(
                value = as.numeric(value),
                other_info1 = first(other_info1),
                content1 = first(content1),
                other_info2 = first(other_info2),
                content2 = first(content2),
                District_Name = first(District_Name),
                count = n()
                
              )%>%
              arrange(as.yearmon(reporting_month))%>%
              mutate(reporting_month = as.character(as.yearmon(reporting_month)))%>%
              as.data.frame()
            
            title <- sub("_"," ",input$variableController)
            
            
            
            alarm <- if(length(as.numeric(summary[summary$status == "Alarm","value"]))>0){
              min(
                as.numeric(summary[summary$status == "Alarm","value"])
              )
            }else{
              0
            }
              
            alert <- if(length(as.numeric(summary[summary$status == "Alert","value"]))>0){
              min(
                as.numeric(summary[summary$status == "Alert","value"])
              )
            }else{
              0
            }
            
            normal <- if(length(as.numeric(summary[summary$status == "Normal","value"]))>0){
              min(
                as.numeric(summary[summary$status == "Normal","value"])
              )
            }else{
              0
            }
            
          
            summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
            
            ggplotly(
              ggplot(summary,aes(x = reporting_month, y = value)) +
                
                geom_line(aes(group = 1),color="#00A65A") + 
                geom_point(aes(color = status),size = 5) + 
                scale_color_manual(values = c("Normal" = "#00A65A", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +
                geom_text(aes(x = reporting_month, y = value, label = value),
                          vjust = -0.5, position = position_dodge(width = 0.9),size=5)+
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      
                      plot.background = element_rect(fill = "white", colour = "white")) +
                labs(
                  title = paste("Monthly ",title," at ", input$districtDropDown),
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
            summary <- ewea1 %>%
              drop_na(value) %>%
              group_by(reporting_month ,value ) %>%
              reframe(
                status = first(status),
                other_info1 = first(other_info1),
                content1 = first(content1),
                other_info2 = first(other_info2),
                content2 = first(content2),
                District_Name = first(District_Name),
                count = n()
              )%>%
              arrange(as.yearmon(reporting_month))%>%
              mutate(reporting_month = as.character(as.yearmon(reporting_month))
              )%>%
              as.data.frame()
            
            summary$truncated <- sapply(summary$value , trunc)
            summary$hover_text <- with(summary, paste('District Name:', District_Name, '<br>',
                                                      'Count:', count, '<br>',
                                                      'Status:', status))
            
            title <- sub("_"," ",input$variableController)
            
            summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
            
            p <- ggplot(summary, aes(x = reporting_month, y = truncated, label = value, text = hover_text)) +
              geom_tile(width = 0.8, height = 0.5, aes(fill = status)) +
              geom_line(aes(x = reporting_month, y = truncated, group = reporting_month), color = "#40419A") +
              
              labs(
                title = paste("Monthly ", title, " at ", input$districtDropDown),
                x = "",
                y = "Value"
              ) +
              scale_fill_manual(values = c("Normal" = "#00A65A", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +
              theme_minimal() +
              theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 14)  # Optional: Adjust title font size
              )
            
            ggplotly(p)
          }, error = function(e) {
            message("Not able to visualize data")
          })
          
          
        }
      })
      
      output$districtsbyvariable <- renderPlotly({
        
        req(input$variableController)
        ewea1 <- main_ewea_all%>%filter(variable == input$variableController )
        
        
        
        if(can_convert_to_numeric(ewea1$value)){
          
          summary <- ewea1 %>%
            drop_na(value) %>% 
            group_by(District_Name ,reporting_month,status) %>%
            reframe(
              value = mean(as.numeric(value)),
              count = n()
            )%>%
            dplyr::rename(District_Name = District_Name)%>%
            arrange((reporting_month))%>%
            mutate(reporting_month = as.character(as.yearmon(reporting_month)))
          
          summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
      
          
          height <- length(unique(summary$District_Name))* 215
          
          runjs(paste0("document.getElementById('ewea-districtsbyvariable').style.height = '",height,"px';"))
       
          
          p <- ggplot(summary) +
            geom_line(aes(x = reporting_month, y = value,group=1), color = "#ED7667") +
            geom_point(aes(x = reporting_month, y = value,color = status),size = 3) +
            scale_color_manual(values = c("Normal" = "#00A65A", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +
            geom_text(
              aes(x = reporting_month, y = value, label = value),
              nudge_y = 0.5,  # Push text above points
              size = 4,
              check_overlap = TRUE  # Prevents overlapping text
            ) +
            facet_grid(rows = vars(District_Name), scales = "free") +  # Facet by districtswitch = "y"  # Moves facet labels to the left
            scale_y_continuous(
              breaks = seq(0, max(summary$count, na.rm = TRUE), by = 1),  # Ensures only whole numbers
              labels = scales::number_format(accuracy = 1),  # Removes decimals
              expand = expansion(mult = c(0.05, 0.15))  # Maintain buffer space at the top
            )+
            theme(
              panel.spacing.y = unit(0.1, "lines"),
              legend.position = "top",
              axis.text.x = element_text(angle = 45, hjust = 1),
              strip.text.y = element_text(angle = 0, hjust = 0, margin = margin(l = 10, r = 10)),  # Prevent text cutoff
              strip.placement = "outside",  # Keeps facet labels outside the plot
              plot.margin = margin(20, 200, 20, 20)  # Increased right margin to avoid text overflow
            ) +
            labs(
              title = paste(input$variableController, "over the districts"),
              x = "",
              y = "",
              fill=""
            ) +
            coord_cartesian(clip = "off")   # Allow labels outside plot area

          
          ggplotly(p)
          
          
          
          
          
        }else{
          summary <- ewea1 %>%
            drop_na(value) %>%
            group_by(reporting_month,District_Name ,value ) %>%
            reframe(
              status = first(status),
              count = n(),
            )%>%
            dplyr::rename(District_Name=District_Name)%>%
            arrange((reporting_month))%>%
            mutate(reporting_month = as.character(as.yearmon(reporting_month)))
          
          summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
          
          
          
    
          # Calculate midpoint positions for each stacked segment
           
          height <- length(unique(summary$District_Name))* 215
          
          runjs(paste0("document.getElementById('ewea-districtsbyvariable').style.height = '",height,"px';"))

          # Generate plot
            p <- ggplot(summary) +
              geom_tile(width = 0.8, height = 0.5, aes(fill = status,x = reporting_month, y = value, label = value)) +
              geom_line(aes(x = reporting_month, y = value, group = reporting_month), color = "#40419A") +
              scale_fill_manual(values = c("Normal" = "#00A65A", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +
              facet_grid(rows = vars(District_Name), scales = "free") +  # Facet by district
            theme(
              panel.spacing.y = unit(0.1, "lines"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              strip.text.y = element_text(angle = 0, hjust = 0, margin = margin(l = 10, r = 10)),  # Prevent text cutoff
              strip.placement = "outside",  # Keeps facet labels outside the plot
              plot.margin = margin(20, 200, 20, 20)  # Increased right margin to avoid text overflow
            ) +
            labs(
              legend.position = "top",
              
              title = paste("District Level Analysis of", input$variableController),
              x = "",
              y = "",
              fill=""
            ) +
            coord_cartesian(clip = "off")  # Allow overflow for labels
  
          
          ggplotly(p)
          
        }
      })
      
      
      
      output$alarmCasesPerVariable <- renderPlotly({
        summary <-main_ewea_all %>%
          group_by(reporting_month,variable , status) %>%
          reframe(
            district = first(District_Name),
            variable_type = first(indicator_type),
            indicator = first(indicator),
            value = n()
          ) %>%
          filter(status == "Alarm")%>%
          arrange((reporting_month))%>%
          mutate(reporting_month = as.character(as.yearmon(reporting_month)))
        
        summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
        
        
        
        dis <- ifelse(length(unique(summary$district)) > 1,'All Districts', unique(summary$district))
        
        height <- length(unique(summary$variable))* 205
        
        runjs(paste0("document.getElementById('ewea-alarmCasesPerVariable').style.height = '",height,"px';"))
        
        p <- ggplot(summary) +
          geom_line(aes(x = reporting_month, y = value,group=1), color = "#ED7667") +
          geom_point(aes(x = reporting_month, y = value), color = "#00A65A") +
          geom_text(
            aes(x = reporting_month, y = value, label = value),
            vjust = -0.5, 
            
            size = 4, 
            position = position_nudge(y = 0.05 * max(summary$value)),
            check_overlap = TRUE 
          ) +
          facet_grid(rows = vars(variable), scales = "free") +
          scale_y_continuous(
            breaks = seq(0, max(summary$value, na.rm = TRUE), by = 1),
            labels = scales::number_format(accuracy = 1),
            expand = expansion(mult = c(0.05, 0.15))
          )+
          theme(
            panel.spacing.y = unit(0.1, "lines"),
            legend.position = "top",
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text.y = element_text(angle = 0, hjust = 0, margin = margin(l = 10, r = 10)),
            strip.placement = "outside", 
            plot.margin = margin(20, 200, 20, 20)
          ) +
          labs(
            title = paste("Alarm Cases Per Variable in",dis),
            x = "",
            y = "",
            fill=""
          ) +
          coord_cartesian(clip = "off")   # Allow labels outside plot area
        
        
        ggplotly(p)
      })
      
      
      output$variableAnalysis <- renderDT({
        
        ewea1 <- main_ewea
        
        summaryTable <-ewea1 %>%
          group_by(reporting_month,District_Name , variable , status) %>%
          reframe(
            variable_type = first(indicator_type),
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
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
                exportOptions = list(modifier = list(page = 'all'))
              ),
              list(
                extend = 'excel',
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
                exportOptions = list(modifier = list(page = 'all'))
              ),
              list(
                extend = 'pdf',
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
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
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
                exportOptions = list(modifier = list(page = 'all'))
              )
              
            )
          ))
        
        
      })
      
      
      output$datebyvariable <- renderPlotly({
        req(input$variableController)
        ewea1 <- main_ewea%>%filter(variable == input$variableController )
        
        if(can_convert_to_numeric(ewea1$value)){
          
          summary <- ewea1 %>%
            drop_na(value) %>% 
            group_by(reporting_month ,value,status ) %>%
            reframe(
              value = mean(as.numeric(value)),
              District_Name = first(District_Name),
              count = n()
            )%>%arrange(as.yearmon(reporting_month))%>%
            mutate(reporting_month = as.character(as.yearmon(reporting_month)))
          
          summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
          
          
          totals <- summary %>%
            group_by(reporting_month, status) %>%
            summarise(total = round(mean(as.numeric(value)),2),
                      `status count` = sum(count))
          
          summary$hover_text <- with(summary, paste('District Name:', District_Name, '<br>',
                                                    'Count:', count, '<br>',
                                                    paste('Avarage ',status, ":"), round(value,3), '<br>',
                                                    'Status:', status))
          
          
         # Calculate label positions with buffer
            totals <- totals %>%
            group_by(status) %>%
            mutate(
              label_y = total + (0.08 * max(total))  # 8% buffer above highest value in each status group
            ) %>%
            ungroup()
          
            p <- ggplot(totals) +
            geom_line(aes(x = reporting_month, y = total, group = status), color = "#ED7667") +
            geom_point(aes(x = reporting_month, y = total, group = status), color = "#00A65A") +
            geom_text(
              aes(x = reporting_month, y = label_y, label = total),
              vjust = 0,        # Start at buffer position
              size = 5,
              angle = 45,       # Diagonal labels for compact fit
              check_overlap = TRUE
            ) +
            facet_grid(rows = vars(status), scales = "free") +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.margin = margin(30, 40, 20, 20)  # Increased top and right margins
            ) +
            labs(
              title = paste("Average", input$variableController, "over the districts"),
              x = "",
              y = "Value"
            ) +
            coord_cartesian(clip = "off") +
            scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))  # 15% top buffer
          
          
          ggplotly(p)
          
          
          
          
          
        }else{
          summary <- ewea1 %>%
            drop_na(value) %>%
            group_by(reporting_month ,value ,status ) %>%
            reframe(
             
              District_Name = first(District_Name),
              count = n(),
            )%>%arrange(as.yearmon(reporting_month))%>%
            mutate(reporting_month = as.character(as.yearmon(reporting_month)))
          
          summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
          
          totals <- summary %>%
            group_by(reporting_month, status) %>%
            summarise(total = sum(count))
          
          summary$hover_text <- with(summary, paste('District Name:', District_Name, '<br>',
                                                    'Count:', count, '<br>',
                                                    'Value:', value, '<br>',
                                                    'Status:', status))
          
          
          # Calculate midpoints for stacked bars
            summary <- summary %>%
            group_by(reporting_month, status) %>%
            arrange(desc(value)) %>%  # Match stacking order
            mutate(
              label_pos = cumsum(count) - (count / 2)  # Center position calculation
            ) %>%
            ungroup()
          
            p <- ggplot(summary) +
            geom_bar(stat = "identity", aes(x = reporting_month, y = count, fill = value)) +
            geom_text(
              aes(x = reporting_month, y = label_pos, label = count),  # Use calculated midpoints
              size = 5,
              color = "black",  # Better contrast
              vjust = 0.5  # Perfect vertical centering
            ) +
            facet_grid(rows = vars(status), scales = "free") +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.margin = margin(20, 40, 20, 20)  # Add right margin space
            ) +
            labs(
              title = paste("Monthly Analysis of", input$variableController),
              x = "",
              y = "Value"
            ) +
            coord_cartesian(clip = "off") +  # Allow label overflow
            scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Add 10% headroom
          
          
          # +
          #   geom_line(data = totals , aes(x = reporting_month, y = total, group = status), color = "blue")
          
          ggplotly(p)
          
        }
        
        
        
        
        
        
      })
      
      output$variableReportTable <- renderDT(server = FALSE,{
        
          main_ewea%>%select(District_Name,reporting_month,variable_desc , value,status,content2,content1,other_info1,other_info2)%>%
          mutate(
            badge_class = case_when(
              status == "Alarm" ~ "danger",
              status == "Alert" ~ "warning",
              status == "Normal" ~ "success",
              TRUE ~ "secondary"
            ),
            value = case_when(
              variable_desc == "Coping strategies" ~ paste0(
                "<ul>",
                "<span class='badge bg-", badge_class, "'>", status, "</span>",
                paste0("<li>", str_replace_all(content2, ",\\s*", "</li><li>")), "</li>",
                "</ul>"
              ),
              variable_desc == "AWD/sus Cholera Cases under 5" &  grepl("Double", content1) ~ paste(
                value ,content1, paste("(Previous Two week cases were ",content2,")"),sep = "<br>"
              ),
              TRUE ~ paste(
                sep = "<br>",
                paste0(
                  value, " <span class='badge bg-", badge_class, "'>", status, "</span>"
                ),
                ifelse(other_info1 != "", paste0(other_info1, ": ", content1), ""),
                ifelse(other_info2 != "", paste0(other_info2, ": ", content2), "")
              )
            ),
           
          )%>%
          select(District_Name,variable_desc,value)%>%
          pivot_wider(id_cols =variable_desc , names_from = District_Name ,values_from = value )%>%
          datatable(escape = FALSE,class = 'cell-border stripe', options = list(
            scrollX = TRUE,
            pageLength = 100,
            dom = 'lBfrtip',
            buttons = list(
              list(
                extend = 'csv',
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
                exportOptions = list(modifier = list(page = 'all'))
              ),
              list(
                extend = 'excel',
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
                exportOptions = list(modifier = list(page = 'all'))
              ),
              list(
                extend = 'pdf',
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
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
                filename = "Variable Analaysis",
                title = "Variable Analaysis",
                exportOptions = list(modifier = list(page = 'all'))
              )
              
            )
          ))
      })
      
      #------------------------------------- district levels:
      
      
      
      
      
     

      
      output$downloadButtonThreshold <-  downloadHandler(
        filename = function() {
          # URL-encode spaces in filename
          URLencode("Early Warning Indicators and Thresholds.pdf")
        },
        content = function(file) {
          # Use absolute path construction
          file_path <- here::here("downloads", "ewea threshold and indicators.pdf")
          
          # Debugging checks
          message("File path: ", file_path)
          message("File exists? ", file.exists(file_path))
          
          if (!file.exists(file_path)) {
            stop("File not found at: ", file_path)
          }
          
          # Copy with explicit MIME type
          file.copy(file_path, file)
        },
        contentType = "application/pdf"  # Critical for PDF recognition
      )
      
      
    }, error = function(e) {
      output$messageBox <- renderUI({
        print(e$message)
        HTML(paste(sep = "",
                   '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bxs-message-square-x"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-white">Error</h6>
											<div class="text-white">',e$message,' Error on analysing , please contact BRCIS CMU MEL for any support</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>' 
        ))
      })
      print(e$message)
    })
  
    
  })
  
  
  

  
  
}

