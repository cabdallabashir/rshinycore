
districts_fms <- readxl::read_excel("docs/ewea/ewea communities districts and fms.xlsx")%>%
  as.data.frame()


districts_fms%>%
  filter(District_Name == "Jariiban")
segrationDropdown <- names(districts_fms[c("FMS",	"Region_name",	"District_Name" , "Community_name","Member_name")])
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



new_eweaUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/new_ewea.html",
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
       
                 FMSController = uiOutput(ns("FMSControllerFill")),
                 
                 regionController = uiOutput(ns("regionControllerFill")),
                 
                 districtController = uiOutput(ns("districtControllerFill")),
                 
                 communityController = uiOutput(ns("communityControllerFill")),
                 
                 districtAggregation = checkboxInput(
                   inputId = ns("districtAggregation"),
                   label   = tags$span("Aggregate Results in district level", class = "font-weigth-bold text-primary"),
                   value   = FALSE
                 ),
                 
                 total_alarms = uiOutput(ns("total_alarms")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 total_alerts = uiOutput(ns("total_alerts")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 total_normals = uiOutput(ns("total_normals")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 CommunityDistrictOverview = DTOutput(ns("CommunityDistrictOverview")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 RedFalggedDistrictMap = leafletOutput(ns("displayRedFlaggedMap")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 redFlagBarChartSummarybyMonth  = plotlyOutput(ns("redFlagBarChartSummarybyMonth"),height = "100%")%>% withSpinner(size=0.5,proxy.height = "50px"),
                 # displayRedFlagTable = DTOutput(ns("displayRedFlagTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 weightingScoreSummary = plotlyOutput(ns("weightingScoreSummary"),height = "100%")%>%withSpinner(size=0.5,proxy.height = "50px"),
                 variableController = uiOutput(ns("variableControllerFill"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 communityDropDown = uiOutput(ns("communityDropDownFill"))%>% withSpinner(size=0.5,proxy.height = "50px"),

                 datebyvariable = plotlyOutput(ns("datebyvariable")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 communityVariableAnalysis = plotlyOutput(ns("communityVariableAnalysis"),height = "100%") %>% withSpinner(size=0.5,proxy.height = "50px"),
                 variableReportTable = DTOutput(ns("variableReportTable")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 top3DistrictwithNormal= uiOutput(ns("top3DistrictwithNormal")),
                 redFlagStatusByDistrict = plotlyOutput(ns("redFlagStatusByDistrict"),height = "2000px") %>% withSpinner(size=0.5,proxy.height = "50px"),
                 whereWeWokMap = leafletOutput(ns("whereWeWokMap")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 downloadButtonThreshold = downloadButton(class ="btn btn-success",outputId = ns("downloadButtonThreshold"),label =  "Early Warning Indicators and Thresholds"),
                 factorsContributingRedFlagging = DTOutput(ns("factorsContributingRedFlagging")) %>% withSpinner(size=0.5,proxy.height = "50px"),
                 mostDominantHazards = uiOutput(ns("mostDominantHazards"))%>% withSpinner(size=0.5,proxy.height = "50px"),
    )
  )
  
}



new_ewea <- function(input ,output , session,sharedValues){
  ns1 <- NS("new_ewea")
  color_mapping <- setNames(c("#ED7667","#F99D1E","#00A65A"), c("Alarm" ,"Alert", "Normal"))

  get_color <- function(classify) {
    return(color_mapping[classify])
  }
  
  

  
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

    global_vars$global_ewea =  readTable(postgresConnection,"ewea_community")
    global_vars$global_shock_occurence = readTable(postgresConnection,"shock_occurence_community")
    global_vars$global_shock_effect = readTable(postgresConnection,"shock_effect_community")
    global_vars$global_red_flagging = readTable(postgresConnection,"red_flagging_community")
    global_vars$global_market_threshold = readTable(postgresConnection,"market_threshold_community")
    global_vars$global_ewea_weighting_scores = readTable(postgresConnection,"ewea_weighting_scores_community")
    
    aggregated_main_ewea <- readTable(postgresConnection,"ewea_community")
    
    dbDisconnect(postgresConnection)
    
    
    
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
  
  
  output$FMSControllerFill <- renderUI({
    fms <- districts_fms%>%distinct(FMS)%>%pull(FMS)
    
    selectInput(
      ns1("FMSController"),
      paste("FMS"),
      choices = c("all",fms),
      multiple = TRUE,
      selected = "all"
    )
    
  })
  
  output$regionControllerFill <- renderUI({
    
    regions <- districts_fms
    
    if(is.null(input$FMSController) || "all" %in% input$FMSController) {
      regions <- regions
    }else{
      regions <- regions %>%
        filter(FMS %in% input$FMSController)
    }
    
    regions <- regions%>%distinct(Region_name)%>%pull(Region_name)
    
    selectInput(
      ns1("regionController"),
      paste("Region"),
      choices = c("all",regions),
      multiple = TRUE,
      selected = "all"
    )
  })
  
 
  
  output$districtControllerFill <- renderUI({
    
    districts <- districts_fms
    
  
    if(is.null(input$FMSController) || "all" %in% input$FMSController) {
      districts <- districts
    }else{
      districts <- districts %>%
        filter(FMS %in% input$FMSController)
    }
    
    if(is.null(input$regionController) || "all" %in% input$regionController) {
      districts <- districts
    }else{
      districts <- districts %>%
        filter(Region_name %in% input$regionController)
    }
    
    districts <- districts%>%distinct(District_Name)%>%pull(District_Name)
    
    selectInput(
      ns1("districtController"),
      paste("District"),
      choices = c("all",districts),
      multiple = TRUE,
      selected = "all"
    )
  })
  
  output$communityControllerFill <- renderUI({
    
    
    communities <- districts_fms
    
    
    
    if(is.null(input$FMSController) || "all" %in% input$FMSController) {
      communities <- communities
    }else{
      communities <- communities %>%
        filter(FMS %in% input$FMSController)
    }
    
    if(is.null(input$regionController) || "all" %in%  input$regionController) {
      communities <- communities
    }else{
      communities <- communities %>%
        filter(Region_name %in% input$regionController)
    }

    if(is.null(input$districtController) || "all" %in% input$districtController) {
      communities <- communities 
    }else{
      communities <- communities %>%
        filter(District_Name %in% input$districtController)
    }
    
    communities <- communities%>%distinct(Community_name)%>%pull(Community_name)
    
    selectInput(
      ns1("communityController"),
      paste("Community"),
      choices = c("all",communities),
      multiple = TRUE,
      selected = "all"
    )
  })
  
  # which multi-selects should enforce "all" logic
  ids <- c("communityController", "districtController", "regionController", "FMSController")
  
  # create an observer for each control
  lapply(ids, function(id) {
    observeEvent(input[[id]], ignoreInit = TRUE, {
      sel <- input[[id]]
      if (is.null(sel)) return()
      
      if ("all" %in% sel && length(sel) > 1) {
        updateSelectInput(session, id, selected = setdiff(sel, "all"))
        # If you're using selectizeInput or pickerInput, swap the updater:
        # updateSelectizeInput(session, id, selected = setdiff(sel, "all"))
        # updatePickerInput(session, id, selected = setdiff(sel, "all"))
      }
    }, label = paste0("obs_", id))
  })
  
  output$fromDateControllerFILL <- renderUI({
    selectInput(
      ns1("fromDateController"),
      paste("Date"),
      choices = unique(sort(as.yearmon(global_vars$global_ewea%>%filter(Residence_type == input$ruralUrbanController)%>%pull(reporting_month)))),
      selected = max(as.yearmon(global_vars$global_ewea%>%filter(Residence_type == input$ruralUrbanController)%>%pull(reporting_month)))  # Selects the latest month
    )
    
  })
  
  observeEvent(input$ruralUrbanController,{
    sub_global <- global_vars$global_ewea%>%filter(Residence_type == input$ruralUrbanController) 
    output$fromDateControllerFILL <- renderUI({
      selectInput(
        ns1("fromDateController"),
        paste("Date"),
        choices = unique(sort(as.yearmon(sub_global$reporting_month))),
        selected = max(as.yearmon(sub_global$reporting_month))  # Selects the latest month
      )
      
    })
    
   
  })
  
  observeEvent(input$districtAggregation , {
    print(input$districtAggregation)
  })
  

  
  
  observe({
    tryCatch({
      
    
      
      if(is.null(nrow(global_vars$global_ewea)) || nrow(global_vars$global_ewea) == 0){
        return()
      }
      
      
      if( is.null(input$ruralUrbanController) ||
          is.null(input$FMSController) || 
          is.null(input$regionController) ||
          is.null(input$districtController) ||
          is.null(input$communityController) ||
          is.null(input$fromDateController)
          ){
        return()
      }
     
      print("here")
     
      
      
      red_flagging<- global_vars$global_red_flagging%>%filter(Residence_type == input$ruralUrbanController) 
      main_ewea <- global_vars$global_ewea%>%filter(Residence_type == input$ruralUrbanController) 
      
      # Find missing districts per month
      Communities_fms_sub <- districts_fms%>%filter(Residence_type == input$ruralUrbanController) 
      missing_communities_per_month <- main_ewea %>%
        group_by(reporting_month) %>%
        summarise(Communities_not_reported = list(setdiff(Communities_fms_sub$Community_name, Community_name))) %>%
        unnest(cols = c(Communities_not_reported))%>%
        left_join(Communities_fms_sub, by = c("Communities_not_reported" = "Community_name"))
      
      community_points <- somalia_districts_communities%>%filter(Residency == input$ruralUrbanController) 
     
      red_flagging_all <- red_flagging
      main_ewea_all <-main_ewea
     
      
      red_flagging%<>%mutate(reporting_month = as.yearmon(reporting_month)) %>%
        filter(as.Date(as.yearmon(reporting_month)) == as.Date(as.yearmon(input$fromDateController)))
    
      
      main_ewea%<>%mutate(reporting_month = as.yearmon(reporting_month)) %>%
        filter(as.Date(as.yearmon(reporting_month)) == as.Date(as.yearmon(input$fromDateController)))
      
      
      if(nrow(missing_communities_per_month)>0){
        missing_communities_per_month%<>%
          filter(as.Date(as.yearmon(reporting_month)) == as.Date(as.yearmon(input$fromDateController)))
        
        
      }
      
    
      red_flagging%<>%filter(strategy == "new")
      red_flagging_all%<>%filter(strategy == "new")

      
      
      # Treat NULL/empty or only "all" as no filter
      active_vals <- function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        if (length(x) == 1 && is.character(x) && tolower(x) == "all") return(NULL)
        x[x != "all"]
      }
      
      apply_all_filters <- function(df) {
        # columns allowed for the dynamic Segrigation filter
        allowed_cols <- c("Member_name", "FMS", "Region_name", "District_Name", "Community_name")
        
        # 1) Dynamic Segrigation (e.g., user chooses which column to filter)
        seg_vals <- active_vals(input$Segrigation_element_drill)
        seg_col  <- input$Segrigation
        if (!is.null(seg_vals) && !is.null(seg_col) &&
            seg_col %in% allowed_cols && seg_col %in% names(df)) {
          df <- dplyr::filter(df, .data[[seg_col]] %in% seg_vals)
        }
        
        # 2) Fixed controllers → fixed columns
        ctrl_map <- list(
          communityController = "Community_name",
          districtController  = "District_Name",
          regionController    = "Region_name",
          FMSController       = "FMS"
        )
        
        for (id in names(ctrl_map)) {
          vals <- active_vals(input[[id]])
          if (is.null(vals)) next
          col <- ctrl_map[[id]]
          if (col %in% names(df)) {
            df <- dplyr::filter(df, .data[[col]] %in% vals)
          }
        }
        
        df
      }
      
      # Apply to all your frames
      red_flagging        %<>% apply_all_filters()
      main_ewea           %<>% apply_all_filters()
      red_flagging_all    %<>% apply_all_filters()
      main_ewea_all       %<>% apply_all_filters()
      Communities_fms_sub %<>% apply_all_filters()
      community_points    %<>% apply_all_filters()
     
      output$communityDropDownFill <- renderUI({
        ns1 <- NS("new_ewea")
        
        selectInput(
          ns1("communityDropDown"),
          paste("Select Community"),
          choices = unique(main_ewea$Community_name)
        )
      })
      
      output$variableControllerFill <- renderUI({
        ns1 <- NS("new_ewea")
        
        selectInput(
          ns1("variableController"),
          paste("Select Variable"),
          choices = unique(main_ewea$variable)
        )
      })
      
      aggregated_red_flagging <- red_flagging
      aggregated_red_flagging_all <- red_flagging_all
      aggregated_main_ewea <- main_ewea
      
      if(input$districtAggregation){
        
       
        aggregated_red_flagging%<>%
          group_by(reporting_month, Member_name,      FMS  ,  Region_name ,District_Name  ,  Residence_type)%>%
          summarise(
            Community_name =first(District_Name),
            Alarm = round(mean(Alarm)),
            Normal = round(mean(Normal)),
            Alert = round(mean(Alert)),
            variable_score = round(mean(variable_score)),
            variable_score_percentage = round(mean(variable_score_percentage)),
            strategy = first(strategy),
            dominant_hazard = paste0(dominant_hazard,collapse = ",")
          )%>%
          mutate(
            classify = case_when(
              variable_score_percentage <= 24 ~ "Normal",
              variable_score_percentage > 24 & variable_score_percentage <= 49 ~ "Alert",
              variable_score_percentage > 49 ~ "Alarm"
            ),
            red_flagged = ifelse(variable_score_percentage> 49,"Yes","No")
          )%>%
          separate_rows(dominant_hazard,sep = ",")%>%
          mutate(
            dominant_hazard_status = str_extract(dominant_hazard, "(?<=-)\\s*[^-]+$"),
            dominant_hazard        = str_trim(str_remove(dominant_hazard, "-\\s*[^-]+$")),
            dominant_hazard_status = case_when(
              dominant_hazard_status == "Alarm" ~ "2",
              dominant_hazard_status == "Alert" ~ "1",
              .default = dominant_hazard_status
            )
          )%>%
          group_by(reporting_month, Member_name,FMS,Region_name, District_Name,Community_name,
                   Residence_type, classify, Alarm, Normal, Alert, variable_score,
                   variable_score_percentage, strategy, red_flagged,dominant_hazard)%>%
          summarise(
            dominant_hazard_status = round(mean(as.numeric(dominant_hazard_status)))
          )%>%
          mutate(
            dominant_hazard_status = case_when(
              dominant_hazard_status == "2" ~ "Alarm",
              dominant_hazard_status == "1" ~ "Alert",
              .default = as.character(dominant_hazard_status)
            )
          )%>%
          group_by(reporting_month, Member_name,FMS,Region_name, District_Name,Community_name,
                   Residence_type, classify, Alarm, Normal, Alert, variable_score,
                   variable_score_percentage, strategy, red_flagged)%>%
          summarise(
            dominant_hazard = paste0(dominant_hazard,'-',dominant_hazard_status, collapse = ",")
          )
        
        aggregated_red_flagging_all%<>%
          group_by(reporting_month, Member_name,      FMS  ,  Region_name ,District_Name  ,  Residence_type)%>%
          summarise(
            Community_name =first(District_Name),
            Alarm = round(mean(Alarm)),
            Normal = round(mean(Normal)),
            Alert = round(mean(Alert)),
            variable_score = round(mean(variable_score)),
            variable_score_percentage = round(mean(variable_score_percentage)),
            strategy = first(strategy),
            dominant_hazard = paste0(dominant_hazard,collapse = ",")
          )%>%
          mutate(
            classify = case_when(
              variable_score_percentage <= 24 ~ "Normal",
              variable_score_percentage > 24 & variable_score_percentage <= 49 ~ "Alert",
              variable_score_percentage > 49 ~ "Alarm"
            ),
            red_flagged = ifelse(variable_score_percentage> 49,"Yes","No")
          )%>%
          separate_rows(dominant_hazard,sep = ",")%>%
          mutate(
            dominant_hazard_status = str_extract(dominant_hazard, "(?<=-)\\s*[^-]+$"),
            dominant_hazard        = str_trim(str_remove(dominant_hazard, "-\\s*[^-]+$")),
            dominant_hazard_status = case_when(
              dominant_hazard_status == "Alarm" ~ "2",
              dominant_hazard_status == "Alert" ~ "1",
              .default = dominant_hazard_status
            )
          )%>%
          group_by(reporting_month, Member_name,FMS,Region_name, District_Name,Community_name,
                   Residence_type, classify, Alarm, Normal, Alert, variable_score,
                   variable_score_percentage, strategy, red_flagged,dominant_hazard)%>%
          summarise(
            dominant_hazard_status = round(mean(as.numeric(dominant_hazard_status)))
          )%>%
          mutate(
            dominant_hazard_status = case_when(
              dominant_hazard_status == "2" ~ "Alarm",
              dominant_hazard_status == "1" ~ "Alert",
              .default = as.character(dominant_hazard_status)
            )
          )%>%
          group_by(reporting_month, Member_name,FMS,Region_name, District_Name,Community_name,
                   Residence_type, classify, Alarm, Normal, Alert, variable_score,
                   variable_score_percentage, strategy, red_flagged)%>%
          summarise(
            dominant_hazard = paste0(dominant_hazard,'-',dominant_hazard_status, collapse = ",")
          )
        
        
        aggregated_main_ewea%<>%
          select(reporting_month, Member_name,      FMS  ,  Region_name ,District_Name,Community_name  ,  Residence_type,
                 variable_desc , status , status_code)%>%
          group_by(reporting_month, Member_name,      FMS  ,  Region_name ,District_Name  ,  Residence_type,variable_desc)%>%
          summarise(
            Community_name =first(District_Name),
            status_code = round(mean(status_code))
          )%>%
          mutate(
            status = case_when(
              status_code == 0 ~ "Normal",
              status_code == 1 ~ "Alert",
              status_code == 2 ~ "Alarm",
            )
          )
          
        
      }
    
      output$total_alarms <- renderUI({
        
        tot <- aggregated_red_flagging%>%filter(classify =="Alarm")%>%nrow()
        per = paste0(round((tot / length(aggregated_red_flagging$Community_name))*100),'%')
        paste(per ,"(",aggregated_red_flagging%>%filter(classify =="Alarm")%>%nrow(),")")
      })
      
      output$total_alerts <- renderUI({
        
        tot <- aggregated_red_flagging%>%filter(classify =="Alert")%>%nrow()
        per = paste0(round((tot / length(aggregated_red_flagging$Community_name))*100),'%')
        paste(per ,"(",aggregated_red_flagging%>%filter(classify =="Alert")%>%nrow(),")")
      })
      
      output$total_normals <- renderUI({
        
        tot <- aggregated_red_flagging%>%filter(classify =="Normal")%>%nrow()
        per = paste0(round((tot / length(aggregated_red_flagging$Community_name))*100),'%')
        paste(per ,"(",aggregated_red_flagging%>%filter(classify =="Normal")%>%nrow(),")")
      })
      
      output$mostDominantHazards <- renderUI({
        
        mostDominantHazards <- red_flagging
        mostDominantHazards <- mostDominantHazards%>%select(reporting_month ,Member_name    ,   FMS  ,  Region_name, District_Name ,  Community_name,classify,variable_score_percentage,dominant_hazard)%>%
          summarise(n=n(),dominant_hazard = paste0(dominant_hazard,collapse = ","),.groups = "drop")%>%
          separate_rows(dominant_hazard,sep = ",")%>%
          mutate(
            dominant_hazard_status = str_extract(dominant_hazard, "(?<=-)\\s*[^-]+$"),
            dominant_hazard        = str_trim(str_remove(dominant_hazard, "-\\s*[^-]+$")),
            dominant_hazard_status = case_when(
              dominant_hazard_status == "Alarm" ~ "2",
              dominant_hazard_status == "Alert" ~ "1",
              .default = dominant_hazard_status
            )
          )%>%
          group_by(dominant_hazard)%>%
          summarise(
            dominant_hazard_status = round(mean(as.numeric(dominant_hazard_status)))
          )%>%
          mutate(
            dominant_hazard_status = case_when(
              dominant_hazard_status == "2" ~ "Alarm",
              dominant_hazard_status == "1" ~ "Alert",
              .default = as.character(dominant_hazard_status)
            )
          )%>%
          drop_na(dominant_hazard)%>%
          filter(dominant_hazard != '')%>%
          summarise(
            dominant_hazard = paste0(dominant_hazard,'-',dominant_hazard_status, collapse = ",")
          )%>%
          mutate(
            dominant_hazard = sapply(dominant_hazard, function(x) {
              if (is.na(x) || !nzchar(x)) return(NA_character_)
              parts <- trimws(unlist(strsplit(x, ",")))
              parts <- parts[nzchar(parts)]
              
              # choose class by suffix, default to primary
              cls   <- ifelse(grepl("-Alarm$", parts, ignore.case = TRUE),  "danger",
                              ifelse(grepl("-Alert$", parts, ignore.case = TRUE), "warning", "primary"))
              
              # display label without the "-Alarm/-Alert" suffix
              labels <- sub("-(?i:Alarm|Alert)$", "", parts, perl = TRUE)
              
              spans <- paste0("<span class='badge bg-", cls, "'>", labels, "</span>")
              paste(spans, collapse = " ")
            })
          )%>%pull(dominant_hazard)
        
        HTML(paste(mostDominantHazards))
      })
      
      output$whereWeWokMap <- renderLeaflet({

        whereWeWokMap <- districts_fms%>%
          filter(Residence_type == input$ruralUrbanController)%>%
          group_by(District_Name)%>%
          summarise(
            Region_name = first(Region_name),
            Member_name = paste(Member_name,collapse = ","),
            Community_name = paste(Community_name,collapse = ",")
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

      output$displayRedFlaggedMap <- renderLeaflet({

        red_flagged_district_names_with_other_info <- red_flagging %>%
          group_by(District_Name) %>%
          summarise(
            Community = paste(paste(Community_name,"(",variable_score_percentage,"%) - ",classify),collapse = "<br>"),
            variable_score = round(mean(variable_score),1),
            variable_score_percentage = round(mean(variable_score_percentage),1),
            Member_name = paste(unique(Member_name),collapse = ",")
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
            district_status = case_when(
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



        not_reported_districts <- Communities_fms_sub%>%
          group_by(District_Name)%>%
          summarise(
            FMS = first(FMS),
            Region_name = first(Region_name),
            District_Name = first(District_Name),
            .groups = "drop"
          )%>%
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
                                      paste("<b>Overall Risk Score: </b>",paste0(red_flagged_district_names_with_other_info$variable_score_percentage , " (",red_flagged_district_names_with_other_info$district_status,")"),"<hr>") ,
                                      paste("<b>Communities",paste0(red_flagged_district_names_with_other_info$Community),"<hr>") ,
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
              position = "bottomright",
              colors = c("#fff","#40419A","#ED7667","#F99D1E","#00A65A","black"),
              labels = c("Out of scope districts","Districts not Reported","Alarmed Districts","Alerted Districts","Normal Districts","Communities"),
              title = "Risk Levels",
              opacity = 0.7
            )%>%
            addLayersControl(
              baseGroups = c("Satellite Map","Normal Map","CartoDB.Positron"),
              overlayGroups = red_flagged_district_names_with_other_info$District_Name,
              options = layersControlOptions(collapsed = TRUE),position = "bottomleft")


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

      output$redFlagBarChartSummarybyMonth <- renderPlotly({

       if(nrow( aggregated_red_flagging_all %>%
                filter(classify == "Alarm")) <=0){
         return()
       }

        cutoff <- as.Date(as.yearmon(input$fromDateController)) - years(1)
        

        summary <-aggregated_red_flagging_all %>%
          filter(classify == "Alarm") %>%
          group_by(reporting_month) %>%
          summarise(
            Alarm = sum(Alarm),
            Alert = sum(Alert),
            Normal = sum(Normal),
            RedFlags  = n(),
          )%>%
          select(reporting_month,RedFlags)%>%
          filter(between(as.Date(as.yearmon(reporting_month)), cutoff, as.Date(as.yearmon(input$fromDateController))))%>%
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
            title = "",
            x = NULL,
            y = NULL
          ) +
          scale_fill_brewer(palette = "Set2")  # Improve color distinction

        ggplotly(p)  # Convert ggplot to interactive plotly chart


      })
      
      output$CommunityDistrictOverview <- renderDT(server = FALSE , {
          datatable(escape = FALSE,
            aggregated_red_flagging%>%select(reporting_month ,Member_name    ,   FMS  ,  Region_name, District_Name ,  Community_name,classify,variable_score_percentage,dominant_hazard)%>%
              rename(Status = classify , `Risk Score`=variable_score_percentage , `Dominant Hazards` = dominant_hazard)%>%
              mutate(
                    Status = case_when(
                      Status == 'Alarm' ~ paste0("<span class='badge bg-danger'>", Status, "</span>"),
                      Status == 'Alert' ~ paste0("<span class='badge bg-warning'>", Status, "</span>"),
                      Status == 'Normal' ~ paste0("<span class='badge bg-success'>", Status, "</span>"),
                      .default = Status
                    ),
                    `Risk Score` = paste0(`Risk Score`,'%'),
                     reporting_month = format(reporting_month, "%Y-%m"),
                    `Dominant Hazards` = sapply(`Dominant Hazards`, function(x) {
                      if (is.na(x) || !nzchar(x)) return(NA_character_)
                      parts <- trimws(unlist(strsplit(x, ",")))
                      parts <- parts[nzchar(parts)]
                      
                      # choose class by suffix, default to primary
                      cls   <- ifelse(grepl("-Alarm$", parts, ignore.case = TRUE),  "danger",
                                      ifelse(grepl("-Alert$", parts, ignore.case = TRUE), "warning", "primary"))
                      
                      # display label without the "-Alarm/-Alert" suffix
                      labels <- sub("-(?i:Alarm|Alert)$", "", parts, perl = TRUE)
                      
                      spans <- paste0("<span class='badge bg-", cls, "'>", labels, "</span>")
                      paste(spans, collapse = " ")
                    })
            ),
            extensions = 'Buttons',
            options = list(
              scrollX = TRUE,
              paging = FALSE, 
              dom = 'rtip'
            ))
      })

      output$weightingScoreSummary <- renderPlotly({
        summary <- aggregated_red_flagging %>%
          arrange(desc(variable_score_percentage))



        summary$color <- ifelse(summary$variable_score_percentage <= 24, "#00A65A",  # Green
                                ifelse(summary$variable_score_percentage <= 49, "#F99D1E",  # Red
                                       "#ED7667"))  # Yellow

        # Create the bar chart with dynamic colors
        fig <- plot_ly(summary,
                       x = ~Community_name,
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
      
      output$factorsContributingRedFlagging <- renderDT(server = FALSE,{
        aggregated_red_flagging %>%
          select(Community_name,District_Name,reporting_month,red_flagged)%>%
          left_join(
            aggregated_main_ewea%>%select(Community_name,District_Name,reporting_month,variable_desc ,status)
          )%>%
          select(Community_name,District_Name,red_flagged,variable_desc,status)%>%
          pivot_wider(id_cols =Community_name:red_flagged , names_from = variable_desc ,values_from = status )%>%
          datatable(escape = FALSE, options = list(
            scrollX = TRUE,
            paging = FALSE, 
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
      
  
      
      output$redFlagStatusByDistrict <- renderPlotly({
        redFlagStatusByDistrict <- aggregated_red_flagging%>%
          select(Community_name,District_Name,Alarm, Normal ,Alert)%>%
          pivot_longer(cols = c(Alarm, Normal ,Alert) , names_to = "status",values_to = "status_value")%>%
          group_by(Community_name,District_Name,status)%>%
          reframe(
            count = sum(status_value)
          )
        
        if(!input$districtAggregation){
          runjs(paste0("document.getElementById('new_ewea-redFlagStatusByDistrict').style.height = '2000px';"))
        }else{
          runjs(paste0("document.getElementById('new_ewea-redFlagStatusByDistrict').style.height = '100%';"))
        }
        
        
        
        ggplotly(
          ggplot(redFlagStatusByDistrict, aes(x = count, y = Community_name, fill = status)) +
            geom_col() +
            geom_text(aes(label = count), hjust = -0.2, size = 4) +
            facet_grid(cols = vars(status), scales = "free") +  # Facet by 'status'
            theme_minimal() +
            labs(title = paste("Indicator categorization per status", input$fromDateController),
                 x = NULL, y = NULL) +
            theme(
              strip.text = element_text(face = "bold"),  # Make facet titles bold
              plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
            ) +
            scale_fill_manual(values = c("Alert" = "#F99D1E", "Normal" = "#00A65A", "Alarm" = "#ED7667")) +  # Apply custom colors
            xlim(0, max(redFlagStatusByDistrict$count) * 1.2)  # Extend x-axis to ensure text is visible
        )

      })
      
      output$communityVariableAnalysis <- renderPlotly({
        
        req(input$variableController,input$communityDropDown)
        ewea1 <- main_ewea_all%>%filter(variable == input$variableController &
                                          Community_name ==   input$communityDropDown )
        
        if(can_convert_to_numeric(ewea1$value)){
          summary <- ewea1 %>%
            drop_na(value) %>%
            group_by(reporting_month ,status ) %>%
            reframe(
              value = as.numeric(value),
              other_info1 = first(other_info1),
              content1 = first(content1),
              other_info2 = first(other_info2),
              content2 = first(content2),
              Community_name = first(Community_name),
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
                title = paste("Monthly ",title," at ", input$communityDropDown),
                x = "",           # Replace with your x-axis label
                y = "Value"           # Replace with your y-axis label
              )
          )
          
        }else{
          
          trunc <- function(s) {
            
            return(
              substr(s, 1, 20)
            )
          }
          
          summary <- ewea1 %>%
            drop_na(value) %>%
            group_by(reporting_month ,value ) %>%
            reframe(
              status = first(status),
              other_info1 = first(other_info1),
              content1 = first(content1),
              other_info2 = first(other_info2),
              content2 = first(content2),
              Community_name = first(Community_name),
              count = n()
            )%>%
            arrange(as.yearmon(reporting_month))%>%
            mutate(reporting_month = as.character(as.yearmon(reporting_month))
            )%>%
            as.data.frame()
          
          summary$truncated <- sapply(summary$value , trunc)
          summary$hover_text <- with(summary, paste('District Name:', Community_name, '<br>',
                                                    'Count:', count, '<br>',
                                                    'Status:', status))
          
          title <- sub("_"," ",input$variableController)
          
          summary$reporting_month <- factor(summary$reporting_month, levels = unique(summary$reporting_month))
          
          p <- ggplot(summary, aes(x = reporting_month, y = truncated, label = value, text = hover_text)) +
            geom_tile(width = 0.8, height = 0.5, aes(fill = status)) +
            geom_line(aes(x = reporting_month, y = truncated, group = reporting_month), color = "#40419A") +
            
            labs(
              title = paste("Monthly ", title, " at ", input$communityDropDown),
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
          
          
        }
      })
      
      output$variableReportTable <- renderDT(server = FALSE,{

          main_ewea%>%select(FMS  ,  Region_name, District_Name ,  Community_name,variable_desc , value,status,content2,content1,other_info1,other_info2)%>%
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
          select(FMS  ,  Region_name, District_Name ,  Community_name,variable_desc,value)%>%
          pivot_wider(id_cols =c(FMS  ,  Region_name, District_Name ,  Community_name) , names_from = variable_desc ,values_from = value )%>%
          datatable(escape = FALSE,class = 'cell-border stripe', options = list(
            scrollX = TRUE,
            paging = FALSE, 
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

