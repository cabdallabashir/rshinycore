ittdashUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/css/select2.min.css"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/js/select2.min.js"),
      tags$style(HTML("
  /* Define your theme vars (adjust as you like) */
  :root{
    --card-bg:#000;           /* black background */
    --card-border:#333;       /* subtle border */
    --text-primary:#fff;      /* white text */
  }

  /* Your wrapper style (kept) */
  .filter-select {
    padding: 0.5rem;
    background-color: var(--card-bg);
    border-radius: 0.375rem;
    color: var(--text-primary);
    font-size: 0.875rem;
  }

  /* Selectize (selectizeInput) */
  .filter-select .selectize-input {
    background-color: var(--card-bg) !important;
    color: var(--text-primary) !important;
    border-color: var(--card-border) !important;
    box-shadow: none !important;
  }
  .filter-select .selectize-input input {
    color: var(--text-primary) !important;
  }
  .filter-select .selectize-dropdown {
    background-color: var(--card-bg) !important;
    color: var(--text-primary) !important;
    border-color: var(--card-border) !important;
  }
  .filter-select .selectize-dropdown .option,
  .filter-select .selectize-dropdown .active {
    background-color: var(--card-bg) !important;
    color: var(--text-primary) !important;
  }
  .filter-select .selectize-input .item {
    background: transparent !important;
    color: var(--text-primary) !important;
    border-radius: 0.25rem;
    padding: 0 .25rem;
  }

  /* Plain select (selectInput with selectize = FALSE) */
  .filter-select select.form-control {
    background-color: var(--card-bg) !important;
    color: var(--text-primary) !important;
    border-color: var(--card-border) !important;
  }
  
  /* Let the control and wrappers overflow */
  .filter-group,
  .filter-select,
  .filter-select .selectize-control { overflow: visible !important; }

  /* Make the dropdown float above cards/tables/modals */
  .filter-select .selectize-dropdown { z-index: 99999 !important; }

  /* If you sometimes use plain selectInput (non-selectize), keep it visible too */
  .filter-select select.form-control { overflow: visible !important; }

  /* (Optional) If your layout uses Bootstrap cards that clip content */
  /* .widget-content, .card, .card-body, .table-responsive { overflow: visible !important; } */
"))
    ),
    htmlTemplate("views/ittdash.html",
                 generateData = actionButton(ns("applyFilters"), "Apply filters", class = "filter-apply-btn"),
                 projectSelectionDropDown = uiOutput(ns("projectSelectionDropDownFill")),
                 memberSelectionDropDown = uiOutput(ns("memberSelectionDropDownFill")),
                 districSelectionDropDown = uiOutput(ns("districSelectionDropDownFill")),
                 milestoneSelectionDropDown = uiOutput(ns("milestoneSelectionDropDownFill")),
                 quarterSelectionDropDown =uiOutput(ns("quarterSelectionDropDownFill")),
                 indicatorGroupSelectionDropDown =uiOutput(ns("indicatorGroupSelectionDownFill")),
             
                 activitySelectionDropDown = uiOutput(ns("activitySelectionDropDownFill")),
                 indicatorSelectionDropDown = uiOutput(ns("indicatorSelectionDropDownFill")),
                 mainTargetVsAchievement = plotlyOutput(ns("mainTargetVsAchievement"),width = "300px", height = "250px")%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainTargetVsAchievementStatus = uiOutput(ns("mainTargetVsAchievementStatus")),
                 totalPeopleReached= plotlyOutput(ns("totalPeopleReached"),width = "300px", height = "250px")%>% withSpinner(size=0.5,proxy.height = "50px"),
                 totalPeopleReachedDisaggregation = uiOutput(ns("totalPeopleReachedDisaggregation"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 memberAchievementDash = plotlyOutput(ns("memberAchievementDash"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 districtAchievementDash = leafletOutput(ns("districtAchievementDash"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 quarterlyAchievementDash = plotlyOutput(ns("quarterlyAchievementDash"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 measurementCategoryCounts=uiOutput(ns("measurementCategoryCounts"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainIndicatorProgressTable = DTOutput(ns("mainIndicatorProgressTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainIndicatorProgressByMemberTable = DTOutput(ns("mainIndicatorProgressByMemberTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainMemberDemographicTable = DTOutput(ns("mainMemberDemographicTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainIndicatorProgressByDistrictTable = DTOutput(ns("mainIndicatorProgressByDistrictTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainDistrictDemographicTable =DTOutput(ns("mainDistrictDemographicTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainIndicatorProgressByQuareterTable =DTOutput(ns("mainIndicatorProgressByQuareterTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 mainQuarterDemographicTable=DTOutput(ns("mainQuarterDemographicTable"))%>% withSpinner(size=0.5,proxy.height = "50px"),
                 commentTop = uiOutput(ns("commentTop"))
                   
                   
    )
  )
}

ittdash <- function(input , output ,session,sharedValues){
  ns1 <- NS("ittdash")
  
  
  
  global_vars <- reactiveValues(
   
  )
  current_tab <- reactiveVal("")
  tryCatch({
    

    
    achv <- read_excel("docs/itt/target and achievement.xlsx",sheet = "achievement")%>%
      filter(!(project == "BRCISIII" & milestone == "M2" & indicator_code %in% c("1.1b","3.5b","4.6b","1.5b","2.13b","2.14b","4.3b","4.4b","4.6b","4.7b","4.8b","4.9b","1.1CM","1.2CM","1.3CM","1.5CM","1.8CM","1.9CM",
                                   "1.10CM","1.12CM","1.13CM","1.14CM","1.15CM","1.18CM","1.19CM","1.20CM")))
      
    tar <- read_excel("docs/itt/target and achievement.xlsx",sheet = "target")%>%
      filter(!(project == "BRCISIII" & milestone == "M2" & indicator_code %in% c("1.1b","3.5b","4.6b","1.5b","2.13b","2.14b","4.3b","4.4b","4.6b","4.7b","4.8b","4.9b","1.1CM","1.2CM","1.3CM","1.5CM","1.8CM","1.9CM",
                                     "1.10CM","1.12CM","1.13CM","1.14CM","1.15CM","1.18CM","1.19CM","1.20CM")))%>%
      select(project,member,	indicator_code	, target,indicator_group,cm,milestone)
    
    
    achv <- achv%>%left_join(tar)
    
    achv%<>%
      mutate(measurement_category = case_when(
        # Training (must be checked first to avoid matching under 'People')
        grepl("training", unit_of_measurement, ignore.case = TRUE) ~ "Training",
        
        # Infrastructure
        grepl("storage|infrastructures|flood|water source", unit_of_measurement, ignore.case = TRUE) ~ "Infrastructure",
        
        # Groups
        grepl("group|committee|cooperative|cluster", unit_of_measurement, ignore.case = TRUE) ~ "Group",
        
        # People
        grepl("people|producer|household|farmer", unit_of_measurement, ignore.case = TRUE) ~ "People",
        
        # Everything else
        TRUE ~ "Other"
      ))
    
  
    
    
    global_vars$achievements =  achv
   
    global_vars$project_member_district =  read_excel("docs/itt/target and achievement.xlsx",sheet = "project_member_district")
    
    global_vars$logframe_indicators = read_excel("docs/itt/target and achievement.xlsx",sheet = "target")%>%
      group_by(project,indicator_code)%>%
      summarise(
        activity_code	= first(activity_code),
        activity_group = first(activity_group),
        indicator_desc = first(indicator_desc),
        unit_of_measurement = first(unit_of_measurement),
        indicator_group = first(indicator_group),
        cm = first(cm)
      )%>%
      select(project,activity_code,	activity_group,	indicator_code,	indicator_desc,	unit_of_measurement,indicator_group)
   
   
    
    
  }, error = function(e) {
  
    print(e$message)
  })
  
  projectFilter <- global_vars$project_member_district%>%distinct(projectID)
  
  output$projectSelectionDropDownFill <- renderUI({
    div(class = "filter-group filter-select",
        selectizeInput(
          inputId  = ns1("projectSelection"),
          label    = tags$span("Project:", class = "filter-label"),
          choices  = c("All" = "all", sort(unique(projectFilter$projectID)),"URBANISE"),
          selected = "all",
          multiple = TRUE,
          options  = list(plugins = list("remove_button"))
        )
    )
  })
  
  
  
 
  output$milestoneSelectionDropDownFill <- renderUI({
    
    if(is.null(input$projectSelection) || "all" %in% input$projectSelection ||anyNA(input$projectSelection)){
      milestones <- global_vars$achievements%>%
        distinct(milestone)%>%arrange(milestone)%>%pull(milestone)
    }else{
      milestones <- global_vars$achievements%>%
        filter(project %in% input$projectSelection)%>%
        distinct(milestone)%>%arrange(milestone)%>%pull(milestone)
    }
    
    
    output$milestoneSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("milestoneSelection"),
            label    = tags$span("Milestone:", class = "filter-label"),
            choices  = c("All" = "all", unique(as.character(milestones))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
  })

  output$indicatorGroupSelectionDownFill <- renderUI({
    if(is.null(input$projectSelection) || "all" %in% input$projectSelection || anyNA(input$projectSelection)){
      indicatorGroup <-  global_vars$logframe_indicators%>%
        group_by(project,indicator_group)%>%
        summarise(indicator_group = first(indicator_group))%>%
        drop_na(indicator_group)
    }else{
      indicatorGroup <-  global_vars$logframe_indicators%>%
        filter(project %in% input$projectSelection)%>%
        group_by(project,indicator_group)%>%
        summarise(indicator_group = first(indicator_group))%>%
        drop_na(project,indicator_group)
    }
    
    
   
    output$indicatorGroupSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("indicatorGroupSelection"),
            label    = tags$span("Indicator Group:", class = "filter-label"),
            choices  = c("All" = "all",
                         sort(unique(na.omit(as.character(indicatorGroup$indicator_group))))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
  })
  
  output$activitySelectionDropDownFill <- renderUI({
    
    if(is.null(input$projectSelection) ||  "all" %in% input$projectSelection || anyNA(input$projectSelection)){
      activities <-  global_vars$logframe_indicators%>%
        group_by(activity_code)%>%
        summarise(activity_group = first(activity_group))
    }else{
      activities <-  global_vars$logframe_indicators%>%
        filter(project %in% input$projectSelection)%>%
        group_by(activity_code)%>%
        summarise(activity_group = first(activity_group))
    }
   
    
    output$activitySelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("activitySelection"),
            label    = tags$span("Activity:", class = "filter-label"),
            choices  = c(
              "All" = "all",
              setNames(activities$activity_code,
                       paste(activities$activity_code, "-", activities$activity_group))
            ),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
  })
  
  #------------
  
  observeEvent(input$projectSelection,{
    if( "all" %in% input$projectSelection || anyNA(input$projectSelection)){
      memberFilter <- global_vars$project_member_district%>%distinct(member)
    }else{
      memberFilter <- global_vars$project_member_district%>%filter(projectID %in% input$projectSelection)%>%distinct(member)
    }
    
    
    output$memberSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("memberSelection"),
            label    = tags$span("Member:", class = "filter-label"),
            choices  = c("All" = "all",
                         setNames(as.character(memberFilter$member),
                                  as.character(memberFilter$member))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    
    if("all" %in% input$projectSelection || anyNA(input$projectSelection)){
      districtFilter <- global_vars$project_member_district
    }else{
      districtFilter <- global_vars$project_member_district%>%filter(projectID %in% input$projectSelection)
    }
    
    if("all" %in% input$memberSelection || anyNA(input$memberSelection)){
      districtFilter <- districtFilter%>%distinct(district)
    }else{
      districtFilter <- districtFilter%>%
        filter(member %in% input$memberSelection)%>%distinct(district)
    }
    
    output$districSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("districSelection"),
            label    = tags$span("District:", class = "filter-label"),
            choices  = c("All" = "all",
                         setNames(as.character(districtFilter$district),
                                  as.character(districtFilter$district))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    if(is.null(input$projectSelection) || "all" %in% input$projectSelection ||anyNA(input$projectSelection)){
      milestones <- global_vars$achievements%>%
        distinct(milestone)%>%arrange(milestone)%>%pull(milestone)
    }else{
      milestones <- global_vars$achievements%>%
        filter(project %in% input$projectSelection)%>%
        distinct(milestone)%>%arrange(milestone)%>%pull(milestone)
    }
    
    
    output$milestoneSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("milestoneSelection"),
            label    = tags$span("Milestone:", class = "filter-label"),
            choices  = c("All" = "all", unique(as.character(milestones))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    
    if("all" %in% input$projectSelection  || anyNA(input$projectSelection)){
      quarters <- global_vars$achievements
    }else{
      quarters <- global_vars$achievements %>%
        filter(project %in% input$projectSelection)
    }
    
    
    if( "all" %in% input$milestoneSelection || anyNA(input$milestoneSelection)){
      quarters <- quarters%>%distinct(quarter)%>%pull(quarter)
    }else{
      quarters <- quarters %>%
        filter(milestone %in% input$milestoneSelection)%>%distinct(quarter)%>%pull(quarter)
    }
    
    output$quarterSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("quarterSelection"),
            label    = tags$span("Quarter:", class = "filter-label"),
            choices  = c("All" = "all",
                         setNames(as.character(quarters), as.character(quarters))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    
    if("all" %in% input$projectSelection || anyNA(input$projectSelection)){
      indicatorsFiltered <- global_vars$logframe_indicators
    }else{
      indicatorsFiltered <- global_vars$logframe_indicators%>%
        filter(project %in% input$projectSelection)
    }
    
    if("all" %in% input$activitySelection || anyNA(input$activitySelection)){
      indicatorsFiltered <- indicatorsFiltered%>%
        group_by(indicator_code)%>%
        summarise(indicator_desc  = first(indicator_desc))
    }else{
      indicatorsFiltered <- indicatorsFiltered%>%
        filter(activity_code %in% input$activitySelection)%>%
        group_by(indicator_code)%>%
        summarise(indicator_desc  = first(indicator_desc))
    }
    
    output$indicatorSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("indicatorSelection"),
            label    = tags$span("Indicator:", class = "filter-label"),
            choices  = c(
              "All" = "all",
              setNames(
                as.character(indicatorsFiltered$indicator_code),
                paste(indicatorsFiltered$indicator_code, "-", indicatorsFiltered$indicator_desc)
              )
            ),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    
   
  })
  
  observeEvent(input$memberSelection,{
    
    if("all" %in% input$projectSelection || anyNA(input$projectSelection)){
      districtFilter <- global_vars$project_member_district
    }else{
      districtFilter <- global_vars$project_member_district%>%filter(projectID %in% input$projectSelection)
    }

    if("all" %in% input$memberSelection || anyNA(input$memberSelection)){
      districtFilter <- districtFilter%>%distinct(district)
    }else{
      districtFilter <- districtFilter%>%
        filter(member %in% input$memberSelection)%>%distinct(district)
    }
    output$districSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("districSelection"),
            label    = tags$span("District:", class = "filter-label"),
            choices  = c("All" = "all",
                         setNames(as.character(districtFilter$district),
                                  as.character(districtFilter$district))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
  })
  
  observeEvent(input$milestoneSelection,{
    
    if("all" %in% input$projectSelection  || anyNA(input$projectSelection)){
      quarters <- global_vars$achievements
    }else{
      quarters <- global_vars$achievements %>%
        filter(project %in% input$projectSelection)
    }
    
    
    if( "all" %in% input$milestoneSelection || anyNA(input$milestoneSelection)){
      quarters <- quarters%>%distinct(quarter)%>%pull(quarter)
    }else{
      quarters <- quarters %>%
        filter(milestone %in% input$milestoneSelection)%>%distinct(quarter)%>%pull(quarter)
    }
    
    output$quarterSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("quarterSelection"),
            label    = tags$span("Quarter:", class = "filter-label"),
            choices  = c("All" = "all",
                         setNames(as.character(quarters), as.character(quarters))),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    if("all" %in% input$projectSelection || anyNA(input$projectSelection)){
      indicatorsFiltered <- global_vars$logframe_indicators
    }else{
      indicatorsFiltered <- global_vars$logframe_indicators%>%
        filter(project %in% input$projectSelection)
    }
    
    if("all" %in% input$activitySelection || anyNA(input$activitySelection)){
      indicatorsFiltered <- indicatorsFiltered%>%
        group_by(indicator_code)%>%
        summarise(indicator_desc  = first(indicator_desc))
    }else{
      indicatorsFiltered <- indicatorsFiltered%>%
        filter(activity_code %in% input$activitySelection)%>%
        group_by(indicator_code)%>%
        summarise(indicator_desc  = first(indicator_desc))
    }
    
    output$indicatorSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("indicatorSelection"),
            label    = tags$span("Indicator:", class = "filter-label"),
            choices  = c(
              "All" = "all",
              setNames(
                as.character(indicatorsFiltered$indicator_code),
                paste(indicatorsFiltered$indicator_code, "-", indicatorsFiltered$indicator_desc)
              )
            ),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    
    
  })
  
  observeEvent(input$activitySelection,{
    
    if("all" %in% input$projectSelection || anyNA(input$projectSelection)){
      indicatorsFiltered <- global_vars$logframe_indicators
    }else{
      indicatorsFiltered <- global_vars$logframe_indicators%>%
        filter(project %in% input$projectSelection)
    }
    
    if("all" %in% input$activitySelection || anyNA(input$activitySelection)){
      indicatorsFiltered <- indicatorsFiltered%>%
        group_by(indicator_code)%>%
        summarise(indicator_desc  = first(indicator_desc))
    }else{
      indicatorsFiltered <- indicatorsFiltered%>%
        filter(activity_code %in% input$activitySelection)%>%
        group_by(indicator_code)%>%
        summarise(indicator_desc  = first(indicator_desc))
    }
    
    output$indicatorSelectionDropDownFill <- renderUI({
      div(class = "filter-group filter-select",
          selectizeInput(
            inputId  = ns1("indicatorSelection"),
            label    = tags$span("Indicator:", class = "filter-label"),
            choices  = c(
              "All" = "all",
              setNames(
                as.character(indicatorsFiltered$indicator_code),
                paste(indicatorsFiltered$indicator_code, "-", indicatorsFiltered$indicator_desc)
              )
            ),
            selected = "all",
            multiple = TRUE,
            width    = "100%",
            options  = list(plugins = list("remove_button"))
          )
      )
    })
    
    
  })
  
  observeEvent(input$current_tab, {
    current_tab("")
    
  })

 
  # In your (module) server ----
  make_all_exclusive <- function(id) {
    observeEvent(input[[id]], ignoreInit = TRUE, {
      sel <- input[[id]]
      if (is.null(sel)) return()
      
      # If user has 'all' + others, drop 'all' so specifics win
      if (length(sel) > 1 && "all" %in% sel) {
        new_sel <- setdiff(sel, "all")
        # Try selectize first; fall back to plain select
        tried <- try(updateSelectizeInput(session, id, selected = new_sel), silent = TRUE)
        if (inherits(tried, "try-error")) {
          try(updateSelectInput(session, id, selected = new_sel), silent = TRUE)
        }
      }
    })
  }
  
  # Apply to your controls
  ids <- c(
    "projectSelection",
    "activitySelection",
    "indicatorSelection",
    "districSelection",
    "memberSelection",
    "milestoneSelection",
    "quarterSelection",
    "indicatorGroupSelection"
  )
  invisible(lapply(ids, make_all_exclusive))
 
  
  observeEvent(list(input$applyFilters, input$current_tab),{
   
    
    
    if( is.null(nrow(global_vars$achievements))){
      return()
    }
    
    cat("projectSelection length:", length(input$projectSelection), "\n")
    cat("activitySelection length:", length(input$activitySelection), "\n")
    cat("indicatorSelection length:", length(input$indicatorSelection), "\n")
    cat("districSelection length:", length(input$districSelection), "\n")
    cat("memberSelection length:", length(input$memberSelection), "\n")
    cat("milestoneSelection length:", length(input$milestoneSelection), "\n")
    cat("quarterSelection length:", length(input$quarterSelection), "\n")
    
    req(
      input$projectSelection,
      input$activitySelection,
      input$indicatorSelection,
      input$districSelection,
      input$memberSelection,
      input$milestoneSelection,
      input$quarterSelection,
      input$indicatorGroupSelection
    )

   
   
    output$commentTop <- renderUI({
      paste("Indicator Tracking Tool",current_tab())
    })
    
    main_achievements <- global_vars$achievements
    
    
    
    if(!("all" %in% input$projectSelection)){
      main_achievements <-  main_achievements%>%
        filter(
          project %in% input$projectSelection
        )
      
    }
    
    
    if(!("all" %in% input$memberSelection)){
      main_achievements <-  main_achievements%>%
        filter(
          member %in% input$memberSelection
        )
      
    }
    
    
    
    if(!("all" %in% input$districSelection)) {
      main_achievements <-  main_achievements%>%
        filter(
          district %in% input$districSelection
        )
    }
    
    
    
    if(!("all" %in% input$milestoneSelection)){
      main_achievements <-  main_achievements%>%
        filter(
          milestone %in% input$milestoneSelection
        )
    }
    
    if(!("all" %in% input$quarterSelection)){
      main_achievements <-  main_achievements%>%
        filter(
          quarter %in% input$quarterSelection
        )
    }
    
    if(!("all" %in% input$activitySelection)){
      main_achievements <-  main_achievements%>%
        filter(
          Activity_code %in% input$activitySelection
        )
    }
    
    if(!("all" %in% input$indicatorSelection)) {
      main_achievements <-  main_achievements%>%
        filter(
          indicator_code %in% input$indicatorSelection
        )
    
    }
    
    if(!("all" %in% input$indicatorGroupSelection)){
      main_achievements <-  main_achievements%>%
        filter(
          indicator_group %in% input$indicatorGroupSelection
        )
      
    }
    
    
    #----------
    output$mainTargetVsAchievementStatus <- renderUI({
      ach_pm <- main_achievements %>%
        group_by(project, indicator_code, milestone) %>%
        summarise(achievement = sum(achievement, na.rm = TRUE))
      
      tgt_pm <- main_achievements %>%
        group_by(project, member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0)) %>%
        group_by(project, indicator_code, milestone) %>%
        summarise(target = sum(target))                # sum across members
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, NA_real_))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      project_perc <- milestone_perc %>%
        group_by(project, milestone) %>%
        reframe(
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_milestone = if_else(target > 0, (achievement / target) * 100, NA_real_)
        )%>%
        group_by(project)%>%
        reframe(
          pct_project = round(sum(pct_milestone) /n())
        )%>%
        summarise(
          pct_project = round(sum(pct_project) /n())
        )%>%pull(pct_project)
      
      
      
      value <-  as.numeric(project_perc)
      
      if(value < 100){
        HTML(paste("<span style='font-size:12px;font-weight:bold;'>Under Achieved</span>"))
      }else if(value > 100){
        HTML(paste("<span style='font-size:12px;font-weight:bold;'>Over Achieved</span>"))
      }else{
        HTML(paste("<span style='font-size:12px;font-weight:bold;'>Achieved</span>"))
      }
    })
    
    output$measurementCategoryCounts <- renderUI({
      ms <- main_achievements%>%
        filter(measurement_category %in% c("Infrastructure","Training","Group"))%>%
        group_by(measurement_category)%>%
        summarise(
          total = sum(achievement,na.rm = TRUE)
        )%>%
        pivot_wider(names_from =   measurement_category , values_from = total)
      
      infra <- tryCatch(
        sum(ms$Infrastructure, na.rm = TRUE),
        error = function(e) 0,
        warning = function(w) 0
      )
      grp <- tryCatch(
        sum(ms$Group, na.rm = TRUE),
        error = function(e) 0,
        warning = function(w) 0
      )
      trn <- tryCatch(
        sum(ms$Training, na.rm = TRUE),
        error = function(e) 0,
        warning = function(w) 0
      )
      
      HTML(paste0('
                 <div class="category-counts">
                  <div class="category-item">
                    <div class="category-icon">
                      <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#3B82F6" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                        <rect x="2" y="3" width="20" height="14" rx="2" ry="2"></rect>
                        <line x1="8" y1="21" x2="16" y2="21"></line>
                        <line x1="12" y1="17" x2="12" y2="21"></line>
                      </svg>
                    </div>
                    <div class="category-text">
                      <div class="category-value">~',format(infra,big.mark = ","),'</div>
                      <div class="category-label">Infrastructures</div>
                    </div>
                  </div>
                  <div class="category-item">
                    <div class="category-icon">
                      <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#6366F1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                        <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"></path>
                        <circle cx="9" cy="7" r="4"></circle>
                        <path d="M23 21v-2a4 4 0 0 0-3-3.87"></path>
                        <path d="M16 3.13a4 4 0 0 1 0 7.75"></path>
                      </svg>
                    </div>
                    <div class="category-text">
                      <div class="category-value">~',format(grp,big.mark = ","),'</div>
                      <div class="category-label">Groups</div>
                    </div>
                  </div>
                  <div class="category-item">
                    <div class="category-icon">
                      <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#F59E0B" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                        <path d="M22 10v6M2 10l10-5 10 5-10 5z"></path>
                        <path d="M6 12v5c3 3 9 3 12 0v-5"></path>
                      </svg>
                    </div>
                    <div class="category-text">
                      <div class="category-value">~',format(trn,big.mark = ","),'</div>
                      <div class="category-label">Trainings</div>
                    </div>
                  </div>
                </div>
      '))
    })
    
    output$mainTargetVsAchievement <- renderPlotly({
      
      # dd<- main_achievements%>%
      #   group_by(project,indicator_code,milestone)%>%
      #   summarise(
      #     achievement = sum(achievement,na.rm = TRUE)
      #   )%>%mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))%>%
      #   left_join(
      #     main_achievements%>%
      #       group_by(project,member,indicator_code,milestone)%>%
      #       summarise(
      #         target = first(target)
      #       )%>%mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))%>%
      #       group_by(project,indicator_code,milestone)%>%
      #       summarise(
      #         target = sum(target)
      #       )
      #   )%>%
      #   mutate(
      #     achievement_percentage = ifelse(target == 0 , 0 ,round((achievement/target)*100))
      #   )%>%
      #   group_by(indicator_code,milestone)%>%
      #   summarise(
      #     achievement = sum(achievement),
      #     target = sum(target),
      #     achievement_percentage = sum(achievement_percentage) / n()
      #   )%>%
      #   group_by(indicator_code)%>%
      #   summarise(
      #     achievement = sum(achievement),
      #     target = sum(target),
      #     achievement_percentage = sum(achievement_percentage) / n()
      #   )
      # 1) Build milestone-level ACH & TARGET per (project, indicator_code, milestone)
      ach_pm <- main_achievements %>%
        group_by(project, indicator_code, milestone) %>%
        summarise(achievement = sum(achievement, na.rm = TRUE))
      
      tgt_pm <- main_achievements %>%
        group_by(project, member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0)) %>%
        group_by(project, indicator_code, milestone) %>%
        summarise(target = sum(target))                # sum across members
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, NA_real_))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      project_perc <- milestone_perc %>%
        group_by(project, milestone) %>%
        reframe(
              achievement = sum(achievement),
              target = sum(target),
        ) %>%
        mutate(
          pct_milestone = if_else(target > 0, (achievement / target) * 100, NA_real_)
        )%>%
        group_by(project)%>%
        reframe(
          pct_project = round(sum(pct_milestone) /n())
        )%>%
        summarise(
          pct_project = round(sum(pct_project) /n())
        )%>%pull(pct_project)
      
      
      value <-  as.numeric(project_perc)
      max_value <- 100
      
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number",  # keep delta active
        value = value,
        number = list(
          suffix = "%",
          font = list(size = 28, color = "white", family = "Arial")
        ),
        gauge = list(
          axis = list(range = list(0, max_value), tickwidth = 2, dtick = 10),
          bar = list(color = "black"),
          steps = list(
            list(range = c(0, 50), color = "#ff6666"),
            list(range = c(50, 75), color = "#ffcc66"),
            list(range = c(75, 100), color = "#66cc66")
          ),
          threshold = list(
            line = list(color = "blue", width = 4),
            thickness = 0.75,
            value = value
          )
        ),
        domain = list(x = c(0, 1), y = c(0, 1))
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "white", family = "Arial"),
          margin = list(l = 20, r = 20, t = 40, b = 40)
        )
      
      fig
    })
    
    output$totalPeopleReached <- renderPlotly({
      
      totalPeopleReachedMale <-sum(main_achievements$male,na.rm = TRUE) 
      totalPeopleReachedFemale <- sum(main_achievements$female,na.rm = TRUE)
      
      fig <- plot_ly(
        labels = c("Female", "Male"),
        values = c(totalPeopleReachedFemale, totalPeopleReachedMale),
        type = "pie",
        hole = 0.6,
        textinfo = "label+percent+value",
        textposition = "outside",
        pull = 0.02,
        marker = list(colors = c("#ED7667", "#008BA8")),
        textfont = list(color = "white"),  # 👈 sets label color to white
        showlegend = FALSE
      )
      
      fig <- fig %>%
        layout(
          title = list(
            text = "<b>Total People Reached</b>",
            x = 0.5,
            xanchor = "center",
            font = list(color = "white", size = 16)
          ),
          annotations = list(
            list(
              text = paste0("<b>",format((totalPeopleReachedMale+totalPeopleReachedFemale), big.mark = ","),"</b>"),
              font = list(size = 20, color = "white"),
              showarrow = FALSE,
              x = 0.5,
              y = 0.5,
              xanchor = "center",
              yanchor = "middle"
            )
          ),
          margin = list(t = 40, b = 20, l = 20, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)")
      
      fig
    })
    
    output$totalPeopleReachedDisaggregation <- renderUI({
      
      totalPeopleReachedMinorityMale <-sum(main_achievements$minority_male,na.rm = TRUE) 
      totalPeopleReachedMinorityFemale <- sum(main_achievements$minority_female,na.rm = TRUE)
      totalPeopleReachedDisabledMale <-sum(main_achievements$disabled_male,na.rm = TRUE) 
      totalPeopleReachedDisabledFemale <- sum(main_achievements$disabled_female,na.rm = TRUE)
      
      HTML(paste0('
               <div class="demographics-grid">
                <div class="demographic-box">
                  <div class="demographic-box-title">Minority People</div>
                  <div class="demographic-box-value">',format((totalPeopleReachedMinorityMale+totalPeopleReachedMinorityFemale), big.mark = ",") ,'</div>
                  <div class="demographic-stats">
                    <div class="demographic-stat">
                      <div class="demographic-dot" style="background-color: #008BA8;"></div>
                      <span class="demographic-label">Male: ',format(totalPeopleReachedMinorityMale, big.mark = ","),'</span>
                    </div>
                    <div class="demographic-stat">
                      <div class="demographic-dot" style="background-color: #ED7667;"></div>
                      <span class="demographic-label">Female: ',format(totalPeopleReachedMinorityFemale, big.mark = ","),'</span>
                    </div>
                  </div>
                </div>
                <div class="demographic-box">
                  <div class="demographic-box-title">Disabled People</div>
                  <div class="demographic-box-value">',format((totalPeopleReachedDisabledMale+totalPeopleReachedDisabledFemale), big.mark = ",") ,'</div>
                  <div class="demographic-stats">
                    <div class="demographic-stat">
                      <div class="demographic-dot" style="background-color: #008BA8;"></div>
                      <span class="demographic-label">Male: ',format(totalPeopleReachedDisabledMale, big.mark = ","),'</span>
                    </div>
                    <div class="demographic-stat">
                      <div class="demographic-dot" style="background-color: #ED7667;"></div>
                      <span class="demographic-label">Female: ',format(totalPeopleReachedDisabledFemale, big.mark = ","),'</span>
                    </div>
                  </div>
                </div>
              </div>
                 
      '))
    })
    
    output$memberAchievementDash <- renderPlotly({
      
      
      ach_pm <- main_achievements %>%
        group_by(project, member , indicator_code, milestone) %>%
        summarise(achievement = sum(achievement, na.rm = TRUE))
      
      tgt_pm <- main_achievements %>%
        group_by(project, member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, NA_real_))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      df <- milestone_perc %>%
        group_by(project, milestone,member) %>%
        reframe(
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_member = if_else(target > 0, (achievement / target) * 100, NA_real_)
        )%>%
        group_by(project,member)%>%
        reframe(
          pct_member = round(sum(pct_member) /n())
        )%>%
        group_by(member)%>%
        summarise(
          pct_member = round(sum(pct_member) /n())
        )%>%
        rename(org = member,value = pct_member)%>%
        mutate(
          org = paste0(org," ")
        )%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
      
      
    
     
      
      # Sort descending
      df <- df[order(-df$value), ]
      df$org <- factor(df$org, levels = rev(df$org))
      
      # Compute max for x-axis range (+10%) in 0–1 scale
      x_max <- (max(df$value,na.rm = TRUE) + 10) / 100  # e.g. 119.3% → 1.193
      print(x_max)
    
      # Plot
      fig <- plot_ly(
        data = df,
        x = ~value / 100,
        y = ~org,
        type = "bar",
        orientation = "h",
        text = ~paste0(round(value, 0), "%"),  # round label to nearest %
        textposition = "outside",
        marker = list(color = "#f39c12"),
        hoverinfo = "none"
      )
      
      fig <- fig %>%
        layout(
          xaxis = list(
            title = "",
            tickformat = ".0%",       # rounded percentages
            showgrid = TRUE,
            gridcolor = "#444",
            tickfont = list(color = "white"),
            range = c(0, ceiling(x_max))       # dynamic range
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            tickfont = list(color = "white", size = 12)
          ),
          font = list(color = "white"),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 80, r = 40, t = 20, b = 20),
          height = 350
        )
      
      fig
    })
    
    output$districtAchievementDash <- renderLeaflet({
     
      ach_pm <- main_achievements %>%
        group_by(project, district ,member, indicator_code, milestone) %>%
        summarise(achievement = sum(achievement, na.rm = TRUE))
      
      tgt_pm <- main_achievements %>%
        group_by(project,member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, NA_real_))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      districts_itt <- milestone_perc %>%
        group_by(project, milestone,district) %>%
        reframe(
          member = first(member),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_district = if_else(target > 0, (achievement / target) * 100, NA_real_)
        )%>%
        group_by(project,district)%>%
        reframe(
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        group_by(district)%>%
        summarise(
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        rename(District_Name = district,progress = pct_district)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))%>%
        mutate(
          District_Name = case_when(
            District_Name == "Hudur" ~ "Xudur",
            District_Name == "Elbarde" ~ "Ceel Barde",
            District_Name == "Rabdhure" ~ "Rab Dhuure",
            District_Name == "Afgoye" ~ "Afgooye",
            District_Name == "Belet-hawa" ~ "Belet Xaawo",
            District_Name == "Bardhere" ~ "Baardheere",
            District_Name == "Baidoa" ~ "Baydhaba",
            District_Name == "Wanlaweyn" ~ "Wanla Weyn",
            District_Name == "Adaado" ~ "Cadaado",
            District_Name == "Dhusamareb" ~ "Dhuusamarreeb",
            District_Name == "Galkaio" ~ "Gaalkacyo",
            District_Name == "Kismayo" ~ "Kismaayo",
            District_Name == "Afmadow" ~ "Afmadow",
            District_Name == "Beletweyne" ~ "Belet Weyne",
            District_Name == "Jowha" ~ "Jowhar",
            District_Name == "Dinsor" ~"Diinsoor",
            District_Name == "Jariiban" ~"Jariiban",
            District_Name == "Wajid" ~"Waajid",
            District_Name == "Galdogob" ~"Galdogob",
            .default = District_Name
          )
        )%>%left_join(somalia_districts,by = c("District_Name"="DIST_NAME"))%>%sf::st_as_sf()
      
      
      
     
      # Get centroids
      centroids <- st_centroid(districts_itt)
      coords <- st_coordinates(centroids)
      scale_factor <- 200
      
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
          addPolygons(data = districts_itt , color = "black",fillColor="#ED7667", weight = 0.5, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.9,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = paste(sep = "<br>",
                                    paste("<b>District: </b>",paste0(districts_itt$District_Name)) ,
                                    paste("<b>Member(s): </b>",paste0(districts_itt$member),"<hr>") ,
                                    paste("<b>Target: </b>",format(districts_itt$target, big.mark = ","),"<hr>"),  
                                    paste("<b>Achievement: </b>",format(districts_itt$achievement, big.mark = ","),"<hr>") ,
                                    paste("<b>Porgress(s): </b>",paste0(districts_itt$progress,"%"),"<hr>")
                                    ),
                      
                      label = districts_itt$District_Name ,
                  
                      
                      group = districts_itt$District_Name) %>%
          addCircles(
            lng = coords[, "X"],
            lat = coords[, "Y"],
            radius = districts_itt$progress * scale_factor,
            fillColor = "#008BA8",
            color = "white",
            weight = 1,
            fillOpacity = 0.8,
            label = paste0(districts_itt$District_Name, ": ", districts_itt$progress, "%")
          )%>%
          addLegend(
            position = "bottomleft",
            colors = c("#fff","#ED7667"),
            labels = c("Out of scope districts","Project District Coverage"),
            title = "Where we work",
            opacity = 0.7
          )%>%
          addLayersControl(
            baseGroups = c("Satellite Map","Normal Map","CartoDB.Positron"),
            overlayGroups = districts_itt$District_Name,
            options = layersControlOptions(collapsed = TRUE),position = "bottomright")
      
    })
    
    output$quarterlyAchievementDash <- renderPlotly({
      # Sample data
      ach_pm <- main_achievements %>%
        group_by(project, quarter ,member, indicator_code, milestone) %>%
        summarise(achievement = sum(achievement, na.rm = TRUE))
      
      tgt_pm <- main_achievements %>%
        group_by(project,member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, NA_real_))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      df <- milestone_perc %>%
        group_by(project, milestone,quarter) %>%
        reframe(
          member = first(member),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_district = if_else(target > 0, (achievement / target) * 100, NA_real_)
        )%>%
        group_by(project,quarter)%>%
        reframe(
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        group_by(quarter)%>%
        summarise(
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        rename(progress = pct_district)%>%
        select(quarter , progress)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
    
      # Compute dynamic y-axis max
     
      
      # Calculate dynamic max for Y-axis (add some headroom)
      y_max <- max(df$progress) + 50
      
      # Create chart
      fig <- plot_ly(data = df) %>%
        add_trace(
          x = ~quarter,
          y = ~progress,
          name = "Progress",
          type = "scatter",
          mode = "lines+markers+text",
          line = list(color = "#3498db", width = 3),
          marker = list(color = "#3498db", size = 8),
          text = ~paste0(progress,"%"),
          textposition = "top center",
          textfont = list(color = "white", size = 12),
          hoverinfo = "text",
          hovertext = ~paste("Progress:", progress)
        ) %>%
        layout(
          title = "",
          plot_bgcolor = "#1e293b",
          paper_bgcolor = "#1e293b",
          font = list(color = "white"),
          yaxis = list(
            title = "Progress",
            range = c(0, y_max),
            showgrid = TRUE,
            gridcolor = "#444"
          ),
          xaxis = list(
            title = "Quarter",
            showgrid = FALSE,
            tickfont = list(color = "white")
          ),
          margin = list(l = 40, r = 40, t = 30, b = 40)
        )
      
      fig
      
    })
    
    output$mainIndicatorProgressTable <- renderDT(server = FALSE,{
      
      ach_pm <- main_achievements %>%
        group_by(project, indicator_code, milestone) %>%
        summarise(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE)
          )
      
      tgt_pm <- main_achievements %>%
        group_by(project, member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0)) %>%
        group_by(project, indicator_code, milestone) %>%
        summarise(target = sum(target))                # sum across members
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, 0))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
     
      data <- milestone_perc %>%
        group_by(project, milestone,indicator_code) %>%
        reframe(

          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE),
          target = sum(target),
        ) %>%
        mutate(
          pct_milestone = if_else(target > 0, (achievement / target) * 100, 0)
        )%>%
        group_by(project,indicator_code)%>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE),
          target = sum(target),
          pct_project = round(sum(pct_milestone) /n())
        )%>%
        group_by(indicator_code)%>%
        reframe(
          project = first(project),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          male = format(sum(male),big.mark = ","),
          female = format(sum(female),big.mark = ","),
          minority_male = format(sum(minority_male),big.mark = ","),
          minority_female = format(sum(minority_female),big.mark = ","),
          disabled_male = format(sum(disabled_male),big.mark = ","),
          disabled_female = format(sum(disabled_female),big.mark = ","),
          achievement = sum(achievement,na.rm = TRUE),
          target = sum(target,na.rm = TRUE),
          achievement = format(achievement,big.mark = ","),
          target = format(target,big.mark = ","),
          pct_project = round(sum(pct_project) /n()),
          Status = ifelse(target==0 , "Achieved",ifelse(pct_project >100 , "Overachieved",ifelse(pct_project <100,"Underachieved","Achieved"))),
          progress = paste0(pct_project , "%"),
        )%>%
        rename(
          Project = project,
          Code = indicator_code,
          Description = indicator_desc,
          `Unit of measurement` = unit_of_measurement,
          target =target,
          Achievement = achievement,
          Progress = progress,
          Male	= male,
          Female	=female,
          `Minority Male` = minority_male,
          `Minority Female`	=minority_female,
          `Disabled Male` = disabled_male,
          `Disabled Female` =disabled_female
        )%>%
        select(Project,Code,Description,`Unit of measurement`,target,Achievement,Progress,Status,Male,Female,`Minority Male` ,`Minority Female`,
               `Disabled Male`,`Disabled Female`)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
      
     
      report_name <- "Indicator Achievement"
      datatable(
        data,
        rownames = FALSE, 
        selection = "none",
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE,
          pageLength = 100, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename =report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Indicator Achievement',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ))%>%
        formatStyle(
          'Status',
          target = 'cell',
          color = DT::styleEqual(
            c("Achieved", "Overachieved","Underachieved"),
            c("#82BA96", "#82BA96","#ED7667")
          )
        )
    })
    
    output$mainIndicatorProgressByMemberTable <- renderDT(server = FALSE,{
      
      
      ach_pm <- main_achievements %>%
        group_by(project, member , indicator_code, milestone) %>%
        summarise(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE)
          )
      
      tgt_pm <- main_achievements %>%
        group_by(project, member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, 0))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      data <- milestone_perc %>%
        group_by(project, milestone,member,indicator_code) %>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_member = if_else(target > 0, (achievement / target) * 100, 0)
        )%>%
        group_by(project,member,indicator_code)%>%
        summarise(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          achievement = sum(achievement),
          target = first(target),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          pct_member = round(sum(pct_member) /n())
        )%>%
        group_by(member,indicator_code)%>%
        summarise(
          project = first(project),
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          achievement = sum(achievement),
          target = first(target),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          pct_member = round(sum(pct_member) /n()),
          progress = paste0(format(achievement, big.mark = ",") , "(",pct_member,"%)"),
          Status = ifelse(target==0 , "Achieved",ifelse(pct_member >100 , "Overachieved",ifelse(pct_member <100,"Underachieved","Achieved")))
        )%>%
        rename(
          Project = project,
          Code = indicator_code,
          Description = indicator_desc,
          `Unit of measurement` = unit_of_measurement,
          Progress = progress,
          Member = member
        )%>%
        select(Project,Code,Description,`Unit of measurement`,Member,Progress)%>%
        pivot_wider(id_cols = c(Project,Code,Description,`Unit of measurement`),names_from = Member , values_from = Progress)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
    
      
      report_name <- "Indicator Achievement by Member"
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      col_indices <- which(names(data) %in% numeric_cols) - 1  # JS is 0-indexed
      datatable(
        data,
        rownames = FALSE,
        selection = "none",
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          pageLength = 100,
          columnDefs = list(
            list(
              targets = col_indices,
              render = JS("function(data, type, row, meta) { return data + '%'; }")
            )
          ),
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Indicator Achievement',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            )
          )
        ),
        escape = FALSE
      ) %>%
        {
          dt <- .
          for (col in numeric_cols) {
            dt <- formatStyle(
              dt,
              columns = col,
              color = styleInterval(99, c("#ED7667", "#82BA96")),
              fontWeight = 'bold'
            )
          }
          
          dt
        }
    })
    
    output$mainMemberDemographicTable <- renderDT(server = FALSE,{
      ach_pm <- main_achievements %>%
        group_by(project, member , indicator_code, milestone) %>%
        summarise(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE),
          )
      
      tgt_pm <- main_achievements %>%
        group_by(project, member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, 0))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      data <- milestone_perc %>%
        group_by(project, milestone,member) %>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
        
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_member = if_else(target > 0, (achievement / target) * 100, 0)
        )%>%
        group_by(project,member)%>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          target = first(target),
          achievement = sum(achievement,na.rm = TRUE),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          pct_member = round(sum(pct_member) /n())
        )%>%
        group_by(member)%>%
        summarise(
          project = first(project),
          male = format(sum(male),big.mark = ","),
          female = format(sum(female),big.mark = ","),
          minority_male = format(sum(minority_male),big.mark = ","),
          minority_female = format(sum(minority_female),big.mark = ","), 
          disabled_male = format(sum(disabled_male),big.mark = ","),
          disabled_female = format(sum(disabled_female),big.mark = ","),
          achievement = sum(achievement,na.rm = TRUE),
          target = sum(target,na.rm = TRUE),
          progress = round(sum(pct_member) /n()),
          Status = ifelse(target==0 , "Achieved",ifelse(progress >100 , "Overachieved",ifelse(progress <100,"Underachieved","Achieved"))),
          progress = paste0(progress , "%"),
          achievement = format(achievement,big.mark = ","),
          target = format(target,big.mark = ",")
          
        )%>%
        rename(
          Project = project,
          Member = member,
          target =target,
          Achievement = achievement,
          Progress = progress,
          Male	= male,
          Female	=female,
          `Minority Male` = minority_male,
          `Minority Female`	=minority_female,
          `Disabled Male` = disabled_male,
          `Disabled Female` =disabled_female
        )%>%
        select(Member,target,Achievement,Progress,Status,Male,Female,`Minority Male` ,`Minority Female`,
               `Disabled Male`,`Disabled Female`)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
      
      
      
  
      
      report_name <- "Member Demographcis"
      datatable(
        data,
        rownames = FALSE, 
        selection = "none",
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE,
          pageLength = 100, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename =report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Indicator Achievement',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ))%>%
        formatStyle(
          'Status',
          target = 'cell',
          color = DT::styleEqual(
            c("Achieved", "Overachieved","Underachieved"),
            c("#82BA96", "#82BA96","#ED7667")
          )
        )
    })
    
    output$mainIndicatorProgressByDistrictTable <- renderDT(server = FALSE,{
      
      ach_pm <- main_achievements %>%
        group_by(project, district ,member, indicator_code, milestone) %>%
        summarise(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE)
          )
      
      tgt_pm <- main_achievements %>%
        group_by(project,member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, 0))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      data <- milestone_perc %>%
        group_by(project, milestone,district,indicator_code) %>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          member = first(member),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_district = if_else(target > 0, (achievement / target) * 100, 0)
        )%>%
        group_by(project,district,indicator_code)%>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        group_by(district,indicator_code)%>%
        summarise(
          project = first(project),
          achievement = sum(achievement,na.rm = TRUE),
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          target = sum(target),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          progress =  paste0(format(achievement, big.mark = ",") , "(",pct_district,"%)"),
          Status = ifelse(target==0 , "Achieved",ifelse(progress >100 , "Overachieved",ifelse(progress <100,"Underachieved","Achieved")))
          
        )%>%
        rename(
          Project = project,
          Code = indicator_code,
          Description = indicator_desc,
          `Unit of measurement` = unit_of_measurement,
          Progress = progress,
          District = district
        )%>%
        select(Project,Code,Description,`Unit of measurement`,District,Progress)%>%
        pivot_wider(id_cols = c(Project,Code,Description,`Unit of measurement`),names_from = District , values_from = Progress)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
      
      
      
      
      report_name <- "Indicator Achievement by District"
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      col_indices <- which(names(data) %in% numeric_cols) - 1  # JS is 0-indexed
      datatable(
        data,
        rownames = FALSE,
        selection = "none",
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          pageLength = 100,
          columnDefs = list(
            list(
              targets = col_indices,
              render = JS("function(data, type, row, meta) { return data + '%'; }")
            )
          ),
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Indicator Achievement',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            )
          )
        ),
        escape = FALSE
      ) %>%
        {
          dt <- .
          for (col in numeric_cols) {
            dt <- formatStyle(
              dt,
              columns = col,
              color = styleInterval(99, c("#ED7667", "#82BA96")),
              fontWeight = 'bold'
            )
          }
          
          dt
        }
    })
    
    output$mainDistrictDemographicTable <- renderDT(server = FALSE,{
      
      ach_pm <- main_achievements %>%
        group_by(project, district ,member, indicator_code, milestone) %>%
        summarise(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE)
          )
      
      tgt_pm <- main_achievements %>%
        group_by(project,member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, 0))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      data <- milestone_perc %>%
        group_by(project, milestone,district) %>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          member = first(member),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_district = if_else(target > 0, (achievement / target) * 100, 0)
        )%>%
        group_by(project,district)%>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        group_by(district)%>%
        summarise(
          male = format(sum(male),big.mark = ","),
          female = format(sum(female),big.mark = ","),
          minority_male = format(sum(minority_male),big.mark = ","),
          minority_female = format(sum(minority_female),big.mark = ","), 
          disabled_male = format(sum(disabled_male),big.mark = ","),
          disabled_female = format(sum(disabled_female),big.mark = ","),
          achievement = sum(achievement,na.rm = TRUE),
          target = sum(target,na.rm = TRUE),
          progress = round(sum(pct_district) /n()),
          Status = ifelse(target==0 , "Achieved",ifelse(progress >100 , "Overachieved",ifelse(progress <100,"Underachieved","Achieved"))),
          progress = paste0(progress , "%"),
          achievement = format(achievement,big.mark = ","),
          target = format(target,big.mark = ",")
        )%>%
        rename(
          District = district,
          target =target,
          Achievement = achievement,
          Progress = progress,
          Male	= male,
          Female	=female,
          `Minority Male` = minority_male,
          `Minority Female`	=minority_female,
          `Disabled Male` = disabled_male,
          `Disabled Female` =disabled_female
        )%>%
        select(District,target,Achievement,Progress,Status,Male,Female,`Minority Male` ,`Minority Female`,
               `Disabled Male`,`Disabled Female`)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
      
      
      report_name <- "District Demographcis"
      datatable(
        data,
        rownames = FALSE, 
        selection = "none",
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE,
          pageLength = 100, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename =report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Indicator Achievement',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ))%>%
        formatStyle(
          'Status',
          target = 'cell',
          color = DT::styleEqual(
            c("Achieved", "Overachieved","Underachieved"),
            c("#82BA96", "#82BA96","#ED7667")
          )
        )
    })
    
    output$mainIndicatorProgressByQuareterTable <- renderDT(server = FALSE,{
      
      ach_pm <- main_achievements %>%
        group_by(project, quarter ,member, indicator_code, milestone) %>%
        summarise(
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE)
          )
      
      tgt_pm <- main_achievements %>%
        group_by(project,member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, 0))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      data <- milestone_perc %>%
        group_by(project, milestone,quarter,indicator_code) %>%
        reframe(
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          member = first(member),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_district = if_else(target > 0, (achievement / target) * 100, 0)
        )%>%
        group_by(project,quarter,indicator_code)%>%
        reframe(
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        group_by(quarter,indicator_code)%>%
        summarise(
          project = first(project),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          progress =  paste0(format(achievement, big.mark = ",") , "(",pct_district,"%)")
        )%>%
        rename(
          Code = indicator_code,
          Description = indicator_desc,
          `Unit of measurement` = unit_of_measurement,
          Progress = progress,
          Quarter = quarter
        )%>%
        select(Code,Description,`Unit of measurement`,Quarter,Progress)%>%
        pivot_wider(id_cols = c(Code,Description,`Unit of measurement`),names_from = Quarter , values_from = Progress)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
      
     
      
      report_name <- "Indicator Achievement by Quarter"
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      col_indices <- which(names(data) %in% numeric_cols) - 1  # JS is 0-indexed
      datatable(
        data,
        rownames = FALSE,
        selection = "none",
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          pageLength = 100,
          columnDefs = list(
            list(
              targets = col_indices,
              render = JS("function(data, type, row, meta) { return data + '%'; }")
            )
          ),
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Indicator Achievement',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            )
          )
        ),
        escape = FALSE
      ) %>%
        {
          dt <- .
          for (col in numeric_cols) {
            dt <- formatStyle(
              dt,
              columns = col,
              color = styleInterval(99, c("#ED7667", "#82BA96")),
              fontWeight = 'bold'
            )
          }
          
          dt
        }
    })
    
    output$mainQuarterDemographicTable <- renderDT(server = FALSE,{
      
      ach_pm <- main_achievements %>%
        group_by(project, quarter ,member, indicator_code, milestone) %>%
        summarise(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement, na.rm = TRUE)
          )
      
      tgt_pm <- main_achievements %>%
        group_by(project,member, indicator_code, milestone) %>%
        summarise(target = dplyr::first(target)) %>%   # target per member
        mutate(target = replace_na(target, 0))
      
      milestone_perc <- ach_pm %>%
        left_join(tgt_pm, by = c("project","member","indicator_code","milestone")) %>%
        mutate(target = replace_na(target, 0),
               # Use NA when target==0 so those milestones don't skew the mean
               pct_indicator = if_else(target > 0, (achievement / target) * 100, NA_real_))
      
      # 2) Project-level % = mean of milestone % within each project (unweighted)
      data <- milestone_perc %>%
        group_by(project, milestone,quarter) %>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          member = first(member),
          achievement = sum(achievement),
          target = sum(target),
        ) %>%
        mutate(
          pct_district = if_else(target > 0, (achievement / target) * 100, NA_real_)
        )%>%
        group_by(project,quarter)%>%
        reframe(
          male = sum(male),
          female = sum(female),
          minority_male = sum(minority_male),
          minority_female = sum(minority_female),
          disabled_male = sum(disabled_male),
          disabled_female = sum(disabled_female),
          indicator_desc = first(indicator_desc),
          unit_of_measurement = first(unit_of_measurement),
          achievement = sum(achievement),
          target = sum(target),
          member = first(member),
          pct_district = round(sum(pct_district) /n())
        )%>%
        group_by(quarter)%>%
        summarise(
          project = first(project),
          male = format(sum(male),big.mark = ","),
          female = format(sum(female),big.mark = ","),
          minority_male = format(sum(minority_male),big.mark = ","),
          minority_female = format(sum(minority_female),big.mark = ","), 
          disabled_male = format(sum(disabled_male),big.mark = ","),
          disabled_female = format(sum(disabled_female),big.mark = ","),
          achievement = sum(achievement,na.rm = TRUE),
          target = sum(target,na.rm = TRUE),
          progress = round(sum(pct_district) /n()),
          Status = ifelse(target==0 , "Achieved",ifelse(progress >100 , "Overachieved",ifelse(progress <100,"Underachieved","Achieved"))),
          progress = paste0(progress , "%"),
          achievement = format(achievement,big.mark = ","),
          target = format(target,big.mark = ",")
        )%>%
        rename(
          Quarter = quarter,
          target =target,
          Achievement = achievement,
          Progress = progress,
          Male	= male,
          Female	=female,
          `Minority Male` = minority_male,
          `Minority Female`	=minority_female,
          `Disabled Male` = disabled_male,
          `Disabled Female` =disabled_female
        )%>%
        select(Quarter,target,Achievement,Progress,Status,Male,Female,`Minority Male` ,`Minority Female`,
               `Disabled Male`,`Disabled Female`)%>%
        mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
      
      
      report_name <- "Quarter Demographcis"
      datatable(
        data,
        rownames = FALSE, 
        selection = "none",
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE,
          pageLength = 100, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename =report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Indicator Achievement',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = report_name,
              title = report_name,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ))%>%
        formatStyle(
          'Status',
          target = 'cell',
          color = DT::styleEqual(
            c("Achieved", "Overachieved","Underachieved"),
            c("#82BA96", "#82BA96","#ED7667")
          )
        )
    })
    
  })
  

}