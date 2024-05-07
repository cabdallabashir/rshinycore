



get_data <- function(username, password ,formID){
  
 
  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/",formID,sep = ""), authenticate(username,password))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()

  
    if("groups_registration/group_members" %in% names(formData)){
      formData %<>%
        unnest("groups_registration/group_members",keep_empty = TRUE)%>%
        as.data.frame()%>%mutate(index = row_number())%>%
        dplyr::rename(
          name_group_members = "groups_registration/group_members/name",
          ind_sex_group_members  = "groups_registration/group_members/ind_sex",
          ind_age_group_members  ="groups_registration/group_members/ind_age",
          ind_position_group_members  ="groups_registration/group_members/ind_position",
          tel_number_group_members  ="groups_registration/group_members/tel_number",
          disability_group_members  ="groups_registration/group_members/disability_",
          disability_Type_group_members  ="groups_registration/group_members/disability_Type_",
          minority_group_members  ="groups_registration/group_members/minority_",
          representative_criteria_group_members  ="groups_registration/group_members/representative_criteria",
          vulnerability_other_group_members  ="groups_registration/group_members/vulnerability_other",
          
        )
    }
    
    if("hh_registration/hh_members" %in% names(formData)){
     
      for (i in seq_along(formData$`hh_registration/hh_members`)) {
        # Access each member in the list
        member <- formData$`hh_registration/hh_members`[[i]]
        
        # Check if member is a list and has the required fields
        if (is.list(member)) {
          # Standardize to one age column, converting all to character for consistency
          if ("hh_registration/hh_members/ind_age_2" %in% names(member)) {
            member$ind_age_final <- as.character(member$`hh_registration/hh_members/ind_age_2`)
          } else if ("hh_registration/hh_members/ind_age" %in% names(member)) {
            member$ind_age_final <- as.character(member$`hh_registration/hh_members/ind_age`)
          }
          # Remove old age columns if they exist
          
          member$`hh_registration/hh_members/ind_age_2` <- NULL
          member$`hh_registration/hh_members/ind_age` <- NULL
          
          # Put the modified member back into the dataframe
          formData$`hh_registration/hh_members`[[i]] <- member
        }
      }
      
      
      formData <- formData %>%
        unnest(`hh_registration/hh_members`, keep_empty = TRUE) %>%
        as.data.frame() %>%
        mutate(index = row_number()) %>%
        rename(
          name_hh_members = "hh_registration/hh_members/name",
          ind_sex_hh_members = "hh_registration/hh_members/ind_sex",
          ind_age_hh_members = "ind_age_final",
          tel_number_hh_members = "hh_registration/hh_members/tel_number"
        )
    }
    
   
    
    formData %<>%purrr::discard(is.list) 
    
    names(formData) <- gsub(".*/", "", names(formData))
    
    
    
    if(nrow(formData) == 0){
      return(
        data.frame(
          status= 200,
          message = "empty",
          data = ""
        )
      )
    }else{
      return(
        data.frame(
          status= 200,
          message = "success",
          formData
        )
      )
    }
    
    
  } 
  else if (status == 401) {
    # Unauthorized - likely incorrect username/password
    return(
      data.frame(
        status= 401,
        message = "incorrect username/password",
        data = ""
      )
    )
  } 
  else if (status >= 500) {
    # Server error
    return(
      data.frame(
        status= 500,
        message = "Server error: Try again later.",
        data = ""
      )
    )
  } 
  else {
    # Other errors
    return(
      data.frame(
        status= status,
        message = paste("Error encountered. Status code:", status),
        data = ""
      )
    )
  }
  
  
}



baseline_targets <- readxl::read_excel("docs/brcis_selecttion_communities.xlsx")
segrationDropdown <- names(baseline_targets[c("Member_id",	"Region_id",	"District_id",	"Community_id")])





brcisRegistrationUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/brcisRegistration.html",
                 
                 onaUser = textInput(
                   ns("onaUser"),
                   "Enter Ona Username"
                 ),
                 onaPass = passwordInput(
                   ns("onaPass"),
                   "Enter Ona Password"
                 ),
                 submitOnaData = actionButton(
                   ns("submitOnaData"),
                   "submit",
                   class="btn btn-primary"
                 ),
                 
                 messageBox = uiOutput(ns("messageBox")),
                 fromDateController = dateInput(
                   ns("fromDateController"),
                   "From Date",
                   value = "2023-01-01"
                 ),
                 toDateController = dateInput(
                   ns("toDateController"),
                   "To Date"
                 ),
                 Segrigation = selectInput(
                   ns("Segrigation"),
                   "Summarise by",
                   choices = segrationDropdown
                 ),
                 
                 warningToLoadDATA = uiOutput(ns("warningToLoadDATA")),
                 Segrigation_element = uiOutput(ns("Segrigation_element")),
                 interviewProgressByMonth = plotlyOutput(ns("interviewProgressByMonth"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 MemberAnalysisChart = plotlyOutput(ns("MemberAnalysisChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 MemberAnalysisTable =DTOutput(ns("MemberAnalysisTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 DistrictAnalysisChart = plotlyOutput(ns("DistrictAnalysisChart"),height = 800)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 DistrictAnalysisTable = DTOutput(ns("DistrictAnalysisTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 CommunityAnalysisChart = plotlyOutput(ns("CommunityAnalysisChart"),height = 1200)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 CommunityAnalysisTable = DTOutput(ns("CommunityAnalysisTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 verification_category_chart =  plotlyOutput(ns("verification_category_chart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 consentChart = plotlyOutput(ns("consentChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 respondantMaritaStatus = plotlyOutput(ns("respondantMaritaStatus") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondants = 
                 
    )
  )
}

brcisRegistration <- function(input ,output , session,sharedValues){
  
 
  
  
  global_vars <- reactiveValues(
    global_baseline_survey = NULL,
    main_survey = NULL,
  )
  

  
   
  
  observeEvent(input$submitOnaData , {
    
    
    if((input$onaUser == "" | is.na(input$onaUser)) | (input$onaPass == "" | is.na(input$onaPass))){
      output$messageBox <- renderUI({
       
        HTML(
          '<div class="alert alert-warning border-0 bg-warning alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Warning</h6>
											<div class="text-dark">Both user and password required!</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>'
        )
      })
      return()
    }
   
   
    
    onauser <- input$onaUser
    onapass <- input$onaPass

    
    baseline_survey  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","788509")
    
    baseline_survey%>%
      group_by(consent)%>%
      reframe(
        count = n()
      )
    
    max(baseline_survey$X_submission_time)

    required_columns <- c("enum_name", "enum_gender", "enum_phone_number", "Member_id",
                          "Region_id", "District_id", "Community_id", "consent",
                          "registration_category", "regis_activities", "participants_name",
                          "participants_sex", "tel_num", "ecosystem_initiatives", "recovery_assistance_farmers",
                          "recovery_assistance_market_actors",
                          "nfi_", "kitchen_garden", "hoh_name", "hoh_sex", "hoh_age", "tel_number",
                          "marital_status_hoh", "hoh_educ_level", "residential_status", "residential_date",
                          "decision_migrate", "lvhd_zone", "income_sources",
                          "income_sources_O", "income_sources_main", "income_amount", "hh_size",
                          "name_hh_members", "ind_sex_hh_members", "ind_age_hh_members", "tel_number_hh_members",
                          "vulnerability_criteria", "vulnerability_other_", "no_disabled_member",
                          "disability_Type", "clan_minority",
                          "groups", "specify_", "community_groups_", "date_created", "group_size",
                          "name_group_members", "ind_sex_group_members", "ind_age_group_members",
                          "ind_position_group_members", "tel_number_group_members", "disability_group_members",
                          "disability_Type_group_members", "minority_group_members", "representative_criteria_group_members",
                          "infrast_reg", "wast_management", "water_source_", "Ecosystem_infrastructure",
                          "infrast_status", "estimated_cost", "comm_contribution_cost",
                          "estimated_pple", "Picture", "infrast_geopoint", "contact_info",
                          "water1", "water2", "water3", "water3_1", "water4"
                          
    )
    
    # Loop through each required column
    for (col in required_columns) {
      # Check if the column exists
      if (!col %in% names(baseline_survey)) {
        # If it does not exist, add it with NA values
        baseline_survey[[col]] <- NA
      }
    }
    
    if(first(baseline_survey$status) == "200"   & first(baseline_survey$message) != "empty"){
     
      columns_to_replace_na <- c()
      
      main_survey_analysis <-  baseline_survey %>% select(X_submission_time,start,end,X_id,index,
        enum_name, enum_gender, enum_phone_number, Member_id,
        Region_id, District_id, Community_id, consent,
        registration_category, regis_activities, participants_name,
        participants_sex, tel_num, ecosystem_initiatives, recovery_assistance_farmers,
        recovery_assistance_market_actors,
        nfi_, kitchen_garden, hoh_name, hoh_sex, hoh_age, tel_number,
        marital_status_hoh, hoh_educ_level, residential_status, residential_date,
        decision_migrate, lvhd_zone, income_sources,
        income_sources_O, income_sources_main, income_amount, hh_size,
        name_hh_members, ind_sex_hh_members, ind_age_hh_members, tel_number_hh_members,
        vulnerability_criteria, vulnerability_other_, no_disabled_member,
        disability_Type, clan_minority,
        groups, specify_, community_groups_, date_created, group_size,
        name_group_members, ind_sex_group_members, ind_age_group_members,
        ind_position_group_members, tel_number_group_members, disability_group_members,
        disability_Type_group_members, minority_group_members, representative_criteria_group_members,
        infrast_reg, wast_management, water_source_, Ecosystem_infrastructure,
        infrast_status, estimated_cost, comm_contribution_cost,
        estimated_pple, Picture, infrast_geopoint, contact_info,
        water1, water2, water3, water3_1, water4
        
      ) %>%
        mutate(
          across(all_of(columns_to_replace_na), ~replace(., is.na(.), 0)),
          tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
          bens_id = paste(X_id,index,sep = ""),
          id = X_id,
          start_time = format(parse_date_time(start, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
          end_time = format(parse_date_time(end, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
          consent = "yes"
          )
      
      
    
      global_vars$global_baseline_survey <- main_survey_analysis
      

      output$messageBox <- renderUI({
        HTML('<div class="alert alert-success border-0 bg-success alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bxs-check-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-white">Success</h6>
											<div class="text-white">Data Loaded Successfully</div>
										</div>
									</div>
									
								</div>')
      })
      
    }else{
      output$messageBox <- renderUI({
       
        resp <- if_else(first(baseline_survey$status) != "200" ,first(baseline_survey$message) , paste("Data Cannot be loaded please contact CMU MEAL"))
        
        HTML(paste(sep = "",
          '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bxs-message-square-x"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-white">Error</h6>
											<div class="text-white">',resp,'</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>' 
        ))
        })
    }
    
   
    
  })
  
  
  
  # 
  
  

  output$Segrigation_element <- renderUI({
    ns1 <- NS("brcisRegistration")
    titleFilter <- ifelse(is.na(input$Segrigation) , "",input$Segrigation)
    choicesFilter <- ifelse(is.na(input$Segrigation) , c("all"),c("all",unique(pull(baseline_targets,all_of(!!sym(input$Segrigation))))))
    selectInput(
      ns1("Segrigation_element_drill"),
      paste("Filter by",input$Segrigation),
      choices = c("all",unique(pull(baseline_targets,all_of(!!sym(input$Segrigation)))))
    )
  })
  

  
  output$warningToLoadDATA <- renderUI({
    HTML('<div class="row">
              <div class="col-12">
                <div class="alert alert-warning border-0 bg-warning alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Instruction</h6>
											<div class="text-dark">NOTE: the data is not loaded yet, please Load the survey data by clicking the load button at the top-right to use the dashboard!</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>
              </div>
            </div>')
    
  })
  
  
  observe({
    
    if( is.null(nrow(global_vars$global_baseline_survey)) || nrow(global_vars$global_baseline_survey) == 0){
      return()
    }
    
    output$warningToLoadDATA <- renderUI({
      HTML("")
    })
    
   
   
    main_survey<- global_vars$global_baseline_survey 
    
    main_survey %<>%
      filter(as.Date(tab_date) >= as.Date(input$fromDateController) &
               as.Date(tab_date) <= as.Date(input$toDateController))
    
  
   
    
    if(!is.null((input$Segrigation_element_drill))){
      if((input$Segrigation_element_drill) != "all" & !is.null((input$Segrigation))){
      
        
        main_survey %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )
        
     
      }
      
    }
    
    output$interviewProgressByMonth <- renderPlotly({
      data <- main_survey %>%
        group_by(bens_id)%>%
        reframe(tab_date = first(tab_date))%>%
        # mutate(month_year = format(tab_date, "%Y-%m"))%>%
        dplyr::rename(month_year = tab_date)%>%
        group_by(month_year)%>%
        reframe(
          `Total of Registation` = n()
        )%>%
        arrange(month_year)
      
      fig <- plot_ly(data, x = ~month_year, y = ~`Total of Registation`, name = 'Month', type = 'scatter', mode = 'lines+markers+text' ,
                     line = list(shape = 'spline'),
                     marker = list(color = "#ED7667", size = 10),
                     text = ~`Total of Registation` , textposition = "outside")

      fig
      
    })
    
    output$MemberAnalysisTable <- renderDT(server = FALSE , {
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(Member_id)%>%
        summarise(
          `Total Registation` = n() 
        )%>%
        mutate(`Registration %` = `Total Registation` / sum(`Total Registation`,na.rm = TRUE) * 100,
               `Total Registation` = format(`Total Registation`,big.mark=","),
               `Registration %`= round(`Registration %`,2)
               )
      
      datatable(data,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  scrollX = TRUE, 
                  pageLength = 30,
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      
                      filename = "Member Analysis",
                      title = "Member Analysis",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',
                      
                      filename = "Member Analysis",
                      title = "Member Analysis",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',
                      
                      filename = "Member Analysis",
                      title = "Member Analysis",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Member Analysis',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      
                      filename = "Member Analysis",
                      title = "Member Analysis",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )
                    
                  )
                ),escape = FALSE)
    })
    
    output$MemberAnalysisChart <- renderPlotly({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(Member_id)%>%
        summarise(
          `Total Registation` = n()
        )
      
      fig <- plot_ly(data, labels = ~Member_id, values = ~`Total Registation`, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$DistrictAnalysisTable <- renderDT({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(District_id)%>%
        summarise(
          `Total Registation` = n() 
        )%>%
        mutate(`Registration %` = `Total Registation` / sum(`Total Registation`,na.rm = TRUE) * 100,
               `Total Registation` = format(`Total Registation`,big.mark=","),
               `Registration %`= round(`Registration %`,2)
        )
      Label <- 'District Summary'
      datatable(data,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  scrollX = TRUE, 
                  pageLength = 30,
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: '',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )
                    
                  )
                ),escape = FALSE)
    })
    
    output$DistrictAnalysisChart <- renderPlotly({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(District_id)%>%
        summarise(
          `Total Registation` = n()
        )
      
      fig <- plot_ly(data, labels = ~District_id, values = ~`Total Registation`, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
    })
    
    output$CommunityAnalysisTable <- renderDT(server = FALSE ,{
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(Community_id)%>%
        summarise(
          `Total Registation` = n() 
        )%>%
        mutate(`Registration %` = `Total Registation` / sum(`Total Registation`,na.rm = TRUE) * 100,
               `Total Registation` = format(`Total Registation`,big.mark=","),
               `Registration %`= round(`Registration %`,2)
        )
      
      Label <- 'Community Summary'
      datatable(data,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  scrollX = TRUE, 
                  pageLength = 30,
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: '',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      
                      filename = Label,
                      title = Label,
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )
                    
                  )
                ),escape = FALSE)
    })
    
    output$CommunityAnalysisChart <- renderPlotly({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(Community_id)%>%
        summarise(
          `Total Registation` = n()
        )
      
      fig <- plot_ly(data= data , x = ~`Total Registation`, y = ~Community_id, type = 'bar', orientation = 'h')%>%
        add_annotations(text = ~`Total Registation`, x = ~`Total Registation`, y = ~Community_id,
                        showarrow = FALSE, font = list(color = 'black', size = 14))
      
      fig
    })
    
    output$consentChart <- renderPlotly({
      data <- main_survey %>%
        group_by(consent)%>%
        summarise(
          count = n()
        )
      
      fig <- plot_ly(data, labels = ~consent, values = ~count, type = 'pie',
                     marker = list(colors = c("#008BA8","#40419A","#ED7667")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$verification_category_chart <- renderPlotly({
      data <- main_survey %>%
        group_by(registration_category)%>%
        summarise(
          count = n()
        )
      
      fig <- plot_ly(data, labels = ~registration_category, values = ~count, type = 'pie',
                     marker = list(colors = c("#008BA8","#40419A","#ED7667")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    respondant_data <- main_survey %>% 
      group_by(X_id)%>%
      summarise(
        verification_category_chart = first(verification_category_chart),
        hoh_name = first(hoh_name), 
        hoh_sex = first(hoh_sex), 
        hoh_age = first(hoh_age), 
        tel_number = first(tel_number),
        marital_status_hoh = first(marital_status_hoh), 
        hoh_educ_level = first(hoh_educ_level), 
        residential_status = first(residential_status),
        lvhd_zone = first(lvhd_zone), 
        income_sources = first(income_sources),
        income_sources_main = first(income_sources_main), 
        income_amount = first(income_amount),
        hh_size = first(hh_size)
      )
    
    output$respondantMaritaStatus <- renderPlotly({
     
      respondantMaritaStatus <- respondant_data%>%
        filter(verification_category_chart == 'hh_level_or_individual_reg')
        group_by(marital_status_hoh)%>%
        reframe(
          count = n()
        )
      
      fig <- plot_ly(respondantMaritaStatus, labels = ~marital_status_hoh, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
      
    })
    

   
  })
  
}

