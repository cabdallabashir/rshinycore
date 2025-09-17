



get_data <- function(username, password ,formID){
  
  
  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/",formID,sep = ""), authenticate(username,password))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
  
    if(nrow(formData) <= 0){
      column_names <- c("crc_name","crc_role","enum_info/enum_name","enum_info/enum_sex","enum_info/enum_phone_number","project_info/Project_id","project_info/Member_id",
                        "project_info/Region_id","project_info/District_id","project_info/Community_id",'consent',"group_verification/verification_category",
                        "HH_selection_/hoh_name","HH_selection_/hoh_sex","HH_selection_/tel_number_","HH_selection_/selection_criteria",
                        "groups_selection_criteria/groups","groups_selection_criteria/group_size","groups_selection_criteria/name",
                        "ind_sex","ind_age","ind_position","pos","tel_number","selection_criteria_",
                        "X_submission_time","start","end" , "index")

      
      formData <- data.frame(matrix(ncol = length(column_names), nrow = 0))
      names(formData) <- column_names
    }else{
      if("groups_selection_criteria/group_members" %in% names(formData)){
       
          for (i in seq_along(formData$`groups_selection_criteria/group_members`)) {
            # Access each member in the list
            group_members <- formData$`groups_selection_criteria/group_members`[[i]]
            
            is.list(group_members)

            # Check if member is a list and has the required fields
            if (is.list(group_members)) {
              
              group_members$`groups_selection_criteria/group_members/index` <- seq.int(nrow(group_members))
           
              # Put the modified member back into the dataframe
              formData$`groups_selection_criteria/group_members`[[i]] <- group_members
            }
          }
       
        formData %<>%
          unnest("groups_selection_criteria/group_members",keep_empty = TRUE)%>%purrr::discard(is.list) %>%
          as.data.frame()
      }else{
        formData%<>%as.data.frame()%>%mutate(index = 0)
      }
     
    }

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

df_creteria <- # Create the data frame in one step
  data <- data.frame(
    id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24,1, 2,3,4,5, 6,7, 8
    ),
    description = c(
      "Food insecurity (Skipping meals, Inadequate of Food)",
      "Female headed household",
      "Household with limited variety of food to eat",
      "Household headed by child (less than 18 years old)",
      "Rely on humanitarian assistance",
      "Unable to purchase food",
      "Household with malnutrition child",
      "Household with monthly income below 65 USD",
      "Household with debt",
      "Poor housing condition",
      "Limited access to basic needs including nutritious foods, clean water, health, etc.",
      "Rely on less preferred foods",
      "Borrow foods or rely on help from friends/relatives",
      "Limit number of meals eaten per day",
      "Limit portion size at mealtime",
      "HH with pregnant or lactating woman",
      "HHs with more than 2 children aged under 5.",
      "Minority or marginalized HH",
      "Member of the household is older person (above 60)",
      "HH with disabled person",
      "Member of the household is chronically ill",
      "Internally Displaced Persons (IDP)",
      "Refugee",
      "Someone who can rreprsent the community",
      "Someone representing vulnerable groups",
      "Someone with good experience for selected task",
      "Someone with knowledge of the experts selected for this task",
      "Someone familiar with community leadership",
      "Someone knowledgeable about community",
      "Someone who is always available",
      "Someone who is committed to serving the community"
      
    ),
    selection_cate_category = c(
      "primary", "secondary", "primary", "secondary", "primary", "primary", "primary",
      "primary", "primary", "primary", "primary", "primary", "primary", "primary",
      "primary", "secondary", "secondary", "secondary", "secondary", "secondary",
      "secondary", "secondary", "secondary",
      "primary","primary","primary","primary","primary","primary","primary","primary"
    ),
    group = c(
      "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", 
      "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver",
      "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver",
      "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver",
      "hh_level_or_individual_ver", "hh_level_or_individual_ver", "hh_level_or_individual_ver",
      "groups_ver","groups_ver","groups_ver","groups_ver","groups_ver","groups_ver","groups_ver","groups_ver"
    ),
    stringsAsFactors = FALSE # to prevent factors
  )%>%
  mutate(id = as.character(id))

df_groups <- data.frame(
  variable_names = c(
    "Community_Resilience_Committee_(CRCs)", 
    "Water_system_management_committee", 
    "Dispute_resolution_platforms", 
    "Food_producers_(agricultural/pastoral/fisheries)", 
    "Community_groups_establishment_and/or_training_to_support_community_engagement_on_health_&_nutrition_activities",
    "Participation_in_a_VSLA/self-help/savings_group",
    "EWEA_subcommittees_"
  ),
  descriptions = c(
    "Community Resilience Committee (CRCs)", 
    "Water system management committee", 
    "Dispute resolution platforms", 
    "Food producers/Cooperatives (agricultural/pastoral/fisheries)", 
    "Community groups establishment and/or training to support community engagement on health & nutrition activities (Details of group such as type)",
    "Participation in a VSLA/self-help/savings group",
    "EWEA subcommittees"
  ),
  stringsAsFactors = FALSE # to keep strings as characters
)



brcis3selectionUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/brcis3selection.html",
                 
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
                   value = "2024-01-01"
                 ),
                 toDateController = dateInput(
                   ns("toDateController"),
                   "To Date"
                 ),
                 ProjectSelection = selectInput(
                   ns("ProjectSelection"),
                   "Select Project",
                   choices = unique(baseline_targets$Project_id)
                 ),
                 Segrigation = selectInput(
                   ns("Segrigation"),
                   "Summarise by",
                   choices = segrationDropdown
                 ),
                 
                 warningToLoadDATA = uiOutput(ns("warningToLoadDATA")),
                 Segrigation_element = uiOutput(ns("Segrigation_element")),
                 verificationCreteria = selectInput(ns("verificationCreteriaControll"),
                   label = paste("Verification Category"),
                   choices = c("all","Individual"="hh_level_or_individual_ver","Group"="groups_ver")
                 ),
                 
                 verification_category_chart = plotlyOutput(ns("verification_category_chart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 consentChart = plotlyOutput(ns("consentChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 groups_chart = plotlyOutput(ns("groups_chart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 selection_creteria_chart = plotlyOutput(ns("selection_creteria_chart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 hh_summary_chart = DTOutput(ns("hh_summary_chart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 
                 gender_chart = plotlyOutput(ns("gender_chart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 districtDetails = DTOutput(ns("districtDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 regionDetails = DTOutput(ns("regionDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 memberDetails = DTOutput(ns("memberDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondants = uiOutput(ns("indiRespondants")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 ageGroups = plotlyOutput(ns("ageGroups"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 deleteButton = uiOutput(ns("deleteButton"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 valid_screened = uiOutput(ns("valid_screened"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 invalid_screened = uiOutput(ns("invalid_screened"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 groupedScreening = plotlyOutput(ns("groupedScreening")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 selectionCreteria =DTOutput(ns("selectionCreteria"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 selectionCreteriaChart = plotlyOutput(ns("selectionCreteriaChart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7)
                 
    )
  )
}

showErrorModal <- function(error_message) {
  showModal(modalDialog(
    title = "An Error Occurred",
    paste("Error: ", error_message),
    easyClose = TRUE,
    footer = NULL
  ))
}

brcis3selection <- function(input ,output , session,sharedValues){
  
 
  
  
  global_vars <- reactiveValues(
    global_baseline_survey = NULL,
    main_survey = NULL,
    logged_in_member = NULL
  )
  

  
   
  
  observeEvent(input$submitOnaData , {
    
    tryCatch({
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
      
      Authresult <- validate_member_ona_credentials(onauser,onapass,"800118")
      
      if(!Authresult$status){
        output$messageBox <- renderUI({
          
          HTML(
            paste(
              '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
    									<div class="d-flex align-items-center">
    										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
    										</div>
    										<div class="ms-3">
    											<h6 class="mb-0 text-dark">Warning</h6>
    											<div class="text-dark">',Authresult$message,'</div>
    										</div>
    									</div>
    									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
    								</div>',sep=""
            )
          )
        })
        return()
      }
      
      
      
      if(is.null(Authresult$message)){
        output$messageBox <- renderUI({
          
          HTML(
            '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
    									<div class="d-flex align-items-center">
    										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
    										</div>
    										<div class="ms-3">
    											<h6 class="mb-0 text-dark">Warning</h6>
    											<div class="text-dark">Invalid Member</div>
    										</div>
    									</div>
    									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
    								</div>'
          )
        })
        return()
      }
      
      baseline_survey  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","800118")
      
      
      required_columns <- c("crc_name","crc_role","enum_name","enum_sex","enum_phone_number","Project_id","Member_id","Region_id","District_id","Community_id",'consent',"verification_category",
                            "hoh_name","hoh_sex","hoh_age","tel_number_","selection_criteria","groups","group_size","name",
                            "ind_sex","ind_age","ind_position","pos","tel_number","selection_criteria_",
                            "X_submission_time","start","end","group_members_count","index" )
      
      # Loop through each required column
      for (col in required_columns) {
        # Check if the column exists
        if (!col %in% names(baseline_survey)) {
          # If it does not exist, add it with NA values
          baseline_survey[[col]] <- NA
        }
      }
      
      if(first(baseline_survey$status) == "200"   & first(baseline_survey$message) != "empty"){
        
        
        postgresConnection <- dbConnect(RPostgres::Postgres(),
                                        dbname = "brcisShiny",
                                        host = "brcisproddb.postgres.database.azure.com", port = 5432,
                                        user = "brcisShiny", password = "brcisShiny@112233@",
                                        sslmode = 'require')
        
        
        is_valid_beneficiary <- function(selected_criteria_str, criteria ,vcategory) {
          
         if(vcategory == 'hh_level_or_individual_ver'){
           selected_criteria <- unlist(strsplit(str_replace_all(selected_criteria_str," ",","), ","))
           selected_categories <- criteria$selection_cate_category[criteria$id %in% selected_criteria]
           has_primary <- "primary" %in% selected_categories
           has_secondary <- "secondary" %in% selected_categories
         }else{
           has_primary <- TRUE
           has_secondary <- TRUE
         }
         
          
          
          if(has_primary){
            return('Valid')
          }else{
            return('Invalid')
          }
        }
        
      
        
        columns_to_replace_na <- c("group_members_count")
        
        main_survey_analysis <- baseline_survey %>%
          select(
            index,X_id,crc_name, enum_name,enum_phone_number,crc_role,Project_id, Member_id, Region_id, District_id, Community_id, consent, verification_category, 
            hoh_name, hoh_sex,hoh_age, tel_number_, selection_criteria, groups, group_size, name, ind_sex, ind_age, ind_position, pos,
            tel_number, selection_criteria_, X_submission_time, start, end,group_members_count
          ) %>%
          mutate(
            crc_name = enum_name,
            across(all_of(columns_to_replace_na), ~replace(., is.na(.), 0)),
            tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
            index = ifelse(is.na(index),0 , index),
            bens_id = ifelse(verification_category == 'hh_level_or_individual_ver' , X_id , paste(X_id,index,sep = "")),
            bens_id = trimws(bens_id),
            HoH_fullname = ifelse(is.na(hoh_name) , name ,hoh_name ),
            hoh_sex= ifelse(is.na(hoh_sex) ,ind_sex,hoh_sex),
            hoh_age= ifelse(is.na(hoh_age) ,ind_age,hoh_age),
            tel_number = ifelse(is.na(tel_number_) ,tel_number,tel_number_),
            selection_criteria = ifelse(is.na(selection_criteria) ,selection_criteria_,selection_criteria),
            start_time = format(parse_date_time(start, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            end_time = format(parse_date_time(end, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            Project_id = ifelse(is.na(Project_id),'BRCiS III',Project_id),
           
          )%>%rowwise()%>%
          mutate(
            screening_status = is_valid_beneficiary(selection_criteria, df_creteria,verification_category),
            screening_status_reason = case_when(
              screening_status == "Invalid" ~ 'Must meet at least one primary vulnerability criteria',
              .default = ''
            )
          )%>%
          select(
            screening_status,screening_status_reason,index,tab_date,bens_id,crc_name,enum_name,enum_phone_number, crc_role,Project_id, Member_id, Region_id, District_id, Community_id, consent, verification_category, 
            HoH_fullname, hoh_sex,hoh_age, tel_number, selection_criteria, groups, group_size,ind_position, start_time,end_time
          )%>%filter(!(verification_category == "groups_ver" & index == 0))
        
       

        tables <- dbListTables(postgresConnection)
        if("brcisScreening" %in% tables){
          existing_ids <- dbGetQuery(postgresConnection, 'SELECT bens_id FROM "brcisScreening"')
          df_to_insert <- main_survey_analysis[!main_survey_analysis$bens_id %in% existing_ids$bens_id,]

          if (nrow(df_to_insert) > 0) {
            print("Writing To Database..")
            dbWriteTable(postgresConnection, name = "brcisScreening", value = df_to_insert, append = TRUE, row.names = FALSE)
          } else {
            print("No new records to insert.")
          }
        }else{
          dbWriteTable(postgresConnection , "brcisScreening" , main_survey_analysis)
        }

        if(Authresult$message != 'CMU'){
          main_survey_analysis <- dbGetQuery(postgresConnection,sprintf('SELECT * FROM "brcisScreening" WHERE access_status = \'A\' and "Project_id" = \'%s\' and "Member_id" = \'%s\'',input$ProjectSelection, Authresult$message) )
        }else{
          main_survey_analysis <- dbGetQuery(postgresConnection,sprintf('SELECT * FROM "brcisScreening" WHERE access_status = \'A\' and  "Project_id" = \'%s\'', input$ProjectSelection) )
        }



        dbDisconnect(postgresConnection)

       
        
       
    
        global_vars$global_baseline_survey <- main_survey_analysis
        global_vars$logged_in_member <- Authresult$message
        
        
        if(nrow(main_survey_analysis) == 0){
          output$messageBox <- renderUI({
            HTML(paste(
              '<div class="row">
              <div class="col-12">
                <div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Info</h6>
											<div class="text-dark">NOTE: No Data Found for ',Authresult$message,'</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>
              </div>
            </div>',sep=""
            ))
            
          })
        }else{
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
        }
        
      }else{
        output$messageBox <- renderUI({
          
          resp <- if_else(first(baseline_survey$status) != "200" ,first(baseline_survey$message) , 
                          ifelse(first(baseline_survey$message) == "empty",
                                 paste("No Data is submitted yet"),
                                 paste("Data Cannot be loaded please contact CMU MEAL")
                          )
                          
          )
          
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
    }, error = function(e) {
      output$messageBox <- renderUI({
        HTML(paste(sep = "",
                   '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bxs-message-square-x"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-white">Error</h6>
											<div class="text-white">',e$message,'</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>' 
        ))
      })
      print(e$message)
     
    })
    
   
    
   
    
  })
  
  
  
  # 
  
  

  output$Segrigation_element <- renderUI({
    ns1 <- NS("brcis3selection")
    
    filteredSegs = baseline_targets %>%filter(Project_id == input$ProjectSelection)
    
    selectInput(
      ns1("Segrigation_element_drill"),
      paste("Filter by",input$Segrigation),
      choices = c("all",unique(pull(filteredSegs,all_of(!!sym(input$Segrigation)))))
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
    
   print("here")
   
    main_survey<- global_vars$global_baseline_survey 
    
    main_survey %<>%
      filter(as.Date(tab_date) >= as.Date(input$fromDateController) &
               as.Date(tab_date) <= as.Date(input$toDateController))
    
   
    
    # if(!is.null((input$ProjectSelection))){
    #   main_survey %<>% filter(Project_id == input$ProjectSelection
    #   )
    #   
    # }
    
    if(!is.null((input$Segrigation_element_drill))){
      if((input$Segrigation_element_drill) != "all" & !is.null((input$Segrigation))){
      
        
        main_survey %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )
        
     
      }
      
    }
    
    if(!is.null((input$verificationCreteriaControll))){
      if((input$verificationCreteriaControll) != "all"){
        
        
        main_survey %<>% filter(
          verification_category == input$verificationCreteriaControll
        )
        
        
      }
      
    }
    
    output$consentChart <- renderPlotly({
      consented <- main_survey %>%
        filter(
          consent == "yes"

        )

      nonconsented <- main_survey %>%
        filter(
          consent == "no"

        )

      chartData <- data.frame(
        "name" = c("Consented","Non Consented"),
        "value" = c(nrow(consented),nrow(nonconsented)),
        "color" = c("#008BA8","#ED7667")
      )

      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

      fig
    })
    
    output$verification_category_chart <- renderPlotly({
      verification_category_chart <- main_survey %>%
        filter(
          consent == "yes"

        ) %>% group_by(verification_category)%>%
        reframe(
          count = n()
        )%>%
        mutate(
          verification_category = case_when(
            verification_category == 'groups_ver' ~ 'Group Selection',
            verification_category == 'hh_level_or_individual_ver' ~ 'Individual Selection',
            .default = verification_category
          )
        )




      fig <- plot_ly(verification_category_chart, labels = ~verification_category, values = ~count,
                     type = 'pie',marker = list(colors = c("#008BA8","#ED7667")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

      fig
    })
    
    output$gender_chart <- renderPlotly({
      gender_chart <- main_survey %>%
        filter(
          consent == "yes"

        ) %>% group_by(hoh_sex)%>%
        reframe(
          count = n()
        )


      fig <- plot_ly(gender_chart, labels = ~hoh_sex, values = ~count,
                     type = 'pie',marker = list(colors = c("#008BA8","#ED7667")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

      fig
    })
    
    output$groups_chart <- renderPlotly({

      groups_chart <- main_survey %>%
        filter(
          consent == "yes"
        ) %>%
        group_by(groups)%>%
        reframe(
          group_size = sum(as.numeric(group_size), na.rm = TRUE),
          count= n()
        )%>%
        left_join(df_groups , by = c("groups"="variable_names"))%>%
        mutate(
          group_size = ifelse(is.na(group_size),0 , group_size),
          count = ifelse(is.na(count),0 , count)
          
        )%>%
        drop_na(groups)%>%
        arrange(count)

      groups_chart$text_position <- ifelse(groups_chart$count > 5, 'inside', 'outside')

      groups_chart <- groups_chart %>%
        mutate(descriptions = factor(descriptions, levels = descriptions))


      fig <- plot_ly(groups_chart,
                     x = ~count,
                     y = ~descriptions, type = 'bar', orientation = 'h',
                     text = ~paste("group size",group_size),textposition = "auto",
                     marker = list(color = "#008BA8"))%>%
        layout(
          title = list(text = 'Groups', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top',pad =2),
          xaxis = list(title = "Number of Beneficiaries"))

      fig
      
     

    })
    
    output$selection_creteria_chart <-  renderPlotly({
      
    
      creteria_chart <- main_survey %>%
        filter(
          consent == "yes"
        ) %>%
        separate_rows(selection_criteria, sep = " ")%>%
        group_by(selection_criteria)%>%
        reframe(
          count= n()
        )%>%
        left_join(df_creteria , by = c("selection_criteria"="id"))%>%
        mutate(
          count = ifelse(is.na(count),0 , count)
        )%>%
        drop_na(selection_criteria)%>%
        arrange(count)

      creteria_chart <- creteria_chart %>%
        mutate(description = factor(description, levels = description))

      fig <- plot_ly(creteria_chart,
                     x = ~count,
                     y = ~description, type = 'bar', orientation = 'h',
                     text = ~count,textposition = "auto")%>%
        layout(title = list(text = 'Selection Creteria', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top',pad =2),
          yaxis = list(title=""),
          xaxis = list(title = "Number of Beneficiaries"))

      fig
      
    })
    
    output$hh_summary_chart <- renderDT(server = FALSE , {
    
      

      hh_summary_chart <- main_survey %>%
        filter(
          consent == "yes"
        )%>%
        separate_rows(selection_criteria, sep = " ")%>%
        left_join(df_creteria , by = c("selection_criteria"="id","verification_category"="group"))%>%
        left_join(df_groups , by = c("groups"="variable_names"))%>%
        group_by(bens_id)%>%
        reframe(
          HoH_fullname = first(HoH_fullname),
          date = first(tab_date),
          Role = first(ind_position),
          hoh_sex = first(hoh_sex),
          hoh_age = first(hoh_age),
          tel_number = first(tel_number),
          Member_id = first(Member_id),
          Region_id = first(Region_id),
          District_id = first(District_id),
          Community_id = first(Community_id),
          selection_criteria_id = paste(selection_criteria , collapse = ","),
          selection_criteria = paste(description , '(',selection_cate_category,')' , collapse = "<br/> <br/>"),
          selection_cate_category = paste(selection_cate_category , collapse = ","),
          group = first(descriptions),
          verification_category = first(verification_category),
          screening_status_reason = first(screening_status_reason),
          screening_status = first(screening_status)
        )%>%
        mutate(
          verification_category = case_when(
            verification_category == 'groups_ver' ~ 'Group Selection',
            verification_category == 'hh_level_or_individual_ver' ~ 'Individual Selection',
            .default = verification_category
          )

        )%>%
        replace_na(list(group = ''))



      if(global_vars$logged_in_member == 'CMU'){
        hh_summary_chart$actions <- '';
        #
        hh_summary_chart %<>% select(actions,screening_status,screening_status_reason,bens_id,HoH_fullname,date,Role,hoh_sex,hoh_age,tel_number,Member_id,Region_id,District_id,Community_id,
                                     selection_criteria,   group,   verification_category
        )
        datatable(hh_summary_chart,rownames = FALSE,
                  extensions = c('Buttons'),
                  selection = "none",
                  options = list(
                    scrollX = TRUE,
                    columnDefs = list(
                      list(targets = 9, # Replace 3 with the index of your 'description' column
                           render = htmlwidgets::JS(
                             'function(data, type, row, meta) {
                                console.log(type);
                               return type === "display" ? data : data.replace(/<br\\/>/g, "\\n");
                             }'
                           )
                      ),
                      list(
                        title = 'Select',
                        targets = 0, render = JS(
                          "function(data, type, full, meta) {",
                          "return '<input type=\"checkbox\" class=\"row-checkbox\" value=\"' + full[3] + '\">';",
                          "}")
                      )
                      ),

                    preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                    drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }'),

                    dom = 'lBfrtip',
                    buttons = list(
                      list(
                        extend = 'csv',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             ))
                      ),
                      list(
                        extend = 'excel',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             ))
                      ),
                      list(
                        extend = 'pdf',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             )),
                        customize = JS(
                          "function(doc) {",
                          "doc.content.splice(0, 0, {",
                          "text: 'Member Communities',",
                          "fontSize: 18,",
                          "alignment: 'center'",
                          "});",
                          "}"
                        )
                      ),
                      list(
                        extend = 'print',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             ))
                      )

                    )
                  ),escape = FALSE)%>%
          formatStyle(
            'selection_criteria',
            `overflow-x` = 'hidden',
            `white-space` = 'pre-line'
          )%>%
          formatStyle(
            'screening_status',
            backgroundColor = styleEqual(c('Valid', 'Invalid'),
                                         c("#00B050","#dc3545"))
          )
      }else{
        hh_summary_chart %<>% select(screening_status,screening_status_reason,bens_id,HoH_fullname,date,Role,hoh_sex,hoh_age,tel_number,Member_id,Region_id,District_id,Community_id,
                                     selection_criteria,   group,   verification_category  )
        datatable(hh_summary_chart,rownames = FALSE,
                  extensions = c('Buttons'),
                  selection = "none",
                  options = list(
                    scrollX = TRUE,
                    columnDefs = list(
                      list(targets = 9, # Replace 3 with the index of your 'description' column
                           render = htmlwidgets::JS(
                             'function(data, type, row, meta) {
                                console.log(type);
                               return type === "display" ? data : data.replace(/<br\\/>/g, "\\n");
                             }'
                           )
                      )),


                    dom = 'lBfrtip',
                    buttons = list(
                      list(
                        extend = 'csv',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             ))
                      ),
                      list(
                        extend = 'excel',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             ))
                      ),
                      list(
                        extend = 'pdf',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             )),
                        customize = JS(
                          "function(doc) {",
                          "doc.content.splice(0, 0, {",
                          "text: 'Member Communities',",
                          "fontSize: 18,",
                          "alignment: 'center'",
                          "});",
                          "}"
                        )
                      ),
                      list(
                        extend = 'print',

                        filename = "Member Communities",
                        title = "Member Communities",
                        exportOptions = list(modifier = list(page = 'all'),
                                             format = list(
                                               footer = TRUE
                                             ))
                      )

                    )
                  ),escape = FALSE)%>%
          formatStyle(
            'selection_criteria',
            `overflow-x` = 'hidden',
            `white-space` = 'pre-line'
          )%>%
          formatStyle(
            'screening_status',
            backgroundColor = styleEqual(c('Valid', 'Invalid'),
                                         c("#00B050","#dc3545"))
          )
      }
      
    })
    
    output$valid_screened <- renderUI({
      all <- main_survey%>%filter(consent == "yes")
      valid_screened <- main_survey%>%filter( consent == "yes" &
                                               screening_status == 'Valid')
      valid_screened = nrow(valid_screened)
      all <- nrow(all)

      paste(valid_screened , paste('(%',round((valid_screened/all)*100,2),')',sep = ""))
    })
    
    output$invalid_screened <- renderUI({
      all <- main_survey%>%filter(consent == "yes")
      invalid_screened <- main_survey%>%filter( consent == "yes" &
                                                 screening_status == 'Invalid')
      invalid_screened = nrow(invalid_screened)
      all <- nrow(all)

      paste(invalid_screened , paste('(%',round((invalid_screened/all)*100,2),')',sep = ""))
    })
    
    output$groupedScreening <- renderPlotly({
      groupedScreening <- main_survey%>%
        filter( consent == "yes")%>%
        group_by(!!sym(input$Segrigation))%>%
        reframe(
          Valid = sum(screening_status == 'Valid'),
          Invalid =  sum(screening_status == 'Invalid')
        )

      fig <- plot_ly(groupedScreening, x = as.formula(paste0("~`", input$Segrigation, "`")), y = ~Valid,
                     type = 'bar', name = 'Valid',marker = list(color = '#008BA8'),
                     text = ~Valid,
                     textposition = "outside")
      fig <- fig %>% add_trace(y = ~Invalid, name = 'Invalid',marker = list(color = '#ED7667'),
                               text = ~Invalid,
                               textposition = "outside")
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

      fig
      
    })
     
    output$deleteButton <- renderUI({
      
      if(global_vars$logged_in_member == 'CMU'){
        actionButton("brcis3selection-getChecks", "Delete Selected Data",class = "btn btn-sm btn-danger")
      }else{
        HTML("")
      }
    })
      
   
    
    output$memberDetails <- renderDT({

      memberDetails <- main_survey%>%
        filter(consent == "yes")%>%
        group_by(Member_id)%>%
        reframe(
          Individual =sum(verification_category == "hh_level_or_individual_ver", na.rm = TRUE),
          Groups = n_distinct(ifelse(verification_category == "groups_ver", groups, NA_character_),na.rm = TRUE),
          Beneficiaries = n()
        )

      datatable(
        memberDetails,
        options = list(
          scrollX = TRUE,
          pageLength = 5,

          dom = 'lt'
        )
      )
    })
    
    output$regionDetails <- renderDT({
      regionDetails <- main_survey%>%
        filter(consent == "yes")%>%
        group_by(Region_id)%>%
        reframe(
          Individual =sum(verification_category == "hh_level_or_individual_ver", na.rm = TRUE),
          Groups = n_distinct(ifelse(verification_category == "groups_ver", groups, NA_character_),na.rm = TRUE),
          Beneficiaries = n()
        )

      datatable(
        regionDetails,
        options = list(
          scrollX = TRUE,
          pageLength = 5,

          dom = 'lt'
        )
      )
    })
    
    output$districtDetails <- renderDT({
      districtDetails <- main_survey%>%
        filter(consent == "yes")%>%
        group_by(District_id)%>%
        reframe(
          Individual =sum(verification_category == "hh_level_or_individual_ver", na.rm = TRUE),
          Groups = n_distinct(ifelse(verification_category == "groups_ver", groups, NA_character_),na.rm = TRUE),
          Beneficiaries = n()
        )

      datatable(
        districtDetails,
        options = list(
          scrollX = TRUE,
          pageLength = 5,

          dom = 'lt'
        )
      )
    })
    
    output$indiRespondants <- renderUI({
      indiRespondants <- main_survey%>%
        filter(consent == "yes")
     
      nrow(indiRespondants)
    })
    
    output$ageGroups <- renderPlotly({
      ageGroups <- main_survey%>%
        filter(consent == "yes")%>%
        pull(hoh_age)



      fig <- plot_ly(x = ageGroups, type = "histogram",
                     marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")),
                     xbins = list(start = min(ageGroups), end = max(ageGroups), size = 5)) %>% # You can adjust the size for different bin widths
        layout(title = "Age Distribution",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Count"),
               bargap = 0.05)

      # Show the plot
      fig
    })
    
   
    
    output$selectionCreteria <- renderDT(server = FALSE , {
      
      selection_criteria_summary <- main_survey %>%
        filter(
          consent == "yes"
        )%>%
        separate_rows(selection_criteria, sep = " ")%>%
        left_join(df_creteria , by = c("selection_criteria"="id"))%>%
        group_by(selection_criteria)%>%
        reframe(
          description = first(description),
          Category = first(selection_cate_category),
          Total_selection = n()
        )%>%
        mutate(
          `Selection_%` = paste(round((Total_selection / sum(Total_selection)) * 100,2),'%',sep = ""),
          selection_criteria = as.numeric(selection_criteria)
        )%>%
        dplyr::rename(id=selection_criteria)%>%
        arrange(Category,id)
      datatable(selection_criteria_summary,
                caption = 'Creterias are multi-select , single beneficiary can be in different Creterias',
                rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  scrollX = TRUE,

                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',

                      filename = "Selection Creteria",
                      title = "Selection Creteria",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',

                      filename = "Selection Creteria",
                      title = "Selection Creteria",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',

                      filename = "Selection Creteria",
                      title = "Selection Creteria",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Member Communities',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',

                      filename = "Selection Creteria",
                      title ="Selection Creteria",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )

                  )
                ),escape = FALSE)
    })
    
    output$selectionCreteriaChart <- renderPlotly({
      
      selection_criteria_summary <- main_survey %>%
        filter(
          consent == "yes"
        )%>%
        separate_rows(selection_criteria, sep = " ")%>%
        left_join(df_creteria , by = c("selection_criteria"="id"))%>%
        group_by(selection_criteria)%>%
        reframe(
          description = first(description),
          Category = first(selection_cate_category),
          Total_selection = n()
        )%>%
        mutate(
          `Selection_%` = paste(round((Total_selection / sum(Total_selection)) * 100,2),'%',sep = ""),
          selection_criteria = as.numeric(selection_criteria)
        )%>%
        dplyr::rename(id=selection_criteria)%>%
        arrange(Category,id)

      fig <- plot_ly(selection_criteria_summary, y = ~description, x = ~Total_selection, type = 'bar',
                     orientation ="h",
                     text = ~paste(round(Total_selection,1)),
                     textposition = "outside",
                     color = ~Category)
      fig <- fig %>% layout(xaxis = list(title = 'Total Selection'),yaxis = list(title = ''))

      fig
    })
    
    
   
  })
  
  observeEvent(input$getChecks, {
    
    runjs('
      var checked = [];
      $(".row-checkbox:checked").each(function() {
        checked.push(this.value);
      });
      Shiny.setInputValue("brcis3selection-checked_rows", checked);
    ')
    
    showModal(modalDialog(
      title = "Button Clicked",
      tags$div(
        tagList(
          uiOutput("brcis3selection-ms"),
          textInput(
            "brcis3selection-us",
            "enter User name"
          ),
          passwordInput(
            "brcis3selection-ps",
            "enter password"
          ),
          actionButton("brcis3selection-sb","submit",class="btn btn-primary btn-block")
        )
      )
    ))
    
    
  })
  
  observeEvent(input$sb ,{
    us <- input$us
    ps <- input$ps
    
    if(us == "cmuAdmin" & ps =="cmuAdmin"){
      
      
      
      
      
      if(length(input$checked_rows) > 0){
        items_to_delete_str <- paste(sprintf("'%s'", input$checked_rows), collapse = ",")
        query <- sprintf('DELETE FROM public."brcisScreening" WHERE bens_id IN (%s)', items_to_delete_str)

        postgresConnection <- dbConnect(RPostgres::Postgres(),
                                        dbname = "brcisShiny",
                                        host = "brcisproddb.postgres.database.azure.com", port = 5432,
                                        user = "brcisShiny", password = "brcisShiny@112233@",
                                        sslmode = 'require')
        dbExecute(postgresConnection, query)
        print(input$checked_rows)
        output$ms <- renderUI({
          HTML(
            paste(
              '<div class="row">
              <div class="col-12">
                <div class="alert alert-success border-0 bg-success alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Info</h6>
											<div class="text-dark">',length(input$checked_rows),' Removed - Please Reload the Dashboard by clicking the relad button at top right of the dashboard</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>
              </div>
            </div>',sep=""
            )
          )
        })
        
        print(input$checked_rows)
        dbDisconnect(postgresConnection)
        
      }else{
        output$ms <- renderUI({
          HTML(
            paste(
              '<div class="row">
              <div class="col-12">
                <div class="alert alert-warning border-0 bg-warning alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Info</h6>
											<div class="text-dark">Nothing Is selected</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>
              </div>
            </div>',sep=""
            )
          )
        })
      }
      
      
      
      
      
    }else{
      output$ms <- renderUI({
        
        HTML(
          '<div class="row">
              <div class="col-12">
                <div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Info</h6>
											<div class="text-dark">Incorrect Password</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>
              </div>
            </div>'
        )
      })
      
    }
  })
  
  
}

