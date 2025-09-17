



get_data <- function(username, password ,formID){
  
 
  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/",formID,sep = ""), authenticate(username,password))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
# 
#     formData %>% 
#       rowwise()%>%
#       mutate (`groups_registration/group_members/screening_bens_id_` = ifelse("groups_registration/group_members/screening_bens_id_" %in% names(.), 
#                                                                               `groups_registration/group_members/screening_bens_id_`, "test"))%>% 
#       group_by(`groups_registration/group_members/screening_bens_id_`)%>% summarise(n=n())
    
    
    if("groups_registration/group_members" %in% names(formData)){
      
      for (i in seq_along(formData$`groups_registration/group_members`)) {
        # Access each member in the list
        group_members <- formData$`groups_registration/group_members`[[i]]
        
       
        # Check if member is a list and has the required fields
        if (is.list(group_members)) {
          
          group_members$`groups_registration/group_members/index` <- seq.int(nrow(group_members))
          
          # Put the modified member back into the dataframe
          formData$`groups_registration/group_members`[[i]] <- group_members
        }
      }
      
      formData %<>%
        unnest("groups_registration/group_members",keep_empty = TRUE)%>%
        as.data.frame()
      
      columns_to_rename <- c(
        "groups_registration/group_members/name" = "name_group_members",
        "groups_registration/group_members/ind_sex" = "ind_sex_group_members",
        "groups_registration/group_members/ind_age" = "ind_age_group_members",
        "groups_registration/group_members/ind_position" = "ind_position_group_members",
        "groups_registration/group_members/tel_number" = "tel_number_group_members",
        "groups_registration/group_members/disability_" = "disability_group_members",
        "groups_registration/group_members/disability_Type_" = "disability_Type_group_members",
        "groups_registration/group_members/minority_" = "minority_group_members",
        "groups_registration/group_members/representative_criteria" = "representative_criteria_group_members",
        "groups_registration/group_members/vulnerability_other" = "vulnerability_other_group_members",
        "groups_registration/group_members/index" = "index"
      )
      
      # Check if each column exists and conditionally add it if missing
      for (original_name in names(columns_to_rename)) {
        if (!(original_name %in% colnames(formData))) {
          formData[[original_name]] <- NA  # Add with NA values or adjust as needed
        }
      }
      
      formData <- formData %>%
        rename_at(vars(names(columns_to_rename)), ~ columns_to_rename[.])
      
      if("groups_registration/group_members/screening_bens_id_" %in% names(formData)){
        formData %<>% dplyr::rename( screening_bens_id_group_members = "groups_registration/group_members/screening_bens_id_")
      }
    }
    
    # if("hh_registration/hh_members" %in% names(formData)){
    #
    #   for (i in seq_along(formData$`hh_registration/hh_members`)) {
    #     # Access each member in the list
    #     member <- formData$`hh_registration/hh_members`[[i]]
    #
    #     # Check if member is a list and has the required fields
    #     if (is.list(member)) {
    #       # Standardize to one age column, converting all to character for consistency
    #       if ("hh_registration/hh_members/ind_age_2" %in% names(member)) {
    #         member$ind_age_final <- as.character(member$`hh_registration/hh_members/ind_age_2`)
    #       } else if ("hh_registration/hh_members/ind_age" %in% names(member)) {
    #         member$ind_age_final <- as.character(member$`hh_registration/hh_members/ind_age`)
    #       }
    #       # Remove old age columns if they exist
    #
    #       member$`hh_registration/hh_members/ind_age_2` <- NULL
    #       member$`hh_registration/hh_members/ind_age` <- NULL
    #
    #       # Put the modified member back into the dataframe
    #       formData$`hh_registration/hh_members`[[i]] <- member
    #     }
    #   }


    #   formData <- formData %>%
    #     unnest(`hh_registration/hh_members`, keep_empty = TRUE) %>%
    #     as.data.frame() %>%
    #     mutate(index = row_number()) %>%
    #     rename(
    #       name_hh_members = "hh_registration/hh_members/name",
    #       ind_sex_hh_members = "hh_registration/hh_members/ind_sex",
    #       ind_age_hh_members = "ind_age_final",
    #       tel_number_hh_members = "hh_registration/hh_members/tel_number"
    #     )
    # }
    
   
    
    # formData %<>%purrr::discard(is.list) 
    
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

activities <- data.frame(
  Code = c("Gender_equality_and_social_inclusion_(GESI)_training",
                   "Training_on_ecosystem_restoration_activities",
                   "Training_on_Collaborative_Dispute_Resolution_(CDR)_techniques",
                   "Community-led_ecosystem_restoration_initiatives",
                   "Sustainable_Waste_management_infrastructures",
                   "Flood_defenses_river_embankments_rehabilitation_or_improvement_initiatives_through_Cash_for_Work_(CFW)",
                   "Livestock_treatment_or_prevention_from_vector-borne_diseases",
                   "Climate_resilient_water_sources_",
                   "Flood_recovery_assistance_for_farmers",
                   "Distribution_of_Non-Food-Items_(NFIs)",
                   "Kitchen_gardens_support_package",
                   "Business_capital_access_through_the_project",
                   "Flood_recovery_assistance_(Restocking_grants,_etc)_for_market_actors"),
Description = c("Gender equality and social inclusion (GESI) training",
                          "Training on ecosystem restoration activities",
                          "Training on Collaborative Dispute Resolution (CDR) techniques",
                          "Community-led ecosystem restoration initiatives (Specify type of initiative)",
                          "Sustainable Waste management infrastructures",
                          "Flood defenses/river embankments rehabilitation or improvement initiatives through Cash for Work (CFW)",
                          "Livestock treatment or prevention from vector-borne diseases",
                          "Climate resilient water sources",
                          "Flood recovery assistance for farmers (Specify Type of Assistance)",
                          "Distribution of Non-Food-Items (NFIs) (Specify Type of NFIs)",
                          "Kitchen gardens support package (Specify Type of Support)",
                          "Business capital access through the project",
                          "Flood recovery assistance (Restocking grants, etc) for market actors"),
  stringsAsFactors = FALSE
)

# Define the data frame
groups <- data.frame(
  code = c(
    "Community_Resilience_Committee_(CRCs)",
    "Community_led_Platforms",
    "Water_system_management_committee",
    "Dispute_resolution_platforms",
    "Food_producers_(agricultural/pastoral/fisheries)_provision_of_inputs_and_training_on_sustainable_climate_resilient_practices_",
    "Producer_cooperatives_or_associations_support_on_contract_farming",
    "Community_groups_establishment_and/or_training_to_support_community_engagement_on_health_&_nutrition_activities",
    "Participation_in_a_VSLA/self-help/savings_group",
    "Producer_saving_groups_linked_with_financial_services",
    "Smallholder_producers/saving_groups_accessing_business_skills_training_or_coaching",
    "EWEA_subcommittees_",
    "Community_health_workers"
  ),
  description = c(
    "Community Resilience Committee (CRCs)",
    "Community-led Platforms (Specify Type of Platform)",
    "Water system management committee",
    "Dispute resolution platforms",
    "Food producers (agricultural/pastoral/fisheries) provision of inputs and training on sustainable climate resilient practices",
    "Producer cooperatives or associations support on contract farming",
    "Community groups establishment and/or training to support community engagement on health & nutrition activities (Details of group such as type)",
    "Participation in a VSLA/self-help/savings group",
    "Producer/saving groups linked with financial services",
    "Smallholder producers/saving groups accessing business skills training or coaching",
    "EWEA subcommittees",
    "Community health workers (CHWs)"
  ),
  stringsAsFactors = FALSE  # Option to prevent factors
)

infrastructure <- data.frame(
  code = c(
    "Sustainable_Waste_management_infrastructures",
    "flood_defences_river_breakages_rehabilitation/improvement",
    "Area-level_fodder_storage_units_rehabilitated/constructed",
    "Climate_resilient_water_sources_rehabilitated_or_constructed",
    "Community_led_ecosystem_restoration_initiatives"
  ),
  description = c(
    "Sustainable Waste management infrastructures (Specify Type of waste management infrastructure)",
    "flood defences/river breakages rehabilitation/improvement",
    "Area-level fodder storage units rehabilitated/constructed",
    "Climate resilient water sources rehabilitated or constructed (Specify Type of water Point)",
    "Community-led ecosystem restoration initiatives"
  ),
  stringsAsFactors = FALSE  # Option to prevent factors
)


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
                   value = "2024-05-09"
                 ),
                 toDateController = dateInput(
                   ns("toDateController"),
                   "To Date"
                 ),
                 ProjectSelection = selectInput(
                   ns("ProjectSelection"),
                   "Select Project",
                   choices = c(unique(baseline_targets$Project_id),"URBANISE")
                 ),
                 Segrigation = selectInput(
                   ns("Segrigation"),
                   "Summarise by",
                   choices = segrationDropdown
                 ),
                 activityDropdown = selectInput(
                   ns("activityDropdown"),
                   "Activities",
                   choices = c("all",setNames(activities$Code,activities$Description) )
                 ),
                 groupDropdown = selectInput(
                   ns("groupDropdown"),
                   "Groups",
                   choices = c("all",setNames(groups$code,groups$description) )
                 ),
                 InfraDropdown = selectInput(
                   ns("InfraDropdown"),
                   "Infrastructure",
                   choices = c("all",setNames(infrastructure$code,infrastructure$description) )
                 ),
                 Activities_by_Member = '',
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
                 indiRespondants = uiOutput(ns("indiRespondants")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsGenderClassification = plotlyOutput(ns("indiRespondantsGenderClassification") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsAgeGroups =  plotlyOutput(ns("indiRespondantsAgeGroups") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsMaritalStatus = plotlyOutput(ns("indiRespondantsMaritalStatus") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsEducLevel = plotlyOutput(ns("indiRespondantsEducLevel") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsResStatus = plotlyOutput(ns("indiRespondantsResStatus") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsMigraDec = plotlyOutput(ns("indiRespondantsMigraDec") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantslvhd_zone = plotlyOutput(ns("indiRespondantslvhd_zone") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsMainIncomeSources = plotlyOutput(ns("indiRespondantsMainIncomeSources") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsIncomeSources = plotlyOutput(ns("indiRespondantsIncomeSources") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsAverageIncome = plotlyOutput(ns("indiRespondantsAverageIncome") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsVulCreteria =  plotlyOutput(ns("indiRespondantsVulCreteria") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsDisabilityType = plotlyOutput(ns("indiRespondantsDisabilityType") ,height = 400)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 respondatsInfoTable = DTOutput(ns("respondatsInfoTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 registrationActivitiesChart = plotlyOutput(ns("registrationActivitiesChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 valid_screened =uiOutput(ns("valid_screened")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 invalid_screened = uiOutput(ns("invalid_screened")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 not_screened = uiOutput(ns("not_screened")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 groupsRegestered = uiOutput(ns("groupsRegestered")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 groupRespondents = uiOutput(ns("groupRespondents")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 averageGrpupSize = uiOutput(ns("averageGrpupSize")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 registeredGroups = plotlyOutput(ns("registeredGroups")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 groupedScreening = plotlyOutput(ns("groupedScreening")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 
                 groupMembers = DTOutput(ns("groupMembers")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 infrTypes = plotlyOutput(ns("infrTypes")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 infraRegistered =uiOutput(ns("infraRegistered"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 wastinfrTypes= plotlyOutput(ns("wastinfrTypes")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 waterinfrTypes= plotlyOutput(ns("waterinfrTypes")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 ecoinfrTypes= plotlyOutput(ns("ecoinfrTypes")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 infrTable = DTOutput(ns("infrTable")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 totalRegistrations = uiOutput(ns("totalRegistrations"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 registrationType = selectInput(
                   ns("registrationCategory"),
                   "registration Category",
                   choices = c("all","Individual"="hh_level_or_individual_reg",
                               "Groups"="groups_reg",
                               "Infrastructure"="infrastructure_reg")
                 ),
                 deleteButton = uiOutput(ns("deleteButton"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 benefInfo = DTOutput(ns("benefInfo")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7)
                
                 
    )
  )
}

brcisRegistration <- function(input ,output , session,sharedValues){
  
 

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
      
      Authresult <- validate_member_ona_credentials(onauser,onapass,"788509")
      
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
      
      
      baseline_survey  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","788509")
      
      
      
      required_columns <- c("screening_bens_id","enum_name", "enum_gender", "enum_phone_number","Project_id", "Member_id",
                            "Region_id", "District_id", "Community_id", "consent",
                            "registration_category", "regis_activities", "participants_name",
                            "participants_sex", "tel_num", "ecosystem_initiatives", "recovery_assistance_farmers",
                            "recovery_assistance_market_actors",
                            "nfi_", "kitchen_garden", "hoh_name", "hoh_sex", "hoh_age", "tel_number",
                            "marital_status_hoh", "hoh_educ_level", "residential_status", "residential_date",
                            "decision_migrate", "lvhd_zone", "income_sources",
                            "income_sources_O", "income_sources_main", "income_amount", "hh_size",
                            "hh_members",
                            "vulnerability_criteria", "vulnerability_other_", "no_disabled_member",
                            "disability_Type", "clan_minority",
                            "groups", "specify_", "community_groups_", "date_created", "group_size",
                            "name_group_members", "ind_sex_group_members", "ind_age_group_members",
                            "ind_position_group_members", "tel_number_group_members", "disability_group_members",
                            "disability_Type_group_members", "minority_group_members", "representative_criteria_group_members","screening_bens_id_group_members",
                            "infrast_reg", "wast_management", "water_source_", "Ecosystem_infrastructure",
                            "infrast_status", "estimated_cost", "comm_contribution_cost",
                            "estimated_pple", "Picture", "infrast_geopoint", "contact_info",
                            "water1", "water2", "water3", "water3_1", "water4","index",
                            "consent.1","regis_activities.1","registration_category.1","tel_num.1","participants_sex.1", "participants_name.1"
                            
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
        
        postgresConnection <- dbConnect(RPostgres::Postgres(),
                                        dbname = "brcisShiny",
                                        host = "brcisproddb.postgres.database.azure.com", port = 5432,
                                        user = "brcisShiny", password = "brcisShiny@112233@",
                                        sslmode = 'require')
        
        
        columns_to_replace_na <- c()
        
        main_survey_analysis <-  baseline_survey %>% select(screening_bens_id,screening_bens_id_group_members,X_submission_time,start,end,X_id,index,
                                                            enum_name, enum_gender, enum_phone_number,Project_id, Member_id,
                                                            Region_id, District_id, Community_id, consent,
                                                            registration_category, regis_activities, participants_name,
                                                            participants_sex, tel_num, ecosystem_initiatives, recovery_assistance_farmers,
                                                            recovery_assistance_market_actors,
                                                            nfi_, kitchen_garden, hoh_name, hoh_sex, hoh_age, tel_number,
                                                            marital_status_hoh, hoh_educ_level, residential_status, residential_date,
                                                            decision_migrate, lvhd_zone, income_sources,
                                                            income_sources_O, income_sources_main, income_amount, hh_size,
                                                            hh_members,
                                                            vulnerability_criteria, vulnerability_other_, no_disabled_member,
                                                            disability_Type, clan_minority,
                                                            groups, specify_, community_groups_, date_created, group_size,
                                                            name_group_members, ind_sex_group_members, ind_age_group_members,
                                                            ind_position_group_members, tel_number_group_members, disability_group_members,
                                                            disability_Type_group_members, minority_group_members, representative_criteria_group_members,
                                                            infrast_reg, wast_management, water_source_, Ecosystem_infrastructure,
                                                            infrast_status, estimated_cost, comm_contribution_cost,
                                                            estimated_pple, Picture, infrast_geopoint, contact_info,
                                                            water1, water2, water3, water3_1, water4,consent.1,regis_activities.1,registration_category.1,tel_num.1,participants_sex.1, participants_name.1
                                                            
        ) %>%
          mutate(
            across(all_of(columns_to_replace_na), ~replace(., is.na(.), 0)),
            tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
            registration_category = ifelse(is.na(registration_category) , registration_category.1 , registration_category) ,
            screening_bens_id = ifelse(registration_category == 'groups_reg', screening_bens_id_group_members , screening_bens_id),
            screening_bens_id = trimws(screening_bens_id),
            index = ifelse(is.na(index),0 , index),
            bens_id = ifelse(registration_category == 'hh_level_or_individual_reg' | 
                               registration_category == 'infrastructure_reg' |
                               is.na(registration_category), X_id , paste(X_id,index,sep = "")),
            id = X_id,
            start_time = format(parse_date_time(start, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            end_time = format(parse_date_time(end, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            consent =  ifelse(is.na(consent) , consent.1 , consent),
            regis_activities = ifelse(is.na(regis_activities) , regis_activities.1 , regis_activities) ,
            
            tel_num = ifelse(is.na(tel_num) , tel_num.1 , tel_num) ,
            participants_sex = ifelse(is.na(participants_sex) , participants_sex.1 , participants_sex) , 
            participants_name =ifelse(is.na(participants_name) , participants_name.1 , participants_name),
            hoh_name = ifelse(registration_category == 'groups_reg' ,name_group_members ,hoh_name  ), 
            hoh_sex = ifelse(registration_category == 'groups_reg' ,ind_sex_group_members ,hoh_sex  ),  
            hoh_age =ifelse(registration_category == 'groups_reg' ,ind_age_group_members ,hoh_age  ),
            tel_number = ifelse(registration_category == 'groups_reg' ,tel_number_group_members ,tel_number  ), 
            position = ifelse(registration_category == 'groups_reg' ,ind_position_group_members ,NA  ), 
            clan_minority = ifelse(registration_category == 'groups_reg' ,minority_group_members ,clan_minority  ),
            hh_members = map_chr(hh_members, toJSON, auto_unbox = TRUE),
            Project_id = ifelse(is.na(Project_id),'BRCiS III',Project_id)
          )%>%select(-contains('.1'),-screening_bens_id_group_members)%>%
          filter(!(registration_category == "groups_reg" & index == 0))
        
        
        
        # tables <- dbListTables(postgresConnection)
        # if("brcisRegistration_b" %in% tables){
        #   # existing_ids <- dbGetQuery(postgresConnection, 'SELECT bens_id FROM "brcisRegistration" where access_status = \'A\'')
        #   # df_to_insert <- main_survey_analysis[!main_survey_analysis$bens_id %in% existing_ids$bens_id,]
        #   
        #   if (nrow(main_survey_analysis) > 0) {
        #     print(paste("Rows Writing To Database:",nrow(main_survey_analysis)))
        #     # dbWriteTable(postgresConnection, name = "brcisRegistration", value = df_to_insert, append = TRUE, row.names = FALSE)
        #     dbWriteTable(con, "temp_table", main_survey_analysis, temporary = TRUE, overwrite = TRUE)
        #     
        #     # Build the SQL query for upserting
        #     update_columns <- setdiff(names(main_survey_analysis), "bens_id")
        #     update_statement <- paste(
        #       sapply(update_columns, function(col) {
        #         paste0('"', col, '" = EXCLUDED."', col, '"')
        #       }),
        #       collapse = ", "
        #     )
        #     
        #     query <- glue::glue("
        #         INSERT INTO \"brcisRegistration_b\" 
        #         SELECT * FROM temp_table
        #         ON CONFLICT (\"bens_id\") DO UPDATE SET
        #         {update_statement}
        #       ")
        #     
        #     # Execute the query
        #     dbExecute(con, query)
        #     
        #     # Remove the temporary table
        #     dbExecute(con, "DROP TABLE temp_table")
        #   } else {
        #     print("No new records to insert.")
        #   }
        # }else{
        #   dbWriteTable(postgresConnection , "brcisRegistration_b" , main_survey_analysis)
        # }
        # 
        
        tables <- dbListTables(postgresConnection)
        if("brcisRegistration" %in% tables){
          existing_ids <- dbGetQuery(postgresConnection, 'SELECT bens_id FROM "brcisRegistration"')
          df_to_insert <- main_survey_analysis[!main_survey_analysis$bens_id %in% existing_ids$bens_id,]
          
          if (nrow(df_to_insert) > 0) {
            print(paste("Rows Writing To Database :",nrow(df_to_insert)))
            dbWriteTable(postgresConnection, name = "brcisRegistration", value = df_to_insert, append = TRUE, row.names = FALSE)
          } else {
            print("No new records to insert.")
          }
        }else{
          dbWriteTable(postgresConnection , "brcisRegistration" , main_survey_analysis)
        }
        
        if(Authresult$message != 'CMU'){
          main_survey_analysis <- dbGetQuery(postgresConnection, sprintf('SELECT * FROM get_registration_hh_with_screened_info WHERE "Member_id" = \'%s\' and "Project_id" = \'%s\'', Authresult$message,input$ProjectSelection))
        }else{
          main_survey_analysis <- dbGetQuery(postgresConnection, sprintf('SELECT * FROM get_registration_hh_with_screened_info  where "Project_id" = \'%s\'', input$ProjectSelection))
        }
        
        
        
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
    ns1 <- NS("brcisRegistration")
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
    
    if(!is.null((input$registrationCategory))){
      if((input$registrationCategory) != "all"){
        
        
        main_survey %<>% filter(
          registration_category == input$registrationCategory
        )
        
        
      }
      
    }
    
    if(!is.null((input$activityDropdown))){
      if((input$activityDropdown) != "all"){
        
        
        main_survey %<>% filter(
          grepl(escape_regex(input$activityDropdown), regis_activities )
        )
        
        
      }
      
    }
    
    
    if(!is.null((input$groupDropdown))){
      if((input$groupDropdown) != "all"){
        
        
        main_survey %<>% filter(
          groups == input$groupDropdown
        )
        
        
      }
      
    }
    
    if(!is.null((input$InfraDropdown))){
      if((input$InfraDropdown) != "all"){
        
        
        main_survey %<>% filter(
          infrast_reg == input$InfraDropdown
        )
        
        
      }
      
    }
    
    output$interviewProgressByMonth <- renderPlotly({
      data <- main_survey %>%
        group_by(bens_id)%>%
        reframe(tab_date = first(tab_date),count = n())%>%
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
          Individual =sum(registration_category == "hh_level_or_individual_reg", na.rm = TRUE),
          Groups = paste(n_distinct(ifelse(registration_category == "groups_reg", groups, NA_character_),na.rm = TRUE),'(N=',sum(registration_category == "groups_reg", na.rm = TRUE),')',sep = ""),
          Infrastructure =sum(registration_category == "infrastructure_reg", na.rm = TRUE),
          Other =sum(is.na(registration_category), na.rm = TRUE),
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
          Individual =sum(registration_category == "hh_level_or_individual_reg", na.rm = TRUE),
          Groups = paste(n_distinct(ifelse(registration_category == "groups_reg", groups, NA_character_),na.rm = TRUE),'(N=',sum(registration_category == "groups_reg", na.rm = TRUE),')',sep = ""),
          Infrastructure =sum(registration_category == "infrastructure_reg", na.rm = TRUE),
          Other =sum(is.na(registration_category), na.rm = TRUE),
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
          Individual =sum(registration_category == "hh_level_or_individual_reg", na.rm = TRUE),
          Groups = paste(n_distinct(ifelse(registration_category == "groups_reg", groups, NA_character_),na.rm = TRUE),'(N=',sum(registration_category == "groups_reg", na.rm = TRUE),')',sep = ""),
          Infrastructure =sum(registration_category == "infrastructure_reg", na.rm = TRUE),
          Other =sum(is.na(registration_category), na.rm = TRUE),
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
          Individual =sum(registration_category == "hh_level_or_individual_reg", na.rm = TRUE),
          Groups = sum(registration_category == "groups_reg", na.rm = TRUE),
          Infrastructure =sum(registration_category == "infrastructure_reg", na.rm = TRUE),
          Other =sum(is.na(registration_category), na.rm = TRUE),
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
    
    
  
    
    output$indiRespondants <- renderUI({
      respondantMaritaStatus <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg' & consent == "yes")
      
      nrow(respondantMaritaStatus)
    })
    
    output$valid_screened <- renderUI({
      all <- main_survey%>%filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg')  & consent == "yes")
      valid_screened <- main_survey%>%filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg') & consent == "yes" &
                                               screening_status == 'Screened')
      valid_screened = nrow(valid_screened)
      all <- nrow(all)
      
      paste(valid_screened , paste('(%',round((valid_screened/all)*100,2),')',sep = ""))
    })
    
    output$invalid_screened <- renderUI({
      all <- main_survey%>%filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg')  & consent == "yes")
      invalid_screened <- main_survey%>%filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg') & consent == "yes" &
                                               screening_status == 'Invalid Screening')
      invalid_screened = nrow(invalid_screened)
      all <- nrow(all)
      
      paste(invalid_screened , paste('(%',round((invalid_screened/all)*100,2),')',sep = ""))
    })
    
    output$not_screened <- renderUI({
      all <- main_survey%>%filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg')  & consent == "yes")
      not_screened <- main_survey%>%filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg') & consent == "yes" &
                                                 screening_status == 'Not Screened')
      
      not_screened = nrow(not_screened)
      all <- nrow(all)
      
      paste(not_screened , paste('(%',round((not_screened/all)*100,2),')',sep = ""))
    })
    
    output$indiRespondantsGenderClassification <- renderPlotly({
      
      indiRespondantsGenderClassification <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg' &consent == "yes")%>%
      group_by(hoh_sex)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(hoh_gender = hoh_sex)
      
      fig <- plot_ly(indiRespondantsGenderClassification, labels = ~hoh_gender, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
      
    })
    
    output$indiRespondantsAgeGroups <- renderPlotly({
      indiRespondantsAgeGroups <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        drop_na(hoh_age)%>%
        pull(hoh_age)
       
      fig <- plot_ly(x = indiRespondantsAgeGroups, type = "histogram",
                     marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")),
                     xbins = list(start = min(indiRespondantsAgeGroups), end = max(indiRespondantsAgeGroups), size = 5)) %>% # You can adjust the size for different bin widths
        layout(title = "Age Distribution",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Count"),
               bargap = 0.05)
      
      # Show the plot
      fig
    })
    
    output$indiRespondantsMaritalStatus <- renderPlotly({
      indiRespondantsMaritalStatus <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        group_by(marital_status_hoh)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Marital Status` = marital_status_hoh)
      
      fig <- plot_ly(indiRespondantsMaritalStatus, labels = ~`Marital Status`, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
    })
    
    output$registrationActivitiesChart <- renderPlotly({
      registrationActivitiesChart <- main_survey %>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        separate_rows(regis_activities, sep = " ")%>%
        group_by(regis_activities)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Registration Activities` = regis_activities)%>%
        arrange(count)
    
      registrationActivitiesChart$`Registration Activities` <- factor(registrationActivitiesChart$`Registration Activities`, levels = registrationActivitiesChart$`Registration Activities`)
      registrationActivitiesChart$percentage <- (registrationActivitiesChart$count / sum(registrationActivitiesChart$count,na.rm = TRUE))*100
      fig <- plot_ly(registrationActivitiesChart,
                     x = ~count, 
                     y = ~`Registration Activities`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" ,orientation ="h")
      
      fig
    })
    
    output$indiRespondantsEducLevel <- renderPlotly({
      indiRespondantsEducLevel <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        group_by(hoh_educ_level)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Educational Level` = hoh_educ_level)%>%
        arrange(desc(count))
      indiRespondantsEducLevel$`Educational Level` <- factor(indiRespondantsEducLevel$`Educational Level`, levels = indiRespondantsEducLevel$`Educational Level`)
      indiRespondantsEducLevel$percentage <- (indiRespondantsEducLevel$count / sum(indiRespondantsEducLevel$count))*100
      fig <- plot_ly(indiRespondantsEducLevel,
                     y = ~count, 
                     x = ~`Educational Level`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside")
      
      fig
    })
    
    output$indiRespondantsResStatus <- renderPlotly({
      
      
      indiRespondantsResStatus <- main_survey %>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        group_by(residential_status)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Residential Status` = residential_status)%>%
        mutate(
          `Residential Status` = case_when(
            `Residential Status` == 1 ~ "Permanent resident in this location",
            `Residential Status` == 2 ~ "reside and  migrate",
            `Residential Status` == 3 ~ "All household members migrate",
            `Residential Status` == 4 ~ "Internally displaced household",
            `Residential Status` == 5 ~ "Household of returnees were IDP before",
            `Residential Status` == 6 ~ "Household of returnees were REFUGEEs before",
            `Residential Status` == 7 ~ "Seasonal migrants",
          )
        )%>%
        arrange(count)
      indiRespondantsResStatus$`Residential Status` <- factor(indiRespondantsResStatus$`Residential Status`, levels = indiRespondantsResStatus$`Residential Status`)
      indiRespondantsResStatus$percentage <- (indiRespondantsResStatus$count / sum(indiRespondantsResStatus$count,na.rm = TRUE))*100
      fig <- plot_ly(indiRespondantsResStatus,
                     x = ~count, 
                     y = ~`Residential Status`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation = "h")
      
      fig
      
     
    })
    
    output$indiRespondantsMigraDec <- renderPlotly({
      indiRespondantsResStatus <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        group_by(decision_migrate)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Migration Decisions` = decision_migrate)%>%
        drop_na(`Migration Decisions`)%>%
        mutate(
          `Migration Decisions` = case_when(
            `Migration Decisions` == 1 ~ "Due to Flood or Drought",
            `Migration Decisions` == 2 ~ "Due to conflict",
            `Migration Decisions` == 3 ~ "For economic reasons/for work - long term",
            `Migration Decisions` == 96 ~ " Other reason (please specify)"
          )
        )
      
      fig <- plot_ly(indiRespondantsResStatus, labels = ~`Migration Decisions`, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
    })
    
    
    
    
   
    
    
    output$indiRespondantslvhd_zone <- renderPlotly({
      indiRespondantslvhd_zone <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        group_by(lvhd_zone)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`LiveliHood Zones` = lvhd_zone)%>%arrange(desc(count))
      
      indiRespondantslvhd_zone$`LiveliHood Zones` <- factor(indiRespondantslvhd_zone$`LiveliHood Zones`, levels = indiRespondantslvhd_zone$`LiveliHood Zones`)
      indiRespondantslvhd_zone$percentage <- (indiRespondantslvhd_zone$count / sum(indiRespondantslvhd_zone$count,na.rm = TRUE))*100
      fig <- plot_ly(indiRespondantslvhd_zone,
                     y = ~count, 
                     x = ~`LiveliHood Zones`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside")
      
      fig
    })
    
    output$indiRespondantsIncomeSources <- renderPlotly({
      indiRespondantsIncomeSources <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        separate_rows(income_sources, sep = " ")%>%
        group_by(income_sources)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Income Sources` = income_sources)%>%
        mutate(
          `Income Sources` = case_when(
            `Income Sources` == 1 ~ "Agriculture",
            `Income Sources` == 2 ~ "Livestock",
            `Income Sources` == 3 ~ "Casual Labour/ Cash for work",
            `Income Sources` == 4 ~ "Private/ small Business",
            `Income Sources` == 5 ~ "Formal Employment",
            `Income Sources` == 6 ~ "Cash Transfers from Aid Agencies",
            `Income Sources` == 7 ~ "Remittances",
            `Income Sources` == 9 ~ "Fishery",
            `Income Sources` == 10 ~ "Charcoal burning",
            `Income Sources` == 96 ~ "Other",
          )
        )%>%arrange(desc(count))
      
      
      indiRespondantsIncomeSources$`Income Sources` <- factor(indiRespondantsIncomeSources$`Income Sources`, levels = indiRespondantsIncomeSources$`Income Sources`)
      indiRespondantsIncomeSources$percentage <- (indiRespondantsIncomeSources$count / sum(indiRespondantsIncomeSources$count,na.rm = TRUE))*100
      fig <- plot_ly(indiRespondantsIncomeSources,
                     x = ~count, 
                     y = ~`Income Sources`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")
      
      fig
    })
    
    output$indiRespondantsMainIncomeSources <- renderPlotly({
      indiRespondantsMainIncomeSources <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        group_by(income_sources_main)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Main Income Sources` = income_sources_main)%>%
        mutate(
          `Main Income Sources` = case_when(
            `Main Income Sources` == 1 ~ "Agriculture",
            `Main Income Sources` == 2 ~ "Livestock",
            `Main Income Sources` == 3 ~ "Casual Labour/ Cash for work",
            `Main Income Sources` == 4 ~ "Private/ small Business",
            `Main Income Sources` == 5 ~ "Formal Employment",
            `Main Income Sources` == 6 ~ "Cash Transfers from Aid Agencies",
            `Main Income Sources` == 7 ~ "Remittances",
            `Main Income Sources` == 9 ~ "Fishery",
            `Main Income Sources` == 10 ~ "Charcoal burning",
            `Main Income Sources` == 96 ~ "Other",
            .default = `Main Income Sources`
          )
        )
      
      
      fig <- plot_ly(indiRespondantsMainIncomeSources, labels = ~`Main Income Sources`, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$indiRespondantsAverageIncome <- renderPlotly({
      indiRespondantsAverageIncome <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        group_by(income_sources_main)%>%
        reframe(
          total = sum(as.numeric(income_amount) , na.rm = TRUE),
          count = n()
        )%>%
        dplyr::rename(`Main Income Sources` = income_sources_main)%>%
        mutate(
          `Main Income Sources` = case_when(
            `Main Income Sources` == 1 ~ "Agriculture",
            `Main Income Sources` == 2 ~ "Livestock",
            `Main Income Sources` == 3 ~ "Casual Labour/ Cash for work",
            `Main Income Sources` == 4 ~ "Private/ small Business",
            `Main Income Sources` == 5 ~ "Formal Employment",
            `Main Income Sources` == 6 ~ "Cash Transfers from Aid Agencies",
            `Main Income Sources` == 7 ~ "Remittances",
            `Main Income Sources` == 9 ~ "Fishery",
            `Main Income Sources` == 10 ~ "Charcoal burning",
            `Main Income Sources` == 96 ~ "Other",
            .default = `Main Income Sources`
          )
        )
      
      fig <- plot_ly(data = indiRespondantsAverageIncome, x = ~total, y = ~count, color = ~`Main Income Sources`, 
                     colors = "Set1" ,size = ~total)
      
      fig
    })
    
    output$indiRespondantsVulCreteria <- renderPlotly({
      indiRespondantsVulCreteria <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        separate_rows(vulnerability_criteria, sep = " ")%>%
        group_by(vulnerability_criteria)%>%
        reframe(
          count = n()
        )%>%
        mutate(
          vulnerability_criteria = case_when(
            vulnerability_criteria == 1 ~ "Food insecurity",
            vulnerability_criteria == 2 ~ "Poor or financial instability",
            vulnerability_criteria == 3 ~ "Negative coping strategies",
            vulnerability_criteria == 4 ~ "Female headed household",
            vulnerability_criteria == 5 ~ "HHs with pregnant and lactating women",
            vulnerability_criteria == 6 ~ "HHs with more than 2 children aged under 5",
            vulnerability_criteria == 7 ~ "HH head or member of the household is chronically ill",
            vulnerability_criteria == 7 ~ "Minority or marginalized HH.",
            vulnerability_criteria == 9 ~ "HH or a member of HH with disability.",
            vulnerability_criteria == 10 ~ "Head of HH or a member of the household is older person (above 60)",
            vulnerability_criteria == 11 ~ "Internally Displaced Persons (IDP",
            vulnerability_criteria == 12 ~ "Returnee",
            vulnerability_criteria == 13 ~ "Refugee",
            vulnerability_criteria == 96 ~ "Other",
            .default = vulnerability_criteria
          )
        )
      
      fig <- plot_ly(indiRespondantsVulCreteria,x = ~count, y = ~vulnerability_criteria, type = 'bar', orientation = 'h')
      
      fig
      
    })
    
    output$indiRespondantsDisabilityType <- renderPlotly({
      indiRespondantsDisabilityType <- main_survey%>%
        filter(registration_category == 'hh_level_or_individual_reg'  & consent == "yes")%>%
        separate_rows(disability_Type, sep = " ")%>%
        group_by(disability_Type)%>%
        reframe(
          count = n()
        )%>%
        drop_na(disability_Type)
      
      fig <- plot_ly(indiRespondantsDisabilityType,y = ~count, x = ~disability_Type, type = 'bar')
      
      fig
    })
    
    output$respondatsInfoTable <- renderDT(server = FALSE ,{
      
      respondatsInfoTable <- main_survey%>%
        filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg')   & consent == "yes")%>%
        mutate(
          group_name= groups
        )%>%
        select(screening_status,status_reason,name_match_score, phone_match_score,tab_date,screening_bens_id,bens_id,registration_category,group_name,hoh_name, hoh_sex,hoh_age,tel_number,enum_name, enum_phone_number, Member_id,
               Region_id, District_id, Community_id,regis_activities, marital_status_hoh,
               hoh_educ_level, residential_status,  lvhd_zone,income_sources,
               income_sources_main,income_amount, hh_size ,vulnerability_criteria,screened_hh_fullname,
               screened_hh_tel_number,screened_hh_member_id,screened_hh_region_id,screened_hh_district_id,screened_hh_community_id,
               screened_hh_ver_categoryd ,id
        )%>%
        mutate(
          residential_status = case_when(
            residential_status == 1 ~ "Permanent resident in this location",
            residential_status == 2 ~ "reside and  migrate",
            residential_status == 3 ~ "All household members migrate",
            residential_status == 4 ~ "Internally displaced household",
            residential_status == 5 ~ "Household of returnees were IDP before",
            residential_status == 6 ~ "Household of returnees were REFUGEEs before",
            residential_status == 7 ~ "Seasonal migrants",
            .default = residential_status
          ),
          income_sources = case_when(
            income_sources == 1 ~ "Agriculture",
            income_sources == 2 ~ "Livestock",
            income_sources == 3 ~ "Casual Labour/ Cash for work",
            income_sources == 4 ~ "Private/ small Business",
            income_sources == 5 ~ "Formal Employment",
            income_sources == 6 ~ "Cash Transfers from Aid Agencies",
            income_sources == 7 ~ "Remittances",
            income_sources == 9 ~ "Fishery",
            income_sources == 10 ~ "Charcoal burning",
            income_sources == 96 ~ "Other",
          ),
          income_sources_main = case_when(
            income_sources_main == 1 ~ "Agriculture",
            income_sources_main == 2 ~ "Livestock",
            income_sources_main == 3 ~ "Casual Labour/ Cash for work",
            income_sources_main == 4 ~ "Private/ small Business",
            income_sources_main == 5 ~ "Formal Employment",
            income_sources_main == 6 ~ "Cash Transfers from Aid Agencies",
            income_sources_main == 7 ~ "Remittances",
            income_sources_main == 9 ~ "Fishery",
            income_sources_main == 10 ~ "Charcoal burning",
            income_sources_main == 96 ~ "Other",
            .default = income_sources_main
          ),
          vulnerability_criteria = case_when(
            vulnerability_criteria == 1 ~ "Food insecurity",
            vulnerability_criteria == 2 ~ "Poor or financial instability",
            vulnerability_criteria == 3 ~ "Negative coping strategies",
            vulnerability_criteria == 4 ~ "Female headed household",
            vulnerability_criteria == 5 ~ "HHs with pregnant and lactating women",
            vulnerability_criteria == 6 ~ "HHs with more than 2 children aged under 5",
            vulnerability_criteria == 7 ~ "HH head or member of the household is chronically ill",
            vulnerability_criteria == 7 ~ "Minority or marginalized HH.",
            vulnerability_criteria == 9 ~ "HH or a member of HH with disability.",
            vulnerability_criteria == 10 ~ "Head of HH or a member of the household is older person (above 60)",
            vulnerability_criteria == 11 ~ "Internally Displaced Persons (IDP",
            vulnerability_criteria == 12 ~ "Returnee",
            vulnerability_criteria == 13 ~ "Refugee",
            vulnerability_criteria == 96 ~ "Other",
            .default = vulnerability_criteria
          )
          
        )
    
      
      Label <- 'Respondant Info'
      
      if(global_vars$logged_in_member == 'CMU'){
        # 
        respondatsInfoTable$actions <- '';
        # 
        respondatsInfoTable %<>% select(actions,screening_status,status_reason,name_match_score, phone_match_score,tab_date,screening_bens_id,bens_id,registration_category,group_name,hoh_name, hoh_sex,hoh_age,tel_number,enum_name, enum_phone_number, Member_id,
                                        Region_id, District_id, Community_id,regis_activities, marital_status_hoh,
                                        hoh_educ_level, residential_status,  lvhd_zone,income_sources,
                                        income_sources_main,income_amount, hh_size ,vulnerability_criteria,screened_hh_fullname,
                                        screened_hh_tel_number,screened_hh_member_id,screened_hh_region_id,screened_hh_district_id,screened_hh_community_id,
                                        screened_hh_ver_categoryd ,id
        )
        
        datatable(respondatsInfoTable,rownames = FALSE,
                  extensions = c('Buttons'),
                  selection = "none",
                  options = list(
                    scrollX = TRUE, 
                    pageLength = 30,
                    dom = 'lBfrtip',
                    columnDefs = list(list(
                      title = 'Select',
                      targets = 0, render = JS(
                        "function(data, type, full, meta) {",
                        "return '<input type=\"checkbox\" class=\"row-checkbox\" value=\"' + full[7] + '\">';",
                        "}")
                    )),
                    preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                    drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }'),
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
                  ),escape = FALSE)%>%
          formatStyle(
            'screening_status',
            backgroundColor = styleEqual(c('Screened', 'Invalid Screening', 'Not Screened'), 
                                         c("#00B050","#dc3545", "#FFC000"))
          )%>%
          formatStyle(
            'name_match_score',
            backgroundColor = styleInterval(c(30, 50), c("#dc3545", "#FFC000", "#00B050"))
          )%>%
          formatStyle(
            'phone_match_score',
            backgroundColor = styleInterval(c(30, 50), c("#dc3545", "#FFC000", "#00B050"))
          )
      }else{
        datatable(respondatsInfoTable,rownames = FALSE,
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
                  ),escape = FALSE)%>%
          formatStyle(
            'screening_status',
            backgroundColor = styleEqual(c('Screened', 'Invalid Screening', 'Not Screened'), 
                                         c("#00B050","#dc3545", "#FFC000"))
          )%>%
          formatStyle(
            'name_match_score',
            backgroundColor = styleInterval(c(30, 50), c("#dc3545", "#FFC000", "#00B050"))
          )%>%
          formatStyle(
            'phone_match_score',
            backgroundColor = styleInterval(c(30, 50), c("#dc3545", "#FFC000", "#00B050"))
          )
      }
      
      
      
      

        
    })
    
    output$deleteButton <- renderUI({
     
      if(global_vars$logged_in_member == 'CMU'){
        actionButton("brcisRegistration-getChecks", "Delete Selected Data",class = "btn btn-sm btn-danger")
      }else{
        HTML("")
      }
    })
    
    output$groupRespondents <- renderUI({
      groupRespondents <- main_survey%>%
        filter(registration_category == 'groups_reg'  & consent == "yes")
        
        nrow(groupRespondents)
    })
    
    output$groupsRegestered <- renderUI({
      groupsRegestered <- main_survey%>%
        filter(registration_category == 'groups_reg'  & consent == "yes")
        
      
      length(unique(groupsRegestered$id))
    })
    
    output$averageGrpupSize <- renderUI({
      averageGrpupSize <- main_survey%>%
        filter(registration_category == 'groups_reg'  & consent == "yes")%>%drop_na(group_size)
      
      
     round( mean(averageGrpupSize$group_size),2)
    })
    
    output$registeredGroups <- renderPlotly({
      registeredGroups <- main_survey%>%
        filter(registration_category == 'groups_reg'  & consent == "yes")%>%
        drop_na(groups)%>%
        group_by(groups)%>%
        reframe(
          count = n()
        )%>%
        arrange(count)
      
      registeredGroups$groups <- factor(registeredGroups$groups, levels = registeredGroups$groups)
      registeredGroups$percentage <- (registeredGroups$count / sum(registeredGroups$count,na.rm = TRUE))*100
      fig <- plot_ly(registeredGroups,
                     x = ~count, 
                     y = ~groups, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation = "h")
      
      fig
    })
    

    
    output$groupMembers <- renderDT({
      
      filteredGroup <- main_survey
      
      
      
      groupMembers <- filteredGroup%>%
      filter(registration_category == 'groups_reg'  & consent == "yes")%>%
        select(screening_bens_id,bens_id,hoh_name, hoh_sex,hoh_age,tel_number,enum_name, enum_phone_number, Member_id,
               Region_id, District_id, Community_id , groups
        )
      
      Label <- 'Group Members Info'
      datatable(groupMembers,rownames = FALSE,
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
    
    output$groupedScreening <- renderPlotly({
      groupedScreening <- main_survey%>%
        filter((registration_category == 'hh_level_or_individual_reg' | registration_category=='groups_reg')   & consent == "yes")%>%
        group_by(!!sym(input$Segrigation))%>%
        reframe(
          screened = sum(screening_status == 'Screened'),
          Invalid_screened =  sum(screening_status == 'Invalid Screening'),
          Not_Screened_at = sum(screening_status == 'Not Screened')
        )
      
      fig <- plot_ly(groupedScreening, x = as.formula(paste0("~`", input$Segrigation, "`")), y = ~screened, 
                     type = 'bar', name = 'Screened',marker = list(color = '#008BA8'),
                     text = ~screened,
                     textposition = "outside")
      fig <- fig %>% add_trace(y = ~Invalid_screened, name = 'Invalid Screening',marker = list(color = '#ED7667'),
                               text = ~Invalid_screened,
                               textposition = "outside")
      fig <- fig %>% add_trace(y = ~Not_Screened_at, name = 'Not Screened at all',marker = list(color = '#F99D1E'),
                               text = ~Not_Screened_at,
                               textposition = "outside")
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
      
      fig
        
    })
    
    output$infraRegistered <- renderUI({
      groupsRegestered <- main_survey%>%
        filter(registration_category == 'infrastructure_reg'  & consent == "yes")
      
      
      nrow(groupsRegestered)
    })
    
    output$infrTypes <- renderPlotly({
      infrTypes <- main_survey%>%
        filter(registration_category == 'infrastructure_reg'  & consent == "yes")%>%
        drop_na(infrast_reg)%>%
        group_by(infrast_reg)%>%
        reframe(
          
          count = n()
        )%>%
        arrange(count)
      
      infrTypes$infrast_reg <- factor(infrTypes$infrast_reg, levels = infrTypes$infrast_reg)
      infrTypes$percentage <- (infrTypes$count / sum(infrTypes$count,na.rm = TRUE))*100
      fig <- plot_ly(infrTypes,
                     x = ~count, 
                     y = ~infrast_reg, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation = "h")
      
      fig
    })
    
    output$wastinfrTypes <- renderPlotly({
      infrTypes <- main_survey%>%
        filter(registration_category == 'infrastructure_reg'  & consent == "yes")%>%
        drop_na(wast_management)%>%
        group_by(wast_management)%>%
        reframe(
          newly_constructed = sum(infrast_status == 'newly_constructed'),
          rehabilitated = sum(infrast_status == 'rehabilitated'),
          count = n()
        )%>%
        arrange(count)
      
      infrTypes$wast_management <- factor(infrTypes$wast_management, levels = infrTypes$wast_management)
      
      fig <- plot_ly(infrTypes, y = ~wast_management, x = ~newly_constructed, type = 'bar', 
                     name = 'Newly Constructed',orientation ="h",
                     text = ~paste(round(newly_constructed,1)),
                     textposition = "inside")
      fig <- fig %>% add_trace(x = ~rehabilitated, name = 'Rehabilitated',
                               text = ~paste(round(rehabilitated,1)),
                               textposition = "outside")
      fig <- fig %>% layout(xaxis = list(title = 'Count'), barmode = 'stack')
      
      fig
    })
    
    output$waterinfrTypes <- renderPlotly({
      infrTypes <- main_survey%>%
        filter(registration_category == 'infrastructure_reg'  & consent == "yes")%>%
        drop_na(water_source_)%>%
        group_by(water_source_)%>%
        reframe(
          newly_constructed = sum(infrast_status == 'newly_constructed'),
          rehabilitated = sum(infrast_status == 'rehabilitated'),
          count = n()
        )%>%
        arrange(count)
      
      infrTypes$water_source_ <- factor(infrTypes$water_source_, levels = infrTypes$water_source_)
      fig <- plot_ly(infrTypes, y = ~water_source_, x = ~newly_constructed, type = 'bar', 
                     name = 'Newly Constructed',orientation ="h",
                     text = ~paste(round(newly_constructed,1)),
                     textposition = "inside")
      fig <- fig %>% add_trace(x = ~rehabilitated, name = 'Rehabilitated',
                               text = ~paste(round(rehabilitated,1)),
                               textposition = "outside")
      fig <- fig %>% layout(xaxis = list(title = 'Count'), barmode = 'stack')
      
      fig
    })
    
    output$ecoinfrTypes <- renderPlotly({
      infrTypes <- main_survey%>%
        filter(registration_category == 'infrastructure_reg'  & consent == "yes")%>%
        drop_na(Ecosystem_infrastructure)%>%
        group_by(Ecosystem_infrastructure)%>%
        reframe(
          newly_constructed = sum(infrast_status == 'newly_constructed'),
          rehabilitated = sum(infrast_status == 'rehabilitated'),
          count = n()
        )%>%
        arrange(count)
      
      infrTypes$Ecosystem_infrastructure <- factor(infrTypes$Ecosystem_infrastructure, levels = infrTypes$Ecosystem_infrastructure)
      fig <- plot_ly(infrTypes, y = ~Ecosystem_infrastructure, x = ~newly_constructed, type = 'bar', 
                     name = 'Newly Constructed',orientation ="h",
                     text = ~paste(round(newly_constructed,1)),
                     textposition = "inside")
      fig <- fig %>% add_trace(x = ~rehabilitated, name = 'Rehabilitated',
                               text = ~paste(round(rehabilitated,1)),
                               textposition = "outside")
      fig <- fig %>% layout(xaxis = list(title = 'Count'), barmode = 'stack')
      
      fig
    })
    
    output$infrTable <- renderDT(server = FALSE,{
      infrTable <- main_survey%>%
        filter(registration_category == 'infrastructure_reg'  & consent == "yes")%>%
        select(screening_bens_id, id,Member_id,
               Region_id, District_id, Community_id , infrast_reg, wast_management, water_source_,Ecosystem_infrastructure,
               infrast_status,   estimated_cost, comm_contribution_cost,   estimated_pple)
               
        # )%>%
        # mutate(
        #   Picture = paste("https://api.ona.io/api/v1/files/",id,"?filename=brcis/attachments/788509_BRCiS_III_Activities_Registration_Tool/",Picture,sep = ""),
        #   Picture = sprintf('<a href="%s" target="_blank">%s</a>', Picture, "Picture")
        # )
      
      Label <- 'Infrastructure Info'
      datatable(infrTable,rownames = FALSE,
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
    
    output$totalRegistrations <- renderUI({
      nrow(main_survey)
    })
    
    output$benefInfo <- renderDT(server = FALSE ,{
      
      benefInfo <- main_survey%>%
        filter(consent == "yes")%>%
        mutate(
          group_name= groups
        )%>%
        select(screening_status,tab_date,screening_bens_id,bens_id,registration_category,group_name,hoh_name, hoh_sex,hoh_age,tel_number,enum_name, enum_phone_number, Member_id,
               Region_id, District_id, Community_id,regis_activities, marital_status_hoh,
               hoh_educ_level, residential_status,  lvhd_zone,income_sources,
               income_sources_main,income_amount, hh_size ,vulnerability_criteria
        )%>%
        mutate(
          residential_status = case_when(
            residential_status == 1 ~ "Permanent resident in this location",
            residential_status == 2 ~ "reside and  migrate",
            residential_status == 3 ~ "All household members migrate",
            residential_status == 4 ~ "Internally displaced household",
            residential_status == 5 ~ "Household of returnees were IDP before",
            residential_status == 6 ~ "Household of returnees were REFUGEEs before",
            residential_status == 7 ~ "Seasonal migrants",
            .default = residential_status
          ),
          income_sources = case_when(
            income_sources == 1 ~ "Agriculture",
            income_sources == 2 ~ "Livestock",
            income_sources == 3 ~ "Casual Labour/ Cash for work",
            income_sources == 4 ~ "Private/ small Business",
            income_sources == 5 ~ "Formal Employment",
            income_sources == 6 ~ "Cash Transfers from Aid Agencies",
            income_sources == 7 ~ "Remittances",
            income_sources == 9 ~ "Fishery",
            income_sources == 10 ~ "Charcoal burning",
            income_sources == 96 ~ "Other",
          ),
          income_sources_main = case_when(
            income_sources_main == 1 ~ "Agriculture",
            income_sources_main == 2 ~ "Livestock",
            income_sources_main == 3 ~ "Casual Labour/ Cash for work",
            income_sources_main == 4 ~ "Private/ small Business",
            income_sources_main == 5 ~ "Formal Employment",
            income_sources_main == 6 ~ "Cash Transfers from Aid Agencies",
            income_sources_main == 7 ~ "Remittances",
            income_sources_main == 9 ~ "Fishery",
            income_sources_main == 10 ~ "Charcoal burning",
            income_sources_main == 96 ~ "Other",
            .default = income_sources_main
          ),
          vulnerability_criteria = case_when(
            vulnerability_criteria == 1 ~ "Food insecurity",
            vulnerability_criteria == 2 ~ "Poor or financial instability",
            vulnerability_criteria == 3 ~ "Negative coping strategies",
            vulnerability_criteria == 4 ~ "Female headed household",
            vulnerability_criteria == 5 ~ "HHs with pregnant and lactating women",
            vulnerability_criteria == 6 ~ "HHs with more than 2 children aged under 5",
            vulnerability_criteria == 7 ~ "HH head or member of the household is chronically ill",
            vulnerability_criteria == 7 ~ "Minority or marginalized HH.",
            vulnerability_criteria == 9 ~ "HH or a member of HH with disability.",
            vulnerability_criteria == 10 ~ "Head of HH or a member of the household is older person (above 60)",
            vulnerability_criteria == 11 ~ "Internally Displaced Persons (IDP",
            vulnerability_criteria == 12 ~ "Returnee",
            vulnerability_criteria == 13 ~ "Refugee",
            vulnerability_criteria == 96 ~ "Other",
            .default = vulnerability_criteria
          )
          
        )
      
      
      Label <- 'Respondant Info'
      
      datatable(benefInfo,rownames = FALSE,
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
                ),escape = FALSE)%>%
        formatStyle(
          'screening_status',
          backgroundColor = styleEqual(c('Screened', 'Invalid Screening', 'Not Screened'), 
                                       c("#00B050","#dc3545", "#FFC000"))
        )
      
      
      
      
      
      
    })
    
    
    
    # Server-side action based on button click
   
  })
  
  observeEvent(input$getChecks, {
    
    runjs('
      var checked = [];
      $(".row-checkbox:checked").each(function() {
        checked.push(this.value);
      });
      Shiny.setInputValue("brcisRegistration-checked_rows", checked);
    ')
    
    showModal(modalDialog(
      title = "Button Clicked",
      tags$div(
        tagList(
          uiOutput("brcisRegistration-ms"),
          textInput(
            "brcisRegistration-us",
            "enter User name"
          ),
          passwordInput(
            "brcisRegistration-ps",
            "enter password"
          ),
          actionButton("brcisRegistration-sb","submit",class="btn btn-primary btn-block")
        )
      )
    ))
    
    
  })
  
  observeEvent(input$sb ,{
    us <- input$us
    ps <- input$ps
    
    if(us == "cmuAdmin" & ps =="cmuAdmin"){
      
    
      
      items_to_delete_str <- paste(sprintf("'%s'", input$checked_rows), collapse = ",")
      query <- sprintf('DELETE FROM public."brcisRegistration" WHERE bens_id IN (%s)', items_to_delete_str)
      
      if(length(input$checked_rows) > 0){
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
  
  # runjs('
  #       $(document).on("click", ".hhs_view_button", function() {
  #         var id = $(this).attr("id");
  #         Shiny.setInputValue("brcisRegistration-hh_view_button", id);
  #       });
  #     ')
  # observeEvent(input$hh_view_button, {
  #   btnId <- input$hh_view_button
  #   btnId <- sub(".*-", "", btnId)
  #   # Extract the row number or perform actions based on btnId
  #   # For example, just printing the ID of the clicked button
  #   print(btnId)
  #   showModal(modalDialog(size = "l",
  #           title = "Screened Household info",
  #           DTOutput("brcisRegistration-screened_hh_table")
  #         ))
  #   output$screened_hh_table<- renderDT({
  #     reg_survey<- isolate(global_vars$global_baseline_survey)
  #     reg_survey %<>% filter(bens_id == btnId)%>%pull(screened_hh_info)
  #     print(reg_survey)
  #     reg_survey = fromJSON(reg_survey) %>% as.data.frame
  #     
  #     datatable(reg_survey)
  #     
  #   })
  # })
  
}

