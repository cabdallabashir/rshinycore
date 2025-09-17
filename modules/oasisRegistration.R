



get_data <- function(username, password ,formID){
  
  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/",formID,sep = ""), authenticate(username,password))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()

    
    
    normalize_nested_types <- function(df) {
      if(!is.null(df)){
        df %>%
          mutate(across(everything(), as.character))
      }else{
        df
      }
     
    }
    
    
    if("groups_registration/group_members" %in% names(formData)){
      # Normalize data types within nested lists
      formData$`groups_registration/group_members` <- formData$`groups_registration/group_members` %>%
        map(normalize_nested_types)
      
    
      
     
      
      
      for (i in seq_along(formData$`groups_registration/group_members`)) {
        # Access each member in the list
        group_members <- formData$`groups_registration/group_members`[[i]]
        
        
        # Check if member is a list and has the required fields
        if (is.list(group_members)) {
          
          group_members$`groups_registration/group_members/index` <- seq.int(nrow(group_members))
          group_members <- group_members[!is.na(group_members$`groups_registration/group_members/name_grp`), ]
          
          # Put the modified member back into the dataframe
          formData$`groups_registration/group_members`[[i]] <- group_members
        }
      }
      
      
      formData %<>%
        unnest("groups_registration/group_members",keep_empty = TRUE)%>%
        as.data.frame()%>%purrr::discard(is.list)
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



oasis_targets <- readxl::read_excel("docs/oasis_baseline_comunity_targets.xlsx")
segrationDropdown <- names(oasis_targets[c("member",	"region",	"district",	"community")])

oasis_activity_groups = readxl::read_excel("docs/oasis_activity_groups.xlsx")%>%
  mutate(
    group_type = case_when(
      grepl("Cooperative", code) ~ "Cooperative",
      grepl("VSLA", code) ~ "VSLA",
      grepl("EMCs", code) ~ "EMCs",
      .default = "Individual"),
  )
oasis_activity = readxl::read_excel("docs/oasis_activity_groups.xlsx",sheet = "Sheet2")%>%
  mutate(
    group_type = case_when(
      grepl("Cooperative", activity_group) ~ "Cooperative",
      grepl("VSLA", activity_group) ~ "VSLA",
      grepl("EMCs", activity_group) ~ "EMCs",
      .default = "Individual"),
  )



oasisRegistrationUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/oasisRegistration.html",
                 
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
                   value = "2024-04-01"
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
                 
                 regisController = selectInput(
                   ns("regisController"),
                   "Reg. Category",
                   choices = c("all",
                     "Groups Registration" = "Groups_cooperatives_VSLAs_commitees",
                     "Individual"="Individuals_households_service_providers__CAHWS"
                     
                   )
                 ),
                 
                 activityFroupTypeController = selectInput(
                   ns("activityFroupTypeController"),
                   "Group Type",
                   choices = c("all",
                               "Cooperative",
                               "VSLA",
                               "EMCs","Individual"
                               
                   )
                 ),
                 
                 activityGroupController =  uiOutput(ns("activityGroupController")),
                 actvitiesController  =  uiOutput(ns("actvitiesController")),
                 warningToLoadDATA = uiOutput(ns("warningToLoadDATA")),
                 Segrigation_element = uiOutput(ns("Segrigation_element")),
                 
                 totalRegistrations = uiOutput(ns("totalRegistrations"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 GroupRegistrations = uiOutput(ns("GroupRegistrations"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 trainingsHeld = uiOutput(ns("trainingsHeld"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 interviewProgressByMonth  = plotlyOutput(ns("interviewProgressByMonth"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 verification_category_chart =  plotlyOutput(ns("verification_category_chart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 consentChart = plotlyOutput(ns("consentChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 activityGroupsChart = plotlyOutput(ns("activityGroupsChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 activityGroupsTable =DTOutput(ns("activityGroupsTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 activityGroupsBaseTable=DTOutput(ns("activityGroupsBaseTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 activityChart = plotlyOutput(ns("activityChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 activityTable =DTOutput(ns("activityTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 performanceSummaryController = selectInput(
                   ns("performanceSummaryController"),
                   "",
                   choices = segrationDropdown,
                   width =  "100%"
                 ),
                 membersAnalysisChart = plotlyOutput(ns("membersAnalysisChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 membersAnalysisTable =DTOutput(ns("membersAnalysisTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 membersAnalysisBarChart = plotlyOutput(ns("membersAnalysisBarChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 membersAnalysisChartbyTraining = plotlyOutput(ns("membersAnalysisChartbyTraining"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                  groupTable = DTOutput(ns("groupTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 educationBackground = plotlyOutput(ns("educationBackground"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 genderClassification = plotlyOutput(ns("genderClassification"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 benMaritalStatus = plotlyOutput(ns("benMaritalStatus"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsAgeGroups = plotlyOutput(ns("indiRespondantsAgeGroups"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantslvhd_zone = plotlyOutput(ns("indiRespondantslvhd_zone"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 indiRespondantsResStatus = plotlyOutput(ns("indiRespondantsResStatus"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 ben_shocks =  plotlyOutput(ns("ben_shocks"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 main_ben_shocks =  plotlyOutput(ns("main_ben_shocks"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 ben_drought_impct_hhs_income =  plotlyOutput(ns("ben_drought_impct_hhs_income"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 ben_drought_impct_hhs_well_being =  plotlyOutput(ns("ben_drought_impct_hhs_well_being"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 ben_drought_recov =  plotlyOutput(ns("ben_drought_recov"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 Disability_Type_mental=  plotlyOutput(ns("Disability_Type_mental"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 Disability_Type_physical=  plotlyOutput(ns("Disability_Type_physical"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 income_sources_main=  plotlyOutput(ns("income_sources_main"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 income_sources=  plotlyOutput(ns("income_sources"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 type_of_work =  plotlyOutput(ns("type_of_work"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 averageIncome =  uiOutput(ns("averageIncome"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 AverageContribution =  uiOutput(ns("AverageContribution"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 otherContributeContent = uiOutput(ns("otherContributeContent"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 otherIncomContent =  uiOutput(ns("otherIncomContent"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 Otherhhsavings=  uiOutput(ns("Otherhhsavings"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 hhsavings=  uiOutput(ns("hhsavings"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 Otherhhdebt =  uiOutput(ns("Otherhhdebt"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 hhdebt =  uiOutput(ns("hhdebt"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 groupedScreening = plotlyOutput(ns("groupedScreening")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 valid_screened =uiOutput(ns("valid_screened")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 invalid_screened = uiOutput(ns("invalid_screened")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 not_screened = uiOutput(ns("not_screened")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 respondatsInfoTable = DTOutput(ns("respondatsInfoTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 hh_debts = plotlyOutput(ns("hh_debts")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 totalNumberInHH = uiOutput(ns("totalNumberInHH")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 under5 =uiOutput(ns("under5")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 above60 = uiOutput(ns("above60")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 pregnantLactating = uiOutput(ns("pregnantLactating")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 totRespondants = uiOutput(ns("totRespondants")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 trainingInfo = DTOutput(ns("trainingInfo")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 beneAllData = DTOutput(ns("beneAllData")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7)
                 
                 
    )
  )
}

oasisRegistration <- function(input ,output , session,sharedValues){
  
 
  
  
  global_vars <- reactiveValues(
    global_baseline_survey = NULL,
    main_survey = NULL
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
      
      Authresult <- validate_member_ona_credentials(onauser,onapass,"799049")
      
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
      
      
      baseline_survey  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","799049")
      
      
      # 
      # baseline_survey <- baseline_survey %>%
      #   select(consent,regis_activities,registration_category,tel_num,participants_sex, participants_name,contains(".1"))
      # 
      # nrow(baseline_survey)
      
      # baseline_survey%>%
      #   group_by(consent)%>%
      #   reframe(
      #     count = n()
      #   )
      # 
      # max(baseline_survey$X_submission_time)
      
      required_columns <- c("start","end","X_submission_time",
                            "enum_name", "enum_gender", "enum_phone_number", "member", "region", "district",
                            "community", "registration_category", "activity_groups", "regis_activities",
                            "screening_bens_id", "screening_hoh_name", "screening_hoh_contact", "consent",
                            "hoh_educ_level_cfw", "HHs_experienced_shocks_1_cfw", "main_shock_cfw",
                            "drought_impct_hhs_income_cfw", "drought_impct_hhs_well_being_cfw", "drought_recov_cfw",
                            "name_indv", "ind_sex_indv", "ind_age_indv", "tel_number_indv",
                            "marital_status_hoh_indv", "hh_individual", "hh_individual_sex", "hoh_educ_level_indv",
                            "hhsize_other_indv", "under_5_indv", "elderly_indv", "pregnant_lactating",
                            "contribute_income", "pwd_physical_indv", "Disability_Type_physical_indv",
                            "Disability_Type_physical_other_indv", "Disability_Type_physical_indv_no",
                            "pwd_mental_indv", "Disability_Type_mental_indv", "Disability_Type_mental_other_indv",
                            "Disability_Type_mental_indv_no", "residential_status_indv", "date_of_displacement_indv",
                            "IDP_origin_indv", "lvhd_zone_indv", "type_work_indv", "income_sources_indv",
                            "income_sources_O_indv", "income_sources_main_indv", "income_amount_indv",
                            "HHs_experienced_shocks_1_indv", "main_shock_indv", "drought_impct_hhs_income_indv",
                            "drought_impct_hhs_well_being_indv", "drought_recov_indv", "csiLess", "csiReduce",
                            "csiFewer", "csiBorrow", "csiAdult", "enogh_food_quantity", "enogh_food_varaiety",
                            "family_overall_well_being", "confid_supprt_ntwrk", "group_name", "date_created",
                            "group_size", "finance_access_group", "own_acc_group", "use_bank_group",
                            "duration_account_group", "bank_future_group", "land_fodder_size_grp",
                            "fodder_land_type_grp", "fodder_land_type_other_grp", "land_ownership_docs_fodder_grp",
                            "fodder_perceive_land_rights_grp", "aware_land_laws_fodder_grp", "name_grp",
                            "ind_sex_grp", "ind_age_grp", "ind_position_grp", "tel_number_grp", "hh_grp",
                            "hh_sex_grp", "hhsize_grp", "under_5_grp", "elderly_grp", "pregnant_lactating_grp",
                            "contribute_income_grp", "pwd_physical_grp", "Disability_Type_physical_grp",
                            "Disability_Type_physical_other_grp", "Disability_Type_physical_grp_no",
                            "pwd_mental_grp", "Disability_Type_mental_grp", "Disability_Type_mental_other_grp",
                            "Disability_Type_mental_grp_no", "residential_status_grp", "date_of_displacement_grp",
                            "IDP_origin_grp", "lvhd_zone_grp", "type_work_grp", "income_sources_grp",
                            "income_sources_O_grp", "income_sources_main_grp", "income_amount_grp", "amount_savings",
                            "debt", "amount_debt", "farming_type", "land_agr_size", "crop_land_type",
                            "crop_land_type_other", "land_ownership_docs", "perceive_land_rights",
                            "aware_land_laws", "crops", "note6_m", "csiLess_m2m", "csiReduce_m2m", "csiFewer_m2m",
                            "csiBorrow_m2m", "csiAdult_m2m", "enogh_food_quantity_m2m", "enogh_food_varaiety_m2m",
                            "family_overall_well_being_m2m", "confid_supprt_ntwrk_m2m", "HHs_experienced_shocks_1_grp",
                            "main_shock_grp", "drought_impct_hhs_income_grp", "drought_impct_hhs_well_being_grp",
                            "drought_recov_grp", "water1", "finance_access_individual", "own_acc_individual",
                            "use_bank_individual", "fodder_land_type_ind", "fodder_land_type_other_ind",
                            "land_ownership_docs_fodder_ind", "fodder_perceive_land_rights_ind",
                            "aware_land_laws_fodder_ind", "partner_coop","training_company","training_group",
                            "training_group_members",
                            "training_group_attend",
                            "total_training_partic",
                            "training_duration",
                            "baseline","index"
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
        
        main_survey_analysis <-  baseline_survey %>% select(X_id,start,end,X_submission_time,
                                                            enum_name, enum_gender, enum_phone_number, member, region, district,
                                                            community, registration_category, activity_groups, regis_activities,
                                                            screening_bens_id, screening_hoh_name, screening_hoh_contact, consent,
                                                            hoh_educ_level_cfw, HHs_experienced_shocks_1_cfw, main_shock_cfw,
                                                            drought_impct_hhs_income_cfw, drought_impct_hhs_well_being_cfw, drought_recov_cfw,
                                                            name_indv, ind_sex_indv, ind_age_indv, tel_number_indv,
                                                            marital_status_hoh_indv, hh_individual, hh_individual_sex, hoh_educ_level_indv,
                                                            hhsize_other_indv, under_5_indv, elderly_indv, pregnant_lactating,
                                                            contribute_income, pwd_physical_indv, Disability_Type_physical_indv,
                                                            Disability_Type_physical_other_indv, Disability_Type_physical_indv_no,
                                                            pwd_mental_indv, Disability_Type_mental_indv, Disability_Type_mental_other_indv,
                                                            Disability_Type_mental_indv_no, residential_status_indv, date_of_displacement_indv,
                                                            IDP_origin_indv, lvhd_zone_indv, type_work_indv, income_sources_indv,
                                                            income_sources_O_indv, income_sources_main_indv, income_amount_indv,
                                                            HHs_experienced_shocks_1_indv, main_shock_indv, drought_impct_hhs_income_indv,
                                                            drought_impct_hhs_well_being_indv, drought_recov_indv, csiLess, csiReduce,
                                                            csiFewer, csiBorrow, csiAdult, enogh_food_quantity, enogh_food_varaiety,
                                                            family_overall_well_being, confid_supprt_ntwrk, group_name, date_created,
                                                            group_size, finance_access_group, own_acc_group, use_bank_group,
                                                            duration_account_group, bank_future_group, land_fodder_size_grp,
                                                            fodder_land_type_grp, fodder_land_type_other_grp, land_ownership_docs_fodder_grp,
                                                            fodder_perceive_land_rights_grp, aware_land_laws_fodder_grp, name_grp,
                                                            ind_sex_grp, ind_age_grp, ind_position_grp, tel_number_grp, hh_grp,
                                                            hh_sex_grp, hhsize_grp, under_5_grp, elderly_grp, pregnant_lactating_grp,
                                                            contribute_income_grp, pwd_physical_grp, Disability_Type_physical_grp,
                                                            Disability_Type_physical_other_grp, Disability_Type_physical_grp_no,
                                                            pwd_mental_grp, Disability_Type_mental_grp, Disability_Type_mental_other_grp,
                                                            Disability_Type_mental_grp_no, residential_status_grp, date_of_displacement_grp,
                                                            IDP_origin_grp, lvhd_zone_grp, type_work_grp, income_sources_grp,
                                                            income_sources_O_grp, income_sources_main_grp, income_amount_grp, amount_savings,
                                                            debt, amount_debt, farming_type, land_agr_size, crop_land_type,
                                                            crop_land_type_other, land_ownership_docs, perceive_land_rights,
                                                            aware_land_laws, crops, note6_m, csiLess_m2m, csiReduce_m2m, csiFewer_m2m,
                                                            csiBorrow_m2m, csiAdult_m2m, enogh_food_quantity_m2m, enogh_food_varaiety_m2m,
                                                            family_overall_well_being_m2m, confid_supprt_ntwrk_m2m, HHs_experienced_shocks_1_grp,
                                                            main_shock_grp, drought_impct_hhs_income_grp, drought_impct_hhs_well_being_grp,
                                                            drought_recov_grp, water1, finance_access_individual, own_acc_individual,
                                                            use_bank_individual, fodder_land_type_ind, fodder_land_type_other_ind,
                                                            land_ownership_docs_fodder_ind, fodder_perceive_land_rights_ind,
                                                            aware_land_laws_fodder_ind, partner_coop, training_company,training_group,
                                                            training_group_members,
                                                            training_group_attend,
                                                            total_training_partic,
                                                            training_duration,baseline,index
                                                            
        ) %>%
          mutate(
            across(all_of(columns_to_replace_na), ~replace(., is.na(.), 0)),
            across(everything(), as.character),
            regis_activities = trimws(regis_activities),
            activity_groups = ifelse(is.na(activity_groups),"",trimws(activity_groups)),
            registration_category = trimws(registration_category),
            group_name = trimws(group_name),
            tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
            id = X_id,
            ben_name = coalesce(screening_hoh_name, name_indv, name_grp),
            ben_tel_number = coalesce(screening_hoh_contact,tel_number_grp,tel_number_indv),
            ben_educ_level = coalesce(hoh_educ_level_cfw,hoh_educ_level_indv),
            ben_shocks = coalesce(HHs_experienced_shocks_1_grp,HHs_experienced_shocks_1_indv,HHs_experienced_shocks_1_cfw),
            ben_main_shocks = coalesce(main_shock_grp,main_shock_indv,main_shock_cfw),
            ben_drought_impct_hhs_income = coalesce(drought_impct_hhs_income_grp,drought_impct_hhs_income_indv,drought_impct_hhs_income_cfw),
            ben_drought_impct_hhs_well_being = coalesce(drought_impct_hhs_well_being_grp,drought_impct_hhs_well_being_indv,drought_impct_hhs_well_being_cfw),
            ben_drought_recov = coalesce(drought_recov_grp,drought_recov_indv,drought_recov_cfw),
            ben_age = coalesce(ind_age_grp,ind_age_indv),
            ben_sex = coalesce(ind_sex_indv,ind_sex_grp,hh_individual_sex),
            ben_marital_status = coalesce(marital_status_hoh_indv),
            ben_head_of_hh = coalesce(hh_grp,hh_individual,),
            hh_size = coalesce(hhsize_other_indv,hhsize_grp),
            under_5_age = coalesce(under_5_indv,under_5_grp),
            elderly_age = coalesce(elderly_indv,elderly_grp),
            pregnant_lactating = coalesce(pregnant_lactating,pregnant_lactating_grp),
            contribute_income = coalesce(contribute_income,contribute_income_grp),
            pwd_physical = coalesce(pwd_physical_indv,pwd_physical_grp),
            Disability_Type_physical = coalesce(Disability_Type_physical_indv,Disability_Type_physical_grp),
            Disability_Type_physical_count = coalesce(Disability_Type_physical_indv_no,Disability_Type_physical_grp_no),
            pwd_mental = coalesce(pwd_mental_indv , pwd_mental_grp),
            Disability_Type_mental = coalesce(Disability_Type_mental_indv , Disability_Type_mental_grp),
            Disability_Type_mental_count = coalesce(Disability_Type_mental_indv_no,Disability_Type_mental_grp_no),
            ben_residential_status = coalesce(residential_status_indv,residential_status_grp),
            ben_lvhd_zone = coalesce(lvhd_zone_indv,lvhd_zone_grp),
            type_work = coalesce(type_work_indv ,type_work_grp),
            income_sources = coalesce(income_sources_indv,income_sources_grp),
            income_sources_main = coalesce(income_sources_main_indv,income_sources_main_grp),
            avrg_income_amount = coalesce(income_amount_indv,income_amount_grp),
            ben_position = coalesce(ind_position_grp),
            ben_hh_sex = hh_sex_grp,
            csiLess = coalesce(csiLess , csiLess_m2m),
            csiReduce = coalesce(csiReduce , csiReduce_m2m),
            csiFewer = coalesce(csiFewer , csiFewer_m2m),
            csiBorrow = coalesce(csiBorrow , csiBorrow_m2m),
            csiAdult = coalesce(csiAdult ,csiAdult_m2m ),
            enogh_food_quantity = coalesce(enogh_food_quantity , enogh_food_quantity_m2m),
            enogh_food_varaiety = coalesce(enogh_food_varaiety , enogh_food_varaiety_m2m),
            family_overall_well_being = coalesce(family_overall_well_being , family_overall_well_being_m2m),
            index = ifelse(is.na(index),0 , index),
            bens_id = ifelse(registration_category == 'Individuals_households_service_providers__CAHWS' , X_id , paste(X_id,index,sep = "")),
            start_time = format(parse_date_time(start, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            end_time = format(parse_date_time(end, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            group_type = case_when(
              grepl("Cooperative", activity_groups) ~ "Cooperative",
              grepl("VSLA", activity_groups) ~ "VSLA",
              grepl("EMCs", activity_groups) ~ "EMCs",
              .default = "Individual"),
            u_group_name = ifelse(group_type=="VSLA" ,  paste(group_name,'-',group_type) , paste(group_name,'-',group_type)),
            u_group_name = toupper(u_group_name)
            
          )%>%
          select(
            X_id,start,end,X_submission_time,
            enum_name, enum_gender, enum_phone_number, member, region, district,
            community, registration_category, activity_groups, regis_activities,
            screening_bens_id, screening_hoh_name, screening_hoh_contact, consent,
            tab_date, id, ben_name, ben_tel_number, ben_educ_level, ben_shocks, ben_main_shocks,ben_drought_impct_hhs_income,ben_drought_impct_hhs_well_being,
            ben_drought_recov,  ben_age,  ben_sex,ben_marital_status, ben_head_of_hh, hh_size,under_5_age,elderly_age,pregnant_lactating, contribute_income,
            pwd_physical, Disability_Type_physical, Disability_Type_physical_count,pwd_mental,Disability_Type_mental,Disability_Type_mental_count,ben_residential_status,
            ben_lvhd_zone,type_work,income_sources, income_sources_main,avrg_income_amount, ben_position, ben_hh_sex,csiLess, csiReduce,csiFewer,csiBorrow,
            csiAdult,enogh_food_quantity,enogh_food_varaiety, family_overall_well_being,bens_id,start_time,end_time,group_name, date_created,
            group_size,group_type,u_group_name, finance_access_group, own_acc_group, use_bank_group,
            duration_account_group, bank_future_group, land_fodder_size_grp,
            fodder_land_type_grp, fodder_land_type_other_grp, land_ownership_docs_fodder_grp,
            fodder_perceive_land_rights_grp, aware_land_laws_fodder_grp, amount_savings,
            debt, amount_debt, farming_type, land_agr_size, crop_land_type,
            crop_land_type_other, land_ownership_docs, perceive_land_rights,
            aware_land_laws, crops ,water1, finance_access_individual, own_acc_individual,
            use_bank_individual, fodder_land_type_ind, fodder_land_type_other_ind,
            land_ownership_docs_fodder_ind, fodder_perceive_land_rights_ind,
            aware_land_laws_fodder_ind, partner_coop, 
            training_company,training_group,
            training_group_members,
            training_group_attend,
            total_training_partic,
            training_duration , baseline
          )
        
        
        #  
        # 
        # main_survey_analysis%>%
        #   select(
        #     ben_drought_impct_hhs_income,
        #     ben_drought_impct_hhs_well_being,
        #     ben_drought_recov
        #   )
        # main_survey_analysis %>%filter(consent=="yes" & X_id %in% c("142033256","142033253","142033251")  & (
        #   registration_category == 'Individuals_households_service_providers__CAHWS' |
        #     activity_groups == 'Partnerships__company_vendor' |
        #     activity_groups == 'Partnerships__cooperatives_groups' |
        #     activity_groups == 'Partnerships__Local_market_actors_grocers' |
        #     activity_groups == 'Training_participants__cooperatives_VSLAs__and_EMCs'
        # ))%>% 
        #   select(X_id,,ben_sex,member , activity_groups , regis_activities , group_name, registration_category)
        
        
        
        
        # g <- main_survey_analysis %>%
        #   filter(consent == "yes" & registration_category == "Groups_cooperatives_VSLAs_commitees")%>%
        #   select(member, community, district,id,u_group_name,group_size,group_type , bens_id , ben_name , ben_tel_number ,activity_groups, regis_activities)
        # 
        # write.xlsx(g, "oasis registrations.xlsx")
        #   
        # print(g , n = 1000)
        
        # g <- main_survey_analysis %>%
        #   filter(consent == "yes" & registration_category == "Groups_cooperatives_VSLAs_commitees" & member == "SCI")%>%
        #   mutate(
        #     group_type = case_when(
        #       grepl("Cooperative", activity_groups) ~ "Cooperative",
        #       grepl("VSLA", activity_groups) ~ "VSLA",
        #       grepl("EMCs", activity_groups) ~ "EMCs",
        #       .default = "Individual"),
        # 
        #   )%>%
        #   group_by(group_name)%>%
        #   summarise(
        #     activity_groups = paste(activity_groups,collapse = "#"),
        #     Cooperatives =n_distinct(ifelse(group_type == "Cooperative", group_name, NA_character_),na.rm = TRUE),
        #     VSLA =n_distinct(ifelse(group_type == "VSLA", group_name, NA_character_),na.rm = TRUE),
        #     EMCs =n_distinct(ifelse(group_type == "EMCs", group_name, NA_character_),na.rm = TRUE),
        #     n = n()
        #   )
        # 
        # View(g)
        # 
        # d <- main_survey_analysis %>% select(member, region, district,
        #                                      community,group_name,ben_name,ben_sex,ben_age,ben_position,ben_tel_number,activity_groups)
        # write_xlsx(d, path = "oasisRegistrationData.xlsx")
        
        # tables <- dbListTables(postgresConnection)
        # if("brcisRegistration" %in% tables){
        #   existing_ids <- dbGetQuery(postgresConnection, 'SELECT bens_id FROM "brcisRegistration"')
        #   df_to_insert <- main_survey_analysis[!main_survey_analysis$bens_id %in% existing_ids$bens_id,]
        # 
        #   if (nrow(df_to_insert) > 0) {
        #     print(paste("Writing To Database..",nrow(df_to_insert)))
        #     dbWriteTable(postgresConnection, name = "brcisRegistration", value = df_to_insert, append = TRUE, row.names = FALSE)
        #   } else {
        #     print("No new records to insert.")
        #   }
        # }else{
        #   dbWriteTable(postgresConnection , "brcisRegistration" , main_survey_analysis)
        # }
        # 
        # main_survey_analysis <- dbGetQuery(postgresConnection, 'select * from get_registration_hh_with_screened_info;')
        tables <- dbListTables(postgresConnection)
        if("oasisRegistration" %in% tables){
          existing_ids <- dbGetQuery(postgresConnection, 'SELECT bens_id FROM "oasisRegistration"')
          df_to_insert <- main_survey_analysis[!main_survey_analysis$bens_id %in% existing_ids$bens_id,]
          
          if (nrow(df_to_insert) > 0) {
            print(paste("Rows Writing To Database :",nrow(df_to_insert)))
            dbWriteTable(postgresConnection, name = "oasisRegistration", value = df_to_insert, append = TRUE, row.names = FALSE)
          } else {
            print("No new records to insert.")
          }
        }else{
          dbWriteTable(postgresConnection , "oasisRegistration" , main_survey_analysis)
        }
        
        if(Authresult$message != 'CMU'){
          main_survey_analysis <- dbGetQuery(postgresConnection, sprintf("SELECT * FROM get_oasis_registration_hh_with_screened_info WHERE member = '%s'", Authresult$message))
        }else{
          main_survey_analysis <- dbGetQuery(postgresConnection, "SELECT * FROM get_oasis_registration_hh_with_screened_info")
        }
        
        dbDisconnect(postgresConnection)
        
        
        global_vars$global_baseline_survey <- main_survey_analysis
        
        
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
  
  
 



  output$Segrigation_element <- renderUI({
    ns1 <- NS("oasisRegistration")
    titleFilter <- ifelse(is.na(input$Segrigation) , "",input$Segrigation)
    choicesFilter <- ifelse(is.na(input$Segrigation) , c("all"),c("all",unique(pull(oasis_targets,all_of(!!sym(input$Segrigation))))))
    selectInput(
      ns1("Segrigation_element_drill"),
      paste("Filter by",input$Segrigation),
      choices = c("all",unique(pull(oasis_targets,all_of(!!sym(input$Segrigation)))))
    )
  })
  

  output$activityGroupController <- renderUI({
    ns1 <- NS("oasisRegistration")
    
    data <- oasis_activity_groups
    
    if(input$activityFroupTypeController != 'all') {
      data %<>% filter(group_type == input$activityFroupTypeController )
    }
    
    if(input$Segrigation == 'member' &&  input$Segrigation_element_drill != "all") {
      data %<>% filter(member ==input$Segrigation_element_drill )
    }
    
   
    
    selectInput(
      ns1("activityGroupControllerDrill"),
      paste("Activity Group"),
      choices = c("all",setNames(data$code,data$description))
    )
  })
  
  output$actvitiesController <- renderUI({
    ns1 <- NS("oasisRegistration")
    
    data <- oasis_activity
    
    if(input$activityGroupControllerDrill != 'all') {
      data %<>% filter(activity_group == input$activityGroupControllerDrill )
    }
    
    if(input$Segrigation == 'member' &&  input$Segrigation_element_drill != "all") {
      data %<>% filter(member ==input$Segrigation_element_drill )
    }
    
    
    
    selectInput(
      ns1("actvitiesControllerDrill"),
      paste("Activities"),
      choices = c("all",setNames(data$code,data$description)) 
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
    
    if(!is.null((input$regisController))){
      if((input$regisController) != "all"){
        
        
        main_survey %<>% filter(
          registration_category == input$regisController
        )
        
        
      }
      
    }
    
    if(!is.null((input$activityFroupTypeController))){
      if((input$activityFroupTypeController) != "all"){
        
        
        main_survey %<>% filter(
          group_type == input$activityFroupTypeController
        )
        
        
      }
      
    }
    
    if(!is.null((input$activityGroupControllerDrill))){
      if((input$activityGroupControllerDrill) != "all"){
        
        
        main_survey %<>% filter(
          grepl(escape_regex(input$activityGroupControllerDrill), activity_groups )
        )
        
        
      }
      
    }
    
    if(!is.null((input$actvitiesControllerDrill))){
      if((input$actvitiesControllerDrill) != "all"){
        
        
        main_survey %<>% filter(
          grepl(escape_regex(input$actvitiesControllerDrill), regis_activities )
        )
        
        
      }
      
    }
 
    
    
    output$totalRegistrations <- renderUI({
      nrow(main_survey)
      
    })
    
    output$GroupRegistrations <- renderUI({
           paste(n_distinct(ifelse(main_survey$registration_category == "Groups_cooperatives_VSLAs_commitees", main_survey$u_group_name, NA_character_),na.rm = TRUE),'(p=',sum(main_survey$registration_category == "Groups_cooperatives_VSLAs_commitees", na.rm = TRUE),')')
    })
    
    output$trainingsHeld <- renderUI({
      data <- main_survey%>%
        filter(consent=="yes" & !is.na(training_group))
      
      paste(nrow(data) , "(p=",sum(as.numeric(data$total_training_partic) ,na.rm = TRUE),")")
    })
    

    output$interviewProgressByMonth <- renderPlotly({
      data <- main_survey %>%
        mutate(month_year = as.character(tab_date))%>%
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
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(
                              orientation = 'h',  # Horizontal legend
                              xanchor = 'center', # Center the legend horizontally
                              x = 0.5,            # Position the legend in the middle
                              y = -0.2            # Move the legend below the chart
                            ))
      
      fig
    })
    
    output$verification_category_chart <- renderPlotly({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(registration_category)%>%
        summarise(
          count = n()
        )
      
      fig <- plot_ly(data, labels = ~registration_category, values = ~count, type = 'pie',
                     marker = list(colors = c("#008BA8","#40419A","#ED7667")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(
                              orientation = 'h',  # Horizontal legend
                              xanchor = 'center', # Center the legend horizontally
                              x = 0.5,            # Position the legend in the middle
                              y = -0.2            # Move the legend below the chart
                            ))
      
      fig
   
    })
    
    output$activityGroupsTable <- renderDT(server = FALSE , {
      activityGroupsTable <- main_survey %>%
        filter(consent == "yes")%>%
        separate_rows(activity_groups, sep = " ")%>%
        # separate_rows(regis_activities, sep = " ")%>%
        group_by(activity_groups)%>%
        reframe(
          group_type = first(group_type),
          groups = paste(n_distinct(ifelse(registration_category == "Groups_cooperatives_VSLAs_commitees", u_group_name, NA_character_),na.rm = TRUE),'(N=',sum(registration_category == "Groups_cooperatives_VSLAs_commitees", na.rm = TRUE),')',sep = ""),
          `Total beneficiaries` = n()
        )%>%
        mutate(
        
          activity_groups = gsub("_", " ", activity_groups),
          percentage = paste(round((`Total beneficiaries` / sum(`Total beneficiaries`))*100,2),'%',sep = '')
          )%>%
        select(activity_groups,group_type,groups,`Total beneficiaries`,percentage)
      
      title <- "Activity Groups"
      
      datatable(
        activityGroupsTable,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = title,
              title = title,
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
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
    })
    
    output$activityGroupsBaseTable <- renderDT(server = FALSE , {
      activityGroupsBaseTable <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(group_type)%>%
        reframe(
          `Groups` = n_distinct(ifelse(registration_category == "Groups_cooperatives_VSLAs_commitees", u_group_name, NA_character_),na.rm = TRUE),
          `Total beneficiaries` = n(),
        )%>%
        mutate(
          percentage = paste(round((`Total beneficiaries` / sum(`Total beneficiaries`))*100,2),'%',sep = '')
        )%>%
        select(group_type,`Groups`,`Total beneficiaries`,percentage)
      
      title <- "Activity Groups"
      
      datatable(
        activityGroupsBaseTable,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = title,
              title = title,
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
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
    })
    output$activityGroupsChart <- renderPlotly({
      activityGroupsChart <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(group_type)%>%
        reframe(
          total = n()
        )
      
      fig <- plot_ly(activityGroupsChart, labels = ~group_type, values = ~total, type = 'pie')
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(
                              orientation = 'h',  # Horizontal legend
                              xanchor = 'center', # Center the legend horizontally
                              x = 0.5,            # Position the legend in the middle
                              y = -0.2            # Move the legend below the chart
                            ))
      
      fig
    })
    
    output$activityTable <- renderDT(server = FALSE , {
      activityTable <- main_survey %>%
        filter(consent == "yes")%>%
        # separate_rows(activity_groups, sep = " ")%>%
        separate_rows(regis_activities, sep = " ")%>%
        group_by(regis_activities,group_type)%>%
        reframe(
          activity_groups = paste(activity_groups , collapse = " "),
          groups = paste(n_distinct(ifelse(registration_category == "Groups_cooperatives_VSLAs_commitees", u_group_name, NA_character_),na.rm = TRUE),'(N=',sum(registration_category == "Groups_cooperatives_VSLAs_commitees", na.rm = TRUE),')',sep = ""),
          `Total Beneficiaries` = n()
        )%>%
        mutate(
          regis_activities = gsub("_", " ", regis_activities),
          percentage = paste(round((`Total Beneficiaries` / sum(`Total Beneficiaries`))*100,2),'%',sep = '')
        )%>%
        select(regis_activities,group_type,groups,`Total Beneficiaries`,percentage)
      
      title <- "Activity Registration"
      
      datatable(
        activityTable,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = title,
              title = title,
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
              filename = title,
              title = title,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
    })
    
    output$membersAnalysisTable <- renderDT(server = FALSE , {
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(!!sym(input$performanceSummaryController))%>%
        summarise(
          Individual =sum(registration_category == "Individuals_households_service_providers__CAHWS", na.rm = TRUE),
          Groups = paste(n_distinct(ifelse(registration_category == "Groups_cooperatives_VSLAs_commitees", u_group_name, NA_character_),na.rm = TRUE),'(N=',sum(registration_category == "Groups_cooperatives_VSLAs_commitees", na.rm = TRUE),')',sep = ""),
          Trainings = paste(sum(!is.na(training_group),na.rm = TRUE) , "(",sum(as.numeric(total_training_partic),na.rm = TRUE),")",sep = ""),
          Cooperatives =n_distinct(ifelse(group_type == "Cooperative", u_group_name, NA_character_),na.rm = TRUE),
          VSLA =n_distinct(ifelse(group_type == "VSLA", u_group_name, NA_character_),na.rm = TRUE),
          EMCs =n_distinct(ifelse(group_type == "EMCs", u_group_name, NA_character_),na.rm = TRUE),
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
    
    output$membersAnalysisChart <- renderPlotly({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(!!sym(input$performanceSummaryController))%>%
        summarise(
          `Total Registation` = n()
        )

      fig <- plot_ly(data, labels = as.formula(paste0("~`", input$performanceSummaryController, "`")), values = ~`Total Registation`,
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(
                              orientation = 'h',  # Horizontal legend
                              xanchor = 'center', # Center the legend horizontally
                              x = 0.5,            # Position the legend in the middle
                              y = -0.2            # Move the legend below the chart
                            ),
                            title = list(
                              text = paste("Total Registration done by",input$performanceSummaryController),
                              x = 0, # x position of the title (0 is left, 1 is right)
                              xanchor = "left" # xanchor positions the title with respect to the x value (left, center, right)
                            ))

      fig
    })
    
    output$membersAnalysisChartbyTraining <- renderPlotly({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(!!sym(input$performanceSummaryController))%>%
        summarise(
          `Total Trainings` = sum(!is.na(training_group))
        )
      
      fig <- plot_ly(data, labels = as.formula(paste0("~`", input$performanceSummaryController, "`")), values = ~`Total Trainings`,
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(
                              orientation = 'h',  # Horizontal legend
                              xanchor = 'center', # Center the legend horizontally
                              x = 0.5,            # Position the legend in the middle
                              y = -0.2            # Move the legend below the chart
                            ),
                            title = list(
                              text = paste("Total Trainings Held by",input$performanceSummaryController),
                              x = 0, # x position of the title (0 is left, 1 is right)
                              xanchor = "left" # xanchor positions the title with respect to the x value (left, center, right)
                            ))
      
      fig
    })
    
    output$membersAnalysisBarChart <- renderPlotly({
      data <- main_survey %>%
        filter(consent == "yes")%>%
        group_by(!!sym(input$performanceSummaryController))%>%
        summarise(
          Individual =sum(registration_category == "Individuals_households_service_providers__CAHWS", na.rm = TRUE),
          Groups = paste(n_distinct(ifelse(registration_category == "Groups_cooperatives_VSLAs_commitees", u_group_name, NA_character_),na.rm = TRUE),'(N=',sum(registration_category == "Groups_cooperatives_VSLAs_commitees", na.rm = TRUE),')',sep = ""),
          Cooperatives =n_distinct(ifelse(group_type == "Cooperative", u_group_name, NA_character_),na.rm = TRUE),
          VSLA =n_distinct(ifelse(group_type == "VSLA", u_group_name, NA_character_),na.rm = TRUE),
          EMCs =n_distinct(ifelse(group_type == "EMCs", u_group_name, NA_character_),na.rm = TRUE),
          `Total Registation` = n()
        )
      
      fig <- plot_ly(data, x = as.formula(paste0("~`", input$performanceSummaryController, "`")), y = ~Individual, 
                     type = 'bar', name = 'individuals',marker = list(color = '#008BA8'),
                     text = ~Individual,
                     textposition = "outside")
      fig <- fig %>% add_trace(y = ~Cooperatives, name = 'Cooperatives',marker = list(color = '#ED7667'),
                               text = ~Cooperatives,
                               textposition = "outside")
      fig <- fig %>% add_trace(y = ~VSLA, name = 'VSLA',marker = list(color = '#F99D1E'),
                               text = ~VSLA,
                               textposition = "outside")
      fig <- fig %>% add_trace(y = ~EMCs, name = 'EMCs',marker = list(color = '#40419A'),
                               text = ~EMCs,
                               textposition = "outside")
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
      
      fig
      
    })
    
    output$groupTable <- renderDT(server = FALSE,{
      data <- main_survey %>%
        filter(consent=="yes")%>%
        group_by(u_group_name)%>%
        reframe(
          regis_activities = first(regis_activities),
          activity_groups = first(activity_groups),
          group_type = first(group_type),
          count = n()
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
    
    output$educationBackground <- renderPlotly({
      data <- main_survey %>%
        filter(consent=="yes")%>%
        drop_na(ben_educ_level)%>%
        group_by(ben_educ_level)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Educational Level` = ben_educ_level)%>%
        arrange(desc(count))
      data$`Educational Level` <- factor(data$`Educational Level`, levels = data$`Educational Level`)
      data$percentage <- (data$count / sum(data$count))*100
      fig <- plot_ly(data,
                     y = ~count, 
                     x = ~`Educational Level`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside")
      
      fig
    })
    
    output$educationBackground <- renderPlotly({
      data <- main_survey %>%
        filter(consent=="yes")%>%
        drop_na(ben_educ_level)%>%
        group_by(ben_educ_level)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Educational Level` = ben_educ_level)%>%
        arrange(desc(count))
      data$`Educational Level` <- factor(data$`Educational Level`, levels = data$`Educational Level`)
      data$percentage <- (data$count / sum(data$count))*100
      fig <- plot_ly(data,
                     y = ~count, 
                     x = ~`Educational Level`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside")
      
      fig
    })
    
    output$benMaritalStatus <- renderPlotly({
     
      data <- main_survey %>%
        filter(consent=="yes")%>%
        drop_na(ben_marital_status)%>%
        group_by(ben_marital_status)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Marital Status` = ben_marital_status)%>%
        arrange(desc(count))
      data$`Marital Status` <- factor(data$`Marital Status`, levels = data$`Marital Status`)
      data$percentage <- (data$count / sum(data$count))*100
      fig <- plot_ly(data,
                     y = ~count, 
                     x = ~`Marital Status`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside")
      
      fig
    })
    
    output$genderClassification <- renderPlotly({
      
        data <- main_survey%>%
        filter(consent=="yes")%>%
        drop_na(ben_sex)%>%
        group_by(ben_sex)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Gender` = ben_sex)%>%
        arrange(desc(count))
      data$`Gender` <- factor(data$`Gender`, levels = data$`Gender`)
      data$percentage <- (data$count / sum(data$count))*100
      fig <- plot_ly(data, labels = ~Gender, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
    })
    
    output$indiRespondantsAgeGroups <- renderPlotly({
      
    
      indiRespondantsAgeGroups <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(ben_age)%>%
        mutate(ben_age=as.numeric(ben_age))%>%
        pull(ben_age)
      
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
    
    output$indiRespondantslvhd_zone <- renderPlotly({
     
      
      indiRespondantslvhd_zone <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(ben_lvhd_zone)%>%
        mutate(
          ben_lvhd_zone = case_when(
            ben_lvhd_zone == "1" ~ "Urban",
            ben_lvhd_zone == "2" ~ "Pastoral",
            ben_lvhd_zone == "3" ~ "Agro-pastoral",
            ben_lvhd_zone == "4" ~ "Riverine",
            ben_lvhd_zone == "5" ~ "Agricultural",
            .default = ben_lvhd_zone
          )
        )%>%
        group_by(ben_lvhd_zone)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`LiveliHood Zones` = ben_lvhd_zone)%>%arrange(desc(count))
      
      indiRespondantslvhd_zone$`LiveliHood Zones` <- factor(indiRespondantslvhd_zone$`LiveliHood Zones`, levels = indiRespondantslvhd_zone$`LiveliHood Zones`)
      indiRespondantslvhd_zone$percentage <- (indiRespondantslvhd_zone$count / sum(indiRespondantslvhd_zone$count,na.rm = TRUE))*100
      fig <- plot_ly(indiRespondantslvhd_zone,
                     x = ~count, 
                     y = ~`LiveliHood Zones`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")
      
      fig
    })
    
    output$indiRespondantsResStatus <- renderPlotly({
      
      
      data <- main_survey%>%
        filter(consent=="yes")%>%
        drop_na(ben_residential_status)%>%
        group_by(ben_residential_status)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Residential Status` = ben_residential_status)%>%
        arrange(desc(count))
      data$`Residential Status` <- factor(data$`Residential Status`, levels = data$`Residential Status`)
      data$percentage <- (data$count / sum(data$count))*100
      fig <- plot_ly(data, labels = ~`Residential Status`, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
      
      
    })
    
    output$ben_shocks <- renderPlotly({
      
      
      ben_shocks <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(ben_shocks)%>%
        separate_rows(ben_shocks,sep = " ")%>%
        mutate(
          ben_shocks = case_when(
            ben_shocks == 1  ~ "Clan conflict (for instance related to land, pasture, water or other resources, might involve revenge)",
            ben_shocks == 2  ~ "Crop disease",
            ben_shocks == 3  ~ "Crop pests (locusts)",
            ben_shocks == 4  ~ "Death of household member(s) (including causes such as diseases, malnutrition, conflict, etc. or natural death)",
            ben_shocks == 5  ~ "Depletion of pasture",
            ben_shocks == 6  ~ "Excessive rains/flooding",
            ben_shocks == 7  ~ "Failed or below average rain/drought",
            ben_shocks == 8  ~ "Fire (including bush fire, electricity shocks, explosions of gas cylinders, etc.)",
            ben_shocks == 9  ~ "Heavy storm",
            ben_shocks == 10 ~ "Human disease outbreak (for example cholera, measles, malaria or any other type)",
            ben_shocks == 11 ~ "Illness of household members",
            ben_shocks == 12 ~ "Increased prices of agricultural/ livestock/fishery inputs",
            ben_shocks == 13 ~ "Increasing food prices",
            ben_shocks == 14 ~ "Livestock disease/death",
            ben_shocks == 15 ~ "Malnutrition/hunger",
            ben_shocks == 16 ~ "Separation/divorce of spouses",
            ben_shocks == 17 ~ "Theft of livestock",
            ben_shocks == 18 ~ "Theft of money",
            ben_shocks == 19 ~ "Unavailability of agricultural/ livestock/fishery inputs",
            ben_shocks == 20 ~ "Unemployment/lack of jobs",
            ben_shocks == 21 ~ "Violence against household members (including injuries and death due to for instance armed clashes, explosions, mortar attacks, gunfire, assassination, attacks by crocodiles or pirates, etc.)",
            ben_shocks == 22 ~ "Weeds/invasive species",
            ben_shocks == 25 ~ "My household did not experience any shocks in the last 12 months",
            TRUE ~ "Unknown")
        )%>%
        group_by(ben_shocks)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Shocks` = ben_shocks)%>%arrange(count)
      
      ben_shocks$`Shocks` <- factor(ben_shocks$`Shocks`, levels = ben_shocks$`Shocks`)
      ben_shocks$percentage <- (ben_shocks$count / sum(ben_shocks$count,na.rm = TRUE))*100
      fig <- plot_ly(ben_shocks,
                     x = ~count, 
                     y = ~`Shocks`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
  
    output$main_ben_shocks <- renderPlotly({
      
      
      ben_shocks <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(ben_main_shocks)%>%
        separate_rows(ben_main_shocks,sep = " ")%>%
        mutate(
          ben_main_shocks = case_when(
            ben_main_shocks == 1  ~ "Clan conflict (for instance related to land, pasture, water or other resources, might involve revenge)",
            ben_main_shocks == 2  ~ "Crop disease",
            ben_main_shocks == 3  ~ "Crop pests (locusts)",
            ben_main_shocks == 4  ~ "Death of household member(s) (including causes such as diseases, malnutrition, conflict, etc. or natural death)",
            ben_main_shocks == 5  ~ "Depletion of pasture",
            ben_main_shocks == 6  ~ "Excessive rains/flooding",
            ben_main_shocks == 7  ~ "Failed or below average rain/drought",
            ben_main_shocks == 8  ~ "Fire (including bush fire, electricity shocks, explosions of gas cylinders, etc.)",
            ben_main_shocks == 9  ~ "Heavy storm",
            ben_main_shocks == 10 ~ "Human disease outbreak (for example cholera, measles, malaria or any other type)",
            ben_main_shocks == 11 ~ "Illness of household members",
            ben_main_shocks == 12 ~ "Increased prices of agricultural/ livestock/fishery inputs",
            ben_main_shocks == 13 ~ "Increasing food prices",
            ben_main_shocks == 14 ~ "Livestock disease/death",
            ben_main_shocks == 15 ~ "Malnutrition/hunger",
            ben_main_shocks == 16 ~ "Separation/divorce of spouses",
            ben_main_shocks == 17 ~ "Theft of livestock",
            ben_main_shocks == 18 ~ "Theft of money",
            ben_main_shocks == 19 ~ "Unavailability of agricultural/ livestock/fishery inputs",
            ben_main_shocks == 20 ~ "Unemployment/lack of jobs",
            ben_main_shocks == 21 ~ "Violence against household members (including injuries and death due to for instance armed clashes, explosions, mortar attacks, gunfire, assassination, attacks by crocodiles or pirates, etc.)",
            ben_main_shocks == 22 ~ "Weeds/invasive species",
            ben_main_shocks == 25 ~ "My household did not experience any shocks in the last 12 months",
            TRUE ~ ben_main_shocks)
        )%>%
        group_by(ben_main_shocks)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Shocks` = ben_main_shocks)%>%arrange(count)
      
      ben_shocks$`Shocks` <- factor(ben_shocks$`Shocks`, levels = ben_shocks$`Shocks`)
      ben_shocks$percentage <- (ben_shocks$count / sum(ben_shocks$count,na.rm = TRUE))*100
      fig <- plot_ly(ben_shocks,
                     x = ~count, 
                     y = ~`Shocks`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$ben_drought_impct_hhs_income <- renderPlotly({
    
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(ben_drought_impct_hhs_income)%>%
        mutate(
          ben_drought_impct_hhs_income = case_when(
            ben_drought_impct_hhs_income == 1  ~ "No impact",
            ben_drought_impct_hhs_income == 2  ~ "Slight impact",
            ben_drought_impct_hhs_income == 3  ~ "Moderate impact",
            ben_drought_impct_hhs_income == 4  ~ "Strong impact",
            ben_drought_impct_hhs_income == 5  ~ "Dont know",
            TRUE ~ ben_drought_impct_hhs_income)
        )%>%
        group_by(ben_drought_impct_hhs_income)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Droup Impact on Income` = ben_drought_impct_hhs_income)%>%arrange(count)
      
      data$`Droup Impact on Income` <- factor(data$`Droup Impact on Income`, levels = data$`Droup Impact on Income`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     x = ~count, 
                     y = ~`Droup Impact on Income`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$ben_drought_impct_hhs_well_being <- renderPlotly({
      
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(ben_drought_impct_hhs_well_being)%>%
        mutate(
          ben_drought_impct_hhs_well_being = case_when(
            ben_drought_impct_hhs_well_being == 1  ~ "No impact",
            ben_drought_impct_hhs_well_being == 2  ~ "Slight impact",
            ben_drought_impct_hhs_well_being == 3  ~ "Moderate impact",
            ben_drought_impct_hhs_well_being == 4  ~ "Strong impact",
            ben_drought_impct_hhs_well_being == 5  ~ "Dont know",
            TRUE ~ ben_drought_impct_hhs_well_being)
        )%>%
        group_by(ben_drought_impct_hhs_well_being)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Drought Impact on Well Being` = ben_drought_impct_hhs_well_being)%>%arrange(count)
      
      data$`Drought Impact on Well Being` <- factor(data$`Drought Impact on Well Being`, levels = data$`Drought Impact on Well Being`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     x = ~count, 
                     y = ~`Drought Impact on Well Being`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$ben_drought_recov <- renderPlotly({
     
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(ben_drought_recov)%>%
        mutate(
          ben_drought_recov = case_when(
            ben_drought_recov == 1  ~ "Did not recover",
            ben_drought_recov == 2  ~ "Recovered some, but worse off than before",
            ben_drought_recov == 3  ~ "Recovered to same level as before",
            ben_drought_recov == 4  ~ " Recovered and better off",
            ben_drought_recov == 5  ~ "Not affected",
            ben_drought_recov == 5  ~ "Do not know",
            TRUE ~ ben_drought_recov)
        )%>%
        group_by(ben_drought_recov)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Drought Recovery` = ben_drought_recov)%>%arrange(count)
      
      data$`Drought Recovery` <- factor(data$`Drought Recovery`, levels = data$`Drought Recovery`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     x = ~count, 
                     y = ~`Drought Recovery`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$Disability_Type_mental <- renderPlotly({
      
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(Disability_Type_mental)%>%
        separate_rows(Disability_Type_mental , sep = " ")%>%
        group_by(Disability_Type_mental)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Mental Disability Types` = Disability_Type_mental)%>%arrange(count)
      
      data$`Mental Disability Types` <- factor(data$`Mental Disability Types`, levels = data$`Mental Disability Types`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     x = ~count, 
                     y = ~`Mental Disability Types`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$Disability_Type_physical <- renderPlotly({
      
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(Disability_Type_physical)%>%
        separate_rows(Disability_Type_physical , sep = " ")%>%
        group_by(Disability_Type_physical)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Physiscal Disability Types` = Disability_Type_physical)%>%arrange(count)
      
      data$`Physiscal Disability Types` <- factor(data$`Physiscal Disability Types`, levels = data$`Physiscal Disability Types`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     x = ~count, 
                     y = ~`Physiscal Disability Types`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" , orientation="h")%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$income_sources <- renderPlotly({
    
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(income_sources)%>%
        separate_rows(income_sources , sep = " ")%>%
        mutate(
          income_sources = case_when(
            income_sources == "1" ~ "Agriculture",
            income_sources == "2" ~ "Livestock",
            income_sources == "3" ~ "Casual Labour",
            income_sources == "4" ~ "small Business",
            income_sources == "5" ~ "Formal Employment",
            income_sources == "6" ~ "Cash Transfers",
            income_sources == "7" ~ "Remittances",
            income_sources == "9" ~ "Fishery",
            income_sources == "10" ~ "Charcoal burning",
            income_sources == "96" ~ "Other",
            .default = income_sources
          )
        )%>%
        group_by(income_sources)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Income Sources` = income_sources)%>%arrange(count)
      
      data$`Income Sources` <- factor(data$`Income Sources`, levels = data$`Income Sources`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     y = ~count, 
                     x = ~`Income Sources`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" )%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$income_sources_main <- renderPlotly({
      
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(income_sources_main)%>%
        mutate(
          income_sources_main = case_when(
            income_sources_main == "1" ~ "Agriculture",
            income_sources_main == "2" ~ "Livestock",
            income_sources_main == "3" ~ "Casual Labour",
            income_sources_main == "4" ~ "small Business",
            income_sources_main == "5" ~ "Formal Employment",
            income_sources_main == "6" ~ "Cash Transfers",
            income_sources_main == "7" ~ "Remittances",
            income_sources_main == "9" ~ "Fishery",
            income_sources_main == "10" ~ "Charcoal burning",
            income_sources_main == "96" ~ "Other",
            .default = income_sources_main
          )
        )%>%
        group_by(income_sources_main)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Main Income Sources` = income_sources_main)%>%arrange(count)
      
      data$`Main Income Sources` <- factor(data$`Main Income Sources`, levels = data$`Main Income Sources`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     y = ~count, 
                     x = ~`Main Income Sources`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" )%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$type_of_work <- renderPlotly({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(type_work)%>%
        group_by(type_work)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Type of work` = type_work)%>%arrange(count)
      
     
      fig <- plot_ly(data, labels = ~`Type of work`, values = ~count, 
                     type = 'pie',marker = list(colors = c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = 'h', y = -0.3))
      
      fig
    })
    
    output$hh_debts <- renderPlotly({
      
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(debt)%>%
        separate_rows(debt ,sep = " ")%>%
        mutate(
          debt = case_when(
            debt == "0" ~ "No debt",
            debt == "1" ~ "Food",
            debt == "2" ~ "Utilities/Housing",
            debt == "3" ~ "Healthcare",
            debt == "4" ~ "Education",
            debt == "5" ~ "Transport",
            debt == "6" ~ "Investment",
            .default = debt
          )
        )%>%
        group_by(debt)%>%
        reframe(
          count = n()
        )%>%
        dplyr::rename(`Hh debts` = debt)%>%arrange(count)
      
      data$`Hh debts` <- factor(data$`Hh debts`, levels = data$`Hh debts`)
      data$percentage <- (data$count / sum(data$count,na.rm = TRUE))*100
      fig <- plot_ly(data,
                     y = ~count, 
                     x = ~`Hh debts`, type = 'bar',
                     text = ~paste(round(percentage,1),'%'),
                     textposition = "outside" )%>%
        layout(
          yaxis = list(title="")
        )
      
      fig
    })
    
    output$averageIncome <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(avrg_income_amount)%>%
        select(avrg_income_amount)
      
      round(mean(as.numeric(data$avrg_income_amount)),2)
    })
    
    output$otherIncomContent <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(avrg_income_amount)%>%
        select(avrg_income_amount)
      
      HTML(
        paste("[",round(min(as.numeric(data$avrg_income_amount)),2),",",round(max(as.numeric(data$avrg_income_amount)),2),"]","(N=",nrow(data),")")
      )
    })
    
    output$AverageContribution <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(contribute_income)%>%
        select(contribute_income)
      
      round(mean(as.numeric(data$contribute_income)),2)
    })
    
    output$otherContributeContent <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(contribute_income)%>%
        select(contribute_income)
      
      HTML(
        paste("[",round(min(as.numeric(data$contribute_income)),2),",",round(max(as.numeric(data$contribute_income)),2),"]","(N=",nrow(data),")")
      )
    })

    output$hhsavings <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(amount_savings)%>%
        mutate(amount_savings = abs(as.numeric(amount_savings)))%>%
        select(amount_savings)

      round(mean(as.numeric(data$amount_savings)),2)
    })

    output$Otherhhsavings <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(amount_savings)%>%
        mutate(amount_savings = abs(as.numeric(amount_savings)))%>%
        select(amount_savings)

      HTML(
        paste("[",round(min(as.numeric(data$amount_savings)),2),",",round(max(as.numeric(data$amount_savings)),2),"]","(N=",nrow(data),")")
      )
    })

    output$hhdebt <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(amount_debt)%>%
        mutate(amount_debt = abs(as.numeric(amount_debt)))%>%
        select(amount_debt)

      round(mean(as.numeric(data$amount_debt)),2)
    })
    # 
    output$Otherhhdebt <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(amount_debt)%>%
        mutate(amount_debt = abs(as.numeric(amount_debt)))%>%
        select(amount_debt)

      HTML(
        paste("[",round(min(as.numeric(data$amount_debt)),2),",",round(max(as.numeric(data$amount_debt)),2),"]","(N=",nrow(data),")")
      )
    })
    
    output$valid_screened <- renderUI({
      all <- main_survey%>%filter(activity_groups == 'Individual_beneficiaries__CFW'  & consent == "yes")
      valid_screened <- main_survey%>%filter(activity_groups == 'Individual_beneficiaries__CFW' & consent == "yes" &
                                               screening_status == 'Screened')
      valid_screened = nrow(valid_screened)
      all <- nrow(all)

      if(all== 0){
        paste("0")
      }else{
        paste(valid_screened , paste('(%',round((valid_screened/all)*100,2),')',sep = ""))
      }
     
    })
    
  

    output$invalid_screened <- renderUI({
      all <- main_survey%>%filter(activity_groups == 'Individual_beneficiaries__CFW'  & consent == "yes")
      invalid_screened <- main_survey%>%filter(activity_groups == 'Individual_beneficiaries__CFW' & consent == "yes" &
                                                 screening_status == 'Invalid Screening')
      invalid_screened = nrow(invalid_screened)
      all <- nrow(all)

      if(all== 0){
        paste("0")
      }else{
        paste(invalid_screened , paste('(%',round((invalid_screened/all)*100,2),')',sep = ""))
      }
     
    })

    output$not_screened <- renderUI({
      all <- main_survey%>%filter(activity_groups == 'Individual_beneficiaries__CFW'  & consent == "yes")
      not_screened <- main_survey%>%filter(activity_groups == 'Individual_beneficiaries__CFW' & consent == "yes" &
                                             screening_status == 'Not Screened')

      not_screened = nrow(not_screened)
      all <- nrow(all)
      
      if(all== 0){
        paste("0")
      }else{
        paste(not_screened , paste('(%',round((not_screened/all)*100,2),')',sep = ""))
      }

     
    })

    output$respondatsInfoTable <- renderDT(server = FALSE ,{

      respondatsInfoTable <- main_survey%>%
        filter(activity_groups == 'Individual_beneficiaries__CFW'   & consent == "yes")%>%
        mutate(
          id = X_id
        )%>%
        select(screening_status,name_match_score, phone_match_score,tab_date,screening_bens_id,bens_id,registration_category,group_name,ben_name
               , ben_sex,ben_age,ben_tel_number,enum_name, enum_phone_number, member,
               region, district, community, ben_marital_status,
               ben_educ_level, ben_residential_status,  ben_lvhd_zone,income_sources,
               income_sources_main,avrg_income_amount, hh_size ,screened_hh_fullname,
               screened_hh_tel_number,screened_hh_member_id,screened_hh_region_id,screened_hh_district_id,screened_hh_community_id ,id
        )%>%
        mutate(
          income_sources = case_when(
            income_sources == "1" ~ "Agriculture",
            income_sources == "2" ~ "Livestock",
            income_sources == "3" ~ "Casual Labour",
            income_sources == "4" ~ "small Business",
            income_sources == "5" ~ "Formal Employment",
            income_sources == "6" ~ "Cash Transfers",
            income_sources == "7" ~ "Remittances",
            income_sources == "9" ~ "Fishery",
            income_sources == "10" ~ "Charcoal burning",
            income_sources == "96" ~ "Other",
            .default = income_sources
          ),
          income_sources_main = case_when(
            income_sources_main == "1" ~ "Agriculture",
            income_sources_main == "2" ~ "Livestock",
            income_sources_main == "3" ~ "Casual Labour",
            income_sources_main == "4" ~ "small Business",
            income_sources_main == "5" ~ "Formal Employment",
            income_sources_main == "6" ~ "Cash Transfers",
            income_sources_main == "7" ~ "Remittances",
            income_sources_main == "9" ~ "Fishery",
            income_sources_main == "10" ~ "Charcoal burning",
            income_sources_main == "96" ~ "Other",
            .default = income_sources_main
          ),
          ben_lvhd_zone = case_when(
            ben_lvhd_zone == "1" ~ "Urban",
            ben_lvhd_zone == "2" ~ "Pastoral",
            ben_lvhd_zone == "3" ~ "Agro-pastoral",
            ben_lvhd_zone == "4" ~ "Riverine",
            ben_lvhd_zone == "5" ~ "Agricultural",
            .default = ben_lvhd_zone
          )

        )
      #
      # respondatsInfoTable$actions <- sprintf('<button id="row-%s" class="hhs_view_button">View</button>', respondatsInfoTable$bens_id)
      #
      # respondatsInfoTable %<>% select(screening_status,name_match_score, phone_match_score,actions,tab_date,screening_bens_id,bens_id,hoh_name, hoh_sex,hoh_age,tel_number,enum_name, enum_phone_number, Member_id,
      #                                 Region_id, District_id, Community_id, marital_status_hoh,
      #                                 hoh_educ_level, residential_status,  lvhd_zone,income_sources,
      #                                 income_sources_main,income_amount, hh_size ,vulnerability_criteria
      # )

      Label <- 'Respondant Info'
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





    })

    output$groupedScreening <- renderPlotly({
      groupedScreening <- main_survey%>%
        filter(activity_groups == 'Individual_beneficiaries__CFW'   & consent == "yes")%>%
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
    
    output$totalNumberInHH <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(hh_size)%>%
        select(hh_size)
      
      round(sum(as.numeric(data$hh_size),na.rm = TRUE),2)
    })
    
    output$under5 <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(under_5_age)%>%
        select(under_5_age)
      
      round(sum(as.numeric(data$under_5_age),na.rm = TRUE),2)
    })
    
    output$above60 <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(elderly_age)%>%
        select(elderly_age)
      
      round(sum(as.numeric(data$elderly_age),na.rm = TRUE),2)
    })
    
    output$pregnantLactating <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")%>%
        drop_na(pregnant_lactating)%>%
        select(pregnant_lactating)
      
      round(sum(as.numeric(data$pregnant_lactating),na.rm = TRUE),2)
    })
    
    output$totRespondants <- renderUI({
      data <- main_survey%>%
        filter(consent == "yes")
       
      
      nrow(data)
    })
    
    
    
    output$trainingInfo <- renderDT(server=FALSE , {
      data <- main_survey%>%
        filter(consent == "yes" & !is.na(training_group))%>%
        select(bens_id,enum_name, enum_phone_number,tab_date,member, region, district,
               community, registration_category ,activity_groups, regis_activities , training_group , training_group_members,
               training_group_attend , total_training_partic , training_duration , training_company)
      
      Label <- 'Training Info'
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
    
    output$beneAllData <- renderDT(server = FALSE ,{
      
      beneAllData <- main_survey%>%
        filter(consent == "yes")%>%
        mutate(
          id = X_id
        )%>%
        select(screening_status,tab_date,screening_bens_id,bens_id,registration_category,regis_activities,group_name,group_type,ben_name
               , ben_sex,ben_age,ben_tel_number,enum_name, enum_phone_number, member,
               region, district, community, ben_marital_status,
               ben_educ_level, ben_residential_status,  ben_lvhd_zone,income_sources,
               income_sources_main,avrg_income_amount, hh_size
        )%>%
        mutate(
          income_sources = case_when(
            income_sources == "1" ~ "Agriculture",
            income_sources == "2" ~ "Livestock",
            income_sources == "3" ~ "Casual Labour",
            income_sources == "4" ~ "small Business",
            income_sources == "5" ~ "Formal Employment",
            income_sources == "6" ~ "Cash Transfers",
            income_sources == "7" ~ "Remittances",
            income_sources == "9" ~ "Fishery",
            income_sources == "10" ~ "Charcoal burning",
            income_sources == "96" ~ "Other",
            .default = income_sources
          ),
          income_sources_main = case_when(
            income_sources_main == "1" ~ "Agriculture",
            income_sources_main == "2" ~ "Livestock",
            income_sources_main == "3" ~ "Casual Labour",
            income_sources_main == "4" ~ "small Business",
            income_sources_main == "5" ~ "Formal Employment",
            income_sources_main == "6" ~ "Cash Transfers",
            income_sources_main == "7" ~ "Remittances",
            income_sources_main == "9" ~ "Fishery",
            income_sources_main == "10" ~ "Charcoal burning",
            income_sources_main == "96" ~ "Other",
            .default = income_sources_main
          ),
          ben_lvhd_zone = case_when(
            ben_lvhd_zone == "1" ~ "Urban",
            ben_lvhd_zone == "2" ~ "Pastoral",
            ben_lvhd_zone == "3" ~ "Agro-pastoral",
            ben_lvhd_zone == "4" ~ "Riverine",
            ben_lvhd_zone == "5" ~ "Agricultural",
            .default = ben_lvhd_zone
          )
          
        )
      #
      # respondatsInfoTable$actions <- sprintf('<button id="row-%s" class="hhs_view_button">View</button>', respondatsInfoTable$bens_id)
      #
      # respondatsInfoTable %<>% select(screening_status,name_match_score, phone_match_score,actions,tab_date,screening_bens_id,bens_id,hoh_name, hoh_sex,hoh_age,tel_number,enum_name, enum_phone_number, Member_id,
      #                                 Region_id, District_id, Community_id, marital_status_hoh,
      #                                 hoh_educ_level, residential_status,  lvhd_zone,income_sources,
      #                                 income_sources_main,income_amount, hh_size ,vulnerability_criteria
      # )
      
      Label <- 'Respondant Info'
      datatable(beneAllData,rownames = FALSE,
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
   
  })
  
  
  
}

