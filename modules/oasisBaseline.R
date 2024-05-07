

get_data <- function(username, password ,formID){
  
  
  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/",formID,sep = ""), authenticate(username, password))
  status <- status_code(response_data)
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON()%>%
      purrr::discard(is.list) %>%
      as.data.frame()
    
    names(formData) <- gsub(".*/", "", names(formData))
    
    formData%<>%
      as.data.table()
    
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
    
    
  } else if (status == 401) {
    # Unauthorized - likely incorrect username/password
    return(
      data.frame(
        status= 401,
        message = "incorrect username/password",
        data = ""
      )
    )
  } else if (status >= 500) {
    # Server error
    return(
      data.frame(
        status= 500,
        message = "Server error: Try again later.",
        data = ""
      )
    )
  } else {
    # Other errors
    return(
      data.frame(
        status= status,
        message = paste("Error encountered. Status code:", status),
        data = ""
      )
    )
  }
  ## covert to dataframe
  
  
}




baseline_targets <- readxl::read_excel("docs/oasis_baseline_comunity_targets.xlsx")
segrationDropdown <- names(baseline_targets[c("community","member","district","region")])

data_enumirators <- readxl::read_excel("docs/oasis_enumirators.xlsx")

data_enumirators$enumirator_id <- gsub("['’]", "", data_enumirators$enumirator_id )


generateData <- function(from_date , to_date ,data_variables,global_vars){
  
  baseline_targets_copy <- global_vars$global_baseline_survey %>%
    filter((spotcheck_flag == "Valid" | is.na(spotcheck_flag)) &
             (duration_status == "Valid" | is.na(duration_status)) &
                consent == "1" &
             (as.Date(tab_date) >= as.Date(from_date) &
                as.Date(tab_date) <= as.Date(to_date))) %>%
    group_by(community) %>%
    summarise(
      r_achieved = n()
    )%>%
    right_join(baseline_targets) %>%
    replace_na(list(r_achieved = 0)) %>%
    select(member , community , district , region,r_target,r_achieved) 
  
 
  
  
  data_variables$main_survey <- global_vars$global_baseline_survey %>%
    filter(as.Date(tab_date) >= as.Date(from_date) &
             as.Date(tab_date) <= as.Date(to_date))
  
  data_variables$duplicate_survey <- global_vars$duplicate_survey%>%
    filter(as.Date(tab_date) >= as.Date(from_date) &
             as.Date(tab_date) <= as.Date(to_date))
  
  data_variables$main_spotcheck_survey <- global_vars$global_spotcheck_survey %>%
    filter(as.Date(tab_date) >= as.Date(from_date) &
             as.Date(tab_date) <= as.Date(to_date))
  
  
  data_variables$baseline_targets_copy <-baseline_targets_copy
 
}




oasisBaselineUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/oasisBaseline.html",
                 
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
                 Segrigation = selectInput(
                   ns("Segrigation"),
                   "Summarise by",
                   choices = segrationDropdown
                 ),
                 
                 total_target = uiOutput(ns("total_target")),
                 total_achieved = uiOutput(ns("total_achieved")), 
                 target_vs_achieved_chart = plotlyOutput(ns("target_vs_achieved_chart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 
                 SegrigationAnalysis = DTOutput(ns("SegrigationAnalysis")),
                 Segrigation_element = uiOutput(ns("Segrigation_element")),
                 
                 ChartSegrigationAnalysis = plotlyOutput(ns("ChartSegrigationAnalysis"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 
                 total_interviews = uiOutput(ns("total_interviews")),
                 valid_interviews = uiOutput(ns("valid_interviews")),
                 invalid_interviews = uiOutput(ns("invalid_interviews")),
                 invalid_interview_spot_check = uiOutput(ns("invalid_interview_spot_check")),
                
                 invalid_interview_short_period = uiOutput(ns("invalid_interview_short_period")),
                 consented_interview = uiOutput(ns("consented_interview")),
                 non_consented_interview = uiOutput(ns("non_consented_interview")),
                 avarage_interview_time = uiOutput(ns("avarage_interview_time")),
                 max_interview_time = uiOutput(ns("max_interview_time")),
                 min_interview_time = uiOutput(ns("min_interview_time")),
                 SummaryAnalysisTable = DTOutput(ns("SummaryAnalysisTable")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
              
                 enumirator_performance_DT = DTOutput(ns("enumirator_performance_DT")),
                 enumirator_dropdown = selectInput(
                   ns("enumirator_dropdown"),
                   "Select Enumirator",
                   choices = data_enumirators$enumirator_id
                 ),
                 enumirator_dropdown2 = selectInput(
                   ns("enumirator_dropdown2"),
                   "Select Enumirator",
                   choices = data_enumirators$enumirator_id
                 ),
                 enumirator_survey_table = DTOutput(ns("enumirator_survey_table")),
                 # individual_enumirator_performance_DT = DTOutput(ns("individual_enumirator_performance_DT")),
                 total_spotchecks_done = uiOutput(ns("total_spotchecks_done")),
                 total_valid_spotchecks_done = uiOutput(ns("total_valid_spotchecks_done")),
                 total_invalid_spotchecks_done = uiOutput(ns("total_invalid_spotchecks_done")),
                 
                 spotcheckAnalysisTable = DTOutput(ns("spotcheckAnalysisTable")),
                 spotcheckAnalysisEnumiratorTable = DTOutput(ns("spotcheckAnalysisEnumiratorTable")),
                 invalidSpotcheckTable = DTOutput(ns("invalidSpotcheckTable")),
                 
                 dataToBeBackChecked = DTOutput(ns("dataToBeBackChecked")),
                 allSurveyDataTable = DTOutput(ns("allSurveyDataTable")),
                 
                 total_backchecks_done = uiOutput(ns("total_backchecks_done")),
                 total_valid_backchecks_done = uiOutput(ns("total_valid_backchecks_done")),
                 total_invalid_backchecks_done = uiOutput(ns("total_invalid_backchecks_done")),
                
                 backcheckAnalysisTable = DTOutput(ns("backcheckAnalysisTable")),
                 backcheckAnalysisEnumiratorTable = DTOutput(ns("backcheckAnalysisEnumiratorTable")),
                 invalidBackcheckTable = DTOutput(ns("invalidBackcheckTable")),
                 
                 
                 warningToLoadDATA = uiOutput(ns("warningToLoadDATA")),
                 surveyMap = leafletOutput(ns("surveyMap"),height = "1000px") %>% withSpinner(),
                 interviewStatusChart = plotlyOutput(ns("interviewStatusChart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 InvalidinterviewStatusChart = plotlyOutput(ns("InvalidinterviewStatusChart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 ConsentinterviewStatusChart = plotlyOutput(ns("ConsentinterviewStatusChart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 DurationinterviewStatusChart = plotlyOutput(ns("DurationinterviewStatusChart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 chartSummaryAnalysisTable = plotlyOutput(ns("chartSummaryAnalysisTable")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 shortInterviewTable = DTOutput(ns("shortInterviewTable")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 SpotcheckFailureInterviewsTable = DTOutput(ns("SpotcheckFailureInterviewsTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 male_household_size = uiOutput(ns("male_household_size")),
                 female_household_size = uiOutput(ns("female_household_size")),
                 HHGenderterviewStatusChart=plotlyOutput(ns("HHGenderterviewStatusChart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 
                 MinorityGroupsChart =plotlyOutput(ns("MinorityGroupsChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7) ,
                 GenderMinorityGroupsChart = plotlyOutput(ns("GenderMinorityGroupsChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 spotcheckAnalysisChart = plotlyOutput(ns("spotcheckAnalysisChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 databackcheckOverviewChart = plotlyOutput(ns("databackcheckOverviewChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 allSpotchecksDone = DTOutput(ns("allSpotchecksDone")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 allBackchecksDone= DTOutput(ns("allBackchecksDone")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 duplicateInterviewsTable = DTOutput(ns("duplicateInterviewsTable")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 duplicateInterViewCount = uiOutput(ns("duplicateInterViewCount")),
                 dailyInterview = plotlyOutput(ns("dailyInterview")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 total_mismatch_spotchecks_done = uiOutput(ns("total_mismatch_spotchecks_done")),
                 mismatchSpotcheckTable = DTOutput(ns("mismatchSpotcheckTable")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 enumirator_with_more_than_1_invalid_spotcheck =  DTOutput(ns("enumirator_with_more_than_1_invalid_spotcheck")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 max_age_hh = uiOutput(ns("max_age_hh")),
                 min_age_hh = uiOutput(ns("min_age_hh")),
                 avarage_age_hh = uiOutput(ns("avarage_age_hh")),
                 total_poeple_with_disability= uiOutput(ns("total_poeple_with_disability")),
                 total_poeple_with_disability_physical= uiOutput(ns("total_poeple_with_disability_physical")),
                 total_poeple_with_disability_mental= uiOutput(ns("total_poeple_with_disability_mental")),
                 peopleWithDisabilityChart = plotlyOutput(ns("peopleWithDisabilityChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 LivelihoodZoneChart =  plotlyOutput(ns("LivelihoodZoneChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 typesOfFarmingChart =  plotlyOutput(ns("typesOfFarmingChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 cropsCultivated =  plotlyOutput(ns("cropsCultivated"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 mainSourceofincome =  plotlyOutput(ns("mainSourceofincome"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 mainSourceOfDrinkingWater =   plotlyOutput(ns("mainSourceOfDrinkingWater"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 memberSummaryAnalysisTable = DTOutput(ns("memberSummaryAnalysisTable")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 total_varience = uiOutput(ns("total_varience"))
    )
  )
}

oasisBaseline <- function(input ,output , session,sharedValues){
  
 
  
  global_vars <- reactiveValues(
    global_baseline_survey = NULL,
    global_spotcheck_survey = NULL,
    all_survey_Data = NULL,
    duplicate_survey = NULL
  )
  
  data_variables <- reactiveValues(
    baseline_targets_copy =NULL,
    main_survey = NULL,
    main_spotcheck_survey = NULL,
    fromDateControl = NULL,
    toDateControl = NULL,
    variable_to_filter = NULL,
    value_to_filter = NULL,
    duplicate_survey = NULL
  )
  
    spin <- function(){
     
      output$messageBox <- renderUI({
              HTML(paste('<div class="spinner-border text-danger" role="status"> <span class="visually-hidden">Loading...</span>
        								</div>'))
            })
    }
  
  
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
    baseline_survey  <- get_data(onauser,onapass,"788480")
    spotcheck_survey <- get_data(onauser,onapass,"790172")
    
    # baseline_survey$latitude = as.numeric(sapply(strsplit(baseline_survey$hh_geopoint, " "), `[`, 1))
    # baseline_survey$longitude = as.numeric(sapply(strsplit(baseline_survey$hh_geopoint, " "), `[`, 2))
    # 
    # baseline_survey %>%
    #   select(hh_name,resp_name,latitude,longitude,hh_geopoint_manual,hh_geopoint,consent)
    #   filter()
    # 
    # baseline_survey%>%
    #   mutate(Minutes = paste(X_duration/60,"Minutes"))%>%
    #   select(start_time,end_time,X_duration , username,Minutes)
    
   
    
    if(first(spotcheck_survey$message) == "empty"){
      spotcheck_survey %<>% 
        mutate(
          member= NA,region= NA,district= NA,community= NA ,teamlead_name= NA,enumerator_name= NA,enum_gender= NA,
          HH_Name= NA,
          HH_Maritalstatus= NA,
          tab_date= NA,X_submission_time=NA,houseHoldIdentifier= NA,HH_Maritalstatus= NA , lvhd_zone = NA, residential_status = NA, 
          contact2= NA , pwd_mental = NA,pwd_physical = NA, HouseholdSize = NA,farming_type = NA,income_sources_main = NA,
          water1=NA , consent=NA
        )
      
      
    }
    
  #   baseline_survey %>%
  #     mutate(across(c(male5_17 , male18_49 , male50_69 , male70), ~ifelse(is.na(.), 0, .)),
  #            male_above5 = as.numeric(male5_17) + as.numeric(male18_49) + as.numeric(male50_69) + as.numeric(male70),
  #            female_above5 =  as.numeric(female5_17) +  as.numeric(female18_49) +  as.numeric(female50_69) +  as.numeric(female70))%>%
  # select(male5_17 , male18_49 , male50_69 , male70,male_above5,female_above5) 
    
    if(first(baseline_survey$status) == "200" & first(spotcheck_survey$status) =="200"  & first(baseline_survey$message) != "empty"){
     
      main_survey_analysis <- baseline_survey %>%
        select(
          member,region,district ,community ,enumerator_name,enum_gender,
          first_name,second_name,third_name,fourth_name
          ,resp_sex,hoh_first_name,hoh_second_name,hoh_third_name,hoh_fourth_name
          ,hh_sex, hh_age,
          tab_date,start_time,end_time , X_duration , consent  ,
          maritalstatus , lvhd_zone , residential_status ,
          contact2 , pwd_mental,pwd_physical,hh_size,
          farming_type  ,crops, water1,income_sources_main,
          hh_geopoint_manual,hh_geopoint,X_submission_time,timestamp1
         
        ) %>%
        mutate(
          tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
          contact2 = sub("^0+", "", contact2),
          houseHoldIdentifier = paste(community,contact2,sep = ""),
          TotalHHMembers = hh_size,
          hoh_first_name = ifelse(is.na(hoh_first_name) ,first_name , hoh_first_name ),
          hoh_second_name = ifelse(is.na(hoh_second_name) ,second_name , hoh_second_name ),
          hoh_third_name = ifelse(is.na(hoh_third_name) ,third_name , hoh_third_name ),
          hoh_fourth_name = ifelse(is.na(hoh_fourth_name) ,fourth_name , hoh_fourth_name ),
          hh_name =  paste(hoh_first_name,hoh_second_name,hoh_third_name,hoh_fourth_name),
          hh_sex = ifelse(is.na(hh_sex) ,resp_sex,hh_sex),
          start_time = format(parse_date_time(start_time, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
          end_time = format(parse_date_time(end_time, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
          X_duration = if_else(X_duration < 0, as.numeric(X_duration) * -1,  as.numeric(X_duration)),
          X_submission_time = format(ymd_hms(X_submission_time), "%B %d, %Y, %H:%M"),
          enumerator_name = ifelse(enumerator_name == "Ali_Osman_Guled_","Ali_Osman_Guled",enumerator_name)
         
           )
      
 
     
      main_survey_analysis$enumerator_name <- gsub("['’]", "", main_survey_analysis$enumerator_name)
      
      
      
      duplicate_surveys <- main_survey_analysis %>%
        group_by(houseHoldIdentifier) %>%
        filter(n() > 1) %>%
        ungroup()
      
    
      
      
     
      main_survey_analysis <- main_survey_analysis %>%
        mutate(
          duration_status = ifelse(round(X_duration / 60) < 25 ,"Invalid","Valid")
        )
      
    
      
      main_survey_analysis <- main_survey_analysis%>%
        arrange(desc(duration_status),(as_datetime(start_time)))%>%
        group_by(houseHoldIdentifier) %>%
        slice(1) %>%
        ungroup() %>%
        as.data.frame()
      
     
      
      # main_survey_analysis %>%
      #   group_by(houseHoldIdentifier) %>%
      #   summarise(count  =n())%>%
      #   as.data.frame()
      
      # main_survey_analysis %>% arrange(desc(as_datetime(tab_date))) %>% select(start_time)
     
      
      spotcheck_survey_questions <- spotcheck_survey %>%
        select(member,region,district ,community ,teamlead_name,enumerator_name,enum_gender,
               HH_Name,
               HH_Maritalstatus,
               tab_date,X_submission_time,houseHoldIdentifier , lvhd_zone , residential_status , 
               contact2 , pwd_physical,pwd_mental , HouseholdSize,
               income_sources_main , farming_type , water1,consent
        ) %>%
        mutate(
          tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
          contact2 = sub("^0+", "", contact2),
          houseHoldIdentifier = paste(community,contact2,sep = "")
        )%>%
        select(-X_submission_time)%>%
        arrange(as.Date(tab_date)) %>%
        group_by(houseHoldIdentifier) %>%
        slice(1) %>%
        ungroup() %>%
        as.data.frame()%>%
        mutate(across(everything(), ~ifelse(is.na(.), "", .)),
               houseHoldIdentifierChecking = houseHoldIdentifier)
     
      
      main_survey_analysis_spot_joining <- main_survey_analysis %>%
        left_join(spotcheck_survey_questions, by = "houseHoldIdentifier", suffix = c("_spot_main", "_spot_check"))
      # c
      
      duplicate_surveys <- duplicate_surveys %>%
        left_join(spotcheck_survey_questions, by = "houseHoldIdentifier", suffix = c("_main", "_spot_check"))
        
    
      main_survey_analysis_spot_checked <- main_survey_analysis_spot_joining %>%
        mutate(
          mutate(across(c(maritalstatus,HH_Maritalstatus,pwd_mental_spot_main,pwd_mental_spot_check,pwd_physical_spot_main,
                          pwd_physical_spot_check,
                          lvhd_zone_spot_main ,lvhd_zone_spot_check, residential_status_spot_main,residential_status_spot_check,
                          contact2_spot_main ,contact2_spot_check , farming_type_spot_check,farming_type_spot_main , 
                          water1_spot_main,water1_spot_check ,farming_type_spot_check , farming_type_spot_main,
                          income_sources_main_spot_check ,income_sources_main_spot_main), ~ifelse(is.na(.), "", .)))
        )%>%
        mutate(
          spotcheck_mismatch_count = ifelse(
            is.na(houseHoldIdentifierChecking)
            ,
            NA
            ,
            (
              ((maritalstatus) != (HH_Maritalstatus)) +
                ((pwd_mental_spot_main)  != (pwd_mental_spot_check)) +
                ((pwd_physical_spot_main)  != (pwd_physical_spot_check)) +
                ((lvhd_zone_spot_main)  != (lvhd_zone_spot_check)) +
                ((residential_status_spot_main)  != (residential_status_spot_check))+
                ((contact2_spot_main)   != (contact2_spot_check )) +
                ((TotalHHMembers)   != (HouseholdSize)  )+
                ((farming_type_spot_main)    != (farming_type_spot_check)   ) +
                ((income_sources_main_spot_main)    != (income_sources_main_spot_check)) +
                ((water1_spot_main)     != (water1_spot_check))
            )
          ),
          spotcheck_total_questions = 10,
          spotcheck_mismatch_rate = spotcheck_mismatch_count / spotcheck_total_questions,
          spotcheck_flag = ifelse(
            is.na(houseHoldIdentifierChecking),
            NA,
            ifelse(spotcheck_mismatch_rate > 0.3, "Invalid", "Valid")
          )
        )
      
      duplicate_surveys <- duplicate_surveys%>%
        mutate(
          mutate(across(c(maritalstatus,HH_Maritalstatus,pwd_mental_main,pwd_mental_spot_check,pwd_physical_main,
                          pwd_physical_spot_check,
                          lvhd_zone_main ,lvhd_zone_spot_check, residential_status_main,residential_status_spot_check,
                          contact2_main ,contact2_spot_check , farming_type_spot_check,farming_type_main , 
                          water1_main,water1_spot_check ,farming_type_spot_check , farming_type_main,
                          income_sources_main_spot_check ,income_sources_main_main), ~ifelse(is.na(.), "", .)))
        )%>%
        mutate(
          spotcheck_mismatch_count = ifelse(
            is.na(houseHoldIdentifierChecking)
            ,
            NA
            ,
            (
              ((maritalstatus) != (HH_Maritalstatus)) +
                ((pwd_mental_main)  != (pwd_mental_spot_check)) +
                ((pwd_physical_main)  != (pwd_physical_spot_check)) +
                ((lvhd_zone_main)  != (lvhd_zone_spot_check)) +
                ((residential_status_main)  != (residential_status_spot_check))+
                ((contact2_main)   != (contact2_spot_check )) +
                ((TotalHHMembers)   != (HouseholdSize)  )+
                ((farming_type_main)    != (farming_type_spot_check)   ) +
                ((income_sources_main_main)    != (income_sources_main_spot_check)) +
                ((water1_main)     != (water1_spot_check))
            )
          ),
          spotcheck_total_questions = 10,
          spotcheck_mismatch_rate = spotcheck_mismatch_count / spotcheck_total_questions,
          spotcheck_flag = ifelse(
            is.na(houseHoldIdentifierChecking),
            NA,
            ifelse(spotcheck_mismatch_rate > 0.3, "Invalid", "Valid")
          )
        )
      
     
      
      
        
      
      main_survey_analysis_spot_checked_summary <- main_survey_analysis_spot_checked %>%
        select(houseHoldIdentifier,tab_date_spot_check ,houseHoldIdentifierChecking ,spotcheck_mismatch_count ,spotcheck_total_questions,
               spotcheck_mismatch_rate ,spotcheck_flag,
               HH_Maritalstatus,pwd_mental_spot_check,pwd_physical_spot_check
                ,lvhd_zone_spot_check,residential_status_spot_check
                ,contact2_spot_check 
                ,HouseholdSize  , farming_type_spot_check ,water1_spot_check 
                ,income_sources_main_spot_check)
  
      
      duplicate_surveys <- duplicate_surveys %>%
    
        select(houseHoldIdentifier,tab_date_main,region_main,district_main,
               community_main,member_main, hh_name , contact2_main,
               enumerator_name_main ,TotalHHMembers,
               pwd_physical_main,pwd_mental_main,
               spotcheck_total_questions , spotcheck_mismatch_count , spotcheck_mismatch_rate 
               ,spotcheck_flag , X_duration, start_time , end_time,
               consent_main,X_submission_time)%>%
        dplyr::rename(
          hh_phone = contact2_main,
          tab_date = tab_date_main,
          consent = consent_main,
          member = member_main,
          region = region_main,
          district = district_main,
          community = community_main,
          enumerator_name = enumerator_name_main,
          submission_time = X_submission_time
        )
        
    
      
      main_survey_analysis <- main_survey_analysis %>%
        left_join(main_survey_analysis_spot_checked_summary ,by = "houseHoldIdentifier")
      
      spotcheck_survey_questions <- spotcheck_survey_questions %>%
        left_join(main_survey_analysis_spot_checked_summary ,by = "houseHoldIdentifier" ,suffix = c("","spot_check"))
      
    
 
      spotcheck_verifier_fuc <- function(data, enumer, cimmun, id) {
        d <- data %>%
          filter(enumerator_name == enumer & community == cimmun & spotcheck_flag == 'Valid' & houseHoldIdentifier != id)
        
        e <- data %>%
          filter(enumerator_name == enumer & community == cimmun & spotcheck_flag == 'Invalid' & houseHoldIdentifier != id)

        if(nrow(d)>=2 & nrow(e) == 0){
          return("Valid")
        }else{
          return("Invalid")
        }
      }

      # Apply the function row-wise
      main_survey_analysis <- main_survey_analysis %>%
        rowwise() %>%
        mutate(spotcheck_flag = ifelse(spotcheck_flag == 'Invalid',
                                       spotcheck_verifier_fuc(main_survey_analysis, enumerator_name, community, houseHoldIdentifier),spotcheck_flag)) %>%
        ungroup()

    
     
     spotcheck_survey_questions <- spotcheck_survey_questions %>%
       left_join(main_survey_analysis%>%select(houseHoldIdentifier,spotcheck_flag)%>%dplyr::rename(spotcheck_flag1 = spotcheck_flag) ,
                 by = "houseHoldIdentifier") %>%
       mutate(
         spotcheck_flag = spotcheck_flag1
       )%>%select(-spotcheck_flag1)
     
    

       
     

      
      duplicate_surveys <- duplicate_surveys %>%
        mutate(
          duration_status = ifelse(round(X_duration / 60) < 25 ,"Invalid","Valid")
        )
      
    
      
      if(first(spotcheck_survey$message) == "empty"){
        
     
        spotcheck_survey_questions <- spotcheck_survey_questions[FALSE, ]
      }
      
      
      
      # main_survey_analysis%>%
      #   group_by(community)%>%
      #   select(
      #     spotcheck_flag,
      #     backcheck_flag,
      #     duration_status
      #   )
      
      # main_survey_analysis %>%
      #   select(start_time , end_time , X_duration,houseHoldIdentifier , spotcheck_flag,backcheck_flag,duration_status)
      # 
      # 
      
     
      
      global_vars$all_survey_Data = baseline_survey
      global_vars$global_baseline_survey <- main_survey_analysis
      global_vars$duplicate_survey <- duplicate_surveys
      global_vars$global_spotcheck_survey <- spotcheck_survey_questions
      

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
    ns1 <- NS("oasisBaseline")
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
    
    req(input$fromDateController,input$toDateController)
    # data_variables$fromDateControl = input$fromdatecontroller
    # data_variables$toDateControl =input$todatecontroller
    
    if( is.null(nrow(global_vars$global_baseline_survey)) || nrow(global_vars$global_baseline_survey) == 0){
      return()
    }
    
    output$warningToLoadDATA <- renderUI({
      HTML("")
    })
    
    generateData(input$fromDateController, input$toDateController , data_variables,global_vars)
    
    report_data <- isolate(data_variables$baseline_targets_copy)
    main_survey = isolate(data_variables$main_survey)
    main_spotcheck_survey = isolate(data_variables$main_spotcheck_survey )
    duplicate_interview = isolate(data_variables$duplicate_survey)

    if(!is.null((input$Segrigation_element_drill))){
      if((input$Segrigation_element_drill) != "all" & !is.null((input$Segrigation))){
        report_data %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )

        main_survey %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )

        main_spotcheck_survey %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )



        duplicate_interview %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )
      }

    }
    output$total_target <- renderUI({
      format(
        sum(report_data$r_target,na.rm = TRUE),big.mark=",")
    })
    
    output$total_achieved <- renderUI({
      
      format(sum(report_data$r_achieved,na.rm = TRUE),big.mark=",")
    })
    
    output$total_varience <- renderUI({
      format(sum(report_data$r_achieved,na.rm = TRUE)-sum(report_data$r_target,na.rm = TRUE),big.mark=",")
    })
   
    output$target_vs_achieved_chart <- renderPlotly({
      
      
      chartData <- data.frame(
        "name" = c("Target","Achievement"),
        "value" = c(sum(report_data$r_target,na.rm = TRUE),sum(report_data$r_achieved,na.rm = TRUE)),
        "color" = c("#40419A","#008BA8")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
      
    })
    
    
    output$SegrigationAnalysis <- renderDT(server = FALSE,{
      summaryTable <- report_data %>%
        group_by(community) %>%
        reframe(
          region = first(region),
          district = first(district),
          member = first(member),
          
          `Total Target` = sum(r_target,na.rm = TRUE),
          `Total Achieved` = sum(r_achieved,na.rm = TRUE),
          `Varience` = `Total Achieved` - `Total Target`
        )
      
      datatable(
        summaryTable,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE,
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Total vs Achieved",
              title = "Total vs Achieved",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Total vs Achieved",
              title = "Total vs Achieved",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Total vs Achieved",
              title = "Total vs Achieved",
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
              filename = "Total vs Achieved",
              title = "Total vs Achieved",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ))
    })
    
    output$ChartSegrigationAnalysis <- renderPlotly({
      summaryTable <- report_data %>%
        group_by(community) %>%
        reframe(
          `Total Target` = sum(r_target,na.rm = TRUE),
          `Total Achieved` = sum(r_achieved,na.rm = TRUE)
          
        )
      
      fig <- plot_ly(summaryTable, x = ~community, y = ~`Total Target`, 
                     type = 'bar', 
                     name = 'Total Target'
      )
      fig <- fig %>% add_trace(y = ~`Total Achieved` ,name= 'Total Achieved')
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
      
      fig
    })
    
    output$dailyInterview <- renderPlotly({
      all_report_data <- main_survey %>%
        group_by(tab_date) %>%
        summarise(
          count = n()
        ) %>%
        arrange(as.Date(tab_date))
      
      fig <- plot_ly(all_report_data)%>% 
        add_lines(x = ~tab_date, y = ~count, name = "line",line = list(color = '#F99D1E'))%>%
        add_markers(x = ~tab_date, y = ~count, name = "Market" ,marker = list(color = 'red'))%>%
        add_text(x = ~tab_date, y = ~count,text = ~count ,name = "text")%>%
        add_bars(x = ~tab_date, y = ~count, name = "bars",marker = list(color = '#008BA8'))%>%
        layout(
          xaxis = list(tickangle = 45) # Rotate x-axis labels to diagonal (45 degrees)
        
        )
      
      
      fig
    })
    
    output$total_interviews <- renderUI({
      all_report_data <- main_survey
      
      
      format(nrow(all_report_data),big.mark=",")
    })
    
    output$valid_interviews <- renderUI({
     
      valid_report_data <- main_survey %>%
        filter(
          (spotcheck_flag == "Valid" | is.na(spotcheck_flag)) &
            (duration_status == "Valid" | is.na(duration_status))&
               consent == "1"
        )
      
    
      
     
      format( nrow(valid_report_data),big.mark=",")
    })
    
    output$invalid_interviews <- renderUI({
      
      invalid_report_data <- main_survey %>%
        filter(
          (spotcheck_flag == "Invalid" |
            duration_status == "Invalid") & consent == "1"
        )
      
      
      format(nrow(invalid_report_data),big.mark=",")
    })
    
    output$interviewStatusChart <- renderPlotly({
      
      all_interviews <- main_survey %>%
        filter(consent == "1")
      
      all_valid_interviews <- main_survey %>%
        filter(
          (spotcheck_flag == "Valid" | is.na(spotcheck_flag)) &
            (duration_status == "Valid" | is.na(duration_status)) &
               consent == "1"
        )
      
      all_invalid_interviews <- main_survey %>%
        filter(
          (spotcheck_flag == "Invalid" |
            duration_status == "Invalid") & consent == "1"
        )
      
      chartData <- data.frame(
        "name" = c("Total Interviews","Valid Interviews","Invalid Interviews"),
        "value" = c(nrow(all_interviews),nrow(all_valid_interviews),nrow(all_invalid_interviews)),
        "color" = c("#40419A","#008BA8","#ED7667")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
      
      
    })
    
 
    
    output$invalid_interview_spot_check <- renderUI({
      invalid_report_data_spotcheck <- main_survey %>%
        filter(
          spotcheck_flag == "Invalid" & consent == "1"
          
        )
      
      
      format(nrow(invalid_report_data_spotcheck),big.mark=",")
    })
    
  
    
    output$invalid_interview_short_period <- renderUI({
      invalid_report_data_short_period <- main_survey %>%
        filter(
          duration_status == "Invalid"
          & consent == "1"
        )
      
      
      format(nrow(invalid_report_data_short_period),big.mark=",")
    })
    
    output$InvalidinterviewStatusChart <- renderPlotly({
      invalid_report_data_spotcheck <- main_survey %>%
        filter(
          spotcheck_flag == "Invalid"
          & consent == "1"
        )
      
      
      invalid_report_data_short_period <- main_survey %>%
        filter(
          duration_status == "Invalid"
          & consent == "1"
        )
      
      chartData <- data.frame(
        "name" = c("Invalid through Spotcheck","Invalid through Short period"),
        "value" = c(nrow(invalid_report_data_spotcheck),nrow(invalid_report_data_short_period)),
        "color" = c("#40419A","#ED7667")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$consented_interview <- renderUI({
      consented <- main_survey %>%
        filter(
          consent == "1"
          
        )
      
     
      format( nrow(consented),big.mark=",")
    })
    
    output$non_consented_interview <- renderUI({
      nonconsented <- main_survey %>%
        filter(
          consent == "2"
          
        )
      
      
      format(nrow(nonconsented),big.mark=",")
    })
    
    output$ConsentinterviewStatusChart <- renderPlotly({
      
      consented <- main_survey %>%
        filter(
          consent == "1"
        )
      
      nonconsented <- main_survey %>%
        filter(
          consent == "2"
          
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
    
    output$total_poeple_with_disability <- renderUI({
      total_poeple_with_disability1 <- main_survey %>% filter(
        pwd_physical == "1" 
        
      )
      
      total_poeple_with_disability2 <- main_survey %>% filter(
        pwd_mental == "1"
        
      )
      
      
      format(nrow(total_poeple_with_disability1) + nrow(total_poeple_with_disability2),big.mark=",")
    })
    
    output$total_poeple_with_disability_mental <- renderUI({
      total_poeple_with_disability_mental <- main_survey %>% filter(
       
          pwd_mental == "1"
        
      )
      
      format(nrow(total_poeple_with_disability_mental),big.mark=",")
    })
    
    output$total_poeple_with_disability_physical <- renderUI({
      total_poeple_with_disability_physical <- main_survey %>% filter(
        pwd_physical == "1" 
        
      )
      
      format(nrow(total_poeple_with_disability_physical),big.mark=",")
    })
    
    output$peopleWithDisabilityChart <- renderPlotly({
      
    
      
      total_poeple_with_disability_mental <- main_survey %>% filter(
        
        pwd_mental == "1"
        
      )
      
      total_poeple_with_disability_physical <- main_survey %>% filter(
        pwd_physical == "1" 
        
      )
      
      chartData <- data.frame(
        "name" = c("Total People With Disability","Mental Disabled People","Physical Disabled People"),
        "value" = c(nrow(total_poeple_with_disability_mental) + nrow(total_poeple_with_disability_physical),nrow(total_poeple_with_disability_mental),nrow(total_poeple_with_disability_physical)),
        "color" = c("#40419A","#008BA8","#ED7667")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color),
                     marker = list(colors = c("#40419A","#008BA8","#ED7667","#F99D1E")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$LivelihoodZoneChart <- renderPlotly({
      LivelihoodZoneChart <-  main_survey %>%
        filter(consent == "1") %>%
        group_by(lvhd_zone) %>%
        summarise(
          count = n()
        )%>%
        mutate(
          lvhd_zone = case_when(
            lvhd_zone == "1" ~ "Urban",
            lvhd_zone == "2" ~ "Pastoral",
            lvhd_zone == "3" ~ "Agro-pastoral",
            lvhd_zone == "4" ~ "Riverine",
            lvhd_zone == "5" ~ "Agricultural",
            .default = lvhd_zone
          )
        )
      
      
      fig <- plot_ly(LivelihoodZoneChart, labels = ~lvhd_zone, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$typesOfFarmingChart <- renderPlotly({
      typesOfFarmingChart <-  main_survey %>%
        filter(consent == "1") %>%
        separate_rows(farming_type, sep = " ")%>%
        group_by(farming_type) %>%
        summarise(
          count = n()
        )%>%
        mutate(
          farming_type = case_when(
            farming_type == "egg" ~ "Egg production",
            farming_type == "milk" ~ "Milk production",
            farming_type == "agricultural" ~ "Agricultural production",
            farming_type == "fodder" ~ "Fodder farming",
            .default = farming_type
          )
        )
      
      
      fig <- plot_ly(typesOfFarmingChart, labels = ~farming_type, values = ~count, type = 'pie',
                     marker = list(colors = c("#40419A","#008BA8","#ED7667","#F99D1E")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$cropsCultivated <- renderPlotly({
      cropsCultivated <-  main_survey %>%
        filter(consent == "1") %>%
        separate_rows(crops, sep = " ")%>%
        group_by(crops) %>%
        summarise(
          count = n()
        )%>%
        mutate(
          crops = case_when(
            crops == "1" ~ "Sorghum",
            crops == "2" ~ "Maize",
            crops == "3" ~ "Cow Peas",
            crops == "4" ~ "Vegetables",
            crops == "5" ~ "Sesame",
            crops == "6" ~ "Beans",
            crops == "96" ~ "Other",
            .default = crops
          )
        )
    
      
      fig <- plot_ly(cropsCultivated, labels = ~crops, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$mainSourceOfDrinkingWater <- renderPlotly({
      mainSourceOfDrinkingWater <-  main_survey %>%
        filter(consent == "1") %>%
        group_by(water1) %>%
        summarise(
          count = n()
        )%>%
        mutate(
          water1 = case_when(
            water1 == "1" ~ "Piped into dwelling",
            water1 == "2" ~ "Piped to yard/plot",
            water1 == "3" ~ "Piped to neighbor",
            water1 == "4" ~ "Public tap/ standpipe",
            water1 == "5" ~ "Tube well or borehole",
            water1 == "6" ~ "Protected well",
            water1 == "7" ~ "Unprotected well",
            water1 == "8" ~ "Protected spring",
            water1 == "9" ~ "Rainwater collection",
            water1 == "10" ~ "Tanker truck",
            water1 == "11" ~ "Cart with small tank",
            water1 == "12" ~ "Surface water",
            water1 == "13" ~ "Bottled water",
            water1 == "14" ~ "Berkard",
            water1 == "15" ~ "Balley",
            water1 == "96" ~ "Other",
            .default = water1
          )
        )
      
    
      
      fig <- plot_ly(mainSourceOfDrinkingWater, labels = ~water1, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$mainSourceofincome <- renderPlotly({
      mainSourceofincome <-  main_survey %>%
        filter(consent == "1") %>%
        group_by(income_sources_main) %>%
        summarise(
          count = n()
        )%>%
        mutate(
          income_sources_main = case_when(
            income_sources_main == "1" ~ "Agriculture",
            income_sources_main == "2" ~ "Livestock",
            income_sources_main == "3" ~ "Casual Labour",
            income_sources_main == "4" ~ "Private Business",
            income_sources_main == "5" ~ "Formal Employment",
            income_sources_main == "6" ~ "Cash Transfers from Aid Agencies",
            income_sources_main == "7" ~ "Remittances",
            income_sources_main == "9" ~ "Fishery",
            income_sources_main == "10" ~ "Charcoal burning",
            income_sources_main == "96" ~ "Other",
            .default = income_sources_main
          )
        )
      
      
      
      fig <- plot_ly(mainSourceofincome, labels = ~income_sources_main, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$avarage_age_hh <- renderUI({
      avarage_age_hh  <-main_survey %>%
        filter(consent == "1")
      if(nrow(avarage_age_hh) >0){
        avarage_age_hh <- round(mean(avarage_age_hh$hh_age),0)
      }else{
        avarage_age_hh <- 0
      }
      avarage_age_hh
    })
    
    output$min_age_hh <- renderUI({
      min_age_hh  <-main_survey %>%
        filter(consent == "1")
      if(nrow(min_age_hh) >0){
        min_age_hh <- round(min(min_age_hh$hh_age),0)
      }else{
        min_age_hh <- 0
      }
      min_age_hh
    })
    
    output$max_age_hh <- renderUI({
      max_age_hh  <-main_survey %>%
        filter(consent == "1")
      if(nrow(max_age_hh) >0){
        max_age_hh <- round(max(max_age_hh$hh_age),0)
      }else{
        max_age_hh <- 0
      }
      max_age_hh
    })
    
    output$avarage_interview_time <- renderUI({
      
      main_survey1  <-main_survey %>%
        filter(consent == "1" & 
                 (X_duration/60)<180 & (X_duration/60)>20 )
      
      if(nrow(main_survey1) >0 ){
        avragetime <- round(mean(main_survey1$X_duration) /60,0)
      }else{
        avragetime <- 0
      }
      
      paste( avragetime , "Minutes")
      
    })
    
    output$max_interview_time <- renderUI({
      main_survey1  <-main_survey %>%
        filter(consent == "1" & (X_duration/60)<180)
     
      if(nrow(main_survey1) >0){
        maxavarage <- round(max(main_survey1$X_duration) /60,0)
      }else{
        maxavarage <- 0
      }
      
      paste( maxavarage , "Minutes")
      
    })
    
    output$min_interview_time <- renderUI({
      
      main_survey1  <-main_survey %>%
        filter(consent == "1" & 
                 (X_duration/60)>20)
      
      if(nrow(main_survey1) >0){
        minavarage <- round(min(main_survey1$X_duration) /60,0)
      }else{
        minavarage <- 0
      }
      
      paste( minavarage , "Minutes")
      
    })
    
    output$male_household_size <- renderUI({
      male_household_size  <-main_survey %>%
        filter(consent == "1" & hh_sex == "1")
      
      
      format(nrow(male_household_size),big.mark=",")
      
    })
    
    output$female_household_size <- renderUI({
      female_household_size  <-main_survey %>%
        filter(consent == "1" & hh_sex == "2")
      
      
      format(nrow(female_household_size),big.mark=",")
      
    })
    
    output$HHGenderterviewStatusChart <- renderPlotly({
      
      male_household_size  <-main_survey %>%
        filter(consent == "1" & hh_sex == "1")
      female_household_size  <-main_survey %>%
        filter(consent == "1" & hh_sex == "2")
     
      chartData <- data.frame(
        "name" = c("Male Head of houseHold","Female Head of houseHold"),
        "value" = c(nrow(male_household_size),nrow(female_household_size)),
        "color" = c("#008BA8","#ED7667")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title = "Minutes",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$SummaryAnalysisTable <- renderDT(server = FALSE , {
      
      baseline_targets_dt <- report_data
      
      
      totalInterviewDT <- main_survey %>%
        group_by(community) %>%
        reframe(
          `Total Interviews` = n()
        )
      
      totalValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Valid" | is.na(spotcheck_flag)) &
                
                 (duration_status == "Valid" | is.na(duration_status)) &
                 consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Valid Interviews` = n()
        )
      
      totalInValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Invalid" |
                
                 duration_status == "Invalid") & consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Invalid Interviews` = n()
        )
      
      totalInvalidSpotcheck <- main_survey %>%
        filter(
          spotcheck_flag == "Invalid"
          & consent == "1"
        )%>%
        group_by(community) %>%
        reframe(
          `Invalid Spotcheck` = n()
        )
      
     
      
      
      totalShortPeriodDT <- main_survey %>%
        filter(duration_status == "Invalid" & consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Short Surveys` = n()
        )
      
      totalConsentedInterviews <- main_survey %>%
        filter(consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Consented Surveys` = n()
        )
      
      totalNonConsentedSurveys <- main_survey %>%
        filter(consent == "2") %>%
        group_by(community) %>%
        reframe(
          `Non-Consented Surveys` = n()
        )
      
      totalAvarageDurationDT <- main_survey %>%
        filter(consent == "1" & 
                 (X_duration/60)<180 & (X_duration/60)>20)%>%
        group_by(community) %>%
        reframe(
          `Avarage Survey Time` = round(mean(X_duration) /60,0)
        )
      
     
      baseline_targets_dt %<>% 
        left_join(totalInterviewDT , by = "community") %>%
        replace_na(list(`Total Interviews` = 0)) %>%
        left_join(totalValidInterviewDT , by = "community") %>%
        replace_na(list(`Valid Interviews` = 0)) %>%
        left_join(totalInValidInterviewDT , by = "community") %>%
        replace_na(list(`Invalid Interviews` = 0)) %>%
        left_join(totalShortPeriodDT , by = "community") %>%
        replace_na(list(`Short Surveys` = 0))%>%
        left_join(totalInvalidSpotcheck , by = "community") %>%
        replace_na(list(`Invalid Spotcheck` = 0))%>%
        left_join(totalConsentedInterviews , by = "community") %>%
        replace_na(list(`Consented Surveys` = 0))%>%
        left_join(totalNonConsentedSurveys , by = "community") %>%
        replace_na(list(`Non-Consented Surveys` = 0))%>%
        left_join(totalAvarageDurationDT , by = "community") %>%
        replace_na(list(`Avarage Survey Time` = 0)) %>%
        select(-c('r_target'))
      
      # if(input$Segrigation == "community"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #        region = first(region),
      #        cluster = first(cluster),
      #        district = first(district),
      #        member = first(member),
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = sum(`Short Surveys`,na.rm = TRUE),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "region"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       communities = n(),
      #       
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "district"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       region = first(region),
      #       communities = n(),
      #       region = first(region),
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "member"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       region = first(region),
      #       district = first(district),
      #       communities = n(),
      #      
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "cluster"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       region = first(region),
      #       district = first(district),
      #       member = first(member),
      #       communities = n(),
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else{
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }
      # 
      baseline_targets_dt %<>% group_by(community) %>%
        reframe(
          region = first(region),
          district = first(district),
          member = first(member),
          `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
          `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
          `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
          `Short Surveys` = sum(`Short Surveys`,na.rm = TRUE),
          `Invalid Spotcheck` = sum(`Invalid Spotcheck`,na.rm = TRUE),
          `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
          `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
          `Avarage Survey Time` = paste(sum(`Avarage Survey Time`,na.rm = TRUE),"Minutes")
          
        )
      
      datatable(
        baseline_targets_dt,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Overall Summary",
              title = "Overall Summary",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Overall Summary",
              title = "Overall Summary",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Overall Summary",
              title = "Overall Summary",
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
              filename = "Overall Summary",
              title = "Overall Summary",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
      
      
    })
    
    output$memberSummaryAnalysisTable <- renderDT({
      baseline_targets_dt <- report_data
      
      
      totalInterviewDT <- main_survey %>%
        group_by(community) %>%
        reframe(
          `Total Interviews` = n()
        )
      
      totalValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Valid" | is.na(spotcheck_flag)) &
                 
                 (duration_status == "Valid" | is.na(duration_status)) &
                 consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Valid Interviews` = n()
        )
      
      totalInValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Invalid" |
                  
                  duration_status == "Invalid") & consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Invalid Interviews` = n()
        )
      
      totalInvalidSpotcheck <- main_survey %>%
        filter(
          spotcheck_flag == "Invalid"
          & consent == "1"
        )%>%
        group_by(community) %>%
        reframe(
          `Invalid Spotcheck` = n()
        )
      
      
      
      
      totalShortPeriodDT <- main_survey %>%
        filter(duration_status == "Invalid" & consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Short Surveys` = n()
        )
      
      totalConsentedInterviews <- main_survey %>%
        filter(consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Consented Surveys` = n()
        )
      
      totalNonConsentedSurveys <- main_survey %>%
        filter(consent == "2") %>%
        group_by(community) %>%
        reframe(
          `Non-Consented Surveys` = n()
        )
      
      totalAvarageDurationDT <- main_survey %>%
        filter(consent == "1" & 
                 (X_duration/60)<180 & (X_duration/60)>20)%>%
        group_by(community) %>%
        reframe(
          `Avarage Survey Time` = round(mean(X_duration) /60,0)
        )
      
      
      baseline_targets_dt %<>% 
        left_join(totalInterviewDT , by = "community") %>%
        replace_na(list(`Total Interviews` = 0)) %>%
        left_join(totalValidInterviewDT , by = "community") %>%
        replace_na(list(`Valid Interviews` = 0)) %>%
        left_join(totalInValidInterviewDT , by = "community") %>%
        replace_na(list(`Invalid Interviews` = 0)) %>%
        left_join(totalShortPeriodDT , by = "community") %>%
        replace_na(list(`Short Surveys` = 0))%>%
        left_join(totalInvalidSpotcheck , by = "community") %>%
        replace_na(list(`Invalid Spotcheck` = 0))%>%
        left_join(totalConsentedInterviews , by = "community") %>%
        replace_na(list(`Consented Surveys` = 0))%>%
        left_join(totalNonConsentedSurveys , by = "community") %>%
        replace_na(list(`Non-Consented Surveys` = 0))%>%
        left_join(totalAvarageDurationDT , by = "community") %>%
        replace_na(list(`Avarage Survey Time` = 0)) %>%
        select(-c('r_target'))
      
      # if(input$Segrigation == "community"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #        region = first(region),
      #        cluster = first(cluster),
      #        district = first(district),
      #        member = first(member),
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = sum(`Short Surveys`,na.rm = TRUE),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "region"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       communities = n(),
      #       
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "district"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       region = first(region),
      #       communities = n(),
      #       region = first(region),
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "member"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       region = first(region),
      #       district = first(district),
      #       communities = n(),
      #      
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else if(input$Segrigation == "cluster"){
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       region = first(region),
      #       district = first(district),
      #       member = first(member),
      #       communities = n(),
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }else{
      #   baseline_targets_dt %<>% group_by(!!sym(input$Segrigation)) %>%
      #     reframe(
      #       
      #       `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
      #       `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
      #       `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
      #       `Short Surveys` = round(sum(`Short Surveys`,na.rm = TRUE) /60,0),
      #       `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
      #       `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
      #       `Avarage Survey Time` = sum(`Avarage Survey Time`,na.rm = TRUE)
      #       
      #     )
      # }
      # 
      baseline_targets_dt %<>% group_by(member) %>%
        reframe(
          communities = n(),
          `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
          `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
          `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE),
          `Short Surveys` = sum(`Short Surveys`,na.rm = TRUE),
          `Invalid Spotcheck` = sum(`Invalid Spotcheck`,na.rm = TRUE),
          `Consented Surveys` = sum(`Consented Surveys`,na.rm = TRUE),
          `Non-Consented Surveys` =sum(`Non-Consented Surveys`,na.rm = TRUE),
          `Avarage Survey Time` = paste(sum(`Avarage Survey Time`,na.rm = TRUE),"Minutes")
          
        )
      
      datatable(
        baseline_targets_dt,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Overall Summary",
              title = "Overall Summary",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Overall Summary",
              title = "Overall Summary",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Overall Summary",
              title = "Overall Summary",
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
              filename = "Overall Summary",
              title = "Overall Summary",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
    })
    
    output$chartSummaryAnalysisTable <- renderPlotly({
      
      baseline_targets_dt <- report_data
      
      totalInterviewDT <- main_survey %>%
        filter(consent == "1")%>%
        group_by(community) %>%
        reframe(
          `Total Interviews` = n()
        )
      
      totalValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Valid" | is.na(spotcheck_flag)) &
                 (duration_status == "Valid" | is.na(duration_status)) &
                 consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Valid Interviews` = n()
        )
      
      totalInValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Invalid" |
                 duration_status == "Invalid") & consent == "1") %>%
        group_by(community) %>%
        reframe(
          `Invalid Interviews` = n()
        )
      
      baseline_targets_dt %<>% 
        left_join(totalInterviewDT , by = "community") %>%
        replace_na(list(`Total Interviews` = 0)) %>%
        left_join(totalValidInterviewDT , by = "community") %>%
        replace_na(list(`Valid Interviews` = 0)) %>%
        left_join(totalInValidInterviewDT , by = "community") %>%
        replace_na(list(`Invalid Interviews` = 0)) %>%
        select(-c('r_target','r_achieved'))%>%
        group_by(member) %>%
        reframe(
          
          `Total Interviews` = sum(`Total Interviews`,na.rm = TRUE),
          `Valid Interviews` = sum(`Valid Interviews`,na.rm = TRUE),
          `Invalid Interviews` = sum(`Invalid Interviews`,na.rm = TRUE)
          
        )
      # as.formula(paste0("~`", community, "`"))
     
      fig <- plot_ly(baseline_targets_dt, x = ~member, y = ~`Total Interviews`, 
                     type = 'bar', 
                     name = 'Total Interviews',
                     marker = list(color = '#008BA8'),
                     text = ~round(`Total Interviews`, 2), textposition = 'outside'
                     )
      fig <- fig %>% add_trace(y = ~`Valid Interviews` ,name= 'Valid Interviews',
                               marker = list(color = '#F99D1E'),
                               text = ~round(`Valid Interviews`, 2), textposition = 'outside')
      fig <- fig %>% add_trace(y = ~`Invalid Interviews` ,name= 'Invalid Interviews',
                               marker = list(color = '#ED7667'),
                               text = ~round(`Invalid Interviews`, 2), textposition = 'outside')
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
      
      fig
    })
    
    output$shortInterviewTable <- renderDT(server = FALSE ,{
      shortInterviewTable <- main_survey %>%
        filter(consent == "1")%>%
        select(houseHoldIdentifier,member,region,district ,community,enumerator_name,
               hh_name,contact2,
               tab_date,start_time,end_time , X_duration , consent ,duration_status)%>%
        dplyr::rename(
          hh_phone = contact2,
          interview_date = tab_date
        ) %>%
        mutate(
          interview_duration = paste(round(X_duration/60),"Minutes"),
          consent = ifelse(consent == "1" ,"Consented","Non-consented")
        ) %>%
        select (
          houseHoldIdentifier,interview_date, interview_duration,duration_status,start_time,end_time,member,region,district ,community,enumerator_name,
          hh_name,hh_phone , interview_duration  , consent
        )%>%
        filter(
          duration_status == "Invalid"
        )
      
      datatable(
        shortInterviewTable,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "short period",
              title = "short period",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "short period",
              title = "short period",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "short period",
              title = "short period",
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
              filename = "short period",
              title = "short period",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
      
      
    })
    
    output$SpotcheckFailureInterviewsTable <- renderDT(server = FALSE ,{
     
     
      
      SpotcheckFailureInterviewsTable <- main_survey %>%
        filter(consent == "1")%>%
        mutate(
          "Q1 Marital Status" = paste(paste("Main Survey:",maritalstatus),paste("Spotchech Survey:",HH_Maritalstatus),sep = " | "),
          "Q2 pwd_physical" = paste(paste("Main Survey:",pwd_physical),paste("Spotchech Survey:",pwd_physical_spot_check),sep = " | "),
          "Q3 pwd_mental" = paste(paste("Main Survey:",pwd_mental),paste("Spotchech Survey:",pwd_mental_spot_check),sep = " | "),
          "Q4 Livelihood zone" = paste(paste("Main Survey:",lvhd_zone),paste("Spotchech Survey:",lvhd_zone_spot_check),sep = " | "),
          "Q5 Residential status" = paste(paste("Main Survey:",residential_status),paste("Spotchech Survey:",residential_status_spot_check),sep = " | "),
          "Q6 Phone" = paste(paste("Main Survey:",contact2),paste("Spotchech Survey:",contact2_spot_check),sep = " | "),
          "Q7 Household Size" = paste(paste("Main Survey:",TotalHHMembers),paste("Spotchech Survey:",HouseholdSize),sep = " | "),
          "Q8 Marital Faramers Type" = paste(paste("Main Survey:",farming_type),paste("Spotchech Survey:",farming_type_spot_check),sep = " | "),
          "Q9 Water Source" = paste(paste("Main Survey:",water1),paste("Spotchech Survey:",water1_spot_check),sep = " | "),
          "Q10 Main Income" = paste(paste("Main Survey:",income_sources_main),paste("Spotchech Survey:",income_sources_main_spot_check),sep = " | ")
        )%>%
        select(member,region,district ,community,enumerator_name,
               hh_name,contact2,
               tab_date,tab_date_spot_check,start_time,end_time , X_duration , consent ,duration_status,
               spotcheck_total_questions, spotcheck_mismatch_count, spotcheck_mismatch_rate,
               spotcheck_flag,`Q1 Marital Status`,`Q2 pwd_physical`,`Q3 pwd_mental`,`Q4 Livelihood zone`,`Q5 Residential status`,`Q6 Phone`,`Q7 Household Size`
               ,`Q8 Marital Faramers Type`,`Q9 Water Source`,`Q10 Main Income`)%>%
        dplyr::rename(
          hh_phone = contact2,
          interview_date = tab_date,
          Spotcheck_date = tab_date_spot_check
        ) %>%
        mutate(
          interview_duration = paste(round(X_duration/60),"Minutes"),
          consent = ifelse(consent == "1" ,"Consented","Non-consented"),
          spotcheck_mismatch_rate = paste(spotcheck_mismatch_rate*100,"%",sep = "")
        ) %>%
        select (
          interview_date,Spotcheck_date,member,region,district ,community,enumerator_name,
          hh_name,hh_phone , interview_duration , interview_duration,duration_status , consent,
          spotcheck_total_questions, spotcheck_mismatch_count, spotcheck_mismatch_rate,
          spotcheck_flag,`Q1 Marital Status`,`Q2 pwd_physical`,`Q3 pwd_mental`,`Q4 Livelihood zone`,`Q5 Residential status`,`Q6 Phone`,`Q7 Household Size`
          ,`Q8 Marital Faramers Type`,`Q9 Water Source`,`Q10 Main Income`
        )%>%
        filter(
          spotcheck_flag == "Invalid"
        )
      
      datatable(
        SpotcheckFailureInterviewsTable,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Spotcheck Failure",
              title = "Spotcheck Failure",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Spotcheck Failure",
              title = "Spotcheck Failure",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Spotcheck Failure",
              title = "Spotcheck Failure",
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
              filename = "Spotcheck Failure",
              title = "Spotcheck Failure",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
      
      
    })
    
   
    
    output$duplicateInterviewsTable <- renderDT(server = FALSE,{
      duplicateInterviewsTable <- duplicate_interview%>%
        filter(consent == "1")%>%
        dplyr::rename(
          interview_date = tab_date
        )%>%
        mutate(
          interview_duration = paste(round(X_duration/60),"Minutes"),
          consent = ifelse(consent == "1" ,"Consented","Non-consented")
        ) %>%
        select (TotalHHMembers,X_duration,houseHoldIdentifier,
          interview_date, interview_duration,duration_status,submission_time,start_time,end_time,member,region,district ,community ,enumerator_name,
          hh_name,hh_phone  , spotcheck_flag,consent
        )%>%
        arrange(houseHoldIdentifier,desc(duration_status),as_datetime(start_time)) %>%
        group_by(houseHoldIdentifier) %>%
        mutate(
          state = if_else(row_number() == 1, "taken", "ignored")
        ) %>%
        ungroup() %>%
        select (state,houseHoldIdentifier,
                interview_date, interview_duration,duration_status,start_time,end_time,member,region,district ,community ,enumerator_name,
                hh_name,TotalHHMembers,hh_phone  , spotcheck_flag,consent
        )
      
      datatable(
        duplicateInterviewsTable,
        
        extensions = 'Buttons', 
        options = list(
         
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Duplicate Interview",
              title = "Duplicate Interview",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Duplicate Interview",
              title = "Duplicate Interview",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Duplicate Interview",
              title = "Duplicate Interview",
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
              filename = "Duplicate Interview",
              title = "Duplicate Interview",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
      
    })
    
   
      
    
    output$duplicateInterViewCount <- renderUI({
      
      duplicate_interview1 <-duplicate_interview%>%
        filter(consent == "1")
      
      unique_hh <- length(unique(duplicate_interview1$houseHoldIdentifier))
      
      total_duplicate_entries <- nrow(duplicate_interview1) - unique_hh
      
      total_duplicate_entries
    })
    
    
    
    
    
    
    
    
    
    #-------------vulnerability groups-----------
    
   
    
    
   
    
    output$enumirator_performance_DT <- renderDT(server = FALSE ,{
      enums <- data_enumirators %>% data.frame()
     
      # main_survey_analysis %>%
      #   group_by(enumerator_name) %>%
      #   reframe(
      #     `Total Interviews` = n()
      #   )
      
      if(!is.null((input$Segrigation_element_drill))){
        if((input$Segrigation_element_drill) != "all" & !is.null((input$Segrigation))){
          if(input$Segrigation == "member"){
            enums <- enums %>% filter(ngo == input$Segrigation_element_drill)
          }
        }
        
      }
      
     
        
      
      enumsTotalInterviewDT <- main_survey %>%
        
        group_by(enumerator_name) %>%
        reframe(
          `Total Interviews` = n()
        )
      
      enumsTotalValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Valid" | is.na(spotcheck_flag)) &
                 (duration_status == "Valid" | is.na(duration_status)) & consent == "1") %>%
        group_by(enumerator_name) %>%
        reframe(
          `Valid Interviews` = n()
        )
      
      enumsTotalInValidInterviewDT <- main_survey %>%
        filter((spotcheck_flag == "Invalid" |
                 duration_status == "Invalid") & consent == "1") %>%
        group_by(enumerator_name) %>%
        reframe(
          `Invalid Interviews` = n()
        )
      
      
      
      enumsTotalShortPeriodDT <- main_survey %>%
        filter(duration_status == "Invalid" & consent == "1") %>%
        group_by(enumerator_name) %>%
        reframe(
          `Short Surveys` = n()
        )
      
      enumsTotalConsentedInterviews <- main_survey %>%
        filter(consent == "1") %>%
        group_by(enumerator_name) %>%
        reframe(
          `Consented Surveys` = n()
        )
      
      enumsTotalNonConsentedSurveys <- main_survey %>%
        filter(consent == "2") %>%
        group_by(enumerator_name) %>%
        reframe(
          `Non-Consented Surveys` = n()
        )
      
      enumsTotalFailedSpotCheckSurveys <- main_survey %>%
        filter(spotcheck_flag == "Invalid" & consent == "1") %>%
        group_by(enumerator_name) %>%
        reframe(
          `Failed Spot-Checks` = n()
        )
     
      
      enumsTotalSpotCheckSurveys <- main_survey %>%
        filter(spotcheck_flag == "Valid" & consent == "1") %>%
        group_by(enumerator_name) %>%
        reframe(
          `Success Spot-Checks` = n()
        )
      
    
      
      enumsAvarageDUration <- main_survey %>%
        group_by(enumerator_name) %>%
        reframe(
          `Avarage Duration (mins)` = round(mean(X_duration) /60,0)
        )
      
      enumsMinDUration <- main_survey %>%
        group_by(enumerator_name) %>%
        reframe(
          `Min Duration (mins)` = round(min(X_duration) /60,0)
        )
      
      enumsMaxDUration <- main_survey %>%
        group_by(enumerator_name) %>%
        reframe(
          `Max Duration (mins)` = round(max(X_duration) /60,0)
        )
      
      
      enums %<>% 
        left_join(enumsTotalInterviewDT , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Total Interviews` = 0)) %>%
        left_join(enumsTotalValidInterviewDT , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Valid Interviews` = 0)) %>%
        left_join(enumsTotalInValidInterviewDT , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Invalid Interviews` = 0)) %>%
        left_join(enumsTotalShortPeriodDT , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Short Surveys` = 0))%>%
        left_join(enumsTotalConsentedInterviews , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Consented Surveys` = 0))%>%
        left_join(enumsTotalNonConsentedSurveys , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Non-Consented Surveys` = 0))%>%
        left_join(enumsTotalFailedSpotCheckSurveys , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Failed Spot-Checks` = 0))%>%
        left_join(enumsTotalSpotCheckSurveys , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Success Spot-Checks` = 0))%>%
        left_join(enumsAvarageDUration , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Avarage Duration (mins)` = 0))%>%
        left_join(enumsMinDUration , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Min Duration (mins)` = 0))%>%
        left_join(enumsMaxDUration , by = c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Max Duration (mins)` = 0)) %>%
        select(-c("enumirator_id"))
      
      datatable(
        enums,
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "enumirator performance",
              title = "enumirator performance",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "enumirator performance",
              title = "enumirator performance",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "enumirator performance",
              title = "enumirator performance",
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
              filename = "enumirator performance",
              title = "enumirator performance",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        ))
    })
    
    output$enumirator_survey_table <- renderDT(server= FALSE,{
      
      enumirator_survey_table <- main_survey %>%
        filter(
          enumerator_name == input$enumirator_dropdown2) %>%
        rename(HH_phone = contact2)%>%
        mutate(
               
               spotcheck_total_questions =  ifelse(is.na(spotcheck_total_questions) | spotcheck_total_questions =="" ,0,spotcheck_total_questions),
               spotcheck_mismatch_rate =  ifelse(is.na(spotcheck_mismatch_rate) | spotcheck_mismatch_rate =="" ,0,spotcheck_mismatch_rate),
               spotcheck_flag =  ifelse(is.na(spotcheck_flag) | spotcheck_flag =="" ,"NA",spotcheck_flag),
               spotcheck_mismatch_count = ifelse(is.na(spotcheck_mismatch_count) | spotcheck_mismatch_count =="" ,"NA",spotcheck_mismatch_count),
              
               consent  = ifelse(consent == "1" , "Consented" , "Non-Consented"),
               interview_duration = paste((X_duration /60),"Minutes")) %>%
        select(tab_date,region,district,member  , community  , enumerator_name,
               hh_name ,HH_phone,
               TotalHHMembers,
               spotcheck_total_questions ,spotcheck_mismatch_count,spotcheck_mismatch_rate ,spotcheck_flag ,
               interview_duration,duration_status ,consent
        )
      
      
      datatable(enumirator_survey_table,
                extensions = 'Buttons', 
                options = list(
                  scrollX = TRUE, 
                  pageLength = 20, 
                  
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = paste(input$enumirator_dropdown2,"surveys"),
                      title = paste(input$enumirator_dropdown2,"surveys"),
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'excel',
                      filename = paste(input$enumirator_dropdown2,"surveys"),
                      title = paste(input$enumirator_dropdown2,"surveys"),
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'pdf',
                      filename = paste(input$enumirator_dropdown2,"surveys"),
                      title = paste(input$enumirator_dropdown2,"surveys"),
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
                      filename = paste(input$enumirator_dropdown2,"surveys"),
                      title = paste(input$enumirator_dropdown2,"surveys"),
                      exportOptions = list(modifier = list(page = 'all'))
                    )
                    
                  )
                ))
      
      
    })
    
    output$enumirator_with_more_than_1_invalid_spotcheck <- renderDT(server = FALSE , {
      enumirator_with_more_than_1_invalid_spotcheck <- main_survey %>%
        select(member,region,district ,community,enumerator_name,
               hh_name,contact2,
               tab_date,tab_date_spot_check,start_time,end_time , X_duration , consent ,duration_status,
               spotcheck_total_questions, spotcheck_mismatch_count, spotcheck_mismatch_rate,
               spotcheck_flag)%>%
        filter(spotcheck_flag == "Invalid") %>%
        group_by(enumerator_name , community) %>%
        filter(n() > 1) %>%
        ungroup()
      
      datatable(enumirator_with_more_than_1_invalid_spotcheck,
                extensions = 'Buttons', 
                options = list(
                  scrollX = TRUE, 
                  pageLength = 20, 
                  
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = "Enumirator with more than 1 invalid spotcheck",
                      title = "Enumirator with more than 1 invalid spotcheck",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'excel',
                      filename = "Enumirator with more than 1 invalid spotcheck",
                      title = "Enumirator with more than 1 invalid spotcheck",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'pdf',
                      filename = "Enumirator with more than 1 invalid spotcheck",
                      title = "Enumirator with more than 1 invalid spotcheck",
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
                      filename = "Enumirator with more than 1 invalid spotcheck",
                      title = "Enumirator with more than 1 invalid spotcheck",
                      exportOptions = list(modifier = list(page = 'all'))
                    )
                    
                  )
                ))
     
    })
    
    #--------spotchck analysis.
    
    output$total_spotchecks_done <- renderUI({
      total_spotchecks_done <- main_spotcheck_survey
      
      nrow(total_spotchecks_done) 
      
    
    })
    
    output$total_valid_spotchecks_done <- renderUI({
      total_valid_spotchecks_done <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Valid"
          
        )
      
      nrow(total_valid_spotchecks_done)
      
      
    })
    
    output$total_invalid_spotchecks_done <- renderUI({
      total_invalid_spotchecks_done <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Invalid"
          
        )
      
      nrow(total_invalid_spotchecks_done)
      
      
    })
    
    output$total_mismatch_spotchecks_done <- renderUI({
      total_mismatch_spotchecks_done <- main_spotcheck_survey %>%
        filter(
          is.na(spotcheck_flag)
          
        )
      
      nrow(total_mismatch_spotchecks_done)
      
      
    })
    
   
    
    output$spotcheckAnalysisChart <- renderPlotly({
      total_spotchecks_done <- main_spotcheck_survey
      
      total_valid_spotchecks_done <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Valid"
          
        )
      total_invalid_spotchecks_done <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Invalid"
          
        )
      
      total_mismatch_spotchecks_done <- main_spotcheck_survey %>%
        filter(
          is.na(spotcheck_flag)
          
        )
      
      
      
      
      chartData <- data.frame(
        "name" = c("Total Spotchecks","Valid Spotchecks","Invalid Spotchecks","Mismatch Spotchecks"),
        "value" = c( nrow(total_spotchecks_done),nrow(total_valid_spotchecks_done),
                     nrow(total_invalid_spotchecks_done),nrow(total_mismatch_spotchecks_done)),
        "color" = c("#008BA8","#40419A","#ED7667","#F99D1E")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title = "value",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$spotcheckAnalysisTable <- renderDT(server = FALSE , {
      spotcheckAnalysisTable_baseline_data <- report_data
      
      total_spotchecks <- main_spotcheck_survey %>%
        group_by(community) %>%
        reframe(
          `Total Spotchecks` = n()
        )
      
      total_valid_spotchecks <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Valid"
          
        )%>%
        group_by(community) %>%
        reframe(
          `Total Valid Spotchecks` = n()
        )
      
      total_invalid_spotchecks <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Invalid"
          
        )%>%
        group_by(community) %>%
        reframe(
          `Total Invalid Spotchecks` = n()
        )
      
      total_match_spotchecks <- main_spotcheck_survey %>%
        filter(
          is.na(spotcheck_flag)
          
        )%>%
        group_by(community) %>%
        reframe(
          `Total Mismatch Spotchecks` = n()
        )
      
     
      
      spotcheckAnalysisTable_baseline_data %<>%
        left_join(total_spotchecks , by = "community") %>%
        replace_na(list(`Total Spotchecks` = 0)) %>%
        left_join(total_valid_spotchecks , by = "community") %>%
        replace_na(list(`Total Valid Spotchecks` = 0)) %>%
        left_join(total_invalid_spotchecks , by = "community") %>%
        replace_na(list(`Total Invalid Spotchecks` = 0)) %>%
        left_join(total_match_spotchecks , by = "community") %>%
        replace_na(list(`Total Mismatch Spotchecks` = 0)) %>%
        group_by(community) %>%
        reframe(
          region = first(region),
          district = first(district),
          member = first(member),
          `Total Spotchecks` = sum(`Total Spotchecks`,na.rm = TRUE),
          `Total Valid Spotchecks` = sum(`Total Valid Spotchecks`,na.rm = TRUE),
          `Total Invalid Spotchecks` = sum(`Total Invalid Spotchecks`,na.rm = TRUE),
          `Total Mismatch Spotchecks` = sum(`Total Mismatch Spotchecks`,na.rm = TRUE)
          
        )
      
      # print(spotcheckAnalysisTable_baseline_data)
      
      datatable(
        spotcheckAnalysisTable_baseline_data,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = input$Segrigation,
              title = input$Segrigation,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = input$Segrigation,
              title = input$Segrigation,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = input$Segrigation,
              title = input$Segrigation,
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
              filename = input$Segrigation,
              title = input$Segrigation,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
        
      
      
    })
    
    output$spotcheckAnalysisEnumiratorTable <- renderDT(server = FALSE , {
      spotcheckAnalysisTable_baseline_data_enum <- data_enumirators
     
      if(!is.null((input$Segrigation_element_drill))){
        if((input$Segrigation_element_drill) != "all" & !is.null((input$Segrigation))){
          if(input$Segrigation == "member"){
            spotcheckAnalysisTable_baseline_data_enum <- spotcheckAnalysisTable_baseline_data_enum %>% 
              filter(ngo == input$Segrigation_element_drill)
          }
        }
        
      }
      
      total_spotchecks_enum <- main_spotcheck_survey %>%
        group_by(!!sym(input$Segrigation),enumerator_name) %>%
        reframe(
          `Total Spotchecks` = n()
        )
      
      total_valid_spotchecks_enum <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Valid"
          
        )%>%
        group_by(!!sym(input$Segrigation),enumerator_name) %>%
        reframe(
          `Total Valid Spotchecks` = n()
        )
      
      total_invalid_spotchecks_enum <- main_spotcheck_survey %>%
        filter(
          spotcheck_flag == "Invalid"
          
        )%>%
        group_by(!!sym(input$Segrigation),enumerator_name) %>%
        reframe(
          `Total Invalid Spotchecks` = n()
        )
      
      total_mismtach_spotchecks_enum <- main_spotcheck_survey %>%
        filter(
          is.na(spotcheck_flag)
          
        )%>%
        group_by(!!sym(input$Segrigation),enumerator_name) %>%
        reframe(
          `Total Mismatch Spotchecks` = n()
        )
      
      
      
      total_spotchecks_enum %<>%
        replace_na(list(`Total Spotchecks` = 0)) %>%
        left_join(total_valid_spotchecks_enum ,  by = c(input$Segrigation,"enumerator_name")) %>%
        replace_na(list(`Total Valid Spotchecks` = 0)) %>%
        left_join(total_invalid_spotchecks_enum ,  by =c(input$Segrigation,"enumerator_name")) %>%
        replace_na(list(`Total Invalid Spotchecks` = 0))%>%
        left_join(total_mismtach_spotchecks_enum ,  by =c(input$Segrigation,"enumerator_name")) %>%
        replace_na(list(`Total Mismatch Spotchecks` = 0))
      # 
      spotcheckAnalysisTable_baseline_data_enum %<>%
        left_join(total_spotchecks_enum , by =  c("enumirator_id"="enumerator_name")) %>%
        replace_na(list(`Total Spotchecks` = 0,`Total Valid Spotchecks` = 0,`Total Invalid Spotchecks` = 0,`Total Mismatch Spotchecks` = 0))%>%
        select(-c("enumirator_id"))
     

      datatable(
        spotcheckAnalysisTable_baseline_data_enum,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = input$Segrigation,
              title = input$Segrigation,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = input$Segrigation,
              title = input$Segrigation,
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = input$Segrigation,
              title = input$Segrigation,
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
              filename = input$Segrigation,
              title = input$Segrigation,
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
      
      
      
    })
    
    
    
    output$invalidSpotcheckTable <- renderDT(server= FALSE,{
    
      invalidSpotcheckTable <- main_spotcheck_survey %>%
        filter(spotcheck_flag == "Invalid") %>%
        rename(HH_phone = contact2)%>%
        mutate(HouseholdSize,
               spotcheck_total_questions =  ifelse(is.na(spotcheck_total_questions) | spotcheck_total_questions =="" ,0,spotcheck_total_questions),
               spotcheck_mismatch_rate =  ifelse(is.na(spotcheck_mismatch_rate) | spotcheck_mismatch_rate =="" ,0,paste(spotcheck_mismatch_rate*100,"%",sep="")),
               spotcheck_flag =  ifelse(is.na(spotcheck_flag) | spotcheck_flag =="" ,"Mismatch",spotcheck_flag)) %>%
        select(region,district,member  , community , teamlead_name , enumerator_name,
               HH_Name,tab_date,HH_phone,
               
               HouseholdSize,
               spotcheck_mismatch_count,
               spotcheck_total_questions ,spotcheck_mismatch_rate ,spotcheck_flag
        )
        
        datatable(invalidSpotcheckTable,
                  extensions = 'Buttons', 
                  options = list(
                    scrollX = TRUE, 
                    pageLength = 20, 
                    
                    dom = 'lBfrtip',
                    buttons = list(
                      list(
                        extend = 'csv',
                        filename = "invalid Spotchecks",
                        title = "invalid Spotchecks",
                        exportOptions = list(modifier = list(page = 'all'))
                      ),
                      list(
                        extend = 'excel',
                        filename = "invalid Spotchecks",
                        title = "invalid Spotchecks",
                        exportOptions = list(modifier = list(page = 'all'))
                      ),
                      list(
                        extend = 'pdf',
                        filename = "invalid Spotchecks",
                        title = "invalid Spotchecks",
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
                        filename = "invalid Spotchecks",
                        title = "invalid Spotchecks",
                        exportOptions = list(modifier = list(page = 'all'))
                      )
                      
                    )
                  ))
      
      
    })
    
    output$mismatchSpotcheckTable <- renderDT(server= FALSE,{
      
      mismatchSpotcheckTable <- main_spotcheck_survey %>%
        filter(is.na(spotcheck_flag)) %>%
        rename(HH_phone = contact2)%>%
        mutate(HouseholdSize,
               spotcheck_total_questions =  ifelse(is.na(spotcheck_total_questions) | spotcheck_total_questions =="" ,0,spotcheck_total_questions),
               spotcheck_mismatch_rate =  ifelse(is.na(spotcheck_mismatch_rate) | spotcheck_mismatch_rate =="" ,0,paste(spotcheck_mismatch_rate*100,"%",sep="")),
               spotcheck_flag =  ifelse(is.na(spotcheck_flag) | spotcheck_flag =="" ,"Mismatch",spotcheck_flag)) %>%
        select(houseHoldIdentifier,region,district,member , community , teamlead_name , enumerator_name,
               HH_Name,tab_date,HH_phone,
               
               HouseholdSize,
               spotcheck_mismatch_count,
               spotcheck_total_questions ,spotcheck_mismatch_rate ,spotcheck_flag
        )
      
      datatable(mismatchSpotcheckTable,
                extensions = 'Buttons', 
                options = list(
                  scrollX = TRUE, 
                  pageLength = 20, 
                  
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = "mismatch Spotchecks",
                      title = "invalid Spotchecks",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'excel',
                      filename = "mismatch Spotchecks",
                      title = "invalid Spotchecks",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'pdf',
                      filename = "mismatch Spotchecks",
                      title = "invalid Spotchecks",
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
                      filename = "mismatch Spotchecks",
                      title = "mismatch Spotchecks",
                      exportOptions = list(modifier = list(page = 'all'))
                    )
                    
                  )
                ))
      
      
    })
    
   
    
    output$allSpotchecksDone <- renderDT(server = FALSE,{
      allSpotchecksDone <- main_spotcheck_survey %>%
        filter(
          !is.na(spotcheck_flag)
          
        ) %>%
        rename(HH_phone = contact2)%>%
        mutate(HouseholdSize,
               spotcheck_total_questions =  ifelse(is.na(spotcheck_total_questions) | spotcheck_total_questions =="" ,0,spotcheck_total_questions),
               spotcheck_mismatch_count =  ifelse(is.na(spotcheck_mismatch_count) | spotcheck_mismatch_count =="" ,0,spotcheck_mismatch_count),
               spotcheck_mismatch_rate =  ifelse(is.na(spotcheck_mismatch_rate) | spotcheck_mismatch_rate =="" ,0,paste(spotcheck_mismatch_rate*100,"%",sep = "")),
               spotcheck_flag =  ifelse(is.na(spotcheck_flag) | spotcheck_flag =="" ,"Mismatch",spotcheck_flag),
               consent = ifelse(consent == "1","Consented","Non-Consented")) %>%
        select(region,district,member  , community , teamlead_name , enumerator_name,
               HH_Name,tab_date,HH_phone,
               HouseholdSize,
               spotcheck_mismatch_count,
               
               spotcheck_total_questions ,spotcheck_mismatch_rate ,spotcheck_flag,
               consent
        )
      
      
      datatable(allSpotchecksDone,
                extensions = 'Buttons', 
                options = list(
                  scrollX = TRUE, 
                  pageLength = 20, 
                  
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = "All Spotchecks",
                      title = "All Spotchecks",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'excel',
                      filename = "All Spotchecks",
                      title = "All Spotchecks",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'pdf',
                      filename = "All Spotchecks",
                      title = "All Spotchecks",
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
                      filename = "All Spotchecks",
                      title = "All Spotchecks",
                      exportOptions = list(modifier = list(page = 'all'))
                    )
                    
                  )
                ))
    })
    
    
    #--------backcheck analysis
    
  
    output$dataToBeBackChecked <- renderDT(server = FALSE ,{
      dataToBeBackCheckedDT <- main_survey %>%
        filter((is.na(spotcheck_flag)) &
                 (duration_status == "Valid" | is.na(duration_status)) & consent == "1")%>%
        rename(HH_phone = contact2)%>%
        select(region,district,member  , community  , enumerator_name,
               hh_name,tab_date,HH_phone,
               TotalHHMembers
               
        )
      
      datatable(dataToBeBackCheckedDT,
                extensions = 'Buttons', 
                options = list(
                  scrollX = TRUE, 
                  pageLength = 20, 
                  
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = "Data to be back-checked",
                      title = "Data to be back-checked",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'excel',
                      filename = "Data to be back-checked",
                      title = "Data to be back-checked",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'pdf',
                      filename = "Data to be back-checked",
                      title = "Data to be back-checked",
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
                      filename = "Data to be back-checked",
                      title = "Data to be back-checked",
                      exportOptions = list(modifier = list(page = 'all'))
                    )
                    
                  )
                ))
    })
    
    output$allSurveyDataTable <- renderDT(server = FALSE , {



      allSurveyDataTableDT <- main_survey %>%
        rename(HH_phone = contact2)%>%
        mutate(
               interview_duration = paste(round(X_duration/60),"Minutes"),
               consent = ifelse(consent == "1","Consented","Non-Consented"),
           
               spotcheck_flag = ifelse(is.na(spotcheck_flag),"NA",spotcheck_flag)) %>%
        select(tab_date,region,district,member  , community  , enumerator_name,
               hh_name,hh_age,hh_sex,HH_phone,

               TotalHHMembers,
               spotcheck_mismatch_count,spotcheck_total_questions ,spotcheck_mismatch_rate ,spotcheck_flag,
         
               interview_duration,duration_status,consent
        )

      datatable(allSurveyDataTableDT,
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE, 
                  pageLength = 20,

                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = "Survey Data",
                      title = "Survey Data",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'excel',
                      filename = "Survey Data",
                      title = "Survey Data",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'pdf',
                      filename = "Survey Data",
                      title = "Survey Data",
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
                      filename = "Survey Data",
                      title = "Survey Data",
                      exportOptions = list(modifier = list(page = 'all'))
                    )

                  )
                ))

    })
    
    # output$downloadAllData <- downloadHandler(
    #   
    #   filename = function() {
    #     paste("all-data-", Sys.Date(), ".csv", sep="")
    #   },
    #   content = function(file) {
    #     write.csv(isolate(global_vars$all_survey_Data), file)
    #   }
    #   
    #   
    # )
    
    output$surveyMap <- renderLeaflet({
      
      somalia_districts_for_baseline <- somalia_districts
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Wajid"] <- "Waajid"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Elbarde"] <- "Cell_berde"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "rabdhure"] <- "Rab_Dhure"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Belet-Hawa"] <- "Belet_Hawa"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Wanlaweyn"] <- "Wanla_Weyn"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Dinsoor"] <- "Diinsoor"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Adaado"] <- "Adado"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Galkaayo"] <- "Galkacyo"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Dhusamareb"] <- "Dhuusamareeb"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Beledweyne"] <- "Belet_weyne"
      somalia_districts_for_baseline$District[somalia_districts_for_baseline$District == "Jowhar"] <- "Jawhar"
      
      
      
      Mapdata <- main_survey %>%
        filter(consent == "1")
      
      # latitude <- 42.375926
      # longitude <- -71.11489
      # 
      # # Create the Leaflet map
      # leaflet() %>%
      #   addTiles() %>% # Add default OpenStreetMap tiles
      #   addMarkers(lng=longitude, lat=latitude, popup="Here is the location!") %>%
      #   setView(lng=longitude, lat=latitude, zoom=16)
      
    
      netDistricts <- subset(somalia_districts_for_baseline, somalia_districts_for_baseline$District %in% 
                               Mapdata$district)
       
      Mapdata$latitude = as.numeric(sapply(strsplit(Mapdata$hh_geopoint, " "), `[`, 1))
      Mapdata$longitude = as.numeric(sapply(strsplit(Mapdata$hh_geopoint, " "), `[`, 2))
      
      # leaflet(data = data) %>% 
      #   addTiles() %>%  # Add default OpenStreetMap map tiles
      #   addCircleMarkers(~longitude, ~latitude, popup = ~as.character(hh_geopoint_manual), radius = 5)
      
      if(nrow(Mapdata) > 0){
        leaflet() %>%
          addTiles() %>%
          setView(lat=5.67, lng=46.189, zoom=6)%>%
          addPolygons(data = somalia,fillColor = "none") %>%
         
          addPolygons(data = netDistricts , fillColor  = "#ED7667",
                      weight = 0.5, smoothFactor = 0.5,
                      opacity = 0.6, fillOpacity = 0.5,
                      popup = paste(netDistricts$District ,  sep = "<br>",
                                    netDistricts$admin1Name))%>%
          addCircleMarkers(data = Mapdata,~longitude, ~latitude,  radius = 5 ,
                           popup = ~as.character(paste(
                             paste("hh name:",hh_name),
                             paste("district:",district),
                             paste("region:",region),
                           
                             paste("member:",member),
                             paste("hh phone:",contact2),
                           
                             sep = "<br>")),clusterOptions = markerClusterOptions())%>%
          addMarkers(data = Mapdata,~longitude, ~latitude, popup = ~as.character(paste(
            paste("hh name:",hh_name),
            paste("district:",district),
            paste("region:",region),
           
            paste("member:",member),
            paste("hh phone:",contact2),
           
            sep = "<br>")) ,
            
            clusterOptions = markerClusterOptions())
      }else{
        leaflet() %>%
          addTiles() %>%
          setView(lat=5.67, lng=46.189, zoom=6)%>%
          addPolygons(data = somalia,fillColor = "none") %>%
          addPolygons(data = somalia_districts , fillColor  = "white",
                      weight = 0.5, smoothFactor = 0.5,
                      opacity = 0.3, fillOpacity = 1,
                      popup = paste(somalia_districts$District ,  sep = "<br>",
                                    somalia_districts$admin1Name)) %>%
          addControl(html = "<div style='background-color: white; padding: 5px; color=#008BA8'>No Data is available</div>",
                     position = "topright" # You can change the position: topright, topleft, bottomright, bottomleft
          )
      }
      
    })
    
  })
#   # shinyjs::runjs('alert("Button clicked in module!");')
#   observeEvent(input$removeSolvedShortPeriod, {
#     ns1 <- NS("brcisBaseline")
#     shinyjs::runjs('
#       var ID_checked = [];
#       var start_time_checked = [];
#       var hh_name_checked = [];
#       var hh_phone_checked = [];
#       $(".row-checkbox:checked").each(function() {
#         ID_checked.push($(this).val());
#         start_time_checked.push(this.attr("start_time"));
#         hh_name_checked.push(this.attr("hh_name"));
#         hh_phone_checked.push(this.attr("hh_phone"));
#         
#       });
#      
#       
#       var checked = {
#         hh_id :ID_checked,
#         start_time :start_time_checked,
#         hh_name :hh_name_checked,
#         hh_phone : hh_phone_checked
#       }
#       
#       Shiny.setInputValue("brcisBaseline-checked_rows", checked);
#     ')
#     
#     showModal(modalDialog(
#       title = "Removed Duplicates",
#       tags$div(
#         tagList(
#           uiOutput(ns1("removeMessage")),
#           textInput(
#             ns1("adminUser"),
#             "enter User name"
#           ),
#           passwordInput(
#             ns1("adminPass"),
#             "enter password"
#           ),
#           actionButton(ns1("removeSubmit"),"submit")
#         )
#       )
#     ))
#     
#     
#   })
#   
#   observeEvent(input$removeSubmit ,{
#     adminUser <- input$adminUser
#     adminPass <- input$adminPass
#     
#     if((adminUser == "" | is.na(adminUser)) | (adminPass == "" | is.na(adminPass))){
#       output$removeMessage <- renderUI({
#         
#         HTML(
#           '<div class="alert alert-warning border-0 bg-warning alert-dismissible fade show py-2">
# 									<div class="d-flex align-items-center">
# 										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
# 										</div>
# 										<div class="ms-3">
# 											<h6 class="mb-0 text-dark">Warning</h6>
# 											<div class="text-dark">Both user and password required!</div>
# 										</div>
# 									</div>
# 									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
# 								</div>'
#         )
#       })
#       return()
#     }
#     
#     if(adminUser == "abdalla" & adminPass =="888"){
#       
#       if(is.null(input$checked_rows) | is.na(is.null(input$checked_rows))){
#         
#         HTML(
#           '<div class="alert alert-warning border-0 bg-warning alert-dismissible fade show py-2">
# 									<div class="d-flex align-items-center">
# 										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
# 										</div>
# 										<div class="ms-3">
# 											<h6 class="mb-0 text-dark">No Data is selected</h6>
# 											<div class="text-dark">Please Select Records to remove</div>
# 										</div>
# 									</div>
# 									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
# 								</div>'
#         )
#       }else{
#         output$removeMessage <- renderUI({
#           HTML(
#             '
#           <span style="color:green">Data removed</span>
#           '
#           )
#         })
#       }
#      
#       print(input$checked_rows)
#     }else{
#       output$removeMessage <- renderUI({
#         
#         
#         HTML(
#           '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
# 									<div class="d-flex align-items-center">
# 										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
# 										</div>
# 										<div class="ms-3">
# 											<h6 class="mb-0 text-dark">Warning</h6>
# 											<div class="text-dark">Incorrect User or Password!</div>
# 										</div>
# 									</div>
# 									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
# 								</div>'
#         )
#       })
#       
#     }
#   })
  
  
}

