

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
  ## covert to dataframe
  
  
}




communities <- readxl::read_excel("docs/arc-d/oasis_comunity_member_district_region.xlsx")%>%mutate(member =str_to_lower(member))
risks <- readxl::read_excel("docs/arc-d/risk_scenario.xlsx")
arcd_questions <- readxl::read_excel("docs/arc-d/arcd_question.xlsx",sheet = "questions") %>%
  mutate(
    question = str_to_lower(question),# Preserve the original question name
    
  )

arcd_systems <- readxl::read_excel("docs/arc-d/arcd_question.xlsx",sheet = "systems")
arcd_thematic <- readxl::read_excel("docs/arc-d/arcd_question.xlsx",sheet = "thematic")




oasis_arcdUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/oasis_arcd.html",
                 
                 warningToLoadDATA = uiOutput(ns("warningToLoadDATA")),
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
                
                 memberController = selectInput(
                   ns("memberController"),
                   "Member",
                   choices = c("all",unique(communities$member))
                 ),
                 
                 
                 regionController = uiOutput(ns("regionControllerFill")),
                 districtController = uiOutput(ns("districtControllerFill")),
                 
                 communityController = uiOutput(ns("communityControllerFill")),
                 riskController = uiOutput(ns("riskControllerFill")),
                 arcDPhaseController = uiOutput(ns("arcDPhaseController")),
                 
                 total_population = uiOutput(ns("total_population")),
                 total_communities = uiOutput(ns("total_communities")),
                 total_households = uiOutput(ns("total_households")),
                 total_male = uiOutput(ns("total_male")),
                 total_female = uiOutput(ns("total_female")),
                 populationGroupChart = plotlyOutput(ns("populationGroupChart"),height ="430")%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 livelihood_zoneChart =  plotlyOutput(ns("livelihood_zoneChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 education_levelChart =  plotlyOutput(ns("education_levelChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 local_community_groupsChart =  plotlyOutput(ns("local_community_groupsChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 external_org_typeChart =  plotlyOutput(ns("external_org_typeChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),

                 check_list_of_available_plans =plotlyOutput(ns("check_list_of_available_plans"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 vul_groups =plotlyOutput(ns("vul_groups"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 shock_list1 =plotlyOutput(ns("shock_list1"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 stress2 =plotlyOutput(ns("stress2"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 most_devastating_shock =plotlyOutput(ns("most_devastating_shock"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 spiderChart = plotlyOutput(ns("spiderChart"),height ="800",width ="900")%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 spiderChartTable = DTOutput(ns("spiderChartTable")),
                 theRiskScenarioValue= uiOutput(ns("theRiskScenarioValue")),
                 theRiskScenarioType =uiOutput(ns("theRiskScenarioType")),
                 communityName =uiOutput(ns("communityName")),
                 CommunityDistrict =uiOutput(ns("CommunityDistrict")),
                 
                 theRiskScenarioValue2= uiOutput(ns("theRiskScenarioValue2")),
                 theRiskScenarioType2 =uiOutput(ns("theRiskScenarioType2")),
                 communityName2 =uiOutput(ns("communityName2")),
                 CommunityDistrict2 =uiOutput(ns("CommunityDistrict2")),
                 componentScoresTables = DTOutput(ns("componentScoresTables"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 componentScoresChart = plotlyOutput(ns("componentScoresChart"),height = 500)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 thematicScoreTable =DTOutput(ns("thematicScoreTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 thematicScorePercentageComparisonTable=DTOutput(ns("thematicScorePercentageComparisonTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 thematicScoreChart = plotlyOutput(ns("thematicScoreChart"),height = 700)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 
                 theRiskScenarioValue3= uiOutput(ns("theRiskScenarioValue3")),
                 theRiskScenarioType3 =uiOutput(ns("theRiskScenarioType3")),
                 communityName3 =uiOutput(ns("communityName3")),
                 CommunityDistrict3 =uiOutput(ns("CommunityDistrict3")),
                 
                 systemScoreTable =DTOutput(ns("systemScoreTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 systemScorePercentageComparisonTable = DTOutput(ns("systemScorePercentageComparisonTable"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 systemScoreChart =plotlyOutput(ns("systemScoreChart"),height = 700)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 componentScoresBarChart = plotlyOutput(ns("componentScoresBarChart"),height = 800)%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 communityScoresChart = DTOutput(ns("communityScoresChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 communityScoresComparison  = DTOutput(ns("communityScoresComparison"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 memberCommunityReport = DTOutput(ns("memberCommunityReport"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 memberCommunitySummary = DTOutput(ns("memberCommunitySummary"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7)
    )
  )
}

oasis_arcd <- function(input ,output , session,sharedValues){
  ns1 <- NS("oasis_arcd")
  
  
  global_vars <- reactiveValues(
    global_arcd_survey_part_a = NULL,
    global_arcd_survey_part_b = NULL
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
    
    Authresult <- validate_member_ona_credentials(onauser,onapass,"811426")
    
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
    
    baseline_survey  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","811426")

    if(first(baseline_survey$status) == "200"  & first(baseline_survey$message) != "empty"){
 
   
     
      
      baseline_survey %<>%
        mutate(
        Member_id = member,
        Region_id = region,
        District_id = district,
        Community_id = community,
        Member_id =str_to_lower(Member_id),
        Member_id = case_when(
          Member_id == "cww" ~ "concern",
          .default = Member_id
        ),
        Region_id = case_when(
          Region_id == "Galgaduud" ~ "Galgaduud",
          Region_id == "Mudug" ~ "Mudug",
          Region_id == "Lower Shabelle" ~ "Lower_Shebelle",
          Region_id == "Gedo" ~ "Gedo",
          Region_id == "Bay" ~ "Bay",
          Region_id == "Lower Juba" ~ "Lower_Juba",
          Region_id == "Hiran" ~ "Hiran",
          Region_id == "Middle shabelle" ~ "Middle_Shabelle",
          Region_id == "Bakool" ~ "Bakool",
          .default = Region_id
        ),
        District_id = case_when(
          District_id == "Cadaado" ~ "Adado",
          District_id == "Galdogob" ~ "Galdogob",
          District_id == "Jariiban" ~ "Jariiban",
          District_id == "Galkaio" ~ "Galkacyo",
          District_id == "Wanlaweyn" ~ "Wanla_Weyn",
          District_id == "Afgoye" ~ "Afgoye",
          District_id == "Belet Hawa" ~ "Belet_Hawa",
          District_id == "Baidoa" ~ "Baidoa",
          District_id == "Afmadow" ~ "Afmadow",
          District_id == "Bardhere" ~ "Baardhere",
          District_id == "Dhusamareb" ~ "Dhusamareeb",
          District_id == "Beletweyne" ~ "Belet_weyne",
          District_id == "Dinsoor" ~ "Diinsoor",
          District_id == "Jowhar" ~ "Jawhar",
          District_id == "Galkaayo" ~ "Galkacyo",
          District_id == "Elbarde" ~ "Cell_berde",
          District_id == "Rabdhure" ~ "Rab_Dhure",
          District_id == "Wajid" ~ "Waajid",
          District_id == "Beletweyne (Mataban)" ~ "Belet_weyne",
          District_id == "Hudur" ~ "Hudur",
         
          .default = District_id
        ),
        Community_id = case_when(
          Community_id == "Riig-Oomane" ~ "Riig_Oomane",
          Community_id == "Jariban Town" ~ "Jariban_Town",
          Community_id == "Shimbiroole East" ~ "Shimbiroole_East",
          Community_id == "Qurunlow-Asharaafed" ~ "Qurunlow_Asharaafed",
          Community_id == "Geedda barkaan" ~ "Geedda_barkaan",
          Community_id == "Madaxwarabe" ~ "Madax_Waraabe",
          Community_id == "Yaq-Bashir" ~ "Yaq_Bashir",
          .default = Community_id
        ),
        risk_scenario1 = case_when(
          Community_id == 'Madax_Waraabe' & Member_id == 'acf' ~ "Drought",
          Community_id == 'Ondare' & Member_id == 'acf' ~ "Drought",
          Community_id == 'Qurunlow_Asharaafed' & Member_id == 'gredo' ~ "Drought",
          Community_id == 'Baxdo' & Member_id == 'irc' ~ "Drought",
          Community_id == 'Bursaalax' & Member_id == 'irc' ~ "Drought",
          Community_id == 'DegAdey' & Member_id == 'nrc' ~ "Drought",
          Community_id == 'Yontoy' & Member_id == 'nrc' ~ "Drought",
          Community_id == "Towfiiq"  & Member_id == 'kaalo' ~ "Drought",
          Community_id == "Malaasle"  & Member_id == 'kaalo' ~ "Drought",
          .default = risk_scenario1
        ),
        risk_scenario2 = case_when(
          Community_id == 'DegAdey' & Member_id == 'nrc' ~ "Economic_or_market_crisis",
          Community_id == 'Ramadhawi' & Member_id == 'gredo' ~ "Animal_disease_epidemic",
          Community_id == 'Yaqshid' & Member_id == 'gredo' ~ "Animal_disease_epidemic",
          Community_id == "Bula_Guduud"  & Member_id == 'nrc' ~ "Human_disease_epidemic",
          Community_id == "Moyko"  & Member_id == 'sci' ~ "Drought",
          Community_id == "Xanshoolay"  & Member_id == 'sci' ~ "Drought",
          Community_id == "Xanshoolay"  & Member_id == 'sci' ~ "Drought",
          Community_id == "Ceelbardaale"  & Member_id == 'kaalo' ~ "Inter_or_intra_communal_conflict_(cattle_rustling_gang_violence_disputes_over_natural_resources_etc)",
          Community_id == "Towfiiq"  & Member_id == 'kaalo' ~ "Inter_or_intra_communal_conflict_(cattle_rustling_gang_violence_disputes_over_natural_resources_etc)",
          Community_id == 'Beergadid' & Member_id == 'sci' ~ "Inter_or_intra_communal_conflict_(cattle_rustling_gang_violence_disputes_over_natural_resources_etc)",
          .default = risk_scenario2
        )
      )%>%
        mutate(
          tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
          
        )
      
      # write.xlsx(main_survey_part_a, "main_survey_part_a3333.xlsx")
      
      # baseline_survey%>%select(member,Member_id , region , Region_id , district , District_id , community,Community_id)
      
    
      
      main_survey_part_a <- baseline_survey%>%
        select(Member_id,tab_date,Region_id,District_id,Community_id,
          total_number_of_households,
          total_number_of_households,
          total_no_of_population,
          girls_younger_than_18_years,
          boys_younger_than_18_years,
          women_aged_18.60_years,
          men_aged_18.60_years,
          women_older_than_60_years,
          men_older_than_60_years,
          total_number_of_populations_check,
          livelihood_zone,
          education_level,
          local_community_groups,
          external_org_type,
          available_plans,
          vul_groups,
          shock_list1,
          stress2,
          most_devastating_shock
        )
      
      # main_survey_part_b %>% group_by(Member_id,Region_id,District_id,Community_id,risk_scenario,risk_scenario_value,question,category) %>%
      #   summarise(n=n())%>%
      #   filter(n>1)%>%
      #   ungroup()
      # 
      # main_survey_part_b%>%filter(Member_id == 'ACF',Region_id=='Bakool',District_id=='Elbarde',Community_id=='Ondare',
      #                             risk_scenario=='risk_scenrio_1',risk_scenario_value=='Drought Economic_or_market_crisis')%>%
      #   select(original_question,question_underscore,question,category)
      
     
      # write.xlsx(main_survey_part_a, "main_survey_part_a.xlsx")
      
      main_survey_part_b <- baseline_survey %>%
        select(Member_id,tab_date,X_submission_time,Region_id,District_id,Community_id,risk_scenario1,risk_scenario2,
          DR_level_menFGD,
          DR_level_OBSERVATION_MenFGD,
          DR_level_womenFGD,
          DR_level_OBSERVATION_womenFGD,
          DR_level,
          DR_level_OBSERVATION,
          scientific_risk_assessment_menFGD,
          scientific_risk_assessment_OBSERVATION_menFGD,
          scientific_risk_assessment_womenFGD,
          scientific_risk_assessment_OBSERVATIONwomenFGD,
          scientific_risk_assessment,
          scientific_risk_assessment_OBSERVATION,
          info_dissemination_menFGD,
          info_dissemination_OBSERVATION_menFGD,
          info_dissemination_womenFGD,
          info_dissemination_OBSERVATION_womenFGD,
          info_dissemination,
          info_dissemination_OBSERVATION,
          education_of_children_in_DDR_menFDG,
          education_of_children_in_DDR_OBSERVATION_menFGD,
          education_of_children_in_DDR_womenFGD,
          education_of_children_in_DDR_OBSERVATION_womenFGD,
          education_of_children_in_DDR,
          education_of_children_in_DDR_OBSERVATION,
          strengthening_govern_menFGD,
          strengthening_govern_OBSERVATION_menFGD,
          strengthening_govern_womenFGD,
          strengthening_govern_OBSERVATION_womenFGD,
          strengthening_govern,
          strengthening_govern_OBSERVATION,
          land_use_planning_menFGD,
          land_use_planning_OBSERVATION_menFGD,
          land_use_planning_womenFGD,
          land_use_planning_OBSERVATION_womenFGD,
          land_use_planning,
          land_use_planning_OBSERVATION,
          com_decision_making_menFGD,
          com_decision_making_OBSERVATION_menFGD,
          com_decision_making_womenFGD,
          com_decision_making_OBSERVATION_womenFGD,
          com_decision_making,
          com_decision_making_OBSERVATION,
          inclusion_of_vul_groups_menFGD,
          inclusion_of_vul_groups_OBSERVATION_menFGD,
          inclusion_of_vul_groups_womenFGD,
          inclusion_of_vul_groups_OBSERVATION_womenFGD,
          inclusion_of_vul_groups,
          inclusion_of_vul_groups_OBSERVATION,
          women_participation_menFGD,
          women_participation_OBSERVATION_menFGD,
          women_participation_womenFGD,
          women_participation_OBSERVATION_womenFGD,
          women_participation,
          women_participation_OBSERVATION,
          rights_awareness_menFGD,
          rights_awareness_OBSERVATION_menFGD,
          rights_awareness_womenFGD,
          rights_awareness_OBSERVATION_womenFGD,
          rights_awareness,
          rights_awareness_OBSERVATION,
          partnership_for_DDR_and_recovery_menFGD,
          partnership_for_DDR_and_recovery_OBSERVATION_menFGD,
          partnership_for_DDR_and_recovery_womenFGD,
          partnership_for_DDR_and_recovery_OBSERVATION_womenFGD,
          partnership_for_DDR_and_recovery,
          partnership_for_DDR_and_recovery_OBSERVATION,
          sus_environment_mgt_menFGD,
          sus_environment_mgt_OBSERVATION_menFGD,
          sus_environment_mgt_womenFGD,
          sus_environment_mgt_OBSERVATION_womenFGD,
          sus_environment_mgt,
          sus_environment_mgt_OBSERVATION,
          water_security_mgt_menFGD,
          water_security_mgt_OBSERVATION_menFGD,
          water_security_mgt_womenFGD,
          water_security_mgt_OBSERVATION_womenFGD,
          water_security_mgt,
          water_security_mgt_OBSERVATION,
          health_access_awareness_menFGD,
          health_access_awareness_OBSERVATION_menFGD,
          health_access_awareness_womenFGD,
          health_access_awareness_OBSERVATION_womenFGD,
          health_access_awareness,
          health_access_awareness_OBSERVATION,
          secure_and_nutritious_food_supply_menFGD,
          secure_and_nutritious_food_supply_OBSERVATION_menFGD,
          secure_and_nutritious_food_supply_womenFGD,
          secure_and_nutritious_food_supply_OBSERVATION_womenFGD,
          secure_and_nutritious_food_supply,
          secure_and_nutritious_food_supply_OBSERVATION,
          hazard_resistant_lvlh_practices_menFGD,
          hazard_resistant_lvlh_practices_OBSERVATION_menFGD,
          hazard_resistant_lvlh_practices_womenFGD,
          hazard_resistant_lvlh_practices_OBSERVATION_womenFGD,
          hazard_resistant_lvlh_practices,
          hazard_resistant_lvlh_practices_OBSERVATION,
          access_to_market_menFGD,
          access_to_market_OBSERVATION_menFGD,
          access_to_market_womenFGD,
          access_to_market_OBSERVATION_womenFGD,
          access_to_market,
          access_to_market_OBSERVATION,
          access_to_financial_services_menFGD,
          access_to_financial_services_OBSERVATION_menFGD,
          access_to_financial_services_womenFGD,
          access_to_financial_services_OBSERVATION_womenFGD,
          access_to_financial_services,
          access_to_financial_services_OBSERVATION,
          income_and_asset_protection_menFGD,
          income_and_asset_protection_OBSERVATION_menFGD,
          income_and_asset_protection_womenFGD,
          income_and_asset_protection_OBSERVATION_womenFGD,
          income_and_asset_protection,
          income_and_asset_protection_OBSERVATION,
          soc_protection_menFGD,
          soc_protection_OBSERVATION_menFGD,
          soc_protection_womenFGD,
          soc_protection_OBSERVATION_womenFGD,
          soc_protection,
          soc_protection_OBSERVATION,
          social_cohesion_conflict_prevention_menFGD,
          social_cohesion_conflict_prevention_OBSERVATION_menFGD,
          social_cohesion_conflict_prevention_womenFGD,
          social_cohesion_conflict_prevention_OBSERVATION_womenFGD,
          social_cohesion_conflict_prevention,
          social_cohesion_conflict_prevention_OBSERVATION,
          critical_infrastructure_menFGD,
          critical_infrastructure_OBSERVATION_menFGD,
          critical_infrastructure_womenFGD,
          critical_infrastructure_OBSERVATION_womenFGD,
          critical_infrastructure,
          critical_infrastructure_OBSERVATION,
          comm_housing_menFGD,
          comm_housing_OBSERVATION_menFGD,
          comm_housing_womenFGD,
          comm_housing_OBSERVATION_womenFGD,
          comm_housing,
          comm_housing_OBSERVATION,
          contingency_and_recov_planning_menFGD,
          contingency_and_recov_planning_OBSERVATION_menFGD,
          contingency_and_recov_planning_womenFGD,
          contingency_and_recov_planning_OBSERVATION_womenFGD,
          contingency_and_recov_planning,
          contingency_and_recov_planning_OBSERVATION,
          early_warning_system_menFGD,
          early_warning_system_OBSERVATION_menFGD,
          early_warning_system_womenFGD,
          early_warning_system_OBSERVATION_womenFGD,
          early_warning_system,
          early_warning_system_OBSERVATION,
          capacities_in_preparedness_and_response_menFGD,
          capacities_in_preparedness_and_response_OBSERVATION_menFGD,
          capacities_in_preparedness_and_response_womenFGD,
          capacities_in_preparedness_and_response_OBSERVATION_womenFGD,
          capacities_in_preparedness_and_response,
          capacities_in_preparedness_and_response_OBSERVATION,
          health_services_in_emergencies_menFGD,
          health_services_in_emergencies_OBSERVATION_menFGD,
          health_services_in_emergencies_womenFGD,
          health_services_in_emergencies_OBSERVATION_womenFGD,
          health_services_in_emergencies,
          health_services_in_emergencies_OBSERVATION,
          education_services_in_emergencies_menFGD,
          education_services_in_emergencies_OBSERVATION_menFGD,
          education_services_in_emergencies_womenFGD,
          education_services_in_emergencies_OBSERVATION_womenFGD,
          education_services_in_emergencies,
          education_services_in_emergencies_OBSERVATION,
          emergency_infras_menFGD,
          emergency_infras_OBSERVATION_menFGD,
          emergency_infras_womenFGD,
          emergency_infras_OBSERVATION_womenFGD,
          emergency_infras,
          emergency_infras_OBSERVATION,
          leadership_and_volunteerism_menFGD,
          leadership_and_volunteerism_OBSERVATION_menFGD,
          leadership_and_volunteerism_womenFGD,
          leadership_and_volunteerism_OBSERVATION_womenFGD,
          leadership_and_volunteerism,
          leadership_and_volunteerism_OBSERVATION,
          DR_level_menFGD_2,
          DR_level_OBSERVATION_MenFGD_2,
          DR_level_womenFGD_2,
          DR_level_OBSERVATION_womenFGD_2,
          DR_level_2,
          DR_level_OBSERVATION_2,
          scientific_risk_assessment_menFGD_2,
          scientific_risk_assessment_OBSERVATION_menFGD_2,
          scientific_risk_assessment_womenFGD_2,
          scientific_risk_assessment_OBSERVATIONwomenFGD_2,
          scientific_risk_assessment_2,
          scientific_risk_assessment_OBSERVATION_2,
          info_dissemination_menFGD_2,
          info_dissemination_OBSERVATION_menFGD_2,
          info_dissemination_womenFGD_2,
          info_dissemination_OBSERVATION_womenFGD_2,
          info_dissemination_2,
          info_dissemination_OBSERVATION_2,
          education_of_children_in_DDR_menFDG_2,
          education_of_children_in_DDR_OBSERVATION_menFGD_2,
          education_of_children_in_DDR_womenFGD_2,
          education_of_children_in_DDR_OBSERVATION_womenFGD_2,
          education_of_children_in_DDR_2,
          education_of_children_in_DDR_OBSERVATION_2,
          strengthening_govern_menFGD_2,
          strengthening_govern_OBSERVATION_menFGD_2,
          strengthening_govern_womenFGD_2,
          strengthening_govern_OBSERVATION_womenFGD_2,
          strengthening_govern_2,
          strengthening_govern_OBSERVATION_2,
          land_use_planning_menFGD_2,
          land_use_planning_OBSERVATION_menFGD_2,
          land_use_planning_womenFGD_2,
          land_use_planning_OBSERVATION_womenFGD_2,
          land_use_planning_2,
          land_use_planning_OBSERVATION_2,
          com_decision_making_menFGD_2,
          com_decision_making_OBSERVATION_menFGD_2,
          com_decision_making_womenFGD_2,
          com_decision_making_OBSERVATION_womenFGD_2,
          com_decision_making_2,
          com_decision_making_OBSERVATION_2,
          inclusion_of_vul_groups_menFGD_2,
          inclusion_of_vul_groups_OBSERVATION_menFGD_2,
          inclusion_of_vul_groups_womenFGD_2,
          inclusion_of_vul_groups_OBSERVATION_womenFGD_2,
          inclusion_of_vul_groups_2,
          inclusion_of_vul_groups_OBSERVATION_2,
          women_participation_menFGD_2,
          women_participation_OBSERVATION_menFGD_2,
          women_participation_womenFGD_2,
          women_participation_OBSERVATION_womenFGD_2,
          women_participation_2,
          women_participation_OBSERVATION_2,
          rights_awareness_menFGD_2,
          rights_awareness_OBSERVATION_menFGD_2,
          rights_awareness_womenFGD_2,
          rights_awareness_OBSERVATION_womenFGD_2,
          rights_awareness_2,
          rights_awareness_OBSERVATION_2,
          partnership_for_DDR_and_recovery_menFGD_2,
          partnership_for_DDR_and_recovery_OBSERVATION_menFGD_2,
          partnership_for_DDR_and_recovery_womenFGD_2,
          partnership_for_DDR_and_recovery_OBSERVATION_womenFGD_2,
          partnership_for_DDR_and_recovery_2,
          partnership_for_DDR_and_recovery_OBSERVATION_2,
          sus_environment_mgt_menFGD_2,
          sus_environment_mgt_OBSERVATION_menFGD_2,
          sus_environment_mgt_womenFGD_2,
          sus_environment_mgt_OBSERVATION_womenFGD_2,
          sus_environment_mgt_2,
          sus_environment_mgt_OBSERVATION_2,
          water_security_mgt_menFGD_2,
          water_security_mgt_OBSERVATION_menFGD_2,
          water_security_mgt_womenFGD_2,
          water_security_mgt_OBSERVATION_womenFGD_2,
          water_security_mgt_2,
          water_security_mgt_OBSERVATION_2,
          health_access_awareness_menFGD_2,
          health_access_awareness_OBSERVATION_menFGD_2,
          health_access_awareness_womenFGD_2,
          health_access_awareness_OBSERVATION_womenFGD_2,
          health_access_awareness_2,
          health_access_awareness_OBSERVATION_2,
          secure_and_nutritious_food_supply_menFGD_2,
          secure_and_nutritious_food_supply_OBSERVATION_menFGD_2,
          secure_and_nutritious_food_supply_womenFGD_2,
          secure_and_nutritious_food_supply_OBSERVATION_womenFGD_2,
          secure_and_nutritious_food_supply_2,
          secure_and_nutritious_food_supply_OBSERVATION_2,
          hazard_resistant_lvlh_practices_menFGD_2,
          hazard_resistant_lvlh_practices_OBSERVATION_menFGD_2,
          hazard_resistant_lvlh_practices_womenFGD_2,
          hazard_resistant_lvlh_practices_OBSERVATION_womenFGD_2,
          hazard_resistant_lvlh_practices_2,
          hazard_resistant_lvlh_practices_OBSERVATION_2,
          access_to_market_menFGD_2,
          access_to_market_OBSERVATION_menFGD_2,
          access_to_market_womenFGD_2,
          access_to_market_OBSERVATION_womenFGD_2,
          access_to_market_2,
          access_to_market_OBSERVATION_2,
          access_to_financial_services_menFGD_2,
          access_to_financial_services_OBSERVATION_menFGD_2,
          access_to_financial_services_womenFGD_2,
          access_to_financial_services_OBSERVATION_womenFGD_2,
          access_to_financial_services_2,
          access_to_financial_services_OBSERVATION_2,
          income_and_asset_protection_menFGD_2,
          income_and_asset_protection_OBSERVATION_menFGD_2,
          income_and_asset_protection_womenFGD_2,
          income_and_asset_protection_OBSERVATION_womenFGD_2,
          income_and_asset_protection_2,
          income_and_asset_protection_OBSERVATION_2,
          soc_protection_menFGD_2,
          soc_protection_OBSERVATION_menFGD_2,
          soc_protection_womenFGD_2,
          soc_protection_OBSERVATION_womenFGD_2,
          soc_protection_2,
          soc_protection_OBSERVATION_2,
          social_cohesion_conflict_prevention_menFGD_2,
          social_cohesion_conflict_prevention_OBSERVATION_menFGD_2,
          social_cohesion_conflict_prevention_womenFGD_2,
          social_cohesion_conflict_prevention_OBSERVATION_womenFGD_2,
          social_cohesion_conflict_prevention_2,
          social_cohesion_conflict_prevention_OBSERVATION_2,
          critical_infrastructure_menFGD_2,
          critical_infrastructure_OBSERVATION_menFGD_2,
          critical_infrastructure_womenFGD_2,
          critical_infrastructure_OBSERVATION_womenFGD_2,
          critical_infrastructure_2,
          critical_infrastructure_OBSERVATION_2,
          comm_housing_menFGD_2,
          comm_housing_OBSERVATION_menFGD_2,
          comm_housing_womenFGD_2,
          comm_housing_OBSERVATION_womenFGD_2,
          comm_housing_2,
          comm_housing_OBSERVATION_2,
          contingency_and_recov_planning_menFGD_2,
          contingency_and_recov_planning_OBSERVATION_menFGD_2,
          contingency_and_recov_planning_womenFGD_2,
          contingency_and_recov_planning_OBSERVATION_womenFGD_2,
          contingency_and_recov_planning_2,
          contingency_and_recov_planning_OBSERVATION_2,
          early_warning_system_menFGD_2,
          early_warning_system_OBSERVATION_menFGD_2,
          early_warning_system_womenFGD_2,
          early_warning_system_OBSERVATION_womenFGD_2,
          early_warning_system_2,
          early_warning_system_OBSERVATION_2,
          capacities_in_preparedness_and_response_menFGD_2,
          capacities_in_preparedness_and_response_OBSERVATION_menFGD_2,
          capacities_in_preparedness_and_response_womenFGD_2,
          capacities_in_preparedness_and_response_OBSERVATION_womenFGD_2,
          capacities_in_preparedness_and_response_2,
          capacities_in_preparedness_and_response_OBSERVATION_2,
          health_services_in_emergencies_menFGD_2,
          health_services_in_emergencies_OBSERVATION_menFGD_2,
          health_services_in_emergencies_womenFGD_2,
          health_services_in_emergencies_OBSERVATION_womenFGD_2,
          health_services_in_emergencies_2,
          health_services_in_emergencies_OBSERVATION_2,
          education_services_in_emergencies_menFGD_2,
          education_services_in_emergencies_OBSERVATION_menFGD_2,
          education_services_in_emergencies_womenFGD_2,
          education_services_in_emergencies_OBSERVATION_womenFGD_2,
          education_services_in_emergencies_2,
          education_services_in_emergencies_OBSERVATION_2,
          emergency_infras_menFGD_2,
          emergency_infras_OBSERVATION_menFGD_2,
          emergency_infras_womenFGD_2,
          emergency_infras_OBSERVATION_womenFGD_2,
          emergency_infras_2,
          emergency_infras_OBSERVATION_2,
          leadership_and_volunteerism_menFGD_2,
          leadership_and_volunteerism_OBSERVATION_menFGD_2,
          leadership_and_volunteerism_womenFGD_2,
          leadership_and_volunteerism_OBSERVATION_womenFGD_2,
          leadership_and_volunteerism_2,
          leadership_and_volunteerism_OBSERVATION_2
        )%>% dplyr::rename(scientific_risk_assessment_OBSERVATION_womenFGD = scientific_risk_assessment_OBSERVATIONwomenFGD,
                           scientific_risk_assessment_OBSERVATION_womenFGD_2 = scientific_risk_assessment_OBSERVATIONwomenFGD_2)%>%
        pivot_longer(cols = DR_level_menFGD:leadership_and_volunteerism_OBSERVATION_2,names_to = "question",values_to = "scores")%>%
        mutate(
          risk_scenario = case_when(
            str_ends(question, "_2") ~ "risk_scenrio_2",
            TRUE ~ "risk_scenrio_1"
          ),
          risk_scenario_value = case_when(
            str_ends(question, "_2") ~ risk_scenario2,
            TRUE ~ risk_scenario1
          ),
          # question = str_replace(question, "_2", ""),
          question = str_to_lower(question),# Preserve the original question name
          original_question = question,
        
          # Remove specific tags to unify question names
          category = case_when(
            str_detect(question, "observation_men") ~ "male observation",
            str_detect(question, "observation_women") ~ "female observation",
            str_detect(question, "observation") & !str_detect(question, "_(men|women)") ~ "observation",
            str_detect(question, "_men") & !str_detect(question, "observation") ~ "male score",
            str_detect(question, "_women") & !str_detect(question, "observation") ~ "female score",
            TRUE ~ "score_all" # Default for questions without specific gender or observation tags
          ),
          question_cleaned = str_replace_all(question, "(_men|_women|_observation(?:women|men)?)\\w*?(?=_2|$)", ""),
          question_cleaned = str_replace_all(question_cleaned, "_{2,}", "_"),
          question = str_remove_all(question_cleaned, "_$"),
          # Assuming 'text_field' is the column from which you want to remove "_3"
          question = str_remove_all(question, "_2$")
         )%>%
         select(Member_id,tab_date,Region_id,District_id,Community_id,risk_scenario,risk_scenario_value,question,category,scores)
         
      
      # write.xlsx(main_survey_part_b, "systemcheck.xlsx")
      
      # dd <- main_survey_part_b %>% filter(is.na(risk_scenario_value)) %>% group_by(Member_id,Region_id,District_id,Community_id,risk_scenario,risk_scenario_value)%>%
      #   summarise(n=n()) %>% select(Member_id,Region_id,District_id,Community_id,risk_scenario,risk_scenario_value)
      # print(dd)
      # dd<-main_survey_part_b |>
      #   dplyr::summarise(n = dplyr::n(), .by = c(Member_id, tab_date, Region_id, District_id, Community_id, risk_scenario, risk_scenario_value, question,
      #                                            category)) |>
      #   dplyr::filter(n > 1L) 
      # 
      # print(dd,n = 1000)
    
      
      # write.xlsx(main_survey_part_b, "againnnnnnnnnnnnnnnnnnnnnn.xlsx")
      
      if(Authresult$message != 'CMU'){
        main_survey_part_a %<>% filter(str_to_lower(Member_id) == str_to_lower(Authresult$message))
        main_survey_part_b %<>% filter(str_to_lower(Member_id) == str_to_lower(Authresult$message))
      }
 
      global_vars$global_arcd_survey_part_a <- main_survey_part_a
      global_vars$global_arcd_survey_part_b <- main_survey_part_b
      
      if(nrow(main_survey_part_a) == 0){
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
    
    
    
  })
  
  
  
  # 
  
  
  
  output$regionControllerFill <- renderUI({

   
    if(is.null(input$memberController) || input$memberController == "all") {
      regions <- communities %>%
        distinct(region) %>%
        pull(region)
    }else{
      regions <- communities %>%
        filter(member == input$memberController)%>%
        distinct(region) %>%
        pull(region)
    }
    
    selectInput(
      ns1("regionController"),
      paste("Region"),
      choices = c("all",regions)
    )
  })
  
  output$districtControllerFill <- renderUI({
    
    communities1 <- communities
    if(!is.null(input$memberController) && input$memberController != "all") {
      communities1 <-communities %>%
        filter(member == input$memberController)
    }
    
    if(is.null(input$regionController) || input$regionController == "all") {
      districts <- communities1 %>%
        distinct(district) %>%
        pull(district)
    }else{
      districts <- communities1 %>%
        filter(region == input$regionController)%>%
        distinct(district) %>%
        pull(district)
    }
    
    selectInput(
      ns1("districtController"),
      paste("District"),
      choices = c("all",districts)
    )
  })
  
  output$communityControllerFill <- renderUI({
    
    communities1 <- communities
    if(!is.null(input$memberController) && input$memberController != "all") {
      communities1 <-communities %>%
        filter(member == input$memberController)
    }
    
    if(!is.null(input$regionController) &&  input$regionController != "all") {
      communities1 <-communities %>%
        filter(region == input$regionController)
    }
    
    
    if(is.null(input$districtController) || input$districtController == "all") {
      community <- communities1 %>%
        distinct(community) %>%
        pull(community)
    }else{
      community <- communities1 %>%
        filter(district == input$districtController)%>%
        distinct(community) %>%
        pull(community)
    }
    
    selectInput(
      ns1("communityController"),
      paste("Community"),
      choices = c("all",community)
    )
  })
  
  output$riskControllerFill <- renderUI({
    
    risk_scenario_data <- global_vars$global_arcd_survey_part_b
    
    if(!is.null((input$memberController)) && !is.null(risk_scenario_data)){
      if((input$memberController) != "all"){
        
      
        risk_scenario_data %<>% filter(
          Member_id == input$memberController
        )
        
        
      }
      
    }
    
    if(!is.null((input$regionController)) && !is.null(risk_scenario_data)){
      if((input$regionController) != "all"){
      
        risk_scenario_data %<>% filter(
          Region_id == input$regionController
        )
        
        
      }
      
    }
    
    if(!is.null((input$districtController)) && !is.null(risk_scenario_data)){
      if((input$districtController) != "all"){
        
      
        risk_scenario_data %<>% filter(
          District_id == input$districtController
        )
        
        
      }
      
    }
    
    if(!is.null((input$communityController)) && !is.null(risk_scenario_data)){
      if((input$communityController) != "all"){
      
        risk_scenario_data %<>% filter(
          Community_id == input$communityController
        )
        
        
      }else{
        risk_scenario_data %<>% filter(
          FALSE
        )
      }
      
    }

    
    selectInput(
      ns1("riskController"),
      paste("Risk Scenario"),
      choices = c("all",unique(risk_scenario_data$risk_scenario_value))
    )
    
    
  })
  
  output$arcDPhaseController <- renderUI({
    
    if( is.null(nrow(global_vars$global_arcd_survey_part_a)) || nrow(global_vars$global_arcd_survey_part_a) == 0){
      selectInput(
        ns1("phaseController"),
        paste("Phases"),
        choices = c("all")
      )
    }else{
      phases <- global_vars$global_arcd_survey_part_b
      
      if(!is.null((input$communityController)) && !is.null(phases)){
        if((input$communityController) != "all"){
          
          phases %<>% filter(
            Community_id == input$communityController
          )
          
          
        }
        
      }
      
      selectInput(
        ns1("phaseController"),
        paste("Phases"),
        choices = c("all",unique(format(phases$tab_date, "%Y")))
      )
    }
   
    
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
  
  calculateResilenceStateByLikert <- function(likert){
   
    
    
      if(likert == 1){
        return("Very Low Resilience")
      }else if(likert == 2){
        return("Low Resilience")
      }else if(likert == 3){
        return("Medium Resilience")
      }else if(likert == 4){
        return("Close to Resilience")
      }else if(likert == 5){
        return("Resilience")
      }else{
        return("NA")
      }
    
    
  }
  

  
  calculateResilenceState <- function(score_percentage){
    if(!is.na(score_percentage)){
      if(score_percentage >= 0 & score_percentage < 31){
        return("Very Low Resilience")
      }else if(score_percentage >=31 & score_percentage < 51){
        return("Low Resilience")
      }else if(score_percentage >=51 & score_percentage < 71){
        return("Medium Resilience")
      }else if(score_percentage >=71 & score_percentage < 91){
        return("Close to Resilience")
      }else{
        return("Resilience")
      }
    }else{
      return("NA")
    }
    
  }
  
  calculateResilenceColor <- function(score_percentage){
    if(!is.na(score_percentage)){
      if(score_percentage >= 0 & score_percentage < 31){
        return("#dc3545")
      }else if(score_percentage >=31 & score_percentage < 51){
        return("#FFC000")
      }else if(score_percentage >=51 & score_percentage < 71){
        return("#FFD966")
      }else if(score_percentage >=71 & score_percentage < 91){
        return("#A8D08C")
      }else{
        return("#00B050")
      }
    }else{
      return("")
    }
    
  }
  
  custom_aggregate <- function(x) {
    numeric_values <- suppressWarnings(as.numeric(x))
    
    # Check if the conversion to numeric was successful for all elements
    if (!all(is.na(numeric_values))) {
      # Handle as numeric data: Calculate mean of numeric values, ignoring NAs
      return(as.character(mean(numeric_values, na.rm = TRUE)))
    } else {
      # Handle non-numeric data: Check if all values are identical
      if (length(unique(x)) == 1) {
        # All non-numeric values are identical
        return(as.character(unique(x)))
      } else {
        # Multiple different non-numeric values
        return("aggregated")
      }
    }
  }

  observe({
    
   
    
    if( is.null(nrow(global_vars$global_arcd_survey_part_a)) || nrow(global_vars$global_arcd_survey_part_a) == 0){
      return()
    }
    
    output$warningToLoadDATA <- renderUI({
      HTML("")
    })
    
  
    
    main_survey_part_a <- isolate(global_vars$global_arcd_survey_part_a)
    main_survey_part_b <- isolate(global_vars$global_arcd_survey_part_b)
    
    if(!is.null((input$phaseController))){
      if((input$phaseController) != "all"){
        
        
        
        main_survey_part_b %<>% filter(
          format(tab_date, "%Y") == input$phaseController
        )
        
        main_survey_part_a%<>% filter(
          format(tab_date, "%Y") == input$phaseController
        )
        
        
      }
      
    }
    
    arcd_systems_grouped <- arcd_systems %>%
      group_by(qno)%>%
      summarise(
        question_system = paste(question_system,collapse = "#")
      )
    
    
    main_survey_part_b%<>%pivot_wider(
      
      names_from = category,
      values_from = scores,
      values_fn = list(scores = custom_aggregate), # Use custom function
      values_fill = list(scores = NA) # Fill missing observations or scores with NA
      
    )%>%
      select(Member_id,tab_date,Region_id,District_id,Community_id,risk_scenario,risk_scenario_value,question,score_all,observation,"male score","male observation",
             "female score","female observation")%>%
      left_join(arcd_questions,by = "question")%>%
      left_join(arcd_thematic,by = "qno")%>%
      left_join(arcd_systems_grouped,by = "qno")%>%
      select(Member_id,tab_date,Region_id,District_id,Community_id,risk_scenario,risk_scenario_value,qno,question,question_desc,
             question_system,thematic_area,	thematic_desc,
             score_all,observation,"male score","male observation",
             "female score","female observation")
    
    
    main_communities <- communities

    if(!is.null((input$memberController))){
      if((input$memberController) != "all"){
        
        main_survey_part_a %<>% filter(
          Member_id == input$memberController
        )

        main_survey_part_b %<>% filter(
          Member_id == input$memberController
        )
        main_communities %<>% filter(
          member  == input$memberController
        )

      }

    }

    if(!is.null((input$regionController))){
      if((input$regionController) != "all"){

        main_survey_part_a %<>% filter(
          Region_id == input$regionController
        )

        main_survey_part_b %<>% filter(
          Region_id == input$regionController
        )
        
        main_communities %<>% filter(
          region  == input$regionController
        )

      }

    }

    if(!is.null((input$districtController))){
      if((input$districtController) != "all"){

        main_survey_part_a %<>% filter(
          District_id == input$districtController
        )

        main_survey_part_b %<>% filter(
          District_id == input$districtController
        )

        main_communities %<>% filter(
          district  == input$districtController
        )

      }

    }

    if(!is.null((input$communityController))){
      if((input$communityController) != "all"){

        main_survey_part_a %<>% filter(
          Community_id == input$communityController
        )

        main_survey_part_b %<>% filter(
          Community_id == input$communityController
        )
        
        main_communities %<>% filter(
          community  == input$communityController
        )

      }

    }

    if(!is.null((input$riskController))){
      if((input$riskController) != "all"){


       
        main_survey_part_b %<>% filter(
          risk_scenario_value == input$riskController
        )


      }

    }
    
    
   
    
    output$total_population <- renderUI({
      
      format(round(sum(as.numeric(main_survey_part_a$total_number_of_populations_check),na.rm = TRUE),2), big.mark = ",")
      
    })
    
    output$total_households <- renderUI({
      
      format(round(sum(as.numeric(main_survey_part_a$total_number_of_households),na.rm = TRUE),2), big.mark = ",")
    })
    
  
    
    output$total_communities <- renderUI({
      
      length(unique(main_survey_part_a$Community_id))
    })
    
    output$total_male <- renderUI({
      
      format(round(
        sum(as.numeric(main_survey_part_a$boys_younger_than_18_years),na.rm = TRUE) +sum(as.numeric(main_survey_part_a$men_aged_18.60_years),na.rm = TRUE)+
          sum(as.numeric(main_survey_part_a$men_older_than_60_years),na.rm = TRUE),2
      ),big.mark = ",")
      
      
    })
    
    output$total_female <- renderUI({
      
      
      format(round(
        sum(as.numeric(main_survey_part_a$girls_younger_than_18_years),na.rm = TRUE) +sum(as.numeric(main_survey_part_a$women_aged_18.60_years),na.rm = TRUE)+
          sum(as.numeric(main_survey_part_a$women_older_than_60_years),na.rm = TRUE),2
      ),big.mark = ",")
     
    })
    
    output$populationGroupChart <- renderPlotly({
      chartData <- data.frame(
        "name" = c("Male below 18","Female below 18","Male 18-60","Female 18-60","Male above 60","Female above 60"),
        "value" = c(
                    sum(as.numeric(main_survey_part_a$boys_younger_than_18_years),na.rm = TRUE),
                    sum(as.numeric(main_survey_part_a$girls_younger_than_18_years),na.rm = TRUE),
                    sum(as.numeric(main_survey_part_a$men_aged_18.60_years),na.rm = TRUE),
                    sum(as.numeric(main_survey_part_a$women_aged_18.60_years),na.rm = TRUE),
                    sum(as.numeric(main_survey_part_a$men_older_than_60_years),na.rm = TRUE),
                    sum(as.numeric(main_survey_part_a$women_older_than_60_years),na.rm = TRUE)
        )
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie')
      fig <- fig %>% layout(title = list(text = 'Age and Gender Classification', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
                            margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
                            xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$livelihood_zoneChart <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(livelihood_zone, sep = " ")%>%
        group_by(livelihood_zone)%>%
        summarise(
          count = n()
        )
      
      fig <- plot_ly(chartData, labels = ~livelihood_zone, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = list(text = 'Livelihood Zones', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
                            margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
                            xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$education_levelChart <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(education_level, sep = " ")%>%
        group_by(education_level)%>%
        summarise(
          count = n()
        )
      
      fig <- plot_ly(chartData, labels = ~education_level, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = list(text = 'Educational Level', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
                            margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
                            xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$local_community_groupsChart <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(local_community_groups, sep = " ")%>%
        group_by(local_community_groups)%>%
        summarise(
          count = n()
        )
      
      fig <- plot_ly(chartData, labels = ~local_community_groups, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = list(text = 'Community Groups', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
                            margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
                            xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$external_org_typeChart <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(external_org_type, sep = " ")%>%
        group_by(external_org_type)%>%
        summarise(
          count = n()
        )
      
      fig <- plot_ly(chartData, labels = ~external_org_type, values = ~count, type = 'pie')
      fig <- fig %>% layout(title = list(text = 'External Community Groups', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
                            margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
                            xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    
    output$check_list_of_available_plans <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(available_plans, sep = " ")%>%
        group_by(available_plans)%>%
        summarise(
          count = n()
        )
      
      chartData$text_position <- ifelse(chartData$count > 5, 'inside', 'outside')
      
      fig <- plot_ly(chartData)%>%
        add_bars(
          x = ~available_plans,
          y = ~count,
          text = ~count,
          textposition = ~text_position
        )%>%
       layout(title = list(text = 'Available Plans', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
              margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
              xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              legend = list(x = 1.05, y = 1, orientation = "v"))
      
      fig
    })
    
    output$vul_groups <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(vul_groups, sep = " ")%>%
        group_by(vul_groups)%>%
        summarise(
          count = n()
        )
      
      chartData$text_position <- ifelse(chartData$count > 5, 'inside', 'outside')
      
      fig <- plot_ly(chartData)%>%
        add_bars(
          x = ~vul_groups,
          y = ~count,
          text = ~count,
          textposition = ~text_position
        )%>%
        layout(title = list(text = 'Vulnerable Groups', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
               margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
               xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(x = 1.05, y = 1, orientation = "v"))
      
      fig
    })
    
    output$shock_list1 <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(shock_list1, sep = " ")%>%
        group_by(shock_list1)%>%
        summarise(
          count = n()
        )
      
      chartData$text_position <- ifelse(chartData$count > 5, 'inside', 'outside')
      
      fig <- plot_ly(chartData)%>%
        add_bars(
          x = ~shock_list1,
          y = ~count,
          text = ~count,
          textfont = list(color = 'black'),
          textposition = ~text_position,
          color = ~shock_list1
        )%>%
        layout(title = list(text = 'Shock List', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
               margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
               xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(x = 1.05, y = 1, orientation = "v"))
      
      fig
    })
    
    output$stress2 <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(stress2, sep = " ")%>%
        group_by(stress2)%>%
        summarise(
          count = n()
        )
      
      chartData$text_position <- ifelse(chartData$count > 5, 'inside', 'outside')
      
      fig <- plot_ly(chartData)%>%
        add_bars(
          x = ~stress2,
          y = ~count,
          text = ~count,
          textposition = ~text_position
        )%>%
        layout(title = list(text = 'Stresses', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
               margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
               xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(x = 1.05, y = 1, orientation = "v"))
      
      fig
    })
    
    output$most_devastating_shock <- renderPlotly({
      chartData <- main_survey_part_a %>%
        separate_rows(most_devastating_shock, sep = " ")%>%
        group_by(most_devastating_shock)%>%
        summarise(
          count = n()
        )
      
      chartData$text_position <- ifelse(chartData$count > 5, 'inside', 'outside')
      
      fig <- plot_ly(chartData)%>%
        add_bars(x = ~most_devastating_shock,
                 y = ~count,
                 text = ~count,
                 textfont = list(color = 'black'),
                 textposition = ~text_position,
                 color = ~most_devastating_shock)%>%
        layout(title = list(text = 'Most Devatating shocks', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
               margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
               xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(x = 1.05, y = 1, orientation = "v"))
      fig
      
     
      
    })
    
    output$spiderChart <- renderPlotly({
      
      if(is.null(input$selectedspiderCheckboxtype)){
        group_by_c <- "qno"
      }else{
        group_by_c <- input$selectedspiderCheckboxtype
      }
      
      if(group_by_c == 'qno'){
        chartData <- main_survey_part_b %>%
          group_by(qno)%>%
          reframe(
            main_group_value = first(question),
            second_group_value = first(question_desc),
            score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
            score_male = round(mean(as.numeric(`male score`),na.rm = TRUE),0),
            score_female = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
            third_group_value = paste(qno,question_desc,sep = "-"),
            observation = first(observation),
            "male observation" = first(`male observation`),
            "female observation" = first(`female observation`),
            count = n()
          )%>%
          arrange(qno)
        
        max_score <- 5
        
      }else if(group_by_c == 'thematic_desc'){
        
        chartData <- main_survey_part_b %>%
          group_by(qno,thematic_desc)%>%
          reframe(
            thematic_desc = first(thematic_desc),
            thematic_area = first(thematic_area),
            score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
            score_male = round(mean(as.numeric(`male score`),na.rm = TRUE),0),
            score_female = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
            observation = if_else(is.na(observation),'',first(observation)),
            "male observation" = first(`male observation`),
            "female observation" = first(`female observation`),
            count = n()
          ) %>%
          group_by(thematic_desc)%>%
          reframe(
            main_group_value = first(thematic_desc),
            second_group_value = first(thematic_area),
            score_all = round(sum(as.numeric(score_all),na.rm = TRUE),0),
            score_male = round(sum(as.numeric(score_male),na.rm = TRUE),0),
            score_female = round(sum(as.numeric(score_female),na.rm = TRUE),0),
            third_group_value = paste(thematic_desc,sep = "-"),
            observation = if_else(is.na(observation),'',first(observation)),
            "male observation" = first(`male observation`),
            "female observation" = first(`female observation`),
            count = n()
          )%>%
          arrange(second_group_value)
        
        max_score <- 5 * max(chartData$count)
      }else{
        
        chartData <- main_survey_part_b %>%
          separate_rows(question_system, sep = "#")%>%
          group_by(qno,question_system)%>%
          reframe(
            question_system = first(question_system),
            score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
            score_male = round(mean(as.numeric(`male score`),na.rm = TRUE),0),
            score_female = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
            observation = if_else(is.na(observation),'',first(observation)),
            "male observation" = first(`male observation`),
            "female observation" = first(`female observation`),
            count = n()
          ) %>%
          group_by(question_system)%>%
          reframe(
            main_group_value = first(question_system),
            second_group_value = first(question_system),
            score_all = round(sum(as.numeric(score_all),na.rm = TRUE),0),
            score_male = round(sum(as.numeric(score_male),na.rm = TRUE),0),
            score_female = round(sum(as.numeric(score_female),na.rm = TRUE),0),
            third_group_value = paste(question_system,sep = "-"),
            observation = if_else(is.na(observation),'',first(observation)),
            "male observation" = first(`male observation`),
            "female observation" = first(`female observation`),
            count = n()
          )%>%
          arrange(second_group_value)
        
        max_score <- 5 * max(chartData$count)
      }
        
      
     print(chartData,)
     
      
      # if (nrow(chartData) > 0) {
      #   max_score <- 5*as.numeric(max(chartData$count,na.rm = TRUE))
      # } else {
      #   max_score <- 5
      # }
      # 
      
      if (nrow(chartData)<=0){
        max_score <- 5
      }
     
      tick_values <- seq(0, max_score, length.out = 6)  # Remove 0 to get 5 divisions
     
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      
      fig <- plot_ly(chartData,
        type = 'scatterpolar',
        r = ~get(score_to_display),
        theta = ~third_group_value,
        fill = 'toself',
        mode   = 'markers+lines',
        marker = list(size = 10),
        fillcolor  = 'rgba(255, 0, 0, 0.2)',  # Set fill color to red
        line = list(color = '#F99D1E'),  # Set line color to red
        
        hovertext = ~paste("Score:",get(score_to_display),"<br>Component:",third_group_value,"<br>Questions:", count, "<br>Observation:", 
                      ifelse(max(chartData$count,na.rm = TRUE) > 1 , 
                             "Aggregated",
                              ifelse(score_to_display == "score_all",chartData$observation,
                                     ifelse(score_to_display == "score_male",chartData$`male observation`,chartData$`female observation`)))) ,
        hoverinfo = ''
      ) 
      
     
      fig <- fig %>%
        layout(
          title = list(text = paste( paste("Community:",gsub("_", " ", input$communityController)),"|", 
                                     paste("Risk Scenrio:",ifelse(input$communityController == "all","Aggregated",gsub("_", " ", input$riskController))) , "|",
                                     gsub("_", " ", ifelse(score_to_display == "score_all","Community Score",score_to_display))), 
                       x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top'),
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,max_score),
              tickvals = tick_values,
              ticktext = tick_values
            ),
            angularaxis = list(direction = "clockwise", rotation = 90)
            
          ),
          showlegend = FALSE,
          margin = list(l = 200, r = 200) # Adjust margins as needed
        )
      
      fig
    })
    
    output$spiderChartTable <- renderDT(server = FALSE,{
      
      if(input$communityController == "all" || input$riskController == "all"){
        chartDataTable <- main_survey_part_b %>%
          group_by(qno)%>%
          summarise(
            question_desc = first(question_desc),
            score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
            "male score" = round(mean(as.numeric(`male score`),na.rm = TRUE),0),
            "female score" = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
            "Total Community" = n()
          )%>%
          arrange(qno)%>%
          select(qno , question_desc,score_all,"male score","female score","Total Community")
      }else{
        chartDataTable <- main_survey_part_b %>%
          group_by(qno)%>%
          summarise(
            question_desc = first(question_desc),
            score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
            observation = first(observation),
            "male score" = round(mean(as.numeric(`male score`),na.rm = TRUE),0),
            "male observation" = first(`male observation`),
            "female score" = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
            "female observation" = first(`female observation`),
            count = n()
          )%>%
          arrange(qno)%>%
          select(qno , question_desc,score_all,observation,"male score","male observation","female score","female observation")
      }
     
      
      datatable(chartDataTable,rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                   
                  pageLength = 30,
                  
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = "Summary",
                      title = "Summary",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'excel',
                      filename = "Summary",
                      title = "Summary",
                      exportOptions = list(modifier = list(page = 'all'))
                    ),
                    list(
                      extend = 'pdf',
                      filename = "Summary",
                      title = "Summary",
                      exportOptions = list(modifier = list(page = 'all')),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Summary',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      filename = "Summary",
                      title = "Summary",
                      exportOptions = list(modifier = list(page = 'all'))
                    )
                    
                  )
                ))
    })
    
    output$theRiskScenarioType <- renderUI({
     
      
      if((input$communityController) == "all"){
        return("All Communities")
      }else{
        if(input$riskController == "all"){
          return("All Risk Scenarios")
        }else{
          gsub("_", " ", first(main_survey_part_b$risk_scenario))
        }
        
      }
      
    })
    
    output$theRiskScenarioValue <- renderUI({
     
      
      gsub("_", " ", input$riskController)
    })
    
    output$communityName <- renderUI({
      
      if((input$communityController) == "all"){
        return("All Communities")
      }else{
        
        gsub("_", " ", first(main_survey_part_b$Community_id))
      }
      
    })
    
    output$CommunityDistrict <- renderUI({
      
      if((input$communityController) == "all"){
        
        gsub("_", " ", input$districtController)
      }else{
        
        gsub("_", " ", first(main_survey_part_b$District_id))
      }
      
    })
    
    
    output$theRiskScenarioType2 <- renderUI({
      
      if((input$communityController) == "all"){
        return("All Communities")
      }else{
        if(input$riskController == "all"){
          return("All Risk Scenarios")
        }else{
          gsub("_", " ", first(main_survey_part_b$risk_scenario))
        }
        
      }
      
    })
    
    output$theRiskScenarioValue2 <- renderUI({
      
      gsub("_", " ", input$riskController)
    })
    
    output$communityName2 <- renderUI({
      
      if((input$communityController) == "all"){
        return("All Communities")
      }else{
        
        gsub("_", " ", first(main_survey_part_b$Community_id))
      }
      
    })
    
    output$CommunityDistrict2 <- renderUI({
      
      if((input$communityController) == "all"){
        
        gsub("_", " ", input$districtController)
      }else{
        
        gsub("_", " ", first(main_survey_part_b$District_id))
      }
      
    })
    
    output$componentScoresTables <- renderDT({
      
      if((input$communityController) == "all" || input$riskController == "all"){
        chartDataTable <- main_survey_part_b %>%
          group_by(qno)%>%
          summarise(
            question_desc = first(question_desc),
            score_all = round( mean(as.numeric(score_all),na.rm = TRUE),0),
            observation = "Avg Aggregated",
            "male score" = round( mean(as.numeric(`male score`),na.rm = TRUE),0),
            "male observation" = "Avg Aggregated",
            "female score" = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
            "female observation"= "Avg Aggregated",
             communities = n()
          )%>%
          mutate(
            "Community score"=score_all
          )%>%
          arrange(qno)%>%
          select(qno , question_desc,"Community score",observation,"male score","male observation","female score","female observation")
      }else{
        chartDataTable <- main_survey_part_b %>%
          group_by(qno)%>%
          summarise(
            question_desc = first(question_desc),
            score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
            observation = first(observation),
            "male score" = round(mean(as.numeric(`male score`),na.rm = TRUE)),
            "male observation" = first(`male observation`),
            "female score" = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
            "female observation" = first(`female observation`),
            communities = n()
          )%>%
          mutate(
            "Community score"=score_all
          )%>%
          arrange(qno)%>%
          select(qno , question_desc,"Community score",observation,"male score","male observation","female score","female observation")
      }
      
      # if (nrow(chartDataTable) > 0) {
      #   max_score <- (5*as.numeric(max(chartDataTable$communities,na.rm = TRUE))) * 30
      # } else {
      #   max_score <- 5 * 30
      # }
      
      max_score <- 5 * 30
      
      
     
      
      sums <- format(colSums(chartDataTable[, c("Community score", "male score","female score")], na.rm = TRUE),big.mark =",")
      
      footer <- paste0('
      <tfoot>
        <tr>
            <th colspan="2">Total resilience score</th>
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[1]))/max_score)*100,0)),'">', sums[1], '</th>
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[2]))/max_score)*100,0)),'">', sums[2], '</th>
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[3]))/max_score)*100,0)),'">', sums[3], '</th>
            <th></th>
        </tr>
        
        <tr>
            <th colspan="2">Resilience %</th>
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[1]))/max_score)*100,0)),'">', paste0(round((as.numeric(gsub(",", "", sums[1]))/max_score)*100,0),"%"), '</th>
           
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[2]))/max_score)*100,0)),'">', paste0(round((as.numeric(gsub(",", "", sums[2]))/max_score)*100,0),"%"),'</th>
            
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[3]))/max_score)*100,0)),'">', paste0(round((as.numeric(gsub(",", "", sums[3]))/max_score)*100,0),"%"), '</th>
            
            <th></th>
        </tr>
        
        <tr>
            <th colspan="2">Resilience Level</th>
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[1]))/max_score)*100,0)),'">', calculateResilenceState(round((as.numeric(gsub(",", "", sums[1]))/max_score)*100,0)) , '</th>
            
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[2]))/max_score)*100,0)),'">', calculateResilenceState(round((as.numeric(gsub(",", "", sums[2]))/max_score)*100,0)) ,'</th>
           
            <th colspan="2" class="dt-right" style="background-color:',calculateResilenceColor(round((as.numeric(gsub(",", "", sums[3]))/max_score)*100,0)),'">', calculateResilenceState(round((as.numeric(gsub(",", "", sums[3]))/max_score)*100,0)) , '</th>
            
            <th></th>
        </tr>
      </tfoot>')
      
      datatable(chartDataTable,class = "display dataTable componentScoresTables",rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                  
                  pageLength = 30,
                  
                  dom = 'lBfrtip',
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().footer()).remove();",
                    "$(this.api().table().header()).after(`", footer, "`);",
                    "}"
                  ),
                  buttons = list(
                    list(
                      extend = 'csv',
                     
                      filename = "Component Scores",
                      title = "Component Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',
                      
                      filename = "Component Scores",
                      title = "Component Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',
                     
                      filename = "Component Scores",
                      title = "Component Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Component Scores',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      
                      filename = "Component Scores",
                      title = "Component Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )
                    
                  )
                ))
      
    
    })
    
    output$componentScoresChart <- renderPlotly({
      
      chartDataTable <- main_survey_part_b %>%
        group_by(qno)%>%
        summarise(
          question_desc = first(question_desc),
          score_all = round( mean(as.numeric(score_all),na.rm = TRUE),0),
          observation = "Avg Aggregated",
          "male score" = round( mean(as.numeric(`male score`),na.rm = TRUE),0),
          "male observation" = "Avg Aggregated",
          "female score" = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
          "female observation"= "Avg Aggregated",
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        arrange(qno)%>%
        select(qno , question_desc,"Community score",observation,"male score","male observation","female score","female observation",communities)
      
      fig <- plot_ly(chartDataTable, x = ~question_desc, y = ~`Community score`, 
                     type = 'bar', 
                     name = 'Community score',
                     marker = list(color = '#008BA8'),
                     text = ~round(`Community score`, 0), textposition = 'outside'
      )
      fig <- fig %>% add_trace(y = ~`male score` ,name= 'Male score',
                               marker = list(color = '#F99D1E'),
                               text = ~round(`male score`, 2), textposition = 'outside')
      fig <- fig %>% add_trace(y = ~`female score` ,name= 'Female score',
                               marker = list(color = '#ED7667'),
                               text = ~round(`female score`, 2), textposition = 'outside')
      fig <- fig %>% layout(yaxis = list(title = 'Score'), barmode = 'group')
      
      fig
      
    })
    
    output$componentScoresBarChart <- renderPlotly({
      

      
      chartDataTable <- main_survey_part_b %>%
        group_by(qno,Community_id)%>%
        reframe(
          question_desc = first(question_desc),
          score_all = round( mean(as.numeric(score_all),na.rm = TRUE),0),
          observation = "Avg Aggregated",
          "male score" = round( mean(as.numeric(`male score`),na.rm = TRUE),0),
          "male observation" = "Avg Aggregated",
          "female score" = round(mean(as.numeric(`female score`),na.rm = TRUE),0),
          "female observation"= "Avg Aggregated",
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        arrange(qno)%>%
        select(qno , question_desc,communities,Community_id,"Community score")%>%
        group_by(qno , question_desc,`Community score`)%>%
        reframe(
          communities = max(communities),
          scores_count = n()
        )%>%
        pivot_wider(id_cols = qno:communities ,names_from =`Community score`,values_from = scores_count )
      
      chartDataTable <- chartDataTable %>%
        {
          if (!'1' %in% names(.)) {
            chartDataTable <- chartDataTable%>%
            mutate(., `1` = 0)
          } else {
            chartDataTable <- chartDataTable%>%
            mutate(., `1` = `1`)
          }
          if (!'2' %in% names(.)) {
            chartDataTable <- chartDataTable%>%
            mutate(., `2` = 0)
          } else {
            chartDataTable <- chartDataTable%>%
            mutate(., `2` = `2`)
          }
          if (!'3' %in% names(.)) {
            chartDataTable <- chartDataTable%>%
            mutate(., `3` = 0)
          } else {
            chartDataTable <- chartDataTable%>%
            mutate(., `3` = `3`)
          }
          if (!'4' %in% names(.)) {
            chartDataTable <- chartDataTable%>%
            mutate(., `4` = 0)
          } else {
            chartDataTable <- chartDataTable%>%
            mutate(., `4` = `4`)
          }
          if (!'5' %in% names(.)) {
            chartDataTable <- chartDataTable%>%
            mutate(., `5` = 0)
          } else {
            chartDataTable <- chartDataTable%>%
            mutate(., `5` = `5`)
          }
        }%>%
        mutate(across(c(`1`,`2`,`3`,`4`,`5`), ~ifelse(is.na(.), 0, .))) %>%
        select(qno,question_desc,communities,`1`,`2`,`3`,`4`,`5`)%>%
        pivot_longer(cols = `1`:`5` ,names_to = 'likert',values_to = 'count')%>%
        mutate(
           percentage = round((as.numeric(count) /(length(unique(main_survey_part_b$Community_id)))) *100,2)
        )%>%
        arrange(qno)
        
      
      chartDataTable$resilienceStates <- sapply(chartDataTable$likert, calculateResilenceStateByLikert)
      
     
      
      # Colors for Likert scale responses (5 to 1) - dark to light
      colors <- c('#011f4b', '#03396c', '#005b96', '#6497b1', '#b3cde3') # Reversed color order
      
      # Create the plot with reversed Likert scale
      p <- plot_ly(chartDataTable, type = 'bar', orientation = 'h',
                   x = ~percentage, y = ~question_desc,
                   color = ~factor(likert, levels = 5:1), # Ensure correct color mapping
                   hoverinfo = '',
                   colors = colors,
                   hovertext = ~paste("Communities:",count,"<br>Question:",question_desc,"<br>likert",likert,
                                      "<br> Resilience Level:",resilienceStates,
                                 "<br>Community Percentage",percentage)
                   ) %>%
        layout(title = 'Component Score Distribution over communities',
               barmode = 'stack',
               xaxis = list(title = 'Community Distribution on Rilience',autorange = 'reversed'),
               yaxis = list(title = 'Components'))
      
      # Show the plot
      p
      
      
      
    })
    
    
    output$thematicScoreTable <- renderDT({
      
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      
      chartDataTable <- main_survey_part_b %>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        group_by(qno,thematic_desc,risk_scenario)%>%
        reframe(
          risk_scenario = ifelse((input$communityController) == "all" &&  input$riskController == "all", risk_scenario , 
                                 paste(risk_scenario,risk_scenario_value , sep = "-")) ,
          thematic_area =first(thematic_area),
          question_desc = first(question_desc),
          score_all = round( mean(as.numeric(get(score_to_display)),na.rm = TRUE),0),
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        group_by(thematic_desc,risk_scenario)%>%
        reframe(
          thematic_area = first(thematic_area),
          "Community score" = round(sum(as.numeric(`Community score`),na.rm = TRUE),0),
          communities=max(communities),
          questions = n()
        )%>%
        arrange(risk_scenario,thematic_area)%>%
        select(thematic_area,thematic_desc,communities,questions,risk_scenario,"Community score")%>%
        pivot_wider(id_cols = thematic_area:questions , names_from =risk_scenario , values_from = `Community score`)
      
        if(nrow(chartDataTable) >1){
          if(input$riskController == "all" || input$communityController == "all"){
            last_two_col_names <- names(chartDataTable)[(ncol(chartDataTable)-1):ncol(chartDataTable)]
            
          }else{
            last_two_col_names <- names(chartDataTable)[ncol(chartDataTable)]
          }
          
          chartDataTable%<>%
            rowwise()%>%
            mutate(
              `total risk scenarios` = ifelse(input$riskController == "all" || input$communityController == "all" , 
                                         get(last_two_col_names[1]) + get(last_two_col_names[2]) , 
                                         get(last_two_col_names[1]))
            )%>%
            pivot_longer(cols =c(last_two_col_names,'total risk scenarios') ,names_to = 'risk_scenario' , values_to = 'score' )%>%
            select(risk_scenario,thematic_area,thematic_desc,communities,questions,score)%>%
            rowwise()%>%
            mutate(questions = ifelse(risk_scenario == 'total risk scenarios',as.numeric(questions)*length(last_two_col_names),as.numeric(questions)),
                   max_score =5*as.numeric(questions),
                   `Resilence %` = round((score/max_score)*100,0),
                   `Resilience Level` = calculateResilenceState(round((score/max_score)*100,0)))%>%
            mutate(
              `Resilence %` = paste(`Resilence %`,'%',sep = '')
            )%>%
            select(risk_scenario,thematic_area,thematic_desc,communities,questions,score,max_score,`Resilence %`,`Resilience Level`)%>%
            arrange(risk_scenario,thematic_area)
          
        }else{
          chartDataTable %<>%
            mutate(
              `Resilience Level` = NA
            )
        }
        
      
        
        
      # datatable(chartDataTable)
      
      datatable(chartDataTable,rownames = FALSE,
                extensions = c('Buttons', 'RowGroup'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting
                  autoWidth = TRUE,
                  pageLength = 30,
                  rowGroup = list(dataSrc=c(0),
                                  startRender = JS(
                                    "function(rows, group) {",
                                    "  var style ='background-color: #008BA8;';",
                                    "  var td = `<td style='${style}' colspan=8>${group}</td>`;",
                                    "  return $(`<tr>${td}</tr>`);",
                                    "}"
                                  )),
                                                columnDefs = list(list(visible=FALSE, targets=c(0),orderable = FALSE),
                                                                  list(targets = "_all", orderable = FALSE)),

                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',

                      filename = "Thematic Area Scores",
                      title = "Thematic Area Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',

                      filename = "Thematic Area Scores",
                      title = "Thematic Area Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',

                      filename = "Thematic Area Scores",
                      title = "Thematic Area Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Component Scores',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',

                      filename = "Thematic Area Scores",
                      title = "Thematic Area Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )

                  )
                ))%>%
        formatStyle(
          'Resilience Level',
          backgroundColor = styleEqual(c('Very Low Resilience', 'Low Resilience', 'Medium Resilience','Close to Resilience', 'Resilience'), 
                                       c("#dc3545", "#FFC000", "#FFD966", "#A8D08C","#00B050"))
        )
      
    })
    
    output$thematicScorePercentageComparisonTable <- renderDT({
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      
      chartDataTable <- main_survey_part_b %>%
        group_by(qno,thematic_desc,risk_scenario)%>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        reframe(
          risk_scenario = ifelse((input$communityController) == "all" &&  input$riskController == "all", risk_scenario , 
                                 paste(risk_scenario,risk_scenario_value , sep = "-")) ,
          thematic_area =first(thematic_area),
          question_desc = first(question_desc),
          score_all = round( mean(as.numeric(get(score_to_display)),na.rm = TRUE),0),
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        group_by(thematic_desc,risk_scenario)%>%
        reframe(
          thematic_area = first(thematic_area),
          "Community score" = round(sum(as.numeric(`Community score`),na.rm = TRUE),0),
          communities=max(communities),
          questions = n()
        )%>%
        arrange(risk_scenario,thematic_area)%>%
        select(thematic_area,thematic_desc,communities,questions,risk_scenario,"Community score")%>%
        pivot_wider(id_cols = thematic_area:questions , names_from =risk_scenario , values_from = `Community score`)
      
      if(nrow(chartDataTable) >1){
        if(input$riskController == "all" || input$communityController == "all"){
          last_two_col_names <- names(chartDataTable)[(ncol(chartDataTable)-1):ncol(chartDataTable)]
          
        }else{
          last_two_col_names <- names(chartDataTable)[ncol(chartDataTable)]
        }
        
        chartDataTable%<>%
          rowwise()%>%
          mutate(
            `total risk scenarios` = ifelse(input$riskController == "all" || input$communityController == "all" , 
                                            get(last_two_col_names[1]) + get(last_two_col_names[2]) , 
                                            get(last_two_col_names[1]))
          )%>%
          pivot_longer(cols =c(last_two_col_names,'total risk scenarios') ,names_to = 'risk_scenario' , values_to = 'score' )%>%
          select(risk_scenario,thematic_area,thematic_desc,communities,questions,score)%>%
          rowwise()%>%
          mutate(questions = ifelse(risk_scenario == 'total risk scenarios',as.numeric(questions)*length(last_two_col_names),as.numeric(questions)),
                 max_score =5*as.numeric(questions),
                 `Resilence %` = round((score/max_score)*100,0),
                 `Resilience Level` = calculateResilenceState(round((score/max_score)*100,0)))%>%
          mutate(
            `Resilence %` = paste(`Resilence %`,'%',sep = '')
          )%>%
          select(thematic_area,thematic_desc,communities,risk_scenario,`Resilence %`)%>%
          pivot_wider(id_cols = thematic_area:communities , names_from =risk_scenario ,values_from = `Resilence %`)
          
        
      }
      
      
      
      datatable(chartDataTable,rownames = FALSE,
                extensions = c('Buttons', 'RowGroup'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting

                  pageLength = 30,
                  autoWidth = TRUE,
                  scrollX = TRUE,

                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',

                      filename = "Thematic Area Scores comparison",
                      title = "Thematic Area Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',

                      filename = "Thematic Area Scores comparison",
                      title = "Thematic Area Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',

                      filename = "Thematic Area Scores comparison",
                      title = "Thematic Area Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Thematic Area Scores comparison',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',

                      filename = "Thematic Area Scores comparison",
                      title = "Thematic Area Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )

                  )
                ))

    })
    
    output$thematicScoreChart <- renderPlotly({
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      
      chartDataTable <- main_survey_part_b %>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        group_by(qno,thematic_desc,risk_scenario)%>%
        reframe(
          risk_scenario = ifelse((input$communityController) == "all" &&  input$riskController == "all", risk_scenario , 
                                 paste(risk_scenario,risk_scenario_value , sep = "-")) ,
          thematic_area =first(thematic_area),
          question_desc = first(question_desc),
          score_all = round( mean(as.numeric(get(score_to_display)),na.rm = TRUE),0),
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        group_by(thematic_desc,risk_scenario)%>%
        reframe(
          thematic_area = first(thematic_area),
          "Community score" = round(sum(as.numeric(`Community score`),na.rm = TRUE),0),
          communities=max(communities),
          questions = n()
        )%>%
        arrange(risk_scenario,thematic_area)%>%
        select(thematic_area,thematic_desc,communities,questions,risk_scenario,"Community score")%>%
        pivot_wider(id_cols = thematic_area:questions , names_from =risk_scenario , values_from = `Community score`)
      
      if(nrow(chartDataTable) >1){
        if(input$riskController == "all" || input$communityController == "all"){
          last_two_col_names <- names(chartDataTable)[(ncol(chartDataTable)-1):ncol(chartDataTable)]
          
        }else{
          last_two_col_names <- names(chartDataTable)[ncol(chartDataTable)]
        }
        
        chartDataTable%<>%
          rowwise()%>%
          mutate(
            `total risk scenarios` = ifelse(input$riskController == "all" || input$communityController == "all" , 
                                            get(last_two_col_names[1]) + get(last_two_col_names[2]) , 
                                            get(last_two_col_names[1]))
          )%>%
          pivot_longer(cols =c(last_two_col_names,'total risk scenarios') ,names_to = 'risk_scenario' , values_to = 'score' )%>%
          select(risk_scenario,thematic_area,thematic_desc,communities,questions,score)%>%
          rowwise()%>%
          mutate(questions = ifelse(risk_scenario == 'total risk scenarios',as.numeric(questions)*length(last_two_col_names),as.numeric(questions)),
                 max_score =5*as.numeric(questions),
                 `Resilence %` = round((score/max_score)*100,0),
                 `Resilience Level` = calculateResilenceState(round((score/max_score)*100,0)))%>%
          mutate(
            `Resilence %` = paste(`Resilence %`,'%',sep = '')
          )%>%
          select(risk_scenario,thematic_area,thematic_desc,communities,questions,score,max_score,`Resilence %`,`Resilience Level`)%>%
          arrange(risk_scenario,thematic_area)
        
        fig <- plot_ly(chartDataTable, 
                       x = ~thematic_area, 
                       y = ~score, 
                       type = 'bar', 
                       name = ~risk_scenario,
                       color = ~risk_scenario,  # Use color to differentiate groups
                       text = ~paste(score,"(",`Resilence %`,")"),  # Display score at the top of each bar
                       textposition = 'outside',  # Ensures text is outside the bar
                       hoverinfo = 'text',
                       hovertext = ~paste("Risk Scenario:", risk_scenario, "<br>Desc:", thematic_desc,"<br>Resilience Level:",`Resilience Level`),
                       split = ~risk_scenario)  # Use split instead of group
        
        fig <- fig %>% layout(barmode = 'group',title = list(text = 'Thematic area Resilience scores Summary', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top',
                                                             pad = list(t = 2)),
                              margin = list(l = 50, r = 50, b = 50, t = 150, pad = 10),
                              legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center", yanchor = "bottom"),
                              xaxis = list(title="Thematic Area",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                              yaxis = list(title="Score",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
        
      }else{
        plot_ly()
      }
      
      
      
    })
    
    
    output$theRiskScenarioType3 <- renderUI({
      
      if((input$communityController) == "all"){
        return("All Communities")
      }else{
        if(input$riskController == "all"){
          return("All Risk Scenarios")
        }else{
          gsub("_", " ", first(main_survey_part_b$risk_scenario))
        }
        
      }
      
    })
    
    output$theRiskScenarioValue3 <- renderUI({
      
      gsub("_", " ", input$riskController)
    })
    
    output$communityName3 <- renderUI({
      
      if((input$communityController) == "all"){
        return("All Communities")
      }else{
        
        gsub("_", " ", first(main_survey_part_b$Community_id))
      }
      
    })
    
    output$CommunityDistrict3 <- renderUI({
      
      if((input$communityController) == "all"){
        
        gsub("_", " ", input$districtController)
      }else{
        
        gsub("_", " ", first(main_survey_part_b$District_id))
      }
      
    })
    
    output$systemScoreTable <- renderDT({
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      
      chartDataTable <- main_survey_part_b %>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        separate_rows(question_system, sep = "#")%>%
        group_by(qno,question_system,risk_scenario)%>%
        reframe(
          risk_scenario = ifelse((input$communityController) == "all" &&  input$riskController == "all", risk_scenario , 
                                 paste(risk_scenario,risk_scenario_value , sep = "-")) ,
          question_desc = first(question_desc),
          score_all = round(mean(as.numeric(get(score_to_display)),na.rm = TRUE),0),
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        group_by(question_system,risk_scenario)%>%
        reframe(
          "Community score" = round(sum(as.numeric(`Community score`),na.rm = TRUE),0),
          communities=max(communities),
          questions = n()
        )%>%
        arrange(risk_scenario,question_system)%>%
        select(question_system,communities,questions,risk_scenario,"Community score")%>%
        pivot_wider(id_cols = question_system:questions , names_from =risk_scenario , values_from = `Community score`)
      
      if(nrow(chartDataTable) >1){
        if(input$riskController == "all" || input$communityController == "all"){
          last_two_col_names <- names(chartDataTable)[(ncol(chartDataTable)-1):ncol(chartDataTable)]
          
        }else{
          last_two_col_names <- names(chartDataTable)[ncol(chartDataTable)]
        }
        
        chartDataTable%<>%
          rowwise()%>%
          mutate(
            `total risk scenarios` = ifelse(input$riskController == "all" || input$communityController == "all" , 
                                            get(last_two_col_names[1]) + get(last_two_col_names[2]) , 
                                            get(last_two_col_names[1]))
          )%>%
          pivot_longer(cols =c(last_two_col_names,'total risk scenarios') ,names_to = 'risk_scenario' , values_to = 'score' )%>%
          select(risk_scenario,question_system,communities,questions,score)%>%
          rowwise()%>%
          mutate(questions = ifelse(risk_scenario == 'total risk scenarios',as.numeric(questions)*length(last_two_col_names),as.numeric(questions)),
                 max_score =5*as.numeric(questions),
                 `Resilence %` = round((score/max_score)*100,0),
                 `Resilience Level` = calculateResilenceState(round((score/max_score)*100,0)))%>%
          mutate(
            `Resilence %` = paste(`Resilence %`,'%',sep = '')
          )%>%
          select(risk_scenario,question_system,communities,questions,score,max_score,`Resilence %`,`Resilience Level`)%>%
          arrange(risk_scenario,question_system)
        
      }else{
        chartDataTable %<>%
          mutate(
            `Resilience Level` = NA
          )
      }
      
      
      
      
      # datatable(chartDataTable)
      
      datatable(chartDataTable,rownames = FALSE,
                extensions = c('Buttons', 'RowGroup'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting
                  autoWidth = TRUE,
                  pageLength = 30,
                  rowGroup = list(dataSrc=c(0),
                                  startRender = JS(
                                    "function(rows, group) {",
                                    "  var style ='background-color: #008BA8;';",
                                    "  var td = `<td style='${style}' colspan=8>${group}</td>`;",
                                    "  return $(`<tr>${td}</tr>`);",
                                    "}"
                                  )),
                  columnDefs = list(list(visible=FALSE, targets=c(0),orderable = FALSE),
                                    list(targets = "_all", orderable = FALSE)),
                  
                  dom = 'Bt',
                  buttons = list(
                    list(
                      extend = 'csv',
                      
                      filename = "System Scores",
                      title = "System Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',
                      
                      filename = "System Scores",
                      title = "System Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',
                      
                      filename = "System Scores",
                      title = "System Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'System Scores',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      
                      filename = "Thematic Area Scores",
                      title = "Thematic Area Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )
                    
                  )
                ))%>%
        formatStyle(
          'Resilience Level',
          backgroundColor = styleEqual(c('Very Low Resilience', 'Low Resilience', 'Medium Resilience','Close to Resilience', 'Resilience'), 
                                       c("#dc3545", "#FFC000", "#FFD966", "#A8D08C","#00B050"))
        )
      
    })
    
    output$systemScorePercentageComparisonTable <- renderDT({
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      chartDataTable <- main_survey_part_b %>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        separate_rows(question_system, sep = "#")%>%
        group_by(qno,question_system,risk_scenario)%>%
        reframe(
          risk_scenario = ifelse((input$communityController) == "all" &&  input$riskController == "all", risk_scenario , 
                                 paste(risk_scenario,risk_scenario_value , sep = "-")) ,
          
          question_desc = first(question_desc),
          score_all = round( mean(as.numeric(get(score_to_display)),na.rm = TRUE),0),
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        group_by(question_system,risk_scenario)%>%
        reframe(
          "Community score" = round(sum(as.numeric(`Community score`),na.rm = TRUE),0),
          communities=max(communities),
          questions = n()
        )%>%
        arrange(risk_scenario,question_system)%>%
        select(question_system,communities,questions,risk_scenario,"Community score")%>%
        pivot_wider(id_cols = question_system:questions , names_from =risk_scenario , values_from = `Community score`)
      
      if(nrow(chartDataTable) >1){
        if(input$riskController == "all" || input$communityController == "all"){
          last_two_col_names <- names(chartDataTable)[(ncol(chartDataTable)-1):ncol(chartDataTable)]
          
        }else{
          last_two_col_names <- names(chartDataTable)[ncol(chartDataTable)]
        }
        
        chartDataTable%<>%
          rowwise()%>%
          mutate(
            `total risk scenarios` = ifelse(input$riskController == "all" || input$communityController == "all" , 
                                            get(last_two_col_names[1]) + get(last_two_col_names[2]) , 
                                            get(last_two_col_names[1]))
          )%>%
          pivot_longer(cols =c(last_two_col_names,'total risk scenarios') ,names_to = 'risk_scenario' , values_to = 'score' )%>%
          select(risk_scenario,question_system,communities,questions,score)%>%
          rowwise()%>%
          mutate(questions = ifelse(risk_scenario == 'total risk scenarios',as.numeric(questions)*length(last_two_col_names),as.numeric(questions)),
                 max_score =5*as.numeric(questions),
                 `Resilence %` = round((score/max_score)*100,0),
                 `Resilience Level` = calculateResilenceState(round((score/max_score)*100,0)))%>%
          mutate(
            `Resilence %` = paste(`Resilence %`,'%',sep = '')
          )%>%
          select(question_system,communities,risk_scenario,`Resilence %`)%>%
          pivot_wider(id_cols = question_system:communities , names_from =risk_scenario ,values_from = `Resilence %`)
        
        
      }
      
      
      datatable(chartDataTable,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting
                  autoWidth = TRUE,
                  scrollX = TRUE,
                  pageLength = 30,
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',

                      filename = "system sector Scores comparison",
                      title = "system sector Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',

                      filename = "system sector Scores comparison",
                      title = "system sector Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',

                      filename = "system sector Scores comparison",
                      title = "system sector Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'system sector Scores comparison',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',

                      filename = "system sector Scores comparison",
                      title = "system sector Scores comparison",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )

                  )
                ))
      
    })
    
    output$systemScoreChart <- renderPlotly({
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      
      chartDataTable <- main_survey_part_b %>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        separate_rows(question_system, sep = "#")%>%
        group_by(qno,question_system,risk_scenario)%>%
        reframe(
          risk_scenario = ifelse((input$communityController) == "all" &&  input$riskController == "all", risk_scenario , 
                                 paste(risk_scenario,risk_scenario_value , sep = "-")) ,
         
          question_desc = first(question_desc),
          score_all = round( mean(as.numeric(get(score_to_display)),na.rm = TRUE),0),
          communities = n()
        )%>%
        mutate(
          "Community score"=score_all
        )%>%
        group_by(question_system,risk_scenario)%>%
        reframe(
         
          "Community score" = round(sum(as.numeric(`Community score`),na.rm = TRUE),0),
          communities=max(communities),
          questions = n()
        )%>%
        arrange(risk_scenario,question_system)%>%
        select(question_system,communities,questions,risk_scenario,"Community score")%>%
        pivot_wider(id_cols = question_system:questions , names_from =risk_scenario , values_from = `Community score`)
      
      if(nrow(chartDataTable) >1){
        if(input$riskController == "all" || input$communityController == "all"){
          last_two_col_names <- names(chartDataTable)[(ncol(chartDataTable)-1):ncol(chartDataTable)]
          
        }else{
          last_two_col_names <- names(chartDataTable)[ncol(chartDataTable)]
        }
        
        chartDataTable%<>%
          rowwise()%>%
          mutate(
            `total risk scenarios` = ifelse(input$riskController == "all" || input$communityController == "all" , 
                                            get(last_two_col_names[1]) + get(last_two_col_names[2]) , 
                                            get(last_two_col_names[1]))
          )%>%
          pivot_longer(cols =c(last_two_col_names,'total risk scenarios') ,names_to = 'risk_scenario' , values_to = 'score' )%>%
          select(risk_scenario,question_system,communities,questions,score)%>%
          rowwise()%>%
          mutate(questions = ifelse(risk_scenario == 'total risk scenarios',as.numeric(questions)*length(last_two_col_names),as.numeric(questions)),
                 max_score =5*as.numeric(questions),
                 `Resilence %` = round((score/max_score)*100,0),
                 `Resilience Level` = calculateResilenceState(round((score/max_score)*100,0)))%>%
          mutate(
            `Resilence %` = paste(`Resilence %`,'%',sep = '')
          )%>%
          select(risk_scenario,question_system,communities,questions,score,max_score,`Resilence %`,`Resilience Level`)%>%
          arrange(risk_scenario,question_system)
        
        fig <- plot_ly(chartDataTable, 
                       x = ~question_system, 
                       y = ~score, 
                       type = 'bar', 
                       name = ~risk_scenario,
                       color = ~risk_scenario,  # Use color to differentiate groups
                       text = ~paste(score,"(",`Resilence %`,")"),  # Display score at the top of each bar
                       textposition = 'outside',  # Ensures text is outside the bar
                       hoverinfo = 'text',
                       hovertext = ~paste("Risk Scenario:", risk_scenario, "<br>Desc:", question_system,"<br>Resilience Level:",`Resilience Level`),
                       split = ~risk_scenario)  # Use split instead of group
        
        fig <- fig %>% layout(barmode = 'group',title = list(text = 'Thematic area Resilience scores Summary', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top',pad =2),
                              margin = list(l = 50, r = 50, b = 50, t = 150, pad = 10),
                              legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center", yanchor = "bottom"),
                              xaxis = list(title="System Sectors",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                              yaxis = list(title="Scores",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
        
      }else{
        plot_ly()
      }
      
     
      
    })
    
    output$communityScoresChart <- renderDT({
      
      if(is.null(input$selectedspiderCheckbox)){
        score_to_display <- "score_all"
      }else{
        score_to_display <- input$selectedspiderCheckbox
      }
      
      chartDataTable <- main_survey_part_b %>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        group_by(qno,question_desc,Community_id)%>%
        reframe(
          Member_id = first(Member_id),
          Region_id = first(Region_id),
          District_id = first(District_id),
          score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
          score_male = round(mean(as.numeric(score_male),na.rm = TRUE),0),
          score_female = round(mean(as.numeric(score_female),na.rm = TRUE),0)
        )%>%
        group_by(Community_id)%>%
        reframe(
          Member_id = first(Member_id),
          Region_id = first(Region_id),
          District_id = first(District_id),
          score  = round(sum(as.numeric(get(score_to_display)),na.rm = TRUE),0),
        )%>%
        rowwise()%>%
        mutate(
          max_score = 150,
          percentage = round((as.numeric(score)/max_score)*100,0),
          resilience_level = calculateResilenceState(percentage),
          percentage = paste(percentage,"%",sep = ""),
        )%>%
        select(Community_id,Member_id,Region_id,District_id ,score,percentage ,resilience_level)
      
      datatable(chartDataTable,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting
                  
                  pageLength = 30,
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Community Scores',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )
                    
                  )
                ))%>%
        formatStyle(
          'resilience_level',
          backgroundColor = styleEqual(c('Very Low Resilience', 'Low Resilience', 'Medium Resilience','Close to Resilience', 'Resilience'), 
                                       c("#dc3545", "#FFC000", "#FFD966", "#A8D08C","#00B050"))
        )
    })
  

    output$communityScoresComparison <- renderDT({
      
    
      
      chartDataTable <- main_survey_part_b %>%
        dplyr::rename(score_male ="male score", score_female = "female score")%>%
        group_by(qno,question_desc,Community_id)%>%
        reframe(
          Member_id = first(Member_id),
          Region_id = first(Region_id),
          District_id = first(District_id),
          score_all = round(mean(as.numeric(score_all),na.rm = TRUE),0),
          score_male = round(mean(as.numeric(score_male),na.rm = TRUE),0),
          score_female = round(mean(as.numeric(score_female),na.rm = TRUE),0)
        )%>%
        group_by(Community_id)%>%
        reframe(
          Member_id = first(Member_id),
          Region_id = first(Region_id),
          District_id = first(District_id),
          community_score = round(sum(as.numeric(score_all),na.rm = TRUE),0),
          score_male = round(sum(as.numeric(score_male),na.rm = TRUE),0),
          score_female = round(sum(as.numeric(score_female),na.rm = TRUE),0)
        )%>%
        select(Community_id,Member_id,Region_id,District_id ,community_score ,score_male,score_female)
      
      datatable(chartDataTable,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting
                  
                  pageLength = 30,
                  dom = 'lBfrtip',
                  buttons = list(
                    list(
                      extend = 'csv',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'excel',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    ),
                    list(
                      extend = 'pdf',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           )),
                      customize = JS(
                        "function(doc) {",
                        "doc.content.splice(0, 0, {",
                        "text: 'Community Scores',",
                        "fontSize: 18,",
                        "alignment: 'center'",
                        "});",
                        "}"
                      )
                    ),
                    list(
                      extend = 'print',
                      
                      filename = "Community Scores",
                      title = "Community Scores",
                      exportOptions = list(modifier = list(page = 'all'),
                                           format = list(
                                             footer = TRUE
                                           ))
                    )
                    
                  )
                ))
    })
    
    output$memberCommunityReport <- renderDT({
      memberCommunityReport <-main_survey_part_a %>%
        group_by(Community_id)%>%
        reframe(
          total_interviews = n()
        )
      
      merged <- main_communities %>% left_join(memberCommunityReport , by = c("community"="Community_id")) %>%
        replace_na(list(total_interviews = 0))
      
      datatable(merged,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting
                  
                  pageLength = 30,
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
                ))
      
    })
    
    output$memberCommunitySummary <- renderDT({
      memberCommunitySummary <-main_communities %>%
        group_by(member)%>%
        reframe(
          assigned_communities = n()
        )
      
      memberCommunityReport <-main_survey_part_a %>%
        mutate(tab_date = format(tab_date, "%Y"))%>%
        group_by(Community_id,tab_date)%>%
        reframe(
          Member_id = first(Member_id)
        )%>%
        group_by(Member_id)%>%
        reframe(
          interviewed_communities = n()
        )
      
      merged <- memberCommunitySummary %>% left_join(memberCommunityReport ,by = c("member"="Member_id"))%>%
        select(member , assigned_communities,interviewed_communities)
      
      datatable(merged,rownames = FALSE,
                extensions = c('Buttons'),
                selection = "none",
                options = list(
                  order = list(),  # Disable initial sorting
                  
                  pageLength = 30,
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
                ))
    })
    
  })

  
  
}

