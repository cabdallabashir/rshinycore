

get_data <- function(username, password ,formID){
  
  # if(memberID == "CMU"){
  #   curl <-paste("https://api.ona.io/api/v1/data/",formID,sep = "")
  # }else{
  #   curl <-paste('https://api.ona.io/api/v1/data/',formID,'?query={"enum_location_info/member":"',memberID,'"}',sep = '')
  # }
  curl <-paste("https://api.ona.io/api/v1/data/",formID,sep = "")
  response_data <- GET(url = curl, authenticate(username, password))
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
distrcit_screening_target <- readxl::read_excel("docs/oasis_baseline_comunity_targets.xlsx",sheet = "distrcit_screening_target")
segrationDropdown <- names(baseline_targets[c("member","district","region")])

data_enumirators <- readxl::read_excel("docs/oasis_enumirators.xlsx")

data_enumirators$enumirator_id <- gsub("['’]", "", data_enumirators$enumirator_id )




oasisBaselineRegistrationScreeningUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/oasisBaselineRegistrationScreening.html",
                 
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
                 
                 warningToLoadDATA = uiOutput(ns("warningToLoadDATA")),
                 Segrigation_element = uiOutput(ns("Segrigation_element")),
                 communityScores = DTOutput(ns("communityScores"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 communityScoresDetails = DTOutput(ns("communityScoresDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 memberScoresDetails = DTOutput(ns("memberScoresDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 regionScoresDetails = DTOutput(ns("regionScoresDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 districtScoresDetails = DTOutput(ns("districtScoresDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 non_consented = uiOutput(ns("non_consented")),
                 consented = uiOutput(ns("consented")),
                 interviewStatusChart =   plotlyOutput(ns("interviewStatusChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 genderChart = plotlyOutput(ns("genderChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 FemaleHH = uiOutput(ns("FemaleHH")),
                 maleHH = uiOutput(ns("maleHH")),
                 debitchart = plotlyOutput(ns("debitchart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 worktypechart = plotlyOutput(ns("worktypechart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 pwdNumberChart =plotlyOutput(ns("pwdNumberChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 pwdphysicalCate = plotlyOutput(ns("pwdphysicalCate"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 pwdmentalCate = plotlyOutput(ns("pwdmentalCate"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 avarage_age_hh = uiOutput(ns("avarage_age_hh")),
                 avarage_debt =uiOutput(ns("avarage_debt")),
                 avarage_income = uiOutput(ns("avarage_income")),
                 topTenSpiderChart = plotlyOutput(ns("topTenSpiderChart"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 topTenSpiderChartbyCategory= plotlyOutput(ns("topTenSpiderChartbyCategory"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 scoreFrequency = plotlyOutput(ns("scoreFrequency"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 communityScoresCategoryDetails = DTOutput(ns("communityScoresCategoryDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 scoreCategory = plotlyOutput(ns("scoreCategory"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 segrigation_target = uiOutput(ns("segrigation_target")),
                 segrigation_name_as_target = uiOutput(ns("segrigation_name_as_target")),
                 scoreQuestion =  plotlyOutput(ns("scoreQuestion"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7)
    )
  )
}

oasisBaselineRegistrationScreening <- function(input ,output , session,sharedValues){
  
 
  
  
  global_vars <- reactiveValues(
    global_baseline_survey = NULL,
    main_survey = NULL,
    memberToFilter = NULL
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
      
      Authresult <- validate_member_ona_credentials(onauser,onapass,"794593")
      
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
      
      baseline_survey  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","794593")
      
      required_columns <- c("enum_name","member","region","district","community",'reg_activity_id',"age_cfw_benef",
                            "consent","benef_name","benef_sex","benef_age","tel_number","benef_hoh","HoH_fullname",
                            "hoh_age","marital_status_hoh","hh_sex","hhsize","under_5","elderly","pregnant_lactating",
                            "contribute_income","pwd_physical","pwd_mental","pwd_physical_number","pwd_mental_number",
                            "montly_income","amount_debt","contribute_income","type_work","fcs" , "HDDS",
                            "X_submission_time","start","end","debt","Disability_Type_mental","Disability_Type_physical","hh_geopoint",
                            "hh_geopoint_manual"
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
        
        
        columns_to_replace_na <- c("contribute_income", "montly_income", "pwd_mental_number",
                                   "pwd_physical_number", "pregnant_lactating", "under_5",
                                   "hhsize", "elderly","amount_debt")
        
        main_survey_analysis <- baseline_survey %>%
          select(
            X_id,enum_name,member,region,district,community,reg_activity_id,age_cfw_benef,
            consent,benef_name,benef_sex,benef_age,tel_number,benef_hoh,HoH_fullname,
            hoh_age,marital_status_hoh,hh_sex,hhsize,under_5,elderly,pregnant_lactating,
            contribute_income,pwd_physical,pwd_mental,pwd_physical_number,pwd_mental_number,
            montly_income,amount_debt,contribute_income,type_work,fcs,fcsCat , HDDS,
            X_submission_time,start,end,debt,Disability_Type_mental,Disability_Type_physical,
            hh_geopoint,  hh_geopoint_manual
          ) %>%
          mutate(
            across(all_of(columns_to_replace_na), ~replace(., is.na(.), 0)),
            district = gsub("\\s+", "", district),
            community = gsub("\\s+", "", community),
            tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
            bens_id = X_id,
            HoH_fullname = ifelse(is.na(HoH_fullname) , benef_name ,HoH_fullname ),
            hoh_sex= ifelse(is.na(hh_sex) ,benef_sex,hh_sex),
            hoh_age= ifelse(is.na(hoh_age) ,benef_age,hoh_age),
            start_time = format(parse_date_time(start, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            end_time = format(parse_date_time(end, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
            
            Household_size_score = ifelse(as.numeric(hhsize) <= 6 , 0 ,2 ),
            elderly_score = ifelse(as.numeric(elderly) == 0 , 0 ,
                                   ifelse(as.numeric(elderly) == 1,1,2)
            ),
            woman_h_hh_score = ifelse(hoh_sex == "female" & benef_hoh == "1",5,0),
            pregnant_lactating_score = ifelse(as.numeric(pregnant_lactating) == 0 ,0,2),
            under_5_score = ifelse(as.numeric(under_5) == 0 ,0,
                                   ifelse(as.numeric(under_5) == 1,1,3)
            ),
            pwd_physical_score = ifelse(as.numeric(pwd_physical_number) ==0 ,0,
                                        ifelse(as.numeric(pwd_physical_number) == 1 , 2 , 3 )
            ),
            pwd_mental_score = ifelse(as.numeric(pwd_mental_number) ==0 ,0,
                                      ifelse(as.numeric(pwd_mental_number) == 1 , 2 , 3 )
            ),
            monthly_income_score = ifelse(as.numeric(montly_income) < 100,5,0),
            debt_score = ifelse(as.numeric(amount_debt) >200,5,0),
            income_contribution_score = ifelse(as.numeric(contribute_income) == 0,5,0),
            type_work_score = ifelse(type_work == "temporary",2,0),
            fcs_score = ifelse(fcsCat == "Poor",10 , 
                               ifelse(fcsCat == "Borderline",5,0) 
            ),
            hdds_score = ifelse(as.numeric(HDDS) < 7,10,0),
            total_score =  Household_size_score + elderly_score + woman_h_hh_score + # Be careful, you've listed woman_h_hh_score twice
              pregnant_lactating_score + under_5_score + pwd_physical_score + pwd_mental_score +
              monthly_income_score + debt_score + income_contribution_score + type_work_score +
              fcs_score + hdds_score
          )%>%
          select(
            tab_date,start_time,end_time,bens_id,enum_name,member,region,district,community,reg_activity_id,age_cfw_benef,
            consent,benef_name,benef_sex,benef_age,tel_number,benef_hoh,HoH_fullname,debt,
            hoh_age,marital_status_hoh,hoh_sex,hhsize,under_5,elderly,pregnant_lactating,
            contribute_income,pwd_physical,pwd_mental,pwd_physical_number,pwd_mental_number,
            montly_income,amount_debt,contribute_income,type_work,fcs ,fcsCat, HDDS,
            X_submission_time,Household_size_score,elderly_score,woman_h_hh_score,woman_h_hh_score,pregnant_lactating_score,under_5_score,
            pwd_physical_score,pwd_mental_score,monthly_income_score,debt_score,income_contribution_score,type_work_score,fcs_score,
            hdds_score,total_score,Disability_Type_mental,Disability_Type_physical,hh_geopoint,  hh_geopoint_manual
          )
        
        
        
        main_survey_analysis$enum_name <- gsub("['’]", "", main_survey_analysis$enum_name)
        
        tables <- dbListTables(postgresConnection)
        if("oasisScreening" %in% tables){
          existing_ids <- dbGetQuery(postgresConnection, 'SELECT bens_id FROM "oasisScreening"')
          df_to_insert <- main_survey_analysis[!main_survey_analysis$bens_id %in% existing_ids$bens_id,]
          
          if (nrow(df_to_insert) > 0) {
            print("Writing To Database..")
            dbWriteTable(postgresConnection, name = "oasisScreening", value = df_to_insert, append = TRUE, row.names = FALSE)
          } else {
            print("No new records to insert.")
          }
        }else{
          dbWriteTable(postgresConnection , "oasisScreening" , main_survey_analysis)
        }
        
        if(Authresult$message != 'CMU'){
          main_survey_analysis <- dbGetQuery(postgresConnection,sprintf('SELECT * FROM "oasisScreening" WHERE access_status = \'A\' and member = \'%s\'', Authresult$message) )
        }else{
          main_survey_analysis <- dbGetQuery(postgresConnection,'SELECT * FROM "oasisScreening" WHERE access_status = \'A\'')
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
  
  
  
  # 
  
  

  output$Segrigation_element <- renderUI({
    ns1 <- NS("oasisBaselineRegistrationScreening")
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
    
   
    
    main_survey <- global_vars$global_baseline_survey %>%
      filter(as.Date(tab_date) >= as.Date(input$fromDateController) &
               as.Date(tab_date) <= as.Date(input$toDateController))
    
    screening_target = distrcit_screening_target
   

    if(!is.null((input$Segrigation_element_drill))){
      if((input$Segrigation_element_drill) != "all" & !is.null((input$Segrigation))){
      
        main_survey %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )
        
        screening_target %<>% filter(
          !!sym(input$Segrigation) == input$Segrigation_element_drill
        )

     
      }

    }
    
    # memberToFilter <- global_vars$memberToFilter
    # 
    # if(memberToFilter != "CMU"){
    #   main_survey %<>% filter(
    #     member == memberToFilter
    #   )
    # }
    # 
    
    
    output$communityScores <- renderDT(server = FALSE,{
      top_target <- sum(as.numeric(screening_target$r_target),na.rm = TRUE)
      
      communityScoresTable <- main_survey%>%
        filter(consent == "1")%>%
        select(
          tab_date,bens_id,HoH_fullname,enum_name,member,region,district,community,reg_activity_id,
          tel_number,hoh_age,hoh_sex,hhsize,total_score
        )%>%
        arrange(desc(total_score))%>%
        slice_head(n = top_target)
      
      datatable(
        communityScoresTable,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 10, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Community Scores",
              title = "Community Scores",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Community Scores",
              title = "Community Scores",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Community Scores",
              title = "Community Scores",
              exportOptions = list(modifier = list(page = 'all')),
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
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
    })
    
    output$communityScoresDetails <- renderDT(server = FALSE,{
      top_target <- sum(as.numeric(screening_target$r_target),na.rm = TRUE)
      
      communityScoresDetails <- main_survey%>%
        filter(consent == "1")%>%
        select(
          tab_date,bens_id,HoH_fullname,member,region,district,community,tel_number,
          Household_size_score,elderly_score,woman_h_hh_score,woman_h_hh_score,pregnant_lactating_score,under_5_score,
          pwd_physical_score,pwd_mental_score,monthly_income_score,debt_score,income_contribution_score,type_work_score,fcs_score,
          hdds_score,total_score
        )%>%
        arrange(desc(total_score))%>%
        slice_head(n = top_target)
        
      
      datatable(
        communityScoresDetails,
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 10, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Community Scores Details",
              title = "Community Scores",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Community Scores Details",
              title = "Community Scores Details",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Community Scores Details",
              title = "Community Scores Details",
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Community Scores Details',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = "Community Scores Details",
              title = "Community Scores Details",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
    })
    
    output$communityScoresCategoryDetails <- renderDT(server = FALSE,{
      communityScoresCategoryDetails <- main_survey %>%
        filter(consent == "1")%>%
        select(
          tab_date,bens_id,member,region,district,community,HoH_fullname,tel_number,total_score,
          Household_size_score,elderly_score,woman_h_hh_score,pregnant_lactating_score,under_5_score,
          pwd_physical_score,pwd_mental_score,monthly_income_score,debt_score,income_contribution_score,type_work_score,fcs_score,
          hdds_score
        )%>%
        arrange(desc(total_score))%>%
        slice_head(n = 10)%>%
        pivot_longer(cols = Household_size_score:hdds_score , names_to = "variable" , values_to = "value") %>%
        mutate(
          `category` = case_when(
            variable %in% c("Household_size_score",
                            "elderly_score",
                            "woman_h_hh_score",
                            "pregnant_lactating_score",
                            "under_5_score") ~ "Household vulnerability",
            variable %in% c("pwd_physical_score",
                            "pwd_mental_score") ~ "Disability",
            variable %in% c("monthly_income_score",
                            "debt_score",
                            "income_contribution_score",
                            "type_work_score") ~ "Income",
            variable %in% c("fcs_score",
                            "hdds_score") ~ "Food Security",
          )
        )%>%
        select(tab_date,bens_id,member,region,district,community,HoH_fullname,tel_number,total_score,category,value)%>%
        group_by(tab_date,bens_id,member,region,district,community,HoH_fullname,tel_number,total_score,category)%>%
        reframe(
          value = sum(value,na.rm = TRUE)
        )%>%
        pivot_wider(id_cols = tab_date:total_score , names_from = category , values_from = value)
      
      datatable(
        communityScoresCategoryDetails,
        
        extensions = 'Buttons', 
        options = list(
          scrollX = TRUE, 
          pageLength = 20, 
          
          dom = 'lBfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = "Community Category Scores",
              title = "Community Category Scores",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              filename = "Community Category Scores",
              title = "Community Category Scores",
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'pdf',
              filename = "Community Category Scores",
              title = "Community Category Scores",
              exportOptions = list(modifier = list(page = 'all')),
              customize = JS(
                "function(doc) {",
                "doc.content.splice(0, 0, {",
                "text: 'Community Category Scores',",
                "fontSize: 18,",
                "alignment: 'center'",
                "});",
                "}"
              )
            ),
            list(
              extend = 'print',
              filename = "Community Category Scores",
              title = "Community Category Scores",
              exportOptions = list(modifier = list(page = 'all'))
            )
            
          )
        )
      )
        
    })
    
    output$memberScoresDetails <- renderDT(server = FALSE,{
      communityScoresDetails <- main_survey%>%
        filter(consent == "1")%>%
        select(
          member,total_score
        )%>%
        group_by(member)%>%
        reframe(
          Min = min(total_score,na.rm = TRUE),
          Max = max(total_score,na.rm = TRUE),
          Avarage = round(mean(total_score,na.rm = TRUE),2)
        )
      
      datatable(
        communityScoresDetails,
        options = list(
          scrollX = TRUE, 
          pageLength = 5, 
          
          dom = 'lt'
        )
      )
    })
    output$regionScoresDetails <- renderDT(server = FALSE,{
      communityScoresDetails <- main_survey%>%
        filter(consent == "1")%>%
        select(
          region,total_score
        )%>%
        group_by(region)%>%
        reframe(
          Min = min(total_score,na.rm = TRUE),
          Max = max(total_score,na.rm = TRUE),
          Avarage = round(mean(total_score,na.rm = TRUE),2)
        )
      
      datatable(
        communityScoresDetails,
        options = list(
          scrollX = TRUE, 
          pageLength = 5, 
          
          dom = 'lt'
        )
      )
    })
    output$districtScoresDetails <- renderDT(server = FALSE,{
      communityScoresDetails <- main_survey%>%
        filter(consent == "1")%>%
        select(
          district,total_score
        )%>%
        group_by(district)%>%
        reframe(
          Min = min(total_score,na.rm = TRUE),
          Max = max(total_score,na.rm = TRUE),
          Avarage = round(mean(total_score,na.rm = TRUE),2)
        )
      
      datatable(
        communityScoresDetails,
        options = list(
          scrollX = TRUE, 
          pageLength = 5, 
          
          dom = 'lt'
        )
      )
    })
    
    output$consented <- renderUI({
      consented <- main_survey%>%
        filter(consent == "1")
      
      nrow(consented)
    })
    
    output$non_consented <- renderUI({
      non_consented <- main_survey%>%
        filter(consent == "2")
      
      nrow(non_consented)
    })
    
    output$interviewStatusChart <- renderPlotly({
      consented <- main_survey%>%
        filter(consent == "1")
      
      
      
      non_consented <- main_survey%>%
        filter(consent == "2")
      
      
      chartData <- data.frame(
        "name" = c("Consented","Non-Consented"),
        "value" = c(nrow(consented),nrow(non_consented)),
        "color" = c("#008BA8","#ED7667")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title = "Minutes",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$maleHH <- renderUI({
      maleHH <- main_survey%>%
        filter(consent == "1" & hoh_sex == "male")
      
      nrow(maleHH)
    })
    
    output$FemaleHH <- renderUI({
      FemaleHH <- main_survey%>%
        filter(consent == "1" & hoh_sex == "female")
      
      nrow(FemaleHH)
    })
    
    output$genderChart <- renderPlotly({
      maleHH <- main_survey%>%
        filter(consent == "1" & hoh_sex == "male")
      
     
      
      FemaleHH <- main_survey%>%
        filter(consent == "1" & hoh_sex == "female")
      
      
      
      
      chartData <- data.frame(
        "name" = c("Male HH","Female HH"),
        "value" = c( nrow(maleHH),nrow(FemaleHH)),
        "color" = c("#008BA8","#ED7667")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title = "Minutes",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$worktypechart <- renderPlotly({
      worktypechart <-  main_survey %>%
        filter(consent == "1") %>%
        group_by(type_work) %>%
        summarise(
          count = n()
        )
      
      
      fig <- plot_ly(worktypechart, labels = ~type_work, values = ~count, type = 'pie',
                     marker = list(colors = c("#40419A","#F99D1E")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    output$debitchart <- renderPlotly({
      debitchart <-  main_survey %>%
        filter(consent == "1") %>%
        group_by(debt) %>%
        summarise(
          count = n()
        )%>%
        mutate(
          debt = case_when(
            debt == "1" ~ "Yes",
            debt == "0" ~ "No",
            .default = debt
          )
        )
      
      
      fig <- plot_ly(debitchart, labels = ~debt, values = ~count, type = 'pie',
                     marker = list(colors = c("#40419A","#F99D1E")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$pwdNumberChart <- renderPlotly({
      pwdNumberChart <- main_survey%>%
        filter(consent == "1")
      
      pwd_physical = sum(as.numeric(pwdNumberChart$pwd_physical_number) ,na.rm = TRUE)
      pwd_mental =sum(as.numeric(pwdNumberChart$pwd_mental_number) ,na.rm = TRUE)
      
      chartData <- data.frame(
        "name" = c("Physically Disabled","Mentally Disabled"),
        "value" = c(pwd_physical,pwd_mental),
        "color" = c("#008BA8","#ED7667")
      )
      
      fig <- plot_ly(chartData, labels = ~name, values = ~value, type = 'pie',marker = list(colors = ~color))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(title = "Minutes",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$pwdphysicalCate <- renderPlotly({
      pwdphysicalCate <-  main_survey %>%
        drop_na(Disability_Type_physical)%>%
        filter(consent == "1") %>%
        separate_rows(Disability_Type_physical, sep = " ")%>%
        group_by(Disability_Type_physical) %>%
        summarise(
          count = n()
        )
      
      
      fig <- plot_ly(pwdphysicalCate, labels = ~Disability_Type_physical, values = ~count, type = 'pie',
                     marker = list(colors = c("#40419A","#008BA8","#ED7667","#F99D1E")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$pwdmentalCate <- renderPlotly({
      pwdmentalCate <-  main_survey %>%
        drop_na(Disability_Type_mental)%>%
        filter(consent == "1") %>%
        separate_rows(Disability_Type_mental, sep = " ")%>%
        group_by(Disability_Type_mental) %>%
        summarise(
          count = n()
        )
      
      
      fig <- plot_ly(pwdmentalCate, labels = ~Disability_Type_mental, values = ~count, type = 'pie',
                     marker = list(colors = c("#40419A","#008BA8","#ED7667","#F99D1E")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    output$avarage_age_hh <- renderUI({
      avarage_age_hh <- main_survey %>%
        drop_na(hoh_age)%>%
        filter(consent == "1")
      
      avarage_age_hh <-round( mean(as.numeric(avarage_age_hh$hoh_age),na.rm = TRUE))
      
    })
    
    output$avarage_income <- renderUI({
      avarage_income <- main_survey %>%
        drop_na(montly_income)%>%
        filter(consent == "1")
      
      avarage_income <- round(mean(as.numeric(avarage_income$montly_income),na.rm = TRUE))
      
    })
    
    output$avarage_debt <- renderUI({
      avarage_income <- main_survey %>%
        drop_na(amount_debt)%>%
        filter(consent == "1")
      
      avarage_income <- round(mean(as.numeric(avarage_income$amount_debt),na.rm = TRUE))
      
    })
    
    output$topTenSpiderChart <- renderPlotly({
      
      communityScoresTable <- main_survey%>%
        filter(consent == "1")%>%
        select(
          community,Household_size_score,elderly_score,woman_h_hh_score,woman_h_hh_score,pregnant_lactating_score,under_5_score,
          pwd_physical_score,pwd_mental_score,monthly_income_score,debt_score,income_contribution_score,type_work_score,fcs_score,
          hdds_score
        )%>%
        pivot_longer(cols = Household_size_score:hdds_score ,names_to = "variable", values_to = "value" )%>%
        group_by(variable)%>%
        reframe(
          value =round( mean(value),2)
        )
      
      max_score <- 10
      tick_values <- seq(0, max_score, length.out = 6)
      
      fig <- plot_ly(communityScoresTable,
                     type = 'scatterpolar',
                     r = ~value,
                     theta = ~variable,
                     fill = 'toself',
                     mode   = 'markers+lines',
                     marker = list(size = 10),
                     fillcolor  = 'rgba(255, 0, 0, 0.2)',  # Set fill color to red
                     line = list(color = '#F99D1E') # Set line color to red
                     
      ) 
      
      
      fig <- fig %>%
        layout(
          title = '',
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
    
    output$topTenSpiderChartbyCategory <- renderPlotly({
      
      topTenSpiderChartbyCategory <-main_survey %>%
        filter(consent == "1")%>%
        select(
          community,
          Household_size_score,elderly_score,woman_h_hh_score,pregnant_lactating_score,under_5_score,
          pwd_physical_score,pwd_mental_score,monthly_income_score,debt_score,income_contribution_score,type_work_score,fcs_score,
          hdds_score
        )%>%
        pivot_longer(cols = Household_size_score:hdds_score ,names_to = "variable", values_to = "value" )%>%
        group_by(variable)%>%
        reframe(
          value =round( mean(value),2)
        )%>%
        mutate(
          `category` = case_when(
            variable %in% c("Household_size_score",
                            "elderly_score",
                            "woman_h_hh_score",
                            "pregnant_lactating_score",
                            "under_5_score") ~ "Household vulnerability",
            variable %in% c("pwd_physical_score",
                            "pwd_mental_score") ~ "Disability",
            variable %in% c("monthly_income_score",
                            "debt_score",
                            "income_contribution_score",
                            "type_work_score") ~ "Income",
            variable %in% c("fcs_score",
                            "hdds_score") ~ "Food Security",
          )
        )%>%
        select(category,value)%>%
        group_by(category)%>%
        reframe(
          value = sum(value,na.rm = TRUE)
        )
      
     
      
      max_score <- 20
      tick_values <- seq(0, max_score, length.out = 6)
      
      fig <- plot_ly(topTenSpiderChartbyCategory,
                     type = 'scatterpolar',
                     r = ~value,
                     theta = ~category,
                     fill = 'toself',
                     mode   = 'markers+lines',
                     marker = list(size = 10),
                     fillcolor  = 'rgba(255, 0, 0, 0.2)',  # Set fill color to red
                     line = list(color = '#F99D1E') # Set line color to red
                     
      ) 
      
      
      fig <- fig %>%
        layout(
          title = '',
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
    
    output$scoreFrequency <- renderPlotly({
      communityScoresTable <- main_survey%>%
        filter(consent == "1")%>%
        select(
          community,total_score
        )%>%group_by(community)%>%
        reframe(total_score = round(mean(as.numeric(total_score),na.rm = TRUE),2))
      
      fig <- plot_ly(communityScoresTable)%>% 
        add_lines(x = ~community, y = ~total_score, name = "line",line = list(color = '#F99D1E'))%>%
        add_markers(x = ~community, y = ~total_score, name = "Market" ,marker = list(color = 'red'))%>%
        add_text(x = ~community, y = ~total_score,text = ~total_score ,name = "text",textfont = list(size = 20))%>%
        add_bars(x = ~community, y = ~total_score, name = "bars",marker = list(color = '#008BA8'))%>%
        layout(
          xaxis = list(tickangle = 45) # Rotate x-axis labels to diagonal (45 degrees)
          
        )
      
      
      fig
    })
    
    output$scoreCategory <- renderPlotly({
      scoreCategory<-main_survey %>%
        filter(consent == "1")%>%
        select(
          Household_size_score,elderly_score,woman_h_hh_score,pregnant_lactating_score,under_5_score,
          pwd_physical_score,pwd_mental_score,monthly_income_score,debt_score,income_contribution_score,type_work_score,fcs_score,
          hdds_score
        )%>%
        pivot_longer(cols = Household_size_score:hdds_score , names_to = "variable" , values_to = "value") %>%
        group_by(variable)%>%
        reframe(
          value =round( mean(value),2)
        )%>%
        mutate(
          `category` = case_when(
            variable %in% c("Household_size_score",
                            "elderly_score",
                            "woman_h_hh_score",
                            "pregnant_lactating_score",
                            "under_5_score") ~ "Household vulnerability",
            variable %in% c("pwd_physical_score",
                            "pwd_mental_score") ~ "Disability",
            variable %in% c("monthly_income_score",
                            "debt_score",
                            "income_contribution_score",
                            "type_work_score") ~ "Income",
            variable %in% c("fcs_score",
                            "hdds_score") ~ "Food Security",
          )
        )%>%
        select(category,value)%>%
        group_by(category)%>%
        reframe(
          value = sum(value,na.rm = TRUE)
        )
      
      fig <- plot_ly(scoreCategory, labels = ~category, values = ~value, type = 'pie',
                     marker = list(colors = c("#40419A","#008BA8","#ED7667","#F99D1E")))
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(
                              orientation = "h", 
                              x = 0.5, 
                              xanchor = "center", 
                              y = -0.1, 
                              yanchor = "top"
                            ))
      
      fig
    })
    
    output$scoreQuestion <- renderPlotly({
      scoreQuestion<-main_survey %>%
        filter(consent == "1")%>%
        select(
          Household_size_score,elderly_score,woman_h_hh_score,pregnant_lactating_score,under_5_score,
          pwd_physical_score,pwd_mental_score,monthly_income_score,debt_score,income_contribution_score,type_work_score,fcs_score,
          hdds_score
        )%>%
        pivot_longer(cols = Household_size_score:hdds_score , names_to = "variable" , values_to = "value") %>%
        group_by(variable)%>%
        reframe(
          value =round( mean(value),2)
        )
      
      fig <- plot_ly(scoreQuestion, labels = ~variable, values = ~value, type = 'pie')
      fig <- fig %>% layout(title = '',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(
                              orientation = "h", 
                              x = 0.5, 
                              xanchor = "center", 
                              y = -0.1, 
                              yanchor = "top"
                            ))
      
      fig
    })
    
    output$segrigation_name_as_target <- renderUI({
      
      tools::toTitleCase(paste(input$Segrigation, "( ",input$Segrigation_element_drill," )" ,"target:"))
    })
    
    output$segrigation_target <- renderUI({
      segrigation_target <- sum(as.numeric(screening_target$r_target),na.rm = TRUE)
    })
    
  })
  
}

