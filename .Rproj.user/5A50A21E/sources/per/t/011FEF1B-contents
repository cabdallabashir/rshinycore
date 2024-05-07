



get_data <- function(username, password ,formID){
  
  
  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/",formID,sep = ""), authenticate(username,password))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
  
    if(nrow(formData) <= 0){
      column_names <- c("crc_name","crc_role","enum_info/enum_name","enum_info/enum_sex","enum_info/enum_phone_number","project_info/Member_id",
                        "project_info/Region_id","project_info/District_id","project_info/Community_id",'consent',"group_verification/verification_category",
                        "HH_selection_/hoh_name","HH_selection_/hoh_sex","HH_selection_/tel_number_","HH_selection_/selection_criteria",
                        "groups_selection_criteria/groups","groups_selection_criteria/group_size","groups_selection_criteria/name",
                        "ind_sex","ind_age","ind_position","tel_number","selection_criteria_",
                        "X_submission_time","start","end" , "index")

      
      formData <- data.frame(matrix(ncol = length(column_names), nrow = 0))
      names(formData) <- column_names
    }else{
      if("groups_selection_criteria/group_members" %in% names(formData)){
        formData %<>%
          unnest("groups_selection_criteria/group_members",keep_empty = TRUE)%>%purrr::discard(is.list) %>%
          as.data.frame()%>%mutate(index = row_number())
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

df_creteria <- data.frame(
  id = 1:24,
  description = c(
    "Qoys si jooga ah cuntada u cunin",
    "Guri masuul katahay qof dumar ah oo aan laqabin",
    "Kala duwanaashaha xadidadan ee noocyada cuntada",
    "Guri uu masuul kayahay qof caruur ah kana yar 18 sano",
    "Iskuhaleynta caawimaada bani aadanimo",
    "Awood la'aan in cuntada lasoo iibsado",
    "Ilmo nafaqadaro heyso",
    "Daqli kayar 65 dollar bisihi",
    "Deyn aad u badan lagu leeyahay",
    "Guri xaaladisa dhismeed wanaagsaneyn",
    "Awood u laheyn in uhelo waxyaabaha asaasiga u ah bani aadamka sida cunto, biyo, caafimad iwm",
    "Isku haleyo cunto raqiis ah ama cunto uusan jecleyn",
    "Cunto soo deynsado ama isku haleyo caawimada asxaabta ama qaraabada",
    "Xadidida inta jeer maalinti ay dadka wax cunaan",
    "Yareynta cunatada dadku cunaan halki mar",
    "Qof dumar ah oo uur leh ama naas nuujineyso",
    "Guri ay ku noolyihin in kabadn 2 caruur 5 sano kayar",
    "Qoys kasoo jeedo dadka laga tiro badanyahay",
    "Qof ka weyn 70 sano",
    "Qof ka weyn 60 sano",
    "Qof lixaadka la'",
    "Qof la xuunsan cudurada raaga",
    "Qof soo barakay (IDP)",
    "Qof qaxooti ah"
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
                 community_chart = plotlyOutput(ns("community_chart"),height = 3000) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 gender_chart = plotlyOutput(ns("gender_chart")) %>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 districtDetails = DTOutput(ns("districtDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 regionDetails = DTOutput(ns("regionDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7),
                 memberDetails = DTOutput(ns("memberDetails"))%>% withSpinner(size=0.5,proxy.height = "50px",type = 7)
                 
    )
  )
}

brcis3selection <- function(input ,output , session,sharedValues){
  
 
  
  
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

    
    baseline_survey  <- get_data(onauser,onapass,"800118")
    print(baseline_survey)
  
    required_columns <- c("crc_name","crc_role","enum_name","enum_sex","enum_phone_number","Member_id","Region_id","District_id","Community_id",'consent',"verification_category",
                          "hoh_name","hoh_sex","tel_number_","selection_criteria","groups","group_size","name",
                          "ind_sex","ind_age","ind_position","tel_number","selection_criteria_",
                          "X_submission_time","start","end","group_members_count" )
    
    # Loop through each required column
    for (col in required_columns) {
      # Check if the column exists
      if (!col %in% names(baseline_survey)) {
        # If it does not exist, add it with NA values
        baseline_survey[[col]] <- NA
      }
    }
    
    if(first(baseline_survey$status) == "200"   & first(baseline_survey$message) != "empty"){
     
      columns_to_replace_na <- c("group_members_count")
      
      main_survey_analysis <- baseline_survey %>%
        select(
          index,X_id,crc_name, enum_name,enum_phone_number,crc_role, Member_id, Region_id, District_id, Community_id, consent, verification_category, 
          hoh_name, hoh_sex, tel_number_, selection_criteria, groups, group_size, name, ind_sex, ind_age, ind_position, 
          tel_number, selection_criteria_, X_submission_time, start, end,group_members_count
        ) %>%
        mutate(
          crc_name = enum_name,
          across(all_of(columns_to_replace_na), ~replace(., is.na(.), 0)),
          tab_date = as.Date(format(parse_date_time(X_submission_time, orders = "Y-m-d H:M:OSz"),"%Y-%m-%d")),
          bens_id = paste(X_id,index,sep = ""),
          HoH_fullname = ifelse(is.na(hoh_name) , name ,hoh_name ),
          hoh_sex= ifelse(is.na(hoh_sex) ,ind_sex,hoh_sex),
          hoh_age= ind_age,
          tel_number = ifelse(is.na(tel_number_) ,tel_number,tel_number_),
          selection_criteria = ifelse(is.na(selection_criteria) ,selection_criteria_,selection_criteria),
          start_time = format(parse_date_time(start, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"),
          end_time = format(parse_date_time(end, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S")
          )%>%
        select(
          index,tab_date,bens_id,crc_name,enum_name,enum_phone_number, crc_role, Member_id, Region_id, District_id, Community_id, consent, verification_category, 
          HoH_fullname, hoh_sex,hoh_age, tel_number, selection_criteria, groups, group_size,ind_position, start_time,end_time
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
    ns1 <- NS("brcis3selection")
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
        drop_na(groups)
      
      groups_chart$text_position <- ifelse(groups_chart$count > 5, 'inside', 'outside')
      
      
      if(nrow(groups_chart) <= 0){
        maxs <- 0
      }else{
        maxs <- max(groups_chart$count)
      }

      fig <- plot_ly(groups_chart,
                     y = ~count, 
                     x = ~descriptions, type = 'bar', orientation = 'v',
                     text = paste("group size",groups_chart$count),textposition = "auto",
                     marker = list(color = "#008BA8"))%>%
        layout(
          title = list(text = 'Groups', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top',pad =2),
          xaxis = list(title = "Number of Beneficiaries"),
          yaxis = list(
          tickmode = "array",
          tickvals = seq(0, maxs, by = 1),  # Assuming your maximum value is reasonable for integer ticks
          ticktext = seq(0, maxs, by = 1)
        ))
      
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
        drop_na(selection_criteria)
      
      if(nrow(creteria_chart) <= 0){
        maxs <- 0
      }else{
        maxs <- max(creteria_chart$count)
      }
      
      fig <- plot_ly(creteria_chart,
                     x = ~count, 
                     y = ~description, type = 'bar', orientation = 'h',
                     text = ~count,textposition = "auto")%>%
        layout(title = list(text = 'Selection Creteria', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top',pad =2),
          yaxis = list(title=""),
          xaxis = list(title = "Number of Beneficiaries"),
          xaxis = list(
            tickmode = "array",
            tickvals = seq(0, maxs, by = 1),  # Assuming your maximum value is reasonable for integer ticks
            ticktext = seq(0, maxs, by = 1)
          ))
      
      fig
      
    })
    
    output$hh_summary_chart <- renderDT({
    
   
     
      hh_summary_chart <- main_survey %>%
        filter(
          consent == "yes"
        )%>%
        separate_rows(selection_criteria, sep = " ")%>%
        left_join(df_creteria , by = c("selection_criteria"="id"))%>%
        left_join(df_groups , by = c("groups"="variable_names"))%>%
        group_by(bens_id)%>%
        reframe(
          HoH_fullname = first(HoH_fullname),
          hoh_sex = first(hoh_sex),
          hoh_age = first(hoh_age),
          tel_number = first(tel_number),
          Member_id = first(Member_id),
          Region_id = first(Region_id),
          District_id = first(District_id),
          Community_id = first(Community_id),
          selection_criteria = paste(description , collapse = "<br/> <br/>"),
          group = first(descriptions),
          verification_category = first(verification_category),
        )%>%
        mutate(
          verification_category = case_when(
            verification_category == 'groups_ver' ~ 'Group Selection',
            verification_category == 'hh_level_or_individual_ver' ~ 'Individual Selection',
            .default = verification_category
          )
        )%>%
        replace_na(list(group = ''))
        
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
                ),escape = FALSE)%>%
        formatStyle(
          'selection_criteria',
          `overflow-x` = 'hidden',
          `white-space` = 'pre-line'
        )
    })
     
      
    output$community_chart <- renderPlotly({
      
      main_survey1 <- main_survey%>%filter(
        consent == "yes"
      )%>%separate_rows(selection_criteria, sep = " ")%>%
        group_by(Community_id,selection_criteria)%>%
        reframe(
          count= n()
        )
      
      communities <- 
        baseline_targets%>%select(Community_id)%>%
        left_join(main_survey1,by = c("Community_id")) %>%
        left_join(df_creteria , by = c("selection_criteria"="id"))%>%
        mutate(selection_criteria=as.numeric(selection_criteria))%>%
        replace_na(list(selection_criteria = 0,count =0 , description = ''))
    
      
      p <- plot_ly(communities, y = ~Community_id, x = ~count, color = ~description, 
                   type = 'bar', orientation = 'h', text = ~paste(count), # Add count as text on each bar
                   textposition = 'auto',
                   textfont = list(color = 'black'),
                   colors =  c("#00B050","#008BA8","#40419A","#ED7667","#F99D1E","#82BA96"),
                   visible = ifelse(communities$description == "trace 0", "legendonly", TRUE) ) %>%
        layout(
          barmode = 'stack', # Set barmode to stack for a stacked bar chart
          title = "Community Level selection creteria",
          xaxis = list(title = "Selection Creteria  frequency"),
          yaxis = list(title = "", autorange = "reversed"), # Optionally reverse the category axis
          legend = list(title = list(text = 'Status')),
          autosize = TRUE# Optionally format the legend title
        )
      
      # Display the plot
      p
      
    
    
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
    
    
   
  })
  
}

