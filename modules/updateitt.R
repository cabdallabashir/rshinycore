





updateittUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/updateitt.html",
                 loadIttData = actionButton(ns("loadIttData"),"Update ITT" , class="btn btn-primary"),
                 warningToLoadDATA = uiOutput(ns("warningToLoadDATA")),
    )
  )
}



updateitt <- function(input ,output , session,sharedValues){
  
  
  observeEvent(input$loadIttData , {
    
    tryCatch({
      
   
   response_data <- GET(url = "https://api.ona.io/api/v1/data/826896",
                         authenticate("abdullahiaweis","tutka_sulsulaaye@123"))
    
    
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
    
    
    
    if(nrow(formData) <= 0){
      column_names <- c(
                        "_id","project_Details/project_id",
                        "reporting_Location_Details/pos", "reporting_Location_Details/sel_district",
                        "reporting_Location_Details/achievment_information/indicator_result",
                        "reporting_Location_Details/achievment_information/total_male_beneficiaries", "reporting_Location_Details/achievment_information/total_female_beneficiaries",
                        "reporting_Location_Details/achievment_information/total_disabled_male_beneficiaries", "reporting_Location_Details/achievment_information/total_disabled_female_beneficiaries",
                        "reporting_Location_Details/achievment_information/total_minority_male_beneficiaries", "reporting_Location_Details/achievment_information/total_disabled_female_beneficiaries",
                        "project_Details/district_id", "reporting_Details/Member_id", "project_Details/indicator_code",
                        "project_Details/indicator_desc", "reporting_Details/reporting_month",
                        "index")
      
      formData <- data.frame(matrix(ncol = length(column_names), nrow = 0))
      names(formData) <- column_names
    }else{
      
      if("reporting_Location_Details" %in% names(formData)){
        
        for (i in seq_along(formData$`reporting_Location_Details`)) {
          # Access each member in the list
          Details <- formData$reporting_Location_Details[[i]]
          
          # Check if member is a list and has the required fields
          if (is.list(Details)) {
            
            Details$`reporting_Location_Details/index` <- seq.int(nrow(Details))
            # Standardize to one age column, converting all to character for consistency
            # if ("reporting_Location_Details/total_male_beneficiaries" %in% names(Details)) {
            #   Details$`reporting_Location_Details/total_male_beneficiaries` <- as.character(Details$`reporting_Location_Details/total_male_beneficiaries`)
            # } else{
            #   Details$`reporting_Location_Details/total_male_beneficiaries` <- as.character("0")
            # }
            # 
            # if ("reporting_Location_Details/total_disabled_male_beneficiaries" %in% names(Details)) {
            #   Details$`reporting_Location_Details/total_disabled_male_beneficiaries` <- as.character(Details$`reporting_Location_Details/total_disabled_male_beneficiaries`)
            # } else{
            #   Details$`reporting_Location_Details/total_disabled_male_beneficiaries` <- as.character("0")
            # }
            # 
            # if ("reporting_Location_Details/total_female_beneficiaries" %in% names(Details)) {
            #   Details$`reporting_Location_Details/total_female_beneficiaries` <- as.character(Details$`reporting_Location_Details/total_female_beneficiaries`)
            # } else{
            #   Details$`reporting_Location_Details/total_female_beneficiaries` <- as.character("0")
            # }
            # 
            # if ("reporting_Location_Details/total_disabled_female_beneficiaries" %in% names(Details)) {
            #   Details$`reporting_Location_Details/total_disabled_female_beneficiaries` <- as.character(Details$`reporting_Location_Details/total_disabled_female_beneficiaries`)
            # } else{
            #   Details$`reporting_Location_Details/total_disabled_female_beneficiaries` <- as.character("0")
            # }
            
            # Put the modified member back into the dataframe
            formData$reporting_Location_Details[[i]] <- Details
          }
        }
      }
      formData %<>%unnest("reporting_Location_Details",keep_empty = TRUE)%>%
        purrr::discard(is.list) %>%
        as.data.frame()
    }
    
    
    
    
    
    
    required_columns <- c(
      "_id","project_Details/project_id",
      "reporting_Location_Details/pos", "reporting_Location_Details/sel_district",
      "reporting_Location_Details/achievment_information/indicator_result",
      "reporting_Location_Details/achievment_information/total_male_beneficiaries", "reporting_Location_Details/achievment_information/total_female_beneficiaries",
      "reporting_Location_Details/achievment_information/total_disabled_male_beneficiaries", "reporting_Location_Details/achievment_information/total_disabled_female_beneficiaries",
      "reporting_Location_Details/achievment_information/total_minority_male_beneficiaries", "reporting_Location_Details/achievment_information/total_minority_female_beneficiaries",
      "project_Details/district_id", "reporting_Details/Member_id", "project_Details/indicator_code",
      "project_Details/indicator_desc", "reporting_Details/reporting_month")
    
    for (col in required_columns) {
      # Check if the column exists
      if (!col %in% names(formData)) {
        # If it does not exist, add it with NA values
        formData[[col]] <- NA
      }
    }
    
    names(formData) <- gsub(".*/", "", names(formData))
    
    formData %<>% mutate(
      dataID = paste(`_id`,pos,sep="")
    )%>% select(
      "dataID","_id","project_id","pos","sel_district","indicator_result","total_male_beneficiaries",
      "total_female_beneficiaries","total_disabled_male_beneficiaries","total_disabled_female_beneficiaries",
      "total_minority_male_beneficiaries" , "total_minority_female_beneficiaries",
      "district_id",
      "Member_id","indicator_code","indicator_desc",
      "reporting_month" 
    )%>%
      dplyr::rename(
        id = `_id`,
        `indicator_id`=indicator_code,
        `indicator_name`=indicator_desc
      )
    
    # indicator_mapping_brics3 = read_excel("data/indicator_mapping_brics3.xlsx")
    # 
    # 
    # get_new_indicator <- function(indicator){
    #  indc <- indicator_mapping_brics3 %>%
    #    filter(old_indicator == indicator & project == 'BRCiS III')%>%
    #    pull(indicator_code)
    #  
    #  if(length(indc) > 0){
    #    return(indc)
    #  }else{
    #    return(indicator)
    #  }
    # }
    # 
    # formData %<>%
    #   mutate(
    #     project_id = case_when(
    #       project_id == 'CRISIS_MODIFIER' ~ 'BRCIS III',
    #       .default = project_id
    #     )
    #   )%>%
    #   rowwise()%>%
    #   mutate(
    #     indicator_id = if_else(project_id == 'BRCIS III',
    #                            get_new_indicator(indicator_id),
    #                            indicator_id
    #                            )
    #   )
    
    
    
    postgresConnection <- dbConnect(RPostgres::Postgres(),
                                    dbname = "brcisShiny",
                                    host = "brcisproddb.postgres.database.azure.com", port = 5432,
                                    user = "brcisShiny", password = "brcisShiny@112233@",
                                    sslmode = 'require')
    
    
    
    tables <- dbListTables(postgresConnection)
    if("itt_results" %in% tables){
      existing_ids <- dbGetQuery(postgresConnection, 'SELECT itt_results."dataID" FROM "itt_results"')
      df_to_insert <- formData[!formData$dataID %in% existing_ids$dataID,]
      
      if (nrow(df_to_insert) > 0) {
        print(paste("Rows Writing To Database :",nrow(df_to_insert)))
        dbWriteTable(postgresConnection, name = "itt_results", value = df_to_insert, append = TRUE, row.names = FALSE)
      } else {
        print("No new records to insert.")
      }
    }else{
      dbWriteTable(postgresConnection , "itt_results" , formData)
    }
    
    
    output$warningToLoadDATA <- renderUI({
      HTML('<div class="row">
              <div class="col-12">
                <div class="alert alert-success border-0 bg-success alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Instruction</h6>
											<div class="text-dark">Data Updated Successfull</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>
              </div>
            </div>')
      
    })
    }, error = function(e) {
      output$warningToLoadDATA <- renderUI({
        HTML(paste(sep = "",
                   '<div class="row">
              <div class="col-12">
                <div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Error</h6>
											<div class="text-dark">',e$message,'</div>
										</div>
									</div>
									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
								</div>
              </div>
            </div>' 
        ))
      })
      print(e$message)
    })
  
  })
  
  
  

  
  
}

