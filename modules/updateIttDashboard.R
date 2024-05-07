



get_data <- function(username, password ,formID){
  

  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/","783054",sep = ""), authenticate("abdullahiaweis","tutka_sulsulaaye@123"))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData$reporting_Location_Details <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
    
    names(formData )
    
    if(nrow(formData) <= 0){
      column_names <- c("_id", "end", "_uuid", "start", "_edited", "_status", "_version", "_duration",
                        "_xform_id", "count_sel", "Device_Tag", "System_Date", "_media_count", "_total_media",
                        "formhub/uuid", "_submitted_by", "_date_modified", "meta/instanceID", "_submission_time",
                        "_xform_id_string", "meta/instanceName", "_bamboo_dataset_id", "_media_all_received",
                        "project_Details/unit", "project_Details/Region", "project_Details/project_id",
                        "reporting_Location_Details/pos", "reporting_Location_Details/sel_district",
                        "reporting_Location_Details/indicator_result", "reporting_Location_Details/gender_classification_applicable",
                        "reporting_Location_Details/total_male_beneficiaries", "reporting_Location_Details/total_female_beneficiaries",
                        "reporting_Location_Details/total_disabled_male_beneficiaries", "reporting_Location_Details/total_disabled_female_beneficiaries",
                        "project_Details/district_id", "reporting_Details/Member_id", "project_Details/indicator_id",
                        "project_Details/project_name", "project_Details/district_name", "project_Details/activity_group",
                        "project_Details/indicator_name", "reporting_Location_Details_count", "reporting_Details/reporting_month",
                        "interview_status1/interview_status", "project_Details/activity_group_number", "index")
      
      formData <- data.frame(matrix(ncol = length(column_names), nrow = 0))
      names(formData) <- column_names
    }else{
       if(formID !='798667'){
          formData %<>%
          unnest("reporting_Location_Details",keep_empty = TRUE)%>%purrr::discard(is.list) %>%
          as.data.frame()%>%mutate(index = row_number())
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


post_data <- function(username, password ,json_data){
  
  credentials <- paste(username, password, sep = ":")
  encoded_credentials <- base64_enc(credentials)
  # Make the API request
  response_data <- POST(url = "http://api.ona.io/api/v1/submissions",
                        authenticate = authenticate(username,password),
                        body = json_data,
                        encode = "json",
                        config = add_headers(`Content-Type` = "application/json"),
                        authenticate(username,password))
  
  status <- status_code(response_data)
  
  res <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
  
  
  return(
    data.frame(
      status= status,
      message = res[1,1]
    )
  )
  
   
  
}


# Create the dataframe with credentials
Credentials <- data.frame(
  users = c("abdullahiaweis"),
  password = c(""),
  member = c("CMU")
)

# Function to validate user input
validate_credentials <- function(username_input, password_input) {
  # Check if the input username and password match any row in the dataframe
  match_row <- Credentials$users == username_input #& Credentials$password == password_input
  
  # If there's a match, match_row will contain TRUE for the matching row(s)
  if (any(match_row)) {
   
    return(
      data.frame(
        status = TRUE
      )
    )
  } else {
    return(
      data.frame(
        status = FALSE
      )
    )
  }
}




updateIttDashboardUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .shiny-input-container {
        width: 100% !important;
      }
    ")),
    htmlTemplate("views/updateIttDashboard.html",
                 
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
                 uploadResult = DTOutput(ns("uploadResult"))
               
    )
  )
}

updateIttDashboard <- function(input ,output , session,sharedValues){
  
 
  
  
  
  
   
  
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
    
        result <- validate_credentials(onauser, onapass)
        
        if(!result$status){
          output$messageBox <- renderUI({

            HTML(
              '<div class="alert alert-danger border-0 bg-danger alert-dismissible fade show py-2">
    									<div class="d-flex align-items-center">
    										<div class="font-35 text-dark"><i class="bx bx-info-circle"></i>
    										</div>
    										<div class="ms-3">
    											<h6 class="mb-0 text-dark">Warning</h6>
    											<div class="text-dark">User Not Found in the system</div>
    										</div>
    									</div>
    									<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
    								</div>'
            )
          })
          return()
        }

    
    repeated_itt  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","783054")
    newform <- get_data("abdalla_ona","Ona@123456","798667")
    
    
    
    
    
    
    
  
    if(first(repeated_itt$status) == "200"   & first(repeated_itt$message) != "empty" & first(newform$status) == "200"){
     
     
      data_to_be_uploaded <- repeated_itt %>% filter(!X_uuid %in% newform$custom_UUID & !X_id %in% newform$customer_ID)
      
      if(nrow(data_to_be_uploaded) > 0){
        result_data <- apply(data_to_be_uploaded, 1, function(row) {
          # Construct the data list for the submission
          data <- list(
            id = "BRCiS3_OASIS_Monthly_Indicator_Tracking_Tool_dashboard",  # Assuming a static ID; change if dynamic
            submission = list(
              custom_UUID = row['X_uuid'],
              customer_ID = row['X_id'],
              start = row['start'],
              end = row['end'],
              Member_id = row["Member_id"],
              reporting_month = row["reporting_month"],
              project_id = row["project_id"],
              activity_group = row["activity_group"],
              indicator_id = row["indicator_id"],
              district_id = row["district_id"],
              project_name = row["project_name"],
              activity_group_number = row["activity_group_number"],
              indicator_name = row["indicator_name"],
              unit = row["unit"],
              district_name = row["district_name"],
              Region = row["Region"],
              indicator_result = row["indicator_result"],
              gender_classification_applicable = row["gender_classification_applicable"],
              total_male_beneficiaries = row["total_male_beneficiaries"],
              total_female_beneficiaries = row["total_female_beneficiaries"],
              total_disabled_male_beneficiaries = row["total_disabled_male_beneficiaries"],
              total_disabled_female_beneficiaries = row["total_disabled_female_beneficiaries"],
              interview_status = row["interview_status"],
              int_comment = row["int_comment"]
            )
          )
          
          result <- post_data("abdalla_ona","Ona@123456",data)
          
          # if(result$status != 201){
          #   
          #   paste(row['X_id'],paste(result$status,result$message,sep = "=>"),sep = ":")
          #   
          # }
          
         return(
           data.frame(
             row_id = row['X_id'],
             member = row["Member_id"],
             month = row["reporting_month"],
             project = row["project_id"],
             distrct_id = row["district_id"],
             indicator_id = row["indicator_id"],
             indicator_name = row["indicator_name"],
             status = result$status,
             message = result$message
           )
         )
          
         
          
        })
        result_data <- bind_rows(result_data)  # Transpose to correct the orientation
        result_data <- as.data.frame(result_data)
        row.names(result_data) <- NULL
        
        output$uploadResult <- renderDT({
          datatable(result_data)
        })
        
       
    
        if(all(result_data$status == 201)){
          output$messageBox <- renderUI({
            HTML('<div class="alert alert-success border-0 bg-success alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bxs-check-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-white">Success</h6>
											<div class="text-white">Data Updated Successfully</div>
										</div>
									</div>

								</div>')
          })
        }else{
          output$messageBox <- renderUI({
            HTML(paste(
              sep = "",
              '<div class="alert alert-warning border-0 bg-warning alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-dark">Error Info</h6>
											<div class="text-dark">check the table below to see upload results</div>
										</div>
									</div>

								</div>'
            ))
          })
          
          runjs(
            " $('#exampleVerticallycenteredModal').modal('hide'); "
          )
        }
      }else{
        output$messageBox <- renderUI({
          HTML(paste(
            sep = "",
            '<div class="alert alert-warning border-0 bg-warning alert-dismissible fade show py-2">
									<div class="d-flex align-items-center">
										<div class="font-35 text-white"><i class="bx bx-info-circle"></i>
										</div>
										<div class="ms-3">
											<h6 class="mb-0 text-white">Empty</h6>
											<div class="text-white">No To Upload , Dashboard is already Updated</div>
										</div>
									</div>
									
								</div>'
          ))
        })
      }

      
      
    }else{
      output$messageBox <- renderUI({
       
        resp <- if_else(first(repeated_itt$status) != "200" ,first(repeated_itt$message) , paste("Data Cannot be loaded please contact CMU MEAL"))
        
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
  
  
  

  
}

