

get_data <- function(username, password ,formID){
  
  
  # Make the API request
  response_data <- GET(url = paste("https://api.ona.io/api/v1/data/",formID,sep = ""), authenticate(username, password))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
    
    formData %<>%
      unnest(reporting_Location_Details)%>%purrr::discard(is.list) %>%
      as.data.frame()
    
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







ittJsonUI <- function(id){
  ns <- NS(id)
  NULL
 
}

ittJson <- function(input ,output , session,sharedValues){
  
  
  baseline_survey  <- get_data("abdullahiaweis","tutka_sulsulaaye@123","783054")
  
  # Create a custom endpoint
  shinyServer(function(input, output, session) {
    # Serve the JSON at the "/data" endpoint
    shiny::httpResponse(
      req = session$request,
      status = 200,
      content_type = "application/json",
      content = output$myJson()
    )
  })
  
  shiny::httpResponse(
    status = 200,
    content_type = "application/json",
    content = toJSON(baseline_survey)
  )
  
}

