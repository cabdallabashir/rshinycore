# Create the dataframe with credentials

# memberOnaCredentials <- data.frame(
#   users = c("abdullahiaweis",'gredo_username'),
#   password = c("tutka_sulsulaaye@123",'gredo_username'),
#   member = c("CMU",'GREDO')
# )

# Function to validate user input
validate_member_ona_credentials <- function(username_input,pass_input, form_id) {
  # Check if the input username and password match any row in the dataframe
  postgresConnection <- dbConnect(RPostgres::Postgres(),
                                  dbname = "brcisShiny",
                                  host = "brcisproddb.postgres.database.azure.com", port = 5432,
                                  user = "brcisShiny", password = "brcisShiny@112233@",
                                  sslmode = 'require')
  
  memberOnaCredentials <- dbGetQuery(postgresConnection, sprintf('SELECT "MemberCode" as "member" ,"Username" as "users" FROM public."onaUsers" where "Username" = \'%s\' and "STATUS" = \'A\'', username_input))
  dbDisconnect(postgresConnection)
  
  match_row <- memberOnaCredentials$users == username_input #& memberCredentials$password == password_input
  
  # If there's a match, match_row will contain TRUE for the matching row(s)
  if (any(match_row)) {
    member_type <- memberOnaCredentials$member[match_row]
    
    ona_permission <- check_user_ona_permission(username_input,pass_input,form_id)
    
    if(ona_permission$status ==200){
      return(
        data.frame(
          status = TRUE,
          message = member_type
        )
      )
    }else{
      return(
        data.frame(
          status = FALSE,
          message = ona_permission$message
        )
      )
    }
    
    
  } else {
    return(
      data.frame(
        status = FALSE,
        message = "User Not Found in the system"
      )
    )
  }
}

check_user_ona_permission <- function(username,password,form_id) {
  # Make the API request

  response_data <- GET(url = paste("https://api.ona.io/api/v1/forms/",form_id,sep = ""), authenticate(username,password))
  status <- status_code(response_data)
  
  if (status == 200) {
    formData <- prettify(rawToChar(response_data$content))%>% fromJSON()
    
    users <- as.data.frame(formData$users) %>% select(first_name, last_name , user , role)
    
    logged_in_user <- users%>%filter(user == username)
    
   
    
    
    if(nrow(logged_in_user) == 0){
      return(
        data.frame(
          status= 404,
          message = "user not found",
          data = ""
        )
      )
    }else{
      return(
        data.frame(
          status= 200,
          message = "success",
          data = ""
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
  else if (status >= 404) {
    # Server error
    return(
      data.frame(
        status= 403,
        message = "Invalid form / You dont have access to this data",
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
