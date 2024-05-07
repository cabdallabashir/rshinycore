# library(shiny)
# library(DT)
# library(shinyjs)
# 
# ui <- fluidPage(
#   useShinyjs(),
#   tags$head(
#     tags$style(HTML("
#       table.dataTable tbody tr.selected {
#         background-color: transparent !important;
#       }
#     "))
#   ),
#   DTOutput("myTable")
# )
# 
# server <- function(input, output, session) {
#   data <- reactive({
#     # Assuming you have a dataframe `df` with your data
#     df <- data.frame(ID = 1:5, Value = letters[1:5])
#     
#     # Add a column with action buttons, giving each button a unique ID
#     df$actions <- sprintf('<button id="row-%s" class="action-button">Action</button>', df$ID)
#     
#     df
#   })
#   
#   output$myTable <- renderDT({
#     data() %>%
#       datatable(escape = FALSE, options = list(dom = 't'),selection = "none") # `escape = FALSE` allows HTML content
#   })
#   
#   runjs('
#     $(document).on("click", ".action-button", function() {
#       var id = $(this).attr("id");
#       Shiny.setInputValue("button_click", id);
#     });
#   ')
#   
#   # Server-side action based on button click
#   observeEvent(input$button_click, {
#     btnId <- input$button_click
#     # Extract the row number or perform actions based on btnId
#     # For example, just printing the ID of the clicked button
#     print(btnId)
#   })
# }
# 
# shinyApp(ui,server)
# 
# library(shiny)
# library(DT)
# library(shinyjs)
# 
# ui <- fluidPage(
#   useShinyjs(),
#   DTOutput("myTable"),
#   actionButton("getChecks", "Get Checked Rows"),
#   verbatimTextOutput("checkedRows") # To display checked rows
# )
# 
# 
# server <- function(input, output) {
#   # Sample data
#   df <- data.frame(
#     ID = 21:25,
#     Value = LETTERS[1:5]
#   )
#   
#   output$myTable <- renderDT({
#     datatable(selection = "none",df, options = list(
#       columnDefs = list(list(
#         title = 'Select',
#         targets = 0, render = JS(
#           "function(data, type, full, meta) {",
#           "return '<input type=\"checkbox\" class=\"row-checkbox\" value=\"' + full[1] + full[2] + '\">';",
#           "}")
#       )),
#       preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
#       drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }')
#     ), escape = FALSE)
#   }, server = FALSE)
#   
#   observeEvent(input$getChecks, {
#    
#     runjs('
#       var checked = [];
#       $(".row-checkbox:checked").each(function() {
#         checked.push(this.value);
#       });
#       Shiny.setInputValue("checked_rows", checked);
#     ')
# 
#     showModal(modalDialog(
#       title = "Button Clicked",
#       tags$div(
#         tagList(
#           uiOutput("ms"),
#           textInput(
#             "us",
#             "enter User name"
#           ),
#           passwordInput(
#             "ps",
#             "enter password"
#           ),
#           actionButton("sb","submit")
#         )
#       )
#     ))
#     
#     
#   })
#   
#   observeEvent(input$sb ,{
#     us <- input$us
#     ps <- input$ps
#     
#     if(us == "abdalla" & ps =="888"){
#       output$ms <- renderUI({
#         HTML(
#           '
#           <span style="color:green">Data removed</span>
#           '
#         )
#       })
#        print(input$checked_rows)
#     }else{
#       output$ms <- renderUI({
#         
#         HTML(
#           '
#           <span style="color:red">incorrect password</span>
#           '
#         )
#       })
#      
#     }
#   })
#   
#   
# 
# }
# 
# # Run the Shiny app
# shinyApp(ui, server)
# 
# 
# 
# library(shiny)
# library(DT)
# library(shinyjs)
# 
# # Example DataFrame
# df <- data.frame(
#   region = c("East", "West", "East", "West"),
#   value = c(1,2,3,4),
#   stringsAsFactors = FALSE  # Ensure strings are not converted to factors
# )%>%
#   arrange(region)
# 
# 
# ui <- fluidPage(
#   useShinyjs(),
#   DTOutput("datatable")
# )
# 
# server <- function(input, output) {
#   output$datatable <- renderDT({
#     datatable(df,
#               rownames = FALSE, 
#               extensions = 'RowGroup', 
#               options = list(rowGroup = list(dataSrc = 0),  # Group by the first column (region)
#                              columnDefs = list(list(visible=TRUE, targets=c(0)))  # Ensure the region column is visible
#               )
#     )
#   })
# }
# 
# shinyApp(ui = ui, server = server)
# 
# library(shiny)
# runApp(list(
#   ui = fluidPage(
#     tableOutput('foo')
#   ),
#   server = function(input, output) {
#     x1 <- 1:100
#     x2 <- rbind(mtcars, mtcars)
#     env <-globalenv() # environment()  # can use globalenv(), parent.frame(), etc
#     output$foo <- renderTable({
#       data.frame(
#         object = ls(env),
#         size = unlist(lapply(ls(env), function(x) {
#           object.size(get(x, envir = env, inherits = FALSE)) / (1024 * 1024)
#         }))
#       )
#     })
#   }
# ))
# library(shiny)
# library(DT)
# 
# # Sample data
# data <- data.frame(
#   Category = c("A", "B", "C", "D"),
#   Amount1 = c(100, 150, 200, 250),
#   Amount2 = c(200, 250, 300, 350)
# )
# 
# ui <- fluidPage(
#   DTOutput("my_table")
# )
# 
# server <- function(input, output) {
#   output$my_table <- renderDT({
#     # Calculate sums for the footer
#     sums <- colSums(data[, c("Amount1", "Amount2")], na.rm = TRUE)
#     footer <- paste0('<tfoot><tr><th colspan="1">Total</th><th class="dt-right">', sums[1], '</th><th class="dt-right">', sums[2], '</th></tr>
#     <tr><th colspan="1">Total</th><th class="dt-right">', sums[1], '</th><th class="dt-right">', sums[2], '</th></tr>
#                      </tfoot>')
#     
#     datatable(data,rownames = FALSE, options = list(dom = 't', initComplete = JS(
#       "function(settings, json) {",
#       "$(this.api().table().footer()).remove();",
#       "$(this.api().table().header()).after(`", footer, "`);",
#       "}"
#     )), escape = FALSE)
#   })
# }
# 
# shinyApp(ui, server)
# library(shiny)
# library(DT)
# 
# ui <- fluidPage(
#   DTOutput("my_custom_dt_id") # Set custom ID here
# )
# 
# server <- function(input, output) {
#   output$my_custom_dt_id <- renderDT({
#     datatable(iris,class = "display dataTable no-footer test")
#   })
# }
# 
# shinyApp(ui = ui, server = server)

library(shiny)
library(jsonlite)

# Define the server logic
server <- function(input, output, session) {
  # Define a reactive endpoint for returning JSON
  output$myJson <- reactive({
    # Example data frame
    data <- data.frame(
      name = c("Alice", "Bob", "Charlie"),
      age = c(25, 30, 35)
    )
    
    # Convert the data frame to JSON
    toJSON(data)
  })
  
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
}

# Define UI as null since this app will not have a user interface
ui <- NULL

# Run the application
shinyApp(ui = ui, server = server)
