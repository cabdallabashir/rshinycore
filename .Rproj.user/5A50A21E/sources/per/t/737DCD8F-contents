library(dplyr)
library(shiny)
# library(shiny.router)
library(lubridate)
library(RSQLite)
library(readxl)
library(openxlsx)
library(leaflet)
# library(palmerpenguins)
# library(leaflet)
library(sp)
library(sf)
library(ggplot2)
library(httr)
# library(arrow)
library(jsonlite)
library(splitstackshape)
library(DBI)
# library(zoo)
library(data.table)
# library(sodium)
# library(naniar)
library(tidyverse)
library(tidyjson)
library(magrittr)
library(jsonlite)
# library(rdrop2)
library(plotly)
library(shinycssloaders)
library(bslib)
library(DT)
library(shinyjs)
library(assertthat)
library(purrr)

# install.packages("RPostgreSQL")

# install.packages("renv")
# renv::update()
# update.packages()


source("db/dbCreation.R")
source("functions/getEWEA.R")


# update_ewea("abdullahiaweis", "tutka_sulsulaaye@123","0990")
# 
# print(update_ewea("abdullahiaweis","tutka_sulsulaaye@123"))
# 
# report <- query("select * from red_flagging")
# 
# report %<>% mutate(
#   reporting_month = as.yearmon(reporting_month)
# )
# 
# status_per_fms_per_status <- report%>%
#   drop_na(status)%>%
#   dplyr::summarise(count = n(),
#                    .by = c("FMS","status"))
# 
# status_per_district_per_status <- report%>%   

somalia <- read_sf("docs/shp/Somalia.shp")## read Somalia country boundary

somalia_districts <- 

somalia_districts <-read_sf("docs/shp/Somaia districts.shp") %>% 
  mutate(
    District = ifelse(District == "Rab Dhuure",
                      "rabdhure" , 
                      ifelse(
                        District == "Diinsoor",
                        "Dinsoor",
                        District
                      )
    )
  ) %>%sf::st_as_sf()

allowedURL <- c(
  "home",
  "summary",
  "variable",
  "district",
  "eweaHome",
  "itt",
  "brcisBaseline",
  "oasisBaseline",
  "baseline_home",
  "arcd","ittJson",
  "oasisBaselineRegistrationScreening",
  "brcis3selection",
  "updateIttDashboard",
  "brcisRegistration"
)



market_thresholds <- read.csv("docs/EW thresholds _USD.csv")%>%
  rename(`Up to` = Up.to,
         status = Level)


# 
# #
# red_flagged <- readTable("red_flagging") %>% mutate(reporting_month = as.yearmon(reporting_month))



# indicator_total = red_flagged %>%
#   filter(red_flagged == "Yes") %>%
#   summarise(Alarm = sum(Alert),
#             Alert = sum(Alert),
#             Normal = sum(Normal))
# #
# #
# #
# summary <- red_flagged %>%
#   filter(red_flagged == "Yes") %>%
#   group_by(reporting_month) %>%
#   summarise(
#     Alarm = sum(Alarm),
#     Alert = sum(Alert),
#     Normal = sum(Normal),
#     count = n()
#   ) %>%
#   arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(reporting_month))
# 
# fig <- plot_ly(summary, x = ~reporting_month, y = ~count, type = 'bar', name = 'count')
# fig <- fig %>% add_trace(y = ~Alarm, name = 'Alarm')
# fig <- fig %>% add_trace(y = ~Alert, name = 'Alert')
# fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group' , 
#                       xaxis = list(categoryorder = "array",
#                                    categoryarray = ~reporting_month))
# 
# fig
# 
# ewea <- readTable("ewea") %>% mutate(reporting_month = as.yearmon(reporting_month))
# dd <- ewea %>%
#   filter(variable == "Rainfall")%>%
#   group_by(district_name ,value ,status ) %>%
#   reframe(
#     count = n(),
#   ) %>% drop_na(value)
# 
# dd$hover_text <- with(dd, paste('District Name:', district_name, '<br>',
#                                 'Count:', count, '<br>',
#                                 'Value:', value, '<br>',
#                                 'Status:', status))
# 
# fig <- plot_ly(dd, x = ~district_name, y = ~count, type = 'bar', 
#                name = ~value, color = ~value, 
#                text = ~hover_text , hoverinfo = 'text' ,textposition = "none") %>%
#   layout(yaxis = list(title = 'Count'), barmode = 'stack')
# 
# fig
# 
# 
# 
# ee <- ewea %>%
#   filter(variable == "Sorghum Prices")%>%
#   drop_na(value) %>% 
#   group_by(district_name ,status ) %>%
#   reframe(
#     value = mean(as.numeric(value)),
#     count = n()
#   )
# 
# ee$hover_text <- with(ee, paste('District Name:', district_name, '<br>',
#                                 'Count:', count, '<br>',
#                                 paste('Avarage ',status, ":"), round(value,3), '<br>',
#                                 'Status:', status))
# 
# fig <- plot_ly(ee, x = ~district_name, y = ~count, type = 'bar', 
#                name = ~status, color = ~status, 
#                text = ~hover_text , hoverinfo = 'text' ,textposition = "none") %>%
#   layout(yaxis = list(title = 'Count'), barmode = 'stack')
# 
# fig
#   

# 
# summary <- ewea %>%
#   filter(variable == "Sorghum Prices") %>%
#   filter(district_name == "Adaado") %>%
#   drop_na(value) %>%
#   group_by(reporting_month ,status ) %>%
#   summarise(
#     value = as.numeric(value),
#     .groups = "rowwise"
#   )%>%
#   arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(reporting_month)) %>%
#   as.data.frame()
# 
# 
# color_mapping <- setNames(c("#F99D1E", "#008BA8", "#ED7667"), c("Alert", "Normal", "Alarm"))
# get_color <- function(classify) {
#   return(color_mapping[classify])
# }
# 
# fig <- plot_ly(summary, x = ~reporting_month, y = ~value, type = 'scatter', mode = 'lines+markers+text',
#                line = list(shape = "linear"),
#                marker = list(size = 10,color = ~get_color(status)),
#                text = ~status, textposition = "top center", # Add labels from the 'status' column
#                hoverinfo = 'text+x+y') %>%
#   layout(
#     xaxis = list(categoryorder = "array",
#                  categoryarray = ~reporting_month)
#   )
# 
# fig
# 
# 
# 
# summary <- ewea %>%
#   filter(variable == "Rainfall" & district_name == "Adaado") %>%
#   drop_na(value) %>%
#   group_by(reporting_month ,value ) %>%
#   reframe(
#     status = first(status),
#     count = n()
#   )%>%
#   arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(reporting_month)) %>%
#   as.data.frame()
# 
# 
# color_mapping <- setNames(c("#F99D1E", "#008BA8", "#ED7667"), c("Alert", "Normal", "Alarm"))
# get_color <- function(classify) {
#   return(color_mapping[classify])
# }
# 
# fig <- plot_ly(summary, x = ~reporting_month, 
#                y = ~value, 
#                type = 'bar', 
#                name = ~status, 
#                text = ~status,
#                color = ~value,
#                colors =  c("#F99D1E", "#40419A", "#008BA8","#ED7667","#82BA96")) %>%
#   layout(yaxis = list(title = ''), barmode = 'stack',
#          xaxis = list(categoryorder = "array",
#                       categoryarray = ~reporting_month),
#          legend = list(orientation = "h", x = 0, y = 1.1, xanchor = 'left', yanchor = 'top'))
# 
# fig
# 
# 
# # Creating a plot with boxes colored by status
# p <- ggplot(summary, aes(x = reporting_month, y = value)) +
#   geom_tile(width = 0.8, height = 0.5 , aes(fill = status)) +
#   geom_line(aes(x= reporting_month , y=value ,group =reporting_month) ,color = "#40419A") +
#   labs(title = "Monthly Rainfall Categories and Status", x = "Month", y = "Rainfall Category") +
#   scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +  # Custom colors for each status
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust text angle for better readability
# 
# ggplotly(p)
# 

# ewea %>% group_by(variable) %>% summarise(n=)
# 
# summary <- ewea %>%
#   filter(variable == "Rainfall" & district_name == "Adaado") %>%
#   drop_na(value) %>%
#   group_by(reporting_month ,value ) %>%
#   reframe(
#     status = first(status),
#     other = other,
#     count = n()
#   )%>%
#   arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(reporting_month)) %>%
#   as.data.frame()
# 
# p <- ggplot(summary, aes(x = reporting_month, y = value)) +
#   geom_tile(width = 0.8, height = 0.5 , aes(fill = status)) +
#   geom_line(aes(x= reporting_month , y=value ,group =reporting_month) ,color = "#40419A") +
#   labs(title = paste("Monthly ",first(summary$other)," at "), x = "Month", y = "") +
#   scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +  # Custom colors for each status
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust text angle for better readability
# 
# ggplotly(p)
# 
# 

# Create a sample dataset
# 
# www <- function(s) {
#   
#   return(
#     substr(s, 1, 20)
#   )
# }
# 
# summary<-ewea %>%
#   filter(district_name == "Adaado" & variable == "coping_strategies" ) %>%
#   drop_na(value) %>%
#   group_by(reporting_month ,value ) %>%
#   reframe(
#     status = first(status),
#     other = other,
#     count = n()
#   )%>%
#   arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(as.yearmon(reporting_month))
#   )%>%
#   as.data.frame()
# 
# summary$truncated <- sapply(summary$value , www)
# print(summary)
# 
# title <- "hh"
# 
# p <- ggplot(summary, aes(x = reporting_month, y = truncated,label=value)) +
#   geom_tile(width = 0.8, height = 0.5 , aes(fill = status)) +
#   geom_line(aes(x= reporting_month , y=truncated ,group =reporting_month) ,color = "#40419A") +
#   labs(title = paste("Monthly ",title," at "), x = "Month", y = "") +
#   scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +  # Custom colors for each status
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust text angle for better readability
# 
# ggplotly(p)
# 
# 
# 



# eweadu <- readTable("ewea") %>% mutate(reporting_month = as.yearmon(reporting_month))
# 
# ada <- eweadu %>%
#   filter(district_name == "Adaado" & variable == "Rainfall")%>%
#   drop_na(value) %>%
#   group_by(reporting_month ,District ,value,status ) %>%
#   reframe(
#     count = n()
#   )%>%arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(as.yearmon(reporting_month)))
# 
# ada$reporting_month <- factor(ada$reporting_month, levels = unique(ada$reporting_month))
# 
# 
# 
# p <- ggplot(ada, aes(x = reporting_month, y = value,label =count)) +
#   geom_tile(width = 0.8, height = 0.5 , aes(fill = status,label = count)) +
#   geom_line(aes(x= reporting_month , y=value ,group =reporting_month) ,color = "#40419A") +
#   scale_fill_manual(values = c("Normal" = "#008BA8", "Alert" = "#F99D1E", "Alarm" = "#ED7667")) +  # Custom colors for each status
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# Adjust text angle for better readability
#   geom_text(aes(x = reporting_month, y = count, label = count),
#             vjust = -0.5, position = position_dodge(width = 0.9))
# 
# ggplotly(p)
# 
# 
# 
# ada1 <- eweadu %>%
#   filter( variable == "Rainfall")%>%
#   drop_na(value) %>%
#   group_by(reporting_month ,value,status ) %>%
#   reframe(
#     count = sum(n())
#   )%>%arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(as.yearmon(reporting_month)))
# 
# ada1$reporting_month <- factor(ada1$reporting_month, levels = unique(ada1$reporting_month))
# 
# 
# totals <- ada1 %>%
#   group_by(reporting_month, status) %>%
#   summarise(total = sum(count))
# 
# p <- ggplot(ada1) +
#   geom_bar( stat = "identity",aes(x = reporting_month,y = count, fill= value)) +
#   facet_grid(rows = vars(status)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(title = "")) +
#   geom_text(data = totals, aes(x = reporting_month, y = total, label = total),
#             vjust = -0.5, position = position_dodge(width = 0.9))+
#   geom_line(data = totals , aes(x = reporting_month, y = total, group = status), color = "blue")
# 
# ggplotly(p)
# 
# 
# 
# 
# 
# ada2 <- eweadu %>%
#   filter( variable == "Sorghum Prices")%>%
#   drop_na(value) %>%
#   group_by(district_name ,value,status ) %>%
#   reframe(
#     count = sum(n())
#   )
# 
# 
# totals <- ada2 %>%
#   group_by(district_name, status) %>%
#   summarise(total = sum(count))
# 
# p <- ggplot(totals) +
#   geom_line(aes(x = district_name, y = total, group = status), color = "blue") +
#   facet_grid(rows = vars(status)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(title = "")) +
#   geom_text(data = totals, aes(x = district_name, y = total, label = total),
#             vjust = -0.5, position = position_dodge(width = 0.9))
# 
# 
# ggplotly(p)
# 
# 
# 
# 
# 
# 
# 
# ada <- eweadu %>%
#   filter(district_name == "Adaado" & variable == "Rainfall")%>%
#   drop_na(value) %>%
#   group_by(reporting_month ,District ,value,status ) %>%
#   reframe(
#     count = n()
#   )%>%arrange(as.yearmon(reporting_month))%>%
#   mutate(reporting_month = as.character(as.yearmon(reporting_month)))
# 
# ada$reporting_month <- factor(ada$reporting_month, levels = unique(ada$reporting_month))
# 
# 
# 
# p <- ggplot(ada) +
#   geom_bar( stat = "identity",aes(x = reporting_month,y = count, fill= value)) +
#   facet_grid(rows = vars(status),scales = "free_y") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(title = "")) 
# 
# ggplotly(p)

# 
# rrr <- eweadu %>%
#   filter(variable == 'Sorghum Prices')%>%
#   drop_na(value) %>%
#   group_by(district_name ,value,status ) %>%
#   reframe(
#     value = mean(as.numeric(value)),
#     other = first(other),
#     district = first(District),
#     count = n()
#   )
# 
# tttt <- rrr %>%
#   group_by(district_name, status) %>%
#   summarise(total = round(mean(as.numeric(value)),2))
# 
# print(rrr, n=1000)
# 
# 
# p <- ggplot(tttt) +
#   geom_line(aes(x = district_name, y = total, group = status), color = "blue") +
#   geom_point(aes(x = district_name, y = total, group = status), color = "blue") +
#   facet_grid(rows = vars(status),scales = "free") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(title = "")) +
#   geom_text(aes(x = district_name, y = total, label = total), 
#             vjust = -0.5, position = position_dodge(width = 0.9)) 
# 
# 
# ggplotly(p)
# 


# d <-eweadu %>%
# filter(district_name == 'Adaado') %>%
#   group_by(variable_type , status) %>%
#   summarise(
#     count = n()
#   ) %>%
#   pivot_wider(names_from = status , values_from = count)
# 
# ggplotly(
#   ggplot(d, aes(x=variable_type, y=count)) +
#     geom_bar(aes(fill = status,), stat = "identity", position = "dodge") +
#     geom_text(aes(label = count, group = status),
#               position = position_dodge(width = 0.9),
#               vjust = -0.25)
# )





# 
# 
# 
# minutes_data <- data.frame(
#   Type = c("Average Minutes", "Minimum Minutes", "Maximum Minutes"),
#   Minutes = c(4, 2, 6) # replace with actual values
# )
# 
# var = "Type"
# 
# as.formula(paste0("~`", var, "`"))
# 
# # Create the bar chart
# fig <- plot_ly(data = minutes_data, x = ~Type, y = ~Minutes, type = 'bar',
#                marker = list(color = c('blue', 'red', 'green')))
# 
# # Customize the layout
# fig <- fig %>% layout(title = "Comparison of Minutes",
#                       xaxis = list(title = ""),
#                       yaxis = list(title = "Minutes"))
# 
# 
# fig
# 


# 
# 
# start <- "2024-02-13T07:51:53.524+03:00"
# start <- parse_date_time(start, orders = "Y-m-d H:M:OSz")
# 
# start <- format(start, "%Y-%m-%d %H:%M:%S")
# 
# end <- "2024-02-13T08:50:10.296+03:00"
# end <- parse_date_time(end, orders = "Y-m-d H:M:OSz")
# 
# end <- format(end, "%Y-%m-%d %H:%M:%S")
# 
# dt <- data.frame(
#   name = c("abdullahi","ahmed"),
#   time = c("2024-02-13T07:51:53.524+03:00","2024-02-13T07:51:56.524+03:00")
# ) %>%
#   mutate(time = format(parse_date_time(time, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S")) %>%
#   arrange(desc(as_datetime(time)))
# 
# # str(dt)
# 
# 
# dt <- data.frame(
#   name = c("a","b","c","a"),
#   value = c(1,2,4,5),
#   time = c("2024-02-13T07:51:53.524+03:00","2024-02-13T07:51:56.524+03:00"
#            ,"2024-02-13T08:55:56.524+03:00","2024-02-13T09:55:56.524+03:00")
# )%>%
#     mutate(time = format(parse_date_time(time, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S")) 
# 
# 
# dt%>%
#   group_by(name) %>%
#   arrange(as_datetime(time)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   as.data.frame()

# # 
# today <- as.Date("2024-02-19")
# # 
# tab_date = "2024-02-13"
# 
# as.Date(tab_date) < today
# 


# dt <- data.frame(
#     name = c("a","b","a","a"),
#     status = c("invalid","valid","valid","valid"),
#     value = c(1,2,4,5),
#     time = c("2024-02-13T07:51:53.524+03:00","2024-02-13T08:51:56.524+03:00"
#              ,"2024-02-13T09:55:56.524+03:00","2024-02-13T10:55:56.524+03:00")
# )%>%
#   mutate(time = format(parse_date_time(time, orders = "Y-m-d H:M:OSz")+hours(3),"%Y-%m-%d %H:%M:%S"))%>%
#   arrange(desc(status),(as_datetime(time)))%>%
#   group_by(name)%>%
#   slice(1)%>%
#   ungroup()

# Load the necessary library
# library(plotly)
# 
# # Example data
# all_report_data <- data.frame(
#   tab_date = 1:100,
#   count = cumsum(rnorm(100))
# )
# 
# 
# fig <- plot_ly(all_report_data)%>% 
#   add_lines(x = ~tab_date, y = ~count, name = "line",line = list(color = '#F99D1E'))%>%
#   add_markers(x = ~tab_date, y = ~count, name = "Market" ,marker = list(color = 'red'))%>%
#   add_text(x = ~tab_date, y = ~count,text = ~count ,name = "text")%>%
#   add_bars(x = ~tab_date, y = ~count, name = "bars",marker = list(color = '#008BA8'))
# 
# 
# fig



# 
# # Sample dataframe
# df <- data.frame(
#   hid = c(1,2,3,4,5,6),
#   enum = c(1,1,1,1,2,2),
#   comm = c(5,5,5,5,6,6),
#   status = c("i","v","v","v","v","i")
# )
# 
# # Define the function
# fuc <- function(data, en, com, id) {
#   d <- data %>%
#     filter(enum == en & comm == com & status == 'i' & hid != id)
# 
#   if(nrow(d)>=0){
#     return("i")
#   }else{
#     return("v")
#   }
# }
# 
# # Apply the function row-wise
# df <- df %>%
#   rowwise() %>%
#   mutate(verify = ifelse(status == 'i',fuc(df, enum, comm, hid),NA)) %>%
#   ungroup()  # Always ungroup after using rowwise()
# 
# # View the dataframe
# print(df)
# 
# 
# 
# 
# 
# 




# d <- data.frame(
#   id = c(1,2,3),
#   data = c("1 2","3","4 5 1 8")
# )%>%separate_rows(data, sep = " ")
# 
# 


# # Load necessary library
# library(tidyverse)
# 
# # Example dataset
# df_example <- tibble(
#   question = c(
#     "DR_level_menFGD",
#     "DR_level_womenFGD_2",
#     "DR_level_OBSERVATION",
#     "scientific_risk_assessment_menFGD_2",
#     "info_dissemination_OBSERVATION_womenFGD",
#     "education_of_children_in_DDR_menFDG"
#   )
# )
# 
# # Processing steps
# df_processed <- df_example %>%
#   mutate(
#     question_lower = str_to_lower(question),
#     question_cleaned = str_replace_all(question_lower, "(_men|_women|_observation).*?(?=_2|$)", ""),
#     question_cleaned = str_replace_all(question_cleaned, "_{2,}", "_"),
#     question_final = str_remove_all(question_cleaned, "_$")
#   )
# 
# # Display the processed dataset
# print(df_processed)

# 
# library(tidyr)
# 
# # Your original dataset
# sales_data <- data.frame(
#   Product = c("A", "A", "A", "A"),
#   year = c("2020", "2020", "2022", "2023"),
#   Month = c("January", "January", "January", "January"),
#   Sales = c(100, 150, 200, 250)
# )
# 
# # Transforming the data to a wide format
# wider_data <- pivot_wider(sales_data, names_from = Month, values_from = Sales)
# 
# wider_data
# 
# library(tidyverse)
# 
# # Example dataset
# df_example <- tibble(
#   question = c(
#     "DR_level_menFGD",
#     "DR_level_womenFGD_2",
#     "DR_level_OBSERVATION",
#     "scientific_risk_assessment_menFGD_2",
#     "info_dissemination_OBSERVATION_womenFGD",
#     "education_of_children_in_DDR_menFDG",
#     "scientific_risk_assessment_OBSERVATIONwomenFGD" # Example without underscore between OBSERVATION and women
#   )
# )
# 
# # Processing steps
# df_processed <- df_example %>%
#   mutate(
#     question_lower = str_to_lower(question),
#     # Adjust regex to handle missing underscore before "women" or "men" in OBSERVATION cases
#     question_cleaned = str_replace_all(question_lower, "(_men|_women|_observation(?:women|men)?)\\w*?(?=_2|$)", ""),
#     question_cleaned = str_replace_all(question_cleaned, "_{2,}", "_"),
#     question_final = str_remove_all(question_cleaned, "_$")
#   )
# 
# # Display the processed dataset
# print(df_processed)
# 
# 

# 
# 
# library(dplyr)
# library(stringr)
# 
# # Sample data frame
# df <- data.frame(phone_number = c("123456789", "1234567890", "0234567891", "2345678901"))
# 
# # Use mutate to conditionally remove leading zeros
# df <- df %>%
#   mutate(phone_number = sub("^0+", "", phone_number))
# 
# df
# 
# 

# df <- data.frame(
#   a = c("a","b","c"),
#   b= c(1,2,3)
# )
# 
# fig <- plot_ly(df)%>%
#   add_bars(x = ~a,
#             y = ~b,
#             color = ~a)%>%
#   layout(title = list(text = 'Most Devastating Shocks', x = 0.05, y = 0.95, xanchor = 'left', yanchor = 'top',type = 'bar'),
#          margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
#          xaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          legend = list(x = 1.05, y = 1, orientation = "v"))
# 
# fig
# 
# 
# 
# 
# 
# 
# 
# 
# 
# df <- data.frame(
#   categories = c("A", "B", "C"),
#   values = c(100, 200, 300),
#   names = c("Short Name", "Very Long Legend Name That Needs Wrapping", "Another Short Name")
# )
# 
# # Creating a bar plot and manually adding line breaks to legend names
# fig <- plot_ly(data = df) %>%
#   add_bars(x = ~categories, y = ~values, name = ~names) %>%
#   layout(legend = list(title = list(text = 'Legend')))
# 
# # Customizing the legend entry for wrapping (manual line break)
# fig$x$data[[2]]$name <- "Very Long Legend\nName That Needs\nWrapping"
# 
# # Show plot
# fig


# 
# d <- data.frame(
#   d = c(2, 1, 3, 4, 1, 2)
# )
# 
# 
# fig <- plot_ly(
#   type = 'scatterpolar',
#   r = c(2, 1, 3, 4, 1, 2),
#   theta = c('A','B','C', 'D', 'E', 'F'),
#   fill = 'toself'
# ) 
# fig <- fig %>%
#   layout(
#     polar = list(
#       radialaxis = list(
#         visible = T,
#         range = c(0,5*max(d$d))
#       )
#     ),
#     showlegend = F
#   )
# 
# fig
# 
# 
# library(plotly)
# 
# y <- c('The course was effectively<br>organized',
#        'The course developed my<br>abilities and skills for<br>the subject',
#        'The course developed my<br>ability to think critically about<br>the subject',
#        'I would recommend this<br>course to a friend')
# x1 <- c(21, 24, 27, 29)
# x2 <-c(30, 31, 26, 24)
# x3 <- c(21, 19, 23, 15)
# x4 <- c(16, 15, 11, 18)
# x5 <- c(12, 11, 13, 14)
# 
# data <- data.frame(y, x1, x2, x3, x4, x5)
# 
# top_labels <- c('Strongly<br>agree', 'Agree', 'Neutral', 'Disagree', 'Strongly<br>disagree')
# 
# fig <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
#                marker = list(color = 'rgba(38, 24, 74, 0.8)',
#                              line = list(color = 'rgb(248, 248, 249)', width = 1))) 
# fig <- fig %>% add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) 
# fig <- fig %>% add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) 
# fig <- fig %>% add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) 
# fig <- fig %>% add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) 
# fig <- fig %>% layout(xaxis = list(title = "",
#                                    showgrid = FALSE,
#                                    showline = FALSE,
#                                    showticklabels = FALSE,
#                                    zeroline = FALSE,
#                                    domain = c(0.15, 1)),
#                       yaxis = list(title = "",
#                                    showgrid = FALSE,
#                                    showline = FALSE,
#                                    showticklabels = FALSE,
#                                    zeroline = FALSE),
#                       barmode = 'stack',
#                       paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
#                       margin = list(l = 120, r = 10, t = 140, b = 80),
#                       showlegend = FALSE) 
# # labeling the y-axis
# fig <- fig %>% add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
#                                xanchor = 'right',
#                                text = y,
#                                font = list(family = 'Arial', size = 12,
#                                            color = 'rgb(67, 67, 67)'),
#                                showarrow = FALSE, align = 'right') 
# # labeling the percentages of each bar (x_axis)
# fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
#                                x = x1 / 2, y = y,
#                                text = paste(data[,"x1"], '%'),
#                                font = list(family = 'Arial', size = 12,
#                                            color = 'rgb(248, 248, 255)'),
#                                showarrow = FALSE) 
# fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
#                                x = x1 + x2 / 2, y = y,
#                                text = paste(data[,"x2"], '%'),
#                                font = list(family = 'Arial', size = 12,
#                                            color = 'rgb(248, 248, 255)'),
#                                showarrow = FALSE) 
# fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
#                                x = x1 + x2 + x3 / 2, y = y,
#                                text = paste(data[,"x3"], '%'),
#                                font = list(family = 'Arial', size = 12,
#                                            color = 'rgb(248, 248, 255)'),
#                                showarrow = FALSE) 
# fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
#                                x = x1 + x2 + x3 + x4 / 2, y = y,
#                                text = paste(data[,"x4"], '%'),
#                                font = list(family = 'Arial', size = 12,
#                                            color = 'rgb(248, 248, 255)'),
#                                showarrow = FALSE) 
# fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
#                                x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
#                                text = paste(data[,"x5"], '%'),
#                                font = list(family = 'Arial', size = 12,
#                                            color = 'rgb(248, 248, 255)'),
#                                showarrow = FALSE) 
# # labeling the first Likert scale (on the top)
# fig <- fig %>% add_annotations(xref = 'x', yref = 'paper',
#                                x = c(21 / 2, 21 + 30 / 2, 21 + 30 + 21 / 2, 21 + 30 + 21 + 16 / 2,
#                                      21 + 30 + 21 + 16 + 12 / 2),
#                                y = 1.15,
#                                text = top_labels,
#                                font = list(family = 'Arial', size = 12,
#                                            color = 'rgb(67, 67, 67)'),
#                                showarrow = FALSE)
# 
# fig
# library(plotly)
# 
# # Sample data - Replace with your actual data
# categories <- c('Category A', 'Category B', 'Category C')
# # Ensure the responses are ordered in the way that the highest Likert values are handled first
# responses <- data.frame(
#   Category = rep(categories, each = 5),
#   Likert = rep(5:1, times = length(categories)), # Reversed order
#   Count = c(30, 25, 20, 15, 10, 33, 13, 28, 18, 8, 34, 29, 24, 19, 14) # Adjusted for reversed order example
# )
# 
# # Colors for Likert scale responses (5 to 1) - dark to light
# colors <- c('#011f4b', '#03396c', '#005b96', '#6497b1', '#b3cde3') # Reversed color order
# 
# # Create the plot with reversed Likert scale
# p <- plot_ly(data = responses, type = 'bar', orientation = 'h',
#              x = ~Count, y = ~Category,
#              color = ~factor(Likert, levels = 5:1), # Ensure correct color mapping
#              colors = colors,
#              hoverinfo = 'x+y+text',
#              text = ~as.character(Likert)) %>%
#   layout(title = 'Likert Scale Responses by Category (Reversed Scale)',
#          barmode = 'stack',
#          xaxis = list(title = 'Count', autorange = 'reversed'),
#          yaxis = list(title = 'Category', categoryorder = 'total ascending'))
# 
# # Show the plot
# p
# 
# df <- data.frame(a = c('a', 'b', 'c'), b = c(1, 2, 3))
# 
# df <- df %>%
#   {
#     if (!"1" %in% names(df)) {
#         mutate(.,`new_col` = 0) # Adds "new_col" with all values set to 0
#     } else {
#         mutate(.,`new_col` = `new_col`) # Keeps the existing values of "new_col"
#     }
#     
#     if (!"2" %in% names(df)) {
#       df <- df %>%
#         mutate(`new_col2` = 0) # Adds "new_col" with all values set to 0
#     } else {
#       df <- df %>%
#         mutate(`new_col2` = `new_col2`) # Keeps the existing values of "new_col"
#     }
#   }
# # Printing the updated dataframe
# print(df)
# 

# # Create the dataframe with credentials
# memberCredentials <- data.frame(
#   users = c("user1", "user2"),
#   password = c("112233", "332211"),
#   member = c("NRC", "IRC")
# )
# 
# # Function to validate user input
# validate_credentials <- function(username_input, password_input) {
#   # Check if the input username and password match any row in the dataframe
#   match_row <- memberCredentials$users == username_input & memberCredentials$password == password_input
#   
#   # If there's a match, match_row will contain TRUE for the matching row(s)
#   if (any(match_row)) {
#     member_type <- memberCredentials$member[match_row]
#     message <- paste("Login successful! Welcome", member_type)
#     return(message)
#   } else {
#     return("Invalid username or password.")
#   }
# }
# 
# # Assuming user input is stored in variables username_input and password_input
# username_input <- "user1"
# password_input <- "112233"
# 
# # Validate user input
# result <- validate_credentials(username_input, password_input)
# print(result)

# custom_aggregate <- function(x) {
#   if (length(unique(x)) == 1) {  # If there's only one unique value, return it directly.
#     return(as.character(unique(x)))
#   }
#   
#   numeric_values <- suppressWarnings(as.numeric(x))
#   
#   if (all(is.na(numeric_values))) {
#     # All values were non-numeric.
#     return("aggregated")
#   } else if (all(!is.na(numeric_values))) {
#     # All values were numeric, return mean as a character string.
#     return(as.character(mean(numeric_values, na.rm = TRUE)))
#   } else {
#     # Mixed numeric and non-numeric values.
#     return("mixed")
#   }
# }
# 
# df <- data.frame(
#   name = c("a", "b", "c", "c", "d", "d"),
#   score = c("ali", 2, "non-numeric1", "3", 3, 4)
# )
# 
# 
# df_wide <- df %>%
#   pivot_wider(
#     names_from = name, 
#     values_from = score,
#     values_fn = list(score = custom_aggregate), # Use custom function
#     values_fill = list(score = NA) # Fill missing values if necessary
#   )

# df <- data.frame(
#   name = c("a", "b", "c", "c", "d", "d"),
#   score = c(1, 2, 2, 3, 4, 5)
# )
# 
# # Adding a phase column based on repetition within each 'name' group
# df <- df %>%
#   group_by(name) %>%
#   mutate(phase = paste0("phase", row_number())) %>%
#   ungroup()
# 
# # View the modified dataframe
# print(df)

# response_data <- GET(url = paste('https://api.ona.io/api/v1/data/788509?query={"$or":[{"project_info/District_id":"Kismayo"},{"project_info/District_id":"Afmadow"}],"project_info/Member_id":"NRC"}',sep = ""), 
#                      authenticate("abdullahiaweis","tutka_sulsulaaye@123"))
# status <- status_code(response_data)
# 
# formData <- prettify(rawToChar(response_data$content))%>% fromJSON() %>% as.data.frame()
#   
# 
# formData %<>%
#   unnest("groups_registration/group_members",keep_empty = TRUE)%>%purrr::discard(is.list) %>%
#   as.data.frame()%>%dplyr::rename("hoh_information/tel_number_"="hoh_information/tel_number")
# 
# names(formData) <- gsub(".*/", "", names(formData))
# 
# write.xlsx(formData, "BRCIS III activity registration.xlsx")

# formData$tel %>%select(Member_id,Region_id,District_id,Community_id,registration_category,groups,name,
#                   ind_sex,ind_age,ind_position,ind_position_other,tel_number,disability_,
#                   minority_,representative_criteria,vulnerability_other)
# 
# str(formData)


