library(dplyr)
library(shiny)

library(shinyWidgets)
# library(shiny.router)
library(lubridate)
library(RSQLite)
library(readxl)
library(openxlsx)
library(leaflet)

library(palmerpenguins)
library(leaflet)
library(sp)
library(sf)
library(ggplot2)
library(httr)

library(arrow)
library(jsonlite)
library(splitstackshape)
library(DBI) 

library(zoo)
library(data.table)

library(sodium)

library(naniar)
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
library(RPostgres)
library(scales)
library(patchwork)

library(haven)

library(stringr)

# install.packages("gt")

# install.packages("renv")
# renv::update()
# update.packages()

# Initialize renv (if not already done)
#renv::init()

# Install RPostgres package
#renv::install("RPostgres")

# Snapshot the current state of the project's library
#renv::snapshot()



source("db/dbCreation.R")
# source("functions/getEWEA.R")
source("functions/onaLogin.R")

escape_regex <- function(string) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
}



somalia <- read_sf("docs/shp/Somalia.shp")## read Somalia country boundary

somalia_districts <-read_sf("docs/shp/Districts.shp") %>%sf::st_as_sf()

somalia_districts_communities <- read_csv("docs/shp/final BRCIS 3 communities points.csv")

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
  "brcisRegistration",
  "oasisRegistration",
  "updateitt",
  "oasis_arcd",
  "ewea",
  "new_ewea",
  "ittdash"
)






reverine_districts <- c(
  "Afgoye", "Baardhere", "Belet_Hawa", "Belet_weyne", "Jawhar" , "Kismayo"
)

danwadaag_communities <- c(
  "baidoa__ca1", "baidoa__ca3", "baidoa__ca4", "baidoa__ca11", "Shida" , "Wadajir",
  "Fanole","Calanley","Luglow" , "Qurdubey","Kaxaarey","Oktober","x_keskey","ca19","ca6"
)


# brcs_data_bseline <- read_csv("../../../../../projects/BRCIS/MIDLINE Cleaning/data from database.csv")
# 
# # %>%
# #   filter(fixed != "#N/A" & selection_method == "1")
# 
# 
# 
# postgresConnection <- dbConnect(RPostgres::Postgres(),
#                                 dbname = "brcisShiny",
#                                 host = "brcisproddb.postgres.database.azure.com", port = 5432,
#                                 user = "brcisShiny", password = "brcisShiny@112233@",
#                                 sslmode = 'require')
# 
# dbWriteTable(postgresConnection, name = "brcisbasematchedresults", value = brcs_data_bseline, append = FALSE,overwrite = TRUE)


# data_with_registration <- read.csv("../../../../../projects/BRCIS/MIDLINE Cleaning/Matched Participants with registration activities.csv")
# activities_registered <- read.xlsx("../../../../../projects/BRCIS/MIDLINE Cleaning/matched registration activities.xlsx")
# 
# 
# data_with_registration%>%
#   separate_rows(regis_activities_list2 , sep = " ")%>%
#   filter(regis_activities_list2!="" )%>%
#   left_join(activities_registered)%>%
#   write.xlsx("../../../../../projects/BRCIS/MIDLINE Cleaning/Matched Participants with registration activities final.xlsx")
#   



