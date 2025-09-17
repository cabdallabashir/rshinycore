
createDb<- function(){
  MainDB <- dbConnect(RSQLite::SQLite(), "db/coreDB.db")
  dbDisconnect(MainDB)
  return(TRUE)
}


createTable <- function(conn , tableName , dataFrame){
  
  dbWriteTable(conn, tableName, dataFrame,overwrite = TRUE)
 
  return(TRUE)
}


createTableWithAppend <- function(tableName , dataFrame){
  conn <- dbConnect(RSQLite::SQLite(), "db/coreDB.db")
  dbWriteTable(conn, tableName, dataFrame , append = TRUE)
  dbDisconnect(conn)
  return(TRUE)
}

listTables <- function(tables){
  conn <- dbConnect(RSQLite::SQLite(), "db/coreDB.db")
  listTables <- dbListTables(conn)
  dbDisconnect(conn)
  return(
    listTables
  )
}

readTable <- function(conn,table){
  tableResult <- dbReadTable(conn , table)
  return(
    tableResult
  )
}

query <- function(query){
  conn <- dbConnect(RSQLite::SQLite(), "db/coreDB.db")
  tableResult <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  return(
    tableResult
  )
}

execute <- function(query){
  conn <- dbConnect(RSQLite::SQLite(), "db/coreDB.db")
  dbSendQuery(conn,query)
  dbClearResult(conn)
  dbDisconnect(conn)
  return(
    TRUE
  )
}

getOnaData <- function(){

}


