GetTableFromDatabase <- function(path, file, tablename){
  originaldirectory <- getwd()
  setwd(path)
  
  library(RODBC)
  conn <- odbcConnectAccess(file)
  table <- sqlQuery(conn, paste("SELECT * FROM ", tablename, ";", sep = ""))
  odbcClose(conn)
  
  
  setwd(originaldirectory)
  table
}