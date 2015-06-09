Rcode_MAC <- function(mdy_StartDate, mdy_EndDate, Atype = "All",
                      path = "X:\\CX-DB", filename = "CX-DB Data") {
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(RODBC)
  
  GetTableFromDatabase <- function(path, file, tablename){
    originaldirectory <- getwd()
    setwd(path)
    conn <- odbcConnectAccess(file)
    table <- sqlQuery(conn, paste("SELECT * FROM ", tablename, ";", sep = ""))
    odbcClose(conn) 
    setwd(originaldirectory)
    table
  }


start <- mdy(mdy_StartDate)
end <- mdy(mdy_EndDate)

Accounts <- GetTableFromDatabase(path, filename, "Accounts")
AccountID <- GetTableFromDatabase(path, filename, "AccountID")

if (Atype == "All"){
Everything <-  merge(Accounts, AccountID, by.x = "CXID_ALL", by.y = "CXID") %>%
  filter(start <= ymd(Termination) & ymd(Termination) <= end)
} else if (Atype == "Biller"){
  Everything <-  merge(Accounts, AccountID, by.x = "CXID_ALL", by.y = "CXID") %>%
    filter(start <= ymd(Termination) & ymd(Termination) <= end) %>%
    filter(Type == "Biller")
} else if (Atype == "College"){
    Everything <-  merge(Accounts, AccountID, by.x = "CXID_ALL", by.y = "CXID") %>%
      filter(start <= ymd(Termination) & ymd(Termination) <= end) %>%
      filter(Type == "College")
} else if (Atype == "Doc"){
  Everything <-  merge(Accounts, AccountID, by.x = "CXID_ALL", by.y = "CXID") %>%
    filter(start <= ymd(Termination) & ymd(Termination) <= end) %>%
    filter(Type == "Doc")
} else {
  stop("invalid type")
}

rm(start)
rm(end)


numofObs <- as.numeric(count(Everything))
totalAnnualLoss <- as.numeric(sum(Everything$AnnualLoss))


# Cancellation by type
CXBy_Type <- Everything %>%
  select(Type, AnnualLoss) %>%
  group_by(Type) %>%
  summarize(NumOfRecords = n(), 
            NumOfRecordsPercent = paste(round(n()/numofObs*100,digits=1),"%",sep=""), 
            RevenueLost = sum(AnnualLoss), 
            RevenueLostPercent = paste(round(sum(AnnualLoss)/totalAnnualLoss*100,digits=1),"%",sep="")) %>%
  arrange(Type)


# Recommend Us
Everything$RecommendUs <- str_trim(Everything$RecommendUs)
CXBy_Recommend <- Everything %>%
  select(Code, RecommendUs) %>%
  group_by(RecommendUs) %>%
  summarize(NumOfRecords = n(),
            NumOfRecordsPercent = paste(round(n()/numofObs*100,digits=1),"%",sep="")) %>%
  arrange(RecommendUs)


# Defection Reasons
Everything$CancellationReason <- str_trim(tolower(Everything$CancellationReason))
CXReasonChart <- GetTableFromDatabase(path, filename, "CancellationReason")
CXReasonChart$CancellationReason <- tolower(CXReasonChart$CancellationReason) 
cx_everything <- merge(Everything, CXReasonChart, by.x = "CancellationReason", by.y = "CancellationReason", all.x = TRUE)
CXBy_DefectionReasons <- cx_everything %>%
  select(Code, CancellationCategory, AnnualLoss) %>%
  group_by(CancellationCategory) %>%
  summarize(NumOfRecords = n(),
            NumOfRecordsPercent = paste(round(n()/numofObs*100,digits=1),"%",sep=""), 
            RevenueLost = sum(AnnualLoss), 
            RevenueLostPercent = paste(round(sum(AnnualLoss)/totalAnnualLoss*100,digits=1),"%",sep="")) %>%
  arrange(CancellationCategory)
rm(CXReasonChart)
rm(cx_everything)


#Accounts Lost By number of Providers
CXBy_NoOfProviders <- Everything %>%
  select(NoOfProviders) %>%
  group_by(NoOfProviders) %>%
  summarize(NumOfRecords = n(),
            NumOfRecordsPercent = paste(round(n()/numofObs*100,digits=1),"%",sep="")) %>%
  arrange(NoOfProviders)


#Cancellations by payment methods
CXBy_PaymentMethod <- Everything %>%
  select(ContractTerm, AnnualLoss) %>%
  group_by(ContractTerm) %>%
  summarize(NumOfRecords = n(),
            NumOfRecordsPercent = paste(round(n()/numofObs*100,digits=1),"%",sep=""),
            RevenueLost = sum(AnnualLoss), 
            RevenueLostPercent = paste(round(sum(AnnualLoss)/totalAnnualLoss*100,digits=1),"%",sep="")) %>%
  arrange(ContractTerm)


#Accounts Cancelled By Annual Revenue
library(Hmisc)
CXBy_AnnualLoss <- data.frame(table(cut2(Everything$AnnualLoss, 
                                         c(-Inf,1000,2000,3000,4000,
                                           5000,6000,7000,8000,9000,10000, Inf))))
CXBy_AnnualLoss <- mutate(CXBy_AnnualLoss, Percent = paste(round(Freq/numofObs*100,digits=1),"%",sep=""))
names(CXBy_AnnualLoss) <- c("AnnualLoss", "NoOfAccts", "Percent")


#Cancellations by Tenure
dates <- year(ymd(Everything$Termination)) - year(ymd(Everything$Signup))
CXBy_Tenure <- data.frame(table(cut2(dates, c(-Inf,4,7,10,Inf))))
names(CXBy_Tenure) <- c("Years", "NumberOfAccounts")
rm(dates)
detach("package:Hmisc", unload=TRUE)


#Return results
list(CXBy_Type, CXBy_Recommend, CXBy_DefectionReasons, CXBy_NoOfProviders, CXBy_PaymentMethod,
     CXBy_AnnualLoss, CXBy_Tenure)
}