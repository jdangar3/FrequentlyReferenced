#### Open Packages --------------------------------------------------
lapply(c("RODBC", "dplyr", "tidyr", "ggplot2", "stringr", "choroplethr",
         "choroplethrMaps", "gridExtra", "lubridate", "zipcode"),
       suppressMessages(require),
       character.only = T)

### Define Functions ------------------------------------------------
GetTableFromDatabase <- function(path, file, tablename){
     originaldirectory <- getwd()
     setwd(path)
     
     conn <- odbcConnectAccess(file)
     table <- sqlQuery(conn, paste("SELECT * FROM ", tablename, ";", sep = ""))
     odbcClose(conn)
     
     
     setwd(originaldirectory)
     table
}

### Load Data ---------------------------------------------------------
Today <- ymd("2015-05-11")
Accounts <- GetTableFromDatabase("X:\\CX-DB", "CX-DB Data", "Accounts")
AccountID <- GetTableFromDatabase("X:\\CX-DB", "CX-DB Data", "AccountID")

Data_C <- merge(Accounts, AccountID, "CXID", "CXID_ALL", all.x = TRUE)
names(Data_C)[30] <- "Collection" #This makes life easier

SalesReps_and_Locations <- read.csv("X:\\SalesReps_Locations.csv")
Data_A <- read.csv("X:\\703 Active Client Snapshot 20150511.csv")

### Filtering --------------------------------------------------
#Filtering
foo1 <- group_by(Data_C, Location) %>%
     summarize(Count = n(), #Calculates number of cancellations
               Tenure = mean(as.numeric(substr(as.character(as.period(interval(ymd(as.character(Signup)),
                                                                               ymd(as.character(Termination))),
                                                                      unit = "day")),
                                               1,
                                               regexpr("d",
                                                       as.character(as.period(interval(ymd(as.character(Signup)),
                                                                                       ymd(as.character(Termination))),
                                                                              unit = "day"))) - 1)), na.rm = T))

foo1 <- foo1[foo1$Location %in% state.abb,] #Only looks at United States

#Trim spaces in all columns
for (i in 1:ncol(Data_A)){
     Data_A[,i] <- as.character(str_trim(as.character(Data_A[,i])))    
}
# This entry was entered incorrectly. This is the fix
for(i in 2:13){
     if (i == 2){
          Data_A[Data_A$Account.Code == "Mhmmd", 
                 2] <- paste(Data_A[Data_A$Account.Code == "Mhmmd",2],
                             Data_A[Data_A$Account.Code == "Mhmmd", 3])
     } else {
          Data_A[Data_A$Account.Code == "Mhmmd", 
                 i] <- Data_A[Data_A$Account.Code == "Mhmmd", i + 1]    
     }
}

# One other small anomaly that needs to be fixed
Data_A[Data_A$State == "california", 12] <- "CA"
Data_A <- filter(Data_A, State %in% state.abb)

foo2 <- filter(Data_A, !(Specialty == "Billing Company")) %>%
     filter(!(grepl("@nuesoft.com", Email))) %>%
     filter(!(grepl("[Tt]est", Account.Code))) %>%
     filter(!(grepl("[Dd]emo", Account.Name))) %>%
     filter(!(substring(Account.Name, 1, 2) == "QA")) %>%
     filter(!(grepl("1685 Terrell Mill", Address.Line.1))) %>%
     separate(Account.Age, into = c("Year", "Month", "Day"))
foo2$Month <- unlist(lapply(foo2$Month, FUN = function(x)
     if(nchar(x) == 1){
          paste("0",
                x,
                sep = "")
     } else {
          x
     }))

Tenure_UpperBound <- as.numeric(substr(as.period(as.interval(ymd("2009-1-1"), 
                                                             ymd(Today)), 
                                                 unit = "days"),
                                       1,
                                       regexpr("d",
                                               as.period(as.interval(ymd("2009-1-1"), 
                                                                     ymd(Today)),
                                                         unit = "days")) - 1))
foo2 <- cbind(foo2, Tenure = 0)
for (i in 1:nrow(foo2)){
     range <- period(c(as.numeric(foo2$Year[i]), 
                       as.numeric(foo2$Month[i]), 
                       as.numeric(foo2$Day[i])), 
                     units = c("year", "month", "day"))
     new_date <- ymd("2009-1-1") + range
     foo2$Tenure[i] <- as.numeric(substr(as.period(as.interval(ymd("2009-1-1"),
                                                               new_date), 
                                                   unit = "days"),
                                         1,
                                         regexpr("d",
                                                 as.period(as.interval(ymd("2009-1-1"),
                                                                       new_date), 
                                                           unit = "days")) - 1))
}

foo3 <- foo2
foo2 <- foo2[foo2$Tenure <= Tenure_UpperBound,]
foo2 <- filter(foo2, State %in% state.abb)

foo2 <- group_by(foo2, State) %>%
     summarize(Count = n(), #Calculates number of cancellations
               Tenure = mean(Tenure, na.rm = T))

foo3 <- group_by(foo3, State) %>%
     summarize(Count = n(), #Calculates number of cancellations
               Tenure = mean(Tenure, na.rm = T))

### Merge Datasets --------------------------------------------
Data <- merge(foo1, foo2, by.x = "Location", by.y = "State", all.x = T, all.y = T)
names(Data) <- c("region", "C_Count", "C_Tenure", "A_Count", "A_Tenure")
Data <- merge(Data, foo3, by.x = "region", by.y = "State", all.x = T, all.y = T)
names(Data) <- c("region", "C_Count", "C_Tenure", "A_Count", "A_Tenure",
                 "T_Count", "T_Tenure")
Data <- Data[,c(1, 6, 4, 2, 7, 5, 3)]
Data$region <- as.character(Data$region)

for (i in 1:length(Data$region)){
     Data$region[i] <- tolower(state.name[match(Data$region[i], state.abb)])
}

data(zipcode)
colnames(zipcode) = c("region", "city", "state", "latitude", "longitude")
sales_points = merge(SalesReps_and_Locations, zipcode, by.x = 'Zip', by.y = 'region')

### Define Extra Variables --------------------------------------------------
palette <- colorRampPalette(c(rgb(255, 255, 255, max = 255),
                              rgb(0, 179, 173, max = 255)))
theme <- theme(plot.title=element_text(size = rel(2),
                                       colour ="#777777",
                                       vjust=1),
               axis.title.y = element_text(size = rel(1.75), 
                                           angle = 90, 
                                           colour="#777777",
                                           vjust=1.5),
               axis.title.x = element_text(size = rel(1.75),
                                           angle = 0, 
                                           colour="#777777",
                                           vjust=-0.5),
               axis.text = element_text(size=rel(1.5),
                                        colour="#32363f"),
               panel.grid.major=element_line(colour="#aeb0b6",
                                             size=.5),
               panel.grid.minor=element_blank(),
               axis.ticks=element_blank(),
               text=element_text(family="sans"))

theme2 <- theme(plot.title=element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size=rel(1.3),
                                           colour="#32363f",
                                           angle = 90),
                axis.text.y = element_text(size = rel(1.3),
                                           colour="#32363f"),
                panel.background=element_rect(fill="#FFFFFF"),
                plot.background=element_rect(fill="#FFFFFF"),
                panel.grid.major=element_line(colour="#aeb0b6",
                                              size=.5),
                panel.grid.minor=element_blank(),
                axis.ticks=element_blank(),
                text=element_text(family="sans"))

### Create Ratio Column ---------------------
Data <- mutate(Data, ratio = C_Count / A_Count)
Data$region <- unlist(lapply(Data$region, FUN = function(x)
     if(grepl(" ", x)){
          x <- paste(toupper(substr(x, 1, 1)),
                     substr(x, 2, regexpr(" ", x)),
                     toupper(substr(x, 
                                    regexpr(" ",
                                            x) + 1,
                                    regexpr(" ",
                                            x) + 1)),
                     substr(x, regexpr(" ", x) + 2, 
                            nchar(x)),        
                     sep = "")   
     } else {
          x <- paste(toupper(substr(x, 1, 1)),
                     substr(x, 2, nchar(x)),
                     sep = "") 
     }))

### Plot Ratios -----------------------------
Data_ratios <- Data[!(is.na(Data$ratio)), ] %>%
     arrange(ratio)
Pos_five <- Data_ratios[(nrow(Data_ratios) - 4):nrow(Data_ratios), ]
neg_five <- Data_ratios[1:5, ]


Pos_five$region <- factor(Pos_five$region, levels <- arrange(Pos_five,
                                                             desc(ratio))$region)
neg_five$region <- factor(neg_five$region, levels <- arrange(neg_five,
                                                             desc(ratio))$region)

p_pos_five <- ggplot(data = Pos_five,
                     aes(x = region,
                         y = ratio))
p_pos_five <- p_pos_five + geom_bar(stat = "identity",
                                    fill = rgb(0, 179, 173, max = 255))
p_pos_five <- p_pos_five + theme2
p_pos_five <- p_pos_five + labs(x = "State",
                                title = "Highest Ratios")
p_pos_five <- p_pos_five + scale_y_continuous(limits = c(0, max(Pos_five$ratio) + 0.2),
                                 breaks = seq(0, max(Pos_five$ratio) + 0.2,
                                              by = 0.5),
                                 labels = seq(0, max(Pos_five$ratio) + 0.2,
                                              by = 0.5))
png("Highest_Ratios.png", height = 300, width = 450)
p_pos_five
dev.off()

p_neg_five <- ggplot(data = neg_five,
                     aes(x = region,
                         y = ratio))
p_neg_five <- p_neg_five + geom_bar(stat = "identity",
                                    fill = rgb(0, 179, 173, max = 255))
p_neg_five <- p_neg_five + theme2
p_neg_five <- p_neg_five + labs(x = "State",
                                title = "Lowest Ratios")
p_neg_five <- p_neg_five + scale_y_continuous(limits = c(0, max(Pos_five$ratio) + 0.2),
                                              breaks = seq(0, max(Pos_five$ratio) + 0.2,
                                                           by = 0.5),
                                              labels = seq(0, max(Pos_five$ratio) + 0.2,
                                                           by = 0.5))
png("Bottom_Five_States.png", height = 300, width = 450)
p_neg_five
dev.off()

Best_And_Worst <- rbind(Pos_five, neg_five)[ , c(1, 8, 5)]
Best_And_Worst[ , 3] <- round(Best_And_Worst[ , 3] / 365.242, 2)
Best_And_Worst <- arrange(Best_And_Worst, desc(ratio))
write.csv(Best_And_Worst, "Attrition_Rates.csv")

### Plot Accts_Active_Since_2009 ------------------------
max1 <- max(max(Data[ , 3], na.rm = T), 
            max(Data[ , 4], na.rm = T))
labels <- character()
for (i in 1:7){
     labels <- c(labels, paste(floor(max1 * (i - 1) / 7), 
                               " - ", 
                               floor(max1 * i / 7),
                               sep = ""))
}


names(Data)[3] <- "value"
Data$value <- cut(Data$value, 
                  floor(seq(0, max1, max1 / 7)),
                  labels = labels)
p1 = StateChoropleth$new(Data)
p1$ggplot_scale = scale_fill_manual(name = "# of Accounts", 
                                    values = palette(7),
                                    drop = F)
p1 <- p1$render()
p1 <- p1 + geom_point(data = sales_points,
                      aes(x = longitude,
                          y = latitude,
                          group = state),
                      col = rgb(238, 128, 179, max = 255),
                      size = rel(3))
p1 <- p1 + labs(title = "Active")
p1 <- p1 + theme
p1 <- p1 + scale_x_continuous(name = "", breaks = NULL)
p1 <- p1 + scale_y_continuous(name = "", breaks = NULL)
names(Data)[3] <- "A_Count"
png("Accts_Active_Since_2009.png", 550, 420)
p1
dev.off()

### Plot Accts_Cancelled_Since_2009 ------------------------
names(Data)[4] <- "value"
Data$value <- cut(Data$value, 
                  floor(seq(0, max1, max1 / 7)),
                  labels = labels)
p2 = StateChoropleth$new(Data)
p2$ggplot_scale = scale_fill_manual(name = "# of Accounts", 
                                    values = palette(7),
                                    drop = FALSE)
p2 <- p2$render()
p2 <- p2 + geom_point(data = sales_points,
                      aes(x = longitude,
                          y = latitude,
                          group = state),
                      col = rgb(238, 128, 179, max = 255),
                      size = rel(3))
p2 <- p2 + labs(title = "Cancelled")
p2 <- p2 + theme
p2 <- p2 + scale_x_continuous(name = "", breaks = NULL)
p2 <- p2 + scale_y_continuous(name = "", breaks = NULL)
p2 <- p2 + theme(legend.position = "none")
names(Data)[4] <- "C_Count"
png("Accts_Cancelled_Since_2009.png", 450, 420)
p2
dev.off()

### Plot Tenure_Active_Since_2009 ------------------------
max2 <- ceiling(max(max(Data[ , 6], na.rm = T), max(Data[ , 7], na.rm = T)) / 365.242)
labels <- character()
for (i in 1:max2){
     labels <- c(labels, paste(i - 1, " - ", i, sep = ""))
}


names(Data)[6] <- "value"
Data$value <- Data$value / 365.242
Data$value <- cut(Data$value, c(0:max2),
                  labels = labels)
p3 = StateChoropleth$new(Data)
p3$ggplot_scale = scale_fill_manual(name = "Years", 
                                    values = palette(max2),
                                    drop = FALSE)
p3 <- p3$render()
p3 <- p3 + geom_point(data = sales_points,
                      aes(x = longitude,
                          y = latitude,
                          group = state),
                      col = rgb(238, 128, 179, max = 255),
                      size = rel(3))
p3 <- p3 + labs(title = "Active Clients")
p3 <- p3 + theme
p3 <- p3 + scale_x_continuous(name = "", breaks = NULL)
p3 <- p3 + scale_y_continuous(name = "", breaks = NULL)
names(Data)[6] <- "A_Tenure"
png("Tenure_Active_Since_2009.png", 550, 420)
p3
dev.off()

### Plot Tenure_Cancelled_Since_2009 ------------------------
names(Data)[7] <- "value"
Data$value <- Data$value / 365.242
Data$value <- cut(Data$value, c(0:max2),
                  labels = labels)
p4 = StateChoropleth$new(Data)
p4$ggplot_scale = scale_fill_manual(values = palette(max2),
                                    drop = FALSE)
p4 <- p4$render()
p4 <- p4 + geom_point(data = sales_points,
                      aes(x = longitude,
                          y = latitude,
                          group = state),
                      col = rgb(238, 128, 179, max = 255),
                      size = rel(3))
p4 <- p4 + labs(title = "Cancelled Clients")
p4 <- p4 + theme
p4 <- p4 + scale_x_continuous(name = "", breaks = NULL)
p4 <- p4 + scale_y_continuous(name = "", breaks = NULL)
p4 <- p4 + theme(legend.position = "none")
names(Data)[7] <- "C_Tenure"
png("Tenure_Cancelled_Since_2009.png", 450, 420)
p4
dev.off()

### Plot Accts_Active ------------------------
names(Data)[2] <- "value"
labels <- character()
max3 <- max(Data[ , 2])
for (i in 1:7){
     labels <- c(labels, paste(floor(max3 * (i - 1) / 7), 
                               " - ", 
                               floor(max3 * i / 7), 
                               sep = ""))
}
Data$value <- cut(Data$value, 
                  floor(seq(0, max3, max3 / 7)), 
                  labels)
p5 = StateChoropleth$new(Data)
p5$ggplot_scale = scale_fill_manual(name = "# of Accts.", 
                                    values = palette(7),
                                    drop = FALSE)
p5 <- p5$render()
p5 <- p5 + geom_point(data = sales_points,
                      aes(x = longitude,
                          y = latitude,
                          group = state),
                      col = rgb(238, 128, 179, max = 255),
                      size = rel(3))
p5 <- p5 + labs(title = "Count of Active Clients")
p5 <- p5 + theme
p5 <- p5 + scale_x_continuous(name = "", breaks = NULL)
p5 <- p5 + scale_y_continuous(name = "", breaks = NULL)
names(Data)[2] <- "T_Count"
png("Accts_Active.png", 500, 400)
p5
dev.off()

### Plot Tenure_Active ------------------------
names(Data)[5] <- "value"
max4 <- ceiling(max(Data$value) / 365.242)
labels <- character()
for (i in 1:max4){
     labels <- c(labels, paste(i - 1, " - ", i, sep = ""))
}
Data$value <- Data$value / 365.242
Data$value <- cut(Data$value, c(0:max4),
                  labels = labels)
p6 = StateChoropleth$new(Data)
p6$ggplot_scale = scale_fill_manual(name = "Years", 
                                    values = palette(max4),
                                    drop = FALSE)
p6 <- p6$render()
p6 <- p6 + geom_point(data = sales_points,
                      aes(x = longitude,
                          y = latitude,
                          group = state),
                      col = rgb(238, 128, 179, max = 255),
                      size = rel(3))
p6 <- p6 + labs(title = "Average Stay of Active Clients")
p6 <- p6 + theme
p6 <- p6 + scale_x_continuous(name = "", breaks = NULL)
p6 <- p6 + scale_y_continuous(name = "", breaks = NULL)
names(Data)[5] <- "T_Tenure"
png("Tenure_Active.png", 500, 400)
p6
dev.off()

### Write previous six plots to a new csv ----
write.csv(Data, "States_and_Intervals.csv")


