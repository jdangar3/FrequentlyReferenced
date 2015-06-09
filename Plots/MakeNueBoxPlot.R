MakeNueBoxPlot <- function(mdy_Start_String, mdy_End_String, 
                           x_axis = "CancellationCategory",
                           y_axis = "Tenure"){

     
require(ggplot2)
require(dplyr)
require(lubridate)
require(scales)
require(stringr)

#Miracle working table getter function
GetTableFromDatabase <- function(path, file, tablename){
     require(RODBC)
     originaldirectory <- getwd()
     setwd(path)
     conn <- odbcConnectAccess(file)
     table <- sqlQuery(conn, paste("SELECT * FROM ", tablename, ";", sep = ""))
     odbcClose(conn) 
     setwd(originaldirectory)
     table
}

labeller <- function(){
     response <- character()
     for (i in 1:length(labels$NumberOfObs)){
          response <- c(response,
                         paste("n = ", labels$NumberOfObs[i],
                        "\n",
                        "Outliers= ", labels$OutliersOmitted[i],
                        sep = ""))
     }
     response
}

path <- "\\\\nue-server1\\Media\\CX-DB"
filename <- "CX-DB Data"

start <- mdy(mdy_Start_String)
end <- mdy(mdy_End_String)

p1theme <- theme(plot.title=element_text(size = rel(2),
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
                 panel.background=element_rect(fill="#FFFFFF"),
                 plot.background=element_rect(fill="#FFFFFF"),
                 panel.grid.major=element_line(colour="#aeb0b6",
                                               size=.5),
                 panel.grid.minor=element_blank(),
                 axis.ticks=element_blank(),
                 text=element_text(family="sans"))


nueColors <- rep(c(rgb(0,179,173, maxColorValue = 255),
               rgb(238,128,179, maxColorValue = 255), 
               rgb(180,155,202, maxColorValue = 255),
               rgb(248, 159, 89, maxColorValue = 255)), 20)

Accounts <- GetTableFromDatabase(path, filename, "Accounts")
Accounts$CancellationReason <- str_trim(tolower(Accounts$CancellationReason))
CXReasonChart <- GetTableFromDatabase(path, filename, "CancellationReason")
CXReasonChart$CancellationReason <- tolower(CXReasonChart$CancellationReason) 
cx_everything <- merge(Accounts, 
                       CXReasonChart, 
                       by.x = "CancellationReason", 
                       by.y = "CancellationReason", 
                       all.x = TRUE)


# Gets the second two data points. Groups by tenure and Cancellation Category
Accounts<- cx_everything  %>%
     filter(start <= ymd(Termination) & ymd(Termination) <= end) %>%
     mutate(Tenure = day(as.period(new_interval(ymd(Signup), 
                                                ymd(Termination)),
                                   unit = "days")) / 365.00)

Car <-na.omit(Accounts[,c(x_axis, y_axis)])


options <- unique(factor(Car[,1]))

namer <- character()
Max_ys <- data.frame()
lab1 <- data.frame()
lab2 <- data.frame()
for (i in options){
     foo <- Car[Car[,1] == i,]
     range <- quantile(foo[,2], 0.75) - quantile(foo[,2], 0.25)
     range <- range * 1.5
     upper <- range + quantile(foo[,2], 0.75)
     foo2 <- foo[foo[,2] > upper,]
     for (j in 1:length(options)){
          if (i == options[j]){
               if (is.na(upper)){
                    upper <- 0
               }
               if (upper > 0){
                    Max_ys <- rbind(Max_ys, 
                                    data.frame(variable = as.numeric(upper),
                                               factor = i))
               }
               if (nrow(foo) > 0){
                    lab1 <- rbind(lab1, 
                                  data.frame(NumberOfObs = as.numeric(nrow(foo)),
                                             factor = i)) 
               }
               lab2 <- rbind(lab2, 
                             data.frame(OutliersOmitted = as.numeric(nrow(foo2)),
                                        factor = i))
               namer <- c(namer, as.character(options[j]))
          }
     }
}

labels <- merge(lab1, lab2, by.x = "factor", by.y = "factor")
labels <- merge(labels, Max_ys, by.x = "factor", by.y = "factor")
labels <- arrange(labels, as.character(factor))


Car <- Car[Car[,1] %in% namer,]
Car[,1] <- factor(Car[,1])
x1 <- names(c(by(Car[,2], Car[,1], median)))

labels <- data.frame(NumberOfObs = labels$NumberOfObs,
                     OutliersOmitted = labels$OutliersOmitted,
                     x1 = labels$factor,
                     Max_ys = labels$variable)


gety <- function(){
     max(labels$Max_ys)
}

p2 <- ggplot(Car, aes_string(x = x_axis,
                      y = y_axis))
p2 <- p2 + geom_boxplot(outlier.shape = NA,
                        aes_string(fill = x_axis))
p2 <- p2 + coord_cartesian(ylim = c(0, gety() * 1.2))
p2 <- p2 + stat_boxplot(geom = "errorbar")
p2 <- p2 + scale_fill_manual(values = nueColors)
p2 <- p2 + labs(x = names(Car)[1],
                y = names(Car)[2],
                fill = names(Car)[1],
                title = paste(names(Car)[2],
                              " by ",
                              names(Car)[1],
                              "\n", 
                              toString(start),
                              " : ",
                              toString(end)))
p2 <- p2 + p1theme
p2 <- p2 + geom_text(data = labels,
                     aes_string(x = "x1",
                         y = gety() * 1.1,
                         label = deparse(labeller())),
                     col = rgb(218,82,83, maxColorValue = 255),
                     size = 6)
p2
}
