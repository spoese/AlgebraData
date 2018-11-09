library(tidyverse)
library(readxl)
library(lubridate)
dat <- tibble()
classes <- read.csv("Classes.csv",stringsAsFactors = FALSE)
cNames <- read.csv("cNames.csv",stringsAsFactors = FALSE,header = FALSE)[,1]
students <- read_xlsx("MATH050 Students.xlsx")
middles <- read_xlsx("middles.xlsx")
initial.list <- list()
for (i in 1:52) {
        print(paste("Reading sheet",i))
        temp <- read_xlsx("Nov8Data.xlsx",i,skip=9)
        tempNames <- paste("X",1:dim(temp)[2],sep="")
        initial.list[[i]] <- read_xlsx("Nov8Data.xlsx", i,
                                       col_names = tempNames,
                                       col_types = c(rep("guess",dim(temp)[2]-2),rep("text",2)),
                                       na = "-",
                                       skip = 9)
}


initial.list <- lapply(initial.list,merge,classes,by.x="X2",by.y="Class.Name")
initial.list <- lapply(initial.list,function(x) {x[,c(2,1,3:length(names(x)))]})

get_CRNs <- function(x){
        temp <- str_sub(x[,2],-5,-1)
        return(temp)
}

adjustments <- function(x){
        if (unique(x$Where) == "DL" | x[1,2] %in% c("24410","24411","24438")){
                x <- mutate(x,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA)
                x <- x[,c(1:41,78,42,43,79,44,45,80,46:77)]
        } else if (unique(x$When == "Late") & unique(x$Days != 1)) {
                x <- mutate(x,Topics10=NA,Assignment3C=NA,Assignment10B=NA,Assignment11A=NA)
                x <- x[,c(1:39,77,40:46,78,47:59,79,80,60:76)]
        } else if (unique(x$Days == 1)) {
                x <- mutate(x,Topics1=NA,Topics10=NA,Assignment1B=NA,Assignment10B=NA,Assignment11A=NA)
                x <- x[,c(1:30,76,31:38,77,39,78,40:58,79,80,59:75)]
        } else {
                x <- mutate(x,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA)
                x <- x[,c(1:41,78,42,43,79,44,45,80,46:77)]
        }
}

for (i in 1:length(initial.list)){
        initial.list[[i]][,2] <- get_CRNs(initial.list[[i]])
        initial.list[[i]] <- adjustments(initial.list[[i]])
        names(initial.list[[i]]) <- cNames
        dat <- rbind(dat,initial.list[[i]])
}

#Deal with duplicate student names (check Class Changed reason for IKC)
#Decided it was too difficult to amalgamate the results so we will remove duplicates
dat <- filter(dat,!(Student.Name %in% 
                            dat$Student.Name[duplicated(dat$Student.Name) | 
                                                     duplicated(dat$Student.Name, 
                                                                fromLast = TRUE)]))

#Change dates to lubridates
dates <- grep("Date",names(dat))
dat[,dates] <- lapply(dat[,dates],ymd_hms)

#If Test 3 is in the Test 1 spot, rearrange
dat[dat$Class.Name == "24567",7:22] <- dat[dat$Class.Name == "24567",c(11:14,19:22,7:10,15:18)]

#Change time spent to numeric
dat$Total.Time <- as.numeric(dat$Total.Time)
dat$Time.In.Class <- as.numeric(dat$Time.In.Class)

#Change filters to factor
dat[,77:80] <- apply(dat[,77:80],2,as.factor)

write_csv(dat,"FormattedDataNov8.csv")

aleks <- tibble()
for (i in 1:1156) {
        if(!(dat[i,]$Student.Name %in% complete_names)) {
                aleks <- rbind(aleks,dat[i,])
        }
}

mc <- tibble()
for (i in 1:1241) {
        if (!(middle.m[i,]$Student.Name %in% complete_names)) {
                mc <- rbind(mc,middle.m[i,])
        } 
}