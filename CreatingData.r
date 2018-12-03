library(tidyverse)
library(readxl)
library(lubridate)
dat <- tibble()
classes <- read.csv("Classes.csv",stringsAsFactors = FALSE)
cNames <- read.csv("cNames.csv",stringsAsFactors = FALSE,header = FALSE)[,1]
#students <- read_xlsx("MATH050 Students.xlsx")
#middles <- read_xlsx("middles.xlsx")
initial.list <- list()
for (i in 1:52) {
        print(paste("Reading sheet",i))
        temp <- read_xlsx("Dec3Data.xlsx",i,skip=9)
        tempNames <- paste("X",1:dim(temp)[2],sep="")
        initial.list[[i]] <- read_xlsx("Dec3Data.xlsx", i,
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
                x <- mutate(x,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA,Assignment13B=NA)
                x <- x[,c(1:53,100,54,55,101,56,57,102,58:76,103,77:99)]
        } else if (unique(x$When == "Late") & unique(x$Days != 1)) {
                x <- mutate(x,Topics11=NA,Topics14=NA,Assignment3C=NA,Assignment12B=NA,Assignment14A=NA,Assignment14B=NA)
                x <- x[,c(1:48,98,49,50,99,51:57,100,58:74,101,75,76,102,103,77:97)]
        } else if (unique(x$Days == 1)) {
                x <- mutate(x,Test6.Date=NA,Test6.Reason=NA,Test6.Percent=NA,Test6.Topics=NA,
                            Topics1=NA,Topics14=NA,
                            Assignment1B=NA,Assignment12B=NA,Assignment14A=NA,Assignment14B=NA)
                x <- x[,c(1:30,94:97,31:34,98,35:46,99,47,100,48:70,101,71,72,102,103,73:93)]
        } else {
                x <- mutate(x,Topics12=NA,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA,Assignment13B=NA)
                x <- x[,c(1:49,99,50:53,100,54,55,101,56,57,102,58:76,103,77:98)]
        }
}

for (i in 1:length(initial.list)){
        initial.list[[i]][,2] <- get_CRNs(initial.list[[i]])
        initial.list[[i]] <- adjustments(initial.list[[i]])
        names(initial.list[[i]]) <- cNames
        dat <- rbind(dat,initial.list[[i]])
        print(i)
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

#Fix the order of Nying's Tests
dat[dat$Class.Name == "24567",7:22] <- dat[dat$Class.Name == "24567",c(11:14,19:22,7:10,15:18)]

#Change time spent to numeric
dat$Total.Time <- as.numeric(dat$Total.Time)
dat$Time.In.Class <- as.numeric(dat$Time.In.Class)

#Change filters to factor
dat[,100:103] <- apply(dat[,100:103],2,as.factor)

nameToID <- read_csv("NameIdReference.csv")
dat$Student.Name <- tolower(dat$Student.Name)
dat <- merge(dat,nameToID)[,c(104,2:103)]

write_csv(dat,"FormattedDataDec3.csv")
