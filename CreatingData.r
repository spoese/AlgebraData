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
        temp <- read_xlsx("Nov20Data.xlsx",i,skip=9)
        tempNames <- paste("X",1:dim(temp)[2],sep="")
        initial.list[[i]] <- read_xlsx("Nov20Data.xlsx", i,
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
                x <- x[,c(1:47,89,48,49,90,50,51,91,52:88)]
        } else if (unique(x$When == "Late") & unique(x$Days != 1)) {
                x <- mutate(x,Topics11=NA,Topics12=NA,Assignment3C=NA,Assignment12A=NA,Assignment12B=NA)
                x <- x[,c(1:44,87,88,45:51,89,52:67,90,91,68:86)]
        } else if (unique(x$Days == 1)) {
                x <- mutate(x,Topics1=NA,Topics12=NA,Assignment1B=NA,Assignment12B=NA)
                x <- x[,c(1:34,88,35:44,89,45,90,46:68,91,69:87)]
        } else {
                x <- mutate(x,Topics12=NA,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA)
                x <- x[,c(1:45,88,46,89,47,48,90,49,50,91,51:87)]
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

#Fix the order of Nying's Tests
dat[dat$Class.Name == "24567",7:22] <- dat[dat$Class.Name == "24567",c(11:14,19:22,7:10,15:18)]

#Change time spent to numeric
dat$Total.Time <- as.numeric(dat$Total.Time)
dat$Time.In.Class <- as.numeric(dat$Time.In.Class)

#Change filters to factor
dat[,88:91] <- apply(dat[,88:91],2,as.factor)

nameToID <- read_csv("NameIdReference.csv")
dat$Student.Name <- tolower(dat$Student.Name)
dat <- merge(dat,nameToID)[,c(92,2:91)]

write_csv(dat,"FormattedDataNov20.csv")