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
        temp <- read_xlsx("Dec17Data.xlsx",i,skip=9)
        tempNames <- paste("X",1:dim(temp)[2],sep="")
        initial.list[[i]] <- read_xlsx("Dec17Data.xlsx", i,
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
                x <- x[,c(1:57,108,58,59,109,60,61,110,62:80,111,81:107)]
        } else if (unique(x$When == "Late") & unique(x$Days != 1)) {
                x <- mutate(x,Topics11=NA,Topics14=NA,Assignment3C=NA,Assignment12B=NA,Assignment15A=NA,Assignment15B=NA)
                x <- x[,c(1:52,106,53,54,107,55:61,108,62:78,109,79:82,110,111,83:105)]
        } else if (unique(x$Days == 1)) {
                x <- mutate(x,Topics1=NA,Assignment1B=NA,Assignment12B=NA,Assignment14B=NA,Assignment15B=NA)
                x <- x[,c(1:42,107,43:56,108,57:79,109,80:82,110,83,111,84:106)]
        } else {
                x <- mutate(x,Topics12=NA,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA,Assignment13B=NA)
                x <- x[,c(1:53,107,54:56,108,57,58,109,59,60,110,61:79,111,80:106)]
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
dat[,108:111] <- apply(dat[,108:111],2,as.factor)

nameToID <- read_csv("NameIdReference.csv")
dat$Student.Name <- tolower(dat$Student.Name)
dat <- merge(dat,nameToID)[,c(112,2:111)]

write_csv(dat,"FormattedDataDec17.csv")
