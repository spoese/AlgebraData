library(tidyverse)
library(readxl)
dat <- data.frame()
classes <- read.csv("Classes.csv",stringsAsFactors = FALSE)
cNames <- read.csv("cNames.csv",stringsAsFactors = FALSE,header = FALSE)[,1]
initial.list <- list()
for (i in 1:52) {
        print(paste("Reading sheet",i))
        temp <- read_xlsx("Oct30Shiny.xlsx",i,skip=9)
        tempNames <- paste("X",1:dim(temp)[2],sep="")
        initial.list[[i]] <- read_xlsx("Oct30Shiny.xlsx", i,
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
                x <- mutate(x,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA,Assignment9B=NA)
                x <- x[,c(1:35,66,36,37,67,38,39,68,40:50,69,51:65)]
        } else if (unique(x$When == "Late") & unique(x$Days != 1)) {
                x <- mutate(x,Topics8=NA,Assignment3C=NA,Assignment9A=NA,Assignment9B=NA)
                x <- x[,c(1:33,66,34:40,67,41:50,68,69,51:65)]
        } else if (unique(x$Days == 1)) {
                x <- mutate(x,Topics1=NA,Topics8=NA,Assignment1B=NA,Assignment9A=NA,Assignment9B=NA)
                x <- x[,c(1:26,65,27:32,66,33,67,34:49,68,69,50:64)]
        } else {
                x <- mutate(x,Assignment1B=NA,Assignment2C=NA,Assignment3C=NA)
                x <- x[,c(1:35,67,36,37,68,38,39,69,40:66)]
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
dat[,66:69] <- apply(dat[,66:69],2,as.factor)

write_csv(dat,"FormattedData.csv")