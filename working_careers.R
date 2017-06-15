## Work-trajectories

rm(list = ls())

setwd("/Users/aleixfiblasalgado/EUS/TFG/Data_analysis")

## Load required libraries
library(TraMineR)
library(foreign)
library(cluster)
library(dplyr)

# Load stata data and clean it to get only useful variables
## Import demographic variables to get the birth of individuals subsetting for 
## Spain and the Netherlands
demo <- read.dta("Data/Wave3_SHARELIFE/sharew3_rel6-0-0_st.dta")
demo <- select(subset(demo, country == "Spain" | country == "Netherlands"), 
               c(mergeid, sl_st007_))
names(demo)[2] <- "birth"

## Add variables of interest regarding the work history section
employment <- read.dta("Data//Wave3_SHARELIFE/sharew3_rel6-0-0_re.dta")
data <- merge(demo, employment, by = "mergeid", all.x = FALSE)
data <- select(data, c(mergeid, birth, sl_re003_, starts_with("sl_re011_"),
                       starts_with("sl_re016_"), starts_with("sl_re018_"),
                       starts_with("sl_re020_"), starts_with("sl_re026_")))

## Subset for our cohorts born between 1930 and 1960 
data <- subset(data, birth > 1930)
data <- subset(data, birth < 1960)

# Build longitudinal sequences regarding the work history
## In this case as employment trajectories date from many years ago, some individuals
## started working at an age before 15 (Spanish postwar). Therefore we initially extend
## the years analysed from 5 to 65

for(i in 1:nrow(data)){
        for(j in 4:23){
                if(!is.na(data[i, j])){
                        data[i, j] <- data[i, j] - data$birth[i]
                }
        }
}


for(i in 1:nrow(data)){
        for(j in 44:ncol(data)){
                if(!is.na(data[i, j])){
                        data[i, j] <- data[i, j] - data$birth[i]
                }
        }
}

work <- data.frame(id = data$mergeid)
work <- cbind(work, as.data.frame(matrix(rep(0), nrow = nrow(work), 
                                         ncol = 61)))
names(work) <- c( "ID", as.character(seq(5, 65)))

## Set ilogical data values to NA

for(i in 4:23){
        data[ ,i][data[ ,i] < 5] <- NA
        data[ ,i][data[ ,i] > 65] <- NA
}

for(i in 44:ncol(data)){
        data[ ,i][data[ ,i] < 5] <- NA
        data[ ,i][data[ ,i] > 65] <- NA
}

## Build references for the age started working and its type

for(i in 1:nrow(data)){
        for(j in 4:23){
                if((!is.na(data[i, j])) & ((as.numeric(data[i, j+20]) == 3) | (as.numeric(data[i, j+20]) == 5))){
                        work[i, data[i, j]-3] <- "ft"
                }
                if((!is.na(data[i, j])) & ((as.numeric(data[i, j+20]) == 4) | (as.numeric(data[i, j+20]) == 6))){
                        work[i, data[i, j]-3] <- "pt"
                }
                if((!is.na(data[i, j])) & (as.numeric(data[i, j+20]) == 7)){
                        work[i, data[i, j]-3] <- sample(c("ft", "pt"), size = 1, replace = F)
                }
        }
}


## Build references for years changing from ft to pt and from pt to ft

for(i in 1:nrow(data)){
        for(j in 44:60){
                if(!is.na(data[i, j])){
                        work[i, data[i, j]-3] <- "ctpt"
                }
        }
}

for(i in 1:nrow(data)){
        for(j in 61:76){
                if(!is.na(data[i, j])){
                        work[i, data[i, j]-3] <- "ctft"
                }
        }
}

## Build references for years employment relation stopped

for(i in 1:nrow(data)){
        for(j in 77:ncol(data)){
                if((!is.na(data[i,j])) & (!is.na(data[i, j-73]))){
                        if((data[i, j] > data[i, j-73])){
                                if(!is.na(data[i, j-72])){
                                        if(data[i, j] < data[i, j-72]){
                                                work[i, data[i,j]-3] <- "st" 
                                        }
                                }
                                if(is.na(data[i, j-72])){
                                        work[i, data[i,j]-3] <- "st"
                                }   
                        }
                }
        }
}

## fill in the table work

i <- 1
j <- 2

while(i < nrow(work)){
        while(j < ncol(work) -1){
                if((work[i, j] == "ft" | work[i, j] == "pt") & (work[i,j+1] == 0)){
                        work[i, j+1] <- work[i, j]
                }
                j <- j + 1
        }
        i <- i + 1
        j <- 2
}

## loop for changes from pt to ft or vicecersa

for(i in 1:nrow(work)){
        for(j in 2:ncol(work)){
                if(work[i, j] == "ctpt"){
                        work[i, j] <- "pt"
                }
                if(work[i, j] == "ctft"){
                        work[i, j] <- "ft"
                }
        }
}

while(i < nrow(work)){
        while(j < ncol(work) -1){
                if((work[i, j] == "ft" | work[i, j] == "pt") & (work[i,j+1] == 0)){
                        work[i, j+1] <- work[i, j]
                }
                j <- j + 1
        }
        i <- i + 1
        j <- 2
}

## set values for years stopped working

for(i in 1:nrow(work)){
        for(j in 2:ncol(work)){
                if(work[i, j] == "st"){
                        work[i, j] <- work[i, j-1]
                }
        }
}

## rename observations corresponding to "out of the labor force"
work[work == 0] <- "olf"

## subset for sequences from 15 to 65 years

work <- select(work, c(1, 12:ncol(work)))

## Finally, we write the chain of characters in a csv file to use it later
write.csv(work, file = "employment_trajectories.csv")

