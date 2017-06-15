## Family-life courses

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

## Add variables of interest regarding the children section
children <- read.dta("Data/Wave3_SHARELIFE/sharew3_rel6-0-0_rc.dta")
data <- merge(demo, children, by = "mergeid", all.x = FALSE)
data <- select(data, c(mergeid, birth, starts_with("sl_rc024_")))

## Subset for our cohorts born between 1930 and 1960 
data <- subset(data, birth > 1930)
data <- subset(data, birth < 1960)

# Build longitudinal sequences regarding the children section
## Firstly, transform children events in year dates to individual ages

for(i in 1:nrow(data)){
        for(j in 3:ncol(data)){
                if(!is.na(data[i, j])){
                        data[i, j] <- data[i, j] - data$birth[i]
                }
        }
}

life <- data.frame(id = data$mergeid)
life <- cbind(life, as.data.frame(matrix(rep(0), nrow = nrow(life), 
                                           ncol = 51)))
names(life) <- c( "ID", as.character(seq(15, 65)))

## Set ilogical data values to NA
for(i in 3:ncol(data)){
        data[ ,i][data[ ,i] < 15] <- NA
        data[ ,i][data[ ,i] > 65] <- NA
}

## Build references for the first child
for(i in 1:nrow(data)){
        if(!is.na(data[i,3])){
                life[i, data[i, 3] - 13] <- 1
        }
}

## Build references for the category +than 2 children
for(i in 1:nrow(data)){
        for(j in 4:ncol(data)){
                if(!is.na(data[i, j])){
                        life[i, data[i, j] - 13] <- 2
                }
        }
}

## rename sequences

life[life == 0] <- "nc"
life[life == 1] <- "1c"
life[life == 2] <- "2c"

## fill in the table life

i <- 1
j <- 2

while(i < nrow(life)){
        while((j < ncol(life) - 1)){
                if((life[i,j] != "nc") & (life[i, j+1] == "nc")){
                        life[i, j+1] <- life[i, j]
                }
                j <- j +1
        }
        i <- i + 1
        j <- 2
}

rm(i, j)

## Adjustment for the last column of table life
for(i in 1:nrow(life)){
        if(life[, ncol(life)][i] == "nc"){
        life[, ncol(life)][i] <- life[, ncol(life) - 1][i]
        }
}


## Now let's move to the family section (same methodology)
## First of all clean workspace by deleting children and demographic variables already used
rm(demo, children)

## Read family data from stata file
family <- read.dta("Data/Wave3_SHARELIFE/sharew3_rel6-0-0_rp.dta")

## Add variables of interest regarding the family section
data <- merge(data, family, by = "mergeid", all.x = FALSE)
data <- select(data, c(mergeid, birth, starts_with("sl_rp008_"),
                       starts_with("sl_rp011_"),
                       starts_with("sl_rp014_")))

# Build longitudinal sequence regarding the family section
## Firstly, transform family events in year dates to individual ages

for(i in 1:nrow(data)){
        for(j in 3:ncol(data)){
                if(!is.na(data[i, j])){
                        data[i, j] <- data[i, j] - data$birth[i]
                }
        }
}

civil_status <- data.frame(id = data$mergeid)
civil_status <- cbind(civil_status, as.data.frame(matrix(rep(0), nrow = nrow(civil_status), 
                                         ncol = 51)))
names(civil_status) <- c( "ID", as.character(seq(15, 65)))

## Set ilogical data values to NA
for(i in 3:ncol(data)){
        data[ ,i][data[ ,i] < 15] <- NA
        data[ ,i][data[ ,i] > 65] <- NA
}

## Set ilogical data values to NA. we also fix values for events over 65 years
## as they are irrelevant for the analysis
for(i in 3:ncol(data)){
        data[ ,i][data[ ,i] < 15] <- NA
        data[ ,i][data[ ,i] > 65] <- NA
}

## Build references for marriages
for(i in 1:nrow(data)){
        for(j in 3:8){
                if(!is.na(data[i, j])){
                        civil_status[i, data[i, j] - 13] <- 1
                }
        }
}


## Build references for divorces
for(i in 1:nrow(data)){
        for(j in 17:20){
                if(!is.na(data[i, j])){
                        civil_status[i, data[i, j] - 13] <- 2
                }
        }
}


## Build references for widowed individuals
for(i in 1:nrow(data)){
        for(j in 9:16){
                if(!is.na(data[i, j])){
                        civil_status[i, data[i, j] - 13] <- 3
                }
        }
}

## rename sequences

civil_status[civil_status == 0] <- "s"
civil_status[civil_status == 1] <- "m"
civil_status[civil_status == 2] <- "d"
civil_status[civil_status == 3] <- "w"

## fill in the table civil_status

i <- 1
j <- 2

while(i < nrow(civil_status)){
        while((j < ncol(civil_status) - 1)){
                if((civil_status[i,j] != "s") & (civil_status[i, j+1] == "s")){
                        civil_status[i, j+1] <- civil_status[i, j]
                }
                j <- j +1
        }
        i <- i + 1
        j <- 2
}

rm(i, j)

## Adjustment for the last column of table civil_status
for(i in 1:nrow(civil_status)){
        if(civil_status[, ncol(civil_status)][i] == "s"){
                civil_status[, ncol(civil_status)][i] <- civil_status[, ncol(civil_status) - 1][i]
        }
}


## Clean workspace by deleting family variables already used
rm(family)

## Paste both civil_status and life tables to get the longitudinal life experience
## chain of characters for each individual

for(i in 1:nrow(life)){
        for(j in 2:ncol(life)){
                life[i, j] <- paste(as.character(civil_status[i, j]), 
                                     as.character(life[i, j]), sep="")
        }
}

## Finally, we write the chain of characters in a csv file to use it later
write.csv(life, file = "long_life_experience.csv")