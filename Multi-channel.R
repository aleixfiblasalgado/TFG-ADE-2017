## Multi-channel sequence Analysis

rm(list = ls())

setwd("/Users/aleixfiblasalgado/EUS/TFG/Data_analysis")

## Load required libraries
library(TraMineR)
library(foreign)
library(WeightedCluster)
library(dplyr)
library(RColorBrewer)
library(factoextra)
library(NbClust)
library(ggplot2)

## Load character string sequences
load(file = "fw_sequences.RData")

## Create sequence objects for both dimensions
life_seq <- seqdef(life, 2:ncol(life), cpal = brewer.pal(12, "Set3"))
work_seq <- seqdef(work, 2:ncol(work), cpal = brewer.pal(3, "Pastel1"))
agglife <- wcAggregateCases(life[, 2:ncol(life)])
aggwork <- wcAggregateCases(work[, 2:ncol(work)])
uniquelife <- life[agglife$aggIndex, 2:ncol(life)]
uniquework <- work[aggwork$aggIndex, 2:ncol(life)]
life_seq_unique <- seqdef(uniquelife, weights = agglife$aggWeights, cpal = brewer.pal(12, "Set3"))
work_seq_unique <- seqdef(uniquework, weights = aggwork$aggWeights, cpal = brewer.pal(3, "Pastel1"))
diss_life <- seqdist(life_seq_unique, method = "DHD")
diss_work <- seqdist(work_seq_unique, method = "DHD")

## Multichannel distances
mcdiss <- seqdistmc(list(life_seq, work_seq), method = "DHD", full.matrix = TRUE)

## Aglomerative hierarchical clustering
## First of all we employ different methods to select the optimal number of clusters
graphics.off()
averageClust <- hclust(as.dist(mcdiss), method = "average")
averageClust1 <- hclust(as.dist(diss_life), method = "average", members = agglife$aggWeights)
averageClust2 <- hclust(as.dist(diss_work), method = "average", members = aggwork$aggWeights)
avgClustQual <- as.clustrange(averageClust, mcdiss, ncluster = 10)
avgClustQual1 <- as.clustrange(averageClust1, diss_life, weights = agglife$aggWeights, ncluster = 10)
avgClustQual2 <- as.clustrange(averageClust2, diss_work, weights = aggwork$aggWeights, ncluster = 10)
png(file = "/Users/aleixfiblasalgado/EUS/TFG/Data_analysis/Graphs/F-w_opt_clusters.png", width = 560, height = 474, res = 99)
plot(avgClustQual, norm = "zscore", withlegend = F)
dev.off()
png(file = "/Users/aleixfiblasalgado/EUS/TFG/Data_analysis/Graphs/Family_opt_clusters.png", width = 560, height = 474, res = 99)
plot(avgClustQual1, norm = "zscore", withlegend = F)
dev.off()
png(file = "/Users/aleixfiblasalgado/EUS/TFG/Data_analysis/Graphs/Work_opt_clusters.png", width = 560, height = 474, res = 99)
plot(avgClustQual2, norm = "zscore", withlegend = F)
dev.off()
summary(avgClustQual, max.rank = 2)
summary(avgClustQual1, max.rank = 2)
summary(avgClustQual2, max.rank = 2)

## In view of the graph it seems that 8 is our optimal number of clusters
## Alternatively, we conduct PAM clustering
pamClustRange <- wcKMedRange(mcdiss, kvals = 2:10)

## Plot the clusters
par(mar=c(1,1,1,1))
clusterward <- agnes(mcdiss, diss = T, method = "ward")
family_work <- cutree(clusterward, k = 8)
labs <- factor(family_work, labels = paste("Cluster", 1:8))
pdf(file = "/Users/aleixfiblasalgado/EUS/TFG/Data_analysis/Graphs/Family_clusters.pdf", 
    width = 25, height = 40)
seqdplot(life_seq, group = labs, border = NA, withlegend = F)
dev.off()
pdf(file = "/Users/aleixfiblasalgado/EUS/TFG/Data_analysis/Graphs/Work_clusters.pdf", 
    width = 25, height = 40)
seqdplot(work_seq, group = labs, border = NA, withlegend = F)
dev.off()

rm(diss_life, diss_work, life_seq, life_seq_unique, mcdiss, uniquelife,
   uniquework, work_seq, work_seq_unique, agglife, aggwork, averageClust, averageClust1,
   averageClust2, avgClustQual, avgClustQual1, avgClustQual2, clusterward, pamClustRange)

## Descriptive multichannel results
## Build a dataframe with individual belongings to group
descriptive <- data.frame(mergeid = life$ID, cluster = family_work)

## Add gender characteristics
demo <- read.dta("Data/Wave3_SHARELIFE/sharew3_rel6-0-0_st.dta")
descriptive <- merge(descriptive, demo, by = "mergeid", all.x = F)
descriptive <- select(descriptive, c(1, 2, 12))
rm(demo)
descriptive <- rename(descriptive, gender = sl_st011_)
for(i in 1:nrow(descriptive)){
        if(substr(descriptive$mergeid[i], start=1, stop=2) == "ES"){
                descriptive$nationality[i] <- "Spanish"
        }
        if(substr(descriptive$mergeid[i], start=1, stop=2) == "NL"){
                descriptive$nationality[i] <- "Dutch"
        }
}

## Compile educational data
education <- read.dta("Data/Wave1/sharew1_rel6-0-0_gv_isced.dta")
education <- select(education, mergeid, isced1997_r)
descriptive <- merge(descriptive, education, by = "mergeid", all.x = T)
rm(education)


## Summarize work trajectories info
olf <- 0
pt <- 0
ft <- 0

for(i in 1:nrow(work)){
        for(j in 2:ncol(work)){
            if(work[i, j] == "olf"){
                    olf <- olf + 1
            }
                if(work[i, j] == "pt"){
                        pt <- pt + 1
                } 
                if(work[i, j] == "ft"){
                        ft <- ft + 1
                }
        }
        descriptive$years_olf[i] <- olf
        descriptive$years_pt[i] <- pt
        descriptive$years_ft[i] <- ft
        olf <- 0
        pt <- 0
        ft <- 0
}
rm(olf, pt, ft)

## Summarize family trajectories info
snc <- 0
s1c <- 0
s2c <- 0
mnc <- 0
m1c <- 0
m2c <- 0
wnc <- 0
w1c <- 0
w2c <- 0
dnc <- 0
d1c <- 0
d2c <- 0

for(i in 1:nrow(life)){
        for(j in 2:ncol(life)){
                if(life[i, j] == "snc"){
                        snc <- snc + 1
                }
                if(life[i, j] == "s1c"){
                        s1c <- s1c + 1
                } 
                if(life[i, j] == "s2c"){
                        s2c <- s2c + 1
                }
                if(life[i, j] == "mnc"){
                        mnc <- mnc + 1
                }
                if(life[i, j] == "m1c"){
                        m1c <- m1c + 1
                }
                if(life[i, j] == "m2c"){
                        m2c <- m2c + 1
                }
                if(life[i, j] == "wnc"){
                        wnc <- wnc + 1
                }
                if(life[i, j] == "w1c"){
                        w1c <- w1c + 1
                }
                if(life[i, j] == "w2c"){
                        w2c <- w2c + 1
                }
                if(life[i, j] == "dnc"){
                        dnc <- dnc + 1
                }
                if(life[i, j] == "d1c"){
                        d1c <- d1c + 1
                }
                if(life[i, j] == "d2c"){
                        d2c <- d2c + 1
                }
        }
        descriptive$years_snc[i] <- snc
        descriptive$years_s1c[i] <- s1c
        descriptive$years_s2c[i] <- s2c
        descriptive$years_mnc[i] <- mnc
        descriptive$years_m1c[i] <- m1c
        descriptive$years_m2c[i] <- m2c
        descriptive$years_wnc[i] <- wnc
        descriptive$years_w1c[i] <- w1c
        descriptive$years_w2c[i] <- w2c
        descriptive$years_dnc[i] <- dnc
        descriptive$years_d1c[i] <- d1c
        descriptive$years_d2c[i] <- d2c
        snc <- 0
        s1c <- 0
        s2c <- 0
        mnc <- 0
        m1c <- 0
        m2c <- 0
        wnc <- 0
        w1c <- 0
        w2c <- 0
        dnc <- 0
        d1c <- 0
        d2c <- 0
}
rm(snc, s1c, s2c, mnc, m1c, m2c, wnc, w1c, w2c, dnc, d1c, d2c)

## GROUP BY
descriptive <- group_by(descriptive, cluster)
analysis <- summarize(descriptive, N = length(mergeid),
          Men = sum(gender == "Male")/N, 
          Women = sum(gender == "Female")/N,
          Spanish = sum(nationality == "Spanish")/N,
          Dutch = sum(nationality == "Dutch")/N,
          Spanish2 = sum(nationality == "Spanish"),
          Dutch2 = sum(nationality == "Dutch"),
          Isced_code1 = sum(isced1997_r == "ISCED-97 code 1", na.rm = T),
          Isced_code2 = sum(isced1997_r == "ISCED-97 code 2", na.rm = T),
          Isced_code3 = sum(isced1997_r == "ISCED-97 code 3", na.rm = T),
          Isced_code4 = sum(isced1997_r == "ISCED-97 code 4", na.rm = T),
          Isced_code5 = sum(isced1997_r == "ISCED-97 code 5", na.rm = T),
          Isced_code6 = sum(isced1997_r == "ISCED-97 code 6", na.rm = T),
          Still_school = sum(isced1997_r == "Still in school", na.rm = T),
          Other = sum(isced1997_r == "Other", na.rm = T),
          avg_years_olf = mean(years_olf),
          avg_years_pt = mean(years_pt),
          avg_years_ft = mean(years_ft),
          avg_years_snc = mean(years_snc),
          avg_years_s1c = mean(years_s1c),
          avg_years_s2c = mean(years_s2c),
          avg_years_mnc = mean(years_mnc),
          avg_years_m1c = mean(years_m1c),
          avg_years_m2c = mean(years_m2c),
          avg_years_wnc = mean(years_wnc),
          avg_years_w1c = mean(years_w1c),
          avg_years_w2c = mean(years_w2c),
          avg_years_dnc = mean(years_dnc),
          avg_years_d1c = mean(years_d1c),
          avg_years_d2c = mean(years_d2c))

write.table(analysis, file = "descriptive analysis.csv")

