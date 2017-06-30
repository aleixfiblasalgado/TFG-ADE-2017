# Regression Analysis

setwd("/Users/aleixfiblasalgado/EUS/TFG/Data_analysis")
rm(list = ls())

## load required packages
library(foreign)
library(dplyr)



## load workspace image
load("regression_analysis.RData")

## Collect information about financial well-being

earnings_w1 <- read.dta("Data/Wave1/sharew1_rel6-0-0_gv_imputations.dta")
earnings_w2 <- read.dta("Data/Wave2/sharew2_rel6-0-0_gv_imputations.dta")
earnings_w5 <- read.dta("Data/Wave5/sharew5_rel6-0-0_gv_imputations.dta")
earnings_w1 <- subset(earnings_w1, startsWith(earnings_w1$mergeid, "NL") | 
                              startsWith(earnings_w1$mergeid, "ES"))
earnings_w2 <- subset(earnings_w2, startsWith(earnings_w2$mergeid, "NL") | 
                              startsWith(earnings_w2$mergeid, "ES"))
earnings_w5 <- subset(earnings_w5, startsWith(earnings_w5$mergeid, "NL") | 
                              startsWith(earnings_w5$mergeid, "ES"))

financial_w1 <- select(earnings_w1, c(mergeid, starts_with("ypen"),starts_with("yreg"),
                                     aftrec, aftinh, home, ores, yaohm, ybabsmf,
                                     hrass, starts_with("yedu"), sbus, car))
financial_w2 <- select(earnings_w2, c(mergeid, starts_with("ypen"),starts_with("yreg"),
                                      aftrec, aftinh, home, ores, yaohm, hgfass, liab,
                                      hrass, starts_with("yedu"), sbus, car))
financial_w5 <- select(earnings_w5, c(mergeid, politics, ysrent,
                                      starts_with("ypen"),starts_with("yreg"), 
                                      home, ores, yaohm, hrass, starts_with("yedu"), 
                                      sbus, car))
financial_w2 <- mutate(financial_w2, Hnfass = hgfass -liab)
financial_w2 <- select(financial_w2, -c(hgfass, liab))

rm(earnings_w1, earnings_w2, earnings_w5)

## Due to differences in data composition is possible that variables from different
## waves are integrated in our main dataset

spain <- filter(descriptive, nationality == "Spanish")
netherlands <- filter(descriptive, nationality == "Dutch")
rm(descriptive)


## Pensions

spain <- merge(spain, financial_w1, by = "mergeid", all.x = T)
spain <- distinct(spain, mergeid, .keep_all = T)

for(i in 1:nrow(spain)){
        spain$public[i] <- sum(financial_w1$ypen1[i], 
                               financial_w1$ypen3[i], 
                               financial_w1$ypen6[i], na.rm = T)
        spain$private[i] = sum(financial_w1$yreg1[i], na.rm = T)
        spain$alimony[i] = sum(financial_w1$yreg2[i], na.rm = T)
}

spain <- select(spain, -c(ypen1, ypen3, ypen6, yreg1, yreg2))

ES <- read.dta("Data/Wave2/sharew2_rel6-0-0_ep.dta")
ES <- subset(ES, startsWith(ES$mergeid, "ES"))
ES <- select(ES, c(mergeid, starts_with("ep078")))
ES <- select(ES, c(mergeid, ep078e_12, ep078e_13, ep078e_14))

for(i in 1:nrow(ES)){
        if((!is.na(ES$ep078e_12[i])) & (ES$ep078e_12[i] < 0)){
                ES$ep078e_12[i] <- 0
        }
        if((!is.na(ES$ep078e_13[i])) & (ES$ep078e_13[i] < 0)){
                ES$ep078e_13[i] <- 0
        }
        if((!is.na(ES$ep078e_14[i])) & (ES$ep078e_14[i] < 0)){
                ES$ep078e_14[i] <- 0
        }
        if(!is.na(ES$ep078e_14[i])){
                ES$ep078e_14[i] <- ES$ep078e_14[i]*12
        }
}

spain <- merge(spain, ES, by = "mergeid", all.x = T)
ES <- select(financial_w2, c(mergeid,ypen2))
ES <- rename(ES, ypen2_w2 = ypen2)
spain <- merge(spain, ES, by = "mergeid", all.x = T)

for(i in 1:nrow(spain)){
        spain$occupational[i] <- sum(spain$ypen2[i], 
                                     spain$ypen2_w2[i], 
                                     spain$ep078e_12[i], 
                                     spain$ep078e_13[i],
                                     spain$ep078e_14[i],na.rm = T)
}

spain <- distinct(spain, mergeid, .keep_all = T)
spain <- select(spain, -c(ypen2, ypen2_w2, ep078e_12,
                          ep078e_13, ep078e_14))




netherlands <- merge(netherlands, financial_w1, by = "mergeid", all.x = T)
netherlands <- distinct(netherlands, mergeid, .keep_all = T)

for(i in 1:nrow(netherlands)){
        netherlands$public[i] <- sum(financial_w1$ypen1[i], 
                               financial_w1$ypen3[i], 
                               financial_w1$ypen5[i], na.rm = T)
        netherlands$private[i] = sum(financial_w1$yreg1[i], na.rm = T)
        netherlands$alimony[i] = sum(financial_w1$yreg2[i], na.rm = T)
}


netherlands <- select(netherlands, -c(ypen1, ypen3, ypen5, yreg1, yreg2))

NL<- read.dta("Data/Wave2/sharew2_rel6-0-0_ep.dta")
NL <- subset(NL, startsWith(NL$mergeid, "NL"))
NL <- select(NL, c(mergeid, ep078e_12, ep078e_13, ep078e_14))
for(i in 1:nrow(NL)){
        if((!is.na(NL$ep078e_12[i])) & (NL$ep078e_12[i] < 0)){
                NL$ep078e_12[i] <- 0
        }
        if((!is.na(NL$ep078e_13[i])) & (NL$ep078e_13[i] < 0)){
                NL$ep078e_13[i] <- 0
        }
        if((!is.na(NL$ep078e_14[i])) & (NL$ep078e_14[i] < 0)){
                NL$ep078e_14[i] <- 0
        }
        if(!is.na(NL$ep078e_14[i])){
                NL$ep078e_14[i] <- NL$ep078e_14[i]*12
        }
}
netherlands <- merge(netherlands, NL, by = "mergeid", all.x = T)
NL <- select(financial_w2, c(mergeid,ypen2))
NL <- rename(NL, ypen2_w2 = ypen2)
netherlands <- merge(netherlands, NL, by = "mergeid", all.x = T)

for(i in 1:nrow(netherlands)){
        netherlands$occupational[i] <- sum(netherlands$ypen2[i], 
                                     netherlands$ypen2_w2[i], 
                                     netherlands$ep078e_12[i], 
                                     netherlands$ep078e_13[i],
                                     netherlands$ep078e_14[i],na.rm = T)
}

netherlands <- distinct(netherlands, mergeid, .keep_all = T)
netherlands <- select(netherlands, -c(ypen2, ypen2_w2, ep078e_12,
                                      ep078e_13, ep078e_14))

## eliminamos variables no necesarias

spain<- select(spain, -ends_with("_f"))
netherlands <- select(netherlands, -ends_with("_f"))


## corregimos rangos

for(i in 1:nrow(spain)){
        if((!is.na(spain$yedu_p[i])) & (spain$yedu_p[i] < 0)){
                spain$yedu_p[i] <- 0
        }
}

for(i in 1:nrow(netherlands)){
        if((!is.na(netherlands$yedu_p[i])) & (netherlands$yedu_p[i] < 0)){
                netherlands$yedu_p[i] <- 0
        }
}

## Añadimos las variables que nos falten

Hnfass <- select(financial_w2, c(mergeid, Hnfass))
politics <- select(financial_w5, c(mergeid, politics))
politics$politics[politics$politics < 0] <- 0
ysrent <- select(financial_w5, c(mergeid, ysrent))

spain <- merge(spain, Hnfass, by = "mergeid", all.x = T)
spain <- merge(spain, politics, by = "mergeid", all.x = T)
spain <- merge(spain, ysrent, by = "mergeid", all.x = T)
spain <- distinct(spain, mergeid, .keep_all = T)

netherlands <- merge(netherlands, Hnfass, by = "mergeid", all.x = T)
netherlands <- merge(netherlands, politics, by = "mergeid", all.x = T)
netherlands <- merge(netherlands, ysrent, by = "mergeid", all.x = T)
netherlands <- distinct(netherlands, mergeid, .keep_all = T)

rm(Hnfass, politics, ysrent, financial_w1, financial_w2, financial_w5, NL, ES)

save.image("regression_data.RData")


## Run regressions 

for(i in 1:nrow(spain)){
        if(spain$cluster[i] == 1){
                spain$clust1[i] <- 1
        } else{
                spain$clust1[i] <- 0
        }
        if(spain$cluster[i] == 2){
                spain$clust2[i] <- 1
        } else{
                spain$clust2[i] <- 0
        }
        if(spain$cluster[i] == 3){
                spain$clust3[i] <- 1
        } else{
                spain$clust3[i] <- 0
        }
        if(spain$cluster[i] == 4){
                spain$clust4[i] <- 1
        } else{
                spain$clust4[i] <- 0
        }
        if(spain$cluster[i] == 5){
                spain$clust6[i] <- 1
        } else{
                spain$clust6[i] <- 0
        }
        if(spain$cluster[i] == 6){
                spain$clust5[i] <- 1
        } else{
                spain$clust5[i] <- 0
        }
        if(spain$cluster[i] == 7){
                spain$clust7[i] <- 1
        } else{
                spain$clust7[i] <- 0
        }
        if(spain$cluster[i] == 8){
                spain$clust8[i] <- 1
        } else{
                spain$clust8[i] <- 0
        }
        if(spain$gender[i] == "Male"){
                spain$male[i] <- 1
        } else{
                spain$male[i] <- 0
        }
        if(spain$gender[i] == "Female"){
                spain$female[i] <- 1
        } else{
                spain$female[i] <- 0
        }
        
}

for(i in 1:nrow(netherlands)){
        if(netherlands$cluster[i] == 1){
                netherlands$clust1[i] <- 1
        } else{
                netherlands$clust1[i] <- 0
        }
        if(netherlands$cluster[i] == 2){
                netherlands$clust2[i] <- 1
        } else{
                netherlands$clust2[i] <- 0
        }
        if(netherlands$cluster[i] == 3){
                netherlands$clust3[i] <- 1
        } else{
                netherlands$clust3[i] <- 0
        }
        if(netherlands$cluster[i] == 4){
                netherlands$clust4[i] <- 1
        } else{
                netherlands$clust4[i] <- 0
        }
        if(netherlands$cluster[i] == 5){
                netherlands$clust6[i] <- 1
        } else{
                netherlands$clust6[i] <- 0
        }
        if(netherlands$cluster[i] == 6){
                netherlands$clust5[i] <- 1
        } else{
                netherlands$clust5[i] <- 0
        }
        if(netherlands$cluster[i] == 7){
                netherlands$clust7[i] <- 1
        } else{
                netherlands$clust7[i] <- 0
        }
        if(netherlands$cluster[i] == 8){
                netherlands$clust8[i] <- 1
        } else{
                netherlands$clust8[i] <- 0
        }
        if(netherlands$gender[i] == "Male"){
                netherlands$male[i] <- 1
        } else{
                netherlands$male[i] <- 0
        }
        if(netherlands$gender[i] == "Female"){
                netherlands$female[i] <- 1
        } else{
                netherlands$female[i] <- 0
        }
}


attach(netherlands)
attach(spain)
library(car)



## set category base for the dummy trap
spain$male_base <- 0
spain$clust2_base <- 0
netherlands$male_base <- 0
netherlands$clust2_base <- 0


totpension_es <- lm((public+private+occupational) ~
                         (female + male_base
                          + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                          + politics + yedu + sbus + yedu_p),
                 data = na.omit(spain))
summary(totpension_es)

totpension_nl <- lm((public+private+occupational) ~
                            (female + male_base
                             + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                             + politics + yedu + sbus + yedu_p),
                    data = na.omit(netherlands))
summary(totpension_nl)

public_es <- lm((public) ~
                            (female + male_base
                             + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                             + politics + yedu + sbus + yedu_p),
                    data = na.omit(spain))
summary(public_es)

public_nl <- lm((public) ~
                        (female + male_base
                         + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                         + politics + yedu + sbus + yedu_p),
                data = na.omit(netherlands))
summary(public_nl)

oc_es <- lm((occupational) ~
                    (female + male_base
                     + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                     + politics + yedu + sbus + yedu_p),
            data = na.omit(spain))
summary(oc_es)

oc_nl <- lm((occupational) ~
                    (female + male_base
                     + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                     + politics + yedu + sbus + yedu_p),
            data = na.omit(netherlands))
summary(oc_nl)

priv_es <- lm((private) ~
                        (female + male_base
                         + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                         + politics + yedu + sbus + yedu_p),
                data = na.omit(spain))
summary(priv_es)

priv_nl <- lm((private) ~
                      (female + male_base
                       + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                       + politics + yedu + sbus + yedu_p),
              data = na.omit(netherlands))
summary(priv_nl)

cap_es <- lm((home+ores+car+aftinh) ~
                  (female + male_base
                   + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                   + politics + yedu + sbus + yedu_p),
          data = na.omit(spain))
summary(cap_es)

cap_nl <- lm((home+ores+car+aftinh) ~
                     (female + male_base
                      + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                      + politics + yedu + sbus + yedu_p),
             data = na.omit(netherlands))
summary(cap_nl)

hh_es <- lm((Hnfass + hrass) ~
                    (female + male_base
                     + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                     + politics + yedu + sbus + yedu_p),
            data = na.omit(spain))
summary(hh_es)

hh_nl <- lm((Hnfass + hrass) ~
                    (female + male_base
                     + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                     + politics + yedu + sbus + yedu_p),
            data = na.omit(netherlands))
summary(hh_nl)

hhinc_es <- lm((public+private+occupational+yaohm) ~
                    (female + male_base
                     + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                     + politics + yedu + sbus + yedu_p),
            data = na.omit(spain))
summary(hhinc_es)

hhinc_nl <- lm((public+private+occupational+yaohm) ~
                       (female + male_base
                        + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                        + politics + yedu + sbus + yedu_p),
               data = na.omit(netherlands))
summary(hhinc_nl)

other_es <- lm((ysrent + ybabsmf) ~
                       (female + male_base
                        + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                        + politics + yedu + sbus + yedu_p),
               data = na.omit(spain))
summary(other_es)

other_nl <- lm((ysrent+ ybabsmf) ~
                       (female + male_base
                        + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                        + politics + yedu + sbus + yedu_p),
               data = na.omit(netherlands))
summary(other_nl)

inter_es <- lm((public+private+occupational) ~
                            (female + male_base
                             + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                             + (clust1*female) + (clust2_base*male_base) + (clust3*female) +
                                (clust4*female) + (clust5*female) + (clust6*female)+
                                     (clust7*female)+ (clust8*female)),
                    data = na.omit(spain))
summary(inter_es)

inter_nl <- lm((public+private+occupational) ~
                       (female + male_base
                        + clust1 + clust2_base + clust3 + clust4 + clust5 + clust6 + clust7 + clust8
                        + (clust1*female) + (clust2_base*male_base) + (clust3*female) +
                                (clust4*female) + (clust5*female) + (clust6*female)+
                                (clust7*female)+ (clust8*female)),
               data = na.omit(netherlands))
summary(inter_nl)









