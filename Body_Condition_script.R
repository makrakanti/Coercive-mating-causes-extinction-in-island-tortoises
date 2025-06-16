library(tidyverse)
library(writexl)
library(nlme)


model_listMbci <- list(
  mYear = lme(as.numeric(BCI)~as.factor(Year), random=~1|as.factor(Individual), data=datam),
  mSex = lme(as.numeric(BCI)~as.factor(Sex), random=~1|as.factor(Individual), data=datam),
  mSeason = lme(as.numeric(BCI)~as.factor(Season), random=~1|as.factor(Individual), data=datam),
  mLocality = lme(as.numeric(BCI)~as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocality_Season_Sex = lme(as.numeric(BCI)~as.factor(Season)+as.factor(Sex)+as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocality_Year_Sex = lme(as.numeric(BCI)~as.factor(Year)+as.factor(Sex)+as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocality_Sex = lme(as.numeric(BCI)~as.factor(Sex)+as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocality_Season = lme(as.numeric(BCI)~as.factor(Season)+as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocality_Year = lme(as.numeric(BCI)~as.factor(Sex)+as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mSex_Season = lme(as.numeric(BCI)~as.factor(Season)+as.factor(Sex), random=~1|as.factor(Individual), data=datam),
  mSex_Year = lme(as.numeric(BCI)~as.factor(Sex)+as.factor(Sex), random=~1|as.factor(Individual), data=datam),
  mSeason_Sex = lme(as.numeric(BCI)~as.factor(Sex)+as.factor(Season), random=~1|as.factor(Individual), data=datam),
  mSeason_Year = lme(as.numeric(BCI)~as.factor(Sex)+as.factor(Season), random=~1|as.factor(Individual), data=datam),
  mYear_Sex = lme(as.numeric(BCI)~as.factor(Sex)+as.factor(Year), random=~1|as.factor(Individual), data=datam),
  mYear_Season = lme(as.numeric(BCI)~as.factor(Season)+as.factor(Year), random=~1|as.factor(Individual), data=datam),
  mLocalityXSeasonXSex = lme(as.numeric(BCI)~as.factor(Season)*as.factor(Sex)*as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocalityXSex = lme(as.numeric(BCI)~as.factor(Sex)*as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocalityXSeason = lme(as.numeric(BCI)~as.factor(Season)*as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mLocalityXYear = lme(as.numeric(BCI)~as.factor(Sex)*as.factor(Locality), random=~1|as.factor(Individual), data=datam),
  mSexXSeason = lme(as.numeric(BCI)~as.factor(Season)*as.factor(Sex), random=~1|as.factor(Individual), data=datam),
  mSexXYear = lme(as.numeric(BCI)~as.factor(Sex)*as.factor(Sex), random=~1|as.factor(Individual), data=datam),
  mSeasonXSex = lme(as.numeric(BCI)~as.factor(Sex)*as.factor(Season), random=~1|as.factor(Individual), data=datam),
  mSeasonXYear = lme(as.numeric(BCI)~as.factor(Sex)*as.factor(Season), random=~1|as.factor(Individual), data=datam),
  mYearXSex = lme(as.numeric(BCI)~as.factor(Sex)*as.factor(Year), random=~1|as.factor(Individual), data=datam)
)



aic_valuesbci <- sapply(model_listMbci, AIC)


# Create a data frame with model names and AIC values
BCIults_dfbci <- data.frame(
  Model = names(model_listMbci),
  AIC = aic_valuesbci)
