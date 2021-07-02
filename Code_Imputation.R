# R script replication file 
# Multiple Imputation procedure
# July 2021

# Packages
library(tidyverse)
library(psych)  
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(devtools)

# Upload Data
library(readxl)
MyData <- read_excel("~/Data2.xlsx")

## Missing data procedures  ##
library(mice)
library(VIM)

# Build study dataset
final.dat <- MyData %>%
  dplyr::select(QB1_2, QB1_3, QB1_6,
                QB2_1, QB2_4, QB2_5, QB2_7, QB2_8, 
                QB3_1, QB3_2, QB3_4, QB3_7, QB3_8,
                QB4_5, QB4_6,
                QC3_2, QC3_10, 
                QC4_1, QC4_2, QC4_3, QC4_4, QC4_5)

# Find the degree of missing observations
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(final.dat,2,pMiss)

# Visual tools for missing data
aggr_plot <- aggr(final.dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(final.dat), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 

# Multiple Imputation procedure (using the mice function)
tempData <- mice(final.dat,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

# Visual tools to compare imputed and original data
densityplot(tempData)

# Build full (completed) dataset (the number represents the imputed dataset used for completing missing values)
comp.dat1 <- complete(tempData, 1)

# Join co-vars to imputed data
d.covars <- MyData %>%
  dplyr::select(exp., education, age_cat, gender)

# Create final full dataset (based on imputed dataset #1)
data_all <- cbind(comp.dat1, d.covars)

## Testing regression model with imputed dataset #1
library(modelsummary)

summary(m3 <- lm(QC4_4 ~ QB2_1 + QB3_2 + QB2_7 + QB2_8 + 
                   QB3_8 + QC3_2 + QB4_6 +
                   age_cat + job_b + gender, data = data_all))


## Build full datasets based on all imputed data (in addition to comp.dat1)
comp.dat2 <- complete(tempData, 2)
comp.dat3 <- complete(tempData, 3)
comp.dat4 <- complete(tempData, 4)
comp.dat5 <- complete(tempData, 5)

# Generate complete dataset (change comp.dat by selected imputed file)
data_all <- cbind(comp.dat1, d.covars)

# Run model 3 for all 5 datasets 
summary(m3 <- lm(QC4_4 ~ QB2_1 + QB3_2 + QB2_7 + QB2_8 + 
                   QB3_8 + QC3_2 + QB4_6 +
                   age_cat + job_b + gender, data = data_all))

# Save model results for each imputed dataset in list (save after running model with each imputed dataset)
tab1 <- list()
tab1[['Dataset #1']] <- m3
tab1[['Dataset #2']] <- m3
tab1[['Dataset #3']] <- m3
tab1[['Dataset #4']] <- m3
tab1[['Dataset #5']] <- m3

### Compare imputed datasets: plot results 
b <- list(geom_vline(xintercept = 0, linetype = "dotted"))
c_lab <- c('QB2_1' = 'Trust:Fed.Govt', 'QB3_2' = 'Regulation', 'QB2_7' = 'Private Ins.',
           'QB2_8' = 'Health Pros', 'QB3_8' = 'Tech Good', 'QC3_2' = 'RPM Use', 'QB4_6' = 'Net Benefits',
           'age_cat' = 'Age', 'job_b' = 'Govt Job', 'gender' = 'Gender')

coef.compare <- modelplot(tab1, coef_omit = 'Interc', background = b, coef_map = c_lab) + 
  labs(
    x = 'Coefficients',
    y = 'Survey Items',
    title = 'Model 3: Government Regulation') 

coef.compare
