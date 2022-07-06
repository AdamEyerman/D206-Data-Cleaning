#Install packages needed for data cleaning
install.packages(tidyverse)
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("modeest")

#Load libraries from packages
library(ggplot2)  #Data visualizations 
library(readr)  #Read text data into R
library(tidyr)  #Creates tidy data
library(dplyr)  #Data manipulation
library(factoextra)  #Multivariate visualizations
library(FactoMineR)  #Principal Component Analysis
library(modeest)  #Enables mode imputation for categorical values

# Load data set into R
Churn_Raw_Data <- read.csv('D206_Churn_Raw_Data.csv')

#View data and structure
View(Churn_Raw_Data)
str(Churn_Raw_Data)

#Determine unique values for Customer_id
require(dplyr)
n_distinct(Churn_Raw_Data$Customer_id)

#Find columns with NULL values
colSums(is.na(churn_data_clean))

#Rename columns 'item1' through 'item8' to detailed name
churn_data_clean <- Churn_Raw_Data %>% rename(Timely_response = item1, Timely_fixes = item2, Timely_replacements = item3, Reliability = item4, Options = item5, Respectful_response = item6, Courteous_exchange = item7, Active_listening = item8)

#Delete columns that are redundant or irrelevant to analysis
churn_data_clean <- churn_data_clean[,c(2:8,11:52)]

#Impute missing or NULL values using mean, median or mode
churn_data_clean$Children[is.na(churn_data_clean$Children)] <- median(churn_data_clean$Children, na.rm=TRUE)
churn_data_clean$Age[is.na(churn_data_clean$Age)] <- median(churn_data_clean$Age, na.rm=TRUE)
churn_data_clean$Income[is.na(churn_data_clean$Income)] <- mean(churn_data_clean$Income, na.rm=TRUE)
churn_data_clean$Tenure[is.na(churn_data_clean$Tenure)] <- mean(churn_data_clean$Tenure, na.rm=TRUE)
churn_data_clean$Bandwidth_GB_Year[is.na(churn_data_clean$Bandwidth_GB_Year)] <- mean(churn_data_clean$Bandwidth_GB_Year, na.rm=TRUE)
churn_data_clean$Techie[is.na(churn_data_clean$Techie)] <- mfv(churn_data_clean$Techie, na.rm=TRUE)
churn_data_clean$Phone[is.na(churn_data_clean$Phone)] <- mfv(churn_data_clean$Phone, na.rm=TRUE)
churn_data_clean$TechSupport[is.na(churn_data_clean$TechSupport)] <- mfv(churn_data_clean$TechSupport, na.rm=TRUE)

#Change negative values in Outage_sec_perweek column to positive
churn_data_clean$Outage_sec_perweek <- abs(churn_data_clean$Outage_sec_perweek)

#Visualize for outliers in Outage_sec_perweek and Yearly_equip_failure columns as they are viewed as most likely contributors to churn
boxplot.default(churn_data_clean$Outage_sec_perweek)
barplot.default(churn_data_clean$Yearly_equip_failure)
boxplot.default(churn_data_clean$Email)

#PCA of selected numeric variables
churn.pca <- prcomp(churn_data_clean[,c(20:23,39:40,42:49)], center=TRUE, scale=)

summary(churn.pca)

churn.pca$rotation

#Calculate total variance with each principal component
var_by_component <- churn.pca$sdev^2 / sum(churn.pca$sdev^2)

#Display Scree Plot for PCA visualization
require(ggplot2)
qplot(c(1:14), var_by_component) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#Display Eigenvalue visualization
require(factoextra)
fviz_eig(churn.pca, choice = "eigenvalue", addlabels=TRUE, scale. = FALSE)

#Display total variance with each principal component
churn.pca$sdev^2 / sum(churn.pca$sdev^2)
