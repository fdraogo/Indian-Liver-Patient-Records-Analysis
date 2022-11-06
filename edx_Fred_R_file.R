
# All packages should be installed in your computer before running "library"

# Accessing the data from my GitHub.
data_stat <- read.csv("https://raw.githubusercontent.com/fdraogo/Indian-Liver-Patient-Records-Analysis/main/Indian-Liver-Patient-Records-Analysis.csv")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
# Overview of the dataset
summary(data_stat) 
# Gender distribution of patients
table (data_stat$Gender)
# Distribution of patients by gender and liver disease status (positive = 1, negative = 0)
table (data_stat$Gender, data_stat$liver_disease)
# Summary of the patients by gender
ggplot(data_stat, aes(x = Gender)) +
  geom_bar() +
  theme(axis.text.x = element_text(hjust = 1))
# How many people were tested positive? by gender and age group
data_stat <- data_stat %>% 
  mutate(
    Age_group = dplyr::case_when(
      Age <= 15            ~ "0-15",
      Age > 15 & Age <= 45 ~ "16-45",
      Age > 45 & Age <= 65 ~ "46-65",
      Age > 65 & Age <= 85 ~ "66-85",
      Age > 85             ~ "> 85"
    ),
    
    Age_group = factor(
      Age_group,
      level = c("0-15", "16-45","46-65", "66-85","> 85")
    )
  )
# Age distribution of each sub-population in the dataset
ggplot(data_stat, aes(x = Age_group)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Age groups of patients") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Mean age of the patients
mean(data_stat$Age) 

aggregate(x = data_stat$Age,              
          by = list(data_stat$Gender),            
          FUN = mean) 

# Data visualization and identification of possible outlying values
hist(data_stat$Total_Bilirubin,
     xlab = "Levels of total bilirubin",
     main = "Distribution of Total Bilirubin",
     border = "black",
     breaks = sqrt(nrow(data_stat))
) 

hist(data_stat$Direct_Bilirubin,
     xlab = "Levels of direct bilirubin",
     main = "Distribution of Direct Bilirubin",
     border = "black",
     breaks = sqrt(nrow(data_stat))
) 

hist(data_stat$Alkaline_Phosphotase,
     xlab = "Level of alkaline phosphotase",
     main = "Distribution of alkaline phosphotase",
     border = "black",
     breaks = sqrt(nrow(data_stat))
) 

hist(data_stat$Alamine_Aminotransferase,
     xlab = "Levels of alamine aminotransferase",
     main = "Distribution of alamine aminotransferase",
     border = "black",
     breaks = sqrt(nrow(data_stat))
) 

hist(data_stat$Aspartate_Aminotransferase,
     xlab = "Levels of aspartate aminotransferase",
     main = "Distribution of aspartate aminotransferase",
     border = "black",
     breaks = sqrt(nrow(data_stat))
) 

# Package for formal test of outliers
library(outliers)

test_Total_bilirubin  <- grubbs.test(data_stat$Total_Bilirubin)
test_Direct_bilirubin  <- grubbs.test(data_stat$Direct_Bilirubin)
test_Alkaline_phosphotase  <- grubbs.test(data_stat$Alkaline_Phosphotase)
test_Alamine_aminotransferase  <- grubbs.test(data_stat$Alamine_Aminotransferase)
test_Aspartate_aminotransferase <- grubbs.test(data_stat$Aspartate_Aminotransferase)


test_Total_bilirubin  
test_Direct_bilirubin  
test_Alkaline_phosphotase  
test_Alamine_aminotransferase  
test_Aspartate_aminotransferase

# Cleaning the data and removing outliers and observations with missing values
Q1 <- quantile(data_stat$Total_Bilirubin, probs=c(.25, .75), na.rm = FALSE)
Q2 <- quantile(data_stat$Direct_Bilirubin, probs=c(.25, .75), na.rm = FALSE)
Q3 <- quantile(data_stat$Alkaline_Phosphotase, probs=c(.25, .75), na.rm = FALSE)
Q4 <- quantile(data_stat$Alamine_Aminotransferase, probs=c(.25, .75), na.rm = FALSE)
Q5 <- quantile(data_stat$Aspartate_Aminotransferase, probs=c(.25, .75), na.rm = FALSE)


iqr1 <- IQR(data_stat$Total_Bilirubin)
iqr2 <- IQR(data_stat$Direct_Bilirubin)
iqr3 <- IQR(data_stat$Alkaline_Phosphotase)
iqr4 <- IQR(data_stat$Alamine_Aminotransferase)
iqr5 <- IQR(data_stat$Aspartate_Aminotransferase)


eliminated1<- subset(data_stat, data_stat$Total_Bilirubin > (Q1[1] - 1.5*iqr1) & data_stat$Total_Bilirubin < (Q1[2]+1.5*iqr1))
eliminated2<- subset(data_stat, data_stat$Direct_Bilirubin > (Q2[1] - 1.5*iqr2) & data_stat$Direct_Bilirubin < (Q2[2]+1.5*iqr2))
eliminated3<- subset(data_stat, data_stat$Alkaline_Phosphotase > (Q3[1] - 1.5*iqr3) & data_stat$Alkaline_Phosphotase < (Q3[2]+1.5*iqr3))
eliminated4<- subset(data_stat, data_stat$Alamine_Aminotransferase > (Q4[1] - 1.5*iqr4) & data_stat$Alamine_Aminotransferase < (Q4[2]+1.5*iqr4))
eliminated5<- subset(data_stat, data_stat$Aspartate_Aminotransferase > (Q5[1] - 1.5*iqr5) & data_stat$Aspartate_Aminotransferase < (Q5[2]+1.5*iqr5))


dat<- data_stat %>% filter(Total_Bilirubin >= 0.4 & Total_Bilirubin <= 5.3,
                           Direct_Bilirubin >= 0.1 & Direct_Bilirubin <= 4.2,
                           Alkaline_Phosphotase >= 63.0 & Alkaline_Phosphotase <= 187.0,
                           Alamine_Aminotransferase >= 10 & Alamine_Aminotransferase <= 58,
                           Aspartate_Aminotransferase >= 10 & Aspartate_Aminotransferase <= 95)

datfred <-na.omit(dat)

# Overview of the new dataset (after removing outliers and NAs)
summary(datfred)

# Installing package for categorical data analysis
library(bitops)
# Table - distribution of patient by gender and disease status (initial data and cleaned set)
table(data_stat$liver_disease, data_stat$Gender)
table(datfred$liver_disease, datfred$Gender)
# Gender-Status distribution using bar chart
data_stat  %>%
  group_by(Gender, liver_disease)%>% 
  summarize(n = n()) %>%
  mutate(Percent = n/sum(n)) %>%
  ggplot() +
  geom_col(aes(x = Gender, y = Percent, fill = liver_disease))

datfred  %>%
  group_by(Gender, liver_disease)%>% 
  summarize(n = n()) %>%
  mutate(Percent = n/sum(n)) %>%
  ggplot() +
  geom_col(aes(x = Gender, y = Percent, fill = liver_disease))

# Using a fisher test to see if women have high prevalence of liver disease
fisher.test(table(datfred$liver_disease, datfred$Gender), alternative = c("two.sided"))
# Using a fisher test to see if women have high prevalence of liver disease
fisher.test(table(datfred$liver_disease, datfred$Gender), alternative = c("greater"))
# Using a Chi-square test to see if women have high prevalence of liver disease
chisq.test(table(datfred$liver_disease, datfred$Gender))
# A scatterplot to see if outliers persisted and if gender and age could explain their presence.
plot(datfred$Age, datfred$Total_Bilirubin,
     pch = 19,
     col = factor(datfred$Gender))

legend("topleft",
       legend = levels(factor(datfred$Gender)),
       pch = 19,
       col = factor(levels(factor(datfred$Gender))))


plot(datfred$Age, datfred$Direct_Bilirubin ,
     pch = 19,
     col = factor(datfred$Gender))

legend("topleft",
       legend = levels(factor(datfred$Gender)),
       pch = 19,
       col = factor(levels(factor(datfred$Gender))))

plot(datfred$Age, datfred$Alkaline_Phosphotase,
     pch = 19,
     col = factor(datfred$Gender))

legend("topleft",
       legend = levels(factor(datfred$Gender)),
       pch = 19,
       col = factor(levels(factor(datfred$Gender))))

plot(datfred$Age, datfred$Alamine_Aminotransferase,
     pch = 19,
     col = factor(datfred$Gender))

legend("topleft",
       legend = levels(factor(datfred$Gender)),
       pch = 19,
       col = factor(levels(factor(datfred$Gender))))


plot(datfred$Age, datfred$Aspartate_Aminotransferase,
     pch = 19,
     col = factor(datfred$Gender))

legend("topleft",
       legend = levels(factor(datfred$Gender)),
       pch = 19,
       col = factor(levels(factor(datfred$Gender))))

# Exploratory data analysis on new dataset
library(ggstatsplot)
library(caTools)
library(cowplot)
library(PerformanceAnalytics)

# Correlation analytics for a better understanding of the relationship between variables
my_data <- datfred[, c(1,3,4,5,6,7,8,9,10)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# Final preparation of data for machine learning - converting the binary response variable to factor variable
datfred$liver_disease <- factor(datfred$liver_disease)
datfred$Gender <- factor(datfred$Gender)

# Installing packages for ML
library(caret)

# Use 70% of dataset as training set and 30% as test set
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = datfred$liver_disease, times = 1, p = 0.25, list = FALSE)
train <- datfred[-test_index,]
test <- datfred[test_index,]

# Training the data - initial model
initial_model <- glm(liver_disease ~ Gender + Age + Direct_Bilirubin + Alkaline_Phosphotase + 
                       Alamine_Aminotransferase + Total_Bilirubin + Total_Protiens + Albumin + Albumin_and_Globulin_Ratio, 
                     data = train, 
                     family = binomial(link="logit"))

second_model <- glm(liver_disease ~ Gender + Age + Direct_Bilirubin + Alkaline_Phosphotase + 
                      Alamine_Aminotransferase + Total_Protiens + Albumin + Albumin_and_Globulin_Ratio, 
                    data = train, 
                    family = binomial(link="logit"))

third_model <- glm(liver_disease ~ Gender + Age + Direct_Bilirubin + Alkaline_Phosphotase + 
                     Alamine_Aminotransferase + Total_Protiens, 
                   data = train, 
                   family = binomial(link="logit"))

fourth_model <- glm(liver_disease ~ Gender + Age + Alkaline_Phosphotase + Alamine_Aminotransferase + Total_Protiens, 
                    data = train, 
                    family = binomial(link="logit"))

fifth_model <- glm(liver_disease ~ Gender + Age + Alkaline_Phosphotase + Alamine_Aminotransferase, 
                   data = train, 
                   family = binomial(link="logit"))

summary(initial_model)
summary(second_model)
summary(third_model)
summary(fourth_model)
summary(fifth_model)

# Let's test our results on unused data (testing set)
probabs <- predict(fifth_model, test, type = 'response')
preds <- ifelse(probabs > 0.5, 1,0)

# Getting the confusion matrix
confusionMatrix(factor(preds), factor(test$liver_disease))







