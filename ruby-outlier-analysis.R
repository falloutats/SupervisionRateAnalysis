#import data into R
library(readxl)
data <- read_excel('C:/Users/ruby.nganho/OneDrive - Currie & Brown/Desktop/Current Labor Rate Database (Projects) for R.xlsx', sheet = 1)
Current_Labor_Rate_data <- as.data.frame(data)
head(Current_Labor_Rate_data, 10)
names(Current_Labor_Rate_data)
Labor_data_matrix <- as.matrix(data)

#basic data manipulation

#Needed packages
library(dplyr)
library(moments); library(corrplot); library(pso)
library(psych); library(GPArotation); library(lavaan); library(tidyr); library(zoo);
library(gridExtra); library(outliers); library(lubridate);

mean(Current_Labor_Rate_data[,18], na.rm = TRUE)
var(Current_Labor_Rate_data[,18], na.rm = TRUE)
skewness(Current_Labor_Rate_data[,18], na.rm = TRUE)
kurtosis(Current_Labor_Rate_data[,18], na.rm = TRUE)
barplot(table(Current_Labor_Rate_data[,18]))

#position average
(position_mean <- aggregate(x=Current_Labor_Rate_data[,18], by = list(Current_Labor_Rate_data[,1]), FUN = mean))
(position_variance <- aggregate(x=Current_Labor_Rate_data[,18], by = list(Current_Labor_Rate_data[,1]), FUN = var))
(position_median <- aggregate(x=Current_Labor_Rate_data[,18], by = list(Current_Labor_Rate_data[,1]), FUN = median))
(position_skewness <- aggregate(x=Current_Labor_Rate_data[,18], by = list(Current_Labor_Rate_data[,1]), FUN = skewness))
(position_kurtosis <- aggregate(x=Current_Labor_Rate_data[,18], by = list(Current_Labor_Rate_data[,1]), FUN = kurtosis))

#Project Manager rate analysis - NATIONAL

#a. subsetting and manipulating data
Supervision_rate <- Current_Labor_Rate_data[which(Current_Labor_Rate_data[,17]=='Supervision'), c(1,3,5,13,14,15,16,18)] #subsetting Supervision rates with necessary columns
head(Supervision_rate)
names(Supervision_rate)
PM_national <- Supervision_rate[which(Supervision_rate[,1]=='Project Manager'),] #subsetting PM data
PM_national[,2] <- PM_national[,2] %>% replace_na('General') #replacing NA with 'General'
head(PM_national)
(PM_mean <- aggregate(x=PM_national[,8], by = list(PM_national[,2]), FUN = mean)) #calculate means by category
(PM_mean <- aggregate(x=PM_national[,8], by = list(PM_national[,2]), FUN = skewness)) #no outlier for Associate and Lead, only have few data, skew right for other levels
 #b. outlier analysis
summary(PM_national[,8])

#Grouping Date into quarters
PM_national <- cbind(PM_national, Year = NA) #add an NA column
PM_national <- PM_national[,c(1:7,9,8)] #switch positions of Year column and Bill Rates column
names(PM_national)
PM_national[,7] <- as.yearqtr(PM_national[,7],format = '%m/%d/%y') #write date as quarters
PM_national[,8] <- sub(" Q[1-4]", "", PM_national[,7]) #paste years into the Year column
PM_national[,3] <- PM_national[,3] %>% replace_na('Reg')


PM_national_Reg <- PM_national[which(PM_national[,3]=='Reg'),] #subsetting PM data with Regular time
PM_national_OT <- PM_national[which(PM_national[,3]=='OT'),] #subsetting PM data with overtime
PM_national_2T <- PM_national[which(PM_national[,3]=='2T'),] #subsetting PM data with double time

#PM_national_Reg_General <- PM_national_Reg[which(PM_national_Reg[,2]=='General'),]
#PM_national_Reg_Assistant <- PM_national_Reg[which(PM_national_Reg[,2]=='Assistant'),]
#PM_national_Reg_Senior <- PM_national_Reg[which(PM_national_Reg[,2]=='Senior'),]

#finding outliers
boxplot(PM_national_Reg[,9] ~ PM_national_Reg[,2], xlab = "Levels", ylab = "Bill Rate", main = "Boxplots of PM levels")
aggregate(x=PM_national_Reg[,9], by = list(PM_national_Reg[,2]), FUN = summary)



hist(PM_national_Reg[,9],
     xlab = "Bill Rate",
     main = "Histogram of Bill Rate",)
qqnorm(PM_national_Reg[,9])
summary(PM_national_Reg[,9])


boxplot(PM_national_Reg[,9], horizontal = TRUE)
hist(PM_national_Reg[,9],
     xlab = "Bill Rate",
     main = "Histogram of Bill Rate",)
qqnorm(PM_national_Reg[,9])

#find outlier using IQR
##get Q1, Q2, Q3
summary(PM_national_Reg[,9])

##get IQR
PM_national_Reg_IQR <- IQR(PM_national_Reg[,9])
PM_national_Reg_IQR
##get threshold values for outliers
Tmin <- summary(PM_national_Reg[,9])[2] - (1.5*PM_national_Reg_IQR)
Tmax <- summary(PM_national_Reg[,9])[5] + (1.5*PM_national_Reg_IQR)

##find outlier
PM_national_Reg[which(PM_national_Reg[,9] < Tmin | PM_national_Reg[,9] > Tmax),]

#Find outliers using Chi-square test
chisq.out.test(PM_national_Reg_General[,9])


###################################################################################################################

#Find range for PM levels
# Initialize lists to store results
min_salary <- list()
max_salary <- list()

#Get unique Date
Dates_groups <- unique(PM_national[,7])

#Get unique Time
Times_groups <- unique(PM_national[,3])

# Get the unique level groups
levels_groups <- unique(PM_national_Reg[,2])

# Loop over each level group
for (level in levels_groups) {
  # Subset the data for the current trade group
  subset_level <- PM_national_Reg[PM_national_Reg[,2] == level, ]
  
  # Calculate the min and max salary for the current trade group
  min_salary[[level]] <- min(subset_level[,9])
  max_salary[[level]] <- max(subset_level[,9])
}

min_salary_df <- data.frame(MinSalary = unlist(min_salary))
max_salary_df <- data.frame(MaxSalary = unlist(max_salary))

# Convert the lists to data frames for better readability
min_salary_df <- data.frame(Level = names(min_salary), MinSalary = unlist(min_salary))
max_salary_df <- data.frame(Level = names(max_salary), MaxSalary = unlist(max_salary))

# Print the results
print(min_salary_df)
print(max_salary_df)

