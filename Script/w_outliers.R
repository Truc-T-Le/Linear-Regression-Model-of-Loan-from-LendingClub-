# Load necessary libraries
library(openintro)
library(caTools)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(GGally)
library(MASS)
library(MPV)
library(leaps)



# Load dataset
loans <- loans_full_schema


# Proceed with preprocessing on the training set
project <- subset.data.frame(loans, select = c(interest_rate, grade, annual_income, total_credit_lines, num_historical_failed_to_pay,
                                               total_credit_limit, debt_to_income, installment, loan_purpose, term, application_type,
                                               homeownership, loan_amount, public_record_bankrupt, total_credit_utilized, num_total_cc_accounts))



# Find and handle missing elements in the training set
missing_elements <- which(is.na(project), arr.ind = TRUE)
print(missing_elements)
project <- na.omit(project)


# Correlation Matrix for raw quant data
proj_contin <- subset.data.frame(project, select = c(interest_rate, annual_income, total_credit_lines, num_historical_failed_to_pay,
                                                     total_credit_limit, debt_to_income, installment, term
                                                     , loan_amount, public_record_bankrupt, total_credit_utilized, num_total_cc_accounts))
round(cor(proj_contin[, 2:12]),2)


# categorical variables 
project$homeownership <- droplevels(project$homeownership)
contrasts(project$homeownership) <- contr.treatment(3)

project$homeownership <- droplevels(project$homeownership)
contrasts(project$homeownership) <- contr.treatment(3)

project$application_type <- droplevels(project$application_type)
contrasts(project$application_type) <- contr.treatment(2)

project$loan_purpose <- droplevels(project$loan_purpose)
contrasts(project$loan_purpose) <- contr.treatment(12)

project$grade <- droplevels(project$grade)
contrasts(project$grade) <- contr.treatment(7)

project$term <- as.numeric(project$term)




f_raw <- lm(interest_rate ~ annual_income + debt_to_income + loan_amount + installment 
            + term + total_credit_utilized + total_credit_limit + num_total_cc_accounts 
            + public_record_bankrupt + num_historical_failed_to_pay + total_credit_lines
            + loan_purpose + application_type + homeownership, data = project)
summary(f_raw)

plot(fitted.values(f_raw), rstudent(f_raw), col = factor(project$grade), xlab = "Fitted Value"
     , ylab = "Studentized Residual", main = "Residual Plot by Grades")
abline(h = 0, col = "black")

f_y <- lm(I(log(interest_rate)) ~ annual_income + debt_to_income + loan_amount + installment 
            + term + total_credit_utilized + total_credit_limit + num_total_cc_accounts 
            + public_record_bankrupt + num_historical_failed_to_pay + total_credit_lines
            + loan_purpose + application_type + homeownership, data = project)
plot(f_y)

plot(fitted.values(f_y), rstudent(f_y), col = factor(project$grade), xlab = "Fitted Value"
     , ylab = "Studentized Residual", main = "Residual Plot by Grades")

f <- lm(I(log(interest_rate)) ~ term + total_credit_limit + debt_to_income + 
          total_credit_utilized + total_credit_lines + loan_amount + num_total_cc_accounts +  
          num_historical_failed_to_pay + as.factor(loan_purpose) + as.factor(homeownership), data = project)
          
f1 <- lm(I(log(interest_rate)) ~ grade, data = project)      


fa <- lm(interest_rate ~ grade, data = project) 
        
        