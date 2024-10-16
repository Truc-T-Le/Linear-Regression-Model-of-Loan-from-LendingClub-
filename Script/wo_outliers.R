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
library(xtable)


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
cor_ma <- round(cor(proj_contin[, 2:12]),2)
upper<-cor_ma
upper[upper.tri(cor_ma)]<-""
upper<-as.data.frame(upper)
upper


xtable(upper)
f_raw <- lm(interest_rate ~ annual_income + debt_to_income + loan_amount + installment 
            + term + total_credit_utilized + total_credit_limit + num_total_cc_accounts 
            + public_record_bankrupt + num_historical_failed_to_pay + total_credit_lines + grade
            + loan_purpose + application_type + homeownership, data = project)


# calculating leverage
X <- as.matrix(cbind(1, project$annual_income, project$debt_to_income,  project$loan_amount, project$installment, 
                     project$term, project$total_credit_utilized, project$total_credit_limit, project$num_total_cc_accounts, 
                     project$public_record_bankrupt, project$num_historical_failed_to_pay, project$total_credit_lines, project$grade,
                     project$loan_purpose, project$application_type, project$homeownership))
hat <- X%*%solve(t(X)%*%X)%*%t(X)
h_ii <- diag(hat)
sort(h_ii, decreasing = TRUE, index.return = TRUE)

# calculating cook's distance
d <- cooks.distance(f_raw)
sort(d, decreasing = TRUE, index.return = TRUE)

eis <- resid(f_raw)

# residuals


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


## distribution of interest rate
ggplot(project, aes(x=interest_rate, color=grade, fill = grade)) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 0.5) +
  scale_x_continuous(breaks = seq(5, 31, 2), lim = c(5, 31)) +
  labs(title="Interest Rate Histogram",x="Interest Rate (%)", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) 
 


## interest rate vs grade
plot(x = project$grade, y = project$interest_rate,
     main = "Distribution of Interest Rate for each Grade",
     xlab = "Grade",
     ylab = "Interest Rate (Percentage)")

fit <- lm(interest_rate ~ grade , data = project)
plot(fitted.values(fit), rstandard(fit), xlab = "Predicted Response", ylab = "Standarsized Residuals", main = "Residuals vs.Predictor (Grade)")
abline(h = 0)


## Annual Income
df1 <- project[c("interest_rate", "annual_income")] |> 
  filter(annual_income < 500000)

f1 <- lm(interest_rate ~ annual_income , data = project)
plot(log(project$annual_income), rstandard(f1), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")
abline(h = 1)
summary(f1)
qqnorm(rstandard(f1), pch = 1, frame = FALSE, 
       main = "QQ-Plot of Residuals for log(X)")
qqline(rstandard(f1), col = "steelblue", lwd = 2)

f1t <- lm(interest_rate ~ I(log(annual_income)) , data = project)

## Total Credit Lines
f2 <- lm(interest_rate ~ total_credit_lines , data = project)
plot(project$total_credit_lines, rstandard(f2), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")
abline(h = 1)
summary(f2)
qqnorm(rstandard(f2), pch = 1, frame = FALSE, 
       main = "QQ-Plot of Residuals for log(X)")
qqline(rstandard(f2), col = "steelblue", lwd = 2)


f2t <- lm(interest_rate ~ I(log(total_credit_lines)) , data = project)
plot(log(project$total_credit_lines), rstandard(f2), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")



# historical failed to pay
f3 <- lm(interest_rate ~ num_historical_failed_to_pay , data = project)
plot(project$num_historical_failed_to_pay, rstandard(f3), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")
abline(h = 1)
summary(f3)
qqnorm(rstandard(f3), pch = 1, frame = FALSE, 
       main = "QQ-Plot of Residuals for log(X)")
qqline(rstandard(f3), col = "steelblue", lwd = 2)


# total credit limit
f4 <- lm(interest_rate ~ total_credit_limit , data = project)
plot(project$total_credit_limit, rstandard(f4), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")
abline(h = 1)
summary(f4)
qqnorm(rstandard(f4), pch = 1, frame = FALSE, 
       main = "QQ-Plot of Residuals for log(X)")
qqline(rstandard(f4), col = "steelblue", lwd = 2)


# number of historical failed to pay
f5 <- lm(interest_rate ~ num_historical_failed_to_pay , data = project)
plot(project$num_historical_failed_to_pay, rstandard(f5), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")
abline(h = 1)
qqnorm(rstandard(f5), pch = 1, frame = FALSE, 
       main = "QQ-Plot of Residuals for log(X)")
qqline(rstandard(f5), col = "steelblue", lwd = 2)


# total credit limit
f6 <- lm(interest_rate ~ total_credit_limit , data = project)
plot(project$total_credit_limit, rstandard(f6), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")
abline(h = 1)
summary(f6)

f6t <- lm(interest_rate ~ I(sqrt(total_credit_limit)) , data = project)


# total credit utilized
f7 <- lm(interest_rate ~ total_credit_utilized , data = project)
plot(project$total_credit_utilized, rstandard(f7), ylab = "Standarsized Residuals", main = "Residuals vs. Predictor")
abline(h = 1)
summary(f7)

f7t <- lm(interest_rate ~ I(sqrt(total_credit_utilized)) , data = project)

# Debt to Income
f8 <- lm(interest_rate ~ debt_to_income , data = project)

f8t <- lm(interest_rate ~ I(sqrt(debt_to_income)) , data = project)

# Public Record Bankrupt 

f9 <-  lm(interest_rate ~ public_record_bankrupt , data = project)

# number of total credit card accounts

f10 <- lm(interest_rate ~ num_total_cc_accounts , data = project)

f10t <- lm(interest_rate ~ (sqrt(num_total_cc_accounts)) , data = project)


# term

f11 <- lm(interest_rate ~ term , data = project)

# installment 

f12 <- lm(interest_rate ~ installment , data = project)



f12t <- lm(interest_rate ~ I(log(installment)) , data = project)


f <- lm(interest_rate ~ I(log(annual_income)) + I(sqrt(debt_to_income)) + loan_amount + I(log(installment)) 
        + term + I(sqrt(total_credit_utilized)) + I(sqrt(total_credit_limit)) + num_total_cc_accounts 
        + public_record_bankrupt + num_historical_failed_to_pay + I(log(total_credit_lines)) + grade
        + loan_purpose + application_type + homeownership, data = project)

summary(f)


f_raw <- lm(interest_rate ~ annual_income + debt_to_income + loan_amount + installment 
        + term + total_credit_utilized + total_credit_limit + num_total_cc_accounts 
        + public_record_bankrupt + num_historical_failed_to_pay + total_credit_lines + grade
        + loan_purpose + application_type + homeownership, data = project)
summary(f_raw)


ggarrange(
  SOP, sop_box, LOR, LOR_box, rank, rank_box,    
  nrow = 3, ncol = 2, labels = c('A', "",
                                 'B', "",
                                 'C', "")) 


