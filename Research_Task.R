#' ---
#' title: GRA6547 - Tasks for mid-term exam
#' subtitle: Task1, Task2, Task3
#' authors: 1062611,1062899,1067957,1103850
#' date: '27-03-2025'
#' output: pdf_document
#' ---

##############################
## Preparatory steps
# Housekeeping
rm(list=ls())

# Packages 
library(rmarkdown)  # to generate pdf file
library(tinytex)    # to generate pdf file
library(readxl)     # to read Excel files
library(car)        # to perform joint hypothesis tests
library(lmtest)     # to estimate regression using Newely-West standard errors
library(sandwich)   # to estimate regression using Newely-West standard errors

# Set working directory
setwd("C:/Sashwot/Semester_2/Courses/Research_Method/Mid-Term")

########################################
###______________TASK1_______________###
########################################

## Import data and set as data.frame
data1 <- read_excel("task1_data.xlsx")
colnames(data1) <- c("Quarter", "r_t", "Real_GDP", "Potential_GDP", "Inflaltion")
data1 <- as.data.frame(data1)

# Data Structure check
#str(data1)

# Changing the structure of data
data1$Quarter <- as.Date(data1$Quarter)

# Output Gap Calculation
data1$y_t <- 100*((data1$Real_GDP - data1$Potential_GDP)/data1$Potential_GDP)

# Inflation Gap Calculation
data1$p_t <- data1$Inflaltion - 2

##1 a) Estimation the policy rule by performing multivariate regression
# Unrestricted Regression
policy_unrst.reg <- lm(r_t ~ y_t + p_t, data = data1 )
summary(policy_unrst.reg)

# Parameter estimates and their t-stats
# Parameters
alpha_hat <- round(summary(policy_unrst.reg)$coefficients[1,1],4)
beta_hat <- round(summary(policy_unrst.reg)$coefficients[2,1],4)
gamma_hat <- round(summary(policy_unrst.reg)$coefficients[3,1],4)

# t-stats
tstat_alpha_hat <- round(summary(policy_unrst.reg)$coefficients[1,3],4)
tstat_beta_hat <- round(summary(policy_unrst.reg)$coefficients[2,3],4)
tstat_gamma_hat <- round(summary(policy_unrst.reg)$coefficients[3,3],4)

# Summarizing all the results in a table
parameter.1a <- matrix(c(alpha_hat,tstat_alpha_hat,beta_hat,tstat_beta_hat,
                         gamma_hat,tstat_gamma_hat),ncol = 3)
colnames(parameter.1a) <- c('Alpha_hat','Beta_hat','Gamma_hat')
rownames(parameter.1a) <- c('Parameter','t-stat')
print(parameter.1a)

# 1 b) 
alpha = 0.05
T <- nrow(policy_unrst.reg$model)
k <- ncol(policy_unrst.reg$model)
m <- 2

# Performing the F-test 
urss <- sum(policy_unrst.reg$residuals^2)
# Performing Restricted Regression
policy_rst.reg <- lm(r_t ~ 1, data = data1)
#summary(policy_rst.reg)
rrss <- sum(policy_rst.reg$residuals^2)

# F test stat
f.test_stat <- ((rrss - urss)/urss) * ((T-k)/m)
# F critical
f.critical_val <- qf(1-alpha,m,T-k)

# Printing the required residuals, F stat, and Critical Value
residuals.1b <- matrix(c(urss,rrss,f.test_stat,f.critical_val),ncol = 1)
#colnames(parameter.1a) <- c('Alpha_hat','Beta_hat','Gamma_hat')
rownames(residuals.1b) <- c('URSS','RRSS','f.test_stat','f.critical_val')
print(residuals.1b)

# Result of Hypothesis test 
if ( f.test_stat > f.critical_val ) {
  decision <- "reject H0"
} else {
  decision <- "do not reject H0"
}
print(decision)

## 1 c) White Test
data1$u <- summary(policy_unrst.reg)$residuals
data1$u2 <- data1$u^2

# auxiliary regression 
data1$c11 <- data1$y_t^2
data1$c12 <- data1$y_t * data1$p_t
data1$c22 <- data1$p_t^2

white_unr.reg <- lm(u2 ~ y_t + p_t + data1$c11 + data1$c12 + data1$c22,
                    data = data1 )
summary(white_unr.reg)

#Hypothesis Test (Using Chi-squared test)
T <- nrow(white_unr.reg$model)
k <- ncol(white_unr.reg$model) # Number of parameters in the auxiliary regression
R2_aux <- summary(white_unr.reg)$r.squared
m <- 5 

chi.test_stat <- R2_aux * T
#print(chi.test_stat)

chi.crit_val <- qchisq(1-alpha, m) 
#print(chi.crit_val)

if ( chi.test_stat > chi.crit_val ) {
  decision2 <- "reject H0"
} else {
  decision2 <- "do not reject H0"
}
print(decision2)

# Printing the required Chi test Stat, and Critical Value
stats.1c <- matrix(c(round(chi.test_stat,4),round(chi.crit_val,4)),ncol = 1)
rownames(stats.1c) <- c('Chi Test Stat','Chi Critical Value')
print(stats.1c)

## 1d) Breush-Godfrey test for Autocorrelation(First order)
T <- nrow(policy_unrst.reg$model)

# Creating first order lagged estimate residuals (t-1)
data1$u1 <- c(0,data1$u[1:T-1])

# Auxiliary Regression
aux_reg.1d <- lm(u ~ y_t + p_t + u1, data = data1 )
summary(aux_reg.1d)

R2_aux2 <- summary(aux_reg.1d)$r.squared
r <- 1 # Maximum number of lags

# Hypothesis Test (Using Chi-squared test)

aux2.test_stat <- (T-r)*R2_aux2 # Test Statistics
aux2.crit_val <- qchisq(1-alpha, r) # Critical Value

if ( aux2.test_stat > aux2.crit_val ) {
  decision3 <- "reject H0"
} else {
  decision3 <- "do not reject H0"
}
print(decision3)

# Printing the required Chi test Stat, and Critical Value
stats.1d <- matrix(c(round(aux2.test_stat,4),
                     round(aux2.crit_val,4)),ncol = 1)
rownames(stats.1d) <- c('Chi Test Stat','Chi Critical Value')
print(stats.1d)

## 1 e) Test H0: gamma = 1.5, H1: gamma != 1.5
new_west <- coeftest(policy_unrst.reg,
                     vcov. = NeweyWest(policy_unrst.reg,
                                       lag = 1, adjust = FALSE,
                                       prewhite = FALSE) )

SE_gamma_hat <- new_west[3,2]

policy1e.test_stat <- (gamma_hat - 1.5)/SE_gamma_hat # test stat

T <- nrow(policy_unrst.reg$model)
k <- ncol(policy_unrst.reg$model)

policy1e.crit_val <- qt(1/2*alpha, T-k) # Critical Value

# Hypothesis Test (t-test)
if ( abs(policy1e.test_stat) > abs(policy1e.crit_val) ) {
  decision4 <- "reject H0"
} else {
  decision4 <- "do not reject H0"
}
print(decision4)

# Printing the t-stat and critical value
stats.1e <- matrix(c(round(abs(policy1e.test_stat),4),
                     round(abs(policy1e.crit_val),4)),ncol = 1)
rownames(stats.1e) <- c('test stat','Critical Value')
print(stats.1e)

## 1 f) Estimate the linear regression model
## Part 1
# Restrict sample period (from 1983:Q1 to 2007 Q4)
data1_1 <- data1[ (data1$Quarter >= "1983-01-01")
                  &  (data1$Quarter <= "2007-10-01"), ]
reg.1f_1 <- lm(r_t ~ y_t + p_t, data = data1_1)
#summary(reg.1f_1)

# Parameters_1
alpha_hat_1 <- round(summary(reg.1f_1)$coefficients[1,1],4)
beta_hat_1 <- round(summary(reg.1f_1)$coefficients[2,1],4)
gamma_hat_1 <- round(summary(reg.1f_1)$coefficients[3,1],4)

# t-stats_1
tstat_alpha_hat_1 <- round(summary(reg.1f_1)$coefficients[1,3],4)
tstat_beta_hat_1 <- round(summary(reg.1f_1)$coefficients[2,3],4)
tstat_gamma_hat_1 <- round(summary(reg.1f_1)$coefficients[3,3],4)

# Summarizing all the results in a table
parameter.1f1 <- matrix(c(alpha_hat_1,tstat_alpha_hat_1,
                          beta_hat_1,tstat_beta_hat_1,
                         gamma_hat_1,tstat_gamma_hat_1),ncol = 3)
colnames(parameter.1f1) <- c('Alpha_hat','Beta_hat','Gamma_hat')
rownames(parameter.1f1) <- c('Parameter','t-stat')
print(parameter.1f1)

## Part 2
# Restrict sample period (from 2008:Q1 to 2024:Q4)
data1_2 <- data1[ (data1$Quarter >= "2008-01-01")
                  &  (data1$Quarter <= "2024-10-01"), ]
reg.1f_2 <- lm(r_t ~ y_t + p_t, data = data1_2)
#summary(reg.1f_2)

# Parameters_2
alpha_hat_2 <- round(summary(reg.1f_2)$coefficients[1,1],4)
beta_hat_2 <- round(summary(reg.1f_2)$coefficients[2,1],4)
gamma_hat_2 <- round(summary(reg.1f_2)$coefficients[3,1],4)

# t-stats_2
tstat_alpha_hat_2 <- round(summary(reg.1f_2)$coefficients[1,3],4)
tstat_beta_hat_2 <- round(summary(reg.1f_2)$coefficients[2,3],4)
tstat_gamma_hat_2 <- round(summary(reg.1f_2)$coefficients[3,3],4)

# Summarizing all the results in a table
parameter.1f2 <- matrix(c(alpha_hat_2,tstat_alpha_hat_2,
                          beta_hat_2,tstat_beta_hat_2,
                          gamma_hat_2,tstat_gamma_hat_2),ncol = 3)
colnames(parameter.1f2) <- c('Alpha_hat','Beta_hat','Gamma_hat')
rownames(parameter.1f2) <- c('Parameter','t-stat')
print(parameter.1f2)


########################################
###______________TASK2_______________###
########################################

## Preparatory steps
# Housekeeping
rm(list=ls())

# Packages 
library(rmarkdown)  # to generate pdf file
library(tinytex)    # to generate pdf file
library(readxl)     # to read Excel files
library(latex2exp)  # to get formulas into plots

# Set working directory
setwd("C:/Sashwot/Semester_2/Courses/Research_Method/Mid-Term")

##############################
## Import data and set as data.frame
data2 <- read_excel("task2_data.xlsx")
data2 <- as.data.frame(data2)

## 2a) Pooled Regression
pooled_reg <- lm(math4 ~ enrol + expp + lunch, data = data2)
summary(pooled_reg)

# Parameter estimates and their t-stats
# Parameters
beta_hat.1 <- round(summary(pooled_reg)$coefficients[1,1],4)
beta_hat.2 <- round(summary(pooled_reg)$coefficients[2,1],4)
beta_hat.3 <- round(summary(pooled_reg)$coefficients[3,1],4)
beta_hat.4 <- round(summary(pooled_reg)$coefficients[4,1],4)

# t-stats
tstat_beta_hat.1 <- round(summary(pooled_reg)$coefficients[1,3],4)
tstat_beta_hat.2 <- round(summary(pooled_reg)$coefficients[2,3],4)
tstat_beta_hat.3 <- round(summary(pooled_reg)$coefficients[3,3],4)
tstat_beta_hat.4 <- round(summary(pooled_reg)$coefficients[4,3],4)

# Summarizing all the results in a table
parameter.2a <- matrix(c(beta_hat.1,tstat_beta_hat.1,beta_hat.2,
                         tstat_beta_hat.2,beta_hat.3,
                         tstat_beta_hat.3,beta_hat.4,tstat_beta_hat.4),
                       ncol = 4)
colnames(parameter.2a) <- c('Beta_hat1','Beta_hat2','Beta_hat3','Beta_hat4')
rownames(parameter.2a) <- c('Parameter','t-stat')
print(parameter.2a)

## 2b) Regression with distid-fixed effects
# Introduce categories for countries
data2$d_cat <- as.factor(data2$distid) # Category that r can handle to separate variables
#head(data2)
distid_cat <- unique(data2$d_cat)
#summary(distid_cat)
N <- length(distid_cat)
#print(N)

# Regression with distid-fixed effects
reg_distid.fixed <- lm(math4 ~ enrol + expp + lunch + d_cat , data=data2)
summary(reg_distid.fixed)$coefficient[1:4,]

# Parameter estimates and their t-stats
# Parameters
beta_hat.1 <- round(summary(reg_distid.fixed)$coefficients[1,1],4)
beta_hat.2 <- round(summary(reg_distid.fixed)$coefficients[2,1],4)
beta_hat.3 <- round(summary(reg_distid.fixed)$coefficients[3,1],4)
beta_hat.4 <- round(summary(reg_distid.fixed)$coefficients[4,1],4)

# t-stats
tstat_beta_hat.1 <- round(summary(reg_distid.fixed)$coefficients[1,3],4)
tstat_beta_hat.2 <- round(summary(reg_distid.fixed)$coefficients[2,3],4)
tstat_beta_hat.3 <- round(summary(reg_distid.fixed)$coefficients[3,3],4)
tstat_beta_hat.4 <- round(summary(reg_distid.fixed)$coefficients[4,3],4)

# Summarizing all the results in a table
parameter.2b <- matrix(c(beta_hat.1,tstat_beta_hat.1,beta_hat.2,
                         tstat_beta_hat.2,beta_hat.3,
                         tstat_beta_hat.3,beta_hat.4,tstat_beta_hat.4),
                       ncol = 4)
colnames(parameter.2b) <- c('Beta_hat1','Beta_hat2','Beta_hat3','Beta_hat4')
rownames(parameter.2b) <- c('Parameter','t-stat')
print(parameter.2b)

## 2c) Regression with both distid-fixed effects and year(time) fixed effects
# Introduce categories for year

data2$y_cat <- as.factor(data2$year) # Category that r can handle to separate variables
#head(data2)
year_cat <- unique(data2$y_cat)
#summary(year_cat)
N <- length(year_cat)
#print(N)

# Performing the regression
reg_both.eff <- lm(math4 ~ enrol + expp + lunch + d_cat + y_cat ,
                   data=data2)
summary(reg_both.eff)$coefficients[1:4,]

# Parameters
beta_hat.1 <- round(summary(reg_both.eff)$coefficients[1,1],4)
beta_hat.2 <- round(summary(reg_both.eff)$coefficients[2,1],4)
beta_hat.3 <- round(summary(reg_both.eff)$coefficients[3,1],4)
beta_hat.4 <- round(summary(reg_both.eff)$coefficients[4,1],4)

# t-stats
tstat_beta_hat.1 <- round(summary(reg_both.eff)$coefficients[1,3],4)
tstat_beta_hat.2 <- round(summary(reg_both.eff)$coefficients[2,3],4)
tstat_beta_hat.3 <- round(summary(reg_both.eff)$coefficients[3,3],4)
tstat_beta_hat.4 <- round(summary(reg_both.eff)$coefficients[4,3],4)

# Summarizing all the results in a table
parameter.2c <- matrix(c(beta_hat.1,tstat_beta_hat.1,beta_hat.2,
                         tstat_beta_hat.2,beta_hat.3,
                         tstat_beta_hat.3,beta_hat.4,tstat_beta_hat.4),
                       ncol = 4)
colnames(parameter.2c) <- c('Beta_hat1','Beta_hat2','Beta_hat3','Beta_hat4')
rownames(parameter.2c) <- c('Parameter','t-stat')
print(parameter.2c)

########################################
###______________TASK3_______________###
########################################
## Preparatory steps
# Housekeeping
rm(list=ls())

# Packages 
library(rmarkdown)  # to generate pdf file
library(tinytex)    # to generate pdf file
library(readxl)     # to read Excel files
library(car)        # to perform joint hypothesis tests

# Set working directory
setwd("C:/Sashwot/Semester_2/Courses/Research_Method/Mid-Term")

##############################
## Import data and set as data.frame
data3 <- read_excel("task3_data.xlsx")
data3 <- as.data.frame(data3)
colnames(data3) <- c("date", "y1t", "y5t")
data3$date <- as.Date(data3$date)

# Calculating the Spread(st)
data3$st <- data3$y5 - data3$y1

# Adjusting the period of data
data3 <- data3[   (data3$date >= "1970-01-01")&
                    (data3$date <= "2024-12-01"), ]

# Restricting the sample period
in_sample <- data3[data3$date >= "1970-01-01" &
                     data3$date <= "2020-12-01", ]
out_sample <- data3[data3$date >= "2021-01-01" &
                      data3$date <= "2024-12-01", ]

## 3a)
# Information criteria for alternative ARMA(p,q) models
best_SBIC <- Inf
best_model <- NULL
SBIC_alt <- matrix(data=NA,nrow=5,ncol=5)
rownames(SBIC_alt) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)","AR(4)")
colnames(SBIC_alt) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)","MA(4)")
for ( i in 1:5 ) {
  for ( j in 1:5 ) {
    p_i <- i-1
    q_j <- j-1
    arma_pq <- arima(in_sample$st, order = c(p_i, 0, q_j),
                     include.mean=TRUE, method="ML")
    SBIC_alt[i,j] <- BIC(arma_pq)
    if (SBIC_alt[i,j] < best_SBIC) {
      best_SBIC <- SBIC_alt[i,j]
      best_model <- arma_pq
    }
  }
}

# Reporting the optimal model and the corresponding SBIC
aux <- which( SBIC_alt == min(SBIC_alt), arr.ind = TRUE )
paste0("SBIC: ARMA(", aux[1]-1, ",", aux[2]-1, ")")
print(paste('SBIC:',best_SBIC))
print(best_model)

# ACF and PACF for estimated residuals up to 12 lags
par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(residuals(best_model),lag.max = 12, main = "ACF of Residuals")
pacf(residuals(best_model),lag.max = 12, main = "PACF of Residuals")

## 3b) One period ahead forecast using a rolling window of 220 years
# Data used in this part of solution
est_data <- data3[data3$date >= "2001-01-01", ] 
T <- nrow(est_data)
#T <- nrow(in_sample)
roll_window <- 240 # Since the rolling window is 20 years
# Using the predict function
#data3$forecast_st   <- rep(NA, T)
est_data$forecast_st <- rep(NA, T)

p <- 2
q <- 1

for (t in roll_window : (T-1)) {
  # 20-year sample period to estimate the model
  sample_est <- est_data$st[(t - roll_window + 1) : t]
  
  # Estimating ARMA(2,1) model based on the 20-year sample period
  arma_est  <- arima(sample_est, 
                     order = c(p,0,q), 
                     include.mean = TRUE,
                     method = "ML", 
                     optim.control = list(maxit = 1000))
  
  # create one-period ahead forecast
  est_data$forecast_st[t+1] <- predict(arma_est, n.ahead = 1)$pred      
}

# Plotting the Forecasts and Actual Values
# Adjust the margins and outer margins
par(mfrow=c(1,1))
plot(est_data$date[roll_window:T], est_data$st[roll_window:T],
     main = '',
     type = "l", 
     col  = "black",
     xlab = "", 
     ylab = "")
lines(est_data$date[roll_window:T], est_data$forecast_st[roll_window:T], 
      col = "red")
legend("bottomleft", legend = c("Actual", "One-period ahead forecast"), 
       col = c("black", "red"), 
       lty = 1,
       cex = 0.5)

# Mean Squared Forecast Error
#N <- sum(!is.na(est_data$forecast_error))
est_data$forecast_error = est_data$st - est_data$forecast_st
Meansq_error = mean(est_data$forecast_error^2,na.rm = TRUE)
print(paste('Mean Squared Error:', round(Meansq_error,4)))








