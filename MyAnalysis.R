# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script contains the necessary code for the analysis.
df <- read.csv(file.path("data", "data_tidy.csv"))
head(df)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Descriptive Statistics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~



#Initialize FertCat column with 0
df$FertCat <- 0

# Assign value 1 to FertCat where AdolFertRate is in the interval (0, 50]
df$FertCat[df$AdolFertRate > 0 & df$AdolFertRate <= 50] <- 1

# Assign value 2 to FertCat where AdolFertRate is in the interval (50, 100]
df$FertCat[df$AdolFertRate > 50 & df$AdolFertRate <= 100] <- 2

# Assign value 3 to FertCat where AdolFertRate is in the interval (100, +∞)
df$FertCat[df$AdolFertRate > 100] <- 3
df$FertCat

#InflCat
df$InflCat <- 0

#Assign value 1 to InflCat if Inflation is larger than 2%
df$InflCat[df$Inflation > 2] <- 1
df$InflCat

corruption <- df$CorControl
political_stab <- df$PolStab
voice <- df$VoiceAcc
par(mfrow = c(1,2))

#For CorControl(quantitative) Not Normally Distributed
hist(corruption,main = "Έλεγχος Διαφθοράς", breaks= 10, col ="#AAAAAA",freq = FALSE)
lines(density(corruption))
boxplot(corruption, main="Έλεγχος Διαφθοράς")
qqnorm(corruption)
qqline(corruption)
shapiro.test(corruption)
sw_result <- shapiro.test(corruption)
p_value <- sw_result$p.value
if(p_value > 0.05) {
  cat("The data is normally distributed (p=", p_value, ")\n")
} else {
  cat("The data is not normally distributed (p=", p_value, ")\n")
}
median(corruption)
quantile(corruption)

#For PolStab(quantitative) Not Normally Distributed
hist(political_stab,main = "Πολιτική Σταθερότητα",breaks =10,col = "#AAAAAA",freq = FALSE)
lines(density(political_stab))
boxplot(political_stab, main="Πολιτική Σταθερότητα")
qqnorm(political_stab)
qqline(political_stab)
shapiro.test(political_stab)
sw_result <- shapiro.test(political_stab)
p_value <- sw_result$p.value
if(p_value>0.05){
  cat("The data is normally distributed(p=",p_value,")\n")
} else {
  cat("The data is not normally distributed (p=", p_value, ")\n")
}
median(political_stab)
quantile(political_stab)

#For VoiceAcc(quantitative) Not Normally Distributed
hist(voice,main = "Φωνή και Λογοδοσία",breaks = 10, col="#AAAAAA",freq = FALSE)
lines(density(voice))
boxplot(voice, main = "Φωνή και Λογοδοσία")
qqnorm(voice)
qqline(voice)
shapiro.test(voice)
sw_result <- shapiro.test(voice)
p_value <- sw_result$p.value
if(p_value > 0.05) {
  cat("The data is normally distributed (p=", p_value, ")\n")
} else {
  cat("The data is not normally distributed (p=", p_value, ")\n")
}
median(voice)
quantile(voice)

#For FertCat(qualitative)
fertility <- df$FertCat
fert_freq <- table(fertility)
fert_freq
barplot(fert_freq, main = "Ποσοστό Γονιμότητας", xlab= "", col = "red")

# Pie Chart
pie(fert_freq,
    main = "Ποσοστό Γονιμότητας",
    col = c("lightblue", "lightgreen", "lightpink"),
    labels = c("Ποσοστό γονιμότητας εφήβων στο (0,50]",
               "Ποσοστό γονιμότητας εφήβων στο (50,100]",
               "Ποσοστό γονιμότητας εφήβων στο (100,Inf)"),
    cex = 0.8)
legend("topright",
       legend = c("Ποσοστό γονιμότητας εφήβων στο (0,50]",
                  "Ποσοστό γονιμότητας εφήβων στο (50,100]",
                  "Ποσοστό γονιμότητας εφήβων στο (100,Inf)"),
       fill = c("lightblue", "lightgreen", "lightpink"))
title(main = "Ποσοστό Γονιμότητας")

#For InflCat(qualitative)
inflation <- df$InflCat
infl_freq <- table(inflation)
barplot(infl_freq, main = "Ρυθμός πληθωρισμού", xlab= "Αριθμός χωρών με ρυθμό πληθωρισμού μικρότερο ή μεγαλύτερο του 2%", col = "orange")

#Pie chart
pie(infl_freq,
    main = "Ρυθμός πληθωρισμού",
    labels = c("Πληθωρισμός ≤ 2%", "Πληθωρισμός > 2%"),
    col = c("lightblue", "lightgreen"),
    cex = 0.8)
legend("topright",
       legend = c("Πληθωρισμός ≤ 2%", "Πληθωρισμός > 2%"),
       fill = c("lightblue", "lightgreen"))
title(main = "Ρυθμός πληθωρισμού")



# Calculate basic descriptors for CorControl, PolStab, and VoiceAcc by FertCat
library(psych)
?describeBy
descriptors <- describeBy(x = df[, c("CorControl", "PolStab", "VoiceAcc")],
                          group = df$FertCat,
                          mat = TRUE)
print(descriptors)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linear Model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
model <- lm(CorControl ~ . -Country - PolStab - VoiceAcc, data = df)
summary(model)

#Model Selection
# Fit the initial null model
null_model <- lm(CorControl ~ 1, data = df)
summary(null_model)

# Perform backward stepwise selection with AIC
backward_stepwise <- step(model, direction = "backward")

# Print the summary of the final model
summary(backward_stepwise)

#Select the model with the lowest AIC as the "optimal" model, which is in our case the backward_stepwise model.
AIC(model)
AIC(backward_stepwise)

# Print the computed R-squared and adjusted R-squared
r_squared <- summary(backward_stepwise)$r.squared
adj_r_squared <- summary(backward_stepwise)$adj.r.squared
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")


#Computing the coefficients of the following variables
smodel <- summary(backward_stepwise)
smodel

bhat <- backward_stepwise$coefficients
bhat
std_bhat <- smodel$coefficients[,"Std. Error"]
std_bhat

tPopGrowth <- abs((bhat["PopGrowth"])/std_bhat["PopGrowth"])
tPopGrowth
tElec <- abs(bhat["ElectrAccess"]/std_bhat["ElectrAccess"])
tElec
X <- cbind(rep(1, nrow(stepwise_model$model)), as.matrix(stepwise_model$model[-1]))
X
a <- 0.05
qta1 <- qt(1 - a / 2, n - ncol(X))
qta1
qta2 <- qt(a / 2, n - ncol(X), lower.tail = FALSE)
qta2

n <- nrow(stepwise_model$model)
p <- ncol(stepwise_model$model) - 1
df <- n - p - 1

alpha <- 0.05
critical_t <- qt(1 - alpha / 2, df)

is_significant_PopGrowth <- tPopGrowth > critical_t
is_significant_ElectrAccess <- tElectrAccess > critical_t

cat("t-value for PopGrowth:", tPopGrowth, "\n")
cat("t-value for ElectrAccess:", tElectrAccess, "\n")
cat("Critical t-value:", critical_t, "\n")
cat("Is PopGrowth significant?", is_significant_PopGrowth, "\n")
cat("Is ElectrAccess significant?", is_significant_ElectrAccess, "\n")


#Confidence Intervals
bounds <- matrix(NA, nrow = length(std_bhat), ncol = 2, dimnames = list(names(std_bhat), c("Lower Bound", "Upper Bound")))
bounds
for (var in names(std_bhat)){
  alpha<-0.01
  width<-qt(alpha/2,df=n-ncol(X),lower.tail = FALSE)*std_bhat[var]
  UB<-bhat[var]+width
  LB<-bhat[var]-width
  bounds[var,] <- c(LB, UB)
}
bounds


#We need to check model assumptions:
# 1.Independacy
# 2.Homoskedasticity
# 3.Normality
install.packages("lmtest")
library(lmtest)
residuals <- resid(backward_stepwise)
fitted_values <- fitted(backward_stepwise)

# 1. Plot Residuals vs. Fitted Values for Independence
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red")

bp_test <- bptest(backward_stepwise)
print(bp_test)

qqnorm(residuals)
qqline(residuals)

shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
boxplot(residuals, main = "Boxplot of Residuals")

#Model Transformation
constant <- abs(min(df$CorControl)) + 1
df$log_CorControl <- log(df$CorControl + constant)
df$log_CO2 <- log(df$CO2 + 1)
df$log_AgriLand <- log(df$AgriLand + 1)

log_model <- lm(log_CorControl ~ log_CO2 + log_AgriLand, data = df)
summary(log_model)

log_residuals <- resid(log_model)
bp_test_log <- bptest(log_model)
print(bp_test_log)

shapiro_test_log <- shapiro.test(log_residuals)
print(shapiro_test_log)

#Residual plots for the new model
plot(fitted(log_model), log_residuals, main = "Residuals vs Fitted Values (Log Model)", xlab = "Fitted Values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red")
qqnorm(log_residuals)
qqline(log_residuals, col = "red")
hist(log_residuals, breaks = 30, main = "Histogram of Residuals (Log Model)")
boxplot(log_residuals, main = "Boxplot of Residuals (Log Model)")



# New data
df_new <- read.csv(file.path("data", "data_new.csv"))
df_new
reg <- backward_stepwise
summary(reg)
confint(reg)

df_new$FertCat <- 0
df_new$FertCat[df_new$AdolFertRate > 50 & df_new$AdolFertRate <= 100] <- 2
df_new$FertCat[df_new$AdolFertRate > 0 & df_new$AdolFertRate <= 50] <- 1
df_new$FertCat[df_new$AdolFertRate > 100] <- 3

#Point Forecasts for CorControl
predictions <- predict(backward_stepwise, newdata = df_new)
print("Predictions:")
print(predictions)

conf_intervals <- predict(backward_stepwise, newdata = df_new, interval = "confidence", level = 0.95)
pred_intervals <- predict(backward_stepwise, newdata = df_new, interval = "prediction", level = 0.95)

print("Confidence Intervals:")
print(conf_intervals)
print("Prediction Intervals:")
print(pred_intervals)


#---------------ANOVA-----------------------#
install.packages("car","ggplot2")
library(car)
library(ggplot2)

anova_model <- aov(CorControl ~ factor(FertCat) * factor(InflCat), data = df)
summary(anova_model)

# Check the assumptions
# 1. Independence is assumed if the data collection is random.
# 2. Normality of residuals
qqnorm(resid(anova_model))
qqline(resid(anova_model))
shapiro_test <- shapiro.test(resid(anova_model))
print(shapiro_test)

plot(anova_model, which = 1)
levene_test <- leveneTest(resid(anova_model) ~ factor(FertCat) * factor(InflCat), data = df)
print(levene_test)

hist(resid(anova_model), main = "Histogram of Residuals", xlab = "Residuals")
boxplot(resid(anova_model), main = "Boxplot of Residuals")

anova_table <- summary(anova_model)
print(anova_table)

# Post-hoc analysis if there are significant effects
TukeyHSD(anova_model)

# Perform Tukey's HSD test for FertCat
tukey_FertCat <- TukeyHSD(aov(CorControl ~ factor(FertCat), data = df))
print(tukey_FertCat)

# Perform Tukey's HSD test for InflCat
tukey_InflCat <- TukeyHSD(aov(CorControl ~ factor(InflCat), data = df))
print(tukey_InflCat)


#Countries with the highest CorControl values
install.packages("dplyr")
library(dplyr)

top_countries <- df %>%
  arrange(desc(CorControl)) %>%
  head(10)
print(top_countries)


