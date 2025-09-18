library(dplyr)
library(MASS)
library(lmtest)
library(car)
library(sandwich)

df<-read.csv("Students Social Media Addiction.csv")
summary(df)
dim(df)
colnames(df)
colMeans(is.na(df))
numerical_vars <- c("Age", "Avg_Daily_Usage_Hours","Sleep_Hours_Per_Night", 
                      "Conflicts_Over_Social_Media")
par(mfrow = c(2,2))  
boxplot_list <- list()

for (var in numerical_vars) {
  boxplot_list[[var]] <- boxplot(df[[var]], 
                                 main = paste("Boxplot of", var), 
                                 col = "pink", 
                                 cex.axis = 0.8, 
                                 cex.lab = 0.8)
}

ll <- boxplot(df$Avg_Daily_Usage_Hours)
outlier_indices1 <- which(df$Avg_Daily_Usage_Hours %in% ll$out)
df_new <- df[-outlier_indices1, ]
dim(df_new)

par(mfrow = c(2, 2))  
boxplot_list <- list()

for (var in numerical_vars) {
  boxplot_list[[var]] <- boxplot(df_new[[var]], 
                                 main = paste("Boxplot of", var), 
                                 col = "#0E2148", 
                                 cex.axis = 0.8, 
                                 cex.lab = 0.8)
}
summary(df_new[numerical_vars])

desc_stats <- df_new %>%
  summarise(across(all_of(numerical_vars), 
                   list(mean = ~mean(., na.rm = TRUE),
                        median = ~median(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE),
                        q1 = ~quantile(., 0.25, na.rm = TRUE),
                        q3 = ~quantile(., 0.75, na.rm = TRUE))))

print("Descriptive Statistics for Numerical Variables:")
print(desc_stats)

categorical_vars <- c("Gender", "Academic_Level", "Country", "Most_Used_Platform", 
                      "Affects_Academic_Performance", "Relationship_Status", 
                      "Addicted_Score", "Mental_Health_Score")

for (var in categorical_vars) {
cat("\n============================\n")
cat("Frequency Table for:", var, "\n")
cat("============================\n")
freq_table <- table(df_new[[var]])
print(freq_table)
cat("\nProportions (%):\n")
print(round(prop.table(freq_table) * 100, 2))
}

boxplot(df_new$Avg_Daily_Usage_Hours, main = "Without Outliers", col = "lightblue")
str(df)
df_new$Mental_Health_Score<-as.factor(df_new$Mental_Health_Score)
library(vcd)
df_new$Gender <- as.factor(df_new$Gender)
df_new$Relationship_Status <- as.factor(df_new$Relationship_Status)
df_new$Addicted_Score <- as.factor(df_new$Addicted_Score)
tab <- table(df_new$Gender, df_new$Relationship_Status, df_new$Addicted_Score)
categorical_vars_mosic <- c("Gender", 
                       "Relationship_Status", 
                      "Addicted_Score")

tab <- table(df_new[, categorical_vars_mosic])

colnames(df)
mosaic(tab, shade = TRUE)

####interaction
colnames(df)
model_without_interaction<-lm(Avg_Daily_Usage_Hours~Mental_Health_Score
                              +Relationship_Status+Gender+Conflicts_Over_Social_Media
                              ,data=df_new)
model_with_interaction<-lm(Avg_Daily_Usage_Hours~Mental_Health_Score*Conflicts_Over_Social_Media
                           +Relationship_Status+Gender,data=df_new)

df_new$Mental_Health_Score<-as.factor(df_new$Mental_Health_Score)
bc_result<-boxcox(model_without_interaction, lambda = seq(-2, 2, 0.1))  # You can adjust the lambda range
optimal_lambda <- bc_result$x[which.max(bc_result$y)]
df_new$Avg_Daily_Usage_Hours_transformed <- (df_new$Avg_Daily_Usage_Hours^optimal_lambda - 1) / optimal_lambda
model_without_interaction_trans<-lm(Avg_Daily_Usage_Hours_transformed~Mental_Health_Score
                              +Relationship_Status+Gender+Conflicts_Over_Social_Media
                              ,data=df_new)



model_with_interaction2<-lm(Avg_Daily_Usage_Hours~Mental_Health_Score+Conflicts_Over_Social_Media
                           +Relationship_Status*Gender,data=df_new)

par(mfrow = c(2, 2))
plot(model_without_interaction)
summary(model_without_interaction)
par(mfrow = c(2, 2))
summary(model_with_interaction)

shapiro.test(residuals(model_without_interaction)) # Normality
bptest(model_without_interaction) # Homoscedasticity
vif(model_without_interaction) # Multicollinearity 

shapiro.test(residuals(model_with_interaction)) # Normality
bptest(model_with_interaction) # Homoscedasticity
vif(model_with_interaction) # Multicollinearity 


colnames(df)
# Fit robust model (rlm handles non-normality and heteroscedasticity better)
robust_model <- rlm((Avg_Daily_Usage_Hours) ~ Mental_Health_Score * Conflicts_Over_Social_Media + 
                      Relationship_Status+Gender, data = df_new)
summary(robust_model)
par(mfrow = c(2, 2))
plot(robust_model)
shapiro.test(residuals(robust_model)) # Normality
bptest(robust_model) # Homoscedasticity
vif(robust_model) # Multicollinearity 
summary(robust_model)

# Exclude 477 if influential
df_new_clean <- df_new[-477, ]
model_cleanmm <- lm(Avg_Daily_Usage_Hours ~ Mental_Health_Score * Conflicts_Over_Social_Media + 
                            Relationship_Status + Gender, data = df_new_clean)
summary(model_cleanmm)
shapiro.test(residuals(model_cleanmm))
bptest(model_cleanmm)
vif(model_cleanmm)
par(mfrow = c(2, 2))
plot(model_cleanmm)


# Compute standardized residuals
# Identify outliers

cooks_d <- cooks.distance(model_without_interaction)
std_resid <- rstandard(model_without_interaction)
outlier_indices <- which(cooks_d > 0.5 | abs(std_resid) > 3)
df_new_cleaner1 <- df_new[-outlier_indices, ]# Compute Cook's distances

print(outlier_indices)  # Indices of observations to remove
model_clean_with_interaction <- lm(Avg_Daily_Usage_Hours ~ Mental_Health_Score *Conflicts_Over_Social_Media  
                   +  Gender, data = df_new_cleaner1)
model_clean0<- lm(Avg_Daily_Usage_Hours ~   Mental_Health_Score + Conflicts_Over_Social_Media + 
                    Gender, data = df_new_cleaner1)
summary(model_clean_with_interaction)
summary(model_clean0)

par(mfrow = c(2, 2))
plot(model_clean0)
par(mfrow = c(2, 2))
plot(model_clean_with_interaction)

cooks_dtwo <- cooks.distance(model_clean0)
std_residtwo <- rstandard(model_clean0)
outlier_indicestwo <- which(cooks_dtwo > 0.5 | abs(std_residtwo) > 3)
df_new_cleaner2 <- df_new_cleaner1[-outlier_indicestwo, ]# Compute Cook's distances


model_clean02<- lm(Avg_Daily_Usage_Hours ~ Mental_Health_Score+Conflicts_Over_Social_Media + 
                    Gender , data = df_new_cleaner2)

model_clean_with_interaction12 <- lm(Avg_Daily_Usage_Hours ~ Mental_Health_Score*Conflicts_Over_Social_Media + 
                                     Gender , data = df_new_cleaner2)


summary(model_clean_with_interaction)
summary(model_clean02)

par(mfrow = c(2, 2))
plot(model_clean0)

dim(df_new_cleaner2)
dim(df_new_cleaner1)
dim(df_new)
library(sandwich)
library(lmtest)


coeftest(model_clean0, vcov = vcovHC(model_clean0, type = "HC1")) #to fix hetero problem however infernce dosent change 
###########nested
df_new$Addicted_Score<-as.factor(df_new$Addicted_Score)
colnames(df)
model_nested1<-lm(Sleep_Hours_Per_Night ~ Addicted_Score + 
                    Academic_Level+Age , data = df_new)
model_nested2<-lm(Sleep_Hours_Per_Night ~ Addicted_Score + Mental_Health_Score+
                    Academic_Level+Age+Relationship_Status , data = df_new)
summary(model_nested1)
summary(model_nested2)
anova(model_nested1,model_nested2)
AIC(model_nested1,model_nested2)
BIC(model_nested1,model_nested2)
shapiro.test(residuals(model_nested1))
bptest(model_nested2)
shapiro.test(residuals(model_nested2))
vif(model_nested2)
vif(model_nested1)

par(mfrow = c(2, 2))
plot(model_nested2)

par(mfrow = c(2, 2))
plot(model_nested1)

cooks_d1 <- cooks.distance(model_nested2)
std_resid1 <- rstandard(model_nested2)
outlier_indices2 <- which(cooks_d1 > 0.5 | abs(std_resid1) > 3)
print(outlier_indices2)  # Indices of observations to remove
df_new_cleaner12 <- df_new[-outlier_indices2, ]# Compute Cook's distances

model_nested12<-lm(Sleep_Hours_Per_Night ~ Addicted_Score + 
                    Academic_Level+Age , data = df_new_cleaner12)
model_nested22<-lm(Sleep_Hours_Per_Night~ Addicted_Score + Mental_Health_Score+
                    Academic_Level+Age+Relationship_Status , data = df_new_cleaner12)
shapiro.test(residuals(model_nested22))
shapiro.test(residuals(model_nested12))

anova(model_nested12,model_nested22)
AIC(model_nested12,model_nested22)
BIC(model_nested12,model_nested22)
summary(model_nested12)
summary(model_nested22)
par(mfrow = c(2, 2))
plot(model_nested22)
cooks_d12 <- cooks.distance(model_nested22)
influential_points <- which(cooks_d12 > 4/length(cooks_d12))
influential_points

dfbetas_values <- dfbetas(model_nested12)
outlier_indices22 <- influential_points
print(outlier_indices22)  


#######################picewise
library(segmented)
colnames(df_new)
model_without_pi<-lm(Sleep_Hours_Per_Night~Avg_Daily_Usage_Hours+Gender+Age+Relationship_Status,data=df_new)
model_without_pi<-lm((Sleep_Hours_Per_Night)~log(Avg_Daily_Usage_Hours)+Gender+Age+Relationship_Status,data=df_new)
segmented_model <- segmented(model_without_pi, seg.Z = ~Age, psi = 20)
summary(segmented_model)
summary(model_without_pi)


usage_break_point<- 3
df_new$usage_before_break<-ifelse(df_new$Avg_Daily_Usage_Hours<= usage_break_point,df_new$Avg_Daily_Usage_Hours,0)
df_new$usage_after_break<-ifelse(df_new$Avg_Daily_Usage_Hours> usage_break_point,df_new$Avg_Daily_Usage_Hours-usage_break_point,0)
model_with_pi<-lm(Sleep_Hours_Per_Night~usage_after_break+usage_before_break+Gender+Age+Relationship_Status,data=df_new)
summary(model_with_pi)
par(mfrow=c(2,2))
plot(model_with_pi)


age_break<-20
df_new$Age_before<-ifelse(df_new$Age<= age_break,df_new$Age,0)
df_new$Age_after<-ifelse(df_new$Age > age_break,df_new$Age-age_break,0)
model_without_pi2<-lm(Sleep_Hours_Per_Night~Gender+Age_after+Age_before+Relationship_Status,data=df_new)
summary(model_without_pi2)

conflict_break<-1
df_new$conflict_before<-ifelse(df_new$Conflicts_Over_Social_Media<= conflict_break,df_new$Conflicts_Over_Social_Media,0)
df_new$conflict_after<-ifelse(df_new$Conflicts_Over_Social_Media > conflict_break,df_new$Conflicts_Over_Social_Media-conflict_break,0)
df_new$Mental_Health_Score<-as.factor(df_new$Mental_Health_Score)
model_without_pi3<-lm(sqrt(Avg_Daily_Usage_Hours)~Academic_Level+Conflicts_Over_Social_Media+Mental_Health_Score,data=df_new)
model_with_pi3<-lm(sqrt(Avg_Daily_Usage_Hours)~Academic_Level+conflict_before+conflict_after+Mental_Health_Score,data=df_new)
summary(model_with_pi3)
colnames(df)
summary(model_without_pi3)

anova(model_with_pi3,model_without_pi3)
AIC(model_with_pi3,model_without_pi3)
par(mfrow=c(2,2))
plot(model_without_pi3)
plot(model_with_pi3)
shapiro.test(model_without_pi3$residuals)
cooks_d1233mm <- cooks.distance(model_without_pi3)
std_residllmm <- rstandard(model_without_pi3)
outlier_indicesll <- which(cooks_d1233mm > 0.5 | abs(std_residllmm) > 3)
outlier_indicesll

anova(model_with_pi3,model_without_pi3)
AIC(model_with_pi,model_without_pi)
par(mfrow=c(2,2))
plot(model_without_pi)


cooks_d1233mmll <- cooks.distance(model_with_pi3)
std_residllmmll<- rstandard(model_without_pi)
outlier_indicesll <- which(cooks_d1233mm > 0.5 | abs(std_residllmm) > 3)
outlier_indicesll


influential_points22 <- which(cooks_d1233mm> 4/length(cooks_d1233mm))
influential_points22
df_new_cleanermm <- df_new[-influential_points22, ]# Compute Cook's distances
model_without_pi3mm<-lm(sqrt(Avg_Daily_Usage_Hours)~Academic_Level+Conflicts_Over_Social_Media+Mental_Health_Score,data=df_new_cleanermm)
par(mfrow=c(2,2))
plot(model_without_pi)

