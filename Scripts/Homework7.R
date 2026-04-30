# Question One
library(faraway)
data(prostate)

lm_prostate <- lm(lpsa ~ ., data=prostate)
summary(lm_prostate)

# Backward Elimination
drop1(lm_prostate, test='F')
lm_prostate1 <- update(lm_prostate, . ~ . - gleason)
drop1(lm_prostate1, test='F')
lm_prostate2 <- update(lm_prostate1, . ~ . - lcp)
drop1(lm_prostate2, test='F')
lm_prostate3 <- update(lm_prostate2, . ~ . - pgg45)
drop1(lm_prostate3, test='F')
lm_prostate4 <- update(lm_prostate3, . ~ . - age)
drop1(lm_prostate4, test='F')
lm_prostate5 <- update(lm_prostate4, . ~ . - lbph)
drop1(lm_prostate5, test='F')

# AIC
prostate_null <- lm(lpsa ~ 1, data=prostate)
prostate_scope <- reformulate(colnames(prostate)[1:8])
prostate_aic <- step(prostate_null, scope=prostate_scope, direction='forward', trace=0)
summary(prostate_aic)

# Adjusted R^2
library(leaps)
prostate_subsets <- regsubsets(lpsa ~ ., data=data.frame(prostate))
rs <- summary(prostate_subsets)
data.frame(rs$outmat, rs$adjr2)

# Mallow's Cp
plot(1:8, rs$cp, ylim = c(0,26), xlim = c(1,8))
abline(0,1)


# Question Two
data(teengamb)
head(teengamb)
lm_gamble <- lm(gamble ~ ., data=teengamb)
summary(lm_gamble)

# Backward Elimination
drop1(lm_gamble, test='F')
lm_gamble1 <- update(lm_gamble, . ~ . - status)
drop1(lm_gamble1, test='F')
lm_gamble2 <- update(lm_gamble1, . ~ . - verbal)
drop1(lm_gamble2, test='F')

# AIC
gamble_null <- lm(gamble ~ 1, data=teengamb)
gamble_scope <- reformulate(colnames(teengamb)[1:4])
gamble_aic <- step(gamble_null, scope=gamble_scope, direction='forward', trace=0)
summary(gamble_aic)

# Adjusted R^2
gamble_subsets <- regsubsets(gamble ~ ., data=data.frame(teengamb))
rs_gamble <- summary(gamble_subsets)
data.frame(rs_gamble$outmat, rs_gamble$adjr2)

# Mallows Cp
plot(1:4, rs_gamble$cp, ylim=c(0,12))
abline(0,1)


# Question 3
data(trees)
head(trees)
lm_trees <- lm(log(Volume) ~ Height * Girth, data=trees)
summary(lm_trees)


# Question 4
data(gala)
head(gala)

lm_gala <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(lm_gala)

# 4B
data(galamiss)
head(galamiss)

lm_galamiss <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=galamiss)
summary(lm_galamiss)

# 4C
lm_elevation <- lm(Elevation ~ Area + Nearest + Scruz + Adjacent, data=galamiss)
summary(lm_elevation)

pred_vals <- predict(lm_elevation, galamiss[is.na(galamiss$Elevation), ])

galamiss_imputed <- data.frame(galamiss)
galamiss_imputed$Elevation[is.na(galamiss$Elevation)] <- pred_vals

lm_galamiss2 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=galamiss_imputed)
summary(lm_galamiss2)


# Question Five
library(readr)
eagle_data <- read.csv(here::here("Data", "lead_eagles_iowa.csv"))
head(eagle_data)
xtabs( ~ Lead_level_code + Released, data=eagle_data)

# Conditional Probability: Released on Background
54/(54+80)

# Conditional Probability: Released on Sub-Clinical
10/(27+10)

# Conditional Probability: Released on Clinical
5/(94+5)

# Logistic Regression
eagle_glm <- glm((Released=="R")*1 ~ Lead_level_code, data=eagle_data, family = 'binomial')
summary(eagle_glm)

# Interpretation
# Background 
exp(-0.3930)/(1+exp(-0.3930))

#Sub-Clinical
exp(-0.3930 - 0.6002)/(1+exp(-0.3930 - 0.6002))

# Clinical
exp(-0.3930 - 2.5408)/(1+exp(-0.3930 - 2.5408))

