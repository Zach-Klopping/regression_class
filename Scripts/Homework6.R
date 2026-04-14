#' -------------
# Question One
#'------------- 
library(faraway)
library(car)
data(cheddar)
lm_cheddar <- lm(taste ~ Acetic + H2S + Lactic, data=cheddar)

# Partial (Component) Residual Plot
crPlots(lm_cheddar)

# Added Variable Plot
avPlots(lm_cheddar)

#'---------------
# Question Two
#'--------------
data(tvdoctor)
lm_doctor <- lm(life ~ tv + doctor, data=tvdoctor)

# Assessing Assumptions
plot(lm_doctor)

# Assessing Linearity
crPlots(lm_doctor)

# Assessing Influential Points
avPlots(lm_doctor)

#'-----------------
# Question Three
#'-----------------
data(prostate)
lm_prostate <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + 
                    gleason + pgg45, data=prostate)

# Correlation Matrix (for predictors)
cor(prostate)

# Variance Inflation Factor (assesses multicollinearity)
vif(lm_prostate)

#'-----------------
# Question Four
#'-----------------
library(MASS)
data(cornnit)

lm_corn <- lm(yield ~ nitrogen, data=cornnit)

box_cox_transformation <- boxcox(lm_corn, lambda = seq(0, 6))

print(box_cox_transformation$x[which.max(box_cox_transformation$y)])

lm_corn2 <- lm(I(yield^3) ~ nitrogen, data=cornnit)
anova(lm_corn2)









