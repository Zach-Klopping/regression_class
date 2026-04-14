library(faraway)
library(ggplot2)

# Question One
data(fortune)
head(fortune)

ggplot(data=fortune) +
  geom_point(aes(x=age, y=wealth, shape=region))

lm_wealth <- lm(I(wealth^1) ~ age + region, data=fortune)
summary(lm_wealth)

lm_wealth_transformed <- lm(I(wealth^-1) ~ age + region, data=fortune)
summary(lm_wealth_transformed)

# Untransformed Model
plot(fitted(lm_wealth), residuals(lm_wealth), 
     main='Untransformed Residual Plot')
qqnorm(residuals(lm_wealth), main='Untransformed QQ Plot')
qqline(residuals(lm_wealth))

# Transformed Model
plot(fitted(lm_wealth_transformed), residuals(lm_wealth_transformed), 
     main='Transformed Residual Plot')
qqnorm(residuals(lm_wealth_transformed), main='Transformed QQ Plot')
qqline(residuals(lm_wealth_transformed))

# Question Two
data(pulp)
head(pulp)
lm_pulp <- lm(bright ~ operator, data=pulp)
summary(lm_pulp)
anova(lm_pulp)

pairwise.t.test(pulp$bright, pulp$operator, p.adjust.method='none')
pairwise.t.test(pulp$bright, pulp$operator, p.adjust.method='bonferroni')
pairwise.t.test(pulp$bright, pulp$operator, p.adjust.method='fdr')

# Question Three
data(butterfat)
head(butterfat)

mature <- subset(butterfat, Age == 'Mature')
lm_butterfat <- lm(Butterfat ~ Breed, data=mature)
anova(lm_butterfat)

# Residuals
plot(fitted(lm_butterfat), residuals(lm_butterfat), 
     main='Butterfat Residual Plot')
qqnorm(residuals(lm_butterfat), main='Butterfat QQ Plot')
qqline(residuals(lm_butterfat))

# Influential Points
sum(hatvalues(lm_butterfat) > 2*5/50)
boxplot(Butterfat ~ Breed, data=mature)
pairwise.t.test(mature$Butterfat, mature$Breed, p.adjust.method='bonferroni')

# Question Four
data(prostate)
head(prostate)

lm_prostate <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + 
                    gleason + pgg45, data=prostate)

# Checking Leverage Points
sum(hatvalues(lm_prostate) > 2*9/97)

# Checking Outliers 
rstud_pvals <- 2*pt(-abs(rstudent(lm_prostate)), 87)
sum(p.adjust(rstud_pvals, "bonferroni") < 0.05)

# Uncorrected Test of Outliers
sum(abs(rstudent(lm_prostate)) > 3)

# Checking Influential Points
sum(cooks.distance(lm_prostate) > 4/97)

# Plots
plot(fitted(lm_prostate), rstudent(lm_prostate), main='Studentized Residuals')
halfnorm(hatvalues(lm_prostate), main='Leverage Points', nlab=0)
halfnorm(cooks.distance(lm_prostate), main='Cooks Distance', nlab=0)
plot(lm_prostate)


