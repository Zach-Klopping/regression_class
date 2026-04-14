le_gdp <- read.csv('lifeexp_gdp.csv')
row.names(1)
head(le_gdp)
nrow(le_gdp)
plot(le_gdp$gdp, le_gdp$life_exp, xlab="log(GDP)", ylab="Life Expectancy")
log_gdp <- log(le_gdp$gdp)
plot(log_gdp, le_gdp$life_exp, xlab="log(GDP)", ylab="Life Expectancy")
m1 <- lm(le_gdp$life_exp ~ log_gdp)
summary(m1)
anova(m1)

library(faraway)
df <- teengamb
head(df)
mod1 <- lm(gamble ~ sex + status + income + verbal, data=df)
summary(mod1)

