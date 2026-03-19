library(faraway)
# Problem One
md23 <- read.csv("minmax_dec2023.csv", row.names=1)
mod <- lm(TMAX ~ TMIN, data=md23)
sumary(mod)
residuals <- resid(mod)
plot(md23$TMIN, residuals)
abline(h=0)

# Problem Two
nba_total <- read.csv("nbatotal_2022_23.csv")
head(nba_total)
nba_total2 <-nba_total[nba_total$MP >= 96,]
nrow(nba_total) - nrow(nba_total2)
plot(nba_total2$AST, nba_total2$TOV)
mod <- lm(TOV ~ AST, data=nba_total2)
sumary(mod)
mod1 <- lm(TOV ~ AST + MP + FGA, data=nba_total2)
sumary(mod1)
mod1 <- lm(TOV ~ AST + FGA, data=nba_total2)
summary(mod1)

# Problem Three
data(cheddar)
head(cheddar)
mod <- lm (taste ~ Acetic + H2S + Lactic, data=cheddar)
sumary(mod)
r2 <- cor(fitted(mod), cheddar$taste)**2
r2



