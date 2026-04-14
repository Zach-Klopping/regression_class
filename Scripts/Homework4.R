# Question One
library(faraway)
data(sat)
head(sat)
lm_sat <- lm(total ~ salary, data=sat)
summary(lm_sat)

lm1_sat <- lm(total ~ expend + ratio + salary, data=sat)
summary(lm1_sat)

lm2_sat <- lm(total ~ expend + ratio + salary + takers, data=sat)
summary(lm2_sat)
anova(lm1_sat, lm2_sat)

# Question Two
data(prostate)
head(prostate)
names(prostate)
lm_pro <- lm(lpsa ~ lcavol + lweight + age + lbph 
           + svi + lcp + gleason + pgg45, data = prostate)
plot(fitted(lm_pro), residuals(lm_pro))

qqnorm(residuals(lm_pro))
qqline(residuals(lm_pro))
hist(residuals(lm_pro))

# Question Three 
data(swiss)
head(swiss)
names(swiss)
lm_swiss <- lm(Fertility ~ Agriculture + Examination + Education 
               + Catholic + Infant.Mortality, data = swiss)

plot(fitted(lm_swiss), residuals(lm_swiss))

qqnorm(residuals(lm_swiss))
qqline(residuals(lm_swiss))
hist(residuals(lm_swiss))

# Question Four
data(prostate)
lm_pro <- lm(lpsa ~ lcavol + lweight + age + lbph 
             + svi + lcp + gleason + pgg45, data = prostate)

newdata <- data.frame(lcavol=1.44692, lweight=3.62301,
                      age=65, lbph=0.3001, svi=0,
                      lcp=-0.79851, gleason=7, pgg45=15)

predict(lm_pro, newdata, interval = 'prediction', level = 0.95)

newdata1 <- data.frame(lcavol=1.44692, lweight=3.62301,
                      age=20, lbph=0.3001, svi=0,
                      lcp=-0.79851, gleason=7, pgg45=15)

predict(lm_pro, newdata1, interval = 'prediction', level = 0.95)

summary(lm_pro)

lm_pro_sig <- lm(lpsa ~ lcavol + lweight + svi, data = prostate)
summary(lm_pro_sig)
predict(lm_pro_sig, newdata, interval = 'prediction', level = 0.95)
predict(lm_pro_sig, newdata1, interval = 'prediction', level = 0.95)

# Question 5
data(snail)
head(snail)
lm_snail <- lm(water ~ temp + humid, data = snail)

new_snail_data <- data.frame(temp = 25, humid = 60)
predict(lm_snail, new_snail_data, interval = 'prediction', level = 0.95)

new_snail_data1 <- data.frame(temp = 30, humid = 75)
predict(lm_snail, new_snail_data1, interval = 'prediction', level = 0.95)

# Question 6
data(odor)
head(odor)
lm_odor_temp <- lm(odor ~ temp, data = odor)
summary(lm_odor_temp)

lm_odor_full <- lm(odor ~ temp + gas + pack, data = odor)
summary(lm_odor_full)

lm_odor_temp_gas <- lm(odor ~ temp + gas, data = odor)
summary(lm_odor_temp_gas)

lm_odor_temp_pack <- lm(odor ~ temp + pack, data = odor)
summary(lm_odor_temp_pack)


# Question 7
data(teengamb)
head(teengamb)

lm_gamble <- lm(gamble ~ sex + status + income + verbal, data = teengamb)
summary(lm_gamble)

lm_gamble2 <- lm(gamble ~ sex + income + income:sex, data = teengamb)
summary(lm_gamble2)







