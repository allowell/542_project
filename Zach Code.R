
#install.packages("readxl")
library(readxl)
library(ggplot2)

#Read in the data for hypothesis 1 and select appropriate columns
hyp1 <- read_excel("RW28 data for Prelims.xlsx", sheet = "plot data")
hyp1<- hyp1|>
  select(stdy, plot, yst, cump, totp, dbh, ht, vol)|>
  mutate(estabp=totp-cump, TRT_CODE=substr(plot,2,4))

#Running a OLS Model for Study=280601
model_ols <- lm(ht ~ totp + estabp + totp:estabp, data = hyp1[(hyp1$stdy==280601),])
summary(model_ols)
#Checking diagnostics
par(mfrow=c(2,2))
plot(model_ols)  # Residuals vs Fitted, QQ plot, etc.

#Running a OLS Model for Study=282201
model_ols <- lm(ht ~ totp + estabp + totp:estabp + yst, data = hyp1[(hyp1$stdy==282201),])
summary(model_ols)
#Checking diagnostics
par(mfrow=c(2,2))
plot(model_ols)  # Residuals vs Fitted, QQ plot, etc.

#Running a OLS Model for Study=281303
model_ols <- lm(ht ~ totp + estabp + totp:estabp + yst, data = hyp1[(hyp1$stdy==281303),])
summary(model_ols)
#Checking diagnostics
par(mfrow=c(2,2))
plot(model_ols)  # Residuals vs Fitted, QQ plot, etc.

#Running a OLS Model for Study=282401
model_ols <- lm(ht ~ totp + estabp + totp:estabp + yst, data = hyp1[(hyp1$stdy==282401),])
summary(model_ols)
#Checking diagnostics
par(mfrow=c(2,2))
plot(model_ols)  # Residuals vs Fitted, QQ plot, etc.

#Running a OLS Model for Study=284201
model_ols <- lm(ht ~ totp + estabp + totp:estabp + yst, data = hyp1[(hyp1$stdy==284201),])
summary(model_ols)
#Checking diagnostics
par(mfrow=c(2,2))
plot(model_ols)  # Residuals vs Fitted, QQ plot, etc.
