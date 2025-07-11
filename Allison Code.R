
#Mixed effects model
#library(lme4)
#library(Matrix)
library(nlme)
#Deciding to treat estabp as a binary for right now to look at comparison
#between sites with no carryover versus any carryover

#Data in factors for mixed effects model
df <- hyp1 |>
  mutate(
    stdy = factor(stdy),
    plot = factor(plot),
    TRT_CODE = factor(TRT_CODE),
    yst = factor(yst),
    estabp_bin = factor(ifelse(estabp > 0, 1, 0)),
    totp_sc = scale(totp)
  )


#First model
#Response: height, random intercepts for site and plot
m_mod1 <- lmer(ht ~ totp_sc * estabp_bin + (1 | stdy/plot), data = df)

#Second model
hyp1_complete <- na.omit(hyp1[, c("ht", "yst", "cump", "estabp", "stdy", "plot")])
model <- lme(ht ~ yst + cump + estabp, 
             random = ~ 1 | stdy, data = hyp1_complete)
summary(model)
ranef(model)

#See some significant intercept differences between studies

