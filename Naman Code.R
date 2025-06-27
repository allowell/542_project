
#load in libraries
library(ggplot2)

#scatterplot of total p and height
ggplot(hyp1, aes(x = totp, y = ht, color = factor(estabp > 0))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ stdy) +
  labs(color = "Establishment P Added")

#Gamma log link model
model_gamma <- glm(ht ~ totp * estabp, data = hyp1,family = Gamma(link = "log"))
summary(model_gamma)
plot(model_gamma)

#comparing AIC 
model_ols <- lm(ht ~ totp * estabp, data = hyp1)
AIC(model_ols, model_gamma)



