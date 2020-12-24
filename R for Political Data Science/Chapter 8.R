#Chapter 8 Logistic Models ####

library(tidyverse)
library(politicalds)
library(ggcorrplot)
library(margins)
library(prediction)
library(texreg)
library(jtools)
library(skimr)
library(pscl)
library(DescTools)
library(broom)
library(plotROC)
library(separationplot)
library(ggstance)
library(broom.mixed)

#8.3    How are probabilities estimated? ####

data("dem_breakdown")

ls()

dem_breakdown %>% 
  filter(breakdown == 1) %>% 
  count(country_name)

model_1 <- glm(breakdown ~ pres_power_index,
               data = dem_breakdown,
               family = binomial("logit")) #we use the logistic link function

summary(model_1)
  
dem_breakdown %>% 
  select(country_name, year, pres_power_index) %>% 
  arrange(-pres_power_index) %>% 
  slice(1)

ggplot(dem_breakdown, aes(x = pres_power_index)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Presidential Power Index", y = "Frequency")

#8.4    Model estimation ####

ggplot(dem_breakdown, aes(x = regime_age)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Age of the political regime", y = "Frequency")

model_2 <- glm(breakdown ~ pres_power_index + regime_age,
               data = dem_breakdown,
               family = binomial("logit"))

summary(model_2)
  
ggplot(dem_breakdown, aes(x = dem_index)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Freedom House Index", y = "Frequency")

corr_selected <- dem_breakdown %>% 
  select(regime_age, dem_index, pres_power_index) %>% 
  #calculate correlation matrix and round to 1 decimal places:
  cor(use = "pairwise") %>% 
  round(1)

ggcorrplot(corr_selected, type = "lower", lab = T, show.legend = F)

model_3 <- glm(breakdown ~ pres_power_index + regime_age + dem_index,
               data = dem_breakdown,
               family = binomial("logit"))

summary(model_3)

#8.5   Creating tables ####

mp_models <- list(model_1, model_2, model_3)

htmlreg(mp_models,
        custom.model.names = c("Model 1","Model 2", "Model 3"),
        custom.coef.names = c("Intercept", "Shugart & Carey Index",
                              "Regime age", "Freedom House Index"),
        file = "table_1.html")
  
screenreg(mp_models,
          custom.model.names = c("Model 1","Model 2", "Model 3"),
          custom.coef.names = c("Intercept", "Shugart & Carey Index",
                                "Regime age", "Freedom House Index"))

skim(dem_breakdown)

screenreg(model_3,
          custom.model.names = "Model 3 - Odds Ratios",
          override.coef = exp(coef(model_3)),
          #the following function, odds_*, are in the package of the book
          override.se = odds_se(model_3),
          override.pvalues = odds_pvalues(model_3),
          #also, we will omit the coefficient of the intercept
          omit.coef = "Inter")

#8.6   Visual representation of results ####

predict_model_2 <- prediction(
  model_2,
  at = list(pres_power_index = unique(model.frame(model_2)$pres_power_index))
)

summary(predict_model_2)

figure_op_1 <- ggplot(summary(predict_model_2),
                      aes(x = `at(pres_power_index)`, y = Prediction,
                      ymin = lower, ymax = upper,
                      group = 1)) +
  geom_line() +
  geom_errorbar(width = 0.2) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Shugart index",
       y = "Pred. probability of Democratic Breakdown")

figure_op_1

cdat <- cplot(model_2, "pres_power_index", what = "prediction",
              main = "Pr(Breakdown)", draw = F)
  
ggplot(cdat, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(x = "Shugart index",
       y = "Pred. probability of Democratic Breakdown")

marginal_ef <- margins(model_2) %>% 
  summary() %>% 
  as_tibble()

ggplot(marginal_ef, aes(x = factor, y = AME, ymax = upper, ymin = lower)) +
  geom_point() +
  geom_linerange() +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Age of regime", "Shugart index")) +
  labs(x = "")

marginal_pres_power_index <- cplot(
  model_2, "pres_power_index",
  what = "effect", main = "ME(Breakdown)", draw = F
)

ggplot(marginal_pres_power_index, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(x = "Shugart index", y = "Marginal effect")

plot_summs(model_1, model_2, model_3,
           exp = T, scale = F, inner_ci_level = .9,
           coefs = c("Shugart index" = "pres_power_index",
                     "Age of regime" = "regime_age",
                     "Freedom House index" = "dem_index"),
           model.names = c("Model 1", "Model 2", "Model 3")) +
  labs(x = "Exponentiated coefficients", y = "")

plot_summs(model_1, model_2, model_3,
           inner_ci_level = .9,
           coefs = c("Shugart index" = "pres_power_index",
                     "Age of regime" = "regime_age",
                     "Freedom House index" = "dem_index"),
           model.names = c("Model 1", "Model 2", "Model 3"),
           colors = c("black", "darkgray", "gray")) +
  labs(x = "Exponentiated coefficients", y = "")

ggplot(cdat, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  labs(x = "Freedom House index",
       y = "Pred. probability of Democratic Breakdown")

model_4 <- glm(breakdown ~ gini, data = dem_breakdown,
               family = binomial("logit"))

summary(model_4)

model_5 <- glm(breakdown ~ growth_10y * gini + x_oil_min + pres_power_index,
               data = dem_breakdown,
               family = binomial("logit"))

summary(model_5)

persp(model_5, "growth_10y", "gini", what = "prediction",
      xlab = "GDP growth in last 10 years",
      ylab = "Gini coefficient",
      zlab = "Predicted probability"
      #the textbook's code includes the below line, which cannot be displayed on my computer due to an error "font family not found in Windows font database"
      #family = "Latin Modern Roman"
      )

image(model_5, xvar = "growth_10y", yvar = "gini",
      xlab = "GDP growth in the last 10 years", ylab = "Gini coefficient")

#8.7   Measures to evaluate the ﬁt of the models ####

#8.7.0.1     Pseudo-R2 

pR2(model_1)[["McFadden"]]

pR2(model_2)[["McFadden"]]

pR2(model_3)[["McFadden"]]

PseudoR2(model_1, "McFadden")

PseudoR2(model_2, "McFadden")

PseudoR2(model_3, "McFadden")
  
#8.7.0.2    AIC 

AIC(model_1)

AIC(model_2)

AIC(model_3)

#8.7.0.3    BIC 

BIC(model_1)

BIC(model_2)

BIC(model_3)

#8.7.0.4     Percentage of correct predictions 

pred_model_3 <- augment(model_3, type.predict = "response")

pred_model_3

pred_model_3 %>% 
  dplyr::select(.rownames, breakdown, .fitted) %>% 
  mutate(predict_binary = if_else(.fitted >= 0.5, 1, 0),
         predict_binart = if_else(breakdown == predict_binary, 1, 0)) %>% 
  dplyr::summarize(pcp = mean(predict_binary))

#8.7.0.5     Brier Score 

BrierScore(model_1)

BrierScore(model_2)

BrierScore(model_3)
  
#8.7.0.6     ROC plot 

pred_models <- bind_rows(augment(model_1, response.type = "pred") %>% 
                           mutate(model = "Model 1"),
                         augment(model_2, response.type = "pred") %>% 
                           mutate(model = "Model 2"),
                         augment(model_3, response.type = "pred") %>% 
                           mutate(model = "Model 3")) 

roc <- ggplot(pred_models, aes(d = breakdown, m = .fitted,
                               linetype = model)) +
  geom_roc(n.cuts = 0) +
  geom_abline(slope = 1) +
  labs(x = "1 - Specificity", y = "Sensitivity", linetype = "") +
  theme(legend.key.width = unit(2, "cm"))

roc

calc_auc(roc)

#8.7.0.7 Separation Plots

separationplot(pred = predict.glm(model_1, type = "response"),
               actual = as.vector(model_1$y),
               type = "line", BW = T, show.expected = T,
               newplot = F, line = F,
               heading = "Model 1")

separationplot(pred = predict.glm(model_3, type = "response"),
               actual = as.vector(model_3$y),
               type = "line", BW = T, show.expected = T,
               newplot = F, line = F,
               heading = "Model 3")
