#10 Causal inference ####

library(tidyverse)
library(politicalds)
library(ggdag)
library(dagitty)
library(MatchIt)
library(broom)
library(texreg)

#10.6    Drawing and analyzing DAGs ####

#10.6.3    Drawing DAGs with R 

simple_dag <- dagify(
  y ~ x + a + b,
  x ~ a + b,
  exposure = "x",
  outcome = "y"
)

ggdag(simple_dag) +
  theme_dag()

simple_dag <- dagify(
  y ~ x + a + b,
  x ~ a + b,
  exposure = "x",
  outcome = "y"
)

#This may be an error, as there is no command instructing the reader how to generate 'simple_dag_with_coords' yet, despite its use.
#The below command yields a result similar to what is in the book though, and is based on later instruction:

simple_dag_with_coords <- dagify(
  y ~ x + a + b,
  x ~ a + b,
  exposure = "x",
  outcome = "y",
  coords = list(x = c(x = 1, a = 2, b = 2, y = 3),
                y = c(x = 2, a = 1, b = 3, y = 2))
)

ggdag_status(simple_dag_with_coords) +
  theme_dag()

dag_with_var_names <- dagify(
  outcome ~ treatment + confounder1 + confounder2,
  treatment ~ confounder1 + confounder2,
  exposure = "treatment",
  outcome = "outcome"
)

simple_dag_with_coords_and_labels <- dagify(
  y ~ x + a + b,
  x ~ a + b,
  exposure = "x",
  outcome = "y",
  labels = c(y = "Outcome", x = "Treatment",
             a = "Confounder 1", b = "Confounder 2"),
  coords = list(x = c(x = 1, a = 2, b = 2, y = 3),
                y = c(x = 2, a = 1, b = 3, y = 2))
)

ggdag_status(simple_dag_with_coords_and_labels,
             use_labels = "label", text = FALSE) +
  guides(fill = FALSE, color = FALSE) + #Disable the legend
  theme_dag()

#10.6.4    Finding paths and adjustment sets with R 

ggdag_adjustment_set(simple_dag_with_coords, shadow = TRUE) +
  theme_dag()

#10.7     Making adjustments ####

#10.7.1     Simulated mosquito net data 

mosquito_dag <- dagify(
  malaria_risk ~ net + income + health + temperature + resistance,
  net ~ income + health + temperature + eligible + household,
  eligible ~ income + household,
  health ~ income,
  exposure = "net",
  outcome = "malaria_risk",
  coords = list(x = c(malaria_risk = 7, net = 3, income = 4, health = 5,
                      temperature = 6, resistance = 8.5, eligible = 2,
                      household = 1),
                y = c(malaria_risk = 2, net = 2, income = 3, health = 1,
                      temperature = 3, resistance = 2, eligible = 3,
                      household = 2)),
  labels = c(malaria_risk = "Risk of malaria", net = "Mosquito net",
             income = "Income", health = "Health",
             temperature = "Nighttime temperatures",
             resistance = "Insecticide resistance",
             eligible = "Eligible for program",
             household = "Number in household")
)

ggdag_status(mosquito_dag, use_labels = "label", text = FALSE) +
  theme_dag()

data("mosquito_nets")

glimpse(mosquito_nets)

#10.7.2  Verify the conditional independencies 

impliedConditionalIndependencies(mosquito_dag)

cor(mosquito_nets$health, mosquito_nets$household)

cor(mosquito_nets$income, mosquito_nets$resistance)

lm(malaria_risk ~ household + health + income + net + temperature,
   data = mosquito_nets) %>% 
  broom::tidy()

#10.7.3    Find the adjustment set 

paths(mosquito_dag)

adjustmentSets(mosquito_dag)

ggdag_adjustment_set(mosquito_dag, shadow = TRUE,
                     use_labels = "label", text = FALSE)

#10.7.4   Naive unadjusted estimate 

ggplot(mosquito_nets, aes(x = net, y = malaria_risk)) +
  geom_boxplot()

model_naive <- lm(malaria_risk ~ net, data = mosquito_nets)

texreg::screenreg(model_naive)

#10.7.5    Regression 

model_regression <- lm(malaria_risk ~ net + income + temperature + health,
                       data = mosquito_nets)

texreg::screenreg(model_regression)

#10.7.6    Matching

matched <- matchit(net ~ income + temperature + health,
                   data = mosquito_nets,
                   method = "nearest", distance = "mahalanobis",
                   replace = TRUE)

matched

mosquito_nets_matched <- match.data(matched)

glimpse(mosquito_nets_matched)

model_matched <- lm(malaria_risk ~ net, data = mosquito_nets_matched,
                       weights = weights)

texreg::screenreg(model_matched)
  
#10.7.7    Inverse probability weighting

data("edu_age") # from the "politicalds" package

edu_age <- edu_age %>% 
  mutate(treatment = factor(treatment))

model_treatment <- glm(treatment ~ education + age, data = edu_age,
                       family = binomial(link = "logit"))

edu_age_propensities <- broom::augment_columns(
  model_treatment, edu_age, type.predict = "repsponse"
) %>% 
  rename(propensity = .fitted)

edu_age_propensities %>% 
  select(id, treatment, education, age, propensity) %>% 
  slice(59, 27)

edu_age_ipw <- edu_age_propensities %>% 
  mutate(ipw = (treatment_num / propensity) +
           (1 - treatment_num) / (1 - propensity))

edu_age_ipw %>% 
  select(id, treatment, education, age, propensity, ipw) %>% 
  slice(59, 27)

ggplot(edu_age_ipw, aes(x = education, y = age,
       color = treatment, size = ipw)) +
  geom_point()

model_mosquito_net <- glm(net ~ income + temperature + health,
                          data = mosquito_nets,
                          family = binomial(link = "logit"))

mosquito_nets_ipw <- broom:: augment_columns(model_mosquito_net,
                                             mosquito_nets,
                                             type.predict = "response") %>% 
  rename(propensity = .fitted) %>% 
  mutate(ipw = (net_num / propensity) + (1 - net_num) / (1 - propensity))

model_ipw <- lm(malaria_risk ~ net, data = mosquito_nets_ipw,
                weights = ipw)

texreg::screenreg(model_ipw)

#10.7.8    Comparing all the methods 

texreg::screenreg(list(model_naive, model_regression,
                       model_matched, model_ipw),
                  custom.model.names = c("Naive", "Regression",
                                         "Matching", "IPW"))