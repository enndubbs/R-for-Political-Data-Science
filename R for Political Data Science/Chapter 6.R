#Chapter 6 Case Selection Based on Regressions ####

library(tidyverse)
library(politicalds)
library(broom)
  
data("welfare")  

welfare_no_na <- welfare %>%  
  drop_na(gini, education_budget, foreign_inv, health_budget,  
          socialsec_budget, population, sector_dualism,  
          ethnic_diversity, gdp, regime_type, legislative_bal, repression)  

model_2 <- lm(gini ~ 1 + education_budget + foreign_inv + health_budget +  
                socialsec_budget + population + sector_dualism +  
                ethnic_diversity + gdp + regime_type + legislative_bal +  
                repression,  
              data = welfare_no_na)  

model_aug <- broom::augment(model_2, data = welfare_no_na)  

model_aug  

#6.1    Which case study should I select for qualitative research? ####

#6.1.1    Typical cases 

ggplot(data = model_aug, mapping = aes(x = .fitted, y = .resid)) +  
  geom_point() +  
  geom_hline(aes(yintercept = 0)) +  
  geom_text(data = model_aug %>%  
              mutate(.resid_abs = abs(.resid)) %>%  
              top_n(-4, .resid_abs),  
            mapping = aes(label = country_id))  

#6.1.2   Outliers 

ggplot(data = model_aug,  
       mapping = aes(x = .fitted, y = .resid)) +  
  geom_point() +  
  geom_hline(aes(yintercept = 0)) +  
  geom_text(data = model_aug %>%  
              mutate(.resid_abs = abs(.resid)) %>%  
              top_n(4, .resid_abs),  
            mapping = aes(label = country_id))  

#6.1.3    Inﬂuential Cases 

model_aug %>%  
  mutate(dfb_cseduc = as_tibble(dfbetas(model_2))$education_budget) %>%  
  arrange(-dfb_cseduc) %>%  
  slice(1:3) %>%  
  dplyr::select(country_id, dfb_cseduc)  
#In the text, "as_tibble" is written as "as.tibble", but I got error messages indicating as.tibble was deprecated. I have substituted as_tibble.


ggplot(data = model_aug,  
       mapping = aes(x = .fitted, y = .cooksd)) + 
  geom_point() + 
  geom_text(data = model_aug %>% top_n(3, .cooksd),
            mapping = aes(label = country_id)) +  
  labs(title = "Influential cases")  

#6.1.4.1     Extreme cases in the independent variable: x 

ggplot(welfare_no_na, aes(x = education_budget)) + 
  geom_histogram(binwidth = 1) +  
  labs(caption = "Source: Huber et al (2006)",  
       x = "Education Expenditures (% of the GDP spent on Education)",  
       y = "Frequence")  

mean(model_aug$education_budget, na.rm = T)  

model_aug %>%  
  mutate(dif_cseduc = abs(education_budget - mean(education_budget,
                                                  na.rm = T))) %>%  
  top_n(3, dif_cseduc) %>%  
  arrange(-dif_cseduc) %>%  
  dplyr::select(country_id, year, education_budget, dif_cseduc)  

model_aug <- model_aug %>%  
  mutate(dif_cseduc = education_budget - mean(education_budget, na.rm = T))  

ggplot(data = model_aug,  
       mapping = aes(x = .fitted, y = dif_cseduc)) +  
  geom_point() +  
  geom_text(data = model_aug %>% top_n(3, dif_cseduc),  
            mapping = aes(label = country_id))  

#6.1.4.2    Extreme cases in the dependent variable y 

mean(model_aug$gini, na.rm = T)  

model_aug %>%  
  mutate(dif_gini = abs(gini - mean(gini, na.rm = T))) %>%  
  top_n(3, dif_gini) %>%  
  arrange(-dif_gini) %>%  
  dplyr::select(country_id, gini, dif_gini)  

model_aug <- model_aug %>%  
  mutate(dif_gini = abs(gini - mean(gini, na.rm = T))) 
  
ggplot(data = model_aug,
       mapping = aes(x = .fitted, y = dif_gini)) +  
  geom_point() +  
  geom_text(data = model_aug %>% top_n(2, dif_gini),  
            mapping = aes(label = country_id))  
#There were several of formatting errors in the above code in the book, and I have corrected them to what I believe was intended

#6.1.5    Most similar cases 

welfare_no_na <- welfare_no_na %>%  
  mutate(treatment = if_else(education_budget > mean(education_budget),  
                             1, 0))  

m_propensityscore <- glm(treatment ~ sector_dualism + foreign_inv + gdp +  
                             population + ethnic_diversity + regime_type +  
                             regime_type * socialsec_budget + health_budget +  
                             socialsec_budget + legislative_bal + repression,  
                           data = welfare_no_na,  
                           family = binomial(link = logit),  
                           na.action = na.exclude)  

propensity_scores<- augment(m_propensityscore, data = welfare_no_na,  
                            type.predict = "response") %>%  
  dplyr::select(propensity_scores = .fitted, country_id, treatment, year,  
                gini)  

propensity_scores %>%  
  filter(treatment == 1) %>%  
  arrange(propensity_scores) %>%  
  dplyr::select(country_id, year, propensity_scores) %>%  
  slice(1:2)  
 
propensity_scores %>%  
  filter(treatment== 0) %>%  
  arrange(propensity_scores) %>%  
  dplyr::select(country_id, year, propensity_scores) %>%  
  slice(1:2)  

#6.1.6    Most different cases 

propensity_scores <- propensity_scores %>%  
  mutate(gini = if_else(gini > mean(gini, na.rm = T), 1, 0))  

propensity_scores %>%  
  filter(gini == 1 & treatment==0) %>%  
  arrange(propensity_scores) %>%  
  dplyr::select(country_id, year, propensity_scores) %>%  
  slice(1:2)  

propensity_scores %>%  
  filter(gini == 1 & treatment==0) %>%  
  arrange(-propensity_scores) %>%  
  dplyr::select(country_id, year, propensity_scores) %>%  
  slice(1:2)

