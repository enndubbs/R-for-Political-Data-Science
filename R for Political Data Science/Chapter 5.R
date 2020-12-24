#5 Linear Models ####

library(tidyverse)
library(politicalds)
library(skimr)
library(car)
library(ggcorrplot)
library(texreg)
library(prediction)
library(lmtest)
library(sandwich)
library(miceadds)
library(texreg)
library(ggpubr) 

#5.1    OLS in R ####

data("welfare")  

ls()  

#5.1.2    Descriptive statistics and distribution of the variables in the model 

skim(welfare)

#5.1.3    Correlation matrix of independent variables 

corr_selected <- welfare %>%  
  select (gini, education_budget, sector_dualism, foreign_inv, gdp,  
          ethnic_diversity, regime_type, health_budget, socialsec_budget,  
          legislative_bal, population) %>%  
  # calculate correlation matrix and round to 1 decimal place:  
  cor(use = "pairwise") %>%  
  round(1)  

ggcorrplot(corr_selected, type = "lower", lab = T, show.legend = F)  

#5.1.4   Distribution of the variables of interest

ggplot(welfare, aes(x = gini, na.rm = T)) +  
  geom_histogram(binwidth = 1 ) +  
  labs(x = "Gini Index" , y = "Frequency" ,  
       caption = "Source: Huber et al (2012)")  

ggplot(welfare, aes(x = education_budget, na.rm = T)) +  
  geom_histogram(binwidth = 1 ) +  
  labs(x = "Education Expenditure" , y = "Frequency" ,  
       caption = "Source: Huber et al (2012)" )  

#5.1.5   Relation between the dependent and independent variable 


ggplot(welfare, aes(education_budget, gini)) +  
  geom_point() +  
  labs(x = "Education Expenditure",  
     caption = "Source: Huber and Stephens (2012)") 

#5.2    Bivariate model: simple linear regression ####

#5.2.1   Estimating a linear model in R 
  
model_1 <- lm(gini ~ 1 + education_budget, data = welfare)  
   
class(model_1)
   
summary(model_1)  
 
screenreg(model_1)  
 
screenreg (model_1,  
  custom.model.names = "Model 1",  
  custom.coef.names = c("Constant", "Education expenditure"))  

htmlreg(list(model_1), file = "model_1.doc",  
        custom.model.names = "Model 1",  
        custom.coef.names = c("Constant", "Education expenditure"),  
        inline.css = FALSE, doctype = T, html.tag = T,  
        head.tag = T, body.tag = T)  
    
1.233 / 0.25  

coef(summary(model_1))[, "Pr(>|t|)"]  

#5.2.2   Graphic representation 

ggplot(data = welfare, # the dataset is selected  
    aes(x = education_budget, y = gini))+ # indep. and dep. variables  
    geom_point() + # the observed values are plotted  
    geom_smooth(method = "lm", # the regression line is overlapped  
            se = F, # the error area is not plotted at a 95 % CI  
            color = "black") + # color line  
    labs (x = "Education Expenditure", y = "Inequality")  

ggplot(data = welfare, aes(x = education_budget, y = gini)) +  
     geom_point() +  
     geom_smooth(method = "lm", color = "black",  
            se = T) + # we add the prediction  
     labs(x = "Education Expenditure", y = "Inequality")  

#5.3    Multivariate model: multiple regression ####

welfare_no_na <- welfare %>%
  drop_na(gini, education_budget, foreign_inv, health_budget,
          socialsec_budget, population, sector_dualism, ethnic_diversity,
          gdp, regime_type, legislative_bal)  

model_2 <- lm(gini ~ 1 + education_budget + foreign_inv + health_budget +
                socialsec_budget + population + sector_dualism +
                ethnic_diversity + gdp + factor(regime_type) + 
                legislative_bal,
              data = welfare_no_na)  

screenreg(model_2)  

models <- list(model_1, model_2)  
   
screenreg(models,
          custom.model.names = c("Model 1", "Model 2"),
          custom.coef.names = c(
            "Constant", "Education expenditure", "FDI",
            "Health expenditure", "Social sec. expenditure",
            "Young population", "Dualism in economy",
            "Ethnic division", "pc GDP", "Democratic.reg", "Mixed.reg",
            "Authoritarian.reg", "Balance between powers")  
)  
   
model_2_restricted <- lm(gini ~ 1 + education_budget + ethnic_diversity,
                         data = welfare_no_na)  
   
screenreg(model_2_restricted)
   
pred_model_2_restricted <- as_tibble(prediction(model_2_restricted))  
   
ggplot(data = pred_model_2_restricted) + # the new predicted values  
  geom_point(mapping = aes(x = education_budget, y = gini,  
                          color = factor(ethnic_diversity))) +  
  # the regression lines are drawn (differentiated by color):  
  geom_line(mapping = aes(x = education_budget, y = fitted,  
                         color = factor(ethnic_diversity),  
  group = factor(ethnic_diversity))) +  
  labs(x = "Education Expenditure", y = "Inequality",  
  color = "Ethnic division")  

#5.5    Inference in multiple linear models ####

model_2_restricted <- lm(gini ~ 1 + education_budget + foreign_inv +  
                           health_budget + socialsec_budget +  
                           population + sector_dualism + gdp,  
                         data = welfare_no_na)  

anova(model_2, model_2_restricted)  

#5.6    Testing OLS assumptions ####

#5.6.3    Linearity in parameters 

ggplot(mapping = aes(x = model_1$fitted.values, y = model_1$residuals)) +  
  labs(x = "Predicted Values", y = "Residuals") +  
  geom_point() +  
  geom_hline(mapping = aes(yintercept = 0))  

model_1_log <- lm(log(gini) ~ 1 + education_budget, data = welfare)  
   
screenreg(model_1_log)  

crPlots(model_1)  

welfare_no_na <- welfare_no_na  %>%
  mutate(cseduc2 = education_budget * education_budget)  
#In the text there is no piping after "welfare_no_na", I assumed this to be an error
   
model_1_quadratic <- lm(gini ~ 1 + cseduc2 + education_budget,  
  data = welfare_no_na)  

crPlots(model_1_quadratic)  
 
resettest(model_1, power = 2, type = "fitted", data = welfare_no_na)  

#5.6.4    Variation in independent variables and no perfect collinearity

#5.6.4.1     Multicollinearity issues 

corr_selected <- welfare %>%  
   select(gini, education_budget, sector_dualism, foreign_inv, gdp,  
        ethnic_diversity, regime_type, health_budget, socialsec_budget,  
        legislative_bal, population) %>%  
   # calculate correlation matrix and round to 1 decimal place:  
   cor(use = "pairwise") %>%  
   round(1)  
    
ggcorrplot(corr_selected, type = "lower", lab = T, show.legend = F)  

vif(model_2)  

sqrt(vif(model_2)) > 2  

#5.6.5    Homoscedasticity 

#5.6.5.1     Evaluating the assumption 

ggplot(welfare_no_na, aes(education_budget, gini)) +  
  geom_point() +  
  geom_smooth(method = "lm", color = "black")  

residualPlot (model_1)  

residualPlot(model_2)  

residualPlots(model_2, layout = c(3, 4), tests = F, fitted = F)  

#5.6.5.2     Statistical diagnosis 

bptest(model_2, studentize = T)  
 
#5.6.5.3      Solutions to heteroscedasticity 

model_2_robust_3 <- coeftest(model_2, vcov = vcovHC(model_2, "HC3"))  
model_2_robust_1 <- coeftest(model_2, vcov = vcovHC(model_2, "HC1"))  
model_2_robust_0 <- coeftest(model_2, vcov = vcovHC(model_2, "HC0"))  
   
models_robust <- list(model_2, model_2_robust_0,  
                      model_2_robust_1, model_2_robust_3)  
   
screenreg(models_robust,  
          custom.model.names = c("w/o robust SE",  
                                 "robust HC0", "robust HC1", "robust HC3"))  

#5.6.5.4     A special case of heteroscedasticity: error variance associated to clusters 

ggplot(welfare_no_na, aes(education_budget, gini)) +  
  geom_point()  
    
ggplot(welfare_no_na, aes(education_budget, gini)) +  
  geom_point() +  
  facet_wrap(~country_id)  
   
model_2_cluster <- miceadds::lm.cluster(  
  data = welfare_no_na,  
  formula = gini ~ 1 + education_budget + sector_dualism + foreign_inv +  
  gdp + ethnic_diversity + regime_type + health_budget +  
  socialsec_budget + legislative_bal,  
  cluster = "country_id")  
   
summary(model_2_cluster)  

#5.6.6    Normality in the error distribution 

qqPlot(model_2$residuals, col.lines = "black")  

ggdensity(model_2$residuals, main = "Density plot of the residuals")