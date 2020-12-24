#11 Advanced Political Data Management ####

library(tidyverse)
library(politicalds)
library(skimr)
library(countrycode)
library(stringdist)
library(naniar)
library(mice)
library(remotes)
#The 'inexact' package is not available on CRAN, and must be installed with 'remotes::install_github("arcruz0/inexact")'
library(inexact)

# 11.1    Introduction ####

data("treaties")

ls()

glimpse(treaties)

treaties %>% 
  count(treaty_name)

# 11.2    Merging datasets ####

  summary_treaties <- treaties %>%  
  group_by(country_name) %>%  
  summarize(  
    # we will find the ones that are not missing with !is.na()  
    sum_signed = sum(action_type == "Signature" & !is.na(action_year)),  
    sum_ratif = sum(action_type == "Ratification" & !is.na(action_year))  
  )  

summary_treaties  
  
summary_treaties %>%  
  filter(country_name == "United States of America (the)")  

data("americas_gdp_pc")  
  
americas_gdp_pc %>%  
  count(country_name)  

americas_gdp_pc %>%  
  count(year)  

summary_gdp <- americas_gdp_pc %>%  
  group_by(country_name) %>%  
  summarize(avg_gdp_pc = mean(gdp_pc))  

summary_complete <- left_join(x = summary_treaties, y = summary_gdp,  
                                # we can explicitly provide the id  
                                # variable name:  
                                by = "country_name")  

summary_complete %>% #- summary_treaties 
left_join(summary_gdp, by = "country_name")  

summary_complete  
treaties_with_gdp <- treaties %>%  
  left_join(americas_gdp_pc,  
            by = c("country_name", "adoption_year" = "year"))  

treaties_with_gdp  

#11.2.1    Standardizing country codes 

library(countrycode)  

summary_complete_with_codes <- summary_complete %>%  
  mutate(  
    country_iso2c = countrycode(country_name, origin = "country.name",  
                                destination = "iso2c"),  
    country_iso3c = countrycode(country_name, origin = "country.name",  
                                destination = "iso3c"),  
    country_cown = countrycode(country_name, origin = "country.name",  
                               destination = "cown"),  
    country_imf = countrycode(country_name, origin = "country.name",  
                              destination = "imf")  
  )  

summary_complete_with_codes %>%  
  select(starts_with("country_")) 

# 11.3    Fuzzy or inexact join of data ####

data("summary_messy")  

unique(summary_messy$country_name)  

summary_treaties %>%   
  left_join(summary_messy, by = "country_name")  
  
stringdistmatrix(summary_treaties$country_name,  
                 summary_messy$country_name,  
                 # we will use the default algorithm, named "osa" or  
                 # "optimal string alignment"  
                 method = "osa",  
                 useNames = T)  
  
inexact::inexact_addin()  

inexact::inexact_join(  
    x = summary_treaties,  
    y = summary_messy,  
    by = 'country_name',  
    method = 'osa',  
    mode = 'left',  
    custom_match = c(  
      'Bolivia (Plurinational State Of)' = 'Bolivia'  
    )  
  ) 

# 11.4    Missing values’ management ####

#11.4.1    Types of missing values 
  
treaties %>%  
  filter(country_name == "United States of America (the)" &  
           treaty_name == "Comprehensive Nuclear-Test-Ban Treaty") %>%  
  select(country_name, action_type, action_year)  

#11.4.2    Description of missing values in the dataset 

data("cinc_index")  
  
cinc_index %>%  
  filter(year == 2012) %>%  
  select(country_iso3c, capabilities_index)  

gg_miss_var(cinc_index)  

gg_miss_var(cinc_index, show_pct = T)  

gg_miss_fct(x = cinc_index, fct = country_iso3c)  

cinc_index_miss_plot <- cinc_index %>%  
  bind_shadow(only_miss = T)  

ggplot(cinc_index_miss_plot,  
       aes(x = year, fill = urban_population_NA)) +  
  geom_histogram(binwidth = 1) +  
  # the rest is to make the plot more readable:  
  scale_x_continuous(breaks = seq(1860, 2020, 10)) +  
  scale_y_continuous(breaks = c(1, 2)) +  
  scale_fill_manual(values = c("lightgray", "black")) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

ggplot(cinc_index_miss_plot,  
       aes(x = year, fill = mil_expenditures_NA)) +  
  geom_histogram(binwidth = 1) +  
  # the rest is to make the plot more readable:  
  scale_x_continuous(breaks = seq(1860, 2020, 10)) +  
  scale_y_continuous(breaks = c(1, 2)) +  
  scale_fill_manual(values = c("lightgray", "black")) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

# 11.5    Imputation of missing values ####

ggplot(cinc_index) +  
  geom_point(aes(x = year, y = capabilities_index,  
                 group = country_iso3c, color = country_iso3c)) +  
  scale_x_continuous(breaks = seq(1860, 2020, 10)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

cinc_index2 <- cinc_index %>%  
  mutate(capabilities_index = if_else(  
    country_iso3c == "USA" & year %in% 1950: 1970,  
    NA_real_, capabilities_index  
  ))  

ggplot(cinc_index2) +  
  geom_point(aes(x = year, y = capabilities_index, group = country_iso3c,  
                 color = country_iso3c)) +  
  scale_x_continuous(breaks = seq(1860, 2020, 10)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

imputed_init <- mice(cinc_index2, maxit = 0)  

predictor <- imputed_init$predictorMatrix  

predictor[, "country_cown"] <- -2  

predictor[, "year"] <- 2  

method_1 <- imputed_init$method  
method_1[which(method_1 == "pmm")] <- "2l.lmer"  

imputed_mice <- mice(cinc_index2, m = 5, seed = 1,  
                     method = method_1, predictorMatrix = predictor)  
  
imputed_mice$imp$capabilities_index  

stripplot(imputed_mice, capabilities_index, pch = 20)  

complete_data1 <- mice::complete(imputed_mice, 1)  
complete_data2 <- mice::complete(imputed_mice, 2)  
complete_data3 <- mice::complete(imputed_mice, 3)  
complete_data4 <- mice::complete(imputed_mice, 4)  
complete_data5 <- mice::complete(imputed_mice, 5)  

complete_data_all <- bind_rows(  
    complete_data1 %>% mutate(num_imp = 1),  
    complete_data2 %>% mutate(num_imp = 2),  
    complete_data3 %>% mutate(num_imp = 3),  
    complete_data4 %>% mutate(num_imp = 4),  
    complete_data5 %>% mutate(num_imp = 5),  
  )%>%  
  select(num_imp, everything()) %>%  
  mutate(source = "Specific Imp.") %>%  
  filter(country_iso3c == "USA" & year %in% 1950:1970)  

avg_imp <- complete_data_all %>%  
  group_by(country_iso3c, country_cown, year) %>%  
  summarize(capabilities_index = mean(capabilities_index)) %>%  
  ungroup()%>%  
  mutate(source = "Average Imp.") %>%  
  filter(country_iso3c == "USA" & year %in% 1950:1970)  

ggplot(mapping = aes(x = year, y = capabilities_index,  
                       group = country_iso3c, color = country_iso3c)) +  
  geom_point(data = cinc_index2) +  
  # add imputed data and its average values :  
  geom_point(data = complete_data_all, color = "darkgray") +  
  geom_point(data = avg_imp, color = "black") +  
  # add vertical lines for emphasis:  
  geom_vline(xintercept = c(1950, 1970), linetype = "dashed") +  
  scale_x_continuous(breaks = seq(1860, 2020, 10)) +  
  scale_color_manual(values = c("lightgray", "black")) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  .  

#11.5.1    Regressions after imputing data 

model_incomplete <- lm(capabilities_index ~ energy_cons + urban_population,  
                       data = cinc_index2)  
texreg::screenreg(model_incomplete)  

model_imp1 <- lm(capabilities_index ~ energy_cons + urban_population,  
                 data = complete_data1)  

model_imp2 <- lm(capabilities_index ~ energy_cons + urban_population,  
                 data = complete_data2)  

model_imp3 <- lm(capabilities_index ~ energy_cons + urban_population,  
                 data = complete_data3)  

model_imp4 <- lm(capabilities_index ~ energy_cons + urban_population,  
                 data = complete_data4)  

model_imp5 <- lm(capabilities_index ~ energy_cons + urban_population,  
                 data = complete_data5)  

model_list <- list(model_incomplete, model_imp1, model_imp2, model_imp3)  

texreg::screenreg(  
    model_list,  
    custom.model.names = c("Incomp. m.", "Imp. m.1", "Imp. m.2", "Imp. m.3")  
  )  

imputed_mice_form <- with(  
    imputed_mice,  
    lm(capabilities_index ~ energy_cons + urban_population)  
  )  

imputed_model_pooled <- summary(pool(imputed_mice_form))  

imputed_model_pooled 

tr_imputed_model_pooled <- texreg::createTexreg(  
  # coefficient names:  
  coef.names = as.character(imputed_model_pooled$term),  
  # coefficients, SEs and p-values:  
  coef = imputed_model_pooled$estimate,  
  se = imputed_model_pooled$std.error,  
  pvalues = imputed_model_pooled$p.value,  
  # R^2 and number of observations:  
  gof.names = c("R^2", "Num. obs."),  
  gof = c(pool.r.squared(imputed_mice_form)[1, 1], nrow(imputed_mice$data)),  
  gof.decimal = c(T, F)  
)  

texreg::screenreg(tr_imputed_model_pooled) 

texreg::screenreg(list(model_incomplete, tr_imputed_model_pooled),  
                  custom.model.names = c("Incomp. m.", "Pooled imp. m."))  