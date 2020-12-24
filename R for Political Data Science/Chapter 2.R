#Chapter 2 Data Management ####

library(tidyverse)  
library(remotes)  
#install_github("arcruz0/politicalds")  
library(politicalds)  
library(skimr)

#2.1    Introduction to data management ####

data("approval")  

ls()  

#2.2    Describing a dataset ####

approval

glimpse(approval)

view(approval)

skim(approval)

count(approval,president_gender)
  
#2.3    Basic operations ####
#2.3.1   Select Columns

select(approval, country)

reduced_approval <- select(approval, country)
reduced_approval

select(approval, country, year, unemployment)

select(approval, country, year, quarter, president, net_approval)  
select(approval, country:net_approval) # recommended way  
select(approval, 1:5)  
  
select(approval, president, country:year, net_approval:unemployment)  

select(approval, president, everything())  

select(approval, starts_with("gdp"))  

#2.3.2   Rename columns 

rename(approval, gdp_ppp_c2011 = gdp)

rename(approval,
       gdp_ppp_c2011 = gdp,
       unemployment_percentage = unemployment,
       gdp_percent_growth = gdp_growth)  


#2.3.3   Filter observations 

filter(approval, country == "Chile")

filter(approval, net_approval> 0)

filter(approval, 
       country == "Argentina", country == "Chile", country == "Uruguay")

#The same, but with another logical operator:

filter(approval, country %in% c("Argentina","Chile","Uruguay"))
  
filter(approval, corruption > mean(corruption))

filter(approval, is.na(corruption))
 
#2.3.4    Change the order of a dataset 

arrange(approval,corruption)
  
arrange(approval, -corruption)

arrange(approval, desc(president))

arrange(approval, president_gender, -net_approval)

#2.3.5   Transform and create variables 

mutate(approval, population_mill = population / 1000000)

mutate(approval, log_gdp = log(gdp))
  
mutate(approval, gdp_pc = gdp / population)

mutate(approval, 
       population_mill = population / 1000000, 
       gdp_pc = gdp / population)

#2.3.6   Summaries 

summarize(approval, unemployment_mean = mean(unemployment))

summarize(approval, 
          unemployment_mean = mean(unemployment),
          growth_mean = mean(gdp_growth),
          approv_mean = mean(net_approval))
  
#2.3.7    Grouped summaries 

approval_by_country <- group_by(approval, country)  

summarize(approval_by_country, 
          unemployment_mean = mean(unemployment),
          growth_mean = mean(gdp_growth),
          approv_mean = mean(net_approval))
  
approval_by_country_year <- group_by(approval, country, year)

summarize(approval_by_country_year, 
          unemployment_mean = mean(unemployment),
          growth_mean = mean(gdp_growth),
          approv_mean = mean(net_approval))

approval_by_country_year %>% 
  ungroup()

#2.4    Chain commands ####

approval_with_gdp_pc <- mutate(approval,
                               gdp_pc = gdp / population)

filter(approval_with_gdp_pc, gdp_pc > mean(gdp_pc))

approval %>%
  mutate(gdp_pc = gdp / population) %>%
  filter(gdp_pc > mean(gdp_pc))

approval %>% 
  group_by(country) %>% 
  summarize(unemployment_mean = mean(unemployment),
            growth_mean = mean(gdp_growth),
            approv_mean = mean(net_approval))
  
#2.5    Recode values ####

approval %>% 
  mutate(d_woman_pres = if_else(condition = president_gender == "female",
                                true = 1,
                                false = 0)) %>% 
  select(country:president, president_gender, d_woman_pres) # for legibility

approval %>% 
  # we do not explicit the arguments to make the code concise:
  mutate(d_ec_crisis = if_else(gdp_growth <0 | unemployment > 20, 1, 0)) %>% 
  # the following is just to show the results more clearly:
  select(country:quarter, gdp_growth, unemployment, d_ec_crisis) %>% 
  filter(country == "Argentina" & year %in% c(2001, 2013))
  
unique(approval$country)

approval %>% 
  mutate(country_group = case_when(
    country %in% c("Argentina", "Chile", "Uruguay") ~ "Southern Cone",
    country %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras",
                   "Nicaragua", "Panama") ~ "Central America",
    TRUE ~ "Rest of LA"
  )) %>% 
  #we will shrink the dataset to see the results better:
  filter(year == 2000 & quarter == 1) %>% 
  select(country, country_group)

#2.5.1   Data pivoting 

approval_annual <- approval %>% 
  group_by(country, year) %>% 
  summarize(net_approval = mean(net_approval)) %>% 
  ungroup()

approval_annual

data("approval_wide1")

approval_wide1

approval_wide1 %>% 
  pivot_longer(cols = -country,
               names_to = "year", values_to = "net_approval")

approval_annual %>% 
  pivot_wider(names_from = "year", values_from = "net_approval")

approval_wide1 %>% 
  pivot_longer(cols = -country,
               names_to = "year", values_to = "net_approval") %>% 
  pivot_wider(names_from = "year", values_from = "net_approval")

#2.5.2   Wide datasets with more than one variable of interest

data("approval_wide2")  

approval_wide2

approval_wide2 %>% 
  pivot_longer(cols = -country,
               names_to = c("variable","year"), names_sep = "_") %>% 
  pivot_wider(names_from = "variable", values_from = "value")
