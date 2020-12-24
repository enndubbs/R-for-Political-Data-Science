#Chapter 7 Panel Data ####

library(tidyverse)
library(politicalds)
library(unvotes)
library(lubridate)
library(ggcorrplot)
library(plm)

#7.1    Introduction ####
  
data("us_brazil")

ggplot(us_brazil) +
  geom_line(aes(x = year, y = vote)) +
  labs(x = "Year", y = "Vote Convergence")

p_votes_us_br <- un_votes %>% 
  filter(country %in% c("United States of America", "Brazil")) %>% 
  inner_join(un_roll_calls, by = "rcid") %>% 
  inner_join(un_roll_call_issues, by = "rcid") %>% 
  mutate(issue = if_else(issue == "Nuclear weapons",
                         "Nuclear weapons", issue)) %>% 
  group_by(country, year = year(date), issue) %>% 
  summarize(votes = n(),
            percent_yes = mean(vote == "yes")) %>% 
  filter(votes>5)
  
ggplot(p_votes_us_br,
       mapping = aes(x = year, y = percent_yes, color = country)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~ issue, ncol = 2) +
  scale_color_grey() + 
  labs(x = "Year", y = "% Yes", color = "Country")

ggplot(us_brazil) +
  geom_line(aes(x = year, y = country_power)) +
  labs(x = "Year", y = "Country power index")

ggscatter(us_brazil, x = "country_power", y = "vote",
          add = "reg.line", add.params = list(color = "black",
                                              fill = "lightgray"),
          conf.int = TRUE) +
  stat_cor(method = "pearson", label.x = .015)

#7.2    Describing your panel dataset ####

data("us_latam")

us_latam %>%
  count(country)

#7.2.1   Option A. Line graph 

ggplot(us_latam, aes(x=year, y = vote,
                     color = country, linetype = country, group = country)) +
  geom_line() +
  labs(x = "Year", y = "% Yes", color = "", linetype = "")

#7.2.2   Option B. Box plot 

ggplot(us_latam, aes(x= factor(year), y = vote)) +
  geom_boxplot() +
  scale_x_discrete(breaks = seq(1970, 2007, by = 5)) +
  labs(x = "Year", y = "% Convergence with the US")

p_votes_countries <- un_votes %>%
  filter(country %in% c("United States of America", "Brazil", "Bolivia",
                        "Argentina", "Chile", "Peru", "Ecuador", "Colombia",
                        "Venezuela", "Paraguay", "Uruguay")) %>% 
  inner_join(un_roll_calls, by = "rcid") %>% 
  inner_join(un_roll_call_issues, by = "rcid") %>% 
  mutate(issue = if_else(issue == "Nuclear weapons",
                         "Nuclear weapons", issue)) %>% 
  group_by(country, year = year(date), issue) %>% 
  summarize(votes = n(),
            percent_yes = mean(vote == "yes")) %>% 
  filter(votes > 5)

ggplot(p_votes_countries,
       mapping = aes(x = year, percent_yes,
                     linetype = country, color = country)) +
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~issue, ncol = 2) +
  labs(x = "Year", y = "% Convergence with the US", color = "", linetype = "")

us_latam %>% 
  filter(country != "Brazil") %>% 
  ggplot(aes(x = year, y = country_power)) +
  geom_line() +
  facet_wrap(~country, nrow = 3)

#7.3    Modeling group-level variation ####

pooled <- lm(vote ~ country_power, data = us_latam)

summary(pooled)

manual_fe <- lm(vote ~ country_power + factor(country), data = us_latam)

summary(manual_fe)

us_latam <- us_latam %>% 
  mutate(hat_fe = fitted(manual_fe))

ggplot(data = us_latam, aes(x = country_power, y = hat_fe,
                            label = country, group = country)) +
  geom_point() +
  # add country-specific lines
  geom_smooth(method = "lm", se = F, color = "black") +
  # add pooled line
  geom_smooth(mapping = aes(x = country_power, y = hat_fe), inherit.aes = F,
              method = "lm", se = T, color = "black", linetype = "dashed") +
  # label lines
  geom_text(
    data = us_latam %>% 
      group_by(country) %>% 
      top_n(1, country_power) %>% 
      slice(1),
    mapping = aes(label = country), vjust = 1
  )

#7.4    Fixed vs. random effects ####

fe <- plm(vote ~ country_power, data = us_latam, index = c("code", "year"))

summary(fe)
  
fixef(fe, type = "dfirst")

re <- plm(vote ~ country_power, data = us_latam,
          index = c("code", "year"), model = "random")

summary(re)

#7.4.1   Hausman test

phtest(fe, re)

#7.5    Testing for unit roots ####

fe <- plm(vote ~ country_power, data = us_latam, index = c("code", "year"))

summary(fe)

corr_selected <- us_brazil %>% 
  select(year, country_power, vote) %>% 
  # calculate correlation matrix and round to 1 decimal place:
  cor(use = "pairwise") %>% 
  round(1)

ggcorrplot(corr_selected, type = "lower", lab = T, show.legend = F)

purtest(vote ~ 1, data = us_latam,
        index = c("country", "year"), pmax = 10, test = "ips",
        lags = "AIC", exo = "intercept")

purtest(vote ~ 1, data = us_latam,
        index = c("country", "year"), pmax = 10, test = "levinlin",
        lags = "AIC", exo = "intercept")

purtest(vote ~ 1, data = us_latam,
        index = c("country", "year"), pmax = 10, test = "ips",
        lags = "AIC", exo = "intercept")

#7.5.1   Creation of lags and leads 

us_latam <- us_latam %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  mutate(vote_lag1 = dplyr::lag(vote, 1),
         vote_lead1 = dplyr::lead(vote, 1),
         vote_diff1 = c(NA, diff(vote))) %>% 
  ungroup()

us_latam <- us_latam %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  mutate(country_power_lag1 = dplyr::lag(country_power, 1),
         country_power_diff1 = c(NA, diff(vote))) %>% 
  ungroup()

fe_lag <- plm(vote ~ vote_lag1 + country_power, data = us_latam,
              index = c("code", "year"))

summary(fe_lag)

fe_diff <- plm(vote_diff1 ~ country_power_diff1, data = us_latam,
              index = c("code", "year"))

summary(fe_diff)

pwartest(fe)

pwartest(fe_lag)

#7.6    Robust and panel-corrected standard errors ####

#7.6.1    Robust standard errors 

coeftest(fe, vcov. = function(x){vcovHC(x, type = "sss")})

summary(fe)

coeftest(fe, vcov = vcovBK, type = "HC1", cluster = "time")

