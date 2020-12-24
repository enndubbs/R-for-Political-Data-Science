#9 Survival Models ####

library(tidyverse)  
library(politicalds)
library(skimr) 
library(countrycode)
library(ggalt)
library(survival)
library(survminer) 
library(texreg)

# 9.4    Estimating Cox Models in R ####

data("direct_democracy")  

ls()  

skim(direct_democracy)

direct_democracy %>% 
  filter(country_name == "Albania")

direct_democracy_b <- direct_democracy %>% 
  group_by(country_name) %>% 
  # that the cumulative sum of the dummy is maximum 1
  filter(cumsum(direct_dem) <= 1) %>% 
  ungroup()

direct_democracy %>% 
  filter(country_name == "Albania" & direct_dem == 1)

direct_democracy_b %>% 
  filter(country_name == "Albania" & direct_dem == 1) 

gantt_plot_df <- direct_democracy_b %>% 
  # the variables we are interested in
  select(country_name, year, direct_dem) %>% 
  group_by(country_name) %>% 
  filter(year == min(year) | year == max(year)) %>% 
  # we need to remove the observations for countries that "are born" with direct democracy
  filter(!(year == min(year) & direct_dem == 1)) %>% 
  summarize(year_enters = min(year),
            year_exits = max(year),
            exits_bc_dd = max(direct_dem)) %>% 
  ungroup()



gantt_plot_df

gantt_plot_df %>% filter(exits_bc_dd == 1)

gantt_plot_df_region <- gantt_plot_df %>% 
  mutate(region = countrycode(country_name,
                              origin = "country.name", dest = "region"))

gantt_plot_df_region

gantt_plot <- ggplot(data = gantt_plot_df_region,
                     mapping = aes(x = year_enters,
                                   xend = year_exits,
                                   y = fct_rev(country_name),
                                   color = factor(exits_bc_dd))) +
  geom_dumbbell(size_x = 2, size_xend = 2)

gantt_plot

gantt_plot_la <- ggplot(data = gantt_plot_df_region %>% 
                          filter(region == "Latin America & Caribbean"),
                        mapping = aes(x = year_enters,
                                      xend = year_exits,
                                      y = fct_rev(country_name),
                                      color = fct_recode(factor(exits_bc_dd)))) +
  geom_dumbbell(size_x = 2, size_xend = 2)

gantt_plot_la

gantt_plot_la <- gantt_plot_la +
  geom_text(aes(label = year_enters), vjust = -0.4) +
  geom_text(aes(x = year_exits, label = year_exits), vjust = -0.4) +
  labs(x = "year", y = "",
       color = "Do they adopt Direct democracy?") +
  theme(axis.text.x = element_blank())

gantt_plot_la

direct_democracy_c <- direct_democracy_b %>% 
  group_by(country_name) %>% 
  # we will eliminate the first year for each country; as it is not at risk of dying yet!
  filter(year != min(year)) %>% 
  mutate(risk_time_at_end = c(1:n()),
         risk_time_at_start = c(0:(n()-1))) %>% 
  ungroup() %>% 
  select(country_name, year, risk_time_at_start, risk_time_at_end,
                everything())
  
direct_democracy_c  

km <- survfit(Surv(time = risk_time_at_start, time2 = risk_time_at_end,
                   event = direct_dem) ~ rapid_positive_dem,
              type = "kaplan-meier",
              conf.type = "log",
              data = direct_democracy_c)

ggsurvplot(km, data = direct_democracy_c,
           conf.int = T,
           risk.table = T,
           break.x.by = 20,
           legend.labs = c("Fast democratization = 0",
                           "Fast democratization = 1"),
           legend.title = "")

# 9.5    Tools to interpret and present hazard ratios ####

cox_m1 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, direct_dem) ~
    rapid_positive_dem + rapid_negative_dem + memory + vdem_score,
  data = direct_democracy_c,
  method = "breslow"
)

ggforest(cox_m1)

test_cox_m1 <- cox.zph(cox_m1)
  
ggcoxzph(test_cox_m1, point.col = "black")

cox_m2 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, direct_dem) ~
    rapid_positive_dem + rapid_negative_dem + memory + vdem_score + capab_diffusion,
  data = direct_democracy_c,
  method = "breslow"
)
 
cox.zph(cox_m2)

cox_m3 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, direct_dem) ~
    rapid_positive_dem + rapid_negative_dem + memory + vdem_score + capab_diffusion + ocur_difussion,
  data = direct_democracy_c,
  method = "breslow"
)

cox.zph(cox_m3)

cox_m4 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, direct_dem) ~
    rapid_positive_dem + rapid_negative_dem + memory + vdem_score + capab_diffusion + ocur_difussion + log_pop,
  data = direct_democracy_c,
  method = "breslow"
)

cox.zph(cox_m4)

cox_m5 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, direct_dem) ~
    rapid_positive_dem + rapid_negative_dem + memory + vdem_score + capab_diffusion + ocur_difussion + log_pop + colony_gbr,
  data = direct_democracy_c,
  method = "breslow"
)

cox.zph(cox_m5)

cox_m5_int <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, direct_dem) ~
    rapid_positive_dem + rapid_negative_dem + memory + vdem_score + capab_diffusion + ocur_difussion + log_pop + colony_gbr + vdem_score:log(risk_time_at_end) + log_pop:log(risk_time_at_end),
  data = direct_democracy_c,
  method = "breslow"
)

cox.zph(cox_m5_int)

list_models <- list(cox_m1, cox_m2, cox_m3, cox_m4, cox_m5, cox_m5_int)  
         
texreg(
  list_models,
  custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 5b"),
  # adding custom names for the coefficients
  custom.coef.names = c("Quick Democratization",
                        "Quick Democracy Setback",
                        "Memory",
                        "Democracy",
                        "Capacity Diffusion",
                        "Occurency Diffusion",
                        "Population(ln)",
                        "Was a British Colony",
                        "Democracy x time at risk(ln)",
                        "Population(ln) x time at risk(ln)"),
  override.coef = map(list_models, ~exp(coef(.x))),
  override.se = map(list_models, ~odds_se(.x)),
  override.pvalues = map(list_models, ~odds_pvalues(.x)),
  caption = "Regression table for our survival models. Hazard ratios shown."
  )