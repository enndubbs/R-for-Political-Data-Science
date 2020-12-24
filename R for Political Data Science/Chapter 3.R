#Chapter 3 Data Visualization ####

library(ggrepel)
library(tidyverse)
library(politicalds)

#3.2 First Steps ####

#3.2.1.1     Data

data("municipal_data")

glimpse(municipal_data)


#3.2.1.2     Aesthetics 

ggplot(data    = municipal_data,  
       mapping = aes(x = year, y = poverty))  

#3.2.1.3    Geometrical Object 

ggplot(data    = municipal_data,  
       mapping = aes(x = year, y = poverty)) +  
  geom_boxplot()  

#3.2.1.4     Faceting 
  
ggplot(data   = municipal_data %>% filter(year == c(2004, 2008, 2012)),  
       mapping = aes(x = year, y = poverty)) +  
  geom_boxplot() +  
  facet_wrap(~ zone, nrow = 1)  

ggplot(data    = municipal_data %>% filter(year == c(2004, 2008, 2012)),  
       mapping = aes(x = year, y = poverty)) +  
  geom_boxplot() +  
  facet_wrap(zone ~ gender)  

ggplot(data    = municipal_data,  
       mapping = aes(x = year, y = poverty)) +  
  geom_boxplot() +  
  facet_grid(zone ~ gender)  

#3.2.1.5     Transformations 
  
  ggplot(data    = municipal_data %>% filter(year == c(2004, 2008, 2012)),  
         mapping = aes(x = poverty, y = income)) +  
  geom_point()  
  
  ggplot(data    = municipal_data %>% filter(year == c(2004, 2008, 2012)),  
         mapping = aes(x = poverty, y = income)) +  
  geom_point() +  
  scale_y_log10()  

#3.3 Applied example: Local elections and data visualization ####

#3.3.1    Graph bar 

plot_a <- ggplot(municipal_data,     mapping = aes(x = gender))  

plot_a +  
  geom_bar()  

plot_a +  
  geom_bar() +  
  facet_wrap(~year, nrow = 1)  

plot_a +  
  geom_bar(mapping = aes(y = ..prop.., group = 1)) +  
  facet_wrap(~zone, nrow = 1)  

plot_a +  
  geom_bar(mapping = aes(y = ..prop.., group = 1)) +  
  facet_wrap(~zone, nrow = 1) +  
  labs(title = "Proportion of men and women elected as mayors (2004-2012)",  
       subtitle = "By economic zones of Chile",  
       x = "Gender", y = "Proportion",  
       caption = "Source: Based on data from SERVEL and SINIM (2018)")  

plot_a +  
  geom_bar(mapping = aes(y = ..prop.., group = 1)) +  
  facet_wrap(~zone, nrow = 1) +  
  scale_x_discrete(labels = c("Men", "Women")) +  
  labs(title = "Proportion of men and women elected as mayors (2004-2012)",  
       subtitle = "By economic zones of Chile",  
       x = "Gender", y = "Proportion",  
       caption = "Source: Based on data from SERVEL and SINIM (2018)")  

#3.3.2    Line graph 

plot_b <- ggplot(data    = municipal_data,  
                   mapping = aes(x = year, y = income))  

plot_b +  
  geom_line()  

plot_b +  
  geom_line(mapping = aes(group = municipality))  
  
plot_b +  
  geom_line(aes(group = municipality)) +  
  facet_wrap(~zone, nrow = 2)  
  
means <- municipal_data %>%  
  group_by(zone) %>% 
  summarize(mean = mean(income, na.rm = T))  

plot_b +  
  geom_line(color = "lightgray", aes(group = municipality)) +  
  geom_hline(aes(yintercept = mean), data = means, color = "black") +  
  scale_x_discrete(expand = c(0,0)) +  
  scale_y_log10(labels = scales::dollar) +  
  facet_wrap(~ zone, nrow = 2) +  
  labs(title = "Municipal income in electoral years (2004-2012)",  
       y = "Income",  
       x = "Years") +  
  theme(panel.spacing = unit(2, "lines"))  

municipal_data %>%  
  group_by(zone) %>%  
  summarize(mean = mean(income, na.rm = T))  
  
#3.3.3   Boxplot 
  
  plot_c <- ggplot(data = municipal_data %>%  
                     filter(year %in% c(2004, 2008, 2012)),  
                   mapping = aes(x = income, y = zone)) +  
  geom_boxplot() +  
  facet_wrap(~year, ncol = 1)  

plot_c  

plot_c +  
  geom_text(data = municipal_data %>% filter(income > 50000000),  
            mapping = aes(label = municipality))  


plot_c + 
  geom_text_repel(data  = municipal_data %>% filter(income > 50000000),  
                  mapping = aes(label = municipality))  

plot_c +  
  geom_text_repel(data = municipal_data %>%  
                    filter(income > 50000000),  
                  mapping = aes(label = municipality)) +  
  scale_x_continuous(labels = scales::dollar) +  
  labs(title = "Municipal income by zone (2004-2012)",  
       x = "Income", y = "Zone",  
       caption = "Source: Based on data from SINIM (2018)")  
  
#3.3.4   Histogram 

ggplot(data    = municipal_data,  
       mapping = aes(x = income)) +  
  geom_histogram()  
  
ggplot(data   = municipal_data,
       mapping = aes(x = income)) +  
  geom_histogram(bins = 50) +  
  scale_x_continuous(labels = scales::dollar)  

ggplot(data    = municipal_data %>% filter(income < 50000000),  
       mapping = aes(x = income, fill = zone)) +  
  geom_histogram(alpha = 0.5, bins = 50) +  
  scale_x_continuous(labels = scales::dollar) +  
  labs(title = "Number of municipalities according to their annual income",  
       subtitle = "Chile (2004-2012)",  
       x = "Income", y = "Number of municipalities",  
       caption = "Source: Based on data from SINIM (2018)")  

#3.3.5    Relation between variables 

plot_f <- ggplot(data    = municipal_data,
                 mapping = aes(x = poverty, y = log(income)))  

plot_f +  
  geom_smooth(method = "lm", color = "black")  

plot_f +  
  geom_point(alpha = .2) +
#Noah's note: this formula and subsequent variations are incomplete in the book, they read "geom_point(alpha = +" with no end parenthesis or specification of the alpha value. I've used an alpha of .2 which gives a result approximately similar to the book's example.
  geom_smooth(method = "lm", color = "black")  
          
             
plot_f +  
  geom_point(alpha = .2) +
  geom_smooth(method = "lm", color = "black") +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "Relation between municipality income and its poverty rate",
       subtitle = "Chile (2004-2012)",
       x = "CASENs poverty rate",
       y = "Income",
       caption = "Source: Based on data from SINIM (2018)")  
                          
cor(municipal_data $poverty, municipal_data$income,
    use = "pairwise.complete.obs")  
                          
plot_f + 
  geom_point(alpha = .2) +
  geom_smooth(method = "lm", color = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "Relation between municipality income and CASENs poverty rate",
       subtitle = "Chile (2004-2012)",
       x = "CASENs poverty rate ", 
       y = "Income",
       caption = "Source: Based on data from SINIM (2018)") +
  annotate(geom = "text", x = 50, y = 15, label = "Correlation: -0.27")  