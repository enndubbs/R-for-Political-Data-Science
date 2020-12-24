#15 Principal Component Analysis ####

library(tidyverse)
library(politicalds)
library(FactoMineR)
library(stats)
library(factoextra)
library(GGally)

#15.3     Basic notions in R ####

data("lapop")  

lapop <- lapop %>%  
  group_by(country_name) %>%  
  mutate(trust_elections_prom = mean(trust_elections)) %>% 
ungroup()  

ggplot(lapop, aes(x = trust_elections)) +  
  geom_histogram() +  
  scale_x_continuous(breaks = 1:7) +  
  labs(title = "Trust in elections",  
       x = "The national average is expressed as a dashed line.",  
       y = "Frequency")+  
  facet_wrap(~country_name) +  
  geom_vline(aes(xintercept = trust_elections_prom),  
             color = "black", linetype = "dashed", size = 1)  

lapop_num <- lapop %>%  
  select(justify_coup, justify_cong_shutdown, trust_institutions,  
         trust_courts, trust_congress, trust_president, trust_parties,  
         trust_media, trust_elections, satisfied_dem, vote_opposers,  
         demonstrations) %>% 
mutate_all(as.numeric)  

lapop_num  

lapop_num <- lapop_num %>%  
  scale() %>%  
  na.omit() %>%  
  as_tibble()  

corr_lapop <- lapop_num %>%  
  # calculate correlation matrix and round to 1 decimal place:  
  cor(use = "pairwise") %>%  
  round(1)  

#15.4    Dimensionality of the concept ####

pca <- princomp(lapop_num)  

summary(pca, loadings = T, cutoff = 0.3)  

eig_val <- get_eigenvalue(pca)  

eig_val  

fviz_eig(pca, addlabels = T, ylim = c(0, 50))  

fviz_eig(pca, choice = c("eigenvalue"), addlabels = T, ylim = c(0, 3))  

#In the below code ggtheme is set to 'th', which is clearly an error. I've filled it in as 'theme_minimal()'

fviz_eig(pca, choice = c("eigenvalue"), addlabels = T, ylim = c(0, 3),  
         ggtheme = theme_minimal(), barfill = "darkgray", barcolor = "darkgray")  

fviz_pca_biplot(pca, repel = F, col.var = "black", col.ind = "gray")  

fviz_contrib(pca, choice = "var", axes = 1, top = 10)  
fviz_contrib(pca, choice = "var", axes = 2, top = 10)  
fviz_contrib(pca, choice = "var", axes = 3, top = 10)  

lapop <- bind_cols(lapop, as_tibble(pca$scores))  

#15.5    Variation of the concept ####

pca_1 <- PCA(lapop_num, graph = F)  

fviz_eig(pca_1, choice = "eigenvalue", addlabels = T, ylim = c(0, 3.5))  

eig <- get_eig(pca_1)  

eig  

data_pca <- pca_1$ind$coord %>%  
  as_tibble() %>% 
mutate(pca_01 = (Dim.1 * 28.7 + Dim.2 * 12.3 + Dim.3 * 10.3 +  
                   Dim.4 * 7.9) / 60)  

#in the above command in the book there is an error: the select statement has no variables indicated.

lapop <- bind_cols(lapop, data_pca)

lapop <- lapop %>%  
                     mutate(democracy_index = GGally::rescale01(pca_01) * 100) %>%  
                     select(democracy_index, everything())  
                   
index_density <- ggplot(data = lapop,
                        mapping = aes(x = democracy_index)) +  
                     labs(x = "Index of trust in democracy", y = "Density") +  
                     geom_density()  
                   
index_density  
                   
lapop <- lapop %>%  
                     group_by(country_name) %>%  
                     mutate(democracy_avg = mean(democracy_index)) %>%  
                     ungroup()  
                   
ggplot(lapop, aes(x = democracy_index)) +  
                     geom_density() +  
                     labs(title = "Trust in democracy in Latin America (N = 7000)",  
                          x = "Trust in democracy",  
                          y = "Density") +  
                     facet_wrap(~country_name) +  
                     geom_vline(aes(xintercept = democracy_avg),  
                                color = "black", linetype = "dashed", size = 1)  

