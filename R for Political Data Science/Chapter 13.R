#13 Quantitative Analysis of Political Texts ####

#Note that there were a lot of missing pipes in this chapter, I didn't notate them in the code below.

library(tidyverse)
library(politicalds)
library(lubridate)
library(skimr)
library(ggwordcloud)
library(tidytext)
library(stopwords)
library(quanteda)
#I couldn't install quanteda.textmodels without RSSL
library(RSSL)
library(quanteda.textmodels)
library(qdapRegex)
library(stm)
library(remotes)    
#remotes::install_github("mikajoh/tidystm")
library(tidystm)

#13.1    Analysis of political hashtags ####
  
#13.1.1     Twitter data exploration 
 
data("poltweets")  
  
skim(poltweets)
  
poltweets %>% count(gender) %>% mutate(freq = n / sum(n))  
  
poltweets %>% 
  distinct(screen_name, gender) %>% 
  count(gender)  
   
poltweets_hashtags <- poltweets %>%  
      unnest_tokens(output = "hashtag", input = "text", token = "tweets") %>%  
      filter(str_starts(hashtag, "#"))  
    
poltweets_hashtags <- poltweets_hashtags %>%  
      mutate(fem_hashtag = case_when(str_detect(hashtag, "femi") ~ 1,  
                                     str_detect(hashtag, "niunamenos") ~ 1,  
                                     str_detect(hashtag, "aborto") ~ 1,  
                                     str_detect(hashtag, "mujer") ~ 1,  
                                     str_detect(hashtag, "genero")~ 1,   
                                     TRUE ~ 0)) %>%  
      mutate(fem_hashtag = as.character(fem_hashtag))  
    
poltweets_hashtags %>% count(fem_hashtag) %>% mutate(freq = n / sum(n))  
    
poltweets_hashtags %>%  
      filter(fem_hashtag == "1") %>%  
      count(hashtag) %>%  
      arrange(-n)  

#13.1.3    Most-used hashtags
    
poltweets_hashtags %>%  
        count(hashtag, fem_hashtag) %>%  
        arrange(-n) %>%  
        slice(1:20)  

#13.1.4    Wordclouds 
      
data_hashtags_wordcloud <- poltweets_hashtags %>% 
      count(hashtag, fem_hashtag) %>% 
      arrange(-n) %>%  
        slice(1:35)  
      
      
ggplot(data_hashtags_wordcloud,  
             aes(label = hashtag, size = n, color = fem_hashtag)) +  
        geom_text_wordcloud() +  
        scale_size_area(max_size = 8) + # we set a maximum size for the text  
        theme_void()  
      
ggplot(poltweets_hashtags %>%  
                 count(hashtag, gender, fem_hashtag) %>%  
                 arrange(-n) %>%  
                 group_by(gender) %>%  
                 slice(1:20),  
               aes(label = hashtag, size = n, color = fem_hashtag)) +  
          geom_text_wordcloud() +  
          scale_size_area(max_size = 6) +  
          facet_wrap(~gender)  
        
ggplot(poltweets_hashtags %>%  
                   count(hashtag, coalition, fem_hashtag) %>%  
                   arrange(-n) %>%  
                   group_by(coalition) %>%  
                   slice(1:20),  
                 aes(label = hashtag, size = n, color = fem_hashtag)) +  
          geom_text_wordcloud() +  
          scale_size_area(max_size = 6) +  
          facet_wrap(~coalition, nrow = 3)  
        
#13.1.6   Barplots 
        
plot_15 <- poltweets_hashtags %>%  
            group_by(gender) %>%  
            count(hashtag, fem_hashtag)%>%  
            arrange(-n) %>%  
            slice(1:15)  
          
          
ggplot(data = plot_15,  
                 mapping = aes(x = n, y = reorder_within(hashtag, n, gender),  
                               fill = fem_hashtag)) +  
            geom_col() +  
            labs(x = "Frequency", y = "", fill = "Feminist hashtag") +  
            facet_wrap(~gender, scales = "free", nrow = 2) +  
            scale_y_reordered()  
          
hash_tf_idf <- poltweets_hashtags %>%  
  count(coalition, hashtag, fem_hashtag, sort = T) %>%  
              bind_tf_idf(term = hashtag, document = coalition, n = n) %>%  
              # get 10 most distinctive hashtags per coalition:  
              arrange(-tf_idf) %>%  
              group_by(coalition) %>%  
              slice(1:10)  
            
ggplot(data = hash_tf_idf,  
                   mapping = aes(x = tf_idf,  
                                 y = reorder_within(hashtag, tf_idf, coalition),  
                                 fill = fem_hashtag)) +  
              geom_col() +  
              labs(x = "tf_idf", y = "", fill = "Feminist Hashtag") +  
              facet_wrap(~coalition, nrow = 3, scales = "free") +  
              scale_y_reordered()  

#13.1.7 Temporal variation in the use of hashtags

hashtags_weekly <- poltweets_hashtags %>%  
              mutate(week = floor_date(created_at, "week", week_start = 1)) %>%  
              filter(hashtag %in% c("#aborto3causales",  
                                    "#leydeidentidaddegeneroahora")) %>%  
              count(hashtag, week)

ggplot(data = hashtags_weekly,  
                   mapping = aes(x = week, y = n,  
                                 linetype = hashtag, group = hashtag)) +  
              geom_point() +  
              geom_line() +  
              labs(x = "Week", y = "Total weekly use", linetype = "Hashtag")  
            
#13.2    Wordﬁsh ####

#13.2.1 Inspection and data cleaning

glimpse(poltweets)  
            
#13.2.2 Preprocessing 
            
f_remove_accent <- function(x){  
              x %>%  
                str_replace_all("á", "a") %>%  
                str_replace_all("é", "e") %>%  
                str_replace_all("í", "i") %>%  
                str_replace_all("ó", "o") %>%  
                str_replace_all("ú", "u") %>%  
                str_replace_all("ñ", "n") # also replace "ñ", a common letter in Spanish  
            }  
            
poltweets <- poltweets %>%  
              mutate(text = text %>%  
                       # delete user names (which start with @):  
                       str_remove("\\@[[:alnum:]]+") %>%  
                       # delete URLs:  
                       str_remove_all("http[\\w[:punct:]]+") %>%  
                       # all text to lowercase:  
                       str_to_lower() %>%  
                       # remove special characters:  
                       str_remove_all("[\\d\\.,_\\@]+") %>%  
                       f_remove_accent() %>%  
                       # remove emojis  
                       rm_non_ascii()  
              )  
            
by_coalition <- poltweets %>%  
              group_by(coalition) %>%  
              summarize(text = str_c(text, collapse = " ")) %>%  
              ungroup() %>%  
              # reorder the variable:  
              mutate(coalition = fct_relevel(as.factor(coalition), "FA", "LFM", "ChV"))  
            
poltweets_corpus <- corpus(by_coalition)  
            
poltweets_dfm <- dfm(poltweets_corpus,  
                                 remove_numbers = T, remove_punct = T,  
                                 remove_symbols = T, remove = stopwords("spa"))  
            
poltweets_dfm_trimmed <- dfm_trim(  
              poltweets_dfm,  
              min_docfreq = 0.05, max_docfreq = 0.95,  
              docfreq_type = "quantile" # min 5% / max 95%  
            )  
            
#13.2.3    Wordﬁsh 
            
wf <- textmodel_wordfish(poltweets_dfm_trimmed,
                         dir = c(3, 1),
                         sparse = T)  
            
df_wf <- tibble(  
              # coalition labels:  
              coalition = wf[["x"]]@docvars[["coalition"]],  
              # then we extract thetas and their SEs from the mode object:  
              theta = wf$theta,  
              lower = wf$theta - 1.96 * wf$se.theta,  
              upper = wf$theta + 1.96 * wf$se.theta  
            )  
            
df_wf  

ggplot(data = df_wf,  
                   mapping = aes(x = theta, y = fct_reorder(coalition, theta),  
                                 xmin = lower, xmax = upper)) +  
              geom_point() +  
              geom_linerange() +  
              # add vertical line at x=0:  
              geom_vline(xintercept = 0, linetype = "dashed") +  
              scale_x_continuous(limits = c(-1.2, 1.2)) +  
              labs(y = "")  
            
#13.3     Structural Topic Modeling ####
          
#13.3.1     Pre-processing 
            
poltweets_onemonth <- poltweets %>%
  filter(created_at >= "2018-05-01" & created_at > "2018-06-01")  
            
names_surnames <- c(poltweets$names, poltweets$lastname) %>%  
              na.omit() %>%  
              unique() %>%  
              str_to_lower() %>%  
              f_remove_accent() %>%  
              str_split(" ") %>%  
              flatten_chr()  
            
poltweets_words <- poltweets_onemonth %>% 
            unnest_tokens(word, text, "words") %>% 
            # remove stop words:  
            filter(!word %in% stopwords::stopwords("es", "stopwords-iso")) %>%  
              # remove names/surnames of deputies:  
              filter(!word %in% names_surnames) %>%  
              # just keep words that are present ten or more times  
              add_count(word) %>%  
              filter(n > 10)  
            
poltweets_stm <- poltweets_words %>%  
cast_dfm(status_id, word, n) %>% 
convert(to = "stm")  
            
metadata <- tibble(status_id = names(poltweets_stm$documents)) %>%  
              left_join(distinct(poltweets, status_id, coalition, gender),  
                        by = "status_id") %>%  
              as.data.frame()  
            
poltweets_stm$meta <- metadata  
            
summary(poltweets_stm)  

#13.3.2    Diagnostics 
            
stm_search <- searchK(documents = poltweets_stm$documents,  
                                  vocab = poltweets_stm$vocab,  
                                  data = poltweets_stm$meta,  
                                  # our covariates, mentioned above:  
                                  prevalence = ~ coalition + gender,  
                                  # 5-50 topics range:  
                                  K = seq(5, 50, by = 5),  
                                  # use all our available cores (be careful!):  
                                  # Editor's note: this function did not work on my computer unless I deleted the below line
                                  cores = parallel::detectCores(),  
                                  # a seed to reproduce the analysis:  
                                  heldout.seed = 123) 
              
diagnostics <- stm_search$results  %>%  
              # get a tidy structure to plot:  
              mutate_all(flatten_dbl)  %>%  
              pivot_longer(-K, names_to = "diagnostic", values_to = "value")  %>%  
              # we will only use some diagnostics:  
              filter(diagnostic %in% c("exclus", "heldout", "residual", "semcoh")) %>%  
              # give better names to the diagnostics:  
              mutate(diagnostic = case_when(  
                diagnostic == "exclus" ~ "Exclusivity",  
                diagnostic == "heldout" ~ "Held-out likelihood",  
                diagnostic == "residual" ~ "Residuals",  
                diagnostic == "semcoh" ~ "Semantic coherence"  
              ))  
            
            
ggplot(diagnostics, aes(x = K, y = value)) +  
              geom_point() +  
              geom_line() +  
              facet_wrap(~diagnostic, scales = "free")  
            
stm_model_k10 <- stm(documents = poltweets_stm$documents,  
                                   vocab = poltweets_stm$vocab,  
                                   data = poltweets_stm$meta,  
                                   prevalence = ~ coalition + gender,  
                                   K = 10)  
            
stm_model_k25 <- stm(documents = poltweets_stm$documents,  
                                 vocab = poltweets_stm$vocab,  
                                 data = poltweets_stm$meta,  
                                 prevalence = ~ coalition + gender,  
                                 K = 25)  
            
diagnostics2 <- tibble(  
              exclusivity = c(exclusivity(stm_model_k10), exclusivity(stm_model_k25)),  
              semantic_coherence = c(  
                semanticCoherence(stm_model_k10, documents = poltweets_stm$documents),  
                semanticCoherence(stm_model_k25, documents = poltweets_stm$documents)  
              ),  
              k = c(rep("K=10", 10), rep("K=25", 25))  
            )  
            
            
ggplot(data = diagnostics2,  
                   mapping = aes(x = semantic_coherence, y = exclusivity, shape = k)) +  
              geom_point(size = 2) +  
              labs(x = "Semantic coherence", y = "Exclusivity", shape = "")  
            
#13.3.3   Analysis 
            
model_terms <- tibble(  
              topic = as.character(1:10),  
              # obtain the top seven terms:  
              terms = labelTopics(stm_model_k10)$prob %>%  
                t() %>%  
                as_tibble() %>%  
                map_chr(str_c, collapse = ", "),  
              # expected proportion of each topic in the whole set of tweets:  
              expected_proportion = colMeans(stm_model_k10$theta)  
            ) %>%  
              arrange(-expected_proportion)  
            
ggplot(data = model_terms,  
                   mapping = aes(x = expected_proportion,  
                                 y = fct_reorder(topic, expected_proportion),  
                                 label = terms)) +  
              geom_col() +  
              geom_text(size = 3, hjust = "inward") + # to use space better  
              labs(x = "Expected proportion", y = "Topic")  
            
labelTopics(stm_model_k10,  
                        topics = 2) 

stm_effects <- estimateEffect(  
                # c(1:10) means that we want coefficients for all topics in our model  
                formula = c(1:10) ~ coalition + gender,  
                stmobj = stm_model_k10,  
                metadata = poltweets_stm$meta  
              )  
            
effect_gender <- extract.estimateEffect(x = stm_effects,  
                                                    covariate = "gender",  
                                                    method = "difference",  
                                                    cov.value1 = "Female",  
                                                    cov.value2 = "Male",  
                                                    model = stm_model_k10)  
effect_gender  %>% arrange(-estimate)  
            
ggplot(effect_gender,  
                   aes(x = estimate, xmin = ci.lower, xmax = ci.upper,  
                       y = fct_reorder(as.character(topic), estimate))) +  
              # add line of null effect:  
              geom_vline(xintercept = 0, linetype = "dashed") +  
              geom_point() +  
              geom_linerange() +  
              # center the null effect line:  
              scale_x_continuous(limits = c(-0.075, 0.075)) +  
              labs(x = "Estimate for the Female - Male difference",  
                   y = "Topic")  
            
effect_coalition_diff <- extract.estimateEffect(x = stm_effects,  
                                                            covariate = "coalition",  
                                                            method = "difference",  
                                                            cov.value1 = "FA",  
                                                            cov.value2 = "ChV",  
                                                            model = stm_model_k10)  

effect_coalition_diff %>%  
              filter(topic == 2)  

