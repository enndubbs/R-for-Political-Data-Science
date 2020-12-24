#12 Web Mining ####

library(tidyverse)
library(glue)
library(rvest)
library(rtweet)

# 12.3 Web scraping in R ####

#12.3.1 Applied example: the Organization of American States (OAS) press releases

#The OAS website wasn't working when I wrote this script, so I can't vouch that it works without error.

dir.create("webs") # create a "webs/" folder to store the file  
download_html(url = "https://www.oas.org/en/media_center/press_releases.asp?nMes=10&nAnio=2019",  
              file = "webs/pr_oas_10_2019.html")  

#12.3.2 Load the html to work with R

web_pr_10_2019 <- read_html("webs/pr_oas_10_2019.html")  

#12.3.3  Extract the information with html_nodes() + html_text() 

titles_web_pr_10_2019 <- web_pr_10_2019 %>%  
  html_nodes(".itemmenulink") %>%  
  html_text()  

head(titles_web_pr_10_2019, n = 3)  

links_web_pr_10_2019 <- web_pr_10_2019 %>%  
  html_nodes(".itemmenulink") %>%  
  html_attr("href") %>%  
  str_c("https://www.oas.org/en/media_center/", .)  

links_web_pr_10_2019  

df_web_pr_10_2019 <- tibble(  
    title = titles_web_pr_10_2019,  
    link = links_web_pr_10_2019  
  )  

df_web_pr_10_2019  

#12.3.4  Iterations ####

print(1)  

walk(.x = 1:10,  
       .f = ~ {  
         print(.x)  
       })  

iteration_df <- cross_df(list(month = 1:12, year = 2017:2019))  

walk2(.x = iteration_df$month,  
      .y = iteration_df$year,  
      .f = ~ {  
        Sys.sleep(2) # stops  
        download_html(  
          url = glue("https://www.oas.org/en/media_center/press_releases.  
              asp?nMes={.x}&nAnio={.y}"),  
          file = glue("webs/pr_oas_{.x}_{.y}.html"))  
      })  

#12.3.5  Custom functions (recipes) 

f_process_site <- function(file)  {
    web <- read_html(file, encoding = "UTF-8")  

titles <- web %>%  
  html_nodes(".itemmenulink") %>%  
  html_text()  

links <- web %>%  
  html_nodes(".itemmenulink") %>%  
  html_attr("href") %>%  
  str_c("https://www.oas.org/en/media_center/", .)  

df_info <- tibble(  
  title = titles,  
  link = links  
)  

return(df_info) # what returns/delivers the function  
}  

f_process_site(file = "webs/pr_oas_10_2017.html")  

files <- list.files("webs/", full.names = T)  

files  

df_web_pr_2017_2019 <- map_dfr(.x = files, .f = ~ {f_process_site(.x)})  
df_web_pr_2017_2019  
  
#12.4  Using APIs and extracting data from Twitter ####

#12.4.2  Extract the data 

friends_ONU_es <- get_friends("ONU_es")  
friends_ONU_es  

info_friends_ONU_es <- lookup_users(friends_ONU_es$user_id)  

info_friends_ONU_es  

followers_ONU_es <- get_followers("ONU_es",  
                                  n = 200, # we'll only get 200 followers  
                                  retryonratelimit = T)  

info_followers_ONU_es <- lookup_users(followers_ONU_es$user_id)  

info_followers_ONU_es  

#12.4.2.1 Searching for specific tweets 

pinera_tweets <- search_tweets(q ="#piñera",  
                               n = 1000)  

head(pinera_tweets, n = 3)  

lookup_users(pinera_tweets$user_id)  

query_1 <- search_tweets(q = "piñera", n = 20)  

query_2 <- search_tweets(q = "piñera economia", n = 20)  

query_3 <- search_tweets(q = "piñera AND bachelet", n = 20)  

query_4 <- search_tweets("bolsonaro OR piñera OR macri", n = 20)  

query_5 <- search_tweets("piñera", lang = "es", include_rts = FALSE, n=20)  

#12.4.3  Downloading features related to Twitter users 

#12.4.3.1 Retweets 

pinera_tweets_no_rts <- search_tweets("#piñera",  
                                      n = 1000,  
                                      include_rts = F)  
  
head(pinera_tweets_no_rts, n = 3)  

head(unique(pinera_tweets$screen_name))  

pinera_users <- search_users("#piñera",  
                             n = 1000)  
  
head(pinera_users, n = 2)  


length(unique(pinera_users$location))  

pinera_users %>%  
  count(location, sort = TRUE) %>%  
  mutate(location = reorder(location, n)) %>%  
  top_n(20) %>%  
  ggplot(aes(x = n, y = location)) +  
  geom_col() +  
  labs(x = "Frequency", y = "Location")  

pinera_users %>%  
  count(location, sort = T) %>%  
  mutate(location = reorder(location, n)) %>%  
  na.omit() %>% filter(location != "") %>%  
  top_n(20) %>%  
  ggplot(aes(x = n, y = location)) +  
  geom_col() +  
  labs(x = "Frequency", y = "Location")  

bachelet_tweets <- search_tweets(  
    "#bachelet", n = 1000, include_rts = F  
  )  

bachelet_tweets %>%  
  ts_plot("3 hours") +  
  labs(  
    x = "", y = "Frequency",  
    caption = "Source: Data collected from the Twitter API."  
  ) 
