#Chapter 4 Data Loading  ####

library(tidyverse)
library(politicalds)
library(haven)
library(readxl)
library(data.table)
library(ff)
library(janitor)

#4.1    Introduction ####

#' This chapter requires you to obtain datasets found here: "https://arcruz0.github.io/politicalds/data_load_politicalds.zip"
#' The following scripts will not work unless you have obtained and extracted the data as per the chapter's instructions

#4.3    Files separated by delimiters ####

df_desiguales_csv<- read_csv("data/desiguales.csv")  

ls()  

class(df_desiguales_csv)  

df_desiguales_csv %>% 
  select(1:10) %>% 
  glimpse()  

#4.3.2   Files created with R (.Rdata and .rds)

load("data/desiguales.Rdata") 

ls()  
  
df_desiguales_rds <- read_rds("data/desiguales.rds") 

ls()  

#4.3.3    Data sets with labels (from Stata or SPSS) ####

df_desiguales_spss <- read_spss("data/desiguales.sav")  


df_desiguales_stata <- read_stata("data/desiguales.dta",  
                                  # handling special characters  
                                  encoding = "UTF-8")  

ls()  

class(df_desiguales_spss)  

class(df_desiguales_stata)  

df_desiguales_spss %>% select(1:10) %>% glimpse()  
## Rows: 300  

head(df_desiguales_stata$p2) 
#Note that this code was written as "head(df_desiguales_stata $ (p2) which does not run. I have made a small correction to what I believe was the intended code.

df_desiguales_stata %>% select (region, p2)  
          
#4.3.4   Excel Files ####
        
df_cead_excel <- read_excel("data/cead.xls")  
        
ls()  

df_cead_excel %>% glimpse()  
      
df_cead_excel_v2 <- read_excel("data/cead.xls", skip = 18)  

df_cead_excel_v3 <- read_excel("data/cead.xls", range = "A20:G81") 

ls()  

class(df_cead_excel_v3) 

names(df_cead_excel_v3)

df_cead_excel_v4 <- df_cead_excel_v3 %>%
  rename(county = '...1') %>% 
  pivot_longer(cols = -county, names_to = "year", values_to = "n_crime") %>%
  filter(county != "Unidad Territorial") 
        
df_sinim_excel <- read_excel("data/sinim.xls",
                             sheet = 2, skip = 2 , na = "Not received") 


names(df_sinim_excel)  
     
df_sinim_excel_v2 <- df_sinim_excel %>% 
clean_names()
#' The above code as written does not produce later desired results, of "cleaning" column names.
#' I have added the piping and the clean_names() because I believe it to be what the author intended:
      
names(df_sinim_excel_v2)  

#4.4    Large tabular datasets ####
        
df_desiguales_large_100 <- read_csv("data/desiguales.csv", n_max = 100) 

df_desiguales_load_2vars <- read_csv("data/desiguales.csv",    
                                             col_types = cols_only_chr(c("edad",  
                                                                         "p2"))) 

df_desiguales_large_fread <- fread("data/desiguales.csv") 
        
class(df_desiguales_large_fread)  

df_desiguales_large_ff <- read.csv.ffdf(file = "data/desiguales.csv")
