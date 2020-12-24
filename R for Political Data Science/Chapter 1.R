#1.4 Objects and Functions ####

# This is a sterile command. R knows that it is only a comment! 
    
2 + 2 # This is an appendix-command, it does not modify the code  
  
object_1 <- 2 + 2  
  
object_1  

object_1 + 10  
  
object_1 <- "democracy"  
  
object_1  

rm(list = ls()) 
  
#1.4.1 Vectors ####

vector_1 <- c(15, 10, 20)  

vector_1  

vector_2 <- c(9, 7:10, 2, 14)  

vector_2  

vector_2[2] # it gives us the second element 

vector_2[4:6] # it gives us the fourth, fifth and sixth element. 
  
#1.4.2 Functions ####
  
2 + sqrt(25) - log(1) # equivalent to 2 + 5 + 0 

mean(vector_1)    # mean 
 
median(vector_1)  # median 

sd(vector_1)      # standard deviation 

sum(vector_1)     # sum 

min(vector_1)     # minimum value 

max(vector_1)     # maximum value 

log(vector_1)     # natural logarithm 

exp(vector_1)     # exponential 

length(vector_1)  # length (amount of values) 

sort(vector_1)    # orders the vector from smallest to largest: 

sort(vector_1, decreasing = TRUE)  # orders the vector from largest to smallest: 

vector_1_with_na <- c(vector_1, NA)  

vector_1_with_na  

mean(vector_1_with_na)  

mean(vector_1_with_na, na.rm = TRUE)  
  
na.omit(vector_1_with_na)  

is.na(vector_1_with_na)  

#1.4.3 Files / graphs / packages / help  ####
#1.4.3.3 Installing packages

install.packages("tidyverse")  

library(tidyverse)  

install.packages("remotes")  

library(remotes)  

install_github("arcruz0/politicalds")  

library(politicalds)  

