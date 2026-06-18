
# Code written by Sofia Gil-Clavel for the course "R for Social Scientists".
# First version: 2026/06/18

# install.packages("httr2")

rm(list=ls())
gc()

library(tidyverse)
library(httr2)

#### Retrieving data using the Guardian API ####

# Save your API key/token in the following variable as a string:

KEY= "YOUR API KEY/TOKEN"

# Adding the endpoint:

req<-request("?")

# Creating the query:

query="(artificial intelligence) OR (chat-gpt) OR (open AND ai) OR (llm) OR (large language model*)"

# Adding the body:

resp <- req |>
  req_method("GET") |> # request method
  req_headers("Accept" = "application/json",
              "api-key" = KEY) %>%
  req_url_query("q" = query,
                "from-date" = "2014-01-01",
                "to-date" = "2014-06-01")%>%
  req_perform()

# Now, we can save the response as a json and then turn the json into a tibble:

json_response <- resp_body_string(resp)


resp_df <- jsonlite::fromJSON(json_response, flatten = TRUE)

TheGuardianAI<-tibble(resp_df$response$results)

# Finally, we can save the result:

write.csv(TheGuardianAI,"LOCATION TO SAVE/TheGuardian.csv")

#### Using functions ####

# Exercise 2.1: Addition of numbers between 1 and n

# ADDITION<-function(?){
#   # ?
#   return(?)
# }

ADDITION(10)

# Exercise 2.2: Retrieval function

# Now, let's turn our code into a function. That way we can make some 
# changes to it and pass them via the function's parameters.

# RETRIEVAL<-function(?, ?, ?, ?){
#   
#   # ?
#   
#   return(?)
# }

RETRIEVAL(query=query, from_date= "2014-01-01", 
          to_date= "2014-01-02", key=KEY)

#### Using conditions and loops ####

#*** If-Else statements ***#

# Exercise 3.1: 

# ifelse(?,?,?)

# Exercise 3.2:

# VARIABLE<-?
# 
# if(?>?){
#   # ?
# }

# Exercise 3.3:

# VARIABLE<-?
# 
# if(?>?){
#   # ?
# }else{
#   # ?
# }

#*** Loops ***#

# Exercise 3.4:

# a2e<-c(?)
# 
# for(? in ?){
#   # ?
# }

# Exercise 3.5:

# count<-?
# 
# while(?){
#   # ?
# }

# Exercise 3.6:

# count<-?
# 
# repeat{
#   if(?){
#     # ?
#   }
#   # ?
# }

#### Handling errors ####

KEY2= "Wrong Key"

#*** You use an invalid key ***#

RETRIEVAL(query,"2014-01-01", "2014-06-01", key = KEY2)

# Exercise 4.1:

# message<-try(? , silent = TRUE)
# 
# if(?){
#   # ?
# }

# Exercise 4.2:

# if(?){
#   # ?
# }else if(?){
#   # ?
# }

# Exercise 4.3:

# while(?){
#   if(?){
#     # ?
#   }else if(?){
#     # ?
#   }
# }


#### RETRIEVAL in production ####






