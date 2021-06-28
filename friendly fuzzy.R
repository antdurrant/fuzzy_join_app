new <- tibble::tribble(
                ~name,                ~food,
               "Adma",          "mince pie",
               "Alan",          "pizza pie",
              "Aaalice", "lemon meringue pie"
              )

master <- tibble::tribble(
          ~id,    ~name,
         123L,   "Adam",
         124L,   "Alan",
         134L,  "Alice",
         145L,   "Aman",
         144L, "Alicia"
         )

library(tidyverse)
library(fuzzyjoin)
library(usethis)

  
  friendly_fuzzy_join <- function(user_master, new_data, master_id = id){
    
    # exact matches
  res_1 <- inner_join(user_master, new_data) %>% 
    mutate(join_type = "exact") %>% 
    mutate(name_new_data = name)
  
  remainder <- anti_join(new_data, res_1) %>% rename(name_new_data = name)
  
    # tell the user what's going on
  ui_done("{nrow(res_1)} field(s) matched on exact matches. Preview below:")
  print(head(res_1))
  
  ui_todo("{nrow(new_data) - nrow(res_1)} unmatched rows remaining. Preview below: ")
  print(head(remainder))
  
  
  ui_todo("Attempting join with string distance of 1")
  
    # semi-match 1
  res_2 <- stringdist_inner_join(user_master, 
                                 remainder,
                                 by = c("name" = "name_new_data"),
                                 max_dist = 1) %>% 
    mutate(join_type = "stringdist 1") 
  
  res <- bind_rows(res_1, res_2) %>%
    add_count({{master_id}}, name = "id_count")
  
  remainder <- anti_join(remainder, res_2) 
  
  # tell the user what's going on
  ui_done("{nrow(res_2)} field(s) matched on string distance of 1. Preview below:")
  print(head(res_2))
  
  ui_warn("{nrow(res %>% filter(id_count > 1))} fields found with more than one match in original data")
  ui_todo("{nrow(new_data) - nrow(res)} unmatched rows remaining. Preview below:")
  
  print(head(remainder))
  
  ui_todo("Attempting join with string distance of 2")
  
  # semi-match 2
  res_3 <- stringdist_inner_join(user_master, 
                                 remainder,
                                 by = c("name" = "name_new_data"),
                                 max_dist = 2) %>% 
    mutate(join_type = "stringdist 2") 
  
  res <- bind_rows(res, res_3) %>% 
    select(-id_count) %>%
    add_count({{master_id}}, name = "id_count")
  
  remainder <- anti_join(remainder, res_3) 
  
    # tell the user what's going on
  ui_done("{nrow(res_3)} field(s) matched on string distance of 2")
  ui_warn("{nrow(res %>% filter(id_count > 1))} fields found with more than one match in original data")
  ui_todo("{nrow(new_data) - nrow(res)} unmatched rows remaining.
          Check `result$matches` for matched results and `result$remainder` for unmatched results you will have to fix by hand. ")
  
     # return matches and non-matches in a list of two dataframes
  output <- list()
  output$matches <- res
  output$remainder <- remainder
  
  output
  }
  
  friendly_fuzzy_join(new_data = new, user_master = master)
