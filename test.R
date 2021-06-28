a <- read_csv(file.choose())
b <- read_csv(file.choose())


check_joins <- function(base = a, add = b, on = c("id", "name")){
matching_colnames <- colnames(base)[colnames(add) %in% colnames(base)]  
  
matched <- inner_join(base, add)

print(matched)

if(usethis::ui_yeah("Showing exact matches. Try partial matches?" ,
                    n_yes = 1,
                    n_no = 1,
                    yes = "yes",
                    no = "no",
                    shuffle = FALSE
                    )){
  
  remainder <-  anti_join(add, matched)
  print("remainders:")
  print(remainder)
  syms(on)

  additional <- remainder %>% 
    stringdist_inner_join( base %>% select(-on[[1]]), 
                           max_dist = 1,
                           method = "lv")  %>%
    inner_join(base, by = c(on[1])
                            )
               )
  
  
} 

d<- anti_join(b, a)

e <- bind_rows(c,
               stringdist_inner_join(a,b %>% select(-id), max_dist = 1, method = "lv") %>%
                   select(-name.y) %>%
                   rename(name = name.x ) %>%
                   inner_join(b %>% rename(name_join = name), by = c("id" = "id", "score" = "score")) %>%
                   distinct())


f <- anti_join(b, a) %>%
    mutate(name_join = name) %>%
    select(-name) %>%
    anti_join(e)
