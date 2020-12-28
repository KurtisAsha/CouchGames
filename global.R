# FRONT PAGE ####

# Created by    Kurtis Smith    
# Created on    2020-10-03
# R Version     4.0.3 (2020-04-24)

# LOAD PACKAGES ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,     # For loading packages
               httr,       # To call api url
               jsonlite,   # For dealing with json data
               tidyverse,  # For so many reasons
               lubridate)  # For working with dates
              
               
base <- "https://api.rawg.io/api/games?tags=co-op&page_size=40&page="
df <-  tibble (id=integer(),
               name=character(),
               released=lubridate::ymd(),
               rating=double(),
               ratings_count=integer(),
               metacritic=integer(),
               playtime=integer(), 
               genres=list(character()),
               platforms=list(character()))

# CALL API, LOOP THROUGH PAGES & COMBINE ####

for(i in 1:500){
  
  call      <- paste(base,i, sep="")                        # Creating incremental url i = page number
  resp      <- httr::GET(call)                              # Make the API call
  
  if (status_code(resp) == 404) {                           # Iterate until 404 Status Code
    print(status_code(resp))
    break
  }
  
  txtdata   <- httr::content(resp, as = 'text',
                             encoding = "UTF-8")            # Parsing it to JSON
  txtdata2  <- jsonlite::fromJSON(txtdata)                  # Convert from JSON to a list
  df2   <- txtdata2$results %>%
    mutate(released = lubridate::ymd(released)) %>%
    select(id,
           name,
           released,
           rating,
           ratings_count,
           metacritic,
           playtime, 
           genres,
           platforms) %>% 
  filter(!is.na(released))
  df <- rbind(df, df2)                                    # Combine with df
  rm(call, resp, txtdata, txtdata2, df2)                  # Clean up Global Env
  last_run <- Sys.time()
}

# SAVE LAST RUN ####

write_rds(last_run, file = 'last_run.rds')

# SAVE FULLY UNESTED & ADDED TIME INTELLIGENCE DF ####

dat <- df %>%
  mutate(game_id = id, 
         game_name = name) %>%
  select(-"name", -"id") %>%
  unnest(genres) %>%
  mutate(genre_name = name) %>% 
  select(-"name", -"id", -"slug") %>%
  unnest_wider(col = platforms, names_repair = "unique") %>%
  unnest(platform) %>%
  select(-"id", -"slug") %>% 
  mutate(platform_name = as.factor(name), 
         game_name = as.factor(game_name), 
         genre_name = as.factor(genre_name)) %>%
  select(-"name") %>%
  relocate("released", .after = "platform_name") %>%
  mutate(year_released = year(released),
         month_released = month(released, label = TRUE, abbr = TRUE),
         month_num = month(released),
         quarter_released = quarter(released, with_year = TRUE))

write_rds(dat, file = 'dat.rds')

# MAX & MIN VALUES SAVE ####

released_max <- max(dat$released)
write_rds(dat, file = 'released_max.rds')

released_min <- min(dat$released)
write_rds(dat, file = 'released_min.rds')

num_rating_max <- max(dat$ratings_count)
write_rds(dat, file = 'num_rating_max.rds')

# CLEAN #### 
rm(list = ls()) 
detach("package:datasets", unload = TRUE)
dev.off()  # But only if there IS a plot
p_unload(all) # Remove all add-ons
cat("\014")  # ctrl+L
















