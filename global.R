# FRONT PAGE ####

# Created by    Kurtis Smith    
# Created on    2020-10-03
# R Version     4.0.3 (2020-04-24)

# LOAD PACKAGES ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,
               jsonlite,
               tidyverse,
               lubridate)

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

# MAX & MIN VALUES SAVE ####

released_max <- max(dat$released)
released_min <- min(dat$released)
num_rating_max <- max(dat$ratings_count)



