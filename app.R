# LOAD PACKAGES ####

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)
library(viridisLite)
library(whisker)

# UI ####

ui <- dashboardPage(
  skin = 'black',
  
## Header ####
  
  dashboardHeader(title = paste("Couch Games", "| Last ⟳", last_run),
                  titleWidth = 600),

## Sidebar ####

  dashboardSidebar(
    width = 300,
    sliderInput(
      inputId = "date_range", 
      label = "Date Range:",
      min = released_min,
      max = released_max,
      value = c(as.Date("2010-01-01"), Sys.Date()),
      timeFormat = "%Y% %b%",
      ticks = F),
    sliderInput(
      inputId = "num_rate", 
      label = "Number of Ratings:",
      min = 0, 
      max = num_rating_max,
      value = c(50, num_rating_max),
      ticks = F),
    sliderInput(
      inputId = "rating", 
      label = "Rating:",
      min = 0, 
      max = 5,
      value = c(2, 5),
      ticks = F),
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("About", tabName = "about")
    )
    
    ),

## Body ####

 ## dashboard ####
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("game_num"),
                valueBoxOutput("pop_genre"),
                valueBoxOutput("pop_plat")
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Sub Genres of Coop Games By Year",
                  plotOutput("genres_plot", width = "100%", height = 600)
                  ),
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Platforms of Coop Games By Year",
                  plotOutput("platform_plot", width = "100%", height = 600)
                  ),
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Top 50 Coop Games",
                  DTOutput("top50"))
                )
            
             ),
      
    ## about ####
    
    tabItem("about",
            includeHTML("about_tab.Rhtml"))
      
      )
  )
)

## SERVER ####

server <- function(input, output) {
  
## Genres Plot | Dashboard Tab ####
  
  output$genres_plot <- renderCachedPlot({
 
  x <- dat %>%
    nest(c('platform_name')) %>%
    filter(ratings_count >= input$num_rate[1] & ratings_count <= input$num_rate[2], 
           rating >= input$rating[1] & rating <= input$rating[2],
           playtime >= 0) %>% 
    select(released, genre_name, month_released) %>%
    mutate(released = floor_date(ymd(released), 'month')) %>%
    filter(released >= input$date_range[1] & released <= input$date_range[2]) %>% 
    mutate(released_year = floor_date(released, unit = "year")) %>%
    group_by(released_year) %>%
    count(genre_name)
  
  x %>%
    ggplot(aes(released_year, n)) +
    geom_jitter(aes(colour = genre_name), 
                width = 10, 
                height = 10,
                alpha = 0.5) +
    geom_smooth(aes(colour = genre_name), se = F) +
    xlab(element_blank()) +
    ylab(element_blank()) +
    facet_wrap(genre_name~., ncol = 4) +
    theme(plot.background = element_rect(fill = "#ecf0f5"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "#ecf0f5"),
          strip.background = element_rect(fill = "#ecf0f5"),
          legend.position = "none",
          legend.background = element_rect(fill = "#ecf0f5"),
          legend.title = element_blank(), 
          legend.key = element_blank())
    
  },
  cacheKeyExpr =  {list(input$num_rate[1],
                  input$num_rate[2],
                  input$rating[1],
                  input$rating[2],
                  input$date_range[1],
                  input$date_range[2])}
  )

## Platform Plot | Dashboard Tab ####
  
  output$platform_plot <- renderCachedPlot({
    
    y <- dat %>%
      nest(c('genre_name')) %>%
      filter(ratings_count >= input$num_rate[1] & ratings_count <= input$num_rate[2], 
             rating >= input$rating[1] & rating <= input$rating[2],
             playtime >= 0) %>% 
      select(released, platform_name, month_released) %>%
      mutate(released = floor_date(ymd(released), 'month')) %>%
      filter(released >= input$date_range[1] & released <= input$date_range[2]) %>% 
      mutate(released_year = floor_date(released, unit = "year")) %>%
      group_by(released_year) %>%
      count(platform_name)
      
    y %>%
      ggplot(aes(released_year, n)) +
      geom_jitter(aes(colour = platform_name), 
                  width = 10, 
                  height = 10,
                  alpha = 0.5) +
      geom_smooth(aes(colour = platform_name), se = F) +
      xlab(element_blank()) +
      ylab(element_blank()) +
      facet_wrap(platform_name~., ncol = 4) +
      theme(plot.background = element_rect(fill = "#ecf0f5"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill = "#ecf0f5"),
            strip.background = element_rect(fill = "#ecf0f5"),
            legend.position = "none",
            legend.background = element_rect(fill = "#ecf0f5"),
            legend.title = element_blank(), 
            legend.key = element_blank())
    
  },
  cacheKeyExpr =  {list(input$num_rate[1],
                        input$num_rate[2],
                        input$rating[1],
                        input$rating[2],
                        input$date_range[1],
                        input$date_range[2])}
  )

## Number of Games | Dashboard Tab ####

  output$game_num <- renderValueBox({
 
    z <- dat %>%
    select(-"genre_name", -"platform_name") %>% 
    distinct(across(everything())) %>%
    filter(ratings_count >= input$num_rate[1] & ratings_count <= input$num_rate[2], 
           rating >= input$rating[1] & rating <= input$rating[2],
           playtime >= 0) %>% 
    select(released, game_name, month_released) %>%
    mutate(released = floor_date(ymd(released), 'month')) %>%
    filter(released >= input$date_range[1] & released <= input$date_range[2]) %>% 
    select(game_name) %>%
    count()
  
  valueBox(
    value = z,
    subtitle = "Number of Games",
    icon = icon("calculator"))
    
})

## Most Popular Sub Genre | Dashboard Tab ####

output$pop_genre <- renderValueBox({
  
  a <- dat %>%
    nest(c('platform_name')) %>%
    filter(ratings_count >= input$num_rate[1] & ratings_count <= input$num_rate[2], 
           rating >= input$rating[1] & rating <= input$rating[2],
           playtime >= 0) %>% 
    select(released, genre_name, month_released) %>%
    mutate(released = floor_date(ymd(released), 'month')) %>%
    filter(released >= input$date_range[1] & released <= input$date_range[2]) %>% 
    count(genre_name) %>% 
    arrange(desc(n)) %>%
    select(genre_name) %>% 
    head(1)
  
  valueBox(
    value = a,
    subtitle = "Most Popular Sub Genres",
    icon = icon("theater-masks"))
  
  })

## Most Popular Platform | Dashboard Tab ####

output$pop_plat <- renderValueBox({
  
  b <- dat %>%
    nest(c('genre_name')) %>%
    filter(ratings_count >= input$num_rate[1] & ratings_count <= input$num_rate[2], 
           rating >= input$rating[1] & rating <= input$rating[2],
           playtime >= 0) %>% 
    select(released, platform_name, month_released) %>%
    mutate(released = floor_date(ymd(released), 'month')) %>%
    filter(released >= input$date_range[1] & released <= input$date_range[2]) %>% 
    count(platform_name) %>% 
    arrange(desc(n)) %>%
    select(platform_name) %>% 
    head(1)
  
  valueBox(
    value = b,
    subtitle = "Most Popular Platform",
    icon = icon("gamepad"))
  
})

## Most Popular Sub Genre | Top 50 Tab ####

output$top50 <- renderDT({
  c <- dat %>%
    select(-"genre_name", -"platform_name") %>%
    filter(ratings_count >= input$num_rate[1] & ratings_count <= input$num_rate[2], 
           rating >= input$rating[1] & rating <= input$rating[2],
           playtime >= 0) %>% 
    filter(released >= input$date_range[1] & released <= input$date_range[2]) %>% 
    distinct(game_name, released, rating, ratings_count) %>%
    transmute(Game = game_name,
              "Date Released" = released,
              "No. of Ratings" = ratings_count,
              Rating = rating) %>% 
    arrange(desc(Rating)) %>% 
    head(50)
  
  datatable(c,
            options = list(pageLength = 10), 
            width = 400, 
            rownames = FALSE,
            class = 'cell-border stripe')
            
})


}

shinyApp(ui, server)




