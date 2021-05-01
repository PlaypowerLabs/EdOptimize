library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(DT)
library(waiter)
library(htmltools)
library(scales)
library(lubridate)
library(gtools)
library(shinycssloaders)

load('data/general_data.RData')
load('data/overall_usage_tab_data.RData')
load('data/usage_over_time_tab_data.RData')
load('data/content_usage_tab_data.RData')
load('data/assessment_analysis_tab_data.RData')
load('data/skill_performance_tab_data.RData')
load('data/curriculum_pacing_tab_data.RData')
load('data/leaderboard_tab_data.RData')
source('functions.R')
source('appCSS.R')
source('plot.R')


theme_set(
  theme_bw() +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 18, face = "bold"), plot.title = element_text(size = 12 + 4),
          plot.subtitle = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
          plot.caption = element_text(size = 12), strip.text.x = element_text(size = 14, face = 'bold', colour = 'white'), strip.text.y = element_text(size = 14, face = 'bold', colour = 'white'),
          strip.background = element_rect(colour = '#2c3e50', fill = '#2c3e50'), panel.spacing = unit(2, "lines"), axis.title.x = element_text(margin = margin(t = 20)))
)


ui <- tagList(
  waiter::waiter_use(),
  waiter::waiter_show_on_load(
    color = '#2C3E50',
    tagList(
      spin_wave(),
      br(),
      span(h3("Loading ..."), style = "color:white;")
    )
  ),
  useShinyjs(),
  inlineCSS(appCSS),
  tags$head(
    tags$script(
      'var dimension = [0, 0];
              $(document).on("shiny:connected", function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange("dimension", dimension);
              });
              $(window).resize(function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange("dimension", dimension);
              });')),
  navbarPage(
    title = 'Curriculum Analytics',
    theme = shinytheme('flatly'),
    id = 'nav_bar',
    
    source(file.path('ui','overall_usage_ui.R'), local = TRUE)$value,
    source(file.path('ui','usage_over_time_ui.R'), local = TRUE)$value,
    source(file.path('ui','content_usage_ui.R'), local = TRUE)$value,
    source(file.path('ui','assessment_analysis_ui.R'), local = TRUE)$value,
    source(file.path('ui','skill_performance_ui.R'), local = TRUE)$value,
    source(file.path('ui','curriculum_pacing_ui.R'), local = TRUE)$value,
    source(file.path('ui','leaderboard_ui.R'), local = TRUE)$value
  )
)


server <- function(input, output){

  
  source(file.path('server','overall_usage_server.R'), local = TRUE)$value
  waiter::waiter_hide()
  source(file.path('server','usage_over_time_server.R'), local = TRUE)$value
  source(file.path('server','content_usage_server.R'), local = TRUE)$value
  source(file.path('server','assessment_analysis_server.R'), local = TRUE)$value
  source(file.path('server','skill_performance_server.R'), local = TRUE)$value
  source(file.path('server','curriculum_pacing_server.R'), local = TRUE)$value
  source(file.path('server','leaderboard_server.R'), local = TRUE)$value
}


shinyApp(ui = ui, server = server)

