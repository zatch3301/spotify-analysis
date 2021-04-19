
# Load the packages ====
library(spotifyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(highcharter)
library(cowplot)
library(viridisLite)
library(broom)
library(viridis)
library(plotly)
library(shiny)
library(DT)

source("./spotify_functions.R")
# find_artist("justin")
# 
# df <- read_data_function("Justin Bieber")[[1]]
#
# dff <- get_artist_audio_features(artist = "54pgkpWVgQYbQXD8bkUP8n", authorization = access_token)
#
# df <- read_data_function("GG Magree")[[1]]
#
# df_album_years <- read_data_function("Justin Bieber")[[2]]
# 
# artist_id <- get_artist_id(df)
# 
# df_artist_summary(artist_id)
# find_artist("justin")

# 
# df %>% View()

# Ui ====
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  titlePanel("Spotify"),
    sidebarLayout(
      
      sidebarPanel( 
      textInput("artist", "Insert Artist Name", placeholder = 'e.g., Arijit Singh, ...'),
      
      actionButton("search_artist", "Search", icon = icon("spotify")),
      
      htmlOutput("pick_artist"),
      
      htmlOutput("picture"),
      
      br(),
      
      # fluidRow(
      #   column(6, actionButton("addbtn", "Add Artist", class="btn btn-success")),
      #   column(6, actionButton("refbtn", "Clear all"))
      # ),
      
      br(),
      textOutput('select_artist'),
      
      div(id = "placeholder"),
      
      textOutput('max_n_artist'),
      width = 4
      ),
      
      mainPanel(
        h3("Valence and Energy"),
        highchartOutput("valane_energy"),
        DTOutput('tb1'),
        width = 8
      )
    )
  
)

# Server ====
server <- function(input, output, session) {
  
  
  # Reactive element to query all artists related to text inserted into textInput
  artist_list <- eventReactive(input$search_artist, {
    if(input$artist == "") {
      showNotification("Please type the artist", 
                       type = "error")
      return()
    } else if (input$artist > 0) {
      return(find_artist(input$artist))
    }
  })
  
  # SelectInput to select an artist returned by reactive artist_list
  output$pick_artist <- renderUI({
    if(!is.null(artist_list())) {
      artists = artist_list()$uri %>% simplify %>% as.list
      names(artists) = artist_list()$name
      list(br(),
       selectInput(inputId = "chosen_artist", 
                   label = "Results", 
                   choices = artist_list()$name,
                   multiple = FALSE)
      )
      
    }
  })
  
  # Reactive for selected artist in pick_artist selectInput
  selected_artist <- reactive(if (!is.null(artist_list())) {
    artist_list() %>% 
      filter(artist_list()$name == input$chosen_artist)
  })
  
  # Render a picture of the selected artist
  output$picture <- renderText({if (!is.null(artist_list())) {
    paste0('<img src="',
           # selected_artist()$images[[1]]$url[1],
           selected_artist() %>%
           select(images) %>%
           unnest(cols=c(images)) %>%
           slice(1) %>% select(url),
           '"width=277 height=277>')
  }
  })
  
  #Data on the artist
  data <- reactive(if(!is.null(artist_list())){
    read_artist_data_function(selected_artist()$name)
  })
  
  
  output$tb1 = renderDataTable(
    data(), 
    options = list(lengthChange = FALSE)
  )
  
  
  # Valence Energy Graph
  output$valane_energy <- renderHighchart({
    df_valence_energy <- df_valence_energy_function(read_data_function(selected_artist()$name)[[1]],
                                                    read_data_function(selected_artist()$name)[[2]])
    valence_energy_plot(df_valence_energy)
  })
  
}

shinyApp(ui, server)