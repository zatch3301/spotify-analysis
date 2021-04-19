source(".Renviron")

access_token <- get_spotify_access_token(
  client_id = CLIENT_ID,
  client_secret = CLIENT_SECRET_KEY)

# Artist search query ====
find_artist <- function(artist) {
  artist_search_results <- search_spotify(artist, 
                                          type="artist",
                                          authorization = access_token)
  return(artist_search_results)
}

# Read and prep data ====
read_data_function <- function(artist_names) {
  #Get the data of the artists included in artist_names and store them in a list
  df <- get_artist_audio_features(artist_names, 
                                  authorization = access_token)
  
  # Release year to join to other summaries
  df_album_release_years <- df %>% 
    group_by(album_name) %>% 
      summarise(release_year=first(album_release_year), 
                track_count=n_distinct(track_name), 
                album_image=first(album_images)) %>% 
      mutate(album_name_yr=str_c(album_name, " (", release_year, ")"))

  join_data <- df %>% left_join(df_album_release_years)
  
  return(list(df=df, df_album_release_years=df_album_release_years, join_data=join_data))
}

# Get Artist Id ====
get_artist_id <- function(df) {
  artist_id_df <-df %>% 
    select(artist_name, artist_id) %>% 
      summarise(artist_name=first(artist_name), artist_id=first(artist_id))
  
  artist_id <- artist_id_df %>% select(artist_id)
  
  return(artist_id)
}

# Extract artist data ====
df_artist_summary <- function(artist_id){
  df_artist <- get_artist(artist_id, authorization = access_token)
  df_artist_summary <- df_artist %>% 
    mutate_if(is.character, as.factor)

  artist_summary <- df_artist_summary
  artist_summary$genres[!lengths(artist_summary$genres) > 0] <- ''
  
  return(artist_summary)
}

# Define a function for vis data
read_artist_data_function <- function(x){
  df <- get_artist_audio_features(x, authorization = access_token) %>% 
    select("artist_name", "album_name", "track_name", "album_release_date",
           "danceability", "energy", 
           "loudness", "speechiness", 
           "acousticness", "valence", 
           "tempo", "instrumentalness", 
           "liveness")
  return(df)
}

# Define the function to generate the data for all valence and energy plots
df_valence_energy_function <- function(x,y){
  df_valence_energy <- x %>% select(artist_name, album_name, track_name, valence, energy) %>% 
      left_join(y) %>% 
      mutate_if(is.character, as.factor)

  #Combine the valence and energy data of all the artist into a single dataframe
  valence_energy <- df_valence_energy %>% 
    mutate(album_name_yr=fct_reorder(album_name_yr, desc(release_year)))
  
  album_image <- valence_energy %>% select(album_image) %>% filter(album_image$height==64) 
  
  valence_energy$album_image2 <- album_image$album_image$url
  
  return(valence_energy)
}

# Define the function to generate the data of median valence and energy values
median_valence_energy_function <- function(df_valence_energy){
  median_df <- df_valence_energy %>% select(artist_name, valence, energy) %>% 
    group_by(artist_name) %>% summarise(medianValence = median(valence), medianEnergy = median(energy))
  
  return(median_df)
}

# Define the function to generate the valence and energy plot
valence_energy_plot <- function(df){
  # Determine the valence and energy per artist
  centroids_df <- df %>% select(artist_name, valence, energy) %>% 
    nest(data=c(valence, energy)) %>% mutate(model = map(data, kmeans, 1),
                                             centers = map(model, broom::tidy)) %>%
    unnest(centers) %>% select(artist_name,valence,energy)
  
  #color
  colrs <- viridis(3)
  
  #Make Plot of the valence and energy per track per artist
  plotline <- list(color = "#000000",
                   width = 2,
                   value = 0.5)
  
  plot_valence_energy <- highchart() %>% 
    hc_add_series(df, "scatter", hcaes(x=valence, y=energy, group=artist_name), showInLegend = TRUE) %>% 
    hc_add_series(centroids_df, "bubble", 
                  name="centroids", 
                  hcaes(x=valence, y=energy, group=artist_name, size=2), 
                  showInLegend = FALSE) %>%
    hc_colors(colors =  colrs) %>% 
    hc_yAxis(title = list(text = "Energy"), plotLines=list(plotline), min=0, max=1) %>% 
    hc_xAxis(title = list(text = "Valence"), plotLines=list(plotline), min=0, max=1) %>% 
    hc_tooltip(formatter = JS("function () { if (this.series.name=='centroids'){
                                                  return 'Centroid Valence: ' + this.point.valence + '<br/>' +
                                                         'Centroid Energy: ' + this.point.energy
                                                } else {
                                                return '<table><tr><td style=\"padding:5px\">' +
                                                       '<img src=\"' + this.point.album_image2 + '\" width=64>' +
                                                       '</td><td>' +
                                                       'Album Name: ' + this.point.album_name + '<br/>' +
                                                       'Track Name: ' + this.point.track_name + '<br/>' +
                                                       'Valence: ' + this.point.valence + '<br/>' +
                                                       'Energy: ' + this.point.energy +
                                                       '</td></tr></table>'
                                                } ;}"),
               useHTML=TRUE) %>%
    
    hc_annotations(list(
      labels=list(
        list(point = list(x = 0.05, y = 0.95, xAxis = 0, yAxis = 0),
             text = "Turbulent/Angry", style=list(fontWeight='bold', fontSize="14px")),
        list(point = list(x = 0.05, y = 0, xAxis = 0, yAxis = 0),
             text = "Sad/Depressing", style=list(fontWeight='bold', fontSize="14px")),
        list(point = list(x = 0.95, y = 0.95, xAxis = 0, yAxis = 0),
             text = "Happy/Joyful", style=list(fontWeight='bold', fontSize="14px")),
        list(point = list(x = 0.95, y = 0, xAxis = 0, yAxis = 0),
             text = "Chill/Peaceful", style=list(fontWeight='bold', fontSize="14px"))
      ), draggable=FALSE, labelOptions=list(backgroundColor="rgba(255,255,255, 0.5)", borderWidth=0, strokeWidth=4)
    ))
  
  return(plot_valence_energy)  
}
