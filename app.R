library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(fmsb)
library(rsconnect)
library(caret)
library(tibble)
library(tidyr)
library(stringi)
shiny::addResourcePath("www", "www")

# 1. Load your data (if it's in a file)

spotify <- read.csv("Popular_Spotify_Songs.csv")

# Clean all character columns to remove non-ASCII characters
spotify <- as.data.frame(
  lapply(spotify, function(x) {
    if (is.character(x)) {
      x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "")
    }
    return(x)
  }),
  stringsAsFactors = FALSE
)

spotify$streams <- as.numeric(spotify$streams)
spotify$in_deezer_playlists = as.numeric(spotify$in_deezer_playlists)
spotify$in_shazam_charts = as.numeric(spotify$in_shazam_charts)
spotify <- spotify %>%
  mutate(song.artist = paste(track_name, "-", artist.s._name))



# 2. Create your derived data frames (if you haven't saved them)
yearly_top_songs <- spotify %>%
  group_by(released_year) %>%
  slice_max(order_by = streams, n = 5, with_ties = TRUE) %>%
  ungroup()

yearly_top_song <- spotify %>%
  group_by(released_year) %>%
  slice_max(order_by = streams, n = 1, with_ties = TRUE) %>%
  ungroup()

# Step 1: Get the top 10 songs by total streams
top10_yearly <- yearly_top_song %>% 
  arrange(desc(streams)) %>% 
  slice(1:10)

top10_yearly <- top10_yearly %>%
  arrange(desc(streams)) %>%
  mutate(song.artist = factor(song.artist, levels = unique(song.artist)))

# 3. Load or create your model 
data_complete <- spotify %>%
  select(streams, in_spotify_playlists, in_deezer_playlists, in_apple_playlists) %>%
  na.omit()

# Step 2: Set seed for reproducibility
set.seed(123)

# Step 3: Define training control for 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Step 4: Define the model formula (same predictors as before)
model_formula <- streams ~ in_spotify_playlists + in_deezer_playlists + in_apple_playlists

# Step 5: Fit the linear regression model using caret::train()
cv_model <- train(
  model_formula,
  data = data_complete,
  method = "lm",
  trControl = train_control
)

# 3. Predict streams based on new inputs
#predicted_streams <- predict(cv_model, newdata = new_input)

# 4. Create the plot_data data frame

# Extract the linear model from caret's train object
lm_model <- cv_model$finalModel

# Generate predictions with both confidence and prediction intervals
pred_conf <- predict(lm_model, newdata = data_complete, interval = "confidence")
pred_pred <- predict(lm_model, newdata = data_complete, interval = "prediction")

# Combine everything into a data frame
plot_data <- data_complete %>%
  mutate(
    predicted_streams = pred_conf[, "fit"],
    conf_low = pred_conf[, "lwr"],
    conf_high = pred_conf[, "upr"],
    pred_low = pred_pred[, "lwr"],
    pred_high = pred_pred[, "upr"]
  ) %>%
  arrange(streams)  # sort by actual streams for smooth ribbons




# Define UI
ui <- navbarPage(
  title = div(
    img(src = "www/spotify_logo_green.png", height = "30px", style = "margin-top:-5px;"),
    "Spotify Data Dashboard"
  ),
  id = "main_navbar",
  
  # Custom styling
  header = tags$head(
    tags$style(HTML("
    /* Navbar background */
    .navbar-default {
      background-color: #191414;
      border-color: #191414;
    }

    /* Navbar title (brand) text color */
    .navbar-default .navbar-brand {
      color: #1DB954 !important; /* Spotify green */
    }

    /* Navbar title hover/focus */
    .navbar-default .navbar-brand:hover,
    .navbar-default .navbar-brand:focus {
      color: #1ed760 !important;
    }

    /* Tab panel (menu items) text color */
    .navbar-default .navbar-nav > li > a {
      color: #1DB954 !important;
    }

    /* Tab hover/focus */
    .navbar-default .navbar-nav > li > a:hover,
    .navbar-default .navbar-nav > li > a:focus {
      color: #1ed760 !important;
      background-color: transparent;
    }

    /* Keep main panel background */
    .main-panel, .col-sm-8 {
      background-color: #f5f5f5 !important;
      color: black !important;
      padding: 20px;
      border-radius: 6px;
    }
  "))
  ),
  
  # --- HOME TAB ---
  tabPanel("Home",
           fluidPage(
             tags$div(
               style = "text-align: left;",
               img(src = "www/spotify_logo_black.png", height = "120px"),
               h2("Welcome to the Spotify Data Dashboard"),
               p("This data includes songs released from 1930 - 2023. Use the tabs above to explore data visualizations and models related to Spotify songs."),
               tags$ul(
                 tags$li("Explore trends across release years"),
                 tags$li("Discover top streamed tracks"),
                 tags$li("Visualize audio features with radar and scatter plots"),
                 tags$li("Compare playlists and stream predictions")
               ),
               p("Use the tabs above to explore the features.")
             )
           )
  ),  
  
  
  # Group: Release Year Performance
  tabPanel("Release Year Performance",
           tabsetPanel(
             tabPanel("Density: Release Year",
                      plotOutput("densityPlot")
             ),
             tabPanel("Top 10 Overall - Top Streamed Song From Each Released Year",
                      plotOutput("top10Bar"),
                      p('Filtering for the top streamed from each released year and then displaying which songs ranked in top 10 by overall stream counts.')
             ),
             tabPanel("Top 5 Songs by Year",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year", "Select Year:", choices = sort(unique(yearly_top_songs$released_year), decreasing = TRUE))
                        ),
                        mainPanel(
                          plotOutput("topSongsPlot")
                        )
                      )
             )
           )
  ),
  
  # Group: Audio Features
  tabPanel("Radar: Top 3 Songs Audio Features",
           sidebarLayout(
             sidebarPanel(
               selectInput("selected_year", "Choose a Year:",
                           choices = sort(unique(spotify$released_year), decreasing = TRUE),
                           selected = max(spotify$released_year, na.rm = TRUE)),
               tags$ul(
                 style = "padding-left: 15px; margin-left: 0;", # Adjust padding as needed
                 tags$li("BPM - Beats per minute, tempo or speed of the song. "),
                 tags$li("Acousticness - Confidence measure of how much a track sounds like it was made with live instruments and natural sounds, rather than electronic or synthesized sounds. "),
                 tags$li("Energy - Encompasses a broader range of elements, including momentum, intensity, and emotional impact."),
                 tags$li("Speechiness - Measure of how much spoken word content is present in a track, as opposed to purely musical elements."),
                 tags$li("Danceability - Measure of how easily someone could move their body to the rhythm and structure of the music."),
               )
             ),
             mainPanel(
               plotOutput("radarPlot")
             )
           )
  ),
  
  
  # Group: Playlist Metrics & Predictions
  tabPanel("Playlists & Streams Relationship",
           tabsetPanel(
             tabPanel("Scatter: Streams vs Playlists by Mode",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("mode_select", "Select Mode(s):",
                                             choices = unique(spotify$mode),
                                             selected = unique(spotify$mode)),
                          tags$ul(
                            style = "padding-left: 15px; margin-left: 0;", # Adjust padding as needed
                            tags$li("Modes refer to two different types of musical scales and keys."),
                            tags$li("Major - Sounds bright, happy, and uplifting."),
                            tags$li("Minor - Sounds sad, melancholic, or dark."),
                          )
                        ),
                        
                        mainPanel(
                          plotOutput("scatterPlot"),
                        )
                      )
             ),
             
             
             tabPanel("Pairs Plot",
                      plotOutput("pairsPlot"),
                      p('The pairs plot is showing the linear relationship between the number of streams to the number of playlists songs appear in. Spotify, Deezer, and Apple plalylist appearances had the most linear relationship to streams.')
             ),
             tabPanel("Model Predictions",
                      fluidRow(
                        column(6,
                               p('Input an estimate for the number of appearances in each of the playlists in order to predict the number of streams. The model is trained on this data to predict the number of total streams (2008-2024). 
             '),
                               h4("Predict Streams"),
                               numericInput("spotify", "Spotify Playlists:", value = 5000, min = 0),
                               numericInput("deezer", "Deezer Playlists:", value = 1000, min = 0),
                               numericInput("apple", "Apple Playlists:", value = 2000, min = 0),
                               actionButton("predict", "Predict Streams"),
                               br(), br(),
                               h5("Predicted Streams:"),
                               verbatimTextOutput("prediction")
                        ),
                        
                        column(6,
                               h4("Actual vs Predicted Streams"),
                               plotOutput("predictionPlot"),
                               p('The linear model displayed is trained on streams in relation to Spotify, Deezer, and Apple playlist appearances. Using data from each song, it is plotting the actual stream numbers to the predicted value with confidence and predictive intervals.')
                        )
                      )
             )
           )
  )
)



# Define Server
server <- function(input, output, session) {
  
  # Density Plot ----
  output$densityPlot <- renderPlot({
    plot(density(spotify$released_year, na.rm = TRUE),
         main = "Density Plot of Song Counts by Released Year",
         xlab = "Released Year", col = "blue", lwd = 2)
  })
  
  # Scatter Plot ----
  output$scatterPlot <- renderPlot({
    filtered_data <- spotify[spotify$mode %in% input$mode_select, ]
    
    ggplot(filtered_data, aes(x = streams, y = in_spotify_playlists, color = mode)) +
      geom_point() +
      labs(title = "Streams vs Playlist Metrics by Mode",
           x = "Streams", y = "Number in Spotify Playlists") +
      theme_minimal()
  })
  
  # Top 10 Streamed Songs by Year ----
  output$top10Bar <- renderPlot({
    ggplot(top10_yearly, aes(x = factor(released_year), y = streams, fill = song.artist)) +
      geom_bar(stat = "identity") +
      labs(title = "Top Streamed Song For Each Released Year: Top 10 Ranking Overall",
           x = "Year", y = "Streams", fill = "Song - Artist") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top 5 Songs per Year ----
  output$topSongsPlot <- renderPlot({
    selected_year_data <- yearly_top_songs %>%
      filter(released_year == input$year)
    
    
    ggplot(selected_year_data, aes(x = reorder(track_name, -streams), y = streams, fill = song.artist)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 5 Streamed Songs in", input$year),
           x = "Song", y = "Streams", fill = "Song - Artist") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Radar Chart ----
  radar_matrix3 <- reactive({
    req(input$selected_year)
    top3_year <- spotify %>%
      filter(released_year == input$selected_year) %>%
      slice_max(order_by = streams, n = 3, with_ties = FALSE) %>%
      dplyr::rename(
        danceability = `danceability_.`,
        speechiness = `speechiness_.`,
        energy = `energy_.`,
        acousticness = `acousticness_.`
      ) %>%
      dplyr::select(song.artist, bpm, danceability, speechiness, energy, acousticness)
    
    radar_data_norm3 <- top3_year %>%
      mutate(across(where(is.numeric), ~ rescale(.x, to = c(0, 1))))
    
    if (nrow(radar_data_norm3) < 1) return(NULL)
    
    max_min3 <- data.frame(
      bpm = 1, danceability = 1, speechiness = 1, energy = 1, acousticness = 1,
      row.names = c("Max")
    ) %>%
      bind_rows(data.frame(
        bpm = 0, danceability = 0, speechiness = 0, energy = 0, acousticness = 0,
        row.names = c("Min")
      ))
    
    bind_rows(max_min3, radar_data_norm3 %>% column_to_rownames("song.artist"))
  })
  
  output$radarPlot <- renderPlot({
    matrix <- radar_matrix3()
    req(matrix)
    
    colors_border <- rainbow(nrow(matrix) - 2)
    colors_in <- adjustcolor(colors_border, alpha.f = 0.25)
    
    layout(matrix(c(1, 2), nrow = 2), heights = c(4, 1))
    par(mar = c(2, 2, 4, 2))
    radarchart(
      matrix,
      axistype = 1,
      pcol = colors_border,
      pfcol = colors_in,
      plwd = 2,
      plty = 1,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 1, 0.2),
      cglwd = 0.8,
      vlcex = 0.9,
      title = paste("Top 3 Songs in", input$selected_year)
    )
    
    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend("center", legend = rownames(matrix)[-c(1, 2)],
           bty = "n", pch = 20, col = colors_border, text.col = "black", cex = 0.9)
  })
  
  # Pairs Plot ----
  output$pairsPlot <- renderPlot({
    selected_data <- dplyr::select(spotify, in_spotify_playlists, in_deezer_playlists, in_apple_playlists, streams)
    pairs(selected_data, main = "Pairs Plot of Playlist Counts and Streams")
  })
  
  # Actual vs Predicted Plot ----
  output$predictionPlot <- renderPlot({
    ggplot(plot_data, aes(x = predicted_streams, y = streams)) +
      geom_point(alpha = 0.6, color = "darkblue") +
      geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "blue") +
      geom_line(aes(y = conf_low), color = "purple", linetype = "dashed") +
      geom_line(aes(y = conf_high), color = "purple", linetype = "dashed") +
      geom_line(aes(y = pred_low), color = "red", linetype = "dotted") +
      geom_line(aes(y = pred_high), color = "red", linetype = "dotted") +
      labs(title = "Actual vs Predicted Streams (R^2 = 0.68)",
           x = "Predicted Streams", y = "Actual Streams") +
      theme_minimal()
  })
  
  # Predict Streams ----
  observeEvent(input$predict, {
    new_input <- data.frame(
      in_spotify_playlists = input$spotify,
      in_deezer_playlists = input$deezer,
      in_apple_playlists = input$apple
    )
    
    predicted <- predict(cv_model, newdata = new_input)
    
    output$prediction <- renderText({
      format(round(predicted, 0), big.mark = ",")
    })
  })
}

# Run the App
shinyApp(ui = ui, server = server)



