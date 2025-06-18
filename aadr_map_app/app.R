library(shiny)
library(leaflet)
library(geosphere)
library(dplyr)
library(nominatimlite)
library(data.table)
library(here)
library(DT)
# install.packages("reactlog")


sample_data <- fread(here('aadr_data/v62.0_1240k_public.anno.tsv'))

name.id <- colnames(sample_data)[colnames(sample_data) %like% 'Genetic ID']
name.date <- colnames(sample_data)[colnames(sample_data) %like% 'Date mean']
name.lat <- colnames(sample_data)[colnames(sample_data) %like% 'Lat.']
name.lon <- colnames(sample_data)[colnames(sample_data) %like% 'Long.']

setnames(sample_data,
         c(name.id, name.date, name.lat, name.lon),
         c('sample_id', 'age', 'latitude', 'longitude'))

sample_data[, latitude := as.numeric(latitude)]
sample_data[, longitude := as.numeric(longitude)]

range(sample_data$latitude)
sample_data

# # Sample dataset
# set.seed(123)
# sample_data <- data.frame(
#   sample_id = paste0("S", 1:100),
#   age = sample(10:90, 100, replace = TRUE),
#   latitude = runif(100, 35, 45),
#   longitude = runif(100, -120, -70)
# )


ui <- fluidPage(

  titlePanel("Interactive Map with Sample ID Labels"),
  
  sidebarLayout(
    sidebarPanel(
      # sliderInput("ageRange", "Select Age Range:",
      #             min = min(sample_data$age), max = max(sample_data$age),
      #             value = c(4000, 5100)),
      # sliderInput("distance", "Max Distance from Reference (km):",
      #             min = 0, max = 5000, value = 100),

      fluidRow(
        column(6, numericInput("ageMin", "Min Age:", value = 4000, width = "100%")),
        column(6, numericInput("ageMax", "Max Age:", value = 5100, width = "100%"))
      ),
      numericInput("distance", "Max Distance from Reference (km):", value = 500),
      textInput("place", "Enter place name, or click on map:", value = ""),
      # actionButton("go", "Search Location"),
      checkboxInput("showIDs", "Show sample IDs on map", value = FALSE),
      selectInput("labelColumn", "Select column to label samples:",
                  choices = names(sample_data),
                  selected = "sample_id"),
      verbatimTextOutput("clickCoords")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  ),
  
  fluidRow(
    column(12,
           h4("Filtered Sample List"),
           DTOutput("filteredTable")
    )
  )
  
)

server <- function(input, output, session) {
  # Gorzsa, Hungary: Lat: 46.3707, Lon: 20.4258
  ref_point <- reactiveVal(c(lon = 20.4258, lat = 46.3707))
    
  # Geocode place name
  observeEvent(input$place, {
    req(input$place)
    print(input$place)
    result <- tryCatch({
      nominatimlite::geo_lite(input$place, limit = 1)
    }, error = function(e) NULL)
    
    if (!is.null(result) && nrow(result) > 0) {
      ref_point(c(lon = result$lon, lat = result$lat))
      leafletProxy("map") %>%
        setView(lng = result$lon, lat = result$lat, zoom = 6)
    } else {
      showNotification("Place not found", type = "error")
    }
  },
  ignoreInit = TRUE)
  
  # In your server function (add this observeEvent)
  observeEvent(input$map_click, {
    click <- input$map_click
    if (!is.null(click)) {
      ref_point(c(lon = click$lng, lat = click$lat))
      # leafletProxy("map") %>%
      #   setView(lng = click$lng, lat = click$lat, zoom = 6)
    }
  })
  
    # Filtered dataset
  filtered_data <- reactive({
    loc <- ref_point()
    data <- sample_data %>%
      filter(age >= input$ageMin, age <= input$ageMax)
    
    if (nrow(data) == 0) return(data.frame())  # Prevents error if no rows after age filtering

    dists <- distHaversine(
      matrix(c(data$longitude, data$latitude), ncol = 2),
      loc
    ) / 1000
    
    data[dists <= input$distance, ]
  })
  
  output$clickCoords <- renderText({
    loc <- ref_point()
    paste0("Reference Location:\nLat: ", round(loc["lat"], 4),
           ", Lon: ", round(loc["lon"], 4), '\n',
           "Number of samples: ", nrow(filtered_data()), '\n')
  })
  
  # Initial empty map
  output$map <- renderLeaflet({
    loc <- ref_point()
    # paste0("Reference Location:\nLat: ", round(loc["lat"], 4),
    #        ", Lon: ", round(loc["lon"], 4))
    leaflet() %>%
      addTiles() %>%
      setView(lng = loc["lon"], lat = loc["lat"], zoom = 4)
  })
  
  # Update map dynamically
  observe({
    data <- filtered_data()
    loc <- ref_point()
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(lng = loc["lon"], lat = loc["lat"], popup = "Reference Point") %>%
      addCircles(lng = loc["lon"], lat = loc["lat"],
                 radius = input$distance * 1000,
                 color = "red", fill = FALSE, weight = 2) %>%
      {
        if (input$showIDs & 
            nrow(data) > 0) {
          addLabelOnlyMarkers(.,
                              lng = ~longitude,
                              lat = ~latitude,
                              label = ~as.character(get(input$labelColumn)),
                              labelOptions = labelOptions(noHide = TRUE, direction = "auto",
                                                          textOnly = TRUE, style = list(
                                                            "color" = "blue",
                                                            "font-size" = "12px",
                                                            "background" = "white",
                                                            "border" = "1px solid #ccc",
                                                            "padding" = "2px"
                                                          ))
          )
        } else if (nrow(data) > 0) {
          print(input)
          print(input$labelColumn)
          # print(get(input$labelColumn))
          addCircleMarkers(.,
                           lng = ~longitude,
                           lat = ~latitude,
                           radius = 5,
                           color = "blue",
                           label = ~as.character(get(input$labelColumn)),
                           fillOpacity = 0.7
          )
        }

      }
  }) # end update map
  
  ## display a table of the filtered results
  output$filteredTable <- renderDT({
    filtered_data()
  }, options = list(pageLength = 100), rownames = FALSE)
}

shinyApp(ui, server, options = list(launch.browser = TRUE))

# shinyApp(ui, server)
