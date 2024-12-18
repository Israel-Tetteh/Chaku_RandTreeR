library(shiny) # load shiny app.
library(shiny)
library(shinycssloaders)
library(sf)
library(dplyr)
library(ggplot2)

ui <- navbarPage(
  title = NULL,
  id = "navbar",
  header = tags$style(
    HTML("
      /* Style the navbar */
      .navbar {
        background-color: #2b6b32;
        border-color: transparent;
      }
      .navbar-default .navbar-nav > li > a {
        color: white;
        font-size: 16px;
        font-weight: bold;
      }
      .navbar-default .navbar-brand {
        color: white;
        font-weight: bold;
      }

      /* Style the card container */
      #card-container {
        background-color: white;
        border-radius: 15px;
        box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.2);
        padding: 40px;
        max-width: 800px;
        margin: 100px auto; /* Center the card vertically and horizontally */
        text-align: center;
      }

      /* Style the heading inside the card */
      #card-container h1 {
        color: #2b6b32;
        font-family: 'Georgia', serif;
        font-weight: bold;
        font-size: 36px;
      }
      
      /* style the buttoms in the tabs */
      .btn{
      background-color: #2b6b32; /* Green background */
        color: white; /* White text */
        border: none; /* Remove border */
        border-radius: 5px; /* Rounded corners */
        padding: 10px 20px; /* Button padding */
        font-size: 16px;
         width: 100%; /* Full width */
        font-weight: bold;
        cursor: pointer; /* Change cursor to pointer on hover */
      }
      
    ")
  ),
  tabPanel(icon = icon("home"),
           "Home",
           div(id = "card-container",
               h1("WELCOME TO CHAKU RandTreeR"),
               h4("This tool helps to randomize tree points for efficient data collection."),
               h4("The downloaded output file (in KML format) can be uploaded into the Google Earth app. This allows the user to easily locate tree points, with the map serving as a guide to pinpoint their exact locations.")
               
           )
  ),br(),
  tabPanel("Randomize Tree Layout",
           sidebarLayout(
             sidebarPanel(
               fileInput(inputId = "kmltrans",label = "File Upload",placeholder = "Upload KML File",accept = ".kml"),
               numericInput(inputId = "sample_size",label = "Enter Sample Size",value = 10 , min = 1 ,max = 100 ,step = 1),
               br(),
               numericInput(inputId = "plt_dist",label = "Enter Planting Distance (meters)",value = 10,min = 1, max = 100 ,step = 1 ),
               br(),
               numericInput(inputId = "sample_dist",label = "Enter Minimum Sampling Distance (meters)" ,value = 10 ,min = 1 ,max = 100, step = 1),br(),
               textInput(inputId = "farmer_name",label = "Enter Farmer's Name *",placeholder = "Farmer Name"),br(),
               actionButton(inputId = "Submit_2", label = "SUBMIT",width = "100%"),
               br(),br(),
               downloadButton(outputId = "download_1" , label = "DOWNLOAD KML FILE ",width = "100%")
               
             ),mainPanel(div(class = "card", # left blank to later comment
                             div(class = "plot-container", withSpinner(plotOutput("plot_rtrees")))
             ))
           ))
  
)

server <- function(input, output, session) {
  
  Chaku_sample_funct <- function(FILE_PATH, PLANTING_DISTANCE, SAMPLE_SIZE, MINIMUM_DISTANCE) {
    # Validate the file exists
    if (!file.exists(FILE_PATH)) stop("File not found: ", FILE_PATH)
    
    # Attempt to read the spatial data with error handling
    spatial_data <- tryCatch({
      st_read(FILE_PATH)
    }, error = function(e) {
      stop("Error reading spatial data: ", e$message)
    })
    
    # Check geometry type
    geometry_type <- st_geometry_type(spatial_data)
    
    # Process geometry based on type
    if (all(geometry_type == "POINT")) {
      polygon_data <- spatial_data %>%
        summarise(geometry = st_union(geometry)) %>%
        st_concave_hull(ratio = 0.1) # adjust as needed
    } else if (all(geometry_type %in% c("POLYGON", "MULTIPOLYGON"))) {
      polygon_data <- spatial_data
    } else {
      stop("Unsupported geometry type in the KML file.")
    }
    
    # Transform to projected CRS
    polygon_data_projected <- st_transform(polygon_data, 3857)
    
    # Generate grid
    planting_distance <- PLANTING_DISTANCE
    bbox <- st_bbox(polygon_data_projected)
    x_seq <- seq(bbox["xmin"], bbox["xmax"], by = planting_distance)
    y_seq <- seq(bbox["ymin"], bbox["ymax"], by = planting_distance)
    grid_points <- expand.grid(x = x_seq, y = y_seq)
    grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = 3857)
    
    # Filter grid points
    grid_points_within <- grid_sf[st_within(grid_sf, polygon_data_projected, sparse = FALSE), ]
    total_points <- nrow(grid_points_within)
    if (total_points == 0) stop("No points available within the polygon boundary.")
    
    # Validate sample size
    if (SAMPLE_SIZE <= 0 || SAMPLE_SIZE > total_points) {
      stop("Invalid sample size. Must be between 1 and ", total_points, ".")
    }
    
    # Spatial thinning
    shuffled_points <- grid_points_within[sample(nrow(grid_points_within)), ]
    sampled_points <- list()
    for (i in seq_len(nrow(shuffled_points))) {
      candidate_point <- shuffled_points[i, ]
      if (length(sampled_points) == 0) {
        sampled_points <- list(candidate_point)
      } else {
        distances <- st_distance(candidate_point, do.call(rbind, sampled_points))
        if (all(as.numeric(distances) >= MINIMUM_DISTANCE)) {
          sampled_points <- append(sampled_points, list(candidate_point))
        }
      }
      if (length(sampled_points) == SAMPLE_SIZE) break
    }
    
    sampled_points_sf <- do.call(rbind, sampled_points)
    sampled_points_sf$Name <- paste0("Tree ", seq_len(SAMPLE_SIZE))
    sampled_points_sf$Description <- "Sampled Mango Tree"
    
    plot_rand <- ggplot() +
      geom_sf(data = polygon_data, fill = "lightgreen", color = "black") +
      geom_sf(data = st_transform(sampled_points_sf, st_crs(polygon_data)), color = "red", size = 3) +
      geom_sf(data = spatial_data )+
      theme_minimal() +
      labs(title = "Randomly Selected Trees for Data Collection", subtitle = paste(SAMPLE_SIZE, "Trees marked as red points"))
    
    return(list(plot_rand = plot_rand, sampled_points_sf = sampled_points_sf))
  }
  
  # Reactive to handle the result of Chaku_sample_funct
  chaku_result <- eventReactive(input$Submit_2, {
    validate(need(input$kmltrans, "Please upload a KML file."))
    
    # Diagnostics: Log file path and validate existence
    file_path <- input$kmltrans$datapath
    shiny::showNotification(paste("File uploaded at:", file_path), type = "message")
    
    Chaku_sample_funct(
      FILE_PATH = file_path,
      PLANTING_DISTANCE = input$plt_dist,
      SAMPLE_SIZE = input$sample_size,
      MINIMUM_DISTANCE = input$sample_dist
    )
  })
  
  # Render the plot
  output$plot_rtrees <- renderPlot({
    req(chaku_result())
    chaku_result()$plot_rand
  })
  
  # Download handler
  output$download_1 <- downloadHandler(
    filename = function() {
      paste0(input$farmer_name, "_Randomised.kml")
    },
    content = function(file) {
      req(chaku_result())
      sf::st_write(chaku_result()$sampled_points_sf, file, driver = "KML")
    }
  )
  
}

shinyApp(ui, server)



shiny::runApp("C:/Users/rakro/Desktop/Chaku RandTreeR")
