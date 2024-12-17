library(shiny) # load shiny app.
library(shiny)
library(shinycssloaders)
ui <- navbarPage(
  title = NULL,
  id = "navbar",
  header = tags$style(
    HTML("
      /* Set the background image */
      body {
        background: url('Mango-Orchard.png') no-repeat center center fixed;
        background-size: cover;
        font-family: Arial, sans-serif;
      }
      
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
               h1("WELCOME TO CHAKU RandTreeR")
           )
  ),br(),
  tabPanel("Convert to KML File",
           sidebarLayout(
             sidebarPanel(
               fileInput("fileInput", "Upload File", accept = c(".csv", ".txt",".xlsx")),
               checkboxInput(inputId = "checkin",label = "Bulk KML File",value = TRUE),
               actionButton("submit_1", "SUBMIT", class = "btn",width = "100%"),br(), hr(),
               downloadButton("export_1", "EXPORT FILE", class = "btn",width = "100%"),
               width = 4
             ),
             mainPanel(
               div(class = "card",
                   h3( ), # left blank to later comment
                   div(class = "plot-container", withSpinner(plotOutput("landPlot")))
               )) 
           )),br(),
  tabPanel("Randomize Tree Layout",
           sidebarLayout(
             sidebarPanel(
               fileInput(inputId = "kmltrans",label = "File Upload",placeholder = "Upload KML File",accept = ".kml"),
               numericInput(inputId = "sample_size",label = "Enter Sample Size",value = 10 , min = 1 ,max = 100 ,step = 1),
               br(),
               numericInput(inputId = "plt_dist",label = "Enter Planting Distance (meters)",value = 10,min = 1, max = 100 ,step = 1 ),
               br(),
               numericInput(inputId = "sample_dist",label = "Enter Minimum Sampling Distance (meters)" ,value = 10 ,min = 1 ,max = 100, step = 1),br(),
               textInput(inputId = "farmer_name",label = "Enter Farmer's Name",placeholder = "Alex Narh"),br(),
               actionButton(inputId = "Submit_2", label = "SUBMIT",width = "100%"),
               br(),br(),
               downloadButton(outputId = "download_1" , label = "DOWNLOAD FILE ",width = "100%")
               
             ),mainPanel(div(class = "card", # left blank to later comment
                             div(class = "plot-container", withSpinner(plotOutput("plot_rtrees")))
             ))
           ))
  
)


library(sf)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {
 
  Chaku_sample_funct <- function(FILE_PATH , PLANTING_DISTANCE , SAMPLE_SIZE , MINIMUM_DISTANCE ){
    # Load spatial data
    spatial_data <- st_read(FILE_PATH)
    
    # Transform data to polygon if it's POINT
    geometry_type <- st_geometry_type(spatial_data)
    
    if (all(geometry_type == "POINT")) {
      polygon_data <- spatial_data %>%
        summarise(geometry = st_union(geometry)) %>%
        st_concave_hull(ratio = 0.1) # adjust to make polygon boundary more accurate
    } else if (all(geometry_type %in% c("POLYGON", "MULTIPOLYGON"))) {
  polygon_data <- spatial_data
} else {
      stop("Unsupported geometry type in the KML file.")
    }
    
    # Transform polygon to projected CRS for accurate distance calculations
    polygon_data_projected <- st_transform(polygon_data, 3857) # 3857 is the best for West African calculations
    
    # Grid generation
    planting_distance <- PLANTING_DISTANCE   # in meters 
    bbox <- st_bbox(polygon_data_projected)
    x_seq <- seq(bbox["xmin"], bbox["xmax"], by = planting_distance)
    y_seq <- seq(bbox["ymin"], bbox["ymax"], by = planting_distance)
    grid_points <- expand.grid(x = x_seq, y = y_seq)
    grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = 3857)
    
    # Filter grid points within the polygon
    grid_points_within <- grid_sf[st_within(grid_sf, polygon_data_projected, sparse = FALSE), ]
    
    # Ensure enough points are available
    total_points <- nrow(grid_points_within)
    if (total_points == 0) stop("No points available within the polygon boundary.")
    
    # User input for sampling
    sample_size <- SAMPLE_SIZE # required
    if (sample_size <= 0 || sample_size > total_points) {
      stop("Invalid sample size. Enter a number between 1 and ", total_points, ".")
    }
    
    
    # Minimum distance between sampled points 
    min_distance <- MINIMUM_DISTANCE # required
    
    # Spatial thinning algorithm
    shuffled_points <- grid_points_within[sample(nrow(grid_points_within)), ]
    sampled_points <- list()
    
    for (i in seq_len(nrow(shuffled_points))) {
      candidate_point <- shuffled_points[i, ]
      
      # Check distance to all existing sampled points
      if (length(sampled_points) == 0) {
        sampled_points <- list(candidate_point)
      } else {
        distances <- st_distance(candidate_point, do.call(rbind, sampled_points))
        if (all(as.numeric(distances) >= min_distance)) {
          sampled_points <- append(sampled_points, list(candidate_point))
        }
      }
      
      # Stop if the required sample size is reached
      if (length(sampled_points) == sample_size) break
    }
    
    # Convert sampled points to an sf object
    sampled_points_sf <- do.call(rbind, sampled_points)
    
    # Add labels and descriptions
    sampled_points_sf$Name <- paste0("Tree ", seq_len(sample_size))
    sampled_points_sf$Description <- "Sampled Mango Tree"
    
    # Visualization of sampled points
    plot_rand <- ggplot() +
      geom_sf(data = polygon_data, fill = "lightgreen", color = "black") +
      geom_sf(data = st_transform(grid_points_within, st_crs(polygon_data)), color = "darkgreen", size = 1, alpha = 0.5) +
      geom_sf(data = st_transform(sampled_points_sf, st_crs(polygon_data)), color = "red", size = 3) +
      geom_sf(data = spatial_data )+
      theme_minimal() +
      labs(title = "Randomly Selected Trees for Data Collection", subtitle = paste(sample_size, "Trees marked as red points"))
    
    return(list(plot_rand = plot_rand, sampled_points_sf = sampled_points_sf))
  }
  
  # Reactive to hold the result of Chaku_sample_funct
  chaku_result <- eventReactive(input$Submit_2, {
    Chaku_sample_funct(
      FILE_PATH = input$kmltrans$datapath,
      PLANTING_DISTANCE = input$plt_dist,
      SAMPLE_SIZE = input$sample_size,
      MINIMUM_DISTANCE = input$sample_dist
    )
  })
  
  # Render the plot
  output$plot_rtrees <- renderPlot({
    req(chaku_result()) # Ensure the reactive is available
    chaku_result()$plot_rand
  })
  
  output$download_1 <- downloadHandler(
    filename = function() {
      paste0(input$farmer_name, ".kml")
    },
    content = function(file) {
      req(chaku_result())  # Ensure the reactive is available
      sf::st_write(chaku_result()$sampled_points_sf, file, driver = "KML")
    }
  )

}
shinyApp(ui, server)
 
 
 