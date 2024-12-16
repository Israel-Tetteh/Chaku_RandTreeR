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
        background: url('forest.jpg') no-repeat center center fixed;
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
               actionButton("export_1", "EXPORT FILE", class = "btn",width = "100%"),
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
               numericInput(inputId = "plt_dist",label = "Enter Planting Size",value = 10,min = 1, max = 100 ,step = 1 ),
               br(),
               numericInput(inputId = "sample_dist",label = "Enter Minimum Sampling Distance" ,value = 10 ,min = 1 ,max = 100, step = 1),br(),
               actionButton(inputId = "Submit_2", label = "SUBMIT",width = "100%"),
               br(),br(),
               textInput(inputId = "farmer_name",label = "Enter Farmer's Name",placeholder = "Alex Narh"),br(),
               actionButton(inputId = "download_1" , label = "DOWNLOAD FILE ",width = "100%")
               
             ),mainPanel(div(class = "card",
                             h3( ), # left blank to later comment
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
        st_concave_hull(ratio = 0.1) # adjust to make boundary more accurate
    } else if (all(geometry_type %in% c("POLYGON", "MULTIPOLYGON"))) {
  polygon_data <- spatial_data
} else {
      stop("Unsupported geometry type in the KML file.")
    }
    
    # Transform polygon to projected CRS for accurate distance calculations
    polygon_data_projected <- st_transform(polygon_data, 3857)
    
    # Grid generation
    planting_distance <- PLANTING_DISTANCE   # in meters (required)
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
    
    
    # Minimum distance between sampled points (in meters)
    min_distance <- MINIMUM_DISTANCE # required
    
    # Spatial thinning algorithm
    # For reproducibility
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
      labs(title = "Dispersed Sampled Mango Trees", subtitle = paste(sample_size, "Trees for Data Collection"))
    
    return(plot_rand)
  }
  
  # For randomization lets define the server inputs.
  chaku_hold_1 <- eventReactive(input$Submit_2,{
    Chaku_sample_funct(FILE_PATH = input$kmltrans$datapath ,
                       
                       PLANTING_DISTANCE = input$plt_dist,
                       
                       SAMPLE_SIZE = input$sample_size ,
                       
                       MINIMUM_DISTANCE =  input$sample_dist
                       
                       
                       )
  })
  
  output$plot_rtrees <- renderPlot({
    chaku_hold_1()
    
  })
  
  
  
}

shinyApp(ui, server)
 


##########

# Function to covert geopoint to excel file.



# Load required packages
library(readxl)  # To read the Excel file
library(xml2)    # To handle XML generation

#~~~~~~~~~~~~~ Function to create the KML file structure ~~~~~~~~~~~~#
create_kml <- function(coordinate_sets) {
  kml_doc <- xml_new_root("kml", xmlns = "http://www.opengis.net/kml/2.2")
  document_node <- xml_add_child(kml_doc, "Document")
  
  for (coords in coordinate_sets) {
    placemark <- xml_add_child(document_node, "Placemark")
    point <- xml_add_child(placemark, "Point")
    coord_string <- paste(coords$longitude, coords$latitude, coords$altitude, sep = ",")
    xml_add_child(point, "coordinates", coord_string)
  }
  
  return(kml_doc)
}

#~~~~~ Function to process coordinate strings from the Excel file ~~~~~~#
process_coordinates <- function(coordinate_string) {
  coordinate_sets <- list()
  
  # Split multiple coordinate sets by semicolon
  coordinates_list <- unlist(strsplit(coordinate_string, ";"))
  
  for (coord in coordinates_list) {
    # Split longitude, latitude, and altitude
    parts <- unlist(strsplit(trimws(coord), " "))
    if (length(parts) >= 3) {  # Expect at least longitude, latitude, altitude
      coordinate_sets <- append(coordinate_sets, list(list(
        latitude = as.numeric(parts[1]),
        longitude = as.numeric(parts[2]),
        altitude = as.numeric(parts[3])
      )))
    }
  }
  
  return(coordinate_sets)
}

#~~~~~~~~~~~~~~~~~~ End of  process coordinate function ~~~~~~~~~~~~~~#
# Read the geographical file for kml.
Geograph_data <- read_xlsx("C:\\Users\\rakro\\Downloads\\Somanya farm boundaries.xlsx")
head(Geograph_data)
colnames(Geograph_data)

# A for loop to get the kml file for  each farmer.
rows_to_loop <- nrow(Geograph_data) # get the number of rows of the data.

for (take_row in seq_len(rows_to_loop)) {
  
  row_hold <- Geograph_data[ take_row , ]
  
  get_geopoint <- row_hold["geographic_boundaries"] # get row selected  geoboundaries
  get_farmer_name <- row_hold["full_name"] # get the farmer name to be used to save the kml file produced.
  
  # Process each row to extract coordinates
  all_coordinates <- list()
  for (coordinate_string in get_geopoint) {
    all_coordinates <- append(all_coordinates, process_coordinates(coordinate_string))
  }
  
  # Create the KML structure
  kml_doc <- create_kml(all_coordinates)
  
  # Save the KML to file
  output_file <-  paste0(get_farmer_name,"kml Geo_boundaries.kml")  # file name you will like to use .kml
  
  write_xml(kml_doc, file = output_file)
  
}
