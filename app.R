library(shiny)
library(shinycssloaders)
library(readxl)
library(xml2)
library(sf)
library(dplyr)
library(ggplot2)
library(shinyalert)


#~~~~~~~~~~~~~ Function to create the KML file structure with styled polygons ~~~~~~~~~~~~#
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
               h1("WELCOME TO CHAKU Tree SampleR")
           )
  ),br(),
  useShinyalert(),
  tabPanel("Convert to KML File",
           sidebarLayout(
             sidebarPanel(
               fileInput("file_rec", "Upload File", accept = c(".csv", ".txt",".xlsx")),
               selectInput(inputId = "response" ,label = "Select Preferred File Format" ,choices = c("Polygon" ,"Points") ,selected = "Polygon"),
               actionButton("submit_1", "SUBMIT", class = "btn",width = "100%"),br(), br(),
               downloadButton(outputId = "download_01",label = "DOWNLOAD FOLDER!",icon = icon("download")),
               width = 4
             ),
             mainPanel(
               div(class = "card",
                   h3( ), # left blank to later comment
                   div(class = "plot-container")
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
               downloadButton(outputId = "download_1" , label = "DOWNLOAD FILE ",width = "100%",icon = icon("download"))
               
             ),mainPanel(div(class = "card", # left blank to later comment
                             div(class = "plot-container", withSpinner(plotOutput("plot_rtrees")))
             ))
           ))
  
)


server <- function(input, output, session) {
  
  #Points KML script.
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
  # Create polygon kml function
  create_kml_polygon <- function(coordinate_sets) {
    # Create the root KML structure
    kml_doc <- xml_new_root("kml", xmlns = "http://www.opengis.net/kml/2.2")
    document_node <- xml_add_child(kml_doc, "Document")
    
    # Add Style
    style <- xml_add_child(document_node, "Style", id = "polygonStyle")
    linestyle <- xml_add_child(style, "LineStyle")
    xml_add_child(linestyle, "color", "ff00ff00")  # Green border
    xml_add_child(linestyle, "width", "1")
    polystyle <- xml_add_child(style, "PolyStyle")
    xml_add_child(polystyle, "color", "7f00ff00")  # Transparent green fill (AABBGGRR)
    
    # Add Placemark
    placemark <- xml_add_child(document_node, "Placemark")
    xml_add_child(placemark, "styleUrl", "#polygonStyle")
    polygon <- xml_add_child(placemark, "Polygon")
    outer_boundary <- xml_add_child(polygon, "outerBoundaryIs")
    linear_ring <- xml_add_child(outer_boundary, "LinearRing")
    
    # Combine coordinates into a single string
    coord_string <- paste(
      sapply(coordinate_sets, function(coords) {
        paste(coords$longitude, coords$latitude, coords$altitude, sep = ",")
      }),
      collapse = " "
    )
    
    # Add the coordinates to the LinearRing
    xml_add_child(linear_ring, "coordinates", coord_string)
    
    return(kml_doc)
  }
  
  #~~~~~~~~~~~~~ Function to process coordinate strings from the Excel file ~~~~~~~~~~~~#
  process_coordinates <- function(coordinate_string) {
    coordinate_sets <- list()
    
    # Split multiple coordinate sets by semicolon
    coordinates_list <- unlist(strsplit(coordinate_string, ";"))
    
    for (coord in coordinates_list) {
      # Split longitude, latitude, and altitude
      parts <- unlist(strsplit(trimws(coord), " "))
      if (length(parts) >= 3) {  # Ensure at least longitude, latitude, and altitude are present
        coordinate_sets <- append(coordinate_sets, list(list(
          latitude = as.numeric(parts[1]),
          longitude = as.numeric(parts[2]),
          altitude = as.numeric(parts[3])
        )))
      }
    }
    
    return(coordinate_sets)
  }
  
  #~~~~~~~~~~~~~ Main Script to Read Data and Generate KML Files ~~~~~~~~~~~~#
  file_conversion <- function(file_path,choice) {
    # Read the Excel file
    Geograph_data <- read_xlsx(file_path) |> as.data.frame()
    needed_data <- Geograph_data[, c("First Names/Prénom", "Surname/Nom", "Farm Number", "Geographic boundaries")]
    colnames(needed_data)[4] <- "geographic_boundaries"
    
    # Temporary list to store file paths of generated KMLs
    created_files <- c()
    
    # Loop through rows to generate KML files
    for (take_row in seq_len(nrow(needed_data))) {
      row_hold <- needed_data[take_row, ]
      
      # Extract geographic boundaries and farmer name
      get_geopoint <- row_hold[["geographic_boundaries"]]
      get_farmer_name <- paste(row_hold[["First Names/Prénom"]], row_hold[["Surname/Nom"]],
                               "FARM", row_hold[["Farm Number"]], sep = "_")
      
      # Process coordinates
      all_coordinates <- process_coordinates(get_geopoint)
      if (length(all_coordinates) > 0) {
        all_coordinates <- c(all_coordinates, all_coordinates[1])  # Close polygon
      }
      
      if(choice == "Polygon"){
        # Create the KML structure
        kml_doc <- create_kml_polygon(all_coordinates) 
      } else if(choice == "Points") {
        
        kml_doc <- create_kml(all_coordinates)
        
      }
     
      
      # Save KML to a temporary file
      temp_file <- temp_file <- file.path(tempdir(), paste0(get_farmer_name, ".kml"))
      write_xml(kml_doc, file = temp_file)
      
      # Append to created files
      created_files <- c(created_files, temp_file)
    }
    
    # Return the list of generated KML file paths
    return(created_files)
  }
  
 
   

  reactive_files <- eventReactive(input$submit_1, {
    req(input$file_rec)  # Ensure a file is uploaded
    files <- NULL
    withProgress(message = "Generating KML Files...", value = 0, {
      files <- file_conversion(input$file_rec$datapath ,choice = input$response )  # Generate KML files
      incProgress(1, detail = "Finalizing...")
    })
    
    files
  })
  
  # Alert window.
  observeEvent(input$submit_1, {
    req(reactive_files())  # Ensure files are generated
    shinyalert("Done!", "KML files are ready for download.", type = "success", timer = 3000)
  })
  
  output$download_01 <- downloadHandler(
    filename = function() {
      paste0("Converted_KML_Files_", ".zip")
    },
    content = function(file) {
      files_to_zip <- reactive_files()
      zip::zipr(file, files_to_zip)
    },
    contentType = "application/zip"
  )
  
  # Function to process spatial data and sample points
  Chaku_sample_funct <- function(FILE_PATH, PLANTING_DISTANCE, SAMPLE_SIZE, MINIMUM_DISTANCE) {
    
    # Load spatial data
    spatial_data <- st_read(FILE_PATH)
    
    # Transform data to polygon if it's POINT
    geometry_type <- st_geometry_type(spatial_data)
    
    if (all(geometry_type == "POINT")) {
      stop("Geometry type must be polygon")
    } else if (all(geometry_type %in% c("POLYGON", "MULTIPOLYGON"))) {
      polygon_data <- spatial_data
    } else {
      stop("Unsupported geometry type in the uploaded file.")
    }
    
    # Transform polygon to projected CRS for accurate distance calculations
    polygon_data_projected <- st_transform(polygon_data, 3857)  # Web Mercator (suitable for West Africa)
    
    # Grid generation
    planting_distance <- PLANTING_DISTANCE  # Distance in meters
    bbox <- st_bbox(polygon_data_projected)
    x_seq <- seq(bbox["xmin"], bbox["xmax"], by = planting_distance)
    y_seq <- seq(bbox["ymin"], bbox["ymax"], by = planting_distance)
    grid_points <- expand.grid(x = x_seq, y = y_seq)
    grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = 3857)
    
    # Filter grid points within the polygon
    grid_points_within <- grid_sf[st_within(grid_sf, polygon_data_projected, sparse = FALSE), ]
    
    # Check if enough points are available
    total_points <- nrow(grid_points_within)
    if (total_points == 0) stop("No points available within the polygon boundary.")
    
    # User input for sampling
    sample_size <- SAMPLE_SIZE
    if (sample_size <= 0 || sample_size > total_points) {
      stop("Invalid sample size. Enter a number between 1 and ", total_points, ".")
    }
    
    # Minimum distance between sampled points
    min_distance <- MINIMUM_DISTANCE  # Minimum distance in meters
    
    # Spatial thinning algorithm for sampling
    shuffled_points <- grid_points_within[sample(nrow(grid_points_within)), ]
    sampled_points <- list()
    
    for (i in seq_len(nrow(shuffled_points))) {
      candidate_point <- shuffled_points[i, ]
      
      if (length(sampled_points) == 0) {
        sampled_points <- list(candidate_point)
      } else {
        distances <- st_distance(candidate_point, do.call(rbind, sampled_points))
        if (all(as.numeric(distances) >= min_distance)) {
          sampled_points <- append(sampled_points, list(candidate_point))
        }
      }
      
      if (length(sampled_points) == sample_size) break
    }
    
    # Convert sampled points to sf object
    sampled_points_sf <- do.call(rbind, sampled_points)
    
    # Add labels and descriptions
    sampled_points_sf$Name <- paste0("Tree ", seq_len(sample_size))
    sampled_points_sf$Description <- "Sampled Tree"
    
    # Visualization of sampled points
    plot_rand <- ggplot() +
      geom_sf(data = st_transform(polygon_data, st_crs(grid_points_within)), fill = "lightgreen", color = "black") +
      geom_sf(data = st_transform(grid_points_within, st_crs(grid_points_within)), color = "darkgreen", size = 1, alpha = 0.5) +
      geom_sf(data = st_transform(sampled_points_sf, st_crs(grid_points_within)), color = "red", size = 3) +
      theme_minimal() +
      labs(
        title = "Randomly Selected Trees for Data Collection",
        subtitle = paste(sample_size, "Trees marked as red points")
      )
    
    return(list(plot_rand = plot_rand, sampled_points_sf = sampled_points_sf))
  }
  
  # Reactive to process data when the user submits the form
  chaku_result <- eventReactive(input$Submit_2, {
    req(input$kmltrans)  # Ensure file is uploaded
    Chaku_sample_funct(
      FILE_PATH = input$kmltrans$datapath,
      PLANTING_DISTANCE = input$plt_dist,
      SAMPLE_SIZE = input$sample_size,
      MINIMUM_DISTANCE = input$sample_dist
    )
  })
  
  # Render the plot in the UI
  output$plot_rtrees <- renderPlot({
    req(chaku_result())  # Ensure the reactive result is available
    chaku_result()$plot_rand
  })
  
  # Download the sampled points as a KML file
  output$download_1 <- downloadHandler(
    filename = function() {
      paste0(input$farmer_name, "_RANDOMISED.kml")
    },
    content = function(file) {
      req(chaku_result())  # Ensure the reactive result is available
      sf::st_write(chaku_result()$sampled_points_sf, file, driver = "KML")
    }
  )
}

shinyApp(ui, server)


