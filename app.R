
library(shiny)
library(shinycssloaders)
library(readxl)
library(xml2)
library(sf)
library(dplyr)
library(ggplot2)
library(shinyalert)
library(zip)


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
        max-width: 1000px;
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
               h1("WELCOME TO CHAKU Tree Samplr"),hr(),
               h4("The Chaku Tree Sampler is a tool designed to convert farmers' geopoints into KML-ready files. 
    It also facilitates tree sampling for data collection, which can be viewed in the Google Earth app."),
               
               h4("Users must ensure that files intended for conversion to KML format contain the following column names, 
    exactly as listed: 'First Names/Prénom', 'Surname/Nom', 'Farm Number', and 'Geographic boundaries'.")
               
           )
  ),br(),
  useShinyalert(),
  tabPanel("Convert to KML File",
           sidebarLayout(
             sidebarPanel(
               fileInput("file_rec", "Upload File", accept = ".xlsx",placeholder = ".xlsx"),
               selectInput(inputId = "response" ,label = "Select Preferred File Format" ,choices = c("Polygon" ,"Points") ,selected = "Polygon"),
               actionButton("submit_1", "Convert", class = "btn",width = "100%",icon = icon("exchange")),br(), br(),
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
               fileInput(inputId = "kmltrans",label = "File Upload",placeholder = "Upload KML File",accept = ".kml",multiple = TRUE),
               numericInput(inputId = "sample_size",label = "Sample Size Required",value = 10 , min = 10 ,max = 10 ,step = 0),
               br(),
               actionButton(inputId = "Submit_2", label = "Randomize",width = "100%",icon = icon("random")),
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
  # Reactive list to store randomized file contents
  rv <- reactiveValues(randomized_files = list())
  
  # Function to process and randomize a single KML file
  Chaku_sample_funct <- function(FILE_PATH, SAMPLE_SIZE) {
    spatial_data <- st_read(FILE_PATH, quiet = TRUE)
    
    # Ensure valid polygon geometry
    if (!all(st_geometry_type(spatial_data) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("Invalid file type. Geometry must be a polygon.")
    }
    
    # Ensure CRS exists, otherwise set to WGS84
    if (is.na(st_crs(spatial_data))) {
      spatial_data <- st_set_crs(spatial_data, 4326)
    }
    
    # Transform CRS for accurate calculations
    polygon_data_projected <- st_transform(spatial_data, 3857)
    
    # Generate grid points inside the polygon
    planting_distance <- 3  
    bbox <- st_bbox(polygon_data_projected)
    x_seq <- seq(bbox["xmin"], bbox["xmax"], by = planting_distance)
    y_seq <- seq(bbox["ymin"], bbox["ymax"], by = planting_distance)
    grid_points <- expand.grid(x = x_seq, y = y_seq)
    grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = 3857)
    
    # Keep points inside the polygon
    grid_points_within <- grid_sf[st_within(grid_sf, polygon_data_projected, sparse = FALSE), ]
    
    # Validate sample size
    total_points <- nrow(grid_points_within)
    if (total_points == 0 || SAMPLE_SIZE > total_points) {
      return(NULL)
    }
    
    # Random sampling
    shuffled_points <- grid_points_within[sample(nrow(grid_points_within)), ]
    sampled_points <- list()
    min_distance <- 5  
    
    for (i in seq_len(nrow(shuffled_points))) {
      candidate_point <- shuffled_points[i, ]
      if (length(sampled_points) == 0 || all(as.numeric(st_distance(candidate_point, do.call(rbind, sampled_points))) >= min_distance)) {
        sampled_points <- append(sampled_points, list(candidate_point))
      }
      if (length(sampled_points) == SAMPLE_SIZE) break
    }
    
    # Convert to sf object
    sampled_points_sf <- do.call(rbind, sampled_points)
    sampled_points_sf$Name <- paste0("Tree ", seq_len(SAMPLE_SIZE))
    sampled_points_sf$Description <- "Sampled Tree"
    
    return(list(polygon = spatial_data, sampled_points = sampled_points_sf))
  }
  
  # Process multiple files
  observeEvent(input$Submit_2, {
    req(input$kmltrans)  # Ensure files are uploaded
    
    uploaded_files <- input$kmltrans$datapath
    original_filenames <- input$kmltrans$name  # Extract original names
    total_files <- length(uploaded_files)
    
    # Clear previous files
    rv$randomized_files <- list()
    
    withProgress(message = "Randomizing files...", value = 0, {
      for (i in seq_along(uploaded_files)) {
        tryCatch({
          result <- Chaku_sample_funct(uploaded_files[i], input$sample_size)
          if (!is.null(result)) {
            polygon_data <- result$polygon
            sampled_points <- result$sampled_points
            
            # Ensure CRS consistency
            sampled_points <- st_transform(sampled_points, st_crs(polygon_data))
            
            # Drop Z-dimension
            polygon_data_2D <- st_zm(polygon_data, drop = TRUE)
            sampled_points_2D <- st_zm(sampled_points, drop = TRUE)
            
            # Align columns
            all_columns <- union(names(polygon_data_2D), names(sampled_points_2D))
            
            for (col in setdiff(all_columns, names(polygon_data_2D))) {
              polygon_data_2D[[col]] <- NA
            }
            
            for (col in setdiff(all_columns, names(sampled_points_2D))) {
              sampled_points_2D[[col]] <- NA
            }
            
            # Reorder columns
            polygon_data_2D <- polygon_data_2D[, all_columns, drop = FALSE]
            sampled_points_2D <- sampled_points_2D[, all_columns, drop = FALSE]
            
            # Ensure unique column names
            names(polygon_data_2D) <- make.unique(names(polygon_data_2D))
            names(sampled_points_2D) <- make.unique(names(sampled_points_2D))
            
            # Merge polygon and sampled points
            combined_data <- rbind(polygon_data_2D, sampled_points_2D)
            
            # Fix: Create a real temporary file for st_write()
            temp_kml <- tempfile(fileext = ".kml")
            sf::st_write(combined_data, temp_kml, driver = "KML", quiet = TRUE)
            
            # Read file content into memory
            kml_data <- readBin(temp_kml, "raw", file.info(temp_kml)$size)
            
            # Store file in reactive list
            rv$randomized_files[[i]] <- list(
              name = paste0(tools::file_path_sans_ext(original_filenames[i]), "_randomized.kml"),
              content = kml_data
            )
          }
        }, error = function(e) {
          print(paste("Error processing:", uploaded_files[i], e$message))
        })
        
        incProgress(1 / total_files, detail = paste("Processing", i, "of", total_files))
      }
    })
    
    shinyalert("Done!", paste(length(rv$randomized_files), "files have been randomized and saved."), type = "success", timer = 3000)
  })
  
  
  
  # Download handler for randomized files
  output$download_1 <- downloadHandler(
    filename = function() {
      "Randomized_KML_Files.zip"
    },
    content = function(file) {
      req(length(rv$randomized_files) > 0)
      
      # Create a temporary directory to hold files
      temp_dir <- tempdir()
      dir.create(temp_dir, showWarnings = FALSE)
      
      # Write files to temp directory
      zip_file <- file.path(temp_dir, "Randomized_KML_Files.zip")
      file_list <- c()  # Store valid file paths
      
      for (i in seq_along(rv$randomized_files)) {
        file_path <- file.path(temp_dir, rv$randomized_files[[i]]$name)
        writeBin(rv$randomized_files[[i]]$content, file_path)
        file_list <- c(file_list, file_path)
      }
      
      # Zip the valid files
      zip::zipr(zip_file, file_list)
      
      # Return the ZIP file
      file.copy(zip_file, file, overwrite = TRUE)
    },
    contentType = "application/zip"
  )
}


shinyApp(ui, server)







# Function to process and randomize a single KML file
Chaku_sample_funct <- function(FILE_PATH, SAMPLE_SIZE) {
  spatial_data <- st_read(FILE_PATH, quiet = TRUE)
  
  # Ensure it's a valid polygon
  if (!all(st_geometry_type(spatial_data) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Invalid file type. Geometry must be polygon.")
  }
  
  # Transform CRS for accurate calculations
  polygon_data_projected <- st_transform(spatial_data, 3857)
  
  # Generate grid points inside the polygon
  planting_distance <- 3  
  bbox <- st_bbox(polygon_data_projected)
  x_seq <- seq(bbox["xmin"], bbox["xmax"], by = planting_distance)
  y_seq <- seq(bbox["ymin"], bbox["ymax"], by = planting_distance)
  grid_points <- expand.grid(x = x_seq, y = y_seq)
  grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = 3857)
  
  # Keep points inside the polygon
  grid_points_within <- grid_sf[st_within(grid_sf, polygon_data_projected, sparse = FALSE), ]
  
  # Validate sample size
  total_points <- nrow(grid_points_within)
  if (total_points == 0 || SAMPLE_SIZE > total_points) {
    return(NULL)
  }
  
  # Random sampling
  shuffled_points <- grid_points_within[sample(nrow(grid_points_within)), ]
  sampled_points <- list()
  min_distance <- 5  
  
  for (i in seq_len(nrow(shuffled_points))) {
    candidate_point <- shuffled_points[i, ]
    if (length(sampled_points) == 0 || all(as.numeric(st_distance(candidate_point, do.call(rbind, sampled_points))) >= min_distance)) {
      sampled_points <- append(sampled_points, list(candidate_point))
    }
    if (length(sampled_points) == SAMPLE_SIZE) break
  }
  
  # Convert to sf object
  sampled_points_sf <- do.call(rbind, sampled_points)
  sampled_points_sf$Name <- paste0("Tree ", seq_len(SAMPLE_SIZE))
  sampled_points_sf$Description <- "Sampled Tree"
  
  return(list(polygon = spatial_data, sampled_points = sampled_points_sf))
}

# Process multiple files
randomized_files <- eventReactive(input$Submit_2, {
  req(input$kmltrans)  # Ensure files are uploaded
  
  uploaded_files <- input$kmltrans$datapath
  original_filenames <- input$kmltrans$name  # Extract original names
  total_files <- length(uploaded_files)
  output_files <- list()
  
  withProgress(message = "Randomizing files...", value = 0, {
    for (i in seq_along(uploaded_files)) {
      tryCatch({
        result <- Chaku_sample_funct(uploaded_files[i], input$sample_size)
        if (!is.null(result)) {
          polygon_data <- result$polygon
          sampled_points <- result$sampled_points
          
          # Ensure CRS consistency
          sampled_points <- st_transform(sampled_points, st_crs(polygon_data))
          
          # Drop Z-dimension
          polygon_data_2D <- st_zm(polygon_data, drop = TRUE)
          sampled_points_2D <- st_zm(sampled_points, drop = TRUE)
          
          # Align columns
          all_columns <- union(names(polygon_data_2D), names(sampled_points_2D))
          
          for (col in setdiff(all_columns, names(polygon_data_2D))) {
            polygon_data_2D[[col]] <- NA
          }
          
          for (col in setdiff(all_columns, names(sampled_points_2D))) {
            sampled_points_2D[[col]] <- NA
          }
          
          # Reorder columns
          polygon_data_2D <- polygon_data_2D[, all_columns, drop = FALSE]
          sampled_points_2D <- sampled_points_2D[, all_columns, drop = FALSE]
          
          # Ensure unique column names
          names(polygon_data_2D) <- make.unique(names(polygon_data_2D))
          names(sampled_points_2D) <- make.unique(names(sampled_points_2D))
          
          # Merge polygon and sampled points
          combined_data <- rbind(polygon_data_2D, sampled_points_2D)
          
          # Save KML to an in-memory file
          kml_file <- tempfile(fileext = ".kml")
          sf::st_write(combined_data, kml_file, driver = "KML", quiet = TRUE)
          
          # Read the file into memory
          output_files[[i]] <- list(
            name = paste0(tools::file_path_sans_ext(original_filenames[i]), "_randomized.kml"),
            content = readBin(kml_file, "raw", file.info(kml_file)$size)
          )
        }
      }, error = function(e) {
        print(paste("Error processing:", uploaded_files[i], e$message))
      })
      
      incProgress(1 / total_files, detail = paste("Processing", i, "of", total_files))
    }
  })
  
  shinyalert("Done!", paste(length(output_files), "files have been randomized and saved."), type = "success", timer = 3000)
  return(output_files)
})



# Download handler for randomized files
output$download_1 <- downloadHandler(
  filename = function() {
    "Randomized_KML_Files.zip"
  },
  content = function(file) {
    req(randomized_files())
    
    # Create a temporary directory
    temp_dir <- tempdir()
    dir.create(temp_dir, showWarnings = FALSE)
    
    # Write files to the temporary directory
    for (i in seq_along(randomized_files())) {
      file_path <- file.path(temp_dir, randomized_files()[[i]]$name)
      writeBin(randomized_files()[[i]]$content, file_path)
    }
    
    # Zip the files
    zip_file <- file.path(temp_dir, "Randomized_KML_Files.zip")
    zip::zipr(zip_file, list.files(temp_dir, full.names = TRUE))
    
    # Return the zip file
    file.copy(zip_file, file)
  },
  contentType = "application/zip"
)