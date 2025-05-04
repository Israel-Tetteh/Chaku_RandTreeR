#---------------------------------------------------------------------------
                #Function to create Points KML script.
create_kml <- function(coordinate_sets) {
  tryCatch({
    kml_doc <- xml_new_root("kml", xmlns = "http://www.opengis.net/kml/2.2")
    document_node <- xml_add_child(kml_doc, "Document")
    
    for (coords in coordinate_sets) {
      placemark <- xml_add_child(document_node, "Placemark")
      point <- xml_add_child(placemark, "Point")
      coord_string <- paste(coords$longitude, coords$latitude, coords$altitude, sep = ",")
      xml_add_child(point, "coordinates", coord_string)
    }
    
    return(kml_doc)
  }, error = function(e) {
    errors$convert <- paste("Error creating KML points:", e$message)
    return(NULL)
  })
}

#---------------------------------------------------------------------------
                    # Function to create polygon kml
create_kml_polygon <- function(coordinate_sets) {
  tryCatch({
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
  }, error = function(e) {
    errors$convert <- paste("Error creating KML polygon:", e$message)
    return(NULL)
  })
}


#---------------------------------------------------------------------------
# Function to process coordinate strings from the Excel file#
process_coordinates <- function(coordinate_string) {
  tryCatch({
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
  }, error = function(e) {
    errors$convert <- paste("Error processing coordinates:", e$message)
    return(list())
  })
}

#---------------------------------------------------------------------------
            # Main Script to Read Data and Generate KML Files
file_conversion <- function(file_path, choice) {
  tryCatch({
    # Read the Excel file
    Geograph_data <- read_xlsx(file_path) |> as.data.frame()
    
    # Check if required columns exist
    required_cols <- c("First Names/Prénom", "Surname/Nom", "Farm Number", "Geographic boundaries")
    missing_cols <- required_cols[!required_cols %in% colnames(Geograph_data)]
    
    if (length(missing_cols) > 0) {
      errors$convert <- paste("Missing required columns:", paste(missing_cols, collapse = ", "))
      return(character(0))
    }
    
    needed_data <- Geograph_data[, required_cols]
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
      if (length(all_coordinates) == 0) {
        next  # Skip this row if no valid coordinates
      }
      
      if (length(all_coordinates) > 0) {
        all_coordinates <- c(all_coordinates, all_coordinates[1])  # Close polygon
      }
      
      if(choice == "Polygon"){
        # Create the KML structure
        kml_doc <- create_kml_polygon(all_coordinates) 
      } else if(choice == "Points") {
        kml_doc <- create_kml(all_coordinates)
      }
      
      if (is.null(kml_doc)) {
        next  # Skip if KML creation failed
      }
      
      # Save KML to a temporary file
      temp_file <- file.path(tempdir(), paste0(get_farmer_name, ".kml"))
      write_xml(kml_doc, file = temp_file)
      
      # Append to created files
      created_files <- c(created_files, temp_file)
    }
    
    # Return the list of generated KML file paths
    return(created_files)
  }, error = function(e) {
    errors$convert <- paste("Error in file conversion:", e$message)
    return(character(0))
  })
}
              # End of tab to generate KML Files
#---------------------------------------------------------------------------


             # Start for function to create randomized tree points
#---------------------------------------------------------------------------
              # Function to create a combined KML with both polygon and points
create_combined_kml <- function(polygon_data, sampled_points) {
  tryCatch({
    # Create the KML structure
    kml_doc <- xml_new_root("kml", xmlns = "http://www.opengis.net/kml/2.2")
    document_node <- xml_add_child(kml_doc, "Document")
    
    # Add styles
    # Polygon style
    polygon_style <- xml_add_child(document_node, "Style", id = "polygonStyle")
    line_style <- xml_add_child(polygon_style, "LineStyle")
    xml_add_child(line_style, "color", "ff00ff00")  # Green border
    xml_add_child(line_style, "width", "2")
    poly_style <- xml_add_child(polygon_style, "PolyStyle")
    xml_add_child(poly_style, "color", "4d00ff00")  # Transparent green fill
    
    # Point style
    point_style <- xml_add_child(document_node, "Style", id = "pointStyle")
    icon_style <- xml_add_child(point_style, "IconStyle")
    xml_add_child(icon_style, "color", "ff0000ff")  # Red
    xml_add_child(icon_style, "scale", "1.0")
    xml_add_child(point_style, "LabelStyle")
    ?xml_add_child()
    # Add polygon placemark
    for (i in 1:nrow(polygon_data)) {
      polygon_placemark <- xml_add_child(document_node, "Placemark")
      xml_add_child(polygon_placemark, "name", "Farm Boundary")
      xml_add_child(polygon_placemark, "styleUrl", "#polygonStyle")
      
      # Get the polygon geometry
      geom <- st_geometry(polygon_data[i,])
      coords <- st_coordinates(geom)
      
      # Create polygon element
      polygon_elem <- xml_add_child(polygon_placemark, "Polygon")
      outer_boundary <- xml_add_child(polygon_elem, "outerBoundaryIs")
      linear_ring <- xml_add_child(outer_boundary, "LinearRing")
      
      # Format coordinates as "lon,lat,0 lon,lat,0..."
      coord_string <- paste(apply(coords, 1, function(coord) {
        paste(coord[1], coord[2], "0", sep = ",")
      }), collapse = " ")
      
      xml_add_child(linear_ring, "coordinates", coord_string)
    }
    
    # Add point placemarks
    for (i in 1:nrow(sampled_points)) {
      point_placemark <- xml_add_child(document_node, "Placemark")
      xml_add_child(point_placemark, "name", paste0("Tree ", i))
      xml_add_child(point_placemark, "description", "Sampled Tree")
      xml_add_child(point_placemark, "styleUrl", "#pointStyle")
      
      # Get point coordinates
      pt_geom <- st_geometry(sampled_points[i,])
      pt_coords <- st_coordinates(pt_geom)
      
      point_elem <- xml_add_child(point_placemark, "Point")
      coord_string <- paste(pt_coords[1], pt_coords[2], "0", sep = ",")
      xml_add_child(point_elem, "coordinates", coord_string)
    }
    
    # Create a temp file and write the KML
    temp_kml <- tempfile(fileext = ".kml")
    write_xml(kml_doc, file = temp_kml)
    
    return(temp_kml)
  }, error = function(e) {
    errors$randomize <- paste("Error creating combined KML:", e$message)
    return(NULL)
  })
}

#---------------------------------------------------------------------------
            # Function to process and randomize a single KML file
Chaku_sample_funct <- function(FILE_PATH, SAMPLE_SIZE) {
  tryCatch({
    # Read the KML file
    spatial_data <- st_read(FILE_PATH, quiet = TRUE)
    
    # Ensure valid polygon geometry
    if (!all(st_geometry_type(spatial_data) %in% c("POLYGON", "MULTIPOLYGON"))) {
      errors$randomize <- "Invalid file type. Geometry must be a polygon."
      return(NULL)
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
    if (total_points == 0) {
      errors$randomize <- "No points found inside the polygon. The polygon may be too small."
      return(NULL)
    }
    
    if (SAMPLE_SIZE > total_points) {
      errors$randomize <- paste("Requested sample size (", SAMPLE_SIZE, 
                                ") exceeds available points (", total_points, 
                                "). Using maximum available points instead.")
      SAMPLE_SIZE <- total_points
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
    
    # If we didn't get enough points with the minimum distance constraint, just take the first N
    if (length(sampled_points) < SAMPLE_SIZE) {
      remaining_needed <- SAMPLE_SIZE - length(sampled_points)
      used_indices <- sapply(sampled_points, function(pt) which(shuffled_points$geometry == pt$geometry))
      unused_points <- shuffled_points[-used_indices, ][1:min(remaining_needed, nrow(shuffled_points) - length(used_indices)), ]
      sampled_points <- c(sampled_points, lapply(1:nrow(unused_points), function(i) unused_points[i,]))
    }
    
    # Convert to sf object
    sampled_points_sf <- do.call(rbind, sampled_points)
    
    # Transform back to original CRS for consistency
    sampled_points_sf <- st_transform(sampled_points_sf, st_crs(spatial_data))
    
    return(list(polygon = spatial_data, sampled_points = sampled_points_sf))
  }, error = function(e) {
    errors$randomize <- paste("Error in sampling process:", e$message)
    return(NULL)
  })
}

# Install pacman package if not available
if(!require('pacman'))install.packages('pacman')

# Import p_load to automatically install and load packages
library(pacman)
pacman::p_load(shiny,shinycssloaders,
              readxl,xml2,
              sf,dplyr,ggplot2,
              shinyalert,zip,bslib,fontawesome)


# UI COMPONENT
ui <- fluidPage(
  # Use bslib for modern Bootstrap 5 styling
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2b6b32",
    success = "#35953a",
    danger = "#d9534f"
  ),
  
  # Custom CSS with cleaner implementation
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #f5f5f5;
      }
      .nav-tabs .nav-link.active {
        color: #2b6b32;
        font-weight: 500;
      }
      .card {
        border-radius: 12px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        padding: 24px;
        margin-bottom: 20px;
      }
      .home-section {
        padding: 2rem;
        background-color: white;
        border-radius: 12px;
        margin-top: 20px;
        text-align: center;
      }
      .home-card {
        padding: 2rem;
        margin-bottom: 1.5rem;
        background-color: white;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      .feature-icon {
        font-size: 24px;
        color: #2b6b32;
        margin-bottom: 12px;
      }
      .app-header {
        background-color: #2b6b32;
        color: white;
        padding: 15px 0;
        margin-bottom: 20px;
        text-align: center;
      }
      .action-button {
        background-color: #2b6b32;
        border-color: #2b6b32;
        transition: all 0.3s;
      }
      .action-button:hover {
        background-color: #22562a;
        border-color: #22562a;
      }
    "))
  ),
  
  # App header
  div(class = "app-header",
      h1("ChakuTech Tree Sampler", style = "font-weight: 700;"),
      p("Transform farm data into spatial insights")
  ),
  
  # Main navigation
  navbarPage(
    title = NULL,
    id = "navbar",
    
    # Home tab with improved layout and information
    tabPanel(
      icon = icon("home"), "Home",
      div(class = "container",
          div(class = "row justify-content-center",
              div(class = "col-lg-10",
                  div(class = "home-section",
                      h2("Welcome to ChakuTech Tree Sampler", style = "color: #2b6b32; font-weight: 700;"),
                      p(class = "lead", "A powerful tool for agricultural data management and tree sampling optimization"),
                      hr(),
                      
                      # Feature cards with icons
                      div(class = "row text-center mt-4",
                          # Feature 1
                          div(class = "col-md-4",
                              div(class = "home-card",
                                  tags$i(class = "fas fa-file-export feature-icon"),
                                  h4("Convert Farm Data to KML"),
                                  p("Transform your Excel data into KML files compatible with Google Earth and other GIS software.")
                              )
                          ),
                          # Feature 2
                          div(class = "col-md-4",
                              div(class = "home-card",
                                  tags$i(class = "fas fa-random feature-icon"),
                                  h4("Optimize Tree Sampling"),
                                  p("Generate statistically valid random tree samples within farm boundaries for efficient data collection.")
                              )
                          ),
                          # Feature 3
                          div(class = "col-md-4",
                              div(class = "home-card",
                                  tags$i(class = "fas fa-map-marked-alt feature-icon"),
                                  h4("Visualize Results"),
                                  p("Preview your farm boundaries and sampled trees with interactive maps.")
                              )
                          )
                      ),
                      
                      # How to use section
                      div(class = "mt-5 text-start",
                          h3("How to Use", style = "color: #2b6b32;"),
                          div(class = "row",
                              div(class = "col-md-6",
                                  h5("For KML Conversion:"),
                                  tags$ol(
                                    tags$li("Upload your Excel file with the required columns:"),
                                    tags$ul(
                                      tags$li("First Names/Prénom"),
                                      tags$li("Surname/Nom"),
                                      tags$li("Farm Number"),
                                      tags$li("Geographic boundaries")
                                    ),
                                    tags$li("Choose your preferred output format (Polygon or Points)"),
                                    tags$li("Click Convert and download the resulting KML files")
                                  )
                              ),
                              div(class = "col-md-6",
                                  h5("For Tree Randomization:"),
                                  tags$ol(
                                    tags$li("Upload one or more KML files containing farm boundaries"),
                                    tags$li("Select the required sample size for trees"),
                                    tags$li("Click Randomize to generate randomly placed trees"),
                                    tags$li("Download the resulting KML files for use in Google Earth")
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
    ),
    
    # KML Conversion tab with improved layout
    tabPanel(
      "Convert to KML",
      icon = icon("file-export"),
      div(class = "container",
          div(class = "row",
              div(class = "col-md-4",
                  div(class = "card",
                      h3("Upload & Convert", style = "color: #2b6b32; margin-bottom: 20px;"),
                      fileInput("file_rec", "Upload Excel File", 
                                accept = c(".xlsx", ".xls"),
                                placeholder = "Select Excel file...",
                                buttonLabel = "Browse..."),
                      selectInput("response", "Output Format", 
                                  choices = c("Polygon", "Points"),
                                  selected = "Polygon"),
                      actionButton("submit_1", "Convert Files", 
                                   class = "btn btn-primary action-button w-100",
                                   icon = icon("exchange-alt")),
                      hr(),
                      uiOutput("download_kml_ui")
                  )
              ),
              div(class = "col-md-8",
                  div(class = "card",
                      h3("Conversion Status", style = "color: #2b6b32;"),
                      uiOutput("convert_status_ui"),
                      div(id = "convert_progress_container",
                          withSpinner(textOutput("convert_status"))),
                      uiOutput("error_message_convert_ui")
                  )
              )
          )
      )
    ),
    
    # Tree Randomization tab with improved layout
    tabPanel(
      "Randomize Trees",
      icon = icon("random"),
      div(class = "container",
          div(class = "row",
              div(class = "col-md-4",
                  div(class = "card",
                      h3("Tree Sampling", style = "color: #2b6b32; margin-bottom: 20px;"),
                      fileInput("kmltrans", "Upload KML File(s)",
                                accept = ".kml",
                                multiple = TRUE,
                                buttonLabel = "Browse...",
                                placeholder = "Select KML file(s)..."),
                      numericInput("sample_size", "Sample Size", 
                                   value = 10, min = 5, max = 100, step = 5),
                      actionButton("Submit_2", "Randomize Trees", 
                                   class = "btn btn-primary action-button w-100",
                                   icon = icon("random")),
                      hr(),
                      uiOutput("download_random_ui")
                  )
              ),
              div(class = "col-md-8",
                  div(class = "card",
                      h3("Results Preview", style = "color: #2b6b32;"),
                      uiOutput("randomize_status_ui"),
                      div(class = "plot-container", 
                          withSpinner(plotOutput("plot_rtrees", height = "500px"))),
                      uiOutput("error_message_ui")
                  )
              )
          )
      )
    ),
    
    # Help tab
    tabPanel(
      "Help & FAQ",
      icon = icon("question-circle"),
      div(class = "container",
          div(class = "row justify-content-center",
              div(class = "col-lg-10",
                  div(class = "card",
                      h3("Frequently Asked Questions", style = "color: #2b6b32;"),
                      div(class = "accordion", id = "faqAccordion",
                          # FAQ 1
                          div(class = "accordion-item",
                              h2(class = "accordion-header", id = "headingOne",
                                 tags$button(class = "accordion-button", type = "button",
                                             "data-bs-toggle" = "collapse", "data-bs-target" = "#collapseOne",
                                             "aria-expanded" = "true", "aria-controls" = "collapseOne",
                                             "What file format should my input data be in?")
                              ),
                              div(id = "collapseOne", class = "accordion-collapse collapse show",
                                  "aria-labelledby" = "headingOne", "data-bs-parent" = "#faqAccordion",
                                  div(class = "accordion-body",
                                      "Your input data should be in Excel format (.xlsx or .xls) for the KML conversion. For the randomization feature, you should upload KML files (.kml) that contain farm boundary polygons.")
                              )
                          ),
                          # FAQ 2
                          div(class = "accordion-item",
                              h2(class = "accordion-header", id = "headingTwo",
                                 tags$button(class = "accordion-button collapsed", type = "button",
                                             "data-bs-toggle" = "collapse", "data-bs-target" = "#collapseTwo",
                                             "aria-expanded" = "false", "aria-controls" = "collapseTwo",
                                             "What columns must my Excel file contain?")
                              ),
                              div(id = "collapseTwo", class = "accordion-collapse collapse",
                                  "aria-labelledby" = "headingTwo", "data-bs-parent" = "#faqAccordion",
                                  div(class = "accordion-body",
                                      "Your Excel file must contain the following columns with exact spelling: 'First Names/Prénom', 'Surname/Nom', 'Farm Number', and 'Geographic boundaries'. The 'Geographic boundaries' column should contain coordinate data in a format that can be parsed into spatial polygons.")
                              )
                          ),
                          # FAQ 3
                          div(class = "accordion-item",
                              h2(class = "accordion-header", id = "headingThree",
                                 tags$button(class = "accordion-button collapsed", type = "button",
                                             "data-bs-toggle" = "collapse", "data-bs-target" = "#collapseThree",
                                             "aria-expanded" = "false", "aria-controls" = "collapseThree",
                                             "How are the random trees generated?")
                              ),
                              div(id = "collapseThree", class = "accordion-collapse collapse",
                                  "aria-labelledby" = "headingThree", "data-bs-parent" = "#faqAccordion",
                                  div(class = "accordion-body",
                                      "The app uses spatial random sampling to generate points within the farm boundary polygon. This ensures a statistically valid representation of the tree population while maintaining spatial distribution across the entire farm area.")
                              )
                          )
                      )
                  )
              )
          )
      )
    )
  ),
  useShinyalert()  # Enable shinyalert
)

# SERVER COMPONENT
server <- function(input, output, session) {
  # Reactive values for error handling and file management
  rv <- reactiveValues(
    convert_errors = NULL,
    randomize_errors = NULL,
    kml_files = list(),
    randomized_files = list(),
    processing = FALSE,
    randomizing = FALSE
  )
  
  # KML Conversion Logic
  observeEvent(input$submit_1, {
    req(input$file_rec)
    
    # Reset error messages and set processing flag
    rv$convert_errors <- NULL
    rv$kml_files <- list()
    rv$processing <- TRUE
    
    # Validate file
    tryCatch({
      # Check file extension
      if(!grepl("\\.xlsx$|\\.xls$", input$file_rec$name, ignore.case = TRUE)) {
        rv$convert_errors <- "Please upload an Excel file (.xlsx or .xls)"
        rv$processing <- FALSE
        return()
      }
      
      # Process file with progress indication
      withProgress(message = "Processing Excel data...", value = 0, {
        # Update progress
        incProgress(0.3, detail = "Validating data...")
        
        # Call file conversion function (this is a placeholder - your actual function would go here)
        tryCatch({
          # Replace with your actual function
          files <- file_conversion(input$file_rec$datapath, choice = input$response)
          
          incProgress(0.6, detail = "Generating KML files...")
          
          # Store files in reactive value
          if (length(files) > 0) {
            rv$kml_files <- files
          } else {
            rv$convert_errors <- "No valid KML files were generated. Please check your input file format."
          }
          
        }, error = function(e) {
          rv$convert_errors <- paste("Error processing file:", e$message)
        })
        
        incProgress(0.1, detail = "Finalizing...")
      })
      
    }, error = function(e) {
      rv$convert_errors <- paste("Error:", e$message)
    }, finally = {
      rv$processing <- FALSE
    })
    
    # Show success or error alert
    if (length(rv$kml_files) > 0) {
      shinyalert(
        title = "Success!",
        text = paste(length(rv$kml_files), "KML files were successfully created."),
        type = "success",
        timer = 3000
      )
    } else if (!is.null(rv$convert_errors)) {
      shinyalert(
        title = "Error",
        text = rv$convert_errors,
        type = "error"
      )
    }
  })
  
  # Dynamic UI for conversion status
  output$convert_status_ui <- renderUI({
    if (rv$processing) {
      div(
        h4("Processing...", style = "color: #2b6b32;"),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated", 
                role = "progressbar", 
                style = "width: 100%"))
      )
    } else if (length(rv$kml_files) > 0) {
      div(
        h4(icon("check-circle"), "Success!", style = "color: #28a745;"),
        p(paste("Successfully created", length(rv$kml_files), "KML files")),
        p("Your files are ready to download.")
      )
    } else if (!is.null(rv$convert_errors)) {
      div(
        h4(icon("exclamation-triangle"), "Error", style = "color: #dc3545;"),
        p(rv$convert_errors, style = "color: #dc3545;")
      )
    } else {
      div(
        p("Upload an Excel file and click 'Convert Files' to begin."),
        p("Make sure your file contains the required columns.")
      )
    }
  })
  
  # Dynamic download button UI for KML files
  output$download_kml_ui <- renderUI({
    if (length(rv$kml_files) > 0) {
      downloadButton(
        "download_01", 
        "Download KML Files", 
        class = "btn btn-success w-100",
        icon = icon("download")
      )
    } else {
      tags$div(
        downloadButton(
          "download_01", 
          "Download KML Files", 
          class = "btn btn-secondary w-100 disabled",
          icon = icon("download")
        ),
        tags$script("$('#download_01').prop('disabled', true);")
      )
    }
  })
  
  # Tree Randomization Logic
  observeEvent(input$Submit_2, {
    req(input$kmltrans)
    
    # Reset error messages and set processing flag
    rv$randomize_errors <- NULL
    rv$randomized_files <- list()
    rv$randomizing <- TRUE
    
    # Validate files
    uploaded_files <- input$kmltrans$datapath
    original_filenames <- input$kmltrans$name
    total_files <- length(uploaded_files)
    
    # Process files with progress indication
    withProgress(message = "Randomizing tree locations...", value = 0, {
      for (i in seq_along(uploaded_files)) {
        tryCatch({
          # Update progress
          incProgress(0.8 / total_files, detail = paste("Processing file", i, "of", total_files))
          
          # Validate file
          if (!grepl("\\.kml$", original_filenames[i], ignore.case = TRUE)) {
            if (is.null(rv$randomize_errors)) {
              rv$randomize_errors <- paste("File", original_filenames[i], "is not a KML file.")
            }
            next
          }
          
          # Call sampling function (this is a placeholder - your actual function would go here)
          result <- Chaku_sample_funct(uploaded_files[i], input$sample_size)
          
          if (!is.null(result)) {
            polygon_data <- result$polygon
            sampled_points <- result$sampled_points
            
            # Create KML
            temp_kml <- create_combined_kml(polygon_data, sampled_points)
            
            if (!is.null(temp_kml)) {
              # Read file content
              kml_data <- readBin(temp_kml, "raw", file.info(temp_kml)$size)
              
              # Store in reactive value
              rv$randomized_files[[i]] <- list(
                name = paste0(tools::file_path_sans_ext(original_filenames[i]), "_randomized.kml"),
                content = kml_data,
                polygon = polygon_data,
                points = sampled_points
              )
            }
          }
          
        }, error = function(e) {
          message <- paste("Error processing:", original_filenames[i], "-", e$message)
          if (is.null(rv$randomize_errors)) {
            rv$randomize_errors <- message
          }
        })
      }
      
      incProgress(0.2, detail = "Finalizing results...")
    })
    
    # Update randomizing status
    rv$randomizing <- FALSE
    
    # Show first result in plot if available
    if (length(rv$randomized_files) > 0) {
      # Plot using the first file
      first_file <- rv$randomized_files[[1]]
      output$plot_rtrees <- renderPlot({
        ggplot() +
          geom_sf(data = first_file$polygon, fill = "lightgreen", color = "darkgreen", alpha = 0.3) +
          geom_sf(data = first_file$points, color = "red", size = 3) +
          theme_minimal() +
          labs(title = "Farm Boundary with Sampled Trees",
               subtitle = paste(nrow(first_file$points), "trees randomly sampled"))
      })
      
      # Show success alert
      shinyalert(
        title = "Success!",
        text = paste(length(rv$randomized_files), "files have been randomized and are ready to download."),
        type = "success",
        timer = 3000
      )
    } else if (!is.null(rv$randomize_errors)) {
      # Show error alert
      shinyalert(
        title = "Error",
        text = rv$randomize_errors,
        type = "error"
      )
    }
  })
  
  # Dynamic UI for randomization status
  output$randomize_status_ui <- renderUI({
    if (rv$randomizing) {
      div(
        h4("Randomizing trees...", style = "color: #2b6b32;"),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated", 
                role = "progressbar", 
                style = "width: 100%"))
      )
    } else if (length(rv$randomized_files) > 0) {
      div(
        h4(icon("check-circle"), "Success!", style = "color: #28a745;"),
        p(paste("Successfully processed", length(rv$randomized_files), "files")),
        p("Your files are ready to download.")
      )
    } else if (!is.null(rv$randomize_errors)) {
      div(
        h4(icon("exclamation-triangle"), "Error", style = "color: #dc3545;"),
        p(rv$randomize_errors, style = "color: #dc3545;")
      )
    } else {
      div(
        p("Upload KML files and set sample size, then click 'Randomize Trees' to begin."),
        p("The preview will show the first file's result.")
      )
    }
  })
  
  # Dynamic download button UI for randomized files
  output$download_random_ui <- renderUI({
    if (length(rv$randomized_files) > 0) {
      downloadButton(
        "download_1", 
        "Download Randomized KML", 
        class = "btn btn-success w-100",
        icon = icon("download")
      )
    } else {
      tags$div(
        downloadButton(
          "download_1", 
          "Download Randomized KML", 
          class = "btn btn-secondary w-100 disabled",
          icon = icon("download")
        ),
        tags$script("$('#download_1').prop('disabled', true);")
      )
    }
  })
  
  # Error message UI for randomization
  output$error_message_ui <- renderUI({
    if (!is.null(rv$randomize_errors)) {
      div(class = "alert alert-danger",
          icon("exclamation-triangle"),
          rv$randomize_errors)
    }
  })
  
  # Download handler for KML files
  output$download_01 <- downloadHandler(
    filename = function() {
      paste0("Converted_KML_Files_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(length(rv$kml_files) > 0)
      
      # Zip files
      tryCatch({
        zip::zipr(file, rv$kml_files)
      }, error = function(e) {
        # Create an empty zip with error message as fallback
        temp_txt <- tempfile(fileext = ".txt")
        writeLines(paste("Error creating zip file:", e$message), temp_txt)
        zip::zipr(file, temp_txt)
      })
    },
    contentType = "application/zip"
  )
  
  # Download handler for randomized files
  output$download_1 <- downloadHandler(
    filename = function() {
      paste0("Randomized_KML_Files_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(length(rv$randomized_files) > 0)
      
      # Create a temporary directory
      temp_dir <- tempdir()
      file_list <- c()
      
      # Write files to temp directory
      for (i in seq_along(rv$randomized_files)) {
        file_path <- file.path(temp_dir, rv$randomized_files[[i]]$name)
        writeBin(rv$randomized_files[[i]]$content, file_path)
        file_list <- c(file_list, file_path)
      }
      
      # Zip the files
      tryCatch({
        zip::zipr(file, file_list)
      }, error = function(e) {
        # Create an empty zip with error message as fallback
        temp_txt <- tempfile(fileext = ".txt")
        writeLines(paste("Error creating zip file:", e$message), temp_txt)
        zip::zipr(file, temp_txt)
      })
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)