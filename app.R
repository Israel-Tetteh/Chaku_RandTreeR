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
               actionButton("submit2", "SUBMIT", class = "btn",width = "100%"),br(), hr(),
               actionButton("export", "EXPORT FILE", class = "btn",width = "100%"),
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
               fileInput(inputId = "kmltrans",label = "File Upload",placeholder = "Upload KML File"),
               br(),
               numericInput(inputId = "sample_size",label = "Enter Sample Size",value = 10 , min = 1 ,max = 100 ,step = 1),
               br(),
               numericInput(inputId = "plt_dist",label = "Enter Planting Size",value = 10,min = 1, max = 100 ,step = 1 ),
               br(),
               numericInput(inputId = "sample_dist",label = "Enter Minimum Sampling Distance" ,value = 10 ,min = 1 ,max = 100, step = 1),
               actionButton(inputId = "Submit_2", label = "SUBMIT!")
               
             ),mainPanel(div(class = "card",
                             h3( ), # left blank to later comment
                             div(class = "plot-container", withSpinner(plotOutput("plot_rtrees")))
             ))
           ))
  
)

server <- function(input, output, session) {}

shinyApp(ui, server)



# # Color  code for buttons.
# tags$head(
#   tags$style(HTML("
#       .btn {
#         background-color: #2E7D32; /* Green color */
#         color: white; /* White text */
#         font-size: 16px; /* Text size */
#         padding: 10px 20px; /* Button padding */
#         border-radius: 5px; /* Rounded corners */
#         border: none; /* No border */
#         width: 100%; /* Full width */
#         cursor: pointer; /* Pointer cursor on hover */
#       }
#       .btn:hover {
#         background-color: #1B5E20; /* Darker green on hover */
#       }
#     "))
# ),
 