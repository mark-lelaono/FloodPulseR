#' Start FloodPulseR GUI
#' @export
start_floodpulse <- function() {
  # Clear temporary files from previous sessions
  temp_files <- list.files(tempdir(), full.names = TRUE, recursive = TRUE)
  if (length(temp_files) > 0) {
    message("Clearing old temporary files...")
    unlink(temp_files, recursive = TRUE, force = TRUE)
  }

  # Check required packages
  required_packages <- c("shiny", "sf", "terra", "leaflet", "leaflet.extras", "rgee")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("The following required packages are not installed: ",
         paste(missing_packages, collapse = ", "),
         ". Please install them using install.packages() and try again.")
  }
  # Load packages
  lapply(required_packages, library, character.only = TRUE)

  # Verify that FloodPulseR is loaded correctly
  if (!"FloodPulseR" %in% (.packages())) {
    stop("Failed to load the FloodPulseR package. Please reinstall the package using devtools::install().")
  }

  # Initialize Google Earth Engine with Service Account Key (SaK)
  sak_path <- "C:/Users/ADMIN/Documents/fifth-catcher-456205-e8-21eef9f80731.json"
  gee_initialized <- FALSE

  # Check if SaK file exists
  if (!file.exists(sak_path)) {
    stop("Service Account Key (SaK) file not found at: ", sak_path,
         "\nPlease ensure the SaK file is placed in the specified directory.")
  }

  # Copy the SaK to rgee's configuration directory
  tryCatch({
    ee_utils_sak_copy(sakfile = sak_path)
    message("SaK copied successfully for GEE authentication.")
  }, error = function(e) {
    stop("Failed to copy SaK for GEE authentication: ", e$message)
  })

  # Initialize GEE using the SaK
  tryCatch({
    ee_Initialize(ee$Credentials$ServiceAccountCredentials(sak_path), drive = TRUE, gcs = TRUE)
    gee_initialized <- TRUE
    message("Google Earth Engine initialized successfully using Service Account Key.")
  }, error = function(e) {
    stop("Failed to initialize Google Earth Engine with Service Account Key: ", e$message,
         "\nPlease ensure the SaK is valid and has the necessary permissions.")
  })

  # Define UI
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body {
          font-family: Arial, sans-serif;
          background-color: #f5f5f5;
        }
        .sidebar { background-color: #ffffff; border-right: 1px solid #ddd; padding: 15px; }
        .main-panel { padding: 20px; }
        h4 { color: #333; margin-bottom: 10px; }
        .leaflet-container { border: 1px solid #ddd; border-radius: 5px; }
        .well { background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px; }
        .btn { background-color: #007bff; color: white; border: none; border-radius: 5px; }
        .btn:hover { background-color: #0056b3; }
      "))
    ),
    titlePanel("FloodPulseR: Sentinel-1 Flood Tool"),
    sidebarLayout(
      sidebarPanel(
        class = "sidebar",
        width = 3,
        h4("Inputs"),
        fileInput("aoi_zip", "Upload Shapefile as ZIP (include .shp, .shx, .dbf, .prj)",
                  accept = c(".zip"),
                  multiple = FALSE),
        dateInput("start_date", "Start Date", value = "2024-01-01", format = "yyyy-mm-dd"),
        dateInput("end_date", "End Date", value = "2024-01-02", format = "yyyy-mm-dd"),
        checkboxInput("use_drawn_area", "Use manually drawn area instead of shapefile", FALSE),
        conditionalPanel(
          condition = "input.use_drawn_area == true",
          actionButton("clear_draw", "Clear Drawing")
        ),
        actionButton("fetch", "Fetch and Process")
      ),
      mainPanel(
        class = "main-panel",
        h4("Area of Interest"),
        leafletOutput("map", height = "500px", width = "100%"),
        h4("Status"),
        verbatimTextOutput("status"),
        h4("Debug Info"),
        verbatimTextOutput("debug_info")
      )
    )
  )

  # Placeholder for preview_floodpulse
  preview_floodpulse <- function(file_path) {
    return(list(title = basename(file_path)))
  }

  # Define Server
  server <- function(input, output, session) {
    # Initialize reactive values
    rv <- reactiveValues(
      drawn_shapes = NULL,
      aoi = NULL,
      shapefile_error = NULL
    )

    # Initialize map with Eastern Africa view
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addLayersControl(
          baseGroups = c("Satellite", "OpenStreetMap"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addDrawToolbar(
          targetGroup = "drawn_aoi",
          polylineOptions = FALSE,
          markerOptions = FALSE,
          circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions()
        ) %>%
        addLayersControl(
          overlayGroups = c("drawn_aoi", "uploaded_aoi"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(lng = 38.0, lat = 3.0, zoom = 5)
    })

    # Output for debugging information
    output$debug_info <- renderPrint({
      if (!is.null(input$aoi_zip)) {
        cat("Uploaded ZIP: ", input$aoi_zip$name, "\n")
      } else {
        cat("No ZIP uploaded yet\n")
      }
      if (!is.null(rv$drawn_shapes)) {
        cat("Drawn shapes available: Yes\n")
      }
      if (!is.null(rv$shapefile_error)) {
        cat("Shapefile error: ", rv$shapefile_error, "\n")
      }
    })

    # Fetch AOI (Area of Interest) from shapefile or drawn area
    get_active_aoi <- reactive({
      if (input$use_drawn_area && !is.null(rv$drawn_shapes)) {
        return(rv$drawn_shapes)
      } else if (!input$use_drawn_area && !is.null(rv$aoi)) {
        return(rv$aoi)
      } else {
        return(NULL)
      }
    })

    # Fetch and process Sentinel-1 image when user clicks "Fetch and Process"
    observeEvent(input$fetch, {
      tryCatch({
        # Get the active AOI (from shapefile or drawn shape)
        aoi <- get_active_aoi()
        req(aoi)

        # Convert AOI to an Earth Engine object
        aoi_ee <- sf_as_ee(aoi)

        # Fetch Sentinel-1 image collection
        s1_img <- ee$ImageCollection("COPERNICUS/S1_GRD")$
          filterBounds(aoi_ee)$
          filterDate(input$start_date, input$end_date)$
          filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VV"))$
          filter(ee$Filter$eq("orbitProperties_pass", "DESCENDING"))$
          median()$select("VV")

        # Get map tile ID from Earth Engine
        map_id <- s1_img$getMapId(ee$vizParams(
          min = -25,
          max = 0,
          palette = c("black", "white")
        ))

        # Render the map with the Sentinel-1 image
        output$map <- renderLeaflet({
          leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addTiles(
              urlTemplate = map_id$url,
              options = tileOptions(opacity = 0.7)
            ) %>%
            setView(lng = 38.0, lat = 3.0, zoom = 6)
        })

        # Update status message
        output$status <- renderPrint({
          cat("Fetched Sentinel-1 imagery from GEE for AOI and date range.")
        })
      }, error = function(e) {
        # Handle any errors
        output$status <- renderPrint({
          cat("Error during fetching and processing Sentinel-1 data:", e$message)
        })
      })
    })
  }

  # Run the Shiny app
  shinyApp(ui, server)
}
