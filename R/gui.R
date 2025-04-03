#' Start FloodPulseR GUI
#' @export
start_floodpulse <- function() {
  library(shiny)
  library(sf)
  library(terra)

  ui <- fluidPage(
    titlePanel("FloodPulseR: Sentinel-1 Flood Tool"),
    sidebarLayout(
      # Left Sidebar Pane (Inputs)
      sidebarPanel(
        width = 3,  # Reduced to 3 columns (out of 12) to give more space to main panel
        h4("Inputs"),
        fileInput("aoi_file", "Choose Shapefile (.shp)", accept = c(".shp"), multiple = FALSE),
        dateInput("start_date", "Start Date", value = "2024-01-01", format = "yyyy-mm-dd"),
        dateInput("end_date", "End Date", value = "2024-01-02", format = "yyyy-mm-dd"),
        actionButton("fetch", "Fetch and Process")
      ),
      # Right Main Panel (AOI Preview and Status)
      mainPanel(
        h4("AOI Preview"),
        plotOutput("aoi_preview", height = "500px", width = "600px"),  # Increased size
        h4("Status"),
        verbatimTextOutput("status"),
        h4("Debug Info"),
        verbatimTextOutput("debug_info")  # Added for debugging
      )
    )
  )

  server <- function(input, output) {
    # Output for debugging information
    output$debug_info <- renderPrint({
      if (!is.null(input$aoi_file)) {
        cat("Selected shapefile: ", input$aoi_file$name, "\n", sep="")

        # Try to show found related files if possible
        if (!is.null(related_files())) {
          cat("Found related files:\n")
          for (file in related_files()) {
            cat("- ", basename(file), "\n", sep="")
          }
        }
      } else {
        cat("No shapefile selected yet")
      }
    })

    # Reactive function to find related shapefile files
    related_files <- reactive({
      req(input$aoi_file)

      # Get the full path to the .shp file
      shp_path <- input$aoi_file$datapath

      # Get the directory containing the .shp file
      if (file.exists(shp_path)) {
        user_dir <- dirname(shp_path)

        # If this is a temp directory (from Shiny), try to find the original directory
        if (grepl("Rtmp|temp|tmp", user_dir, ignore.case = TRUE)) {
          # Try to get the original file path
          orig_path <- input$aoi_file$name

          # Extract just the filename without path
          orig_filename <- basename(orig_path)

          # Try to find this file in common places
          possible_dirs <- c(
            getwd(),
            normalizePath("~/Documents", mustWork = FALSE),
            normalizePath("~/Downloads", mustWork = FALSE),
            normalizePath("~/Desktop", mustWork = FALSE)
          )

          # Extract the base name without extension for pattern matching
          base_name <- tools::file_path_sans_ext(orig_filename)

          # Find all related files in possible directories
          all_related <- c()
          for (dir in possible_dirs) {
            if (dir.exists(dir)) {
              # Look for files with the same base name but different extensions
              matching_files <- list.files(
                path = dir,
                pattern = paste0("^", base_name, "\\."),
                full.names = TRUE
              )
              all_related <- c(all_related, matching_files)
            }
          }

          if (length(all_related) > 0) {
            return(all_related)
          }
        }
      }

      # If we couldn't find original files, return NULL
      return(NULL)
    })

    # Reactive value to store AOI
    aoi_data <- reactive({
      req(input$aoi_file)

      # Get the full path to the .shp file
      shp_path <- input$aoi_file$datapath
      shp_name <- input$aoi_file$name

      # Create a temporary directory for all shapefile components
      temp_dir <- file.path(tempdir(), paste0("shapefile_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(temp_dir, showWarnings = FALSE)

      # Copy the .shp file to the temporary directory
      temp_shp_path <- file.path(temp_dir, shp_name)
      file.copy(shp_path, temp_shp_path)

      # Extract base name without extension for finding related files
      base_name <- tools::file_path_sans_ext(shp_name)

      # Try to find related files
      related <- related_files()
      if (!is.null(related) && length(related) > 0) {
        # Copy related files to the temporary directory
        for (file_path in related) {
          file_name <- basename(file_path)
          # Only copy if it's not the .shp file we already copied
          if (file_name != shp_name) {
            file.copy(file_path, file.path(temp_dir, file_name))
          }
        }
        message("Copied ", length(related), " related files to temp directory")
      } else {
        message("No related files found in source directory")
      }

      # List all files in the temporary directory
      dir_files <- list.files(temp_dir)
      message("Files in temporary directory: ", paste(dir_files, collapse=", "))

      # Check for essential shapefile components
      has_shx <- any(grepl("\\.shx$", dir_files, ignore.case = TRUE))
      has_dbf <- any(grepl("\\.dbf$", dir_files, ignore.case = TRUE))
      has_prj <- any(grepl("\\.prj$", dir_files, ignore.case = TRUE))

      if (!has_shx || !has_dbf) {
        # If components are missing, try to create them
        message("Missing essential shapefile components. Attempting to restore/create them.")
        Sys.setenv(SHAPE_RESTORE_SHX="YES")
      }

      # Try to read the shapefile
      tryCatch({
        message("Attempting to read shapefile: ", temp_shp_path)
        sf_object <- st_read(temp_shp_path, quiet = TRUE)
        return(sf_object)
      }, error = function(e) {
        message("Error reading shapefile with sf: ", e$message)

        # Try alternative method with terra
        tryCatch({
          message("Trying alternative method with terra package...")
          v <- terra::vect(temp_shp_path)
          sf_object <- sf::st_as_sf(v)
          return(sf_object)
        }, error = function(e2) {
          message("Both methods failed.")
          message("Terra error: ", e2$message)
          stop("Could not read shapefile. Please ensure all required files (.shp, .shx, .dbf) are in the same folder as your .shp file.")
        })
      })
    })

    # Render AOI preview
    output$aoi_preview <- renderPlot({
      tryCatch({
        req(aoi_data())
        plot(st_geometry(aoi_data()), main = "Selected AOI", col = "lightblue")
      }, error = function(e) {
        # Create an empty plot with error message
        plot(c(0, 1), c(0, 1), type = "n", axes = FALSE,
             xlab = "", ylab = "", main = "Error Loading Shapefile")
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      })
    })

    # Fetch and process
    observeEvent(input$fetch, {
      tryCatch({
        req(aoi_data())

        # Get path to the original uploaded .shp file
        shp_file <- input$aoi_file$datapath

        s3_paths <- fetch_floodpulse(
          shp_file,
          format(input$start_date, "%Y%m%d"),
          format(input$end_date, "%Y%m%d")
        )

        if (length(s3_paths) > 0) {
          file_path <- download_floodpulse(s3_paths[1])
          metadata <- preview_floodpulse(file_path)
          processed_file <- process_floodpulse(file_path)
          output$status <- renderPrint({
            cat("Downloaded:", metadata$title, "\nProcessed to:", processed_file)
          })
        } else {
          output$status <- renderPrint("No data found or invalid shapefile")
        }
      }, error = function(e) {
        output$status <- renderPrint({
          cat("Error during processing:", e$message)
        })
      })
    })
  }

  shinyApp(ui, server)
}
