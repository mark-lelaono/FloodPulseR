#' Start FloodPulseR GUI
#' @export
start_floodpulse <- function() {
  # Check and install required packages
  required_packages <- c("shiny", "sf", "terra", "leaflet", "leaflet.extras", "rgee")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }

  # Initialize Google Earth Engine
  tryCatch({
    ee_Initialize()  # Initialize GEE
  }, error = function(e) {
    stop("Failed to initialize Google Earth Engine: ", e$message,
         "\nPlease ensure you have authenticated with GEE and have the Earth Engine API set up.")
  })

  # Define UI
  ui <- fluidPage(
    # Add custom CSS for styling
    tags$head(
      tags$style(HTML("
        body {
          font-family: Arial, sans-serif;
          background-color: #f5f5f5;
        }
        .sidebar {
          background-color: #ffffff;
          border-right: 1px solid #ddd;
          padding: 15px;
        }
        .main-panel {
          padding: 20px;
        }
        h4 {
          color: #333;
          margin-bottom: 10px;
        }
        .leaflet-container {
          border: 1px solid #ddd;
          border-radius: 5px;
        }
        .well {
          background-color: #f9f9f9;
          border: 1px solid #ddd;
          border-radius: 5px;
        }
        .btn {
          background-color: #007bff;
          color: white;
          border: none;
          border-radius: 5px;
        }
        .btn:hover {
          background-color: #0056b3;
        }
      "))
    ),
    titlePanel("FloodPulseR: Sentinel-1 Flood Tool"),
    sidebarLayout(
      # Left Sidebar Pane (Inputs)
      sidebarPanel(
        class = "sidebar",
        width = 3,
        h4("Inputs"),
        fileInput("aoi_file", "Choose Shapefile (.shp)", accept = c(".shp"), multiple = FALSE),
        dateInput("start_date", "Start Date", value = "2024-01-01", format = "yyyy-mm-dd"),
        dateInput("end_date", "End Date", value = "2024-01-02", format = "yyyy-mm-dd"),
        checkboxInput("use_drawn_area", "Use manually drawn area instead of shapefile", FALSE),
        conditionalPanel(
          condition = "input.use_drawn_area == true",
          actionButton("clear_draw", "Clear Drawing")
        ),
        actionButton("fetch", "Fetch and Process")
      ),
      # Right Main Panel (AOI Preview and Status)
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
      aoi = NULL
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
        setView(lng = 38.0, lat = 3.0, zoom = 5) # Zoom to Eastern Africa
    })

    # Output for debugging information
    output$debug_info <- renderPrint({
      if (!is.null(input$aoi_file)) {
        cat("Selected shapefile: ", input$aoi_file$name, "\n", sep="")
        if (!is.null(related_files())) {
          cat("Found related files:\n")
          for (file in related_files()) {
            cat("- ", basename(file), "\n", sep="")
          }
        }
      } else {
        cat("No shapefile selected yet")
      }
      if (!is.null(rv$drawn_shapes)) {
        cat("\nDrawn shapes available: Yes")
      }
    })

    # Copy shapefile components
    copy_shapefile_components <- function(src_file, dst_dir) {
      base_name <- tools::file_path_sans_ext(basename(src_file))
      src_dir <- dirname(src_file)
      exts <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".sbn", ".sbx", ".xml", ".fbn", ".fbx", ".ain", ".aih", ".ixs", ".mxs")
      for (ext in exts) {
        comp_file <- file.path(src_dir, paste0(base_name, ext))
        if (file.exists(comp_file)) {
          file.copy(comp_file, file.path(dst_dir, paste0(base_name, ext)), overwrite = TRUE)
        }
      }
      return(file.path(dst_dir, paste0(base_name, ".shp")))
    }

    # Reactive function to find related shapefile files
    related_files <- reactive({
      req(input$aoi_file)
      basename <- tools::file_path_sans_ext(input$aoi_file$name)
      tempdir <- dirname(input$aoi_file$datapath)
      all_files <- list.files(tempdir, full.names = TRUE)
      related <- grep(basename, all_files, value = TRUE)
      if (length(related) > 0) {
        return(related)
      } else {
        return(NULL)
      }
    })

    # Reactive value to store AOI from shapefile
    aoi_data <- reactive({
      req(input$aoi_file)
      req(!input$use_drawn_area)
      shp_path <- input$aoi_file$datapath
      shp_name <- input$aoi_file$name
      base_name <- tools::file_path_sans_ext(shp_name)
      temp_dir <- file.path(tempdir(), paste0("shapefile_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(temp_dir, showWarnings = FALSE)
      temp_shp_path <- file.path(temp_dir, shp_name)
      file.copy(shp_path, temp_shp_path)
      src_dir <- dirname(shp_path)
      exts <- c(".shx", ".dbf", ".prj", ".cpg")
      for (ext in exts) {
        src_file <- file.path(src_dir, paste0(tools::file_path_sans_ext(basename(shp_path)), ext))
        if (file.exists(src_file)) {
          dst_file <- file.path(temp_dir, paste0(base_name, ext))
          file.copy(src_file, dst_file)
        }
      }
      dir_files <- list.files(temp_dir)
      message("Files in temporary directory: ", paste(dir_files, collapse=", "))
      has_shx <- any(grepl("\\.shx$", dir_files, ignore.case = TRUE))
      has_dbf <- any(grepl("\\.dbf$", dir_files, ignore.case = TRUE))
      has_prj <- any(grepl("\\.prj$", dir_files, ignore.case = TRUE))
      if (!has_shx || !has_dbf) {
        message("Missing essential shapefile components.")
      }
      if (!has_prj) {
        message("No .prj file found. Will attempt to read with a default CRS.")
      }
      tryCatch({
        message("Attempting to read shapefile: ", temp_shp_path)
        sf_opts <- list(quiet = TRUE)
        if (!has_prj) {
          sf_opts$wkt <- sf::st_crs(4326)$wkt
        }
        sf_object <- do.call(st_read, c(list(dsn = temp_shp_path), sf_opts))
        if (is.na(st_crs(sf_object)) || is.null(st_crs(sf_object))) {
          message("No valid CRS found. Assuming WGS84 (EPSG:4326).")
          st_crs(sf_object) <- 4326
        }
        if (!identical(st_crs(sf_object)$epsg, 4326)) {
          sf_object <- st_transform(sf_object, 4326)
        }
        return(sf_object)
      }, error = function(e) {
        message("Error reading shapefile with sf: ", e$message)
        tryCatch({
          message("Trying alternative method with terra package...")
          if (!has_prj) {
            v <- terra::vect(temp_shp_path)
            terra::crs(v) <- "EPSG:4326"
          } else {
            v <- terra::vect(temp_shp_path)
          }
          sf_object <- sf::st_as_sf(v)
          if (is.na(st_crs(sf_object)) || is.null(st_crs(sf_object))) {
            st_crs(sf_object) <- 4326
          }
          if (!identical(st_crs(sf_object)$epsg, 4326)) {
            sf_object <- st_transform(sf_object, 4326)
          }
          return(sf_object)
        }, error = function(e2) {
          message("Both methods failed.")
          message("Terra error: ", e2$message)
          stop("Could not read shapefile. Please ensure all required files (.shp, .shx, .dbf) are in the same folder.")
        })
      })
    })

    # Observer for clearing drawn shapes
    observeEvent(input$clear_draw, {
      leafletProxy("map") %>%
        clearGroup("drawn_aoi")
      rv$drawn_shapes <- NULL
    })

    # Observer for drawn shapes
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      drawn_sf <- tryCatch({
        if (feature$geometry$type == "Polygon" || feature$geometry$type == "Rectangle") {
          coords <- feature$geometry$coordinates[[1]]
          coords_matrix <- do.call(rbind, coords)
          polygon <- st_polygon(list(coords_matrix))
          sf_obj <- st_sf(geometry = st_sfc(polygon, crs = 4326))
          sf_obj
        } else {
          NULL
        }
      }, error = function(e) {
        message("Error converting drawn shape to SF: ", e$message)
        NULL
      })
      if (!is.null(drawn_sf)) {
        rv$drawn_shapes <- drawn_sf
      }
    })

    # Observer for deleted shapes
    observeEvent(input$map_draw_deleted_features, {
      deleted_ids <- sapply(input$map_draw_deleted_features$features, function(x) x$id)
      if (length(deleted_ids) > 0) {
        rv$drawn_shapes <- NULL
      }
    })

    # Update map when shapefile is loaded
    observe({
      if (input$use_drawn_area) return()
      tryCatch({
        aoi <- aoi_data()
        req(aoi)
        rv$aoi <- aoi
        bbox <- st_bbox(aoi)
        leafletProxy("map") %>%
          clearGroup("uploaded_aoi") %>%
          addPolygons(
            data = aoi,
            fillOpacity = 0,  # Transparent fill
            weight = 1,       # Thin border
            opacity = 1,
            color = "black",  # Black border
            group = "uploaded_aoi"
          ) %>%
          fitBounds(
            bbox[["xmin"]],
            bbox[["ymin"]],
            bbox[["xmax"]],
            bbox[["ymax"]]
          )
      }, error = function(e) {
        message("Error updating map with shapefile: ", e$message)
      })
    })

    # Get the final AOI to use
    get_active_aoi <- reactive({
      if (input$use_drawn_area && !is.null(rv$drawn_shapes)) {
        return(rv$drawn_shapes)
      } else if (!input$use_drawn_area && !is.null(rv$aoi)) {
        return(rv$aoi)
      } else {
        return(NULL)
      }
    })

    # Fetch and process
    observeEvent(input$fetch, {
      tryCatch({
        aoi <- get_active_aoi()
        req(aoi)
        if (input$use_drawn_area && !is.null(rv$drawn_shapes)) {
          temp_shp_path <- file.path(tempdir(), "drawn_aoi.shp")
          st_write(rv$drawn_shapes, temp_shp_path, quiet = TRUE, delete_layer = TRUE)
          shp_file <- temp_shp_path
        } else {
          shp_file <- input$aoi_file$datapath
        }

        # Fetch data from GEE
        asset_id <- tryCatch({
          fetch_floodpulse(
            shp_file,
            format(input$start_date, "%Y%m%d"),
            format(input$end_date, "%Y%m%d")
          )
        }, error = function(e) {
          output$status <- renderPrint({
            cat("Error in fetch_floodpulse: ", e$message)
          })
          return(NULL)
        })

        if (!is.null(asset_id)) {
          file_path <- tryCatch({
            download_floodpulse(asset_id)
          }, error = function(e) {
            output$status <- renderPrint({
              cat("Error in download_floodpulse: ", e$message)
            })
            return(NULL)
          })
          if (!is.null(file_path)) {
            metadata <- tryCatch({
              preview_floodpulse(file_path)
            }, error = function(e) {
              output$status <- renderPrint({
                cat("Error in preview_floodpulse: ", e$message)
              })
              return(NULL)
            })
            processed_file <- tryCatch({
              process_floodpulse(file_path, threshold = -15)  # Use default threshold
            }, error = function(e) {
              output$status <- renderPrint({
                cat("Error during processing: ", e$message)
              })
              return(NULL)
            })
            if (!is.null(metadata) && !is.null(processed_file)) {
              output$status <- renderPrint({
                cat("Downloaded:", metadata$title, "\nProcessed to:", processed_file)
              })
            }
          }
        } else {
          output$status <- renderPrint({
            cat("No data found or invalid AOI")
          })
        }
      }, error = function(e) {
        output$status <- renderPrint({
          cat("Error during processing:", e$message)
        })
      })
    })
  }

  # Run the Shiny app
  shinyApp(ui, server)
}
