#' Fetch Sentinel-1 GRD Data from AWS S3
#' @param aoi Path to shapefile (.shp) defining area of interest
#' @param start_date Start date (YYYYMMDD)
#' @param end_date End date (YYYYMMDD)
#' @return List of S3 paths to COG files
#' @export
fetch_floodpulse <- function(aoi, start_date, end_date) {
  library(aws.s3)
  library(sf)

  # Check if the file is a shapefile
  file_ext <- tolower(tools::file_ext(aoi))
  if (file_ext != "shp") {
    stop("AOI must be a shapefile (.shp)")
  }

  # Read the shapefile
  footprint <- st_read(aoi, quiet = TRUE)$geometry[[1]]
  wkt <- st_as_text(footprint)

  # Construct S3 prefix
  year <- substr(start_date, 1, 4)
  bucket <- "sentinel-s1-l1c"
  prefix <- paste0("GRD/", year, "/")

  # List objects from S3 using list_objects_v2
  objects <- list_objects_v2(bucket = bucket, prefix = prefix, request_payer = "requester")
  object_keys <- sapply(objects$Contents, function(x) x$Key)

  # Filter by date (simple for MVP)
  filtered <- grep(paste(start_date, end_date, sep = "|"), object_keys, value = TRUE)
  s3_paths <- paste0("s3://", bucket, "/", filtered)
  return(s3_paths[1: min(5, length(s3_paths))])  # Limit to 5 files
}

#' Download Sentinel-1 COG from AWS S3
#' @param s3_path S3 path from fetch_floodpulse
#' @param output_dir Local directory
#' @return Local file path
#' @export
download_floodpulse <- function(s3_path, output_dir = "downloads") {
  library(aws.s3)
  if (!dir.exists(output_dir)) dir.create(output_dir)
  local_path <- file.path(output_dir, basename(s3_path))
  # Parse bucket and key from s3_path
  bucket <- gsub("s3://([^/]+)/.*", "\\1", s3_path)
  key <- gsub("s3://[^/]+/(.*)", "\\1", s3_path)
  get_object(bucket = bucket, key = key, file = local_path, request_payer = "requester")
  return(local_path)
}
