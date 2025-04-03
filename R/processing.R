#' Process Sentinel-1 COG for Flood Analysis
#' @param cog_path Path to downloaded COG file
#' @param output_path Output GeoTIFF path
#' @return Processed file path
#' @export
process_floodpulse <- function(cog_path, output_path = "output.tif") {
  library(terra)
  rast <- rast(cog_path)
  # Simple flood detection: low backscatter threshold
  flood_mask <- rast < -15  # Adjust this value later
  writeRaster(flood_mask, output_path, overwrite = TRUE)
  return(output_path)
}
