#' Functions for TidyTuesday repo management
#' 
#' @param iso string; the TidyTuesday date in ISO 8601 format
#' 
#' 
#' @author Tom Fowler
#' 
#' @name ttproj
#' 
NULL


#' @describeIn ttproj Create new TidyTuesday directory
tt_dir_create <- function(iso, add.readme = FALSE, readme.ext = "md") {
  
  year <- format.Date(iso, "%Y")
  
  if (!dir.exists(here::here(year))) {
    dir.create(here::here(year))
  } else {
    dir.create(here::here(year, iso))
  }
  
  # Check that new directory 
  if (!dir.exists(here::here(year, iso))) {
    usethis::ui_stop("Final check did not find created directory")
  }
  
  # Add readme if TRUE
  if (add.readme == TRUE) {
    file.create(here::here(year, iso, paste0("README", ".", readme.ext)))
  }
}