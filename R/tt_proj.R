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

tt_git_branch_new <- function(iso, prefix = "tt-") {
  
  # New and old branch names
  new_branch_name <- paste0(prefix, iso)
  current_branch <- gert::git_branch()
  
  # Compose message for prompt
  msg_glue <- paste(
    "Create and checkout new branch with name `{new_branch_name}`",
    "on current branch `{current_branch}`?"
  )
  
  msg <- glue::glue(msg_glue)
  
  # Prompt user for confirmation and create branch if TRUE
  if (ui_yeah(msg)) {
    gert::git_branch_create(new_branch_name)
    rlang::inform(glue::glue("{new_branch_name} created and checked out"))
  } else {
    rlang::inform("New branch abandoned.")
  }
  
  invisible()
}
