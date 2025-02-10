#' @keywords Internal
.single_character_check <- function(value, name) {
  if (!is.character(value) || length(value) != 1) {
    abort(paste(name, "must be a single character string"))
  }
}

#' @keywords Internal
.bool_check <- function(value, name) {
  if (!is.logical(value) || length(value) != 1) {
    abort(paste(name, "must be a bool"))
  }
}

#' @keywords Internal
.is_participants_exists <- function(bids) {
  file_path <- file.path(bids$root, "participants.tsv")
  if (!file.exists(file_path)) {
    abort("`participants.tsv` does not exist.")
  } else {
    file_path
  }
}