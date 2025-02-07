#' @title Check if it's a bids object
#'
#' @importFrom rlang abort
#' @keywords Internal
.bids_obj_check <- function(bids_dataset) {
  if (!inherits(bids_dataset, "bids_dataset")) {
    abort("The object provided is not a 'bids_dataset'")
  }
}

#' @keywords Internal
.bool_check <- function(value, name) {
  if (!is.character(value) || length(value) != 1) {
    abort(paste(name, "must be a single character string"))
  }
}

#' @keywords Internal
.ensure_write_access <- function(bids_dataset) {
  if (is.null(bids_dataset$readonly) | bids_dataset$readonly != FALSE) {
    rlang::abort("Bids dataset argument is readonly. Refusing to write.")
  }
}

#' @keywords Internal
.is_participants_exists <- function(bids_dataset) {
  file_path <- file.path(bids_dataset$root, "participants.tsv")
  if (!file.exists(file_path)) {
    abort("participants.tsv does not exist.")
  } else {
    file_path
  }
}