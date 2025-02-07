#' @title Check if it's a bids object
#'
#' @importFrom rlang abort
#' @keywords Internal
.bids_obj_checker <- function(bd) {
  if (!inherits(bd, "bids_dataset")) {
    abort("The object provided is not a 'bids_dataset'")
  }
}


#' @keywords Internal
.ensure_write_access <- function(bids_dataset) {
  if (is.null(bids_dataset$readonly) | bids_dataset$readonly != FALSE) {
    rlang::abort("Bids dataset argument is readonly. Refusing to write.")
  }
}
