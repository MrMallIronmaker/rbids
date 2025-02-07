#' @export
bids_all_datafiles <- function(bids_dataset) {
  .bids_obj_checker(bids_dataset)
  bids_dataset$index$file_path
}

#' @export
bids_motion_datafiles <- function(bids_dataset) {
  bids_datafiles_filter(bids_dataset, datatype = "motion")
}

#' @export
bids_get_motion_by_subject <- function(bids_dataset, subject) {
  bids_datafiles_filter(bids_dataset, subject = subject, datatype = "motion")
}

#' @export
bids_participants <- function(bids_dataset) {
  .bids_obj_checker(bids_dataset)
  participants_data <- read_tsv(file.path(bids_dataset$root, "participants.tsv"),
                                show_col_types = FALSE)
  glimpse(participants_data)
  invisible(participants_data)
}
