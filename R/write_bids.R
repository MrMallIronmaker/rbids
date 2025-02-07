#' @keywords Internal
#' @importFrom readr write_tsv read_tsv
write_tsv_at <- function(x, file, append = FALSE) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }
  if (append && file.exists(file)) {
    ttsv <- read_tsv(file, col_names = TRUE, n_max = 0, show_col_types = FALSE)
    if (!isTRUE(all.equal(colnames(ttsv), colnames(x)))) {
      rlang::abort(paste0("Could not append to file `", file, "` because column names are not equivalent."))
    }
    write_tsv(x, file, append = TRUE)
  } else {
    # either the file doesn't exist or we're not appending, so erase & include the header.
    write_tsv(x, file, append = FALSE)
  }
}

#' @export
bids_write_motion_file <- function(participant_id, session_id, task_id, data, bids_dataset, append = FALSE) {
  # ugh: I don't like the argument order here.
  # TODO: check if motion.json matches the file

  # TODO: ensure lengths too.

  ensure_write_access(bids_dataset)

  # dataset/sub-<label>/ses-<label>/motion/sub-<label>_ses-<label>_task-<label>_motion.tsv
  sub_label <- glue::glue("sub-{participant_id}")
  ses_label <- glue::glue("ses-{session_id}")
  destination <- glue::glue("{bids_dataset$root}/{sub_label}/{ses_label}/motion/{sub_label}_{ses_label}_task-{task_id}_motion.tsv")

  write_tsv_at(data, destination, append = append)
  "success"
}

#' @export
bids_write_motion_files <- function(df, bids_dataset, .progress = TRUE, append = FALSE) {

  # this assumes df has columns of
  # participant_id, session_id, and data

  required_names <- c("participant_id", "session_id", "task_id", "data")
  match_test <- required_names %in% colnames(df)

  if (!all(match_test)) {
    rlang::abort(paste0("Missing column names: ", paste0(required_names[!match_test], collapse = ", ")))
  }

  if (.progress) {
    pb <- progress::progress_bar$new(total = nrow(df))
    pb$tick(0)
  }

  for (i in 1:nrow(df)) {
    bids_write_motion_file(
      df$participant_id[[i]],
      df$session_id[[i]],
      df$task_id[[i]],
      df$data[[i]],
      bids_dataset,
      append = append
    )

    if (.progress) {
      pb$tick()
    }
  }

  invisible(df)
}

