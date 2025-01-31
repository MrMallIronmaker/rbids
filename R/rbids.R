#' @title Initialize a BIDS Dataset
#'
#' @description The `bids` function scans a specified root directory for TSV files
#' following the BIDS (Brain Imaging Data Structure) motion data.
#' https://bids-specification.readthedocs.io/en/stable/modality-specific-files/motion.html
#'
#' @param root Single character, root of dataset.
#' @param readonly Logical, defaults to TRUE.
#'
#' @return An object of class `bids_dataset` containing:
#' root,
#' index: Each tsv file attributes. Include subject, session, task... follow spec,
#' readyonly
#'
#' @examples
#' # Initialize a BIDS dataset from the specified root directory
#' dataset <- bids("/path/to/bids/root")
#'
#' # Print the dataset summary
#' print(dataset)
#'
#' @importFrom rlang abort
#' @importFrom tools file_path_as_absolute
#' @importFrom stringr str_match
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across
#' @importFrom tidyr replace_na
#' @export
bids <- function(root, readonly = TRUE) {
  if (!is.character(root) || length(root) != 1) {
    abort("Root must be a single character string")
  }

  if (!is.logical(readonly) || length(readonly) != 1) {
    abort("Readonly must be a single logical value")
  }

  if (!dir.exists(root) && readonly) {
    abort(paste0("Root directory `", root, "` does not exist"))
  }

  root <- file_path_as_absolute(root)
  all_files <- list.files(path = root, recursive = TRUE, full.names = TRUE, pattern = "\\.tsv$")
  file_names <- basename(all_files)

  pattern <- "^sub-(?<subject>[[:alnum:]]+)" %>%
    paste0("(?:_ses-(?<session>[[:alnum:]]+))?",
           "_task-(?<task>[[:alnum:]]+)",
           "_tracksys-(?<tracksys>[[:alnum:]]+)",
           "(?:_acq-(?<acq>[[:alnum:]]+))?",
           "(?:_run-(?<run>[0-9]+))?",
           "_(?<datatype>[[:alnum:]]+)\\.tsv$")

  extracted_data <- str_match(file_names, pattern)

  if (any(is.na(extracted_data[, 1]))) {
    warning("Some filenames did not match the regex pattern and will be marked as NA")
  }

  bids_data <- tibble(
    file_path = all_files,                      # full path
    subject = extracted_data[, "subject"],      # sub-<label>
    session = extracted_data[, "session"],      # ses-<label> (optional)
    task = extracted_data[, "task"],            # task-<label>
    tracksys = extracted_data[, "tracksys"],    # tracksys-<label>
    acq = extracted_data[, "acq"],              # acq-<label> (optional)
    run = extracted_data[, "run"],              # run-<index> (optional)
    datatype = extracted_data[, "datatype"]     # datatype (e.g., motion)
  ) %>%
    mutate(across(everything(), ~replace_na(.x, NA_character_)))

  bids_dataset <- list(
    root = root,
    index = bids_data,
    readonly = readonly
  )
  class(bids_dataset) <- "bids_dataset"

  return(bids_dataset)
}


#' Print Method for BIDS Dataset
#'
#' @export
print.bids_dataset <- function(bd) {
  .bids_obj_checker(bd)
  cat("BIDS Dataset Summary\n")
  cat("====================\n")
  cat(sprintf("%-20s %s\n", "Root:", bd$root))
  cat(sprintf("%-20s %d\n", "Data Files:", nrow(bd$index)))

  # Extract unique counts and values
  subject_count <- length(unique(bd$index$subject))
  subjects <- paste(sort(unique(bd$index$subject)), collapse = ", ")

  session_values <- bd$index$session
  session_values <- session_values[!is.na(session_values)]
  sessions <- if (length(session_values) > 0)
    paste(sort(unique(session_values)), collapse = ", ") else "None"

  task_values <- bd$index$task
  task_values <- task_values[!is.na(task_values)]
  tasks <- if (length(task_values) > 0)
    paste(sort(unique(task_values)), collapse = ", ") else "None"

  datatype_values <- bd$index$datatype
  datatype_values <- datatype_values[!is.na(datatype_values)]
  datatypes <- if (length(datatype_values) > 0)
    paste(sort(unique(datatype_values)), collapse = ", ") else "None"

  cat(sprintf("%-20s %d\n", "Total Subjects:", subject_count))
  cat(sprintf("%-20s %s\n", "Subjects:", subjects))
  cat(sprintf("%-20s %s\n", "Sessions:", sessions))
  cat(sprintf("%-20s %s\n", "Tasks:", tasks))
  cat(sprintf("%-20s %s\n", "Datatypes:", datatypes))

  invisible(bd)
}

#' @title Check if it's a bids object
#'
#' @importFrom rlang abort
.bids_obj_checker <- function(bd) {
  if (!inherits(bd, "bids_dataset")) {
    abort("The object provided is not a 'bids_dataset'")
  }
}

#' @title Get BIDS data files
#'
#' @export
bids_all_datafiles <- function(bd) {
  .bids_obj_checker(bd)
  bd$index$file_path
}

#' @export
bids_motion_regex <- function() {
  # I am unsure if this should be exported normally...
  paste0(
    "^",
    subject_capture,
    path_sep,
    "ses-(?<session_id>[[:alnum:]]+)/",
    "motion/",
    subject_backref,
    "_",
    "ses-(?P=session_id)",
    "_task-(?<task_label>[[:alnum:]]+)_motion.tsv$"
  )
}

#' @export
bids_motion <- function(bd, full.names = TRUE) {
  bids_match_path(
    bd,
    bids_motion_regex(),
    full.name = full.names
  )
}

#' @export
bids_subject_data <- function(bd, suffix, full.names = TRUE) {
  # bad name, perhaps subject-level data e.g.?
  bids_match_path(
    bd,
    paste0(
      "^sub-(?<participant_id>[[:alnum:]]+)[\\\\\\/]sub-\\g1_",
      suffix,
      ".tsv$"
    ),
    full.name = full.names
  )
}

#' @export
bids_subjects <- function(bd, full.names = TRUE) {
  read_tsv(file.path(bd$root, "participants.tsv"))
}

#' @export
bids_sessions <- function(bd, full.names = TRUE) {
  bids_subject_data(bd, "sessions", full.names = full.names)
}

#' @export
bids_events <- function(bd, full.names = TRUE) {
  # TODO: this doesn't follow the convention - I'm not sure if it should though
  bids_match_path(
    bd,
    "ses-(?<session_id>[a-zA-Z0-9]+)_task-(?<task_label>[a-zA-Z0-9]+)_events.tsv",
    full.name = full.names
  )
}

# bids_table has a "file_path" column,
# ... becomes the arguments to read_tsv
#' @export
bids_read_tsvs <- function(bids_table, ...) {
  # select only the filenames, read all, and unnest
  bids_table %>%
    mutate(
      ...bids_readable = map(file_path, read_tsv, ...),
    ) %>%
    select(-file_path) %>%
    unnest(...bids_readable)
}

#' @keywords Internal
ensure_write_access <- function(bids_dataset) {
  if (is.null(bids_dataset$readonly) | bids_dataset$readonly != FALSE) {
    rlang::abort("Bids dataset argument is readonly. Refusing to write.")
  }
}

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



