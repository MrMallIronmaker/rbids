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
#' dataset <- bids(tempdir())
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
  all_files <- list.files(path = root, recursive = TRUE, full.names = TRUE,
                          pattern = "\\.tsv$")
  file_names <- basename(all_files)

  pattern <- paste0("^sub-(?<subject>[[:alnum:]]+)",
                    "(?:_ses-(?<session>[[:alnum:]]+))?",
                    "_task-(?<task>[[:alnum:]]+)",
                    "_tracksys-(?<tracksys>[[:alnum:]]+)",
                    "(?:_acq-(?<acq>[[:alnum:]]+))?",
                    "(?:_run-(?<run>[0-9]+))?",
                    "_(?<datatype>[[:alnum:]]+)\\.tsv$")

  extracted_data <- str_match(file_names, pattern)

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
#' @importFrom magrittr %>%
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
  subjects <- paste(
    paste(head(sort(unique(bd$index$subject)), 5), collapse = ", "),
    "..."
  )

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


#' BIDS Dataset Files Filter
#'
#' @importFrom dplyr filter
#'
#' @export
bids_datafiles_filter <- function(bids_dataset,
                                  subject = NULL,
                                  session = NULL,
                                  task = NULL,
                                  tracksys = NULL,
                                  acq = NULL,
                                  run = NULL,
                                  datatype = NULL) {
  .bids_obj_checker(bids_dataset)

  # Helper function to split parameter values
  split_param <- function(param) {
    if (is.null(param)) return(NULL)
    unique(trimws(strsplit(param, "&")[[1]]))
  }

  # Split all parameters
  subjects <- split_param(subject)
  sessions <- split_param(session)
  tasks <- split_param(task)
  tracksystems <- split_param(tracksys)
  acqs <- split_param(acq)
  runs <- split_param(run)
  datatypes <- split_param(datatype)

  bids_data <- bids_dataset$index

  # Apply filters using %in% operator for multiple values
  filtered_data <- bids_data %>%
    dplyr::filter(
      if (!is.null(subjects)) .data$subject %in% subjects else TRUE,
      if (!is.null(sessions)) .data$session %in% sessions else TRUE,
      if (!is.null(tasks)) .data$task %in% tasks else TRUE,
      if (!is.null(tracksystems)) .data$tracksys %in% tracksystems else TRUE,
      if (!is.null(acqs)) .data$acq %in% acqs else TRUE,
      if (!is.null(runs)) .data$run %in% runs else TRUE,
      if (!is.null(datatypes)) .data$datatype %in% datatypes else TRUE
    )

  if (nrow(filtered_data) == 0) {
    warning("No records match the filtering criteria.")
    return(character(0))
  }

  file_paths <- filtered_data %>%
    dplyr::pull(file_path)

  file_paths
}


#' @title Get BIDS data files
#'
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



