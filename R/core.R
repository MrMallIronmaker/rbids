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
#' @export
bids <- function(root, readonly = TRUE) {
  .single_character_check(root, "`root`")
  .bool_check(readonly, "`readonly`")

  if (!dir.exists(root)) {
    if (readonly) {
      abort(paste0("Root directory `", root, "` does not exist."))
    } else {
      warning(paste0("Directory `", root, "` does not exist. Creating directory..."))
      dir.create(root, recursive = TRUE)
    }
  }

  root <- file_path_as_absolute(root)

  # onladfy support .tsv now
  pattern_tsv <- "\\.tsv$"
  all_tsv_files <- list.files(path = root, recursive = TRUE, full.names = TRUE,
                              pattern = pattern_tsv)
  file_names <- basename(all_tsv_files)

  pattern <- paste0("^sub-(?<subject>[[:alnum:]]+)",
                    "(?:_ses-(?<session>[[:alnum:]]+))?",
                    "_task-(?<task>[[:alnum:]]+)",
                    "_tracksys-(?<tracksys>[[:alnum:]]+)",
                    "(?:_acq-(?<acq>[[:alnum:]]+))?",
                    "(?:_run-(?<run>[0-9]+))?",
                    "_(?<datatype>[[:alnum:]]+)\\.tsv$")

  extracted_data <- stringr::str_match(file_names, pattern)

  if (any(is.na(extracted_data[, "subject"]))) {
    warning("Some files did not match the expected BIDS naming convention and will be ignored.")
    valid_idx <- !is.na(extracted_data[, "subject"])
    all_tsv_files <- all_tsv_files[valid_idx]
    extracted_data <- extracted_data[valid_idx, , drop = FALSE]
  }

  bids_index <- tibble::tibble(
    file_path = all_tsv_files,
    subject = extracted_data[, "subject"],
    session = extracted_data[, "session"],
    task = extracted_data[, "task"],
    tracksys = extracted_data[, "tracksys"],
    acq = extracted_data[, "acq"],
    run = extracted_data[, "run"],
    datatype = extracted_data[, "datatype"]
  )

  dplyr::mutate(
    bids_index,
    across(everything(), ~tidyr::replace_na(.x, NA_character_))
  )

  bids_dataset <- list(
    root = root,
    index = bids_index,
    readonly = readonly,
  )
  class(bids_dataset) <- "bids_dataset"

  bids_dataset
}


#' Print Method for BIDS Dataset
#'
#' @export
print.bids_dataset <- function(bids_dataset) {
  .bids_obj_check(bids_dataset)
  cat("BIDS Dataset Summary\n")
  cat("====================\n")
  cat(sprintf("%-20s %s\n", "Root:", bids_dataset$root))
  cat(sprintf("%-20s %d\n", "Data Files:", nrow(bids_dataset$index)))

  subject_count <- length(unique(bids_dataset$index$subject))
  subjects <- paste(
    paste(head(sort(unique(bids_dataset$index$subject)), 5), collapse = ", "),
    "..."
  )

  session_values <- bids_dataset$index$session
  session_values <- session_values[!is.na(session_values)]
  sessions <- if (length(session_values) > 0)
    paste(sort(unique(session_values)), collapse = ", ") else "None"

  task_values <- bids_dataset$index$task
  task_values <- task_values[!is.na(task_values)]
  tasks <- if (length(task_values) > 0)
    paste(sort(unique(task_values)), collapse = ", ") else "None"

  datatype_values <- bids_dataset$index$datatype
  datatype_values <- datatype_values[!is.na(datatype_values)]
  datatypes <- if (length(datatype_values) > 0)
    paste(sort(unique(datatype_values)), collapse = ", ") else "None"

  tracksys_values <- bids_dataset$index$tracksys
  tracksys_values <- tracksys_values[!is.na(tracksys_values)]
  tracksys <- if (length(tracksys_values) > 0)
    paste(sort(unique(tracksys_values)), collapse = ", ") else "None"

  acq_values <- bids_dataset$index$acq
  acq_values <- acq_values[!is.na(acq_values)]
  acq <- if (length(acq_values) > 0)
    paste(sort(unique(acq_values)), collapse = ", ") else "None"

  run_values <- bids_dataset$index$run
  run_values <- run_values[!is.na(run_values)]
  run <- if (length(run_values) > 0)
    paste(sort(unique(run_values)), collapse = ", ") else "None"

  cat(sprintf("%-20s %d\n", "Total Subjects:", subject_count))
  cat(sprintf("%-20s %s\n", "Subjects:", subjects))
  cat(sprintf("%-20s %s\n", "Sessions:", sessions))
  cat(sprintf("%-20s %s\n", "Tasks:", tasks))
  cat(sprintf("%-20s %s\n", "Datatypes:", datatypes))
  cat(sprintf("%-20s %s\n", "Tracksys:", tracksys))
  cat(sprintf("%-20s %s\n", "Acquisition:", acq))
  cat(sprintf("%-20s %s\n", "Run:", run))

  invisible(bids_dataset)
}
