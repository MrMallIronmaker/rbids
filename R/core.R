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
  .single_character_check(root, "`name`")
  .bool_check(readonly, "`readonly`")

  if (!dir.exists(root)) {
    if (readonly) {
      abort(paste0("Root directory `", root, "` does not exist."))
    }
    warning(paste0("You are writing to `"), root, "`")
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
print.bids_dataset <- function(bids_dataset) {
  .bids_obj_check(bids_dataset)
  cat("BIDS Dataset Summary\n")
  cat("====================\n")
  cat(sprintf("%-20s %s\n", "Root:", bids_dataset$root))
  cat(sprintf("%-20s %d\n", "Data Files:", nrow(bids_dataset$index)))

  # Extract unique counts and values
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

  cat(sprintf("%-20s %d\n", "Total Subjects:", subject_count))
  cat(sprintf("%-20s %s\n", "Subjects:", subjects))
  cat(sprintf("%-20s %s\n", "Sessions:", sessions))
  cat(sprintf("%-20s %s\n", "Tasks:", tasks))
  cat(sprintf("%-20s %s\n", "Datatypes:", datatypes))

  invisible(bids_dataset)
}


#' @title Filter BIDS Dataset Files
#' @description Filters a BIDS dataset based on specified subject, session, task,
#' acquisition, tracking system, run, datatype criteria, and participant IDs.
#' @param bids_dataset A BIDS dataset object.
#' @param subject Optional. Subject ID(s) to include or exclude.
#' @param session Optional. Session ID(s) to include or exclude.
#' @param task Optional. Task name(s) to include or exclude.
#' @param tracksys Optional. Tracking system(s) to include or exclude.
#' @param acq Optional. Acquisition type(s) to include or exclude.
#' @param run Optional. Run number(s) to include or exclude.
#' @param datatype Optional. Default is "motion" data. Data type(s) to include or exclude.
#' @param participant_ids Optional. Participant ID(s) to filter on.
#'        Accepts output from bids_participants_filter; note that these IDs include the
#'        "sub-" prefix.
#'
#' @return A character vector of filtered file paths. Returns an empty vector if no
#' matches found.
#'
#' @examples
#' bids_files <- bids_datafiles_filter(bids_dataset, subject = "1&2")
#' bids_files <- bids_datafiles_filter(bids_dataset, subject = "-1")
#' # Using participant_ids returned from bids_participants_filter:
#' p_ids <- bids_participants_filter(bids_dataset, Anomaly = "Yes")
#' bids_files <- bids_datafiles_filter(bids_dataset, participant_ids = p_ids)
#'
#' @export
bids_datafiles_filter <- function(bids_dataset,
                                  subject = NULL,
                                  session = NULL,
                                  task = NULL,
                                  tracksys = NULL,
                                  acq = NULL,
                                  run = NULL,
                                  datatype = "motion",
                                  participant_ids = NULL) {
  .bids_obj_check(bids_dataset)

  parse_param <- function(param) {
    if (is.null(param)) return(list(include = NULL, exclude = NULL))
    tokens <- unique(trimws(unlist(strsplit(param, "&"))))
    list(
      include = if (any(!startsWith(tokens, "-"))) tokens[!startsWith(tokens, "-")] else NULL,
      exclude = if (any(startsWith(tokens, "-"))) sub("^-", "", tokens[startsWith(tokens, "-")]) else NULL
    )
  }

  params <- list(
    subject = parse_param(subject),
    session = parse_param(session),
    task = parse_param(task),
    tracksys = parse_param(tracksys),
    acq = parse_param(acq),
    run = parse_param(run),
    datatype = parse_param(datatype)
  )

  bids_data <- bids_dataset$index

  apply_filter <- function(data, field, param) {
    if (!is.null(param$include)) {
      data <- data %>% dplyr::filter(.data[[field]] %in% param$include)
    }
    if (!is.null(param$exclude)) {
      data <- data %>% dplyr::filter(!(.data[[field]] %in% param$exclude))
    }
    data
  }

  for (field in names(params)) {
    bids_data <- apply_filter(bids_data, field, params[[field]])
  }

  if (!is.null(participant_ids)) {
    cleaned_ids <- gsub("^sub-", "", participant_ids)
    bids_data <- bids_data %>% dplyr::filter(subject %in% cleaned_ids)
  }

  if (nrow(bids_data) == 0) {
    warning("No records match the filtering criteria.")
    return(character(0))
  }

  bids_data %>% dplyr::pull(file_path)
}


#' @title Filter Participants via the Participants File
#'
#' @param bids_dataset A BIDS dataset object.
#' @param ... Named filtering criteria.
#'
#' @return A character vector of participant_id values that match the filtering criteria.
#'
#' @export
bids_participants_filter <- function(bids_dataset, ...) {
  .bids_obj_check(bids_dataset)
  participants <- bids_get_participants_data(bids_dataset)

  if (!"participant_id" %in% colnames(participants)) {
    abort("The participants file must contain a 'participant_id' column.")
  }

  conditions <- rlang::enquos(...)

  filtered <- participants
  if (length(conditions) > 0) {
    filtered <- dplyr::filter(filtered, !!!conditions)
  }

  if (nrow(filtered) == 0) {
    warning("No participants match the filtering criteria.")
    return(character(0))
  }

  filtered$participant_id
}