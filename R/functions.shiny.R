#' @export
getDatasetTable <- function(dataset_list = list(), tz) {
  if (length(dataset_list) == 0) {
    df <- data.frame(matrix(nrow = 0, ncol = 4),
                     stringsAsFactors = F,
                     check.names = F)
    colnames(df) <- c("Name", "Created", "Last update", "Actions")
    return(df)
  }
  row_ids <- sapply(dataset_list, '[[', "id")
  action_buttons <-
    list(c("Edit", "edit"),
         c("Delete", "delete"),
         c("Download", "download"))
  action_buttons_code <- lapply(action_buttons, function(x) {
    shinyInput(
      shiny::actionButton,
      row_ids,
      sprintf('%s_', x[2]),
      label = x[1],
      onclick = 'Shiny.onInputChange(\"action_button\", this.id+"_"+Date())'
    )
  })
  action_buttons_code_summary <-
    sapply(1:length(row_ids), function(x) {
      paste(lapply(action_buttons_code, "[", x), collapse = " ")
    })
  data.frame(
    Name = sapply(dataset_list, '[[', "name"),
    Created = convertTimestampToPOSIXct(sapply(dataset_list, '[[', "created"), tz =
                                          tz),
    "Last update" = convertTimestampToPOSIXct(sapply(dataset_list, '[[', "last_updated"), tz =
                                                tz),
    Actions = action_buttons_code_summary,
    stringsAsFactors = FALSE,
    row.names = row_ids,
    check.names = F
  )
}

#' @export
getReportpackageTable <- function(reportpackage_list = list(), tz) {
  if (length(reportpackage_list) == 0) {
    df <- data.frame(matrix(nrow = 0, ncol = 5),
                     stringsAsFactors = F,
                     check.names = F)
    colnames(df) <-
      c("Name", "Version", "Created", "Last update", "Actions")
    return(df)
  }
  row_ids <- sapply(reportpackage_list, '[[', "id")
  action_buttons <-
    list(c("Edit", "edit"),
         c("Delete", "delete"),
         c("Download", "download"))
  action_buttons_code <- lapply(action_buttons, function(x) {
    shinyInput(
      shiny::actionButton,
      row_ids,
      sprintf('%s_', x[2]),
      label = x[1],
      onclick = 'Shiny.onInputChange(\"action_button_reportpackage\", this.id+"_"+Date())'
    )
  })
  action_buttons_code_summary <-
    sapply(1:length(row_ids), function(x) {
      paste(lapply(action_buttons_code, "[", x), collapse = " ")
    })
  data.frame(
    Name = sapply(reportpackage_list, '[[', "name"),
    Version = sapply(reportpackage_list, '[[', "version"),
    Created = convertTimestampToPOSIXct(sapply(reportpackage_list, '[[', "created"), tz =
                                          tz),
    "Last update" = convertTimestampToPOSIXct(sapply(
      reportpackage_list, '[[', "last_updated"
    ), tz = tz),
    Actions = action_buttons_code_summary,
    stringsAsFactors = FALSE,
    row.names = row_ids,
    check.names = F
  )
}

#' @export
getReportTable <- function(report_list = list(), tz, session_user_data) {
  columns <-
    c(
      "Dataset",
      "Report package",
      "Created",
      "Started",
      "Finished",
      "Status",
      "Reports created",
      "Actions"
    )
  if (length(report_list) == 0) {
    df <- data.frame(
      matrix(nrow = 0, ncol = length(columns)),
      stringsAsFactors = F,
      check.names = F
    )
    colnames(df) <- columns
    return(df)
  }
  row_ids <- sapply(report_list, '[[', "id")
  action_buttons <- list(
    c("Generate reports", "run"),
    c("Download reports", "download"),
    c("Copy", "copy"),
    c("Delete", "delete"),
    c("Logs", "logs")
  )
  action_buttons_code <- lapply(action_buttons, function(x) {
    shinyInput(
      shiny::actionButton,
      row_ids,
      sprintf('%s_', x[2]),
      label = x[1],
      onclick = 'Shiny.onInputChange(\"action_button_report\", this.id+"_"+Date())'
    )
  })
  action_buttons_code_summary <-
    sapply(1:length(row_ids), function(x) {
      paste(lapply(action_buttons_code, "[", x), collapse = " ")
    })
  data.frame(
    "Dataset" = sapply(report_list, function(x) {
      dataset <-
        session_user_data$dataset_adapter$getDatasets(md5sum = x$dataset_md5sum)
      if (length(dataset) == 1) {
        sprintf("%s (%s rows, %s columns)",
                dataset[[1]]$name,
                dataset[[1]]$rows,
                dataset[[1]]$cols)
      } else {
        "Dataset not found."
      }
    }),
    "Report package" = sapply(report_list, function(x) {
      reportpackage <-
        session_user_data$reportpackage_adapter$getReportpackages(md5sum = x$reportpackage_md5sum)
      if (length(reportpackage) == 1) {
        sprintf("%s (version %s)",
                reportpackage[[1]]$name,
                reportpackage[[1]]$version)
      } else {
        "Report package not found."
      }
    }),
    "Created" = convertTimestampToPOSIXct(sapply(report_list, '[[', "created"), tz =
                                            tz),
    "Started" = convertTimestampToPOSIXct(nullToNA(unname(
      sapply(report_list, '[[', "lastrun_started")
    )), tz = tz),
    "Finished" = convertTimestampToPOSIXct(nullToNA(unname(
      sapply(report_list, '[[', "lastrun_finished")
    )), tz = tz),
    "Status" = nullToNA(unname(
      sapply(report_list, '[[', "lastrun_status")
    )),
    "Documents created" = nullToNA(unname(
      sapply(report_list, '[[', "lastrun_filesgenerated")
    )),
    "Actions" = action_buttons_code_summary,
    stringsAsFactors = FALSE,
    row.names = row_ids,
    check.names = F
  )
}

#' @export
convertTimestampToPOSIXct <- function(timestamp_value, tz) {
  format(as.POSIXct(floor(as.numeric(timestamp_value)), origin = "1970-01-01", tz =
                      tz),
         "%Y-%m-%d %H:%M")
}

#' @export
nullToNA <- function(x) {
  sapply(x, function(y) {
    if (is.null(y)) {
      NA
    } else {
      y
    }
  })
}

#' @export
shinyInput <- function(FUN, row_id, id, ...) {
  # Used for creating object-specific buttons
  inputs <- character(length(row_id))
  for (i in row_id) {
    inputs[i == row_id] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

#' @export
UIcomponent_editDataset <- function(dataset) {
  dataset_name <- ifelse(is.null(dataset), "", dataset$name)
  dataset_filename <- ifelse(is.null(dataset), "", dataset$filename)
  dataset_id <- ifelse(is.null(dataset), "", dataset$id)
  tagList(
    textInput(
      inputId = "dataset_update_name",
      label = "Dataset name: ",
      value = dataset_name
    ),
    # need to encode?
    htmlOutput(outputId = "dataset_update_name_eval"),
    fileInput(
      inputId = "dataset_update_file",
      "Choose RDS File",
      multiple = FALSE,
      accept = c(".rds"),
      placeholder = dataset_filename
    ),
    # improve
    htmlOutput(outputId = "dataset_update_file_eval"),
    textInput(
      inputId = "dataset_update_id",
      label = NULL,
      value = dataset_id
    ),
    actionButton(inputId = "button_dataset_save",
                 label = "Save"),
    actionButton(inputId = "button_dataset_cancel",
                 label = "Cancel")
  )
}

#' @export
UIcomponent_deleteDataset <- function(dataset) {
  dataset_name <- dataset$name
  dataset_id <- dataset$id
  list(
    p(
      sprintf(
        "Are you sure that you would like to delete the dataset \"%s\"?",
        dataset_name
      )
    ),
    textInput(
      inputId = "dataset_delete_id",
      label = NULL,
      value = dataset_id
    ),
    actionButton(inputId = "button_dataset_delete_do",
                 label = "Delete"),
    actionButton(inputId = "button_dataset_delete_cancel",
                 label = "Cancel")
  )
}

#' @export
UIcomponent_editReportpackage <- function(reportpackage) {
  reportpackage_name <-
    ifelse(is.null(reportpackage), "", reportpackage$name)
  reportpackage_filename <-
    ifelse(is.null(reportpackage), "", reportpackage$filename)
  reportpackage_id <-
    ifelse(is.null(reportpackage), "", reportpackage$id)
  list(
    fileInput(
      inputId = "reportpackage_update_file",
      "Choose R Package File (.tar.gz)",
      multiple = FALSE,
      accept = c(".tar.gz"),
      placeholder = reportpackage_filename
    ),
    # improve
    htmlOutput(outputId = "reportpackage_update_eval"),
    textInput(
      inputId = "reportpackage_update_id",
      label = NULL,
      value = reportpackage_id
    ),
    actionButton(inputId = "button_reportpackage_save",
                 label = "Save"),
    actionButton(inputId = "button_reportpackage_cancel",
                 label = "Cancel")
  )
}

#' @export
UIcomponent_deleteReportpackage <- function(reportpackage) {
  reportpackage_name <- reportpackage$name
  reportpackage_id <- reportpackage$id
  list(
    p(
      sprintf(
        "Are you sure that you would like to delete the report package \"%s\"?",
        reportpackage_name
      )
    ),
    textInput(
      inputId = "reportpackage_delete_id",
      label = NULL,
      value = reportpackage_id
    ),
    actionButton(inputId = "button_reportpackage_delete_do",
                 label = "Delete"),
    actionButton(inputId = "button_reportpackage_delete_cancel",
                 label = "Cancel")
  )
}

#' @export
ifTrue <- function(x, when_true, when_false) {
  if (x) {
    when_true
  } else {
    when_false
  }
}

#' @export
UIcomponent_editReport <- function(report,
                                   session_user_data) {
  selections <- list(
    "dataset" = ifTrue(R6::is.R6(report),
                       report$dataset_md5sum,
                       NULL),
    "reportpackage" = ifTrue(R6::is.R6(report),
                             report$reportpackage_md5sum,
                             NULL),
    "config" = list(
      "from_line" = ifTrue(R6::is.R6(report),
                           report$config$from_line,
                           NULL),
      "to_line" = ifTrue(R6::is.R6(report),
                         report$config$to_line,
                         NULL)
    )
  )
  dataset_choices <-
    c("", sapply(session_user_data$datasets, "[[", "file_md5sum"))
  names(dataset_choices) <-
    c("Please choose ... ",
      sprintf(
        "%s (%s rows, %s columns)",
        sapply(session_user_data$datasets, "[[", "name"),
        sapply(session_user_data$datasets, "[[", "rows"),
        sapply(session_user_data$datasets, "[[", "cols")
      ))
  reportpackage_choices <-
    c("",
      sapply(session_user_data$reportpackages, "[[", "file_md5sum"))
  names(reportpackage_choices) <-
    c("Please choose ... ",
      sprintf(
        "%s (version %s)",
        sapply(session_user_data$reportpackages, "[[", "name"),
        sapply(session_user_data$reportpackages, "[[", "version")
      ))
  list(
    selectizeInput(
      inputId = "report_dataset_md5sum",
      label = "Select dataset:",
      choices = dataset_choices,
      selected = selections$dataset,
      multiple = FALSE,
      options = NULL
    ),
    selectizeInput(
      inputId = "report_reportpackage_md5sum",
      label = "Select report package:",
      choices = reportpackage_choices,
      selected = selections$reportpackage,
      multiple = FALSE,
      options = NULL
    ),
    h4("Configuration paramters"),
    numericInput(
      inputId = "report_config_from_line",
      label = "Start at line: ",
      value = ifTrue(
        is.null(selections$config$from_line),
        1L,
        selections$config$from_line
      ),
      min = 1L,
      step = 1L
    ),
    numericInput(
      inputId = "report_config_to_line",
      label = "Stop after line: ",
      value = ifTrue(
        is.null(selections$config$to_line),
        1L,
        selections$config$to_line
      ),
      min = 1L,
      step = 1L
    ),
    htmlOutput(outputId = "report_create_eval"),
    actionButton(inputId = "button_report_save",
                 label = "Save"),
    actionButton(inputId = "button_report_cancel",
                 label = "Cancel")
  )
}

#' @export
UIcomponent_deleteReport <- function(report) {
  report_id <- report$id
  list(
    p(
      "Are you sure that you would like to delete this report and all files generated so far?"
    ),
    textInput(
      inputId = "report_delete_id",
      label = NULL,
      value = report_id
    ),
    actionButton(inputId = "button_report_delete_do",
                 label = "Delete"),
    actionButton(inputId = "button_report_delete_cancel",
                 label = "Cancel")
  )
}

#' @export
readConfigfile <- function(collapse = "", directory) {
  paste0(x = scan(
    file = do.call(file.path, as.list(c(
      getwd(), directory, "config.json"
    ))),
    what = "character",
    sep = "\n"
  ),
  collapse = collapse)
}

#' @export
writeConfigfile <- function(string, directory) {
  if (!jsonlite::validate(string)) {
    return(F)
  }
  fcon <-
    file (description = do.call(file.path, as.list(c(
      getwd(), directory, "config.json"
    ))),
    open = "w")
  result <- write(x = string,
                  file = fcon)
  close.connection(fcon)
  return(result)
}

#' @export
loadRights <- function(directory) {
  config_file_contents <- readConfigfile(directory = directory)
  if (!jsonlite::validate(config_file_contents)) {
    return(return_rights)
  }
  return_rights <- list()
  config_list <- rjson::fromJSON(config_file_contents)
  if (!"rights" %in% names(config_list) ||
      !is.list(config_list[["rights"]]) ||
      length(config_list[["rights"]]) == 0) {
    return(return_rights)
  }
  return_rights <-
    as.list(names(config_list[["rights"]][which(sapply(config_list[["rights"]], function (x) {
      identical(x, T)
    }))]))
  return(return_rights)
}

#' @export
refreshRights <- function(session_user_data, directory) {
  session_user_data$rights <- loadRights(directory)
  return(T)
}

#' @export
granted <- function(right, rights) {
  errors <- c(!is.atomic(right),!is.character(right),
              length(right) != 1,!is.list(rights))
  if (any(errors) || !right %in% rights) {
    return(F)
  }
  return(T)
}
