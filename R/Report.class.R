#' @export
Report <- R6::R6Class(
  classname = "Report",
  public = list(
    id = NULL,
    created = NULL,
    lastrun_started = NULL,
    lastrun_finished = NULL,
    lastrun_status = NULL,
    lastrun_messagelog = NULL,
    lastrun_filesgenerated = NULL,
    config = NULL,
    dataset_md5sum = NULL,
    reportpackage_md5sum = NULL,
    config_md5sum = NULL,
    initialize = function(id,
                          created,
                          lastrun_started,
                          lastrun_finished,
                          lastrun_status,
                          lastrun_messagelog,
                          lastrun_filesgenerated,
                          config,
                          dataset_md5sum,
                          reportpackage_md5sum,
                          config_md5sum,
                          db_connection,
                          validator,
                          fileadapter,
                          adapter) {
      self$id <- id
      self$created <- created
      self$lastrun_started <- lastrun_started
      self$lastrun_finished <- lastrun_finished
      self$lastrun_status <- lastrun_status
      self$lastrun_messagelog <- lastrun_messagelog
      self$lastrun_filesgenerated <- lastrun_filesgenerated
      self$config <- config
      self$dataset_md5sum <- dataset_md5sum
      self$reportpackage_md5sum <- reportpackage_md5sum
      self$config_md5sum <- config_md5sum
      private$db_connection <- db_connection
      private$validator <- validator
      private$fileadapter <- fileadapter
      private$adapter <- adapter
      return(invisible(self))
    },
    print = function(...) {
      cat(str(self))
      invisible(self)
    },
    update_self = function(arg_list) {
      for (i in names(arg_list)) {
        self[[i]] <- arg_list[[i]]
      }
      private$adapter$updateReport(self)
    },
    run = function(reportpackage_installation_path,
                   dataset_path,
                   report_path) {
      require(dplyr)
      validate <- private$validator$validate(
        dataset_md5sum = self$dataset_md5sum,
        reportpackage_md5sum = self$reportpackage_md5sum,
        config = self$config,
        report = self
      )
      if (!validate$status) {
        return(F)
      }
      private$fileadapter$cleanup(report = self)
      report_path <-
        private$fileadapter$getFilepath(report = self)
      dataset_table <-
        readRDS(file = file.path(dataset_path,
                                 validate$properties$dataset$filename))[self$config$from_line:self$config$to_line, ]
      dataset_table <- dataset_table %>%
        mutate(
          meta_rownr = 1:nrow(dataset_table),
          meta_dataset_md5sum = validate$properties$dataset$file_md5sum,
          meta_dataset_name = validate$properties$dataset$name,
          meta_reportpackage_md5sum = validate$properties$reportpackage$file_md5sum,
          meta_reportpackage_name = validate$properties$reportpackage$name,
          meta_reportpackage_version = validate$properties$reportpackage$version,
          meta_config_md5sum = validate$properties$config_md5sum
        )
      arg_list <- list(
        "lastrun_started" = floor(as.numeric(Sys.time())),
        "lastrun_status" = 1,
        "lastrun_messagelog" = list(),
        "lastrun_filesgenerated" = 0,
        "lastrun_finished" = NULL
      )
      self$update_self(arg_list = arg_list)
      reportpackage_folder <-
        file.path(
          reportpackage_installation_path,
          paste0(
            validate$properties$reportpackage$name,
            "_",
            validate$properties$reportpackage$version
          )
        )
      file_temptable <-
        file.path(reportpackage_folder, "temptable.rds")
      saveRDS(dataset_table, file = file_temptable)
      R_script <- paste(
        "R.version.string",
        sprintf(
          "sink(file = file (description = file.path(\"%s\",\"log_messages.txt\"), open = \"w\"), type = \"message\")",
          report_path
        ),
        sprintf(
          "sink(file = file (description = file.path(\"%s\",\"log_output.txt\"), open = \"w\"), type = \"output\")",
          report_path
        ),
        "library(tibble)",
        "library(rmarkdown)",
        "library(doParallel)",
        "library(knitr)",
        sprintf("library(%s)", validate$properties$reportpackage$name),
        "cores <- parallel::detectCores()",
        # "registerDoParallel(cores=ifelse(cores>1,cores-1,1))",
        "registerDoParallel(cores=1)",
        "rpts <- readRDS(\"temptable.rds\")",
        sprintf(
          "foreach(i = 1:nrow(rpts), .combine = rbind) %%dopar%% { %s::makeReport(rpts[i,],\"%s\"); cat(paste0(\"\\n*** PROGRESS: \",i,\"/\",%s,\" ***\\n\")) }",
          validate$properties$reportpackage$name,
          report_path,
          nrow(dataset_table)
        ),
        "warnings()",
        "cat(\"*** EOF ***\")",
        "sink()",
        sep = "; "
      )
      return_value <-
        system(sprintf("cd %s; Rscript -e '%s'",
                       reportpackage_folder,
                       R_script),
               wait = F)
      return(list(
        "folder" = file.path(report_path),
        "system_obj" = return_value,
        "report" = self
      ))
    },
    reportListen = function(folder,
                            shiny_callback = function(obj) {
                            }) {
      # this function handles an ongoing report generator procedure, each time called
      return_list <- list(
        "lastrun_messagelog" = list(),
        "lastrun_status" = 2,
        "progress" = 0,
        "total" = 0,
        "lastrun_filesgenerated" = 0,
        "lastrun_finished" = NULL
      )
      outfile_file <-
        file.path(folder, "log_output.txt")
      outfile_message <-
        file.path(folder, "log_messages.txt")
      if (!all(file.exists(outfile_file), file.exists(outfile_message))) {
        return(return_list)
      }
      lines_output <-
        scan(outfile_file, what = "character", sep = "\n")
      lines_messages <-
        scan(outfile_message, what = "character", sep = "\n")
      total_nr <- 0
      total_progress <- 0
      if (length(lines_output) > 0) {
        m <- regexec("\\*\\*\\* PROGRESS: ([0-9]+)/([0-9]+) \\*\\*\\*",
                     lines_output)
        search_results <-
          regmatches(lines_output, m)
        total_progress <-
          max(c(0, sapply(search_results, function(x) {
            if (length(x) == 3) {
              total_nr <<- as.integer(x[3])
              return(as.integer(x[2]))
            }
            return(0)
          })), na.rm = T)
      }
      return_list$progress <- total_progress
      return_list$total <- total_nr
      return_list$lastrun_messagelog <-
        as.list(c(
          paste(lines_messages, collapse = "\n"),
          paste(lines_output, collapse = "\n")
        ))
      error_detected <-
        identical(lines_messages[length(lines_messages)], "Execution halted")
      finish_detected <-
        identical(lines_output[length(lines_output)], "*** EOF ***")
      completion_detected <-
        total_progress > 0 && total_progress == total_nr
      if (any(c(error_detected, finish_detected, completion_detected))) {
        return_list$lastrun_status <- ifelse(error_detected, 1, 0)
        return_list$lastrun_filesgenerated <-
          private$fileadapter$countFiles(report = self)
        return_list$lastrun_finished <-
          floor(as.numeric(Sys.time()))
      }
      return(return_list)
    }
  ),
  private = list(
    db_connection = NULL,
    validator = NULL,
    fileadapter = NULL,
    adapter = NULL
  )
)
