#' @export
Reportvalidator <- R6::R6Class(
  classname = "Reportvalidator",
  public = list(
    initialize = function(db_connection,
                          dataset_adapter,
                          reportpackage_adapter) {
      private$db_connection <- db_connection
      private$dataset_adapter <- dataset_adapter
      private$reportpackage_adapter <-
        reportpackage_adapter
      return(invisible(self))
    },
    getObjectMd5sum = function(obj) {
      return(digest::digest(obj, algo = "md5"))
    },
    validate = function(dataset_md5sum,
                        reportpackage_md5sum,
                        config,
                        report = NULL) {
      return_value <-
        function(return_status,
                 message,
                 check_dataset,
                 check_reportpackage,
                 config,
                 config_md5sum) {
          list(
            "status" = return_status,
            "message" = message,
            "properties" = list(
              "dataset" = check_dataset,
              "reportpackage" = check_reportpackage,
              "config" = config,
              "config_md5sum" = config_md5sum
            )
          )
        }
      config <-
        list(
          "from_line" = as.character(config$from_line),
          "to_line" = as.character(config$to_line)
        )
      check_config <- sapply(config, function(x) {
        grep("^[0-9]+$", x)
      })
      if (is.integer(check_config)) {
        config <- list(
          "from_line" = as.integer(config$from_line),
          "to_line" = as.integer(config$to_line)
        )
      }
      config_md5sum <- NULL
      check_dataset <-
        private$dataset_adapter$getDatasetByMd5sum(md5sum = dataset_md5sum)
      check_reportpackage <-
        private$reportpackage_adapter$getReportpackageByMd5sum(md5sum = reportpackage_md5sum)
      message <-
        trimws(paste0(
          ifelse(
            is.integer(check_config),
            "",
            "Invalid configuration parameters."
          ),
          ifelse(R6::is.R6(check_dataset), "", "Invalid dataset."),
          " ",
          ifelse(
            R6::is.R6(check_reportpackage),
            "",
            "Invalid report package."
          )
        ))
      return_status <- message == ""
      if (!return_status) {
        return(
          return_value(
            return_status,
            message,
            check_dataset,
            check_reportpackage,
            config,
            config_md5sum
          )
        )
      }
      return_status <- !any(
        config$from_line < 1L,
        config$from_line > config$to_line,
        config$to_line > check_dataset$rows
      )
      message <-
        ifelse(return_status, "", "Invalid configuration parameters.")
      if (!return_status) {
        return(
          return_value(
            return_status,
            message,
            check_dataset,
            check_reportpackage,
            config,
            config_md5sum
          )
        )
      }
      config_md5sum <-
        self$getObjectMd5sum(obj = config)
      message <-
        ifelse(
          private$isReportExisting(
            dataset_md5sum = dataset_md5sum,
            reportpackage_md5sum = reportpackage_md5sum,
            config_md5sum = config_md5sum,
            except_report = report
          ),
          "Identical report is already existing.",
          ""
        )
      return_status <- message == ""
      return(
        return_value(
          return_status,
          message,
          check_dataset,
          check_reportpackage,
          config,
          config_md5sum
        )
      )
    }
  ),
  private = list(
    db_connection = NULL,
    dataset_adapter = NULL,
    reportpackage_adapter = NULL,
    isReportExisting = function(dataset_md5sum,
                                reportpackage_md5sum,
                                config_md5sum,
                                except_report) {
      query_addition <- ""
      values <-
        c(dataset_md5sum, reportpackage_md5sum, config_md5sum)
      if (!is.null(except_report)) {
        query_addition <- " AND report_id != ?"
        values <- c(values, except_report$id)
      }
      query <-
        paste0(
          "SELECT report_id from reports where dataset_md5sum = ? AND reportpackage_md5sum = ? AND config_md5sum = ?",
          query_addition
        )
      value_df <-
        data.frame(matrix(ncol = length(values), nrow = 1), stringsAsFactors = F)
      value_df[1, ] <- values
      RODBCext::sqlPrepare(channel = private$db_connection, query = query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          value_df,
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      if (!is.data.frame(result)) {
        stop("SQL execution error. Data frame expected.")
      }
      if (nrow(result) > 0) {
        return(TRUE)
      }
      return(FALSE)
    }
  )
)
