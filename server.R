library(shiny)
library(RODBC)
library(RODBCext)
library(DT)
library(rmarkdownreportmanager)

options(scipen = 999)

onStop(
  fun = function() {
    RODBC::odbcCloseAll()
  }
)

shinyServer(function(input, output, session) {

  ###############################################################
  # Table view refreshers
  ###############################################################

  refreshDatasetTable <- function() {
    session$userData$datasets <-
      session$userData$dataset_adapter$getDatasets()
    session$userData$dataset_table <-
      getDatasetTable(dataset_list = session$userData$datasets,
                      tz = TIMEZONE)
    rv$dataset_table <- session$userData$dataset_table
  }
  refreshReportpackageTable <- function() {
    session$userData$reportpackages <-
      session$userData$reportpackage_adapter$getReportpackages()
    session$userData$reportpackage_table <-
      getReportpackageTable(reportpackage_list = session$userData$reportpackages,
                            tz = TIMEZONE)
    rv$reportpackages_table <- session$userData$reportpackage_table
  }
  refreshReportTable <- function() {
    session$userData$reports <-
      session$userData$report_adapter$getReports()
    session$userData$report_table <-
      getReportTable(
        report_list = session$userData$reports,
        tz = TIMEZONE,
        session_user_data = session$userData
      )
    rv$report_table <- session$userData$report_table
  }

  ###############################################################
  # OOP i/o tools
  ###############################################################

  connection_string <-
    sprintf(
      "DRIVER=SQLite3;Database=%s;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;",
      do.call(file.path, as.list(c(
        getwd(), DB_PATH, "db.db"
      )))
    )
  connection_obj <-
    Databaseconnector$new(connection_string = connection_string)
  session$userData$db_connection <- connection_obj$getConnection()
  session$userData$dataset_validator <-
    Datasetvalidator$new(db_connection = session$userData$db_connection)
  session$userData$dataset_adapter <-
    Datasetadapter$new(
      db_connection = session$userData$db_connection,
      dataset_validator = session$userData$dataset_validator
    )
  session$userData$dataset_fileadapter <-
    Datasetfileadapter$new(directory = do.call(file.path, as.list(c(
      getwd(), DATASET_STORAGE_PATH
    ))))

  session$userData$reportpackage_fileadapter <-
    Reportpackagefileadapter$new(
      directory = do.call(file.path, as.list(c(
        getwd(), REPORTPACKAGE_STORAGE_PATH
      ))),
      install_directory = do.call(file.path, as.list(
        c(getwd(), REPORTPACKAGE_INSTALLATION_PATH)
      ))
    )
  session$userData$reportpackage_validator <-
    Reportpackagevalidator$new(
      db_connection = session$userData$db_connection,
      fileadapter = session$userData$reportpackage_fileadapter
    )
  session$userData$reportpackage_adapter <-
    Reportpackageadapter$new(
      db_connection = session$userData$db_connection,
      reportpackage_validator = session$userData$reportpackage_validator
    )

  session$userData$report_fileadapter <-
    Reportfileadapter$new(directory = do.call(file.path, as.list(c(
      getwd(), REPORT_PATH
    ))))

  session$userData$report_validator <- Reportvalidator$new(
    db_connection = session$userData$db_connection,
    dataset_adapter = session$userData$dataset_adapter,
    reportpackage_adapter = session$userData$reportpackage_adapter
  )

  session$userData$report_adapter <-
    Reportadapter$new(
      db_connection = session$userData$db_connection,
      report_validator = session$userData$report_validator,
      fileadapter = session$userData$report_fileadapter
    )

  ###############################################################
  # status vars
  ###############################################################

  session$userData$generation <- NULL

  ###############################################################
  # output rendering
  ###############################################################

  output$datasets_table <- DT::renderDataTable(
    rv$dataset_table,
    server = FALSE,
    escape = FALSE,
    selection = 'none',
    rownames = F
  )
  output$reportpackages_table <- DT::renderDataTable(
    rv$reportpackages_table,
    server = FALSE,
    escape = FALSE,
    selection = 'none',
    rownames = F
  )
  output$report_table <- DT::renderDataTable(
    rv$report_table,
    server = FALSE,
    escape = FALSE,
    selection = 'none',
    rownames = F
  )

  rv <- reactiveValues(
    "UIcomponent_editDataset" = NULL,
    "dataset_update_name_eval" = NULL,
    "dataset_update_file_eval" = NULL,
    "UIcomponent_deleteDataset" = NULL,
    "dataset_table" = session$userData$dataset_table,

    "UIcomponent_editReportpackage" = NULL,
    "reportpackage_update_eval" = NULL,
    "UIcomponent_deleteReportpackage" = NULL,
    "reportpackage_table" = session$userData$reportpackage_table,

    "UIcomponent_editReport" = NULL,
    "report_create_eval" = NULL,
    "UIcomponent_deleteReport" = NULL,
    "report_table" = session$userData$report_table,
    "UIcomponent_reportMessage" = "",
    "UIcomponent_reportOutput" = "",

    "config_eval" = NULL,

    "timer" = reactiveTimer(1000)

  )

  bindings <- list(
    list("var" = "UIcomponent_editDataset",
         "function" = renderUI),
    list("var" = "dataset_update_name_eval",
         "function" = renderUI),
    list("var" = "dataset_update_file_eval",
         "function" = renderUI),
    list("var" = "UIcomponent_deleteDataset",
         "function" = renderUI),
    list("var" = "UIcomponent_editReportpackage",
         "function" = renderUI),
    list("var" = "reportpackage_update_eval",
         "function" = renderUI),
    list("var" = "UIcomponent_deleteReportpackage",
         "function" = renderUI),
    list("var" = "UIcomponent_editReport",
         "function" = renderUI),
    list("var" = "report_create_eval",
         "function" = renderUI),
    list("var" = "UIcomponent_deleteReport",
         "function" = renderUI),
    list("var" = "UIcomponent_reportMessage",
         "function" = renderText),
    list("var" = "UIcomponent_reportOutput",
         "function" = renderText),
    list("var" = "config_eval",
         "function" = renderText)
  )

  Map(function(i) {
    output[[bindings[[i]][["var"]]]] <<- bindings[[i]][["function"]]({
      rv[[bindings[[i]][["var"]]]]
    })
  }, 1:length(bindings))

  download_buttons <- c(
    "download_placeholder_dataset",
    "download_placeholder_reportpackage",
    "download_placeholder_report"
  )

  for (i in download_buttons) {
    output[[i]] <- downloadHandler(
      filename = function() {
        session$userData$downloadFilename
      },
      content = function(file) {
        file.copy(session$userData$downloadFile, file)
      }
    )
  }

  ###############################################################
  # startup calls
  ###############################################################

  refreshRights(session_user_data = session$userData,
                directory = CONFIG_PATH)
  refreshDatasetTable()
  refreshReportpackageTable()
  refreshReportTable()

  ###############################################################
  # event listeners
  ###############################################################

  observeEvent(input$button_dataset_create, {
    if (!granted("write_datasets", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }
    rv$UIcomponent_editDataset <-
      UIcomponent_editDataset(dataset = NULL)
  })

  observeEvent(input$button_reportpackage_create, {
    if (!granted("write_reportpackages", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }
    rv$UIcomponent_editReportpackage <-
      UIcomponent_editReportpackage(reportpackage = NULL)
  })

  observeEvent(input$button_report_create, {
    if (!granted("write_reports", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }
    rv$UIcomponent_editReport <-
      UIcomponent_editReport(report = NULL,
                             session_user_data = session$userData)
  })

  observeEvent(input$button_dataset_cancel, {
    rv$UIcomponent_editDataset <- NULL
    rv$dataset_update_name_eval <- NULL
    rv$dataset_update_file_eval <- NULL
  })

  observeEvent(input$button_reportpackage_cancel, {
    rv$UIcomponent_editReportpackage <- NULL
    rv$reportpackage_update_eval <- NULL
  })

  observeEvent(input$button_report_cancel, {
    rv$UIcomponent_editReport <- NULL
    rv$report_create_eval <- NULL
  })

  observeEvent(input$button_dataset_delete_cancel, {
    rv$UIcomponent_deleteDataset <- NULL
  })

  observeEvent(input$button_reportpackage_delete_cancel, {
    rv$UIcomponent_deleteReportpackage <- NULL
  })

  observeEvent(input$button_report_delete_cancel, {
    rv$UIcomponent_deleteReport <- NULL
  })

  observeEvent(input$button_dataset_save, {
    if (!granted("write_datasets", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }

    rv$dataset_update_name_eval <- NULL
    rv$dataset_update_file_eval <- NULL
    # validation
    input_id <- as.integer(input$dataset_update_id)
    input_name <- as.character(input$dataset_update_name)
    input_file <- input$dataset_update_file

    checks <- list(
      "update" = function(dataset_update) {
        validations <-
          list(
            "name" = session$userData$dataset_validator$validateName(name = input_name, dataset_update =
                                                                       dataset_update),
            "file" = (function() {
              check2 <-
                is.null(input_file) ||
                (!is.null(input_file) && file.exists(input_file$datapath))
              return(list(
                "status" = check2,
                "message" = ifelse(check2, "", "Please upload a valid RDS file.")
              ))
            })()
          )
        if (!all(c(validations$file$status, validations$name$status))) {
          return(validations)
        }
        if (!is.null(input_file)) {
          check3 <-
            session$userData$dataset_validator$validateFile(filepath = input_file$datapath)
          validations$file <- check3
        }
        return(validations)
      },
      "create" = function(dataset_update = NULL) {
        validations <-
          list(
            "name" = session$userData$dataset_validator$validateName(name = input_name),
            "file" = (function() {
              check2 <- !is.null(input_file) && file.exists(input_file$datapath)
              return(list(
                "status" = check2,
                "message" = ifelse(check2, "", "Please upload a valid RDS file.")
              ))
            })()
          )
        if (!all(c(validations$file$status, validations$name$status))) {
          return(validations)
        }
        check3 <-
          session$userData$dataset_validator$validateFile(filepath = input_file$datapath)
        validations$file <- check3
        return(validations)
      }
    )
    write <- list(
      "update" = function(dataset_update) {
        now <- floor(as.numeric(Sys.time()))
        if (!is.null(input_file)) {
          tibble_obj <- readRDS(file = input_file$datapath)
          occurrence_of_old_checksum <-
            session$userData$dataset_validator$getFileChecksumOccurence(dataset_update)
          # old checksum is unique ==> delete old file; else keep it
          prior_filename <- NULL
          if (occurrence_of_old_checksum == 1) {
            prior_filename <- dataset_update$filename
          }

          dataset_update$setFile(file = input_file$datapath)$setRows(rows = nrow(tibble_obj))$setCols(cols = ncol(tibble_obj))
          session$userData$dataset_fileadapter$updateFile(dataset = dataset_update,
                                                          file = tibble_obj,
                                                          prior_filename = prior_filename)

        }
        dataset_update$last_updated <- now
        dataset_update$setName(name = input_name)
        session$userData$dataset_adapter$updateDataset(dataset = dataset_update)
      },
      "create" = function(dataset_update = NULL) {
        tibble_obj <- readRDS(file = input_file$datapath)
        now <- floor(as.numeric(Sys.time()))
        new_dataset <- Dataset$new(
          id = NULL,
          name = input_name,
          filename = NULL,
          file_md5sum = NULL,
          rows = nrow(tibble_obj),
          cols = ncol(tibble_obj),
          created = now,
          last_updated = now,
          db_connection = session$userData$db_connection,
          validator = session$userData$dataset_validator
        )
        new_dataset$setFile(filepath = input_file$datapath)
        # sync with file directory
        session$userData$dataset_fileadapter$createFile(dataset = new_dataset, file = tibble_obj)
        # sync with db
        session$userData$dataset_adapter$createDataset(dataset = new_dataset)
      }
    )

    showModal(
      modalDialog(
        title = "Wait a moment please",
        "Checking your inputs ... ",
        easyClose = FALSE,
        footer = NULL
      )
    )

    dataset_update <- NULL
    search_datasets <- sapply(session$userData$datasets, function(x) {
      if (identical(x$id, input_id)) {
        dataset_update <<- x
      }
    })
    task <- ifelse(!is.null(dataset_update), "update", "create")
    validation <- checks[[task]](dataset_update)
    if (all(c(validation$file$status, validation$name$status))) {
      # save, confirm per notification
      write[[task]](dataset_update)
      showNotification(
        paste0(sprintf("Dataset successfully %sd.", task)),
        action = NULL,
        duration = 3,
        closeButton = TRUE,
        id = "dataset_created",
        type = "message"
      )
      rv$UIcomponent_editDataset <- NULL
      refreshDatasetTable()
    }
    rv$dataset_update_name_eval <- validation$name$message
    rv$dataset_update_file_eval <- validation$file$message

    removeModal()

  })

  observeEvent(input$button_dataset_delete_do, {
    if (!granted("delete_datasets", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }

    input_id <- as.integer(input$dataset_delete_id)
    dataset_delete <- NULL
    search_datasets <- sapply(session$userData$datasets, function(x) {
      if (identical(x$id, input_id)) {
        dataset_delete <<- x
      }
    })
    stopifnot(!is.null(dataset_delete))
    if (session$userData$dataset_validator$getFileChecksumOccurence(dataset_delete) == 1) {
      session$userData$dataset_fileadapter$deleteFileByDataset(dataset = dataset_delete)
    }
    session$userData$dataset_adapter$deleteDataset(dataset = dataset_delete)
    showNotification(
      "Dataset successfully deleted.",
      action = NULL,
      duration = 3,
      closeButton = TRUE,
      id = "dataset_deleted",
      type = "message"
    )
    rv$UIcomponent_deleteDataset <- NULL
    refreshDatasetTable()
  })

  observeEvent(input$button_reportpackage_save, {
    if (!granted("write_reportpackages", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }
    rv$reportpackage_update_eval <- NULL

    # validation
    input_id <- as.integer(input$reportpackage_update_id)
    input_file <- input$reportpackage_update_file

    checks <- list(
      "update" = function(reportpackage_update) {
        validations <- list("file" = (function() {
          check2 <-
            is.null(input_file) ||
            (!is.null(input_file) && file.exists(input_file$datapath))
          return(list(
            "status" = check2,
            "message" = ifelse(check2, "", "Please upload a valid R package file.")
          ))
        })())
        if (!all(c(validations$file$status))) {
          return(validations)
        }
        if (!is.null(input_file)) {
          check3 <-
            session$userData$reportpackage_validator$validate(package_file = input_file$datapath)
          validations$file <- check3
        }
        return(validations)
      },
      "create" = function(reportpackage_update = NULL) {
        validations <- list("file" = (function() {
          check2 <- !is.null(input_file) && file.exists(input_file$datapath)
          return(list(
            "status" = check2,
            "message" = ifelse(check2, "", "Please upload a valid R package file.")
          ))
        })())
        if (!all(c(validations$file$status))) {
          return(validations)
        }
        check3 <-
          session$userData$reportpackage_validator$validate(package_file = input_file$datapath)
        validations$file <- check3
        return(validations)
      }
    )
    write <- list(
      "update" = function(reportpackage_update, properties) {
        now <- floor(as.numeric(Sys.time()))

        # uninstall package
        session$userData$reportpackage_fileadapter$uninstallPackage(package_name = reportpackage_update$name,
                                                                    version = reportpackage_update$version)

        prior_filename <- reportpackage_update$filename
        reportpackage_update$setFile(filepath = input_file$datapath)
        reportpackage_update$setName(name = properties$package_name)
        reportpackage_update$setVersion(version = properties$package_version)
        reportpackage_update$last_updated <- now
        session$userData$reportpackage_fileadapter$updateFile(
          reportpackage = reportpackage_update,
          file = input_file$datapath,
          prior_filename =
            prior_filename
        )


        session$userData$reportpackage_fileadapter$installPackage(
          package_file = session$userData$reportpackage_fileadapter$getFile(reportpackage = reportpackage_update),
          package_name = reportpackage_update$name,
          version = reportpackage_update$version
        )

        update_db <-
          session$userData$reportpackage_adapter$updateReportpackage(reportpackage = reportpackage_update)
      },
      "create" = function(reportpackage_update = NULL, properties) {
        session$userData$reportpackage_fileadapter$installPackage(
          package_file = input_file$datapath,
          package_name = properties$package_name,
          version = properties$package_version
        )


        # create new object
        now <- floor(as.numeric(Sys.time()))
        reportpackage_new <- Reportpackage$new(
          id = NULL,
          name = properties$package_name,
          version = properties$package_version,
          filename = NULL,
          file_md5sum = NULL,
          created = now,
          last_updated = now,
          db_connection = session$userData$db_connection,
          validator = session$userData$reportpackage_validator
        )
        reportpackage_new$setFile(filepath = input_file$datapath)

        # file sync
        session$userData$reportpackage_fileadapter$createFile(reportpackage_new, input_file$datapath)

        # db sync
        session$userData$reportpackage_adapter$createReportpackage(reportpackage = reportpackage_new)
      }
    )

    showModal(
      modalDialog(
        title = "Wait a moment please",
        "Checking your inputs ... ",
        easyClose = FALSE,
        footer = NULL
      )
    )

    reportpackage_update <- NULL
    search_reportpackages <-
      sapply(session$userData$reportpackages, function(x) {
        if (identical(x$id, input_id)) {
          reportpackage_update <<- x
        }
      })
    task <- ifelse(!is.null(reportpackage_update), "update", "create")
    validation <- checks[[task]](reportpackage_update)
    if (all(c(validation$file$status))) {
      # save, confirm per notification
      write[[task]](reportpackage_update, validation$file$properties)
      showNotification(
        paste0(sprintf(
          "Report package successfully %sd.", task
        )),
        action = NULL,
        duration = 3,
        closeButton = TRUE,
        id = "reportpackage_created",
        type = "message"
      )
      rv$UIcomponent_editReportpackage <- NULL
      refreshReportpackageTable()
    }



    rv$reportpackage_update_eval <- validation$file$message

    removeModal()
  })

  observeEvent(input$button_reportpackage_delete_do, {
    if (!granted("delete_reportpackages", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }
    input_id <- as.integer(input$reportpackage_delete_id)
    reportpackage_delete <- NULL
    search_reportpackages <-
      sapply(session$userData$reportpackages, function(x) {
        if (identical(x$id, input_id)) {
          reportpackage_delete <<- x
        }
      })
    stopifnot(!is.null(reportpackage_delete))


    session$userData$reportpackage_fileadapter$uninstallPackage(package_name = reportpackage_delete$name,
                                                                version = reportpackage_delete$version)
    session$userData$reportpackage_fileadapter$deleteFile(filename = reportpackage_delete$filename)
    session$userData$reportpackage_adapter$deleteReportpackage(reportpackage = reportpackage_delete)
    showNotification(
      "Report package successfully deleted.",
      action = NULL,
      duration = 3,
      closeButton = TRUE,
      id = "reportpackage_deleted",
      type = "message"
    )
    rv$UIcomponent_deleteReportpackage <- NULL
    refreshReportpackageTable()
  })

  observeEvent(input$button_report_save, {
    if (!granted("write_reports", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }
    dataset_md5sum <- as.character(input$report_dataset_md5sum)
    reportpackage_md5sum <-
      as.character(input$report_reportpackage_md5sum)
    config_from <- as.integer(input$report_config_from_line)
    config_to <- as.integer(input$report_config_to_line)

    check <-
      session$userData$report_validator$validate(
        dataset_md5sum = dataset_md5sum,
        reportpackage_md5sum = reportpackage_md5sum,
        config = list("from_line" = config_from, "to_line" =
                        config_to),
        report = NULL
      )
    if (identical(check$status, T)) {
      now <- floor(as.numeric(Sys.time()))
      new_report_obj <- Report$new(
        id = NULL,
        created = now,
        lastrun_started = NULL,
        lastrun_finished = NULL,
        lastrun_status = NULL,
        lastrun_messagelog = list(),
        lastrun_filesgenerated = NULL,
        config = check$properties$config,
        dataset_md5sum = dataset_md5sum,
        reportpackage_md5sum = reportpackage_md5sum,
        config_md5sum = check$properties$config_md5sum,
        db_connection = session$userData$db_connection,
        validator = session$userData$report_validator,
        fileadapter = session$userData$report_fileadapter,
        adapter = session$userData$report_adapter
      )
      create_new_folder <-
        session$userData$report_fileadapter$create(report = new_report_obj)
      create_new_db_entry <-
        session$userData$report_adapter$createReport(report = new_report_obj)
      showNotification(
        paste0(sprintf("Report successfully created.")),
        action = NULL,
        duration = 3,
        closeButton = TRUE,
        id = "report_created",
        type = "message"
      )
      rv$UIcomponent_editReport <- NULL
      refreshReportTable()
    }
    rv$report_create_eval <- check$message
  })

  observeEvent(input$button_report_delete_do, {
    if (!granted("delete_reports", session$userData$rights)) {
      showModal(
        modalDialog(
          title = "Operation not allowed",
          "You do not have sufficient rights to perform the requested operation.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(F)
    }
    input_id <- as.integer(input$report_delete_id)
    report_delete <- NULL
    search_reports <- sapply(session$userData$reports, function(x) {
      if (identical(x$id, input_id)) {
        report_delete <<- x
      }
    })
    stopifnot(!is.null(report_delete))

    if (session$userData$report_adapter$activeReport() == report_delete$id) {
      showModal(
        modalDialog(
          title = "Operation not possible",
          "Documents are currently being generated; deletion is not possible.",
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return(FALSE)
    }

    delete1 <-
      session$userData$report_fileadapter$delete(report = report_delete)
    delete2 <-
      session$userData$report_adapter$deleteReport(report = report_delete)
    showNotification(
      "Report successfully deleted.",
      action = NULL,
      duration = 3,
      closeButton = TRUE,
      id = "report_deleted",
      type = "message"
    )
    rv$UIcomponent_deleteReport <- NULL
    refreshReportTable()

  })

  observe({
    rv$timer()

    if (isolate(!is.list(session$userData$generation))) {
      return(F)
    }








    current_status <-
      session$userData$generation$report$reportListen(
        folder = session$userData$generation$folder,
        shiny_callback = function(obj) {
        }
      )

    if (is.null(session$userData$report_progress) &&
        current_status$total > 0) {
      session$userData$report_progress <-
        shiny::Progress$new(session = session,
                            min = 0,
                            max = current_status$total)
      session$userData$report_progress$set(message = "Generating report ...", value = 0)
    }
    if (length(current_status$lastrun_messagelog) == 2) {
      rv$UIcomponent_reportMessage <-
        current_status$lastrun_messagelog[[1]]
      rv$UIcomponent_reportOutput <-
        current_status$lastrun_messagelog[[2]]
    }
    if (!is.null(session$userData$report_progress)) {
      session$userData$report_progress$set(current_status$progress)

    }
    status_task <- list(
      "0" = function() {
        session$userData$generation$report$update_self(arg_list = current_status[!names(current_status) %in% c("progress", "total")])
        session$userData$generation <- NULL
        if (!is.null(session$userData$report_progress)) {
          session$userData$report_progress$close()
        }
        session$userData$report_progress <- NULL
        refreshReportTable()
        showNotification(
          "Report successfully finished.",
          action = NULL,
          duration = 3,
          closeButton = TRUE,
          id = "report_finished",
          type = "message"
        )
        return(T)

      },
      "1" = function() {
        showModal(
          modalDialog(
            title = "Report generation failed",
            "An error occured during the procedure. Please check the logs for details.",
            easyClose = TRUE,
            footer = modalButton("OK"),
            size = "s"
          )
        )
        session$userData$generation$report$update_self(arg_list = current_status[!names(current_status) %in% c("progress", "total")])
        session$userData$generation <- NULL
        if (!is.null(session$userData$report_progress)) {
          session$userData$report_progress$close()
        }
        session$userData$report_progress <- NULL
        refreshReportTable()
        return(F)
      },
      "2" = function() {
        return(F)
      }
    )
    status_task[[as.character(current_status$lastrun_status)]]()
  })

  observeEvent(input$action_update_config, {
    if (!granted("update_config", session$userData$rights)) {
      rv$config_eval <- "Cannot update config file: Insufficient rights."
      return(F)
    }

    write_file <- writeConfigfile(string = input$configuration,
                                  directory = CONFIG_PATH)

    if (identical(write_file, F)) {
      rv$config_eval <-
        "Cannot update config file: Input has invalid format; or file cannot be overwritten."
      return(F)
    }

    rv$config_eval <- NULL

    refreshRights(session_user_data = session$userData,
                  directory = CONFIG_PATH)
    stopApp()

  })

  observeEvent(input$action_button, {
    cond <- strsplit(input$action_button, "_")[[1]]
    action <- cond[1]
    id <- as.integer(cond[2])
    dataset <- NULL
    search_datasets <- sapply(session$userData$datasets, function(x) {
      if (identical(x$id, id)) {
        dataset <<- x
      }
    })
    stopifnot(!is.null(dataset))

    task <- list(
      "edit" = function() {
        if (!granted("write_datasets", session$userData$rights)) {
          showModal(
            modalDialog(
              title = "Operation not allowed",
              "You do not have sufficient rights to perform the requested operation.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }


        rv$UIcomponent_editDataset <-
          UIcomponent_editDataset(dataset = dataset)
      },
      "delete" = function() {
        if (!granted("delete_datasets", session$userData$rights)) {
          showModal(
            modalDialog(
              title = "Operation not allowed",
              "You do not have sufficient rights to perform the requested operation.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }

        rv$UIcomponent_deleteDataset <-
          UIcomponent_deleteDataset(dataset = dataset)
      },
      "download" = function() {
        session$userData$downloadFile <-
          session$userData$dataset_fileadapter$getFilepath(filename = dataset$filename)
        session$userData$downloadFilename <-
          paste0(dataset$name, ".rds")
        session$sendCustomMessage(type = 'start_download', list(value = "dataset"))
      }
    )
    task[[action]]()
  })

  observeEvent(input$action_button_reportpackage, {
    cond <- strsplit(input$action_button_reportpackage, "_")[[1]]
    action <- cond[1]
    id <- as.integer(cond[2])
    reportpackage <- NULL
    search_reportpackages <-
      sapply(session$userData$reportpackages, function(x) {
        if (identical(x$id, id)) {
          reportpackage <<- x
        }
      })
    stopifnot(!is.null(reportpackage))

    task <- list(
      "edit" = function() {
        if (!granted("write_reportpackages", session$userData$rights)) {
          showModal(
            modalDialog(
              title = "Operation not allowed",
              "You do not have sufficient rights to perform the requested operation.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }

        rv$UIcomponent_editReportpackage <-
          UIcomponent_editReportpackage(reportpackage = reportpackage)
      },
      "delete" = function() {
        if (!granted("delete_reportpackages", session$userData$rights)) {
          showModal(
            modalDialog(
              title = "Operation not allowed",
              "You do not have sufficient rights to perform the requested operation.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }

        rv$UIcomponent_deleteReportpackage <-
          UIcomponent_deleteReportpackage(reportpackage = reportpackage)
      },
      "download" = function() {
        session$userData$downloadFile <-
          session$userData$reportpackage_fileadapter$getFile(reportpackage = reportpackage)
        session$userData$downloadFilename <-
          paste0(reportpackage$name,
                 "_",
                 reportpackage$version,
                 ".tar.gz")
        session$sendCustomMessage(type = 'start_download', list(value = "dataset"))
      }
    )
    task[[action]]()
  })

  observeEvent(input$action_button_report, {
    cond <- strsplit(input$action_button_report, "_")[[1]]
    action <- cond[1]
    id <- as.integer(cond[2])
    report <- NULL
    search_reports <- sapply(session$userData$reports, function(x) {
      if (identical(x$id, id)) {
        report <<- x
      }
    })
    stopifnot(!is.null(report))

    task <- list(
      "run" = function() {
        if (is.integer(session$userData$report_adapter$activeReport())) {
          showModal(
            modalDialog(
              title = "Operation not possible",
              "Documents are currently being generated.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(FALSE)
        }

        run_result <-
          report$run(
            reportpackage_installation_path = do.call(file.path, as.list(
              c(getwd(), REPORTPACKAGE_INSTALLATION_PATH)
            )),
            dataset_path = do.call(file.path, as.list(c(
              getwd(), DATASET_STORAGE_PATH
            ))),
            report_path = do.call(file.path, as.list(c(
              getwd(), REPORT_PATH
            )))
          )

        if (identical(run_result, F)) {
          showModal(
            modalDialog(
              title = "Operation not possible",
              "Invalid parameters. Please make sure that the dataset and the report package are still existing.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }


        session$userData$generation <- run_result
      },
      "download" = function() {
        showModal(
          modalDialog(
            title = "Wait a moment please",
            "Generating archive ... ",
            easyClose = FALSE,
            footer = NULL
          )
        )
        zipfile <-
          session$userData$report_fileadapter$getFiles(report = report)
        if (identical(zipfile, F)) {
          removeModal()
          showModal(
            modalDialog(
              title = "No files to download",
              "The output directory is empty.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }

        check_dataset <-
          session$userData$dataset_adapter$getDatasetByMd5sum(md5sum = report$dataset_md5sum)
        check_reportpackage <-
          session$userData$reportpackage_adapter$getReportpackageByMd5sum(md5sum = report$reportpackage_md5sum)
        new_filename <- paste0(
          "report_",
          ifelse(
            !R6::is.R6(check_dataset),
            "missing",
            check_dataset$name
          ),
          "_",
          ifelse(
            !R6::is.R6(check_reportpackage),
            "missing",
            check_reportpackage$name
          ),
          "-",
          ifelse(
            !R6::is.R6(check_reportpackage),
            "missing",
            check_reportpackage$version
          ),
          ".zip"
        )

        session$userData$downloadFile <- zipfile
        session$userData$downloadFilename <- new_filename

        removeModal()

        session$sendCustomMessage(type = 'start_download', list(value = "report"))
      },
      "copy" = function() {
        if (!granted("write_reports", session$userData$rights)) {
          showModal(
            modalDialog(
              title = "Operation not allowed",
              "You do not have sufficient rights to perform the requested operation.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }

        rv$UIcomponent_editReport <-
          UIcomponent_editReport(report = report,
                                 session_user_data = session$userData)
      },
      "delete" = function() {
        if (!granted("delete_reports", session$userData$rights)) {
          showModal(
            modalDialog(
              title = "Operation not allowed",
              "You do not have sufficient rights to perform the requested operation.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(F)
        }
        rv$UIcomponent_deleteReport <-
          UIcomponent_deleteReport(report = report)
      },
      "logs" = function() {
        if (is.integer(session$userData$report_adapter$activeReport())) {
          showModal(
            modalDialog(
              title = "Operation not possible",
              "Documents are currently being generated.",
              easyClose = TRUE,
              footer = modalButton("OK"),
              size = "s"
            )
          )
          return(FALSE)
        }
        if (length(report$lastrun_messagelog) == 2) {
          rv$UIcomponent_reportMessage <- report$lastrun_messagelog[[1]]
          rv$UIcomponent_reportOutput <-
            report$lastrun_messagelog[[2]]
        } else {
          rv$UIcomponent_reportMessage <- ""
          rv$UIcomponent_reportOutput <- ""
        }
      }
    )
    task[[action]]()
  })
})
