context("Report CRUD tests")

library(testthat)
library(rmarkdownreportmanager)
library(RODBC)
library(RODBCext)
library(RSQLite)

createTestSetting <- function(folder) {
  mydb <-
    dbConnect(RSQLite::SQLite(),
              file.path(folder, "files", "testdb.sqlite"))
  dbDisconnect(mydb)
  connectionString <-
    sprintf(
      "DRIVER=SQLite3;Database=%s/files/testdb.sqlite;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;",
      folder
    )
  connection_obj <-
    Databaseconnector$new(connection_string = connectionString)
  connection <- connection_obj$getConnection()
  query <-
    paste0(readLines(file.path(folder, "dataset_sql.sql")), collapse = " ")
  RODBCext::sqlPrepare(channel = connection, query = query)
  result <- RODBCext::sqlExecute(
    connection,
    NULL,
    NULL,
    fetch = T,
    rows_at_time = 1,
    believeNRows = FALSE
  )
  dir.create(file.path(folder,
                       "files",
                       "reports"))
  dir.create(file.path(folder,
                       "files",
                       "reportpackage_installations"))
  dir.create(file.path(folder,
                       "files",
                       "reportpackages"))
  dir.create(file.path(folder,
                       "files",
                       "datasets"))
  return(TRUE)
}
eraseTestSetting <- function(folder) {
  unlink(file.path(folder, "files", "*"), recursive = T)
  return(TRUE)
}
createTestCases <- function(folder) {
  test_folder_path <- normalizePath(testthat::test_path())
  connectionString <-
    sprintf(
      "DRIVER=SQLite3;Database=%s/files/testdb.sqlite;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;",
      test_folder_path
    )
  connection_obj <-
    Databaseconnector$new(connection_string = connectionString)
  connection <- connection_obj$getConnection()
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  dataset_fileadapter <-
    Datasetfileadapter$new(directory = file.path(test_folder_path, "files", "datasets"))
  temp_dir <- tempdir()
  test_name2 <- "valid_dataset"
  temp_rds3 <- paste0(temp_dir, "/testfile3.RDS")
  tibble_obj <- tibble::as.tibble(mtcars[, ])
  saveRDS(object = tibble_obj, file = temp_rds3)
  check5 <- dataset_validator$validateName(name = test_name2)
  check6 <- dataset_validator$validateFile(filepath = temp_rds3)
  value <- list("status" = TRUE,
                "message" = "")
  expect_equal(check5, value)
  expect_equal(check6, value)
  # create new dataset object
  new_dataset <- Dataset$new(
    id = NULL,
    name = test_name2,
    filename = NULL,
    file_md5sum = NULL,
    rows = nrow(tibble_obj),
    cols = ncol(tibble_obj),
    created = 4754857L,
    last_updated = 4754857L,
    db_connection = connection,
    validator = dataset_validator
  )
  new_dataset$setFile(filepath = temp_rds3)
  # sync with file directory
  dataset_fileadapter$createFile(dataset = new_dataset, file = tibble_obj)
  # sync with db
  dataset_adapter$createDataset(dataset = new_dataset)
  ####################################################################################
  reportpackage_fileadapter <-
    Reportpackagefileadapter$new(
      directory = file.path(test_folder_path, "files", "reportpackages"),
      install_directory =
        file.path(test_folder_path, "files", "reportpackage_installations")
    )
  reportpackage_validator <-
    Reportpackagevalidator$new(db_connection = connection,
                               fileadapter = reportpackage_fileadapter)
  reportpackage_adapter <-
    Reportpackageadapter$new(db_connection = connection,
                             reportpackage_validator = reportpackage_validator)
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/rp001valid_0.1.0.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "rp001valid_0.1.0.tar.gz"),
    to = temp_file
  )
  check1 <-
    reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(list(
    "status" = T,
    "message" = "",
    "properties" = list("package_name" = "rp001valid", "package_version" = "0.1.0")
  ), check1)
  # install new package
  installation <-
    reportpackage_fileadapter$installPackage(
      package_file = temp_file,
      package_name = check1$properties$package_name,
      version = check1$properties$package_version
    )
  expect_equal(installation, T)
  # create new object
  now <- floor(as.numeric(Sys.time()))
  reportpackage_new <- Reportpackage$new(
    id = NULL,
    name = check1$properties$package_name,
    version = check1$properties$package_version,
    filename = NULL,
    file_md5sum = NULL,
    created = now,
    last_updated = now,
    db_connection = connection,
    validator = reportpackage_validator
  )
  reportpackage_new$setFile(filepath = temp_file)
  # file sync
  reportpackage_fileadapter$createFile(reportpackage_new, temp_file)
  # db sync
  create <-
    reportpackage_adapter$createReportpackage(reportpackage = reportpackage_new)
  expect_equal(create, list("status" = TRUE,
                            "message" = NULL))
  return(T)
}
test_that("DB connection can be established", {
  #########################
  test_folder_path <- normalizePath(testthat::test_path())
  eraseTestSetting(folder = test_folder_path)
  createTestSetting(folder = test_folder_path)
  connectionString <-
    sprintf(
      "DRIVER=SQLite3;Database=%s/files/testdb.sqlite;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;",
      test_folder_path
    )
  connection_obj <-
    Databaseconnector$new(connection_string = connectionString)
  connection <- connection_obj$getConnection()
  expect_equal(typeof(connection), "integer")
  expect_equal(connection > 0, TRUE)
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Report objects can be validated and created", {
  #########################
  test_folder_path <- normalizePath(testthat::test_path())
  eraseTestSetting(folder = test_folder_path)
  createTestSetting(folder = test_folder_path)
  createTestCases(folder = test_folder_path)
  connectionString <-
    sprintf(
      "DRIVER=SQLite3;Database=%s/files/testdb.sqlite;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;",
      test_folder_path
    )
  connection_obj <-
    Databaseconnector$new(connection_string = connectionString)
  connection <- connection_obj$getConnection()
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  reportpackage_fileadapter <-
    Reportpackagefileadapter$new(
      directory = file.path(test_folder_path, "files", "reportpackages"),
      install_directory =
        file.path(test_folder_path, "files", "reportpackage_installations")
    )
  reportpackage_validator <-
    Reportpackagevalidator$new(db_connection = connection,
                               fileadapter = reportpackage_fileadapter)
  reportpackage_adapter <-
    Reportpackageadapter$new(db_connection = connection,
                             reportpackage_validator = reportpackage_validator)
  reportpackage_validator <-
    Reportpackagevalidator$new(db_connection = connection,
                               fileadapter = reportpackage_fileadapter)
  reportpackage_adapter <-
    Reportpackageadapter$new(db_connection = connection,
                             reportpackage_validator = reportpackage_validator)
  report_fileadapter <-
    Reportfileadapter$new(directory = file.path(test_folder_path, "files", "reports"))
  report_validator <- Reportvalidator$new(
    db_connection = connection,
    dataset_adapter = dataset_adapter,
    reportpackage_adapter = reportpackage_adapter
  )
  report_adapter <- Reportadapter$new(
    db_connection = connection,
    report_validator = report_validator,
    fileadapter = report_fileadapter
  )
  #################################################################
  # validation case 1:
  #   Invalid dataset checksum,
  #   valid reportpackage checksum,
  #   valid configuration parameters
  check <- report_validator$validate(
    dataset_md5sum = "123",
    reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
    config = list("from_line" = "1", "to_line" =
                    "3"),
    report = NULL
  )
  expect_equal(check$status, F)
  expect_equal(check$message, "Invalid dataset.")
  expect_equal(R6::is.R6(check$properties$dataset), F)
  expect_equal(R6::is.R6(check$properties$reportpackage), T)
  expect_equal(check$properties$config,
               list("from_line" = 1L, "to_line" = 3L))
  expect_equal(check$properties$config_md5sum, NULL)
  #################################################################
  # validation case 2:
  #   Valid dataset checksum,
  #   invalid reportpackage checksum,
  #   valid configuration parameters
  check <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "123",
      config = list("from_line" = "1", "to_line" =
                      "3"),
      report = NULL
    )
  expect_equal(check$status, F)
  expect_equal(check$message, "Invalid report package.")
  expect_equal(R6::is.R6(check$properties$dataset), T)
  expect_equal(R6::is.R6(check$properties$reportpackage), F)
  expect_equal(check$properties$config,
               list("from_line" = 1L, "to_line" = 3L))
  expect_equal(check$properties$config_md5sum, NULL)
  #################################################################
  # validation case 3:
  #   Valid dataset checksum,
  #   valid reportpackage checksum,
  #   invalid configuration parameters
  check <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
      config = list("from_line" = "a", "to_line" =
                      "3"),
      report = NULL
    )
  check2 <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
      config = list("from_line" = "1", "to_line" =
                      "300"),
      report = NULL
    )
  expect_equal(check$status, F)
  expect_equal(check$message, "Invalid configuration parameters.")
  expect_equal(R6::is.R6(check$properties$dataset), T)
  expect_equal(R6::is.R6(check$properties$reportpackage), T)
  expect_equal(check$properties$config,
               list("from_line" = "a", "to_line" = "3"))
  expect_equal(check$properties$config_md5sum, NULL)
  expect_equal(check2$status, F)
  expect_equal(check2$message, "Invalid configuration parameters.")
  expect_equal(R6::is.R6(check2$properties$dataset), T)
  expect_equal(R6::is.R6(check2$properties$reportpackage), T)
  expect_equal(check2$properties$config,
               list("from_line" = 1, "to_line" = 300))
  expect_equal(check2$properties$config_md5sum, NULL)
  #################################################################
  # validation case 4:
  #   Valid dataset checksum,
  #   valid reportpackage checksum,
  #   valid configuration parameters
  check <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
      config = list("from_line" = "1", "to_line" =
                      "32"),
      report = NULL
    )
  expect_equal(check$status, T)
  expect_equal(check$message, "")
  expect_equal(R6::is.R6(check$properties$dataset), T)
  expect_equal(R6::is.R6(check$properties$reportpackage), T)
  expect_equal(check$properties$config,
               list("from_line" = 1L, "to_line" = 32L))
  expect_equal(check$properties$config_md5sum,
               "2d59500463800e78aced996369aab8e9")
  #################################################################
  # create new report
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
    dataset_md5sum = "90014426df46342419768278a12de3da",
    reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
    config_md5sum = check$properties$config_md5sum,
    db_connection = connection,
    validator = report_validator,
    fileadapter = report_fileadapter,
    adapter = report_adapter
  )
  create_new_folder <-
    report_fileadapter$create(report = new_report_obj)
  create_new_db_entry <-
    report_adapter$createReport(report = new_report_obj)
  all_reports <- report_adapter$getReports()
  expect_equal(dir.exists(report_fileadapter$getFilepath(report = new_report_obj)), T)
  expect_equal(create_new_folder, T)
  expect_equal(create_new_db_entry, list("status" = TRUE,
                                         "message" = NULL))
  expect_equal(length(all_reports), 1)
  #################################################################
  # validation case 5 (with properties of existing reports):
  #   Valid dataset checksum,
  #   valid reportpackage checksum,
  #   valid configuration parameters
  check1 <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
      config = list("from_line" = "1", "to_line" =
                      "32"),
      report = NULL
    )
  expect_equal(check1$status, F)
  expect_equal(check1$message, "Identical report is already existing.")
  check2 <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
      config = list("from_line" = "1", "to_line" =
                      "32"),
      report = all_reports[[1]]
    )
  expect_equal(check$status, T)
  expect_equal(check$message, "")
  #################################################################
  # create, then run cases 1,2,3 again with run
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Reports can be run, updated, downloaded and deleted", {
  #########################
  test_folder_path <- normalizePath(testthat::test_path())
  eraseTestSetting(folder = test_folder_path)
  createTestSetting(folder = test_folder_path)
  createTestCases(folder = test_folder_path)
  connectionString <-
    sprintf(
      "DRIVER=SQLite3;Database=%s/files/testdb.sqlite;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;",
      test_folder_path
    )
  connection_obj <-
    Databaseconnector$new(connection_string = connectionString)
  connection <- connection_obj$getConnection()
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  reportpackage_fileadapter <-
    Reportpackagefileadapter$new(
      directory = file.path(test_folder_path, "files", "reportpackages"),
      install_directory =
        file.path(test_folder_path, "files", "reportpackage_installations")
    )
  reportpackage_validator <-
    Reportpackagevalidator$new(db_connection = connection,
                               fileadapter = reportpackage_fileadapter)
  reportpackage_adapter <-
    Reportpackageadapter$new(db_connection = connection,
                             reportpackage_validator = reportpackage_validator)
  reportpackage_validator <-
    Reportpackagevalidator$new(db_connection = connection,
                               fileadapter = reportpackage_fileadapter)
  reportpackage_adapter <-
    Reportpackageadapter$new(db_connection = connection,
                             reportpackage_validator = reportpackage_validator)
  report_fileadapter <-
    Reportfileadapter$new(directory = file.path(test_folder_path, "files", "reports"))
  report_validator <- Reportvalidator$new(
    db_connection = connection,
    dataset_adapter = dataset_adapter,
    reportpackage_adapter = reportpackage_adapter
  )
  report_adapter <- Reportadapter$new(
    db_connection = connection,
    report_validator = report_validator,
    fileadapter = report_fileadapter
  )
  #################################################################
  # create new report
  check <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
      config = list("from_line" = "1", "to_line" =
                      "32"),
      report = NULL
    )
  expect_equal(check$status, T)
  expect_equal(check$message, "")
  expect_equal(R6::is.R6(check$properties$dataset), T)
  expect_equal(R6::is.R6(check$properties$reportpackage), T)
  expect_equal(check$properties$config,
               list("from_line" = 1L, "to_line" = 32L))
  expect_equal(check$properties$config_md5sum,
               "2d59500463800e78aced996369aab8e9")
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
    dataset_md5sum = "90014426df46342419768278a12de3da",
    reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
    config_md5sum = check$properties$config_md5sum,
    db_connection = connection,
    validator = report_validator,
    fileadapter = report_fileadapter,
    adapter = report_adapter
  )
  create_new_folder <-
    report_fileadapter$create(report = new_report_obj)
  create_new_db_entry <-
    report_adapter$createReport(report = new_report_obj)
  all_reports <- report_adapter$getReports()
  expect_equal(dir.exists(report_fileadapter$getFilepath(report = new_report_obj)), T)
  expect_equal(create_new_folder, T)
  expect_equal(create_new_db_entry, list("status" = TRUE,
                                         "message" = NULL))
  expect_equal(length(all_reports), 1)
  #################################################################
  # Run report
  # delete old files
  report_fileadapter$cleanup(report = all_reports[[1]])
  test_run <-
    all_reports[[1]]$run(
      reportpackage_installation_path = file.path(test_folder_path, "files", "reportpackage_installations"),
      dataset_path = file.path(test_folder_path, "files", "datasets"),
      report_path = file.path(test_folder_path, "files", "reports")
    )
  expect_equal(test_run$folder,
               file.path(
                 test_folder_path,
                 "files",
                 "reports",
                 paste(
                   new_report_obj$dataset_md5sum,
                   new_report_obj$reportpackage_md5sum,
                   new_report_obj$config_md5sum,
                   sep = "_"
                 )
               ))
  finish_found <- F
  report_status <- NULL
  while (!finish_found) {
    Sys.sleep(1)
    report_status <-
      all_reports[[1]]$reportListen(folder = test_run$folder)
    print(report_status$progress)
    if (report_status$lastrun_status %in% c(0, 1)) {
      finish_found <- T
    }
  }
  expect_equal(finish_found, T)
  stopifnot(finish_found)
  all_reports[[1]]$update_self(arg_list = report_status[!names(report_status) %in% c("progress", "total")])
  download_files <-
    report_fileadapter$getFiles(report = all_reports[[1]])
  expect_equal(grep("^.*?\\.zip$", download_files), 1L)
  expect_equal(file.exists(download_files), T)
  dirpath <-
    report_fileadapter$getFilepath(report =  all_reports[[1]])
  delete1 <- report_fileadapter$delete(report = all_reports[[1]])
  delete2 <- report_adapter$deleteReport(report = all_reports[[1]])
  all_reports <- report_adapter$getReports()
  expect_equal(length(all_reports), 0)
  expect_equal(dir.exists(dirpath), F)
  expect_equal(delete1, T)
  expect_equal(delete2, list("status" = TRUE,
                             "message" = NULL))
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Report objects can be deleted when no report has been generated yet",{
  #########################
  test_folder_path <- normalizePath(testthat::test_path())
  eraseTestSetting(folder = test_folder_path)
  createTestSetting(folder = test_folder_path)
  createTestCases(folder = test_folder_path)
  connectionString <-
    sprintf(
      "DRIVER=SQLite3;Database=%s/files/testdb.sqlite;LongNames=0;Timeout=1000;NoTXN=0;SyncPragma=NORMAL;StepAPI=0;",
      test_folder_path
    )
  connection_obj <-
    Databaseconnector$new(connection_string = connectionString)
  connection <- connection_obj$getConnection()
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <-
    Datasetadapter$new(db_connection = connection,
                       dataset_validator = dataset_validator)
  reportpackage_fileadapter <-
    Reportpackagefileadapter$new(
      directory = file.path(test_folder_path, "files", "reportpackages"),
      install_directory =
        file.path(test_folder_path, "files", "reportpackage_installations")
    )
  reportpackage_validator <-
    Reportpackagevalidator$new(db_connection = connection,
                               fileadapter = reportpackage_fileadapter)
  reportpackage_adapter <-
    Reportpackageadapter$new(db_connection = connection,
                             reportpackage_validator = reportpackage_validator)
  reportpackage_validator <-
    Reportpackagevalidator$new(db_connection = connection,
                               fileadapter = reportpackage_fileadapter)
  reportpackage_adapter <-
    Reportpackageadapter$new(db_connection = connection,
                             reportpackage_validator = reportpackage_validator)
  report_fileadapter <-
    Reportfileadapter$new(directory = file.path(test_folder_path, "files", "reports"))
  report_validator <- Reportvalidator$new(
    db_connection = connection,
    dataset_adapter = dataset_adapter,
    reportpackage_adapter = reportpackage_adapter
  )
  report_adapter <- Reportadapter$new(
    db_connection = connection,
    report_validator = report_validator,
    fileadapter = report_fileadapter
  )
  #################################################################
  # create new report
  check <-
    report_validator$validate(
      dataset_md5sum = "90014426df46342419768278a12de3da",
      reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
      config = list("from_line" = "1", "to_line" =
                      "32"),
      report = NULL
    )
  expect_equal(check$status, T)
  expect_equal(check$message, "")
  expect_equal(R6::is.R6(check$properties$dataset), T)
  expect_equal(R6::is.R6(check$properties$reportpackage), T)
  expect_equal(check$properties$config,
               list("from_line" = 1L, "to_line" = 32L))
  expect_equal(check$properties$config_md5sum,
               "2d59500463800e78aced996369aab8e9")
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
    dataset_md5sum = "90014426df46342419768278a12de3da",
    reportpackage_md5sum = "3a46ef76bc54575bbce4405d37a45082",
    config_md5sum = check$properties$config_md5sum,
    db_connection = connection,
    validator = report_validator,
    fileadapter = report_fileadapter,
    adapter = report_adapter
  )
  create_new_folder <-
    report_fileadapter$create(report = new_report_obj)
  create_new_db_entry <-
    report_adapter$createReport(report = new_report_obj)
  all_reports <- report_adapter$getReports()
  expect_equal(dir.exists(report_fileadapter$getFilepath(report = new_report_obj)), T)
  expect_equal(create_new_folder, T)
  expect_equal(create_new_db_entry, list("status" = TRUE,
                                         "message" = NULL))
  expect_equal(length(all_reports), 1)
  #################################################################
  dirpath <-
    report_fileadapter$getFilepath(report = new_report_obj)
  delete1 <-
    report_fileadapter$delete(report = all_reports[[1]])
  delete2 <-
    report_adapter$deleteReport(report = all_reports[[1]])
  all_reports <- report_adapter$getReports()
  expect_equal(length(all_reports), 0)
  expect_equal(dir.exists(dirpath), F)
  expect_equal(delete1, T)
  expect_equal(delete2, list("status" = TRUE,
                             "message" = NULL))
  eraseTestSetting(folder = test_folder_path)
  #########################
})
