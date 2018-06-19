context("Dataset CRUD tests")

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
  query <-
    "INSERT into datasets (name, filename, rows, cols, created, last_updated, file_md5sum) VALUES (?, ?, ?, ?, ?, ?, ?)"
  RODBCext::sqlPrepare(connection, query)
  result <-
    RODBCext::sqlExecute(
      connection,
      NULL,
      data.frame(
        "name" = "test_dataset",
        "filename" =
          "3ce9a283c8ba0b8b4ca377bfadaa1f70.rds",
        "rows" =
          32,
        "cols" =
          11,
        "created" =
          1524049286,
        "last_updated" =
          1524049286,
        "file_md5sum" =
          "3ce9a283c8ba0b8b4ca377bfadaa1f70",
        stringsAsFactors = F
      ),
      fetch = T,
      rows_at_time = 1,
      believeNRows = FALSE
    )
  saveRDS(
    object = tibble::as.tibble(mtcars[, ]),
    file = file.path(folder, "files", "3ce9a283c8ba0b8b4ca377bfadaa1f70.rds")
  )
  return(TRUE)
}
eraseTestSetting <- function(folder) {
  unlink(file.path(folder, "files", "*"))
  return(TRUE)
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
test_that("Dataset objects can be read", {
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
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  datasets <- dataset_adapter$getDatasets()
  expect_equal(length(datasets), 1)
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Dataset objects can be validated and created", {
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
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  datasets <- dataset_adapter$getDatasets()
  temp_dir <- tempdir()
  temp_rds1 <- paste0(temp_dir, "/testfile1.RDS")
  test_name <- "test_dataset"
  saveRDS(object = as.data.frame(mtcars), file = temp_rds1)
  check1 <- dataset_validator$validateName(name = test_name)
  print(str(check1))
  expect_equal(list("status" = FALSE, "message" = "A dataset with that name already exists."),
               check1)
  check1b <-
    dataset_validator$validateName(name = test_name, dataset_update = datasets[[1]])
  print("check1b")
  print(check1b)
  expect_equal(list("status" = TRUE, "message" = ""), check1b)
  #####################################################
  temp_rds2 <- paste0(temp_dir, "/testfile2.RDS")
  saveRDS(object = tibble::as.tibble(mtcars[0, ]), file = temp_rds2)
  check2 <- dataset_validator$validateFile(filepath = temp_rds1)
  check3 <- dataset_validator$validateFile(filepath = temp_rds2)
  value <-
    list("status" = FALSE, "message" = "Invalid dataset file format. Non-empty RDS tibble object expected.")
  expect_equal(value, check2)
  expect_equal(value, check3)
  test_name <- "test_dataset$%"
  value <-
    list("status" = FALSE, "message" = "Invalid dataset name. Name may contain small and capital letters, numbers, underscore and dash.")
  check4 <- dataset_validator$validateName(name = test_name)
  expect_equal(check4, value)
  dataset_fileadapter <-
    Datasetfileadapter$new(directory = file.path(test_folder_path, "files"))
  # validate input
  test_name2 <- "valid_dataset"
  temp_rds3 <- paste0(temp_dir, "/testfile3.RDS")
  tibble_obj <- tibble::as.tibble(mtcars[1:10, ])
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
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Dataset objects can be validated and updated", {
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
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  datasets <- dataset_adapter$getDatasets()
  # create valid input
  test_name2 <- "valid_dataset"
  temp_dir <- tempdir()
  temp_rds3 <- paste0(temp_dir, "/testfile3.RDS")
  tibble_obj <- tibble::as.tibble(mtcars[1:10, ])
  saveRDS(object = tibble_obj, file = temp_rds3)
  check7 <- dataset_validator$validateName(name = test_name2)
  check8 <- dataset_validator$validateFile(filepath = temp_rds3)
  value <- list("status" = TRUE,
                "message" = "")
  expect_equal(check7, value)
  expect_equal(check8, value)
  occurrence_of_old_checksum <-
    dataset_validator$getFileChecksumOccurence(datasets[[1]])
  # old checksum is unique ==> delete old file; else keep it
  prior_filename <- NULL
  if (occurrence_of_old_checksum == 1) {
    prior_filename <- datasets[[1]]$filename
  }
  expect_equal(is.integer(occurrence_of_old_checksum), TRUE)
  expect_equal(occurrence_of_old_checksum, 1)
  datasets[[1]]$setName(name = test_name2)$setFile(file = temp_rds3)$setRows(rows = nrow(tibble_obj))$setCols(cols = ncol(tibble_obj))
  expect_equal(datasets[[1]]$name, test_name2)
  expect_equal(datasets[[1]]$filename, paste0(tools::md5sum(files = temp_rds3), ".rds"))
  expect_equal(datasets[[1]]$file_md5sum, tools::md5sum(files = temp_rds3))
  dataset_fileadapter <-
    Datasetfileadapter$new(directory = file.path(test_folder_path, "files"))
  dataset_fileadapter$updateFile(dataset = datasets[[1]],
                                 file = tibble_obj,
                                 prior_filename = prior_filename)
  expect_equal(file.exists(file.path(
    test_folder_path, "files", datasets[[1]]$filename
  )), T)
  # push to db
  dataset_adapter$updateDataset(dataset = datasets[[1]])
  ###########################################################################
  # create second dataset with identical checksum
  test_name2 <- "valid_dataset2"
  temp_dir <- tempdir()
  temp_rds3 <- paste0(temp_dir, "/testfile3.RDS")
  tibble_obj <- tibble::as.tibble(mtcars[1:10, ])
  saveRDS(object = tibble_obj, file = temp_rds3)
  check9 <- dataset_validator$validateName(name = test_name2)
  check10 <- dataset_validator$validateFile(filepath = temp_rds3)
  value <- list("status" = TRUE,
                "message" = "")
  expect_equal(check9, value)
  expect_equal(check10, value)
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
  ##########################################################################################
  # validate input
  test_name2 <- "valid_dataset3"
  temp_rds3 <- paste0(temp_dir, "/testfile3.RDS")
  tibble_obj <- tibble::as.tibble(mtcars[20:30, ])
  saveRDS(object = tibble_obj, file = temp_rds3)
  check11 <- dataset_validator$validateName(name = test_name2)
  check12 <- dataset_validator$validateFile(filepath = temp_rds3)
  value <- list("status" = TRUE,
                "message" = "")
  expect_equal(check11, value)
  expect_equal(check12, value)
  datasets <- dataset_adapter$getDatasets()
  occurrence_of_old_checksum <-
    dataset_validator$getFileChecksumOccurence(datasets[[2]])
  # old checksum is unique ==> delete old file; else keep it
  prior_filename <- NULL
  if (occurrence_of_old_checksum == 1) {
    prior_filename <- datasets[[2]]$filename
  }
  expect_equal(is.integer(occurrence_of_old_checksum), TRUE)
  expect_equal(occurrence_of_old_checksum, 2)
  datasets[[2]]$setName(name = test_name2)$setFile(file = temp_rds3)$setRows(rows = nrow(tibble_obj))$setCols(cols = ncol(tibble_obj))
  expect_equal(datasets[[2]]$name, test_name2)
  expect_equal(datasets[[2]]$filename, paste0(tools::md5sum(files = temp_rds3), ".rds"))
  expect_equal(datasets[[2]]$file_md5sum, tools::md5sum(files = temp_rds3))
  dataset_fileadapter <-
    Datasetfileadapter$new(directory = file.path(test_folder_path, "files"))
  dataset_fileadapter$updateFile(dataset = datasets[[2]],
                                 file = tibble_obj,
                                 prior_filename = prior_filename)
  expect_equal(datasets[[1]]$filename != datasets[[2]]$filename, T)
  expect_equal(file.exists(file.path(
    test_folder_path, "files", datasets[[2]]$filename
  )), T)
  expect_equal(file.exists(file.path(
    test_folder_path, "files", datasets[[1]]$filename
  )), T)
  # push to db
  dataset_adapter$updateDataset(dataset = datasets[[2]])
  ##########################################################################################
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Dataset objects can be deleted", {
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
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  dataset_fileadapter <-
    Datasetfileadapter$new(directory = file.path(test_folder_path, "files"))
  datasets <- dataset_adapter$getDatasets()
  if (dataset_validator$getFileChecksumOccurence(datasets[[1]]) == 1) {
    dataset_fileadapter$deleteFileByDataset(dataset = datasets[[1]])
  }
  dataset_adapter$deleteDataset(dataset = datasets[[1]])
  expect_equal(length(dataset_adapter$getDatasets()), 0)
  expect_equal(file.exists(file.path(
    test_folder_path, "files", datasets[[1]]$filename
  )), F)
  #################################################
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
  dataset_validator <-
    Datasetvalidator$new(db_connection = connection)
  dataset_adapter <- Datasetadapter$new(db_connection = connection,
                                        dataset_validator = dataset_validator)
  dataset_fileadapter <-
    Datasetfileadapter$new(directory = file.path(test_folder_path, "files"))
  # create second dataset with identical checksum
  test_name2 <- "valid_dataset2"
  temp_dir <- tempdir()
  temp_rds3 <- paste0(temp_dir, "/testfile3.RDS")
  tibble_obj <- tibble::as.tibble(mtcars[, ])
  saveRDS(object = tibble_obj, file = temp_rds3)
  check9 <- dataset_validator$validateName(name = test_name2)
  check10 <- dataset_validator$validateFile(filepath = temp_rds3)
  value <- list("status" = TRUE,
                "message" = "")
  expect_equal(check9, value)
  expect_equal(check10, value)
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
  datasets <- dataset_adapter$getDatasets()
  if (dataset_validator$getFileChecksumOccurence(datasets[[2]]) == 1) {
    dataset_fileadapter$deleteFileByDataset(dataset = datasets[[2]])
  }
  dataset_adapter$deleteDataset(dataset = datasets[[2]])
  expect_equal(length(dataset_adapter$getDatasets()), 1)
  expect_equal(file.exists(file.path(
    test_folder_path, "files", datasets[[1]]$filename
  )), T)
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
