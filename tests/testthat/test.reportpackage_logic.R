context("Reportpackage CRUD tests")

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
                       "reportpackages"))
  dir.create(file.path(folder,
                       "files",
                       "reportpackage_installations"))
  test_file <- file.path(folder,
                         "files",
                         "reportpackages",
                         "testpackage001_0.1.0.tar.gz")
  file.copy(from = file.path(folder,
                             "repo",
                             "testpackage001_0.1.0.tar.gz"),
            to = test_file)
  md5checksum <- tools::md5sum(test_file)
  new_file_name <- paste0(md5checksum, ".tar.gz")
  file.rename(test_file,
              file.path(folder,
                        "files",
                        "reportpackages",
                        new_file_name))
  query <-
    "INSERT into reportpackages (name, version, filename, created, last_updated, file_md5sum) VALUES (?, ?, ?, ?, ?, ?)"
  RODBCext::sqlPrepare(connection, query)
  result <-
    RODBCext::sqlExecute(
      connection,
      NULL,
      data.frame(
        "name" = "testpackage001",
        "version" =
          "0.1.0",
        "filename" =
          new_file_name,
        "created" =
          1524049286,
        "last_updated" =
          1524049286,
        "file_md5sum" =
          md5checksum,
        stringsAsFactors = F
      ),
      fetch = T,
      rows_at_time = 1,
      believeNRows = FALSE
    )
  return(TRUE)
}
eraseTestSetting <- function(folder) {
  unlink(file.path(folder, "files", "*"), recursive = T)
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
test_that("Reportpackage objects can be read", {
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
  reportpackages <- reportpackage_adapter$getReportpackages()
  expect_equal(length(reportpackages), 1)
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Reportpackage objects can be validated and created", {
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
  reportpackages <- reportpackage_adapter$getReportpackages()
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/testpackage001_0.1.0.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "testpackage001_0.1.0.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(
    list(
      "status" = F,
      "message" = "An identical package already exists.",
      "properties" = list("package_name" = NULL, "package_version" = NULL)
    ),
    check1
  )
  ##########################################################################################
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/invalidarchive.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "invalidarchive.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  expect_equal(
    list(
      "status" = F,
      "message" = "Could not extract archive content. Please make sure to upload a valid archive file.",
      "properties" = list("package_name" = NULL, "package_version" = NULL)
    ),
    check1
  )
  ##########################################################################################
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/invalidfolderstructure.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "invalidfolderstructure.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(
    list(
      "status" = F,
      "message" = "File contains invalid structure: Package folder missing.",
      "properties" = list("package_name" = NULL, "package_version" = NULL)
    ),
    check1
  )
  ##########################################################################################
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/missingdescriptionfile.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "missingdescriptionfile.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(
    list(
      "status" = F,
      "message" = "DESCRIPTION file is missing.",
      "properties" = list("package_name" = NULL, "package_version" = NULL)
    ),
    check1
  )
  ##########################################################################################
  feedback <-
    list(
      "status" = F,
      "message" = "Could not detect package name or package version, or invalid name or version provided.",
      "properties" = list("package_name" = NULL, "package_version" = NULL)
    )
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/invalidpackagename.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "invalidpackagename.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(feedback, check1)
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/invalidpackageversion.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "invalidpackageversion.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(feedback, check1)
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/missingpackagename.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "missingpackagename.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(feedback, check1)
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/missingpackageversion.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "missingpackageversion.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(feedback, check1)
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/inconsistentpackagename.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "inconsistentpackagename.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(
    list(
      "status" = F,
      "message" = "Package name does not match package folder.",
      "properties" = list("package_name" = NULL, "package_version" = NULL)
    ),
    check1
  )
  ############################################################
  temp_dir <- tempdir()
  temp_file <- paste0(temp_dir, "/redundantnameandversion.tar.gz")
  file.copy(
    from = file.path(test_folder_path, "repo", "redundantnameandversion.tar.gz"),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(
    list(
      "status" = F,
      "message" = "There is already a package with identical name and version.",
      "properties" = list("package_name" = "testpackage001", "package_version" =
                            "0.1.0")
    ),
    check1
  )
  temp_dir <- tempdir()
  temp_file <-
    paste0(temp_dir, "/testpackage002invalidsetup_0.1.0.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "testpackage002invalidsetup_0.1.0.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(check1[["status"]], F)
  expect_equal(substr(check1[["message"]], 1, nchar("Package installation failed.")), "Package installation failed.")
  expect_equal(
    check1[["properties"]],
    list("package_name" = "testpackage002invalidsetup", "package_version" =
           "0.1.0")
  )
  temp_dir <- tempdir()
  temp_file <-
    paste0(temp_dir, "/testpackage003noMakeReport_0.1.0.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "testpackage003noMakeReport_0.1.0.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(
    list(
      "status" = F,
      "message" = "Package does not contain the makeReport() function. Make sure it is included in the package and stated in the NAMESPACE file as an exported function.",
      "properties" = list("package_name" = "testpackage003noMakeReport", "package_version" =
                            "0.1.0")
    ),
    check1
  )
  temp_dir <- tempdir()
  temp_file <-
    paste0(temp_dir, "/testpackage004allvalid_0.1.0.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "testpackage004allvalid_0.1.0.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(list(
    "status" = T,
    "message" = "",
    "properties" = list("package_name" = "testpackage004allvalid", "package_version" =
                          "0.1.0")
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
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Reportpackage objects can be validated and updated", {
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
  reportpackages <- reportpackage_adapter$getReportpackages()
  temp_dir <- tempdir()
  temp_file <-
    paste0(temp_dir, "/testpackage004allvalid_0.1.0.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "testpackage004allvalid_0.1.0.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(list(
    "status" = T,
    "message" = "",
    "properties" = list("package_name" = "testpackage004allvalid", "package_version" =
                          "0.1.0")
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
  reportpackages <- reportpackage_adapter$getReportpackages()
  # make sure update works
  reportpackage_update <- reportpackages[[2]]
  temp_dir <- tempdir()
  temp_file <-
    paste0(temp_dir, "/testpackage004allvalid_0.1.0.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "testpackage004allvalid_0.1.0.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file,
                                             except_reportpackage = reportpackage_update)
  expect_equal(
    check1,
    list(
      "status" = F,
      "message" = "An identical package already exists.",
      "properties" = list("package_name" = NULL, "package_version" = NULL)
    )
  )
  reportpackage_update <- reportpackages[[2]]
  temp_dir <- tempdir()
  temp_file <-
    paste0(temp_dir, "/identicalnameandversion_newcontent.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "identicalnameandversion_newcontent.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file,
                                             except_reportpackage = reportpackage_update)
  expect_equal(list(
    "status" = T,
    "message" = "",
    "properties" = list("package_name" = "testpackage004allvalid", "package_version" =
                          "0.1.0")
  ), check1)
  # uninstall package
  deinstallation <-
    reportpackage_fileadapter$uninstallPackage(package_name = reportpackages[[2]]$name,
                                               version = reportpackages[[2]]$version)
  expect_equal(deinstallation, T)
  now <- floor(as.numeric(Sys.time()))
  prior_filename <- reportpackages[[2]]$filename
  reportpackages[[2]]$setFile(filepath = temp_file)
  reportpackages[[2]]$setName(name = check1$properties$package_name)
  reportpackages[[2]]$setVersion(version = check1$properties$package_version)
  reportpackages[[2]]$last_updated <- now
  fileupdate <-
    reportpackage_fileadapter$updateFile(
      reportpackage = reportpackages[[2]],
      file = temp_file,
      prior_filename = prior_filename
    )
  installation <-
    reportpackage_fileadapter$installPackage(
      package_file = reportpackage_fileadapter$getFile(reportpackage = reportpackages[[2]]),
      package_name = reportpackages[[2]]$name,
      version = reportpackages[[2]]$version
    )
  expect_equal(installation, T)
  update_db <-
    reportpackage_adapter$updateReportpackage(reportpackage = reportpackages[[2]])
  expect_equal(update_db, list("status" = TRUE,
                               "message" = NULL))
  #####################
  reportpackages <- reportpackage_adapter$getReportpackages()
  reportpackage_update <- reportpackages[[2]]
  temp_dir <- tempdir()
  temp_file <-
    paste0(temp_dir, "/newnameandversion_newcontent.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "newnameandversion_newcontent.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file,
                                             except_reportpackage = reportpackage_update)
  expect_equal(list(
    "status" = T,
    "message" = "",
    "properties" = list("package_name" = "testpackage004new", "package_version" =
                          "0.1.2")
  ), check1)
  # uninstall package
  deinstallation <-
    reportpackage_fileadapter$uninstallPackage(package_name = reportpackages[[2]]$name,
                                               version = reportpackages[[2]]$version)
  expect_equal(deinstallation, T)
  now <- floor(as.numeric(Sys.time()))
  prior_filename <- reportpackages[[2]]$filename
  reportpackages[[2]]$setFile(filepath = temp_file)
  reportpackages[[2]]$setName(name = check1$properties$package_name)
  reportpackages[[2]]$setVersion(version = check1$properties$package_version)
  reportpackages[[2]]$last_updated <- now
  fileupdate <-
    reportpackage_fileadapter$updateFile(
      reportpackage = reportpackages[[2]],
      file = temp_file,
      prior_filename = prior_filename
    )
  installation <-
    reportpackage_fileadapter$installPackage(
      package_file = reportpackage_fileadapter$getFile(reportpackage = reportpackages[[2]]),
      package_name = reportpackages[[2]]$name,
      version = reportpackages[[2]]$version
    )
  expect_equal(installation, T)
  update_db <-
    reportpackage_adapter$updateReportpackage(reportpackage = reportpackages[[2]])
  expect_equal(update_db, list("status" = TRUE,
                               "message" = NULL))
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
test_that("Reportpackage objects can be deleted", {
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
  temp_file <-
    paste0(temp_dir, "/testpackage004allvalid_0.1.0.tar.gz")
  file.copy(
    from = file.path(
      test_folder_path,
      "repo",
      "testpackage004allvalid_0.1.0.tar.gz"
    ),
    to = temp_file
  )
  check1 <- reportpackage_validator$validate(package_file = temp_file)
  print(str(check1))
  expect_equal(list(
    "status" = T,
    "message" = "",
    "properties" = list("package_name" = "testpackage004allvalid", "package_version" =
                          "0.1.0")
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
  reportpackages <- reportpackage_adapter$getReportpackages()
  expect_equal(length(reportpackage_adapter$getReportpackages()), 2)
  # uninstall package
  deinstallation <-
    reportpackage_fileadapter$uninstallPackage(package_name = reportpackages[[2]]$name,
                                               version = reportpackages[[2]]$version)
  expect_equal(deinstallation, T)
  reportpackage_fileadapter$deleteFile(filename = reportpackages[[2]]$filename)
  del_file <-
    reportpackage_adapter$deleteReportpackage(reportpackage = reportpackages[[2]])
  expect_equal(del_file, list("status" = TRUE,
                              "message" = NULL))
  expect_equal(file.exists(
    file.path(
      test_folder_path,
      "files",
      "reportpackages",
      reportpackages[[2]]$filename
    )
  ), F)
  expect_equal(length(reportpackage_adapter$getReportpackages()), 1)
  # odbcCloseAll()
  eraseTestSetting(folder = test_folder_path)
  #########################
})
