#' @export
Reportpackagevalidator <-
  R6::R6Class(
    classname = "Reportpackagevalidator",
    public = list(
      initialize = function(db_connection, fileadapter) {
        private$db_connection <- db_connection
        private$fileadapter <- fileadapter
        return(invisible(self))
      },
      validate = function(package_file, except_reportpackage = NULL) {
        if (private$isPackageExisting(package_file = package_file)) {
          return(
            list(
              "status" = F,
              "message" = "An identical package already exists.",
              "properties" = list("package_name" = NULL, "package_version" = NULL)
            )
          )
        }
        temp_dir <-
          file.path(tempdir(), randomStrings(n = 1))
        dir.create(temp_dir)
        unpack_directory <-
          private$fileadapter$unpack(package_file = package_file, directory = temp_dir)
        if (unpack_directory == F) {
          return(
            list(
              "status" = F,
              "message" = "Could not extract archive content. Please make sure to upload a valid archive file.",
              "properties" = list("package_name" = NULL, "package_version" = NULL)
            )
          )
        }
        folder_check <-
          private$checkFolderStructure(unpack_directory)
        if (folder_check == F) {
          return(
            list(
              "status" = F,
              "message" = "File contains invalid structure: Package folder missing.",
              "properties" = list("package_name" = NULL, "package_version" = NULL)
            )
          )
        }
        description_content_vector <-
          private$getDescriptionFile(file.path(unpack_directory, folder_check))
        if (length(description_content_vector) == 1 &&
            description_content_vector == F) {
          return(list(
            "status" = F,
            "message" = "DESCRIPTION file is missing.",
            "properties" = list("package_name" = NULL, "package_version" = NULL)
          ))
        }
        check_package_name <-
          private$extractFromDescription(description_content_vector, what = "package")
        check_package_version <-
          private$extractFromDescription(description_content_vector, what = "version")
        valid_package_name <-
          private$isValidPackageName(name = check_package_name)
        valid_package_version <-
          private$isValidPackageVersion(version = check_package_version)
        if (any(
          c(
            check_package_name,
            check_package_version,
            valid_package_name,
            valid_package_version
          ) == F
        )) {
          return(
            list(
              "status" = F,
              "message" = "Could not detect package name or package version, or invalid name or version provided.",
              "properties" = list("package_name" = NULL, "package_version" = NULL)
            )
          )
        }
        if (!identical(check_package_name, folder_check)) {
          return(
            list(
              "status" = F,
              "message" = "Package name does not match package folder.",
              "properties" = list("package_name" = NULL, "package_version" = NULL)
            )
          )
        }
        if (private$isPackageNameVersionExisting(name = check_package_name,
                                                 version = check_package_version,
                                                 except_reportpackage = except_reportpackage)) {
          return(
            list(
              "status" = F,
              "message" = "There is already a package with identical name and version.",
              "properties" = list("package_name" = check_package_name, "package_version" =
                                    check_package_version)
            )
          )
        }
        temp_dir <-
          file.path(tempdir(), randomStrings(n = 1))
        dir.create(temp_dir)
        check_installation <-
          private$fileadapter$installPackage(
            package_file = package_file,
            package_name = check_package_name,
            version = check_package_version,
            directory = temp_dir
          )
        if (!identical(check_installation, T)) {
          return(list(
            "status" = F,
            "message" = sprintf(
              "Package installation failed. Here comes the error log: %s",
              check_installation
            ),
            "properties" = list("package_name" = check_package_name, "package_version" =
                                  check_package_version)
          ))
        }
        check_function <-
          private$checkMakeReportFunction(package_name = check_package_name,
                                          version = check_package_version,
                                          directory = temp_dir)
        if (!check_function) {
          return(
            list(
              "status" = F,
              "message" = "Package does not contain the makeReport() function. Make sure it is included in the package and stated in the NAMESPACE file as an exported function.",
              "properties" = list("package_name" = check_package_name, "package_version" =
                                    check_package_version)
            )
          )
        }
        return(list(
          "status" = T,
          "message" = "",
          "properties" = list("package_name" = check_package_name, "package_version" =
                                check_package_version)
        ))
      }
    ),
    private = list(
      db_connection = NULL,
      fileadapter = NULL,
      checkFolderStructure = function(directory) {
        all_files <- list.files(directory)
        if (length(all_files) != 1 ||
            !dir.exists(file.path(directory, all_files[1])) ||
            !private$isValidPackageName(all_files[1])) {
          return(FALSE)
        }
        return(all_files[1])
      },
      isValidPackageName = function(name) {
        # https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Creating-R-packages
        grepl(pattern = "^[A-Za-z]{1}[A-Za-z0-9\\.]*?[A-Za-z0-9]{1}$", name)
      },
      isValidPackageVersion = function(version) {
        return (tryCatch({
          numeric_version(version)
          return(TRUE)
        }, error = function(e) {
          return(FALSE)
        }))
      },
      getDescriptionFile = function(directory) {
        if (!"DESCRIPTION" %in% list.files(directory)) {
          return(FALSE)
        }
        con <-
          file(file.path(directory, "DESCRIPTION"), "r", blocking = FALSE)
        return_value <- readLines(con)
        close(con)
        return(return_value)
      },
      extractFromDescription = function(lines, what) {
        search_for <- list("package" = "Package",
                           "version" = "Version")
        pattern <-
          sprintf("^%s:\\s+(.+?)\\s*$", search_for[[what]])
        m <- regexec(pattern, lines)
        search_results <- regmatches(lines, m)
        str_extract <- FALSE
        sapply(search_results, function(x) {
          if (length(x) == 2 && !str_extract) {
            str_extract <<- x[2]
          }
        })
        return(str_extract)
      },
      isPackageExisting = function(package_file) {
        checksum <- tools::md5sum(package_file)
        values <- unname(c(checksum))
        query_addition <- ""
        query <-
          "SELECT reportpackage_id from reportpackages where file_md5sum = ?"
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
      },
      isPackageNameVersionExisting = function(name, version, except_reportpackage = NULL) {
        values <- c(name, version)
        query_addition <- ""
        if (!is.null(except_reportpackage)) {
          values <- c(values, except_reportpackage$id)
          query_addition <-
            " AND reportpackage_id != ?"
        }
        query <-
          paste0(
            "SELECT reportpackage_id from reportpackages where name = ? AND version = ?",
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
      },
      checkMakeReportFunction = function(package_name, version, directory) {
        wd <- file.path(directory, paste0(package_name, "_", version))
        return (tryCatch({
          com <-
            system(
              paste0(
                "cd ",
                wd,
                "; Rscript -e \"library(",
                package_name,
                "); stopifnot('makeReport'%in%ls('package:",
                package_name,
                "'))\""
              ),
              intern = T
            )
          attr_com <- attr(com, "status")
          if (!identical(attr_com, 0) &&
              !is.null(attr_com)) {
            stop()
          }
          return(TRUE)
        }, error = function(e) {
          return(FALSE)
        }))
      }
    )
  )
