#' @export
Reportpackagefileadapter <-
  R6::R6Class(
    classname = "Reportpackagefileadapter",
    public = list(
      initialize = function(directory, install_directory) {
        private$directory <- directory
        private$install_directory <- install_directory
        return(invisible(self))
      },
      unpack = function(package_file, directory) {
        return (tryCatch({
          untar_job <- untar(package_file,
                             exdir = directory)
          return(ifelse(
            !is.integer(untar_job) || !identical(untar_job, 0L),
            F,
            directory
          ))
        }, error = function(e) {
          return(FALSE)
        }))
      },
      installPackage = function(package_file,
                                package_name,
                                version,
                                directory = private$install_directory) {
        checkSystemFeedback <- function(value, domain) {
          status <- attr(value, "status")
          if (identical(status, 0) ||
              is.null(status)) {
            return(T)
          }
          return(sprintf("Error during %s: %s", domain, as.character(value)))
        }
        return (tryCatch({
          project <- paste0(package_name, "_", version)
          targzfile <- package_file
          project_path <-
            file.path(directory, project)
          exdir_path <-
            file.path(directory, package_name)
          lib_path <-
            file.path(directory, project, "lib")
          if (file.exists(project_path)) {
            unlink(project_path,
                   recursive = T)
          }
          untar(targzfile, exdir = directory)
          file.rename(from = exdir_path,
                      to = project_path)
          fileConn <-
            file(description = file.path(project_path, ".Rprofile"),
                 open = "w")
          writeLines(
            c(
              ".First <- function(){",
              ".libPaths(append('./lib',.libPaths()))",
              "options('repos'='https://cran.rstudio.com/')",
              "}"
            ),
            fileConn
          )
          close(fileConn)
          check <-
            checkSystemFeedback(value = system(sprintf("cd %s; ls; R CMD build .", project_path), intern = T),
                                domain = "package building")
          if (!identical(check, T)) {
            stop(check)
          }
          dir.create(lib_path)
          check <-
            checkSystemFeedback(value = system(
              sprintf(
                "cd %s; Rscript -e \"install.packages('devtools')\" ",
                project_path
              ),
              intern = T
            ),
            domain = "devtools installation")
          if (!identical(check, T)) {
            stop(check)
          }
          check <-
            checkSystemFeedback(value = system(
              sprintf(
                "cd %s; Rscript -e \"devtools::install_deps()\" ",
                project_path
              ),
              intern = T
            ),
            domain = "package dependencies installation")
          if (!identical(check, T)) {
            stop(check)
          }
          check <-
            checkSystemFeedback(value = system(
              sprintf("cd %s; Rscript -e \"devtools::install()\" ", project_path),
              intern = T
            ),
            domain = "package self-installation")
          if (!identical(check, T)) {
            stop(check)
          }
          unlink(file.path(
            project_path,
            paste0(package_name, "_", version, ".tar.gz")
          ))
          check <-
            checkSystemFeedback(value = system(
              sprintf(
                "cd %s; Rscript -e \"library(%s)\" ",
                project_path,
                package_name
              ),
              intern = T
            ),
            domain = "package loading")
          if (!identical(check, T)) {
            stop(check)
          }
          return(T)
        }, error = function(e) {
          return(as.character(e))
        }, finally = function() {

        }))
      },
      uninstallPackage = function(package_name,
                                  version,
                                  directory = private$install_directory) {
        project <- paste0(package_name, "_", version)
        project_path <- file.path(directory, project)
        if (file.exists(project_path)) {
          unlink(project_path,
                 recursive = T)
        }
        return(T)
      },
      getFile = function(reportpackage) {
        return(private$getFilepath(reportpackage$filename))
      },
      createFile = function(reportpackage, file) {
        filepath_reportpackage <-
          private$getFilepath(reportpackage$filename)
        if (!file.exists(filepath_reportpackage)) {
          file.copy(from = file,
                    to = filepath_reportpackage)
        }
        return(invisible(self))
      },
      updateFile = function(reportpackage, file, prior_filename =
                              NULL) {
        self$createFile(reportpackage = reportpackage, file = file)
        if (!is.null(prior_filename) &&
            prior_filename != reportpackage$filename) {
          self$deleteFile(filename = prior_filename)
        }
        return(invisible(self))
      },
      deleteFile = function(filename) {
        unlink(private$getFilepath(filename))
        return(invisible(self))
      },
      deleteFileByReportpackage = function(reportpackage) {
        unlink(private$getFilepath(reportpackage$filename))
        return(invisible(self))
      },
      print = function(...) {
        cat(str(self))
        invisible(self)
      }
    ),
    private = list(
      directory = NULL,
      install_directory = NULL,
      getFilepath = function(filename) {
        return(paste0(private$directory, "/", filename))
      }
    )
  )
