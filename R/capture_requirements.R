#' Capture packages in use
#'
#' @description
#' This function creates a dependencies metadata `requirements.txt` file following the minimal conda compatible naming convention for package version definition:
#' `channel::package=version`
#' It can be used for creating a conda environment with `conda create env --name tinyverse --file  requirements.txt`
#'
#' @param file A path to the filename for saving the `requirements.txt`. The default is `_requirements.txt`.
#' @param write Boolean, `TRUE` or `FALSE`. If `TRUE` the function write the list of conda formatted dependencies in a file. If `FALSE`, it returns the dataframe with the package metadata.
#' @param attached Boolean, `TRUE` or `FALSE`. If `TRUE` the function returns only the attached packages. If `FALSE`, all the packages present in the workspace are included. Default is `TRUE`.
#'
#' @return A dataframe or a text file with conda formatted package metadata
#' @export
#'
#' @examples capture_requirements(write = FALSE, attached = TRUE)
capture_requirements <- function(file = "_requirements.txt", write = FALSE, attached = TRUE) {

  # Defense, you must

  if( (!(is.null(file))) && write == FALSE){
    warning(crayon::yellow(paste("The 'file' argument is defined but 'write' is equal to FALSE.\n"),
                           "Set both filename and 'write = TRUE' to write captured requirements in a file.\n",
                           "If 'write = FALSE', requirements metadata will be returned as a dataframe object."))
  }

  ## Do not allow user to overwrite existing file
  ### params.file
  if (file.exists(file)  && write == TRUE) {
    stop(crayon::red(paste0("Prevented file overwrite\nA file named '",
                file,
                " 'already exists at: ",
                file)    ,
                "\nDefine a new filename in the 'file = ' argument."))
  }

  ## Make sure the user is explicit and only uses real boolean values (oomf! coeRcion!)
  ### params.attached
  if (  ! ((( isTRUE(attached) && is.logical(attached))) || ((!( isTRUE(attached)) && is.logical(attached)))) ) {
    stop("The argument 'attached' accepts only boolean, TRUE or FALSE")
  }

  ### params.write
  if (  ! ((( isTRUE(write) && is.logical(write))) || ((!( isTRUE(write)) && is.logical(write)))) ) {
    stop("The argument 'write' accepts only boolean, TRUE or FALSE")
  }

  # Capture dependencies
  sessionmetadata <- devtools::session_info()
  libs <- as.data.frame(sessionmetadata$packages)

  ## well, do you have any dependencies?
  if (nrow(libs) == 0){
    stop("No packages loaded in this workspace - You have no dependencies to care about!")
  }

  # initialise column 'channel'
  libs$channel <- ""


  # Locally developped R packages  - will not have adequate metadata so remove and warn user
  local_packages <- packages <-  libs[ (  ((is.na(libs$source))) | (libs$source == 'local')) , ]

  ## singular package
  if ( nrow(packages) == 1) {
    packages_names <- paste(packages$package, collapse = ",")
    warning(paste0(nrow(packages), " locally developed package will not be included in the requirements:\n",
                   "package name: ",
                   crayon::yellow(packages_names)))
    libs <- libs[ !(libs$source %in% local_packages$source), ]
  }

  ## plural packageS
  if (nrow(packages) > 1 ){
    packages_names <- paste(packages$package, collapse = ",")
    warning(paste0(nrow(packages), " locally developed packages will not be included in the requirements:\n",
                   "package names: ",
                   crayon::yellow(packages_names)))
    libs <- libs[ !(libs$source %in% local_packages$source), ]
  }


  # GitHub R packages - Cannot install via conda unless built from skeleton, so remove and warn
  github_packages <- packages <- libs[ (stringr::str_detect(libs$source, 'Git*')), ]

  ## singular package
  if (nrow(packages) == 1){
    packages_names <- paste(packages$package, collapse = ", ")
    warning(paste(nrow(packages),
                  " GitHub hosted R package will not be included in the requirements:\n",
                   "package name:",
                  crayon::yellow(packages_names)))
    libs <- libs[ (stringr::str_detect(libs$source, 'Git*')), ]
  }

  ## plural packageS
  if (nrow(packages) > 1 ){
    packages_names <- paste(packages$package, collapse = ", ")
    warning(paste(nrow(packages),
                   " GitHub hosted R developed packages will not be included in the requirements:\n",
                   "package names: ",
                  crayon::yellow(packages_names)))
    libs <- libs[ (stringr::str_detect(libs$source, 'Git*')), ]
  }


  # Create channel variable
  libs$channel[stringr::str_detect(libs$source, 'Biocon*')] <- 'bioconda::'
  libs$channel[stringr::str_detect(libs$source, 'CRAN*')]   <- ''

  # Format source variable (conda metadata naming convention for package name prefix)
  libs$source[stringr::str_detect(libs$source, 'Biocon*')] <- 'bioconductor-'
  libs$source[stringr::str_detect(libs$source, 'CRAN*')]   <- 'r-'

  # Lowercase them all!
  libs$package <- tolower(libs$package)

  # Create (channel::)package=version string
  libs$conda_packages <- paste0(libs$channel, libs$source, libs$package,  '=', libs$loadedversion)
  attached_only <- libs[libs$attached == TRUE, ]

  # Return based on user defined parameters

  ## Return only attached dependencies (aka R packages)
  if (attached){
    libs <- attached_only
  }

  ## well, do you have any dependencies after filtering?
  if (nrow(libs) == 0){
    return(message(crayon::bold("No CRAN or GitHub packages loaded in this workspace, check warnings for more information\n")))
  }

  ## Return a dataframe with the captured dependencies
  if (!(write)) {
    return(libs)
  }

  ## Write a file with the captured dependencies
  if (write) {
    data.table::fwrite(libs[, "conda_packages", drop = FALSE],
                       file      = file,
                       quote     = FALSE,
                       col.names = FALSE )
    message(crayon::green(paste0("\nPika pi! A conda style 'requirements.txt' file has been generated can be found at '", file, "' !\n")))
  }
}
