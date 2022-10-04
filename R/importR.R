#' @title Import a Dataset
#'
#' @description
#' This \code{import} function can import data from
#' delimited text files, Excel spreadsheets and statistical packages
#' such as SAS, SPSS, and STATA
#'
#' @details
#' The import function is a wrapper for the
#' \href{https://haven.tidyverse.org/}{haven},
#' \href{https://readxl.tidyverse.org/}{readxl},
#' and  \href{https://github.com/r-lib/vroom}{vroom} packages.
#'
#' @seealso
#' \link[haven]{read_sas},
#' \link[haven]{read_dta},
#' \link[haven]{read_spss},
#' \link[readxl]{read_excel},
#' \link[vroom]{vroom}
#'
#'
#' @param file datafile to import
#' @param ...parameters passed to the import function
#'
#' @import haven
#' @import readxl
#' @import vroom
#' @import tools
#'
#' @export
#' @return a data frame
#'
#' @examples
#' \dontrun{
#' # import a comma delimited file
#' mydatafram <- import("mydata.csv")
#'
#' # import a SAS binary file
#' mydataframe <- import("mydata.sas7ndat")
#'
#' # import the second worksheet of an excel workbook
#' mydataframe <- import("mydata.xlsx",sheet=2)
#'
#' # prompt for a file to import
#' my dataframe <- import()
#'
#'}
#'

import <- function(file){

  if (missing(file)){
    file <- file.choose()
  }

  if (!file.exists(file)){
    stop("File doesn't exist! Please choose another file")
  }

  extension <- tools::file_ext(file)
  extension <- tolower(extension)

  if(extension == "xls" | extension == "xlsx"){
    dataset <- readxl::read_excel(file)
  }

  else if(extension == "sas7bdat") {
    if(!require("haven")){
      answer <- readline(prompt = "You need the haven package. Would you like to install it (y/n)? ")
      if (answer == "y"){
        install.packages("haven")
      } else {
        note("no file imported")
        return()
      }

    }
    dataset <- haven::read_sas(file)
  }

  else if(extension =="sav") {
    dataset <- haven::read_sav(file)
  }

  else if(extension == "dta") {
    dataset <- haven::read_dta(file)
  }

  else {
    if(!require("vroom")){
      print("You do not have the needed vroom package so we will install it for you!")
      install.packages("vroom")
    }
    dataset<- vroom::vroom(file)
  }

  return(dataset)
}
