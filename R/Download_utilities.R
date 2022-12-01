#' Try a function call n times before failing
#'
#' This function will try n times to run a function call before failing. Mainly used for downloading files and accessing URLs
#'
#' @param n Number of times to try function before erroring
#' @param fun Function to try
#' @param ... parameters for function
#' @keywords internal
#' @importFrom magrittr %>%

library(dplyr)

Tryer <- function(n, fun, ...){
try_number <- 1
  tryer<-function(fun, ...){
    tryCatch(fun(...),
                error = function(e) paste(e)
    )}
  out<-"Error"
while(rlang::is_string(out) & all(stringr::str_starts(out, "Error")) & try_number <= n){

  if(try_number>1){
    Sys.sleep(2)
    }
  try_number <- try_number  + 1
  out <- tryer(fun, ...)

}
  if(rlang::is_string(out) & all(stringr::str_starts(out, "Error"))){
    fun_call<-stringr::str_replace_all(deparse1(substitute(fun(...)), backtick=FALSE), stringr::fixed('\"'), "'")
    stop(paste("The following function call failed after", n, "tries:", fun_call))
  }
  invisible(out)
}

#' Create a list of files from an HTML site
#'
#' This function lists all files from a given URL. It has only been tested for the CDFW filelib https://filelib.wildlife.ca.gov/Public/
#'
#' @param URL URL of the site
#' @keywords internal
#' @importFrom magrittr %>%
#'

library(dplyr)

html_file_list<-function(URL){

  html_file_extract<-function(URL){
    page <- rvest::read_html(URL)%>%
                    rvest::html_elements("a")
    files<-rlang::set_names(x=rvest::html_attr(page, "href")%>%
                              rvest::url_absolute(URL),
                            nm=rvest::html_text(page))
    return(files)
  }

  files <- Tryer(n=6, fun=html_file_extract, URL=URL)

  return(files)
}
