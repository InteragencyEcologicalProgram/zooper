#' Try a function call n times before failing
#'
#' This function will try n times to run a function call before failing. Mainly used for downloading files and accessing URLs
#'
#' @param n Number of times to try function before erroring
#' @param fun Function to try
#' @param ... parameters for function
#' @keywords internal
#'

Tryer <- function(n, fun, ...){
try_number <- 1
  tryer<-function(fun, ...){
    tryCatch(fun(...),
                error = function(e) paste(e)
    )}
  out<-"Error"
while(rlang::is_string(out) & all(stringr::str_starts(out, "Error")) & try_number <= n){
  try_number <- try_number  + 1
  out <- tryer(fun, ...)

}
  if(rlang::is_string(out) & all(stringr::str_starts(out, "Error"))){
    fun_call<-stringr::str_replace_all(deparse1(substitute(fun(...)), backtick=FALSE), stringr::fixed('\"'), "'")
    stop(paste("The following function call failed after", n, "tries:", fun_call))
  }
  invisible(out)
}

#' Create a list of files on an FTP
#'
#' This function lists all files from a given FTP URL
#'
#' @param URL URL of the FTP site
#' @keywords internal
#'

ftp_file_list<-function(URL){
  con <- Tryer(n=3, fun=curl::curl, url = URL, "r",
               handle = curl::new_handle(dirlistonly = TRUE))
  on.exit(close(con))
  out<-Tryer(n=3, fun=readLines, con=con)
  return(out)
}
