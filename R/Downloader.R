#' Download files and retry if failed
#'
#' This function will try 3 times to download a file
#'
#' @inheritParams utils::download.file
#' @inheritDotParams utils::download.file
#' @keywords internal
#'

Downloader <- function(url, ...){
try_number <- 1
  try_download<-function(url, ...){
    tryCatch(utils::download.file(url, ...),
                error = function(e) paste(e)
    )}
  out<-"Error"
while(rlang::is_string(out) & stringr::str_starts(out, "Error") & try_number <= 3){
  try_number <- try_number  + 1
  out <- try_download(url, ...)

}
  if(rlang::is_string(out) & stringr::str_starts(out, "Error")){
    stop(paste("Download of", url, "failed after 3 tries.", "Error message: ", out))
  }
  }
