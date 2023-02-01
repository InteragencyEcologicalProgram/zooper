library(testthat)
library(zooper)

# Test skipping functions copied from deltafish
check_os_ci<-function(){
  ci<-isTRUE(as.logical(Sys.getenv("CI")))

  os<-tolower(Sys.info()[["sysname"]])

  out<-list(ci=ci, os=os)

  return(out)
}

skip_os_ci<-function(os, logical="or", ci="either"){
  if(any(!os%in%c("windows", "darwin", "linux"))){
    stop("os can only include 'windows', 'darwin', or 'linux'.")
  }

  if(!ci%in%c("ci", "local", "either")){
    stop("ci should be one of 'ci', 'local', or 'either'.")
  }

  if(!logical%in%c("or", "and")){
    stop("logical should be one of 'run' or 'ignore'.")
  }

  os_ci<-check_os_ci()

  if(!os_ci$os%in%c("windows", "darwin", "linux")){
    stop("This function is only designed to work on 'windows', 'darwin', or 'linux' operating systems.")
  }

  if(logical=="or"){
    log_fun<-`|`
  }else{
    log_fun=`&`
  }

  if(log_fun(os_ci$os%in%os, (ci=="either" | (os_ci$ci & ci=="ci") | (!os_ci$ci & ci=="local")))){
    return(invisible(TRUE)) # don't skip
  }

  # Otherwise skip
  msg<-paste0("Test only run when os is one of: ", paste(os, collapse=", "),  ifelse(ci!="either", paste0(" ", toupper(logical), " platform is ", ci), ""))
  testthat::skip(msg)
}

test_check("zooper")
