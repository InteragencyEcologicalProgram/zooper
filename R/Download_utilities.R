#' Try a function call n times before failing
#'
#' This function will try n times to run a function call before failing. Mainly used for downloading files and accessing URLs
#'
#' @param n Number of times to try function before erroring
#' @param fun Function to try
#' @param ... parameters for function
#' @keywords internal
#' @importFrom magrittr %>%

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

html_file_list<-function(URL){

  html_file_extract<-function(URL){
    page <- rvest::read_html(URL)%>%
      rvest::html_elements("a")
    files<-rlang::set_names(x=rvest::html_attr(page, "href")%>%
                              rvest::url_absolute(URL),
                            nm=rvest::html_text(page))
    return(files)
  }

  files <- Tryer(n=3, fun=html_file_extract, URL=URL)

  return(files)
}

#' Create a list of entities from an EDI package
#'
#' This function lists all data entities for an EDI package
#'
#' @param PID EDI package ID
#' @keywords internal
#'

edi_entity_list<-function(PID){

  latest_revision <- paste("edi", PID, max(EDIutils::list_data_package_revisions("edi", PID)), sep=".")
  entities <- EDIutils::read_data_entity_names(packageId = latest_revision)
  entities<-purrr::set_names(c(entities$entityId, latest_revision), c(entities$entityName, "PID"))

  return(entities)
}

#' Extract latest EDI files
#' This function extracts the latest version of a zooplankton EDI package and the list of files from that package
#'
#' @inheritParams Zoopsynther
#' @return A list with the files and/or URLs for each source dataset
#' @author Sam Bashevkin
#' @export

zoop_urls<-function(Sources){

  if (!purrr::every(Sources, ~.%in%c("EMP", "FMWT", "STN",
                                     "20mm", "FRP", "YBFMP", "DOP"))){
    stop("Sources must contain one or more of the following options:
         'EMP', 'FMWT', 'STN','20mm', 'FRP', 'YBFMP', 'DOP'")
  }

  if(any(Sources%in%c("EMP", "STN", "FMWT", "YBFMP", "DOP", "FRP"))){
    check_EDI_cred()
  }

  out<-list()

  if("EMP"%in%Sources){
    EMP_entities<-edi_entity_list(522)

    out$EMP$Meso<-EMP_entities[grep("CBMatrix", names(EMP_entities))]
    out$EMP$Micro<-EMP_entities[grep("PumpMatrix", names(EMP_entities))]
    out$EMP$Macro<-EMP_entities[grep("MysidMatrix", names(EMP_entities))]
    out$EMP$Lengths<-EMP_entities[grep("Mysid_Length_Data", names(EMP_entities))]
    out$EMP$PID<-EMP_entities["PID"]
  }

  if(any(c("STN", "FMWT")%in%Sources)){
    FMWTSTN_entities<-edi_entity_list(1103)

    SMSCG_URL<-"https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/Zooplankton_SMSCG/"
    SMSCG_files<-html_file_list(SMSCG_URL)

    out$FMWTSTN$Meso<-FMWTSTN_entities[grep("FMWT_STN_CBNet", names(FMWTSTN_entities))]
    out$SMSCG$Meso<-SMSCG_files[grep("CBNet", SMSCG_files)]

    out$FMWTSTN$Macro<-FMWTSTN_entities[grep("MysidNetCPUE", names(FMWTSTN_entities))]
    out$SMSCG$Macro<-SMSCG_files[grep("MysidNet", SMSCG_files)]

    out$FMWTSTN$PID<-FMWTSTN_entities["PID"]
  }

  if("20mm"%in%Sources){
    twentymm_URL<-"https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/"
    twentymm_files<-html_file_list(twentymm_URL)

    out$twentymm$Meso<-twentymm_files[grep("Zooplankton%20Catch%20Matrix", twentymm_files)]
  }

  if("YBFMP"%in%Sources){
    YBFMP_entities<-edi_entity_list(494)

    out$YBFMP$Meso<-YBFMP_entities[grep("Zooplankton Data", names(YBFMP_entities))]

    out$YBFMP$PID<-YBFMP_entities["PID"]

  }

  if("DOP"%in%Sources){
    DOP_entities<-edi_entity_list(1187)

    out$DOP$Meso<-DOP_entities[grep("Mesozooplankton_Abundance", names(DOP_entities))]

    out$DOP$Macro<-DOP_entities[grep("Macrozooplankton_Abundance", names(DOP_entities))]

    out$DOP$trawls<-DOP_entities[grep("TowData", names(DOP_entities))]

    out$DOP$PID<-DOP_entities["PID"]

  }

  if("FRP"%in%Sources){
    FRP_entities<-edi_entity_list(269)

    out$FRP$Meso<-FRP_entities[grep("zoops_FRP", names(FRP_entities))]

    out$FRP$Macro<-FRP_entities[grep("macroinvert_FRP", names(FRP_entities))]

    out$FRP$site<-FRP_entities[grep("sitevisit_FRP", names(FRP_entities))]

    out$FRP$PID<-FRP_entities["PID"]
  }

  return(out)
}

#' check for EDI credentials
#'
#' Check if EDI credentials are properly set, and return a message if not
#'
#' @noRd

check_EDI_cred<-function(){
  #borrowed from https://github.com/ropensci/EDIutils/blob/main/tests/testthat/helper-test-package.R
  # Identical to function in deltafish
  has_token <- (Sys.getenv("EDI_TOKEN") != "") && (Sys.getenv("EDI_TOKEN") != "foobar")
  has_key <- (Sys.getenv("EDI_API_KEY") != "") && (Sys.getenv("EDI_API_KEY") != "foobar")

  if(!has_token && !has_key){
    stop("An EDI API key is now required by EDI. To resolve this:\n",
         "1) Please log in or create an account at https://auth.edirepository.org/ \n",
         "2) Access your API key from your profile -> Access Keys.\n",
         "3) You can then use the key either with EDIutils::login() ",
         "or by adding it to your .Renviron, e.g., by running usethis::edit_r_environ() ",
         "and, adding a line `EDI_API_KEY=YOUR_KEY`, saving, and restarting R.")
  }
}

#set options to allow access to EDI without signing in
#options(HTTPUserAgent="EDI_CodeGen")
