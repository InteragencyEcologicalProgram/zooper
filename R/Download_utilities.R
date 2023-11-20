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

  out<-list()

  if("EMP"%in%Sources){
    EMP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/522"
    EMP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=EMP_revision_url, warn = FALSE), 1)
    EMP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/522/", EMP_latest_revision)
    EMP_entities <- Tryer(n=3, fun=readLines, con=EMP_pkg_url, warn = FALSE)
    EMP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/522", EMP_latest_revision, EMP_entities, sep="/")
    names(EMP_entities) <- purrr::map_chr(EMP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

    out$EMP$Meso<-paste0(EMP_pkg_url, "/", EMP_entities["cb_matrix.csv"])
    out$EMP$Micro<-paste0(EMP_pkg_url, "/", EMP_entities["pump_matrix.csv"])
    out$EMP$Macro<-paste0(EMP_pkg_url, "/", EMP_entities["macro_matrix.csv"])
    out$EMP$Lengths<-paste0(EMP_pkg_url, "/", EMP_entities["macro_lengths.csv"])

  }

  if(any(c("STN", "FMWT")%in%Sources)){
    FMWTSTN_revision_url <- "https://pasta.lternet.edu/package/eml/edi/1103"
    FMWTSTN_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=FMWTSTN_revision_url, warn = FALSE), 1)
    FMWTSTN_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/1103/", FMWTSTN_latest_revision)
    FMWTSTN_entities <- Tryer(n=3, fun=readLines, con=FMWTSTN_pkg_url, warn = FALSE)
    FMWTSTN_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/1103", FMWTSTN_latest_revision, FMWTSTN_entities, sep="/")
    names(FMWTSTN_entities) <- purrr::map_chr(FMWTSTN_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

    SMSCG_URL<-"https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/Zooplankton_SMSCG/"
    SMSCG_files<-html_file_list(SMSCG_URL)

    out$FMWTSTN$Meso<-paste0(FMWTSTN_pkg_url, "/", FMWTSTN_entities["FMWT_STN_CBNetCPUE.csv"])
    out$SMSCG$Meso<-SMSCG_files[grep("CBNet", SMSCG_files)]

    out$FMWTSTN$Macro<-paste0(FMWTSTN_pkg_url, "/", FMWTSTN_entities["FMWT_MysidNetCPUE.csv"])
    out$SMSCG$Macro<-SMSCG_files[grep("MysidNet", SMSCG_files)]
  }

  if("20mm"%in%Sources){
    twentymm_URL<-"https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/"
    twentymm_files<-html_file_list(twentymm_URL)

    out$twentymm$Meso<-twentymm_files[grep("Zooplankton%20Catch%20Matrix", twentymm_files)]
  }

  if("YBFMP"%in%Sources){
    YBFMP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/494"
    YBFMP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=YBFMP_revision_url, warn = FALSE), 1)
    YBFMP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/494/", YBFMP_latest_revision)
    YBFMP_entities <- Tryer(n=3, fun=readLines, con=YBFMP_pkg_url, warn = FALSE)
    YBFMP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/494", YBFMP_latest_revision, YBFMP_entities, sep="/")
    names(YBFMP_entities) <- purrr::map_chr(YBFMP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

    out$YBFMP<-paste0(YBFMP_pkg_url, "/", YBFMP_entities["Zooplankton Data"])

  }

  if("DOP"%in%Sources){
    DOP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/1187"
    DOP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=DOP_revision_url, warn = FALSE), 1)
    DOP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/1187/", DOP_latest_revision)
    DOP_entities <- Tryer(n=3, fun=readLines, con=DOP_pkg_url, warn = FALSE)
    DOP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/1187", DOP_latest_revision, DOP_entities, sep="/")
    names(DOP_entities) <- purrr::map_chr(DOP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

    out$DOP$Meso<-paste0(DOP_pkg_url, "/", DOP_entities[grep("Mesozooplankton_Abundance", names(DOP_entities))])

    out$DOP$Macro<-paste0(DOP_pkg_url, "/", DOP_entities[grep("Macrozooplankton_Abundance", names(DOP_entities))])

    out$DOP$trawls<-paste0(DOP_pkg_url, "/", DOP_entities[grep("TowData", names(DOP_entities))])

  }

  if("FRP"%in%Sources){
    FRP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/269"
    FRP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=FRP_revision_url, warn = FALSE), 1)
    FRP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/269/", FRP_latest_revision)
    FRP_entities <- Tryer(n=3, fun=readLines, con=FRP_pkg_url, warn = FALSE)
    FRP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/269", FRP_latest_revision, FRP_entities, sep="/")
    names(FRP_entities) <- purrr::map_chr(FRP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

    out$FRP$Meso<-paste0(FRP_pkg_url, "/", FRP_entities[grep("zoops_FRP", names(FRP_entities))])

    out$FRP$Macro<-paste0(FRP_pkg_url, "/", FRP_entities[grep("macroinvert_FRP", names(FRP_entities))])

    out$FRP$site<-paste0(FRP_pkg_url, "/", FRP_entities[grep("sitevisit_FRP", names(FRP_entities))])
  }

  return(out)
}
