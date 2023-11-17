#' Convert CPUE to biomass
#' This function converts zooplankton CPUE to biomass for taxa with conversion equations.
#'
#' @inheritParams Zoopsynther
#' @param Biomass_mesomicro The micro and meso zooplankton biomass conversion table. The default is \code{\link{biomass_mesomicro}}
#' @param Biomass_macro The macro zooplankton biomass conversion table. The default is \code{\link{biomass_mesomicro}}
#'

Zoopbiomass<-function(Sources,
                      Size_class,
                      Zoop = zooper::zoopComb,
                      Biomass_mesomicro=zooper::biomass_mesomicro,
                      Biomass_macro=zooper::biomass_macro) {

  #Warnings for improper arguments
  if("YBFMP" %in% Sources){
    stop("YBFMP data cannot be converted to biomass with this function due to taxonomic and life stage issues with that dataset.")
  }

  if (!purrr::every(Sources, ~.%in%c("EMP", "FRP", "FMWT", "STN", "20mm", "DOP"))){
    stop("Sources must contain one or more of the following options: EMP, FRP, FMWT, STN, 20mm, DOP")
  }

  if (!purrr::every(Size_class, ~.%in%c("Micro", "Meso", "Macro"))){
    stop("Size_class must contain one or more of the following options: Micro, Meso, Macro")
  }

  if ("Macro"%in%Size_class & !"EMP"%in%Sources){
    stop("Macro zooplankton biomass conversion is currently only available for EMP")
  }

  if ("Macro"%in%Size_class & any(c("FRP", "FMWT", "STN", "DOP")%in%Sources)){
    message("Note that macro zooplankton biomass conversion is currently only available for EMP, so biomass will not be returned for other surveys")
  }

  zoop_list<-list()

  biomass_join<-function(d, level){

    join_var<-dplyr::if_else(level=="Species", "Taxname", level)

    d%>%
      dplyr::left_join(dplyr::filter(Biomass_mesomicro, .data$Level==level)%>%
                         dplyr::select("Taxname", "Lifestage", "Mass_{level}":="Carbon_mass_micrograms"),
                       by=rlang::set_names(c("Lifestage", "Taxname"), c("Lifestage", join_var)))
  }
  if(any(c("Meso", "Micro")%in%unique(Zoop$SizeClass)) & any(c("Meso", "Micro")%in%Size_class)){
    zoop_list[["MesoMicro"]]<-Zoop%>%
      dplyr::filter(.data$SizeClass%in%intersect(Size_class, c("Meso", "Micro")))%>%
      biomass_join("Phylum")%>%
      biomass_join("Class")%>%
      biomass_join("Order")%>%
      biomass_join("Family")%>%
      biomass_join("Genus")%>%
      biomass_join("Species")%>%
      dplyr::mutate(Mass=dplyr::case_when(
        !is.na(.data$Mass_Species) ~ .data$Mass_Species,
        !is.na(.data$Mass_Genus) ~ .data$Mass_Genus,
        !is.na(.data$Mass_Family) ~ .data$Mass_Family,
        !is.na(.data$Mass_Order) ~ .data$Mass_Order,
        !is.na(.data$Mass_Class) ~ .data$Mass_Class,
        !is.na(.data$Mass_Phylum) ~ .data$Mass_Phylum,
        TRUE ~ NA_real_))
  }

  if("Macro"%in%unique(Zoop$SizeClass) & "Macro"%in%Size_class){
    Biomass_macro<-Biomass_macro%>%
      dplyr::filter(Preservative=="Formalin" & Weight_type=="Dry")%>%
      dplyr::select("Taxname", "a", "b")

    zoop_list[["Macro"]]<-Zoop%>%
      dplyr::filter(.data$SizeClass%in%c("Macro"))%>%
      dplyr::left_join(zoopLengths%>%
                         dplyr::left_join(Biomass_macro, by="Taxname")%>%
                         dplyr::filter(!is.na(a))%>%
                         dplyr::mutate(Mass=0.4*(.data$a*.data$Length^.data$b))%>%
                         dplyr::group_by(.data$SampleID, .data$Taxlifestage)%>%
                         dplyr::summarise(Mass=sum(.data$Mass)),
                       by="Taxlifestage")%>%
      dplyr::mutate(Mass=dplyr::case_when(
        .data$Taxname%in%unique(Biomass_macro$Taxname) &
          .data$Source%in%unique(zoopLengths$Source) &
          .data$CPUE==0 ~ 0,
        !is.na(.data$Mass) ~ Mass,
        TRUE, NA_real_))
  }

Zoop<-dplyr::bind_rows(zoop_list)%>%
    dplyr::mutate(BPUE=.data$Mass/.data$Volume)

  removed<-Zoop%>%
    dplyr::filter(is.na(.data$Mass))%>%
    dplyr::pull(.data$Taxlifestage)%>%
    unique()

  cat(paste("The following taxlifestages were removed because biomass conversations were not available:",
            paste(removed, collapse=", ")))

  out<-list(removed=removed, zoop=Zoop)
  return(out)
}

