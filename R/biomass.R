#' Convert CPUE to biomass
#' This function converts zooplankton CPUE to biomass for taxa with conversion equations.
#'
#' @inheritParams Zoopsynther
#' @param Biomass_mesomicro The biomass conversion table. The default is \code{\link{biomass_mesomicro}}
#'
#'

Zoopbiomass<-function(Sources,
                      Size_class,
                      Zoop = zooper::zoopComb,
                      Biomass_mesomicro=zooper::biomass_mesomicro) {

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

  biomass_join<-function(d, level){

    join_var<-dplyr::if_else(level=="Species", "Taxname", level)

    d%>%
      dplyr::left_join(dplyr::filter(Biomass_mesomicro, .data$Level==level)%>%
                         dplyr::select("Taxname", "Lifestage", "Mass_{level}":="Carbon_mass_micrograms"),
                       by=rlang::set_names(c("Lifestage", "Taxname"), c("Lifestage", join_var)))
  }
  if(any(c("Meso", "Micro")%in%unique(Zoop$SizeClass)) & any(c("Meso", "Micro")%in%Size_class)){
    Zoop<-Zoop%>%
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
      select(Taxname, a, b)

    Zoop<-Zoop%>%
      dplyr::filter(.data$SizeClass%in%c("Macro"))%>%
      dplyr::left_join(Biomass_macro, by="Taxname")#%>%
      #dplyr::mutate(.data$Mass=1)
  }


  removed<-Zoop%>%
    dplyr::filter(is.na(.data$Mass))%>%
    dplyr::pull(.data$Taxlifestage)%>%
    unique()

  print(paste("The following taxlifestages were removed because biomass conversations were not available:",
              paste(removed, collapse=", ")))

  out<-list(removed=removed, zoop=Zoop)
  return(out)
}

