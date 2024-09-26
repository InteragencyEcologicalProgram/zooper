#' Convert CPUE to biomass
#' This function converts zooplankton CPUE to carbon biomass (Carbon biomass per unit effort (\eqn{\mu}g/ \ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}})) for taxa with conversion equations.
#'
#' @param Zoop Zooplankton count dataset
#' @param ZoopLengths Zooplankton length dataset for macrozooplankton.
#' @param Biomass_mesomicro The micro and meso zooplankton biomass conversion table. The default is \code{\link{biomass_mesomicro}}
#' @param Biomass_macro The macro zooplankton biomass conversion table. The default is \code{\link{biomass_macro}}

Zoopbiomass<-function(Zoop,
                      ZoopLengths,
                      Biomass_mesomicro=zooper::biomass_mesomicro,
                      Biomass_macro=zooper::biomass_macro) {

  #Warnings for improper arguments

  Size_class<-unique(Zoop$SizeClass)

  if ("Macro"%in%Size_class & !"EMP"%in%unique(Zoop$Source)){
    stop("Macro zooplankton biomass conversion is currently only available for EMP")
  }

  if ("Macro"%in%Size_class & any(c("FRP", "FMWT", "STN", "DOP")%in%unique(Zoop$Source))){
    message("Note that macro zooplankton biomass conversion is currently only available for EMP, so macrozooplankton biomass will not be returned for other surveys")
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
        TRUE ~ NA_real_),
        BPUE=Mass*CPUE)
  }

  if("Macro"%in%unique(Zoop$SizeClass) & "Macro"%in%Size_class){
    Biomass_macro<-Biomass_macro%>%
      dplyr::filter(.data$Preservative=="Formalin" & .data$Weight_type=="Dry")%>%
      dplyr::select("Taxname", "a", "b")

    zoop_list[["Macro"]]<-Zoop%>%
      dplyr::filter(.data$SizeClass%in%c("Macro"))%>%
      dplyr::left_join(ZoopLengths%>%
                         dplyr::left_join(Biomass_macro, by="Taxname")%>%
                         dplyr::filter(!is.na(.data$a))%>%
                         # apply equation, then convert mg to ug and convert dry to carbon mass,
                         dplyr::mutate(Mass=0.001*0.4*(.data$a*.data$Length^.data$b)*.data$Count)%>%
                         dplyr::group_by(.data$SampleID, .data$Taxlifestage)%>%
                         dplyr::summarise(Mass=sum(.data$Mass), .groups="drop"),
                       by=c("Taxlifestage", "SampleID"))%>%
      dplyr::mutate(Mass=dplyr::case_when(
        .data$Taxname%in%unique(Biomass_macro$Taxname) &
          .data$Source%in%unique(ZoopLengths$Source) &
          .data$CPUE==0 ~ 0,
        !is.na(.data$Mass) ~ Mass,
        TRUE ~ NA_real_),
        BPUE=dplyr::if_else(.data$Volume==0, NA_real_, .data$Mass/.data$Volume))
  }

Zoop<-dplyr::bind_rows(zoop_list)

  nomass<-Zoop%>%
    dplyr::filter(is.na(.data$Mass))%>%
    dplyr::distinct(.data$Taxlifestage, .data$SizeClass)%>%
    dplyr::mutate(nomass=paste(.data$Taxlifestage, .data$SizeClass))%>%
    dplyr::pull(.data$nomass)%>%
    unique()

  Zoop<-Zoop%>%
    dplyr::select(-"Mass")

  cat(paste("Biomass could not be calculated for the following taxlifestages
            in the listed sizeclass because biomass conversations or lengths
            (for macrozooplankton) were not available:",
            paste(nomass, collapse=", ")))

  return(Zoop)
}

