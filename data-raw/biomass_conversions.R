## code to prepare `biomass_conversions` dataset goes here
require(readr)
require(dplyr)

biomass_mesomicro<-read_csv("data-raw/biomass_mesomicro.csv",
                            col_types = cols_only(Taxname="c", Level="c", Lifestage="c",
                                                  Carbon_mass_micrograms="d"))

biomass_macro<-read_csv("data-raw/biomass_macro.csv",
                            col_types = cols_only(Taxname="c", Level="c", Preservative="c",
                                                  Weight_type="c", Min_length="d", Max_length="d",
                                                  N="i", a="d", b="d"))

usethis::use_data(biomass_mesomicro, biomass_macro, overwrite = TRUE)
