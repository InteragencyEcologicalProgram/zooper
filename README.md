
# zooper <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

An R package to download and integrate zooplankton data from the
Sacramento San Joaquin Delta.

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("InteragencyEcologicalProgram/zooper")
```

# Introduction

The `Zoopsynther` function takes zooplankton data from different surveys
and integrates the data according to user-specified parameter choices.
The shiny app is a GUI (graphical user interface) that allows folks
without R experience to run the zooplankton synthesizer function and
download the resulting data. The shiny app (coming soon) also allows
folks with all experience levels to easily visualize the data.

The `Zoopdownloader` function downloads the zooplankton datasets from
their respective online sources and converts them to a consistent
format.

## Community or taxon-specific analyses?

The biggest problem with integrating zooplankton datasets is variability
in taxonomic resolution. To resolve this, we have developed 2 approaches
to consolidating inconsistent data to “least common denominator taxa.”
Depending on what type of analysis you wish to run, you may wish for
different types of synthesized data.

### For community data analyzers

*I want to analyze the community composition at whatever taxonomic level
lets me use all these datasets.*

  - Consistent taxonomic categories
  - No plankters counted more than once
  - Sacrifices some taxonomic resolution
  - Removes taxa with no relatives in all datasets (e.g., Annelida)

### For specific taxa analyzers

*I want all possible data on these specific taxa.*

  - Calculates total CPUE for higher taxonomic levels
  - Some plankters appear in multiple nested taxa (e.g., Calanoida,
    Copepoda)
  - Preserves taxonomic resolution and creates taxonomic categories that
    are comparable across all datasets
  - Labels taxa that are comparable across all datasets, warns about
    those that are not.

## Size classes

We have integrated zooplankton data from 3 net size classes:

1.  Macro (500-505
    ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m): Amphipods
    and mysids
2.  Meso (150 - 160
    ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m): Copepods,
    cladocera
3.  Micro (43 ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m):
    Copepods, rotifers

Nets accurately sample zooplankton larger than the mesh size.
Zooplankton smaller than the mesh size are still captured and often
recorded in datasets, but the resulting CPUEs are not accurate. To
account for this we:

1.  Resolve taxonomic resolution separately within each net size class.
2.  If `Data = 'Taxa'`, we mark “summed groups” with the net size class
    from which they were derived.
3.  All potentially undersampled data are marked with a flag
    `Undersampled == TRUE`
4.  For the plots in the shiny app, all data with `Undersampled == TRUE`
    can be removed. However, data downloaded from the app do contain
    undersampled data.

## Unresolved issues

For many studies, taxonomic resolution has changed over time. This could
confound analyses of zooplankton communities and abundances over time.

# Usage

``` r
library(zooper)
MyZoops <- Zoopsynther(Data_type = "Community", 
                       Sources = c("EMP", "FRP", "FMWT"), 
                       Size_class = "Meso", 
                       Date_range = c("1990-10-01", "2000-09-30"))
#> [1] "No disclaimers here! Enjoy the clean data!"

str(MyZoops)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    151478 obs. of  31 variables:
#>  $ Source      : chr  "EMP" "EMP" "EMP" "EMP" ...
#>  $ SizeClass   : chr  "Meso" "Meso" "Meso" "Meso" ...
#>  $ Volume      : num  7.91 7.91 7.91 7.91 7.91 ...
#>  $ Lifestage   : chr  "Adult" "Adult" "Adult" "Adult" ...
#>  $ Taxname     : chr  "Acanthocyclops vernalis" "Acartia" "Acartiella sinensis" "Asplanchna" ...
#>  $ Phylum      : chr  "Arthropoda" "Arthropoda" "Arthropoda" "Rotifera" ...
#>  $ Class       : chr  "Copepoda" "Copepoda" "Copepoda" "Eurotatoria" ...
#>  $ Order       : chr  "Cyclopoida" "Calanoida" "Calanoida" "Ploima" ...
#>  $ Family      : chr  "Cyclopidae" "Acartiidae" "Acartiidae" "Asplanchnidae" ...
#>  $ Genus       : chr  "Acanthocyclops" "Acartia" "Acartiella" "Asplanchna" ...
#>  $ Species     : chr  "Acanthocyclops vernalis" NA "Acartiella sinensis" NA ...
#>  $ Taxlifestage: chr  "Acanthocyclops vernalis Adult" "Acartia Adult" "Acartiella sinensis Adult" "Asplanchna Adult" ...
#>  $ SampleID    : chr  "EMP NZ098 1990-10-08" "EMP NZ098 1990-10-08" "EMP NZ098 1990-10-08" "EMP NZ098 1990-10-08" ...
#>  $ CPUE        : num  0 0 0 0 1315 ...
#>  $ Year        : num  1990 1990 1990 1990 1990 1990 1990 1990 1990 1990 ...
#>  $ Date        : POSIXct, format: "1990-10-08" "1990-10-08" ...
#>  $ Datetime    : POSIXct, format: NA NA ...
#>  $ Tide        : chr  "High slack" "High slack" "High slack" "High slack" ...
#>  $ Station     : chr  "NZ098" "NZ098" "NZ098" "NZ098" ...
#>  $ Chl         : num  1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 ...
#>  $ Secchi      : num  99 99 99 99 99 99 99 99 99 99 ...
#>  $ Temperature : num  19.5 19.5 19.5 19.5 19.5 19.5 19.5 19.5 19.5 19.5 ...
#>  $ BottomDepth : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ Turbidity   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ Microcystis : chr  NA NA NA NA ...
#>  $ pH          : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ DO          : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ SalSurf     : num  0.203 0.203 0.203 0.203 0.203 ...
#>  $ SalBott     : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ Latitude    : num  38 38 38 38 38 ...
#>  $ Longitude   : num  -122 -122 -122 -122 -122 ...
```

Here’s a graph you could make with the data

``` r
library(dplyr)
library(ggplot2)
library(RColorBrewer)
MyZoops%>%
  filter(!is.na(SalSurf))%>%
  mutate(Salinity_zone=case_when(
    SalSurf < 0.5 ~ "Freshwater",
    SalSurf > 0.5 & SalSurf < 6 ~ "Low salinity zone",
    SalSurf > 6 ~ "High salinity zone"
  ))%>%
  mutate(Salinity_zone=factor(Salinity_zone, levels=c("Freshwater", "Low salinity zone", "High salinity zone")))%>%
  group_by(Year,Phylum, Class, Order, Family, Genus, Species, Lifestage, Taxlifestage, Salinity_zone)%>%
  summarise(CPUE=mean(CPUE, na.rm=T))%>%
  ungroup()%>%
  arrange(Phylum, Class, Order, Family, Genus, Species, Lifestage)%>%
  mutate(Taxlifestage=factor(Taxlifestage, unique(Taxlifestage)))%>%
  ggplot(aes(x=Year, y=CPUE))+
  geom_bar(stat="identity", color="white", size=0.01, aes(fill=Taxlifestage))+
  facet_wrap(~Salinity_zone, nrow=1)+
  coord_cartesian(expand=0)+
  scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=3))), expand=c(0,0))+
  scale_fill_manual(values=sample(colorRampPalette(brewer.pal(12, "Set3"))(length(unique(MyZoops$Taxlifestage)))), 
                    name="Taxa and life stage", 
                    guide = guide_legend(ncol=3, title.position = "top", title.hjust = 0.5))+
  ylab(bquote(Average~CPUE~"("*Catch*"/"*m^3*")"))+
  theme_bw()+
  theme(panel.grid=element_blank(), text=element_text(size=14), legend.text = element_text(size=8), 
        legend.key.size = unit(10, "points"), strip.background=element_blank(), legend.position = "bottom", 
        legend.background = element_rect(color="black"), axis.text.x=element_text(angle=45, hjust=1))
```

<img src="man/figures/README-plots-1.png" width="100%" />

# Code of conduct

Please note that the ‘zooper’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
