skip_os_ci(os=c("darwin", "linux"), logical="or", ci="local")

library(zooper)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
require(stringr)

#Test on all datasets

test_that("Community option produces messages", {
  expect_output(com <<- Zoopsynther(Data_type="Community")%>%
                  summarise(N = nrow(.),
                            N_greater0 = nrow(filter(., CPUE>0)),
                            Source = list(unique(paste(Source, SizeClass, sep="_"))),
                            N_Volume_NA = length(which(is.na(Volume))),
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID)),
                            CPUE_total=sum(CPUE),
                            Taxa=list(select(., Taxlifestage, SizeClass, Undersampled)%>%distinct())), "These species have no relatives in their size class common to all datasets and have been removed from one or more size classes", all=TRUE)
  expect_output(comTime <<- Zoopsynther(Data_type="Community", Time_consistency = TRUE)%>%
                  summarise(N = nrow(.),
                            N_greater0 = nrow(filter(., CPUE>0)),
                            Source = list(unique(paste(Source, SizeClass, sep="_"))),
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID)),
                            CPUE_total=sum(CPUE),
                            Taxa=list(select(., Taxlifestage, SizeClass, Undersampled)%>%distinct())), "These species have no relatives in their size class common to all datasets and have been removed from one or more size classes", all=TRUE)

})

test_that("Taxa option produces messages", {
  expect_output(tax <<- Zoopsynther(Data_type="Taxa")%>%
                  summarise(N = nrow(.),
                            N_greater0 = nrow(filter(., CPUE>0)),
                            Source = list(unique(paste(Source, SizeClass, sep="_"))),
                            N_Volume_NA = length(which(is.na(Volume))),
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID)),
                            Taxa=list(select(., Taxlifestage, SizeClass, Undersampled)%>%distinct())), "[Some taxa were not measured in all datasets|Do not use this data to make additional higher]", all=TRUE)
})

Data_source <- c("EMP", "FMWT", "STN", "20mm", "FRP","DOP", "EMP", "FRP", "EMP", "FMWT", "STN", "DOP")
Size_class <- c(rep("Meso", 6), "Micro", rep("Macro", 5))

Data_sets <- paste(Data_source, Size_class, sep="_")

test_that("Community dataset is created and contains all sources", {
  expect_gt(com$N, 0)
  expect_gt(com$N_greater0, 0)
  expect_setequal(unlist(com$Source), Data_sets)
})

test_that("Community dataset with time consistency is created and contains all sources", {
  expect_gt(comTime$N, 0)
  expect_gt(comTime$N_greater0, 0)
  expect_setequal(unlist(comTime$Source), Data_sets)
})

test_that("Taxa dataset is created and contains all sources", {
  expect_gt(tax$N, 0)
  expect_gt(tax$N_greater0, 0)
  expect_setequal(unlist(tax$Source), Data_sets)
})

test_that("Community dataset contains no duplicated rows", {
  expect_equal(com$N, com$N_Taxsamples)
})

test_that("Community dataset contains no NA Volumes", {
  expect_equal(com$N_Volume_NA, 0)
})

test_that("Community dataset with time consistency contains no duplicated rows", {
  expect_equal(comTime$N, comTime$N_Taxsamples)
})

test_that("Taxa dataset contains no duplicated rows", {
  expect_equal(tax$N, tax$N_Taxsamples)
})

test_that("Taxa dataset contains no NA Volumes", {
  expect_equal(tax$N_Volume_NA, 0)
})

test_that("Community dataset with time consistency contains same total CPUE as normal community dataset", {
  expect_equal(comTime$CPUE_total, com$CPUE_total)
})


test_that("Taxa and community datasets contain same samples", {
  expect_setequal(unlist(tax$Samples), unlist(com$Samples))
})

# Test the Undersampled flag is correctly applied
Crosswalk_reduced<-zooper::crosswalk%>%
  select(c(Taxname, all_of(c("Genus", "Family", "Order", "Class", "Phylum")), Lifestage))%>%
  mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
  distinct()%>%
  select(-Lifestage, -Taxlifestage)%>%
  distinct()

#Create dataframe of undersampled taxa
Undersampled<-zooper::undersampled%>%
  left_join(Crosswalk_reduced, by="Taxname")%>%
  pivot_longer(cols=c(Phylum, Class, Order, Family, Genus, Taxname), names_to = "Level", values_to = "Taxa")%>%
  drop_na()%>%
  mutate(Taxlifestage = paste(Taxa, Lifestage),
                Undersampled = TRUE)%>%
  select(SizeClass, Taxlifestage, Undersampled)%>%
  distinct()

com_undersampled<-com$Taxa[[1]]%>%
  filter(!Undersampled)%>%
  select(-Undersampled)%>%
  left_join(Undersampled, by=c("SizeClass", "Taxlifestage"))

comTime_undersampled<-comTime$Taxa[[1]]%>%
  filter(!Undersampled)%>%
  select(-Undersampled)%>%
  left_join(Undersampled, by=c("SizeClass", "Taxlifestage"))

tax_undersampled<-tax$Taxa[[1]]%>%
  filter(!Undersampled)%>%
  select(-Undersampled)%>%
  left_join(Undersampled, by=c("SizeClass", "Taxlifestage"))

test_that("Community dataset has correctly labeled undersampled taxa", {
  expect_true(all(is.na(com_undersampled$Undersampled)))
})

test_that("Community dataset with time consistency has correctly labeled undersampled taxa", {
  expect_true(all(is.na(comTime_undersampled$Undersampled)))
})

test_that("Taxa dataset has correctly labeled undersampled taxa", {
  expect_true(all(is.na(tax_undersampled$Undersampled)))
})
# Test that zoopsynther community approach does not change overall CPUE

data_com<-Zoopsynther("Community", Shiny = T)
Removed<-unlist(str_split(str_split(data_com$Caveats, ", ")[[1]], ": "))[-1]
Removed_data<-tibble(Taxlifestage=str_remove(Removed, " \\s*\\([^\\)]+\\)"),
                      SizeClass=str_extract(Removed, " \\s*\\([^\\)]+\\)"))%>%
  mutate(SizeClass=str_remove(str_remove(SizeClass, fixed(" (")), fixed(")")))
Data_base_filtered<-zooper::zoopComb%>%
  anti_join(Removed_data, by=c("SizeClass", "Taxlifestage"))%>%
  filter(Source!="YBFMP")

test_that("Zoopsynther with community option and problematic taxa removed produces expected message", {
  expect_output(data_com_filtered<<-Zoopsynther("Community", Zoop=Data_base_filtered), "No disclaimers here")
})

Data_filtered<-data_com_filtered%>%
  group_by(SampleID)%>%
  summarise(CPUE_com=sum(CPUE), .groups="drop")%>%
  full_join(Data_base_filtered%>%
              group_by(SampleID)%>%
              summarise(CPUE_base=sum(CPUE, na.rm = T), .groups="drop"),
            by="SampleID")

test_that("Community approach does not change overall CPUE", {
  expect_equal(Data_filtered$CPUE_base, Data_filtered$CPUE_com)
})



test_that("Community option produces correct messages with a single source", {
  expect_output(comind <<- map2(Data_source, Size_class,
                                ~Zoopsynther(Data_type="Community", Sources=.x, Size_class = .y)%>%
                                  summarise(N = nrow(.),
                                            N_greater0 = nrow(filter(., CPUE>0)),
                                            Source = paste(unique(paste(Source, SizeClass, sep="_")), collapse = ", "),
                                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass))))%>%
                  enframe(name=NULL)%>%
                  unnest(value), "No disclaimers here", all=TRUE)
})

test_that("Taxa option produces correct messages with a single source", {
  expect_output(taxind <<- map2(Data_source, Size_class,
                                ~Zoopsynther(Data_type="Taxa", Sources=.x, Size_class = .y)%>%
                                  summarise(N = nrow(.),
                                            N_greater0 = nrow(filter(., CPUE>0)),
                                            Source = paste(unique(paste(Source, SizeClass, sep="_")), collapse = ", "),
                                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass))))%>%
                  enframe(name=NULL)%>%
                  unnest(value), "[No orphaned taxa here|Do not use this data to make additional higher]", all=TRUE)
})

test_that("Single source community dataset is created and contains all sources", {
  expect_true(all(comind$N>0))
  expect_true(all(comind$N_greater0>0))
  expect_equal(comind$Source, Data_sets)
  expect_equal(comind$N, comind$N_Taxsamples)
})

test_that("Single source taxa dataset is created and contains all sources", {
  expect_true(all(taxind$N>0))
  expect_true(all(taxind$N_greater0>0))
  expect_equal(taxind$Source, Data_sets)
  expect_equal(taxind$N, taxind$N_Taxsamples)
})


