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
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID)),
                            CPUE_total=sum(CPUE)), "These species have no relatives in their size class common to all datasets and have been removed from one or more size classes", all=TRUE)
  expect_output(comTime <<- Zoopsynther(Data_type="Community", Time_consistency = TRUE)%>%
                  summarise(N = nrow(.),
                            N_greater0 = nrow(filter(., CPUE>0)),
                            Source = list(unique(paste(Source, SizeClass, sep="_"))),
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID)),
                            CPUE_total=sum(CPUE)), "These species have no relatives in their size class common to all datasets and have been removed from one or more size classes", all=TRUE)

})

test_that("Taxa option produces messages", {
  expect_output(tax <<- Zoopsynther(Data_type="Taxa")%>%
                  summarise(N = nrow(.),
                            N_greater0 = nrow(filter(., CPUE>0)),
                            Source = list(unique(paste(Source, SizeClass, sep="_"))),
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID[which(Order!="Amphipoda")]))), "[Some taxa were not measured in all datasets|Do not use this data to make additional higher]", all=TRUE)
})

Data_source <- c("EMP", "FMWT", "STN", "20mm", "FRP", "EMP", "FRP", "EMP", "FMWT", "STN")
Size_class <- c(rep("Meso", 5), "Micro", rep("Macro", 4))

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

test_that("Community dataset with time consistency contains no duplicated rows", {
  expect_equal(comTime$N, comTime$N_Taxsamples)
})

test_that("Taxa dataset contains no duplicated rows", {
  expect_equal(tax$N, tax$N_Taxsamples)
})

test_that("Community dataset with time consistency contains same total CPUE as normal community dataset", {
  expect_equal(comTime$CPUE_total, com$CPUE_total)
})


### !!! AMPHIPODS WERE EXCLUDED BECAUSE THEY ARE REMOVED FROM COMMUNITY DATASET SINCE THEY ARE NOT SAMPLED IN ALL MACRO DATASETS !!! ###
test_that("Taxa and community datasets contain same samples", {
  expect_setequal(unlist(tax$Samples), unlist(com$Samples))
})

# Test that zoopsynther community approach does not change overall CPUE

data_com<-Zoopsynther("Community", Shiny = T)
Removed<-unlist(str_split(str_split(data_com$Caveats, ", ")[[1]], ": "))
Data_base_filtered<-zooper::zoopComb%>%
  filter(!Taxlifestage%in%Removed)

test_that("Zoopsynther with community option and problematic taxa removed produces expected message", {
  expect_output(data_com_filtered<<-Zoopsynther("Community", Zoop=Data_base_filtered), "No disclaimers here")
})

Data_filtered<-data_com_filtered%>%
  group_by(SampleID)%>%
  summarise(CPUE_com=sum(CPUE), .groups="drop")%>%
  full_join(Data_base_filtered%>%
              group_by(SampleID)%>%
              summarise(CPUE_base=sum(CPUE), .groups="drop"),
            by="SampleID")

test_that("Community approach does not change overall CPUE", {
  expect_equal(Data_filtered$CPUE_base, Data_filtered$CPUE_com)
})

#Test on each individual dataset

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
