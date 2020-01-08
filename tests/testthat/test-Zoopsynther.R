library(zooper)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

#Test on all datasets

test_that("Community option produces messages", {
  expect_output(com <<- Zoopsynther(Data_type="Community")%>%
                  summarise(N = nrow(.),
                            N_greater0 = nrow(filter(., CPUE>0)),
                            Source = list(unique(paste(Source, SizeClass, sep="_"))),
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID))), "These species have no relatives in their size class common to all datasets and have been removed from one or more size classes", all=TRUE)
})

test_that("Taxa option produces messages", {
  expect_output(tax <<- Zoopsynther(Data_type="Taxa")%>%
                  summarise(N = nrow(.),
                            N_greater0 = nrow(filter(., CPUE>0)),
                            Source = list(unique(paste(Source, SizeClass, sep="_"))),
                            N_Taxsamples = n_distinct(paste(SampleID, Taxlifestage, SizeClass)),
                            Samples = list(unique(SampleID[which(Order!="Amphipoda")]))), "[These species are not counted in all datasets|Do not use this data to make additional higher]", all=TRUE)
})

Data_source <- c("EMP", "FMWT", "TNS", "20mm", "FRP", "EMP", "FRP", "EMP", "FMWT", "TNS")
Size_class <- c(rep("Meso", 5), "Micro", rep("Macro", 4))

Data_sets <- paste(Data_source, Size_class, sep="_")

test_that("Community dataset is created and contains all sources", {
  expect_gt(com$N, 0)
  expect_gt(com$N_greater0, 0)
  expect_setequal(unlist(com$Source), Data_sets)
})

test_that("Taxa dataset is created and contains all sources", {
  expect_gt(tax$N, 0)
  expect_gt(tax$N_greater0, 0)
  expect_setequal(unlist(tax$Source), Data_sets)
})

test_that("Community dataset contains no duplicated rows", {
  expect_equal(com$N, com$N_Taxsamples)
})

test_that("Taxa dataset contains no duplicated rows", {
  expect_equal(tax$N, tax$N_Taxsamples)
})


### !!! AMPHIPODS WERE EXCLUDED BECAUSE THEY ARE REMOVED FROM COMMUNITY DATASET SINCE THEY ARE NOT SAMPLED IN ALL MACRO DATASETS !!! ###
test_that("Taxa and community datasets contain same samples", {
  expect_setequal(unlist(tax$Samples), unlist(com$Samples))
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
