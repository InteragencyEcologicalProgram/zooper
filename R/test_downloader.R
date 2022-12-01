library(dplyr)

tmp_dir <- tempdir()
tmp_dir
list.files(tmp_dir)
files <- list.files(tmp_dir, full.names = T)
files
unlink(file.path(tempdir(), list.files(tempdir(), pattern = "frame.*.png$")), recursive = T)


Zoopdownloader()


