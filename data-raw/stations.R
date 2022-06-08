## code to prepare `stations` dataset goes here
require(dplyr)
require(readxl)
require(readr)
require(tidyr)
require(lubridate)

stations <- read_excel(file.path("data-raw", "stations.xlsx"), sheet="lat_long")%>%
  mutate(Source=recode(Source, TNS="STN", twentymm="20mm"))

stationsEMPEZ<-read_csv(file.path("data-raw", "EZ_stations.csv"),
                        col_types = cols_only(SampleDate="c", StationCode="c", Lat="d", Long="d"))%>%
  select(Date=SampleDate, Station=StationCode, Latitude=Lat, Longitude=Long)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Station=recode(Station, EZ2="NZEZ2", EZ6="NZEZ6", EZ6SJR="NZEZ6SJR", EZ2SJR="NZEZ2SJR"))%>%
  drop_na()

usethis::use_data(stations, stationsEMPEZ, overwrite = TRUE)
