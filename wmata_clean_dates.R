setwd("~/general_learning/DC_Metro_dailydistance/WMATA")
libraries_needed<-c("stringr", "lubridate", "data.table", "readr")
lapply(libraries_needed,require,character.only=TRUE)
TIME_ZONE<-"America/New_York"

clean_date<-function(fn) {
  raw_dt<-fread(paste("raw", fn, sep="/"))
  original_names<-names(raw_dt)
  raw_dt[,Time:=Time |> as.character() |> mdy_hm(tz=TIME_ZONE)]
  fwrite(raw_dt, paste("processed", fn, sep="/"))
  # raw_dt[,Time:=NULL]
  # setnames(raw_dt, "corrected_time", "Time")
}
raw_files<-list.files("raw/")
sapply(raw_files, clean_date)