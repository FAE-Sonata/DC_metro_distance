libraries_needed<-c("stringr", "lubridate", "data.table", "readr", "dplyr")
lapply(libraries_needed,require,character.only=TRUE)
rm(libraries_needed)

OPERATING_DAY_CUTOFF<-"04:45"
EASTERN<-"America/New_York"
RAIL<-"Metrorail"
PROCESSED_DIRECTORY<-"processed/"
processed_files<-list.files(PROCESSED_DIRECTORY)
setwd(PROCESSED_DIRECTORY)

get_cutoff<-function(transaction_time) {
  return(paste(floor_date(transaction_time, "day") |> as.character(),
               OPERATING_DAY_CUTOFF) |> ymd_hm(tz=EASTERN))
}

transactions<-lapply(processed_files, fread) |> rbindlist()
transactions[,Time:=with_tz(Time, tz=EASTERN)]
transactions<-transactions[order(Time)]
transactions[,is_prev_calendar:= Time < get_cutoff(Time)]
transactions[,operating_day:=dplyr::if_else(is_prev_calendar,
                                            floor_date(Time, "day") - days(1),
                                            floor_date(Time, "day"))]
stopifnot(all(transactions[,.SD[1],by=operating_day][
  ,.(Description)] != "Transfer"))

# asiantique_jul2024<-transactions[Time > ymd_hm("2024-07-28 00:00", tz=EASTERN),]
transactions[,shift_description:=shift(Description, 1, type="lag")
                   ,by=operating_day]
transactions[,shift_operator:=shift(Operator, 1, type="lag")
                   ,by=operating_day]
transactions[,actual_entrance:=shift(`Exit Location`, 1, type="lag")
                   ,by=operating_day]
transactions[,is_bus_to_rail:=Description == 'Exit' & Operator == RAIL & shift_description == 'Transfer' & shift_operator == RAIL]
transactions[is_bus_to_rail==T,`Entry Location/ Bus Route`:=actual_entrance]

# [DONE] determine operating day: if time < 04:45, operating day is previous calendar day
# bus-to-rail transfer messing up entry-exit station formatting 28 Jul 2024 20:00 hour transfer from FFX Connector to Rail
# 15-minute grace period exit --> RETURN 0 for a distance