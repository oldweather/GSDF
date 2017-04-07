#' Unfortunately, the data we are interested in use several calendars.
#'
#' The 'standard' or 'gregorian' calendar
#' The 'proleptic gregorian' calendar
#' A '360_day' calendar with 12 months each of 30 days
#' A '365_day' or 'noleap' calendar with 12 months of
#'   c(31,28,31,30,31,30,31,31,30,31,30,31) days.
#'
#' So we need custom date tools - we will store a date as an
#'  ISO-8601 format string down to minutes: 'YYYY-MM-DD:HH:MM'
#'  combined with a string describing the calendar in use.
#' Fortunately, we don't need to bother with time zones.
#' Negative years are not currently supported.
#'
#' @export
#' @param string Vector of date strings in format 'YYYY-MM-DD:HH:SS'
#' @param calendar One of 'gregorian', '360_day' and '365_day'
#' @Result A list containing two components
GSDF.time<-function(string,calendar) {
  result<-list()
  result$date<-string
  result$calendar<-calendar
  if(calendar != 'gregorian' &&
     calendar != '360_day'   &&
     calendar != '365_day') {
    stop(sprintf("Unsupported calendar %s",calendar))
  }
  ck<-GSDF.check.times(result)
  return(result)
}

#' Check that dates are meaningful
#'
#' Check that each string date is valid
#'  given the calendar.
#'
#'
#' @param date A GSDF time object
#' @result TRUE if all the contents are valid.
GSDF.time.check<-function(date) {
  if(date$calendar != 'gregorian' &&
     date$calendar != '360_day'   &&
     date$calendar != '365_day') {
    stop(sprintf("Unsupported calendar %s",date$calendar))
  }
  w<-which(!grepl("\\d\\d\\d\\d-\\d\\d-\\d\\d:\\d\\d:\\d\\d",
                  date$date))
  if(length(w)==1) {
    stop(sprintf("%s is not in format YYYY-MM-DD:HH:MM",
                 date$date[w]))
  }
  if(length(w)>1) {
    stop(sprintf("%s and %d others are not in format YYYY-MM-DD:HH:MM",
                 date$date[w[1]],length(w)-1))
  }
  if(date$calendar=='gregorian') {
    l<-lubridate::ymd_hms(sprintf("%s:00",date$date))
    w<-which(is.na(l))
    if(length(w)==1) {
    stop(sprintf("%s is not a valid gregorian date",
                 date$date[w]))
    }
    if(length(w)>1) {
      stop(sprintf("%s and %d others are not valid gregorian dates",
                   date$date[w[1]],length(w)-1))
    }
  }
  m<-stringr::str_match(date$date,
            "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d:\\d\\d:\\d\\d)")
  years<-m[,2]
  months<-m[,3]
  days<-m[,4]
  hours<-m[,5]
  minutes<-m[,6]
  if(date$calendar=='360_day') {
     w<-which(months<1   | months>12 |
              days<1     | days>30   |
              hours>23   |
              minutes>59 |
              seconds>59)
    if(length(w)==1) {
    stop(sprintf("%s is not a valid 360_day date",
                 date$date[w]))
    }
    if(length(w)>1) {
      stop(sprintf("%s and %d others are not valid 360_day dates",
                   date$date[w[1]],length(w)-1))
    }
  }
  if(date$calendar=='365_day') {
    month.lengths=c(31,28,31,30,31,30,31,31,30,31,30,31)
     w<-which(months<1   | months>12 |
              days<1     | days>month.length[months] |
              hours>23   |
              minutes>59 |
              seconds>59)
    if(length(w)==1) {
    stop(sprintf("%s is not a valid 365_day date",
                 date$date[w]))
    }
    if(length(w)>1) {
      stop(sprintf("%s and %d others are not valid 365_day dates",
                   date$date[w[1]],length(w)-1))
    }
  }
  
  return(TRUE)   

}

#' Convert base + offset into GSDF.time format
#'
#' NetCDF files store date:time as a base time and numeric
#'  offsets from that time given a calendar. The offsets
#' can be in units of days, hours, or minutes.
#'
#' Convert these into GSDF.time format
#'  (Fractions of a minute are discarded).
#'
#' @export
#' @param base String time in format 'YYYY-MM-DD:HH:SS'
#'     smaller components (s, min) may be missing.
#' @param calendar One of 'gregorian', '360_day' and '365_day'
#' @param units Units of the offset - string 'hours', 'days', ...
#' @param offset numeric vector of time offsets.
#' @Result A list containing two components
GSDF.time.from.base.and.offset<-function(offset,base,units,calendar) {

  # Assign base time components from the string
  m<-stringr::str_match(base,"(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)")
  year<-m[,2]
  month<-m[,3]
  day<-m[,4]
  if(is.na(year) | is.na(month) | is.na(day)) {
    stop(sprintf("%s is not a valid base time (expected YYYY-MM-DD + optional:HH:MM)",
                 base))
  }
  m<-stringr::str_match(base,"(\\d\\d\\d\\d)\D(\\d\\d)\D(\\d\\d)\D(\d\d)")
  hour<-m[,5]
  if(is.na(hour)) hour<-0
  minute<-m[,6]
  if(is.na(minute)) minute<-0

  if(calendar=='gregorian') {

     l.base<-lubridate::ymd_hms(sprintf("%04d-%02d-%02d:%02d:%02d:00",
                             year,month,day,hour,minute))
    
     if(units=='days') {
       l.base<-l.base+lubridate::days(as.integer(offset))
       offset<-(offset-as.integer(offset))*24
       units<-'hours'
     }
     if(units=='hours') {
       l.base<-l.base+lubridate::hours(as.integer(offset))
       offset<-(offset-as.integer(offset))*60
       units<-'minutes'
     }
     if(units=='minutes') {
       l.base<-l.base+lubridate::minutes(as.integer(offset))
     } else {
       stop("%s is not a supported time offset unit (expected 'days', 'hours, or 'minutes')",
            units)
     }
     string.times<-sprintf(("%04d-%02d-%02d:%02d:%02d:00",
                            lubridate::year(l.base),
                            lubridate::month(l.base),
                            lubridate::day(l.base),
                            lubridate::hour(l.base),
                            lubridate::minute(l.base)))
     return(GSDF.time(string.times,calendar))
   }
  
}
