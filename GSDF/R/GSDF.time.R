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
  result$calendar<-GSDF.time.check.calendar(calendar)
  result<-GSDF.time.check.time(result)
  return(result)
}

#' Check that the calendar is supported
#'
#' Can do gregorian, 360_day and 365_day calendars.
#'
#' 365_day may be called 'noleap'
#' gregorian may be called 'standard' or
#' 'proleptic_gregorian'.
#'
#' @param date A celendar string
#' @result One of 'gregorian', '360_day' and '365_day'
GSDF.time.check.calendar<-function(calendar) {

  calendar<-tolower(calendar)
  if(calendar=='standard' ||
     calendar=='proleptic_gregorian') {
    calendar<-'gregorian'
  }
  if(calendar=='noleap') {
    calendar<-'365_day'
  }
  if(calendar != 'gregorian' &&
     calendar != '360_day'   &&
     calendar != '365_day') {
    stop(sprintf("Unsupported calendar %s",calendar))
  }
  return(calendar)

}
  
#' Check that dates are meaningful
#'
#' Check that each string date is valid
#'  given the calendar.
#'
#'
#' @param date A GSDF time object
#' @result TRUE if all the contents are valid.
GSDF.time.check.time<-function(date) {
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
  m<-stringr::str_match(date$date,
            "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d):(\\d\\d):(\\d\\d)")
  years   <- as.integer(m[,2])
  months  <- as.integer(m[,3])
  days    <- as.integer(m[,4])
  hours   <- as.integer(m[,5])
  minutes <- as.integer(m[,6])
  date$date<-sprintf("%04d-%02d-%02d:%02d:%02d",
                      years,months,days,hours,minutes)
  if(date$calendar=='gregorian') {
    l<-lubridate::ymd_hms(sprintf("%s:00",date$date),quiet=TRUE)
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
  if(date$calendar=='360_day') {
     w<-which(months<1   | months>12 |
              days<1     | days>30   |
              hours>23   |
              minutes>59 )
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
    month.length=c(31,28,31,30,31,30,31,31,30,31,30,31)
     w<-which(months<1   | months>12 |
              days<1     | days>month.length[months] |
              hours>23   |
              minutes>59 )
    if(length(w)==1) {
    stop(sprintf("%s is not a valid 365_day date",
                 date$date[w]))
    }
    if(length(w)>1) {
      stop(sprintf("%s and %d others are not valid 365_day dates",
                   date$date[w[1]],length(w)-1))
    }
  }
  
  return(date)   

}

#' Add a period to a GSDF.time
#'
#' Increment the time by a number of years, month, days,
#'  hours or minutes.
#'
#' Increments can be negative, and can be fractional.
#'
#' @export
#' @param date - GSDF.time
#' @param units Units of the offset - string 'hours', 'days', ...
#' @param offset numeric vector of time offsets.
#' @Result A list containing two components
GSDF.time.increment<-function(date,offset,units) {

  if(units != 'years' && units != 'months' && units != 'days' &&
     units != 'hours' && units != 'minutes') {
    stop(sprintf("%s is not a supported time offset unit %s",
          units,
         "(expected 'years', 'months','days', 'hours, or 'minutes')"))
  }
  # Cope with different length of date and offset
  #if(length(date$date)!=length(offset)) {
  #  if(length(date$date)!=1 && length(offset)!=1) {
  #    stop(sprintf("Length mismatch: %d dates and %d offsets",
  #                 length(date$date),length(offset)))
  #  }
  #  if(length(date$date)==1) {
  #    date$date<-rep(date$date,length(offset))
  #  }
  #  if(length(offset==1)) {
  #    offset<-rep(offset,length(date$date))
  #  }
  #}

  # Split the string times into components
  m<-stringr::str_match(date$date,
           "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d):(\\d\\d):(\\d\\d)")
  year  <- as.integer(m[,2])
  month <- as.integer(m[,3])
  day   <- as.integer(m[,4])
  hour  <- as.integer(m[,5])
  minute<- as.integer(m[,6])

  if(date$calendar=='gregorian') {
     l.base<-lubridate::ymd_hms(sprintf("%04d-%02d-%02d:%02d:%02d:00",
                             year,month,day,hour,minute))
     if(units=='years') {
       if(any(as.integer(offset)!=offset)) {
         stop("Increments in units of years must be integers")
       }
       l.base<-l.base+lubridate::years(as.integer(offset))
     }
     if(units=='months') {
       if(any(as.integer(offset)!=offset)) {
         stop("Increments in units of months must be integers")
       }
       # Months have variable length - choose here to just
       #  change the month, leave day, hour etc. unchanged.
       l.base<-lubridate::"%m+%"(l.base,months(offset))
     }
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
     }
     string.times<-sprintf("%04d-%02d-%02d:%02d:%02d",
                            lubridate::year(l.base),
                            lubridate::month(l.base),
                            lubridate::day(l.base),
                            lubridate::hour(l.base),
                            lubridate::minute(l.base))
     return(GSDF.time(string.times,date$calendar))
   }
  
   if(date$calendar=='360_day') {
     if(units=='years') {
       if(any(as.integer(offset)!=offset)) {
         stop("Increments in units of years must be integers")
       }
       year<-year+offset
     }
     if(units=='months') {
       if(any(as.integer(offset)!=offset)) {
         stop("Increments in units of months must be integers")
       }
       month<-month+offset
       year<-year+as.integer((month-1)/12)
       month<-month%%12
       w<-which(month==0)
       if(length(w)>0) month[w]<-12
     }
     if(units=='days') {
        offset<-offset*24*60
        units<-'minutes'
     }
     if(units=='hours') {
        offset<-offset*60
        units<-'minutes'
    }
     if(units=='minutes') {
        minute<-minute+as.integer(offset)
        hour<-hour+floor(minute/60)
        minute<-minute%%60
        day<-day+floor(hour/24)
        hour<-hour%%24
        month<-month+floor((day-1)/30)
        day<-day%%30
        w<-which(day==0)
        if(length(w)>0) day[w]<-30
        year<-year+floor((month-1)/12)
        month<-month%%12
        w<-which(month==0)
        if(length(w)>0) month[w]<-12
 
     }
     string.times<-sprintf("%04d-%02d-%02d:%02d:%02d",
                            as.integer(year),
                            as.integer(month),
                            as.integer(day),
                            as.integer(hour),
                            as.integer(minute))
     return(GSDF.time(string.times,date$calendar))
   }
  
   if(date$calendar=='365_day') {
     if(units=='years') {
       if(any(as.integer(offset)!=offset)) {
         stop("Increments in units of years must be integers")
       }
       year<-year+offset
     }
     if(units=='months') {
       if(any(as.integer(offset)!=offset)) {
         stop("Increments in units of months must be integers")
       }
       month<-month+offset
       year<-year+floor((month-1)/12)
       month<-month%%12
       w<-which(month==0)
       if(length(w)>0) month[w]<-12
     }
     if(units=='days') {
       offset<-offset*24*60
        units<-'minutes'
     }
     if(units=='hours') {
        offset<-offset*60
        units<-'minutes'
     }
     month.lengths<-c(31,28,31,30,31,30,31,31,30,31,30,31)
     month.tostart<-c(0,31,59,90,120,151,181,212,243,273,304,334,365)
     if(units=='minutes') {
        julian<-month.tostart[month]*24*60+(day-1)*24*60+
                   hour*60+minute+as.integer(offset)
        year<-year+floor(julian/(365*24*60))
        julian<-julian%%(365*24*60)
        for(m in seq(1,12)) {
          w<-which(julian>=month.tostart[m]*24*60 &
                   julian<month.tostart[m+1]*24*60)
          if(length(w)==0) next
          month[w]<-m
          julian[w]<-julian[w]-month.tostart[m]*24*60
          day[w]<-as.integer(julian[w]/(24*60))+1
          julian[w]<-julian[w]%%(24*60)
          hour[w]<-as.integer(julian[w]/60)
          julian[w]<-julian[w]%%(60)
          minute[w]<-as.integer(julian[w])
        }
     }
     string.times<-sprintf("%04d-%02d-%02d:%02d:%02d",
                            as.integer(year),
                            as.integer(month),
                            as.integer(day),
                            as.integer(hour),
                            as.integer(minute))
     return(GSDF.time(string.times,date$calendar))
   }
  
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

  calendar<-GSDF.time.check.calendar(calendar)
  # Assign base time components from the string
  m<-stringr::str_match(base,"(\\d\\d\\d\\d)-(\\d+)-(\\d+)")
  year   <- as.integer(m[,2])
  month  <- as.integer(m[,3])
  day    <- as.integer(m[,4])
  if(is.na(year) | is.na(month) | is.na(day)) {
    stop(sprintf("%s is not a valid base time (expected YYYY-MM-DD + optional:HH:MM)",
                 base))
  }
  m<-stringr::str_match(base,"(\\d\\d\\d\\d)\\D(\\d+)\\D(\\d+)\\D(\\d+)")
  hour   <- as.integer(m[,5])
  if(is.na(hour)) hour<-0
  m<-stringr::str_match(base,"(\\d\\d\\d\\d)\\D(\\d+)\\D(\\d+)\\D(\\d+)\\D(\\d+)")
  minute <- as.integer(m[,6])
  if(is.na(minute)) minute<-0
  base<-GSDF.time(sprintf("%04d-%02d-%02d:%02d:%02d:00",
                             year,month,day,hour,minute),calendar)
  result<-GSDF.time.increment(base,offset,units)
  return(result)
}

#' Calculate difference (in minutes) between two dates
#'
#' Need this for interpolation in time
#'
#' @export
#' @param first GSDF date
#' @param second GSDF date (same no. of dates as first)
#' @Result Vector of numeric differences (in minutes)
GSDF.time.difference<-function(first,second) {
  
    if(first$calendar!=second$calendar) {
      stop("Calendars do not match")
    }

    # Parse the date strings
    first.c<-stringr::str_match(first$date,
               "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
    second.c<-stringr::str_match(second$date,
               "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")

    if(first$calendar=='gregorian') {
      first.l<-lubridate::ymd_hms(sprintf("%04d-%02d-%02d:%02d:%02d:00",
                 as.integer(first.c[,2]),
                 as.integer(first.c[,3]),
                 as.integer(first.c[,4]),
                 as.integer(first.c[,5]),
                 as.integer(first.c[,6])))
      second.l<-lubridate::ymd_hms(sprintf("%04d-%02d-%02d:%02d:%02d:00",
                 as.integer(second.c[,2]),
                 as.integer(second.c[,3]),
                 as.integer(second.c[,4]),
                 as.integer(second.c[,5]),
                 as.integer(second.c[,6])))
      result<-as.numeric(difftime(second.l,first.l,units='mins'))
      return(result)
    }
    if(first$calendar=='360_day') {
      result<-(as.integer(second.c[,2])-as.integer(first.c[,2]))*60*24*30*12 +
              (as.integer(second.c[,3])-as.integer(first.c[,3]))*60*24*30    +
              (as.integer(second.c[,4])-as.integer(first.c[,4]))*60*24       +
              (as.integer(second.c[,5])-as.integer(first.c[,5]))*60          +
              (as.integer(second.c[,6])-as.integer(first.c[,6]))
      return(result)
    }
    if(first$calendar=='365_day') {
      month.tostart<-c(0,31,59,90,120,151,181,212,243,273,304,334)
      result<-(as.integer(second.c[,2])-as.integer(first.c[,2]))*60*24*365   +
              (month.tostart[as.integer(second.c[,3])]-
               month.tostart[as.integer(first.c[,3])])*60*24                 +
              (as.integer(second.c[,4])-as.integer(first.c[,4]))*60*24       +
              (as.integer(second.c[,5])-as.integer(first.c[,5]))*60          +
              (as.integer(second.c[,6])-as.integer(first.c[,6]))    
      return(result)
    }
    stop(sprintf("Unsupported calendar %s",first$calendar))
}

#' Adjust time values for change in calendar
#'
#' Sometimes we want to specify a time range, but we don't know the calendar
#' e.g. a year is -01-01:00:00 to -12-31:23:59 in Gregorian and
#'                -01-01:00:00 to -12:30:23:59 in 360_day.
#' So it's useful to give the spec in gregorian and auto-shrink it if the
#'  data turns out to have a 360_day calendar when we open the file.
#'
#' Assumes that you are going for the full month or year - use
#'  with caution.
#'
#' @export
#' @param first GSDF date
#' @param second GSDF date (same no. of dates as first)
#' @Result Vector of numeric differences (in minutes)
GSDF.time.recalendar<-function(date,new.calendar) {

  new.calendar<-GSDF.time.check.calendar(new.calendar)
  if(date$calendar==new.calendar) return(date)
  if(new.calendar=='gregorian') {
    stop("Can't recalendar gregorian")
  }
  if(new.calendar=='365_day' && date$calendar=='360_day') {
    stop("Can't recalendar 360_day to 365_day")
  }
  # Move dates at the end of the month in the source calendar,
  #  to the last day of the month in the new calendar.
  if(new.calendar=='360_day') {
    m<-stringr::str_match(date$date,
               "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
    days<-as.integer(m[,4])
    months<-as.integer(m[,3])
    w<-which(days>30                |
             (months==2 & days==29) |
             (months==2 & days==28))
    if(length(w)>0) days[w]<-30
    date$date<-sprintf("%s-%s-%02d:%s:%s",m[,2],m[,3],days,m[,5],m[,6])
    date$calendar<-'360_day'
    return(date)
  }
   if(new.calendar=='365_day') {
    m<-stringr::str_match(date$date,
               "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
    days<-as.integer(m[,4])
    months<-as.integer(m[,3])
    w<-which(months==2 & days==29)
    if(length(w)>0) days[w]<-28
    date$date<-sprintf("%s-%s-%02d:%s:%s",m[,2],m[,3],days,m[,5],m[,6])
    date$calendar<-'365_day'
    return(date)
  }
  stop(sprintf("SNH error - unsupported calendar %s",new.calendar))
}
        
  
  
