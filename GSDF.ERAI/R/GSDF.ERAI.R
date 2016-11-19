# Functions for getting data from the ERA Interim reanalyis

# names and classes of variables
ERAI.monolevel.analysis<-c('prmsl','air.2m','uwnd.10m','vwnd.10m','icec',
                           'sst')
ERAI.monolevel.forecast<-c('prate')

#' Translate to ERA Interim variable names in the .nc files
#'
#' I'm standardising on 20CR variable names - map to ERAI choices
#'
#'
#' @export
#' @param var - 20CR variable name
#' @return name used for ERAI variable names
ERAI.translate.for.variable.names<-function(var) {
  v2<-switch(var,
             prmsl    = 'msl',
             air.2m   = 't2m',
             uwnd.10m = 'u10',
             vwnd.10m = 'v10',
             icec     = 'ci',
             sst      = 'sst',
             prate    = 'tp')
  if(is.null(v2)) stop(sprintf("Unsupported variable %s",var))
  return(v2)
}

#' ERAI get data directory
#'
#' Find local data directory - different for different systems
#'
#' The various different systems on which I run this code all have
#'  different places to keep large data files. This function returns
#'  the right base directory for the system (if there is one).
#'
#' @export
#' @return Base directory name (or NULL is no local files)
ERAI.get.data.dir<-function() {
    base.file<-sprintf("%s/ERA_Interim",Sys.getenv('SCRATCH'))
    if(file.exists(base.file)) {
            return(base.file )
    }
    return(NULL)
}

# Get class of variable: monolevel or pressure-level.
ERAI.get.variable.group<-function(variable) {
  if(length(which(ERAI.monolevel.analysis==variable))>0) return('monolevel.analysis')
  if(length(which(ERAI.monolevel.forecast==variable))>0) return('monolevel.forecast')
  stop(sprintf("Unrecognised variable: %s",variable))
}

#' ERAI get file name (hourly)
#'
#' Get the file name for selected variable and date (hourly data)
#'
#' Called internally by \code{ERAI.get.slice.at.hour} but also useful
#'  called directly - you can then access the data with another tool.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any supported variable
#' @return File containing the requested data 
ERAI.hourly.get.file.name<-function(variable,year,month,day,hour) {
    base.dir<-ERAI.get.data.dir()
    dir.name<-sprintf("%s/hourly/%04d/%02d",base.dir,year,month)
    file.name<-sprintf("%s/%s.nc",dir.name,variable)
    if(ERAI.get.variable.group(variable) == 'monolevel.forecast') {
       if(day==1 && hour==1) { # Forecast variables are offset by 3 hours in the files
         dte<-ymd(sprintf("%04d-%02d-%02d",year,month,day))-days(1)
         dir.name<-sprintf("%s/hourly/%04d/%02d",base.dir,
                        year(dte),month(dte))
       }
       file.name<-sprintf("%s/%s.nc",dir.name,variable)
    }
    if(file.exists(file.name)) return(file.name)
    stop(sprintf("No local data file %s",file.name))
}

ERAI.is.in.file<-function(variable,year,month,day,hour,stream='oper') {
     if(ERAI.get.variable.group(variable) =='monolevel.forecast' && hour%%3==0) return(TRUE)
     if(ERAI.get.variable.group(variable) =='monolevel.analysis' && hour%%6==0) return(TRUE)
     return(FALSE)
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
ERAI.get.interpolation.times<-function(variable,year,month,day,hour,stream='oper') {
    if(ERAI.is.in.file(variable,year,month,day,hour,stream=stream)) {
            stop("Internal interpolation failure")
    }
    if(ERAI.get.variable.group(variable) =='monolevel.forecast') {
        hour<-as.integer(hour)
        pd<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",year,month,day,hour))-
                            lubridate::hours(hour%%3)
        t.previous<-list(year=year(pd),month=month(pd),day=day(pd),hour=hour(pd))
        nd<-pd+lubridate::hours(3)
        t.next<-list(year=year(nd),month=month(nd),day=day(nd),hour=hour(nd))
        return(list(t.previous,t.next))
    }
    if(ERAI.get.variable.group(variable) =='monolevel.analysis') {
        hour<-as.integer(hour)
        pd<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",year,month,day,hour))-
                            lubridate::hours(hour%%6)
        t.previous<-list(year=year(pd),month=month(pd),day=day(pd),hour=hour(pd))
        nd<-pd+lubridate::hours(6)
        t.next<-list(year=year(nd),month=month(nd),day=day(nd),hour=hour(nd))
        return(list(t.previous,t.next))
    }
    stop(sprintf("Unsupported variable %s",variable))
}

# This is the function users will call.
#' Get slice at hour.
#'
#' Get a 2D horizontal slice of a selected variable (as a GSDF field) for a given hour.
#'
#' Interpolates to the selected hour when the data available are less than hourly.
#' Interpolates to the selected height when the selected height is not that of a ERA5 level.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any supported variable.
#' @param height Height in hPa - leave NULL for monolevel
#' @return A GSDF field with lat and long as extended dimensions
ERAI.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL) {
  if(ERAI.get.variable.group(variable)=='monolevel.analysis' ||
     ERAI.get.variable.group(variable)=='monolevel.forecast') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(ERAI.get.slice.at.level.at.hour(variable,year,month,day,hour))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(ERAI.heights>=height))
  if(height==ERAI.heights[level.below]) {
    return(ERAI.get.slice.at.level.at.hour(variable,year,month,day,hour,
                                           height=ERAI.heights[level.below],fc.init=fc.init))
  }
  below<-ERAI.get.slice.at.level.at.hour(variable,year,month,day,hour,
                                         height=ERAI.heights[level.below],fc.init=fc.init)
  above<-ERAI.get.slice.at.level.at.hour(variable,year,month,day,hour,
                                         height=ERAI.heights[level.below+1],fc.init=fc.init)
  above.weight<-(ERAI.heights[level.below]-height)/(ERAI.heights[level.below]-ERAI.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

ERAI.get.slice.at.level.at.hour<-function(variable,year,month,day,hour,height=NULL) {
  # Is it from an analysis time (no need to interpolate)?
    if(ERAI.is.in.file(variable,year,month,day,hour)) {
        hour<-as.integer(hour)
        file.name<-ERAI.hourly.get.file.name(variable,year,month,day,hour)
           t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                        format=c(dates='y/m/d',times='h:m:s'))-1/48
           t2<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                        format=c(dates='y/m/d',times='h:m:s'))+1/48
           v<-GSDF.ncdf.load(file.name,ERAI.translate.for.variable.names(variable),
                             lat.range=c(-90,90),lon.range=c(0,360),
                             height.range=rep(height,2),time.range=c(t,t2))
           return(v)
    }
    # Interpolate from the previous and subsequent analysis times
    interpolation.times<-ERAI.get.interpolation.times(variable,year,month,day,hour)
    v1<-ERAI.get.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,interpolation.times[[1]]$month,
                                           interpolation.times[[1]]$day,interpolation.times[[1]]$hour)
    v2<-ERAI.get.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,interpolation.times[[2]]$month,
                                           interpolation.times[[2]]$day,interpolation.times[[2]]$hour)
    c1<-ymd_hms(sprintf("%04d/%02d/%02d:%02d:00:00",interpolation.times[[1]]$year,
                                                    interpolation.times[[1]]$month,
                                                    interpolation.times[[1]]$day,
                                                    interpolation.times[[1]]$hour))
    c2<-ymd_hms(sprintf("%04d/%02d/%02d:%02d:00:00",interpolation.times[[2]]$year,
                                                    interpolation.times[[2]]$month,
                                                    interpolation.times[[2]]$day,
                                                    interpolation.times[[2]]$hour))
    c3<-ymd_hms(sprintf("%04d/%02d/%02d:%02d:%02d:00",year,
                                                      month,
                                                      day,
                                                      as.integer(hour),
                                                      as.integer((hour%%1)*60)))
    if(c2==c1) stop("Zero interval in time interpolation")
    weight<-as.duration(c2-c3)/as.duration(c2-c1)
    v<-v1
    idx.t<-GSDF.find.dimension(v,'time')
    v$dimensions[[idx.t]]$value<-v1$dimensions[[idx.t]]$value+
                                 as.numeric(v2$dimensions[[idx.t]]$value-v1$dimensions[[idx.t]]$value)*(1-weight)
    v$data[]<-v1$data*weight+v2$data*(1-weight)
    return(v)
}



