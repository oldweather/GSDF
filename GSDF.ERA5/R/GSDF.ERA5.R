# Functions for getting data from the ERA5 reanalyis

# names and classes of variables
#' @export
ERA5.monolevel.analysis<-c('prmsl','air.2m','uwnd.10m','vwnd.10m','icec',
                           'sst')
#' @export
ERA5.monolevel.forecast<-c('prate')

#' Translate to ERA5 variable names in the .nc files
#'
#' I'm standardising on 20CR variable names - map to ERA5 choices
#'
#' ERA5 uses different names for the files and the variable in the file
#'  this function maps to ERA5 variable names.
#'
#' @export
#' @param var - 20CR variable name
#' @return name used for ERA5 variable names
ERA5.translate.for.variable.names<-function(var) {
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

#' Translate to ERA5 file names of the .nc files
#'
#' I'm standardising on 20CR variable names - map to ERA5 choices
#'
#' ERA5 uses different names for the files and the variable in the file
#'  this function maps to ERA5 file names.
#'
#' @export
#' @param var - 20CR variable name
#' @return name used for ERA5 variable names
ERA5.translate.for.file.names<-function(var) {
  v2<-switch(var,
             prmsl    = 'msl',
             air.2m   = '2t',
             uwnd.10m = '10u',
             vwnd.10m = '10v',
             icec     = 'ci',
             sst      = 'sst',
             prate    = 'tp')
  if(is.null(v2)) stop(sprintf("Unsupported variable %s",var))
  return(v2)
}

#' ERA5 get data directory
#'
#' Find local data directory - different for different systems
#'
#' It's much faster to read data from local disc than over openDAP,
#'  also observations and standard deviations are currently only
#'  available locally. But the various different systems on which I run this code all have
#'  different places to keep large data files. This function returns
#'  the right base directory for the system (if there is one).
#'
#' @export
#' @return Base directory name (or NULL is no local files)
ERA5.get.data.dir<-function() {
    base.file<-sprintf("%s/ERA5",Sys.getenv('SCRATCH'))
    if(file.exists(base.file)) {
            return(base.file )
    }
    return(NULL)
}

# Get class of variable: monolevel or pressure-level.
ERA5.get.variable.group<-function(variable) {
  if(variable %in% ERA5.monolevel.analysis) return('monolevel.analysis')
  if(variable %in% ERA5.monolevel.forecast) return('monolevel.forecast')
  stop(sprintf("Unrecognised variable: %s",variable))
}

#' ERA5 get file name (hourly)
#'
#' Get the file name for selected variable and date (hourly data)
#'
#' Called internally by \code{ERA5.get.slice.at.hour} but also useful
#'  called directly - you can then access the data with another tool.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any supported variable
#' @param stream - 'oper', 'eda' or other ERA5 stream.
#' @param fc.init - hour at which the forecast run was initialised (6 or 18) - if NULL
#'  default, uses whicever gives the shortest lead time (ignored for analysis variables).
#' @return File containing the requested data 
ERA5.hourly.get.file.name<-function(variable,year,month,day,hour,
                                    stream='oper',fc.init=NULL,type='mean') {
    base.dir<-ERA5.get.data.dir()
    if(type=='normal') {
      file.name<-ERA5.climatology.get.file.name(variable,month)
      if(file.exists(file.name)) return(file.name)
      stop(sprintf("No local data file %s",file.name))
    }  
    dir.name<-sprintf("%s/%s/hourly/%04d/%02d/",base.dir,stream,
                        year,month)
    file.name<-sprintf("%s/%s.nc",dir.name,variable)
    if(ERA5.get.variable.group(variable) == 'monolevel.forecast') {
      if(is.null(fc.init)) {
        fc.init<-18
        if(hour>=6 && hour<18) fc.init<-6
      }
      if(fc.init!=6 && fc.init!=18) {
        stop("Forcast initialisation time must be 6 or 18")
      }
      if(fc.init==6 && hour<6 && hour>0) {
        stop("Hour more than 18 hours after forecast initialisation")
      }
      if(fc.init==18 && hour<18 && hour>12) {
        stop("Hour more than 18 hours after forecast initialisation")
      }
      if(hour<fc.init) {
         dte<-ymd(sprintf("%04d-%02d-%02d",year,month,day))-days(1)
         dir.name<-sprintf("%s/%s/hourly/%04d/%02d",base.dir,stream,
                        year(dte),month(dte))
       }
       file.name<-sprintf("%s/%s.%02d.nc",dir.name,variable,fc.init)
    }
    if(file.exists(file.name)) return(file.name)
    stop(sprintf("No local data file %s",file.name))
}

ERA5.is.in.file<-function(variable,year,month,day,hour,stream='oper',type='mean') {
     if(stream=='oper' && hour==as.integer(hour)) return(TRUE)
     if(stream=='enda' && hour%%3==0) return(TRUE)
     return(FALSE)
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
ERA5.get.interpolation.times<-function(variable,year,month,day,hour,stream='oper',type='mean') {
    if(ERA5.is.in.file(variable,year,month,day,hour,stream=stream,type=type)) {
            stop("Internal interpolation failure")
    }
    if(stream=='oper') {
        t.previous<-list(year=year,month=month,day=day,hour=as.integer(hour))
        nd<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",year,month,day,
                            as.integer(hour)))+hours(1)
        t.next<-list(year=year(nd),month=month(nd),day=day(nd),hour=hour(nd))
        return(list(t.previous,t.next))
    }
    if(stream=='enda') {
        hour<-as.integer(hour)
        pd<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",year,month,day,hour))-
                            lubridate::hours(hour%%3)
        t.previous<-list(year=year(pd),month=month(pd),day=day(pd),hour=hour(pd))
        nd<-pd+lubridate::hours(3)
        t.next<-list(year=year(nd),month=month(nd),day=day(nd),hour=hour(nd))
        return(list(t.previous,t.next))
    }
    stop(sprintf("Unsupported stream %s",stream))
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
#' @param fc.init - which forecast initialisation hour to use: 6, 18, blend (gives combination
#'   of both with a smooth transition), or NULL (default, uses whichever has shortest lag time).
#' @param type 'mean' (default), can also be 'normal' - 1981-2010 normal.
#' @return A GSDF field with lat and long as extended dimensions
ERA5.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,fc.init=NULL,type='mean') {
  if(!is.null(fc.init) && fc.init=='blend') {
    if(hour<7) {
      return(ERA5.get.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=18,type=type))
    }
    if(hour>=7 && hour<=12) {
      r1<-ERA5.get.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=6,type=type)
      r2<-ERA5.get.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=18,type=type)
      blend<-(hour-6)/6
      r1$data[]<-r1$data*blend+r2$data*(1-blend)
      return(r1)
    }
    if(hour>12 && hour<19) {
      return(ERA5.get.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=6,type=type))
    }
    if(hour>=19) {
      r1<-ERA5.get.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=18,type=type)
      r2<-ERA5.get.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=6,type=type)
      blend<-(hour-18)/6
      r1$data[]<-r1$data*blend+r2$data*(1-blend)
      return(r1)
    }    
  }
  if(ERA5.get.variable.group(variable)=='monolevel.analysis' ||
     ERA5.get.variable.group(variable)=='monolevel.forecast') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(ERA5.get.slice.at.level.at.hour(variable,year,month,day,hour,fc.init=fc.init,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(ERA5.heights>=height))
  if(height==ERA5.heights[level.below]) {
    return(ERA5.get.slice.at.level.at.hour(variable,year,month,day,hour,
                                           height=ERA5.heights[level.below],fc.init=fc.init,type=type))
  }
  below<-ERA5.get.slice.at.level.at.hour(variable,year,month,day,hour,
                                         height=ERA5.heights[level.below],fc.init=fc.init,type=type)
  above<-ERA5.get.slice.at.level.at.hour(variable,year,month,day,hour,
                                         height=ERA5.heights[level.below+1],fc.init=fc.init,type=type)
  above.weight<-(ERA5.heights[level.below]-height)/(ERA5.heights[level.below]-ERA5.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

ERA5.get.slice.at.level.at.hour<-function(variable,year,month,day,hour,height=NULL,fc.init=NULL,type=type) {
  # Is it from an analysis time (no need to interpolate)?
    if(ERA5.is.in.file(variable,year,month,day,hour)) {
        hour<-as.integer(hour)
        file.name<-ERA5.hourly.get.file.name(variable,year,month,day,hour,stream='oper',fc.init=fc.init,type=type)
        if(type=='normal') {
             year<-1981
             if(month==2 && day==29) day<-28
        }
       t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                    format=c(dates='y/m/d',times='h:m:s'))-1/48
       t2<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                    format=c(dates='y/m/d',times='h:m:s'))+1/48
       v<-GSDF.ncdf.load(file.name,ERA5.translate.for.variable.names(variable),
                         lat.range=c(-90,90),lon.range=c(0,360),
                         height.range=rep(height,2),time.range=c(t,t2))
        
        return(v)
    }
    # Interpolate from the previous and subsequent analysis times
    interpolation.times<-ERA5.get.interpolation.times(variable,year,month,day,hour)
    v1<-ERA5.get.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,interpolation.times[[1]]$month,
                                           interpolation.times[[1]]$day,interpolation.times[[1]]$hour,
                                           fc.init=fc.init,type=type)
    v2<-ERA5.get.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,interpolation.times[[2]]$month,
                                           interpolation.times[[2]]$day,interpolation.times[[2]]$hour,
                                           fc.init=fc.init,type=type)
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

#' Get slice at hour for each ensemble member
#'
#' Get a 2D horizontal slice of a selected variable (as a GSDF field) for a given hour.
#'
#' Interpolates to the selected hour when the data available are less than hourly.
#' Interpolates to the selected height when the selected height is not that of a ERA5 level.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any supported variable.
#' @param height Height in hPa - leave NULL for monolevel
#' @param fc.init - which forecast initialisation hour to use: 6, 18, blend (gives combination
#'   of both with a smooth transition), or NULL (default, uses whichever has shortest lag time).
#' @return A GSDF field with lat and long as extended dimensions
ERA5.get.members.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,fc.init=NULL) {
  if(!is.null(fc.init) && fc.init=='blend') {
    if(hour<7) {
      return(ERA5.get.members.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=18))
    }
    if(hour>=7 && hour<=12) {
      r1<-ERA5.get.members.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=6)
      r2<-ERA5.get.members.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=18)
      blend<-(hour-6)/6
      r1$data[]<-r1$data*blend+r2$data*(1-blend)
      return(r1)
    }
    if(hour>12 && hour<19) {
      return(ERA5.get.members.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=6))
    }
    if(hour>=19) {
      r1<-ERA5.get.members.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=18)
      r2<-ERA5.get.members.slice.at.hour(variable,year,month,day,hour,
                                    height=height,fc.init=6)
      blend<-(hour-18)/6
      r1$data[]<-r1$data*blend+r2$data*(1-blend)
      return(r1)
    }    
  }
  if(ERA5.get.variable.group(variable)=='monolevel.analysis' ||
     ERA5.get.variable.group(variable)=='monolevel.forecast') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(ERA5.get.members.slice.at.level.at.hour(variable,year,month,day,hour,fc.init=fc.init))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(ERA5.heights>=height))
  if(height==ERA5.heights[level.below]) {
    return(ERA5.get.members.slice.at.level.at.hour(variable,year,month,day,hour,
                                                   height=ERA5.heights[level.below],fc.init=fc.init))
  }
  below<-ERA5.get.members.slice.at.level.at.hour(variable,year,month,day,hour,
                                                 height=ERA5.heights[level.below],fc.init=fc.init)
  above<-ERA5.get.members.slice.at.level.at.hour(variable,year,month,day,hour,
                                                 height=ERA5.heights[level.below+1],fc.init=fc.init)
  above.weight<-(ERA5.heights[level.below]-height)/(ERA5.heights[level.below]-ERA5.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

ERA5.get.members.slice.at.level.at.hour<-function(variable,year,month,day,hour,height=NULL,fc.init=NULL) {
    # Is it from an analysis time (no need to interpolate)?
    if(ERA5.is.in.file(variable,year,month,day,hour,stream='enda')) {
        hour<-as.integer(hour)
        file.name<-ERA5.hourly.get.file.name(variable,year,month,day,hour,stream='enda',fc.init=fc.init)
           t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                        format=c(dates='y/m/d',times='h:m:s'))-1/48
           t2<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                        format=c(dates='y/m/d',times='h:m:s'))+1/48
           v<-GSDF.ncdf.load(file.name,ERA5.translate.for.variable.names(variable),
                             lat.range=c(-90,90),lon.range=c(0,360),
                             height.range=rep(height,2),time.range=c(t,t2),
                             ens.name='number',ens.range=c(0,9))
           return(v)
    }
    # Interpolate from the previous and subsequent analysis times
    interpolation.times<-ERA5.get.interpolation.times(variable,year,month,day,hour,stream='enda')
    v1<-ERA5.get.members.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,interpolation.times[[1]]$month,
                                           interpolation.times[[1]]$day,interpolation.times[[1]]$hour,fc.init=fc.init)
    v2<-ERA5.get.members.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,interpolation.times[[2]]$month,
                                           interpolation.times[[2]]$day,interpolation.times[[2]]$hour,fc.init=fc.init)
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
