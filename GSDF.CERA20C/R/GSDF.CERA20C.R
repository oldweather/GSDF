# Functions for getting data from the CERA20C reanalyis

# names and classes of variables
CERA20C.monolevel.analysis<-c('prmsl','air.2m','uwnd.10m','vwnd.10m','icec',
                             'hgt.500','sst')
CERA20C.monolevel.forecast<-c('prate')
CERA20C.pressure.level<-NULL

# Get class of variable: monolevel or pressure-level.
CERA20C.get.variable.group<-function(variable) {
  if(length(which(CERA20C.monolevel.analysis==variable))>0) return('monolevel.analysis')
  if(length(which(CERA20C.monolevel.forecast==variable))>0) return('monolevel.forecast')
  stop(sprintf("Unrecognised variable: %s",variable))
}

# Height of each level in hPa - not yet correct
CERA20C.heights<-c(1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,
                 250,200,150,100,70,50,30,20,10)

#' Translate to ERA variable names in the .nc files
#'
#' I'm standardising on 20CR variable names - map to ERA choices
#'
#'
#' @export
#' @param var - 20CR variable name
#' @return name used for ERA 20C variable names
CERA20C.translate.for.variable.names<-function(var) {
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

#' CERA20C show variables
#'
#' List the variables available from 20CR
#'
#' 2 Different grids: 'Monolevel' is
#' Lat x Lon x Time, 'Pressure level'
#' is  Lat x Lon x Height x Time. No parameters or return value
#' prints out a list of available variables.
#' @export
CERA20C.show.variables<-function() {
  print('Monolevel analysis')
  print(CERA20C.monolevel.analysis)
  print('Monolevel forecast')
  print(CERA20C.monolevel.forecast)
  print('Pressure level')
  print(CERA20C.pressure.level)
}

#' CERA20C get data directory
#'
#' Find local data directory - different for different systems
#'
#' The various different systems on which I run this code all have
#'  different places to keep large data files. This function returns
#'  the right base directory for the system (if there is one).
#'
#' @export
#' @return Base directory name (or NULL is no local files)
CERA20C.get.data.dir<-function() {
    base.file<-sprintf("%s/CERA_20C",Sys.getenv('SCRATCH'))
    if(file.exists(base.file)) {
            return(base.file )
    }
    return(NULL)
}

# Get class of variable: monolevel or pressure-level.
CERA20C.get.variable.group<-function(variable) {
  if(length(which(CERA20C.monolevel.analysis==variable))>0) return('monolevel.analysis')
  if(length(which(CERA20C.monolevel.forecast==variable))>0) return('monolevel.forecast')
  if(length(which(CERA20C.pressure.level==variable))>0) return('pressure')
  stop(sprintf("Unrecognised variable: %s",variable))
}

#' CERA20C get file name (hourly)
#'
#' Get the file name for selected variable and date (hourly data)
#'
#' Called internally by \code{CERA20C.get.slice.at.hour} but also useful
#'  called directly - you can then access the data with another tool.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any CERA20C variable
#' @param type - 'mean', 'spread', 'normal', or 'standard.deviation'. - must be 'mean'
#'  Note that standard deviations are not available over opendap.
#' @return File name or URL for netCDF file containing the requested data 
CERA20C.hourly.get.file.name<-function(variable,year,month,day,hour,height=NULL,
                                      fc.init=NULL,type='mean') {
    base.dir<-CERA20C.get.data.dir()
    if(is.null(base.dir)) stop("No base directory available")
    
    name<-NULL
    
    if(type=='normal') {
       name<-sprintf("%s/normals/hourly/%s.nc",base.dir,
                   variable)
       return(name)
    }
    if(type=='standard.deviation') {
       name<-sprintf("%s/standard.deviations/hourly/%s.nc",base.dir,
                   variable)
       return(name)
    }

    if(type!='mean') stop(sprintf("Unsupported data type %s",type)) 
      
    if(CERA20C.get.variable.group(variable) == 'monolevel.forecast') {
      if(is.null(fc.init)) {
        if(hour<21 && day==1) {
          month<-month-1
          day<-15
          if(month<1) {
            month<-12
            year<-year-1
          }
        }
      }
      if(!is.null(fc.init) && fc.init=='last') {
        if(day==1 || (hour<21 && day==2)) {
          month<-month-1
          day<-15
          if(month<1) {
            month<-12
            year<-year-1
          }
        }
        variable<-sprintf("%s.p1d",variable)
      }
    }
      
    name<-sprintf("%s/hourly/%04d/%02d/%s.nc",base.dir,
                       year,month,variable)
    if(file.exists(name)) return(name)
    stop(sprintf("No local file %s",name))
}

CERA20C.is.in.file<-function(variable,year,month,day,hour,type='mean') {
        if(hour%%3==0) return(TRUE)
        return(FALSE)
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
# Could do this directly, but it's vital to keep get.interpolation.times
# and is.in.file consistent.
CERA20C.get.interpolation.times<-function(variable,v.year,v.month,v.day,v.hour,type='mean') {
	if(CERA20C.is.in.file(variable,v.year,v.month,v.day,v.hour,type=type)) {
		stop("Internal interpolation failure")
	}
    if(CERA20C.get.variable.group(variable) =='monolevel.forecast') {
        v.hour<-as.integer(v.hour)
        pd<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",v.year,v.month,v.day,v.hour))-
                            lubridate::hours(v.hour%%3)
        t.previous<-list(year=year(pd),month=month(pd),day=day(pd),hour=hour(pd))
        nd<-pd+lubridate::hours(3)
        t.next<-list(year=year(nd),month=month(nd),day=day(nd),hour=hour(nd))
        return(list(t.previous,t.next))
    }
    if(CERA20C.get.variable.group(variable) =='monolevel.analysis') {
        v.hour<-as.integer(v.hour)
        pd<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",v.year,v.month,v.day,v.hour))-
                            lubridate::hours(v.hour%%3)
        t.previous<-list(year=year(pd),month=month(pd),day=day(pd),hour=hour(pd))
        nd<-pd+lubridate::hours(3)
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
#' Interpolates to the selected height when the selected height is not that of a CERA20C level.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any CERA20C variable.
#'                 can also be 'sst' - will return 'air.sfc' over sea grids.
#' @param type - 'mean' (default), 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param height Height in hPa - leave NULL for monolevel
#' @param fc.init If NULL (default) - use the most recent forecast start. If 'last', use the previous day's
#'   forecast start (only possible for 9am data). If 'blend', smooth the transition between forecast fields
#'   initialised on one day and those initialised on the following day by using the 27-hour forcast
#'   on the first day to interpolate.
#' @return A GSDF field with lat and long as extended dimensions
CERA20C.get.slice.at.hour<-function(variable,v.year,v.month,v.day,v.hour,height=NULL,fc.init=NULL,member=1,type='mean') {
  if(CERA20C.get.variable.group(variable)=='monolevel.analysis' ||
     CERA20C.get.variable.group(variable)=='monolevel.forecast') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(CERA20C.get.slice.at.level.at.hour(variable,v.year,v.month,v.day,v.hour,
                                             fc.init=fc.init,member=member,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(CERA20C.heights>=height))
  if(height==CERA20C.heights[level.below]) {
    return(CERA20C.get.slice.at.level.at.hour(variable,v.year,v.month,v.day,v.hour,height=CERA20C.heights[level.below],
                                         fc.init=fc.init,member=member,type=type))
  }
  below<-CERA20C.get.slice.at.level.at.hour(variable,v.year,v.month,v.day,v.hour,height=CERA20C.heights[level.below],
                                         fc.init=fc.init,member=member,type=type)
  above<-CERA20C.get.slice.at.level.at.hour(variable,v.year,v.month,v.day,v.hour,height=CERA20C.heights[level.below+1],
                                         fc.init=fc.init,member=member,type=type)
  above.weight<-(CERA20C.heights[level.below]-height)/(CERA20C.heights[level.below]-CERA20C.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

CERA20C.get.slice.at.level.at.hour<-function(variable,v.year,v.month,v.day,v.hour,height=NULL,
                                            fc.init=NULL,member=1,type='mean',deaccumulate=TRUE) {
	# Is it from an analysis time (no need to interpolate)?
	if(CERA20C.is.in.file(variable,v.year,v.month,v.day,v.hour,type=type)) {
            v.hour<-as.integer(v.hour)
            if(!is.null(fc.init) && fc.init=='blend') {
              if(v.hour==21) {
                r1<-CERA20C.get.slice.at.level.at.hour(variable,v.year,v.month,v.day,v.hour,height=height,
                                                      fc.init=NULL,member=member,type=type,
                                                      deaccumulate=deaccumulate)
                r2<-CERA20C.get.slice.at.level.at.hour(variable,v.year,v.month,v.day,v.hour,height=height,
                                                      fc.init='last',member=member,type=type,
                                                      deaccumulate=deaccumulate)
                r1$data[]<-(r1$data+r2$data)/2
                return(r1)
              }
              else {
                fc.init<-NULL
              }
            }
            # Precipitation is accumulated over the forecast, and we want instantanious.
            if(variable=='prate' && type=='mean' && v.hour!=21 && deaccumulate) {
                r1<-CERA20C.get.slice.at.level.at.hour(variable,v.year,v.month,v.day,v.hour,height=height,
                                                       fc.init=fc.init,member=member,type=type,
                                                       deaccumulate=FALSE)
                # Subtract the values from 3 hours ago
                lt<-lubridate::ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",v.year,v.month,v.day,v.hour))-
                                                    lubridate::hours(3)
                r2<-CERA20C.get.slice.at.level.at.hour(variable,
                                                       as.integer(lubridate::year(lt)),
                                                       as.integer(lubridate::month(lt)),
                                                       as.integer(lubridate::day(lt)),
                                                       as.integer(lubridate::hour(lt)),height=height,
                                                       fc.init=NULL,member=member,type=type,
                                                       deaccumulate=FALSE)
                r1$data[]<-r1$data-r2$data
                return(r1)
             }
            file.name<-CERA20C.hourly.get.file.name(variable,v.year,v.month,v.day,v.hour,height=height,
                                                   fc.init=fc.init,type=type)
               t<-chron(sprintf("%04d/%02d/%02d",v.year,v.month,v.day),sprintf("%02d:00:00",v.hour),
                        format=c(dates='y/m/d',times='h:m:s'))
               if(type=='normal' || type=='standard.deviation') { # Normals have no Feb 29
                   v.month<-as.integer(v.month) # Sometimes still a factor, why?
                   v.day<-as.integer(v.day)
                   if(v.month==2 && v.day==29) v.day<-28
                  t<-chron(sprintf("%04d/%02d/%02d",1981,v.month,v.day),sprintf("%02d:00:00",v.hour),
                           format=c(dates='y/m/d',times='h:m:s'))
               }
               v<-GSDF.ncdf.load(file.name,CERA20C.translate.for.variable.names(variable),
                                 lat.range=c(-90,90),lon.range=c(0,360),
                                 height.range=rep(height,2),
                                 ens.range=rep(member,2),ens.name='number',
                                 time.range=c(t,t))
               return(v)
	}
	# Interpolate from the previous and subsequent analysis times
	interpolation.times<-CERA20C.get.interpolation.times(variable,v.year,v.month,v.day,v.hour,type=type)
	v1<-CERA20C.get.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,interpolation.times[[1]]$month,
		                               interpolation.times[[1]]$day,interpolation.times[[1]]$hour,
                                               height=height,type=type)
	v2<-CERA20C.get.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,interpolation.times[[2]]$month,
		                               interpolation.times[[2]]$day,interpolation.times[[2]]$hour,
                                               height=height,type=type)
	c1<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[1]]$year,
	                                         interpolation.times[[1]]$month,
	                                         interpolation.times[[1]]$day),
	          times=sprintf("%02d:00:00",interpolation.times[[1]]$hour),
	          format=c(dates='y/m/d',times='h:m:s'))
	c2<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[2]]$year,
	                                         interpolation.times[[2]]$month,
	                                         interpolation.times[[2]]$day),
	          times=sprintf("%02d:00:00",interpolation.times[[2]]$hour),
	          format=c(dates='y/m/d',times='h:m:s'))
	c3<-chron(dates=sprintf("%04d/%02d/%02d",v.year,v.month,v.day),
	          times=sprintf("%02d:%02d:00",as.integer(v.hour),as.integer((v.hour%%1)*60)),
	          format=c(dates='y/m/d',times='h:m:s'))
    if(c2==c1) stop("Zero interval in time interpolation")
    weight<-as.numeric((c2-c3)/(c2-c1))
    v<-v1
    idx.t<-GSDF.find.dimension(v,'time')
    v$dimensions[[idx.t]]$value<-v1$dimensions[[idx.t]]$value+
                                 as.numeric(v2$dimensions[[idx.t]]$value-v1$dimensions[[idx.t]]$value)*(1-weight)
    v$data[]<-v1$data*weight+v2$data*(1-weight)
        return(v)
}

