# Functions for getting data from the ERA20C reanalyis

# names and classes of variables
ERA20C.monolevel.analysis<-c('prmsl','air.2m','uwnd.10m','vwnd.10m','icec',
                             'hgt.500','sst')
ERA20C.monolevel.forecast<-c('prate')
ERA20C.pressure.level<-NULL

# Get class of variable: monolevel or pressure-level.
ERA20C.get.variable.group<-function(variable) {
  if(length(which(ERA20C.monolevel.analysis==variable))>0) return('monolevel.analysis')
  if(length(which(ERA20C.monolevel.forecast==variable))>0) return('monolevel.forecast')
  stop(sprintf("Unrecognised variable: %s",variable))
}

# Height of each level in hPa - not yet correct
ERA20C.heights<-c(1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,
                 250,200,150,100,70,50,30,20,10)

#' Translate to ERA variable names in the .nc files
#'
#' I'm standardising on 20CR variable names - map to ERA choices
#'
#'
#' @export
#' @param var - 20CR variable name
#' @return name used for ERA 20C variable names
ERA20C.translate.for.variable.names<-function(var) {
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

#' ERA20C show variables
#'
#' List the variables available from 20CR
#'
#' 2 Different grids: 'Monolevel' is
#' Lat x Lon x Time, 'Pressure level'
#' is  Lat x Lon x Height x Time. No parameters or return value
#' prints out a list of available variables.
#' @export
ERA20C.show.variables<-function() {
  print('Monolevel analysis')
  print(ERA20C.monolevel.analysis)
  print('Monolevel forecast')
  print(ERA20C.monolevel.forecast)
  print('Pressure level')
  print(ERA20C.pressure.level)
}

#' ERA20C get data directory
#'
#' Find local data directory - different for different systems
#'
#' The various different systems on which I run this code all have
#'  different places to keep large data files. This function returns
#'  the right base directory for the system (if there is one).
#'
#' @export
#' @return Base directory name (or NULL is no local files)
ERA20C.get.data.dir<-function() {
    base.file<-sprintf("%s/ERA_20C",Sys.getenv('SCRATCH'))
    if(file.exists(base.file)) {
            return(base.file )
    }
    return(NULL)
}

# Get class of variable: monolevel or pressure-level.
ERA20C.get.variable.group<-function(variable) {
  if(length(which(ERA20C.monolevel.analysis==variable))>0) return('monolevel.analysis')
  if(length(which(ERA20C.monolevel.forecast==variable))>0) return('monolevel.forecast')
  if(length(which(ERA20C.pressure.level==variable))>0) return('pressure')
  stop(sprintf("Unrecognised variable: %s",variable))
}

#' ERA20C get file name (hourly)
#'
#' Get the file name for selected variable and date (hourly data)
#'
#' Called internally by \code{ERA20C.get.slice.at.hour} but also useful
#'  called directly - you can then access the data with another tool.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any ERA20C variable
#' @param type - 'mean', 'spread', 'normal', or 'standard.deviation'. - must be 'mean'
#'  Note that standard deviations are not available over opendap.
#' @return File name or URL for netCDF file containing the requested data 
ERA20C.hourly.get.file.name<-function(variable,year,month,day,hour,height=NULL,
                                      fc.init=NULL,type='mean') {
    base.dir<-ERA20C.get.data.dir()
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
      
    if(ERA20C.get.variable.group(variable) == 'monolevel.forecast') {
      if(is.null(fc.init)) {
        if(hour<9 && day==1) {
          month<-month-1
          day<-15
          if(month<1) {
            month<-12
            year<-year-1
          }
        }
      }
      if(!is.null(fc.init) && fc.init=='last') {
        if(day==1 || (hour<9 && day==2)) {
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

ERA20C.is.in.file<-function(variable,year,month,day,hour,type='mean') {
        if(variable=='air.2m') {
          if(hour%%6==0) return(TRUE)
        } else {
          if(hour%%3==0) return(TRUE)
        }
        return(FALSE)
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
# Could do this directly, but it's vital to keep get.interpolation.times
# and is.in.file consistent.
ERA20C.get.interpolation.times<-function(variable,v.year,v.month,v.day,v.hour,type='mean') {
	if(ERA20C.is.in.file(variable,v.year,v.month,v.day,v.hour,type=type)) {
		stop("Internal interpolation failure")
	}
    if(variable =='air.2m') {
        v.hour<-as.integer(v.hour)
        pd<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",v.year,v.month,v.day,v.hour))-
                            lubridate::hours(v.hour%%6)
        t.previous<-list(year=year(pd),month=month(pd),day=day(pd),hour=hour(pd))
        nd<-pd+lubridate::hours(6)
        t.next<-list(year=year(nd),month=month(nd),day=day(nd),hour=hour(nd))
        return(list(t.previous,t.next))
    }
    if(ERA20C.get.variable.group(variable) =='monolevel.analysis' ||
       ERA20C.get.variable.group(variable) =='monolevel.forecast') {
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
#' Interpolates to the selected height when the selected height is not that of a ERA20C level.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any ERA20C variable.
#'                 can also be 'sst' - will return 'air.sfc' over sea grids.
#' @param type - 'mean' (default), 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param height Height in hPa - leave NULL for monolevel
#' @param fc.init If NULL (default) - use the most recent forecast start. If 'last', use the previous day's
#'   forecast start (only possible for 9am data). If 'blend', smooth the transition between forecast fields
#'   initialised on one day and those initialised on the following day by using the 27-hour forcast
#'   on the first day to interpolate.
#' @return A GSDF field with lat and long as extended dimensions
ERA20C.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,fc.init=NULL,type='mean') {
  if(ERA20C.get.variable.group(variable)=='monolevel.analysis' ||
     ERA20C.get.variable.group(variable)=='monolevel.forecast') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,
                                             fc.init=fc.init,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(ERA20C.heights>=height))
  if(height==ERA20C.heights[level.below]) {
    return(ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=ERA20C.heights[level.below],
                                         fc.init=fc.init,type=type))
  }
  below<-ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=ERA20C.heights[level.below],
                                         fc.init=fc.init,type=type)
  above<-ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=ERA20C.heights[level.below+1],
                                         fc.init=fc.init,type=type)
  above.weight<-(ERA20C.heights[level.below]-height)/(ERA20C.heights[level.below]-ERA20C.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

ERA20C.get.slice.at.level.at.hour<-function(variable,year,month,day,hour,height=NULL,
                                            fc.init=NULL,type='mean',deaccumulate=TRUE) {
	# Is it from an analysis time (no need to interpolate)?
	if(ERA20C.is.in.file(variable,year,month,day,hour,type=type)) {
            hour<-as.integer(hour)
            if(!is.null(fc.init) && fc.init=='blend') {
              if(hour==9) {
                r1<-ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=height,
                                                      fc.init=NULL,type=type,
                                                      deaccumulate=deaccumulate)
                r2<-ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=height,
                                                      fc.init='last',type=type,
                                                      deaccumulate=deaccumulate)
                r1$data[]<-(r1$data+r2$data)/2
                return(r1)
              }
              else {
                fc.init<-NULL
              }
            }
            # Precipitation is accumulated over the forecast, and we want instantanious.
            if(variable=='prate' && type=='mean' && hour!=9 && deaccumulate) {
                r1<-ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=height,
                                                       fc.init=fc.init,type=type,
                                                       deaccumulate=FALSE)
                # Subtract the values from 3 hours ago
                lt<-lubridate::ymd_hms(sprintf("%04d-%02d-%02d:%02d:00:00",year,month,day,hour))-
                                       lubridate::hours(3)
                r2<-ERA20C.get.slice.at.level.at.hour(variable,
                                                       as.integer(lubridate::year(lt)),
                                                       as.integer(lubridate::month(lt)),
                                                       as.integer(lubridate::day(lt)),
                                                       as.integer(lubridate::hour(lt)),height=height,
                                                       fc.init=NULL,type=type,
                                                       deaccumulate=FALSE)
                r1$data[]<-r1$data-r2$data
                return(r1)
             }
            file.name<-ERA20C.hourly.get.file.name(variable,year,month,day,hour,height=height,
                                                   fc.init=fc.init,type=type)
               t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                        format=c(dates='y/m/d',times='h:m:s'))
               if(type=='normal' || type=='standard.deviation') { # Normals have no Feb 29
                   month<-as.integer(month) # Sometimes still a factor, why?
                   day<-as.integer(day)
                   if(month==2 && day==29) day<-28
                  t<-chron(sprintf("%04d/%02d/%02d",1981,month,day),sprintf("%02d:00:00",hour),
                           format=c(dates='y/m/d',times='h:m:s'))
               }
               v<-GSDF.ncdf.load(file.name,ERA20C.translate.for.variable.names(variable),
                                 lat.range=c(-90,90),lon.range=c(0,360),
                                 height.range=rep(height,2),time.range=c(t,t))
               return(v)
	}
	# Interpolate from the previous and subsequent analysis times
	interpolation.times<-ERA20C.get.interpolation.times(variable,year,month,day,hour,type=type)
	v1<-ERA20C.get.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,interpolation.times[[1]]$month,
		                               interpolation.times[[1]]$day,interpolation.times[[1]]$hour,
                                               height=height,type=type)
	v2<-ERA20C.get.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,interpolation.times[[2]]$month,
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
	c3<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
	          times=sprintf("%02d:%02d:00",as.integer(hour),as.integer((hour%%1)*60)),
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

#' Make annual mean fields for each hour.
#'
#' Poor man's normals - just the mean for each hour (1:24) over a year
#'
#' Note that the indices are 1:24 not 0:23 as 0 can't be used as a list index.
#'
#' @export
#' @param variable ERA20C variable name, only 2d variables will work.
#' @param year Year to make averages for.
#' @return A list (indexed 1:24) of fields, each of which is the annual average for that hour.
ERA20C.annual.mean.hourly<-function(variable,year) {
   fn<-ERA20C.hourly.get.file.name(variable,year,1,1,1,12)
   t.s<-chron(sprintf("%04d/%02d/%02d",year,1,1),sprintf("%02d:00:00",0),
            format=c(dates='y/m/d',times='h:m:s'))
   t.e<-chron(sprintf("%04d/%02d/%02d",year+1,1,1),sprintf("%02d:00:00",0),
            format=c(dates='y/m/d',times='h:m:s'))
   annual<-GSDF.ncdf.load(fn,variable,lat.range=c(-90,90),
                          lon.range=c(0,360),time.range=c(t.s,t.e))
   hourly<-list()
   for(hr in c(0,6,12,18)) {
     tmp<-annual
     w<-which(hours(annual$dimensions[[3]]$values)==hr)
     tmp$data<-annual$data[,,w]
     tmp$dimensions[[3]]$values<-annual$dimensions[[3]]$values[w]
     if(hr==0) {
        hourly[[24]]<-GSDF.reduce.1d(tmp,3,mean)
     } else {
        hourly[[hr]]<-GSDF.reduce.1d(tmp,3,mean)
     }

   }
   for(hr in c(1,2,3,4,5)) {
     hourly[[hr]]<-hourly[[24]]
     weight<-(hr-0)/6
     hourly[[hr]]$data[]<-hourly[[6]]$data*weight+hourly[[24]]$data*(1-weight)
   }
   for(hr in c(7,8,9,10,11)) {
     hourly[[hr]]<-hourly[[24]]
     weight<-(hr-6)/6
     hourly[[hr]]$data[]<-hourly[[12]]$data*weight+hourly[[6]]$data*(1-weight)
   }
   for(hr in c(13,14,15,16,17)) {
     hourly[[hr]]<-hourly[[24]]
     weight<-(hr-12)/6
     hourly[[hr]]$data[]<-hourly[[18]]$data*weight+hourly[[12]]$data*(1-weight)
   }
   for(hr in c(19,20,21,22,23)) {
     hourly[[hr]]<-hourly[[24]]
     weight<-(hr-18)/6
     hourly[[hr]]$data[]<-hourly[[24]]$data*weight+hourly[[18]]$data*(1-weight)
   }
   return(hourly)
}

#' Extract a hyperslab of data.
#'
#' Up to 4d (lat, lon, height, time).
#'
#' This is an excellent way to generate out of memory errors - use with caution.
#'
#' @export
#' @param variable ERA20C variable name.
#' @param date.range A pair of text date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.
#' @param type - 'mean' (default), 'spread', 'normal', or 'standard.deviation'.
#'  Note that standard deviations are not available over opendap.
#' @param height.range Bottom and top heights in hPa - leave NULL for monolevel
#' @param lat.range Min and max latitude in degrees - defaults to c(-90,90)
#' @param lon.range Min and max longitude in degrees - defaults to c(0,360)
#' @return A GSDF field with the selected multidimensional data
ERA20C.get.slab.from.hourly<-function(variable,date.range,
                                             height.range=NULL,
                                             lat.range=c(-90,90),
                                             lon.range=c(0,360),
                                      type='mean') {
    # Get the start and end dates
     start.d<-list()
     start.d$year<-as.integer(substr(date.range[1],1,4))
     start.d$month<-as.integer(substr(date.range[1],6,7))
     start.d$day<-as.integer(substr(date.range[1],9,10))
     start.d$hour<-as.integer(substr(date.range[1],12,13))
     start.d$chron<-chron(sprintf("%04d/%02d/%02d",start.d$year,start.d$month,start.d$day),
                          sprintf("%02d:00:00",start.d$hour),
                          format=c(dates='y/m/d',times='h:m:s'))
     end.d<-list()
     end.d$year<-as.integer(substr(date.range[2],1,4))
     end.d$month<-as.integer(substr(date.range[2],6,7))
     end.d$day<-as.integer(substr(date.range[2],9,10))
     end.d$hour<-as.integer(substr(date.range[2],12,13))
     end.d$chron<-chron(sprintf("%04d/%02d/%02d",end.d$year,end.d$month,end.d$day),
                          sprintf("%02d:00:00",end.d$hour),
                          format=c(dates='y/m/d',times='h:m:s'))
     if(start.d$chron>end.d$chron) stop("End date must be after start date")
     if(start.d$year != end.d$year) {
        v<-ERA20C.get.slab.from.hourly(variable,c(sprintf("%04d-%02d-%02d:%02d",
                                                          start.d$year,start.d$month,
                                                          start.d$day,start.d$hour),
                                                  sprintf("%04d-12-31:23",
                                                          start.d$year)),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,type=type)
        year<-start.d$year+1
        while(year<end.d$year) {
          v<-GSDF.concatenate(v,
                  ERA20C.get.slab.from.hourly(variable,c(sprintf("%04d-01-01:00",
                                                          year),
                                                         sprintf("%04d-12-31:23",
                                                          year)),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,type=type),'time')
          year<-year+1
        }
          v<-GSDF.concatenate(v,
                  ERA20C.get.slab.from.hourly(variable,c(sprintf("%04d-01-01:00",
                                                          end.d$year),
                                                         sprintf("%04d-%02d-%02d:%02d",
                                                          end.d$year,end.d$month,
                                                          end.d$day,end.d$hour),
                                                          end.d$year),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,type=type),'time')
        return(v)
     }


   file.name<-ERA20C.hourly.get.file.name(variable,start.d$year,start.d$month,
					  start.d$day,start.d$hour,
					  height=height.range[1],
					  type=type)
   if(type=='normal' || type=='standard.deviation') { 
      start.d$chron<-chron(sprintf("%04d/%02d/%02d",1981,start.d$month,start.d$day),
		  sprintf("%02d:00:00",start.d$hour),
		  format=c(dates='y/m/d',times='h:m:s'))
      end.d$chron<-chron(sprintf("%04d/%02d/%02d",1981,end.d$month,end.d$day),
		  sprintf("%02d:00:00",end.d$hour),
		  format=c(dates='y/m/d',times='h:m:s'))
   }
   v<-GSDF.ncdf.load(file.name,ERA20C.translate.for.variable.names(variable),
                     lat.range=lat.range,lon.range=lon.range,
		     height.range=height.range,time.range=c(start.d$chron,end.d$chron))
   return(v)
}
 
