# Functions for getting data from the ERA20C reanalyis

# names and classes of variables
ERA20C.monolevel<-c('prmsl','air.2m')
ERA20C.pressure.level<-NULL

# Height of each level in hPa - not yet correct
ERA20C.heights<-c(1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,
                 250,200,150,100,70,50,30,20,10)

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
  print('Monolevel')
  print(ERA20C.monolevel)
  print('Pressure level')
  print(ERA20C.pressure.level)
}

#' ERA20C get data directory
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
ERA20C.get.data.dir<-function() {
    if(file.exists("/Volumes/DataDir/ERA20C/")) {
            return("/Volumes/DataDir/ERA20C/")
    }	
    if(file.exists("/data/cr2/hadpb/ERA20C/")) {
            return("/data/cr2/hadpb/ERA20C/")
    }	
    if(file.exists("/project/projectdirs/incite11/brohan/netCDF.data/ERA20C/")) {
            return("/project/projectdirs/incite11/brohan/netCDF.data/ERA20C/")
    }	
    return(NULL)
}

# Get class of variable: monolevel or pressure-level.
ERA20C.get.variable.group<-function(variable) {
  if(length(which(ERA20C.monolevel==variable))>0) return('monolevel')
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
                                    type='mean') {
    base.dir<-ERA20C.get.data.dir()
    if(!is.null(base.dir)) {
        name<-NULL
        if(type=='mean') {
           name<-sprintf("%s/hourly/%s/%s.%04d.nc",base.dir,
                       variable,variable,year)
        }
        if(is.null(name)) stop(sprintf("Unsupported data type %s",type))
        if(file.exists(name)) return(name)
        stop(sprintf("No local file %s",name))
   }

    stop(sprintf("Unsupported data type %s",type))      
}

ERA20C.is.in.file<-function(variable,year,month,day,hour,type='mean') {
		if(hour%%6==0) return(TRUE)
		return(FALSE)
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
# Could do this directly, but it's vital to keep get.interpolation.times
# and is.in.file consistent.
ERA20C.get.interpolation.times<-function(variable,year,month,day,hour,type='mean') {
	if(ERA20C.is.in.file(variable,year,month,day,hour,type=type)) {
		stop("Internal interpolation failure")
	}
	ct<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
	          times=sprintf("%02d:00:00",hour),
	          format=c(dates='y/m/d',times='h:m:s'))
	t.previous<-list()
        back.hours=1
	while(back.hours<24) {
		p.hour<-hour-back.hours
                p.year<-year
                p.month<-month
                p.day<-day
                if(p.hour<0) {
                  p.year<-as.numeric(as.character(years(ct-1)))
                  p.month<-as.integer(months(ct-1))
                  p.day<-as.integer(days(ct-1))
                  p.hour<-p.hour+24
                }
		if(ERA20C.is.in.file(variable,p.year,p.month,p.day,p.hour,type=type)) {
			t.previous$year<-p.year
			t.previous$month<-p.month
			t.previous$day<-p.day
			t.previous$hour<-p.hour
			break
		}
                back.hours<-back.hours+1
	}
	if(length(t.previous)<4) {
		stop("Interpolation failure, too far back")
	}
	t.next<-list()
	forward.hours<-1
	while(forward.hours<24) {
		n.hour<-hour+forward.hours
                n.year<-year
                n.month<-month
                n.day<-day
                if(n.hour>23) {
                  n.year<-as.numeric(as.character(years(ct+1)))
                  n.month<-as.integer(months(ct+1))
                  n.day<-as.integer(days(ct+1))
                  n.hour<-n.hour-24
                }
		if(ERA20C.is.in.file(variable,n.year,n.month,n.day,n.hour,type=type)) {
			t.next$year<-n.year
			t.next$month<-n.month
			t.next$day<-n.day
			t.next$hour<-n.hour
			break
		}
                forward.hours<-forward.hours+1
	}
	if(length(t.next)<4) {
		stop("Interpolation failure, too far forward")
	}
	return(list(t.previous,t.next))
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
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them), NULL (default)
#'  will use local files if available and network otherwise.
#' @return A GSDF field with lat and long as extended dimensions
ERA20C.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,type='mean') {
  if(ERA20C.get.variable.group(variable)=='monolevel') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(ERA20C.heights>=height))
  if(height==ERA20C.heights[level.below]) {
    return(ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=ERA20C.heights[level.below],
                                         type=type))
  }
  below<-ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=ERA20C.heights[level.below],
                                         type=type)
  above<-ERA20C.get.slice.at.level.at.hour(variable,year,month,day,hour,height=ERA20C.heights[level.below+1],
                                         type=type)
  above.weight<-(ERA20C.heights[level.below]-height)/(ERA20C.heights[level.below]-ERA20C.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

ERA20C.get.slice.at.level.at.hour<-function(variable,year,month,day,hour,height=NULL,type='mean') {
	dstring<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
	# Is it from an analysis time (no need to interpolate)?
	if(ERA20C.is.in.file(variable,year,month,day,hour,type=type)) {
        file.name<-ERA20C.hourly.get.file.name(variable,year,month,day,hour,height=height,
                                                type=type)
           t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                    format=c(dates='y/m/d',times='h:m:s'))
           v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(0,360),
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
	          times=sprintf("%02d:00:00",hour),
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
   fn<-TWCR.hourly.get.file.name(variable,year,1,1,1,12)
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
