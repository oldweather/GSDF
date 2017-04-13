#' Load a GSDF field from a netCDF file or URL
#'
#' Loads a specified hyperslab (range of lat, lon, height and time)
#'  from a NetCDF file (or an openDAP server).
#' New version with GSDF.time dates
#'
#' This works only for some netCDF files (netCDF is a very flexible file 
#'  format and it's necessary to make assumptions about how the data is stored);
#'  Does work on most CMIP5 data and reanalyses.
#' 
#' @export
#' @param file Text name of file or URI.
#' @param variable Text name of variable (in file).
#' @param lat.range Two-element vector with max and min latitudes to retrieve.
#'  Leave NULL only if variable has no latitude dimension.
#' @param lon.range Two-element vector with max and min longitudes to retrieve.
#'  Leave NULL only if variable has no longitude dimension.
#' @param height.range Two-element vector with max and min heights to retrieve.
#'  Leave NULL only if variable has no height dimension.
#' @param ens.range Two-element vector with max and min ensemble members to retrieve.
#'  Leave NULL only if variable has no ensemble dimension.
#' @param time.range GSDF.time with max and min times to retrieve.
#'  Leave NULL only if variable has no time dimension.
#' @param default.calendar What calendar does file use (if not specified in file)? 
#'  Supported values are those in GSDF.time.
#' @param lat.name Text name used by file as label of latitude variable. If NULL, will
#'  try to guess it.
#' @param lon.name Text name used by file as label of longitude variable. If NULL, will
#'  try to guess it.
#' @param height.name Text name used by file as label of height variable. If NULL, will
#'  try to guess it.
#' @param ens.name Text name used by file as label of ensemble number variable. If NULL, will
#'  try to guess it.
#' @param time.name Text name used by file as label of time variable. If NULL, will
#'  try to guess it.
#' @return GSDF field with selected subset of file data.
GSDF.ncdf.load2<-function(file,variable,lat.range=NULL,lon.range=NULL,
                         height.range=NULL,time.range=NULL,
                         ens.range=NULL,custom.range=NULL,
                         default.calendar='gregorian',
                         lat.name=NULL,lon.name=NULL,
                         height.name=NULL,time.name=NULL,
                         ens.name=NULL) {

   f<-nc_open(file)
   v<-GSDF.ncdf.get.var(f,variable)
   lat.i<-GSDF.ncdf.get.lat(v,lat.name)
   if(is.null(lat.range) && !is.null(lat.i)) {
      stop('Latitude range required (lat.range=c(-90,90)')
   } 
   lon.i<-GSDF.ncdf.get.lon(v,lon.name)
   if(is.null(lon.range) && !is.null(lon.i)) {
     if(length(v$dim[[lon.i]]$vals)==1) {
      lon.range<-rep(v$dim[[lon.i]]$vals,2)
     } else stop('Longitude range required (lon.range=c(-180,180)')
   } 
   height.i<-GSDF.ncdf.get.height(v,height.name)
   if(is.null(height.range) && !is.null(height.i)) {
     if(length(v$dim[[height.i]]$vals)==1) {
      height.range<-rep(v$dim[[height.i]]$vals,2)
     } else stop('Height range required (height.range=c(850,850)')
   } 
   ens.i<-GSDF.ncdf.get.ens(v,ens.name)
   if(is.null(ens.range) && !is.null(ens.i)) {
     if(length(v$dim[[ens.i]]$vals)==1) {
      ens.range<-rep(v$dim[[ens.i]]$vals,2)
     } else stop('Ensemble range required (ensemble.range=c(1,1)')
   } 
   time.i<-GSDF.ncdf.get.time(v,time.name)
   if(is.null(time.range) && !is.null(time.i)) {
     if(length(v$dim[[time.i]]$vals)==1) {
      time.range<-rep(v$dim[[time.i]]$vals,2)
      default.calendar<-'raw'
     } else stop('Time range required (time.range=c(chron.1,chron.2)')
   }
   result<-GSDF()
   result$meta<-ncatt_get(f,v)
   start<-rep(NA,v$ndim)
   count<-rep(NA,v$ndim)
   for(d in seq(1,v$ndim)) {
      if(!is.null(lat.i) && lat.i==d) {
         w<-which(v$dim[[d]]$vals>=min(lat.range) &
                  v$dim[[d]]$vals<=max(lat.range))
         if(length(w)==0) return(NULL) # No matching data
         start[d]<-min(w)
         count[d]<-max(w)-start[d]+1
         result$dimensions[[d]]<-list('type'='lat',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(lon.i) && lon.i==d) {
         w<-which(v$dim[[d]]$vals>=min(lon.range) &
                  v$dim[[d]]$vals<=max(lon.range))
         if(length(w)==0) return(NULL) # No matching data
         start[d]<-min(w)
         count[d]<-max(w)-start[d]+1
         result$dimensions[[d]]<-list('type'='lon',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(height.i) && height.i==d) {
         w<-which(v$dim[[d]]$vals>=min(height.range) &
                  v$dim[[d]]$vals<=max(height.range))
         if(length(w)==0) return(NULL) # No matching data
         start[d]<-min(w)
         count[d]<-max(w)-start[d]+1
         result$dimensions[[d]]<-list('type'='height',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(ens.i) && ens.i==d) {
         w<-which(v$dim[[d]]$vals>=min(ens.range) &
                  v$dim[[d]]$vals<=max(ens.range))
         if(length(w)==0) return(NULL) # No matching data
         start[d]<-min(w)
         count[d]<-max(w)-start[d]+1
         result$dimensions[[d]]<-list('type'='ensemble',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(height.i) && height.i==d) {
         w<-which(v$dim[[d]]$vals>=min(height.range) &
                  v$dim[[d]]$vals<=max(height.range))
         if(length(w)==0) return(NULL) # No matching data
         start[d]<-min(w)
         count[d]<-max(w)-start[d]+1
         result$dimensions[[d]]<-list('type'='height',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(time.i) && time.i==d) {
         time.values<-GSDF.ncdf.convert.time2(v$dim[[d]],
                            default.calendar=default.calendar)
         w<-which(time.values>=min(time.range$date) &
                  time.values<=max(time.range$date))
         if(length(w)==0) return(NULL) # No matching data
         start[d]<-min(w)
         count[d]<-max(w)-start[d]+1
         result$dimensions[[d]]<-list('type'='time',
                     'values'=time.values[seq(start[d],start[d]+count[d]-1)])
         if(!is.null(v$dim[[d]]$calendar)) {
           result$meta$calendar<-GSDF.time.check.calendar(v$dim[[d]]$calendar)
         } else {
           result$meta$calendar<-default.calendar
         }
         next
      }
      if(v$dim[[d]]$name=='nbnds') next # TWCR normals fudge
      stop('Custom dimensions not yet supported')
   }
   slab<-ncvar_get(f,v,start,count)
   result$data<-array(data=slab,dim=count)
   f<-nc_close(f)
   return(result)
}

#' Load a GSDF field from a netCDF file or URL
#'
#' Loads a specified hyperslab (range of lat, lon, height and time)
#'  from a NetCDF file (or an openDAP server)
#'
#' This works only for some netCDF files (netCDF is a very flexible file 
#'  format and it's necessary to make assumptions about how the data is stored);
#'  Does work on most CMIP5 data and reanalyses.
#' Original version with chron dates.
#' 
#' @export
#' @param file Text name of file or URI.
#' @param variable Text name of variable (in file).
#' @param lat.range Two-element vector with max and min latitudes to retrieve.
#'  Leave NULL only if variable has no latitude dimension.
#' @param lon.range Two-element vector with max and min longitudes to retrieve.
#'  Leave NULL only if variable has no longitude dimension.
#' @param height.range Two-element vector with max and min heights to retrieve.
#'  Leave NULL only if variable has no height dimension.
#' @param ens.range Two-element vector with max and min ensemble members to retrieve.
#'  Leave NULL only if variable has no ensemble dimension.
#' @param time.range Two-element vector of chron with max and min times to retrieve.
#'  Leave NULL only if variable has no time dimension.
#' @param default.calendar What calendar does file use (if not specified in file)? 
#'  Supported values are 'gregorian', 'proleptic_gregorian', '365_day', and '360_day'.
#' @param lat.name Text name used by file as label of latitude variable. If NULL, will
#'  try to guess it.
#' @param lon.name Text name used by file as label of longitude variable. If NULL, will
#'  try to guess it.
#' @param height.name Text name used by file as label of height variable. If NULL, will
#'  try to guess it.
#' @param ens.name Text name used by file as label of ensemble number variable. If NULL, will
#'  try to guess it.
#' @param time.name Text name used by file as label of time variable. If NULL, will
#'  try to guess it.
#' @param use.cache Unless FALSE, use a cached version of the data if available (requires
#'   GSDF.cache.dir set and the exact data available in that directory).
#' @return GSDF field with selected subset of file data.
GSDF.ncdf.load<-function(file,variable,lat.range=NULL,lon.range=NULL,
                         height.range=NULL,time.range=NULL,
                         ens.range=NULL,custom.range=NULL,
                         default.calendar='gregorian',
                         lat.name=NULL,lon.name=NULL,
                         height.name=NULL,time.name=NULL,
                         ens.name=NULL,use.cache=TRUE) {
   # Use cached version if possible
   cache.file.name<-NULL
   if(use.cache && exists('GSDF.cache.dir') && !is.null(GSDF.cache.dir) &&
                                               file.info(GSDF.cache.dir)$isdir) {
      cache.file.name<-sprintf("%s.%s",file,variable)
      if(!is.null(lat.range)) cache.file.name<-sprintf("%s.%s.%s",cache.file.name,
                                          as.character(lat.range[1]),as.character(lat.range[2]))
      if(!is.null(lon.range)) cache.file.name<-sprintf("%s.%s.%s",cache.file.name,
                                          as.character(lon.range[1]),as.character(lon.range[2]))
      if(!is.null(height.range)) cache.file.name<-sprintf("%s.%s.%s",cache.file.name,
                                          as.character(height.range[1]),as.character(height.range[2]))
      if(!is.null(time.range)) cache.file.name<-sprintf("%s.%s.%s",cache.file.name,
                                          as.character(time.range[1]),as.character(time.range[2]))
      if(!is.null(ens.range)) cache.file.name<-sprintf("%s.%s.%s",cache.file.name,
                                          as.character(ens.range[1]),as.character(ens.range[2]))
      if(!is.null(custom.range)) cache.file.name<-sprintf("%s.%s.%s",cache.file.name,
                                          as.character(custom.range[1]),as.character(custom.range[2]))
      cache.file.name<-sprintf("%s/%s.Rd",GSDF.cache.dir,gsub('/','.',cache.file.name))
      if(file.exists(cache.file.name) && file.info(cache.file.name)$size>0) {
         load(cache.file.name)
         return(result)
       }
    }
   # No cache- fetch from file
   f<-nc_open(file)
   v<-GSDF.ncdf.get.var(f,variable)
   lat.i<-GSDF.ncdf.get.lat(v,lat.name)
   if(is.null(lat.range) && !is.null(lat.i)) {
      stop('Latitude range required (lat.range=c(-90,90)')
   } 
   lon.i<-GSDF.ncdf.get.lon(v,lon.name)
   if(is.null(lon.range) && !is.null(lon.i)) {
     if(length(v$dim[[lon.i]]$vals)==1) {
      lon.range<-rep(v$dim[[lon.i]]$vals,2)
     } else stop('Longitude range required (lon.range=c(-180,180)')
   } 
   height.i<-GSDF.ncdf.get.height(v,height.name)
   if(is.null(height.range) && !is.null(height.i)) {
     if(length(v$dim[[height.i]]$vals)==1) {
      height.range<-rep(v$dim[[height.i]]$vals,2)
     } else stop('Height range required (height.range=c(850,850)')
   } 
   ens.i<-GSDF.ncdf.get.ens(v,ens.name)
   if(is.null(ens.range) && !is.null(ens.i)) {
     if(length(v$dim[[ens.i]]$vals)==1) {
      ens.range<-rep(v$dim[[ens.i]]$vals,2)
     } else stop('Ensemble range required (ensemble.range=c(1,1)')
   } 
   time.i<-GSDF.ncdf.get.time(v,time.name)
   if(is.null(time.range) && !is.null(time.i)) {
     if(length(v$dim[[time.i]]$vals)==1) {
      time.range<-rep(v$dim[[time.i]]$vals,2)
      default.calendar<-'raw'
     } else stop('Time range required (time.range=c(chron.1,chron.2)')
   }
   result<-GSDF()
   result$meta<-ncatt_get(f,v)
   start<-rep(NA,v$ndim)
   count<-rep(NA,v$ndim)
   for(d in seq(1,v$ndim)) {
      if(!is.null(lat.i) && lat.i==d) {
         start[d]<-min(which(v$dim[[d]]$vals>=min(lat.range) &
                             v$dim[[d]]$vals<=max(lat.range)))
         count[d]<-max(which(v$dim[[d]]$vals>=min(lat.range) &
                             v$dim[[d]]$vals<=max(lat.range)))-start[d]+1
         result$dimensions[[d]]<-list('type'='lat',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(lon.i) && lon.i==d) {
         start[d]<-min(which(v$dim[[d]]$vals>=min(lon.range) &
                             v$dim[[d]]$vals<=max(lon.range)))
         count[d]<-max(which(v$dim[[d]]$vals>=min(lon.range) &
                             v$dim[[d]]$vals<=max(lon.range)))-start[d]+1
         result$dimensions[[d]]<-list('type'='lon',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(height.i) && height.i==d) {
         start[d]<-min(which(v$dim[[d]]$vals>=min(height.range) &
                             v$dim[[d]]$vals<=max(height.range)))
         count[d]<-max(which(v$dim[[d]]$vals>=min(height.range) &
                             v$dim[[d]]$vals<=max(height.range)))-start[d]+1
         result$dimensions[[d]]<-list('type'='height',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(ens.i) && ens.i==d) {
         start[d]<-min(which(v$dim[[d]]$vals>=min(ens.range) &
                             v$dim[[d]]$vals<=max(ens.range)))
         count[d]<-max(which(v$dim[[d]]$vals>=min(ens.range) &
                             v$dim[[d]]$vals<=max(ens.range)))-start[d]+1
         result$dimensions[[d]]<-list('type'='ensemble',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(height.i) && height.i==d) {
         start[d]<-min(which(v$dim[[d]]$vals>=min(height.range) &
                             v$dim[[d]]$vals<=max(height.range)))
         count[d]<-max(which(v$dim[[d]]$vals>=min(height.range) &
                             v$dim[[d]]$vals<=max(height.range)))-start[d]+1
         result$dimensions[[d]]<-list('type'='height',
                    'values'=v$dim[[d]]$vals[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(!is.null(time.i) && time.i==d) {
         time.values<-GSDF.ncdf.convert.time(v$dim[[d]],
                            default.calendar=default.calendar)
         start[d]<-min(which(time.values>=min(time.range) &
                             time.values<=max(time.range)))
         count[d]<-max(which(time.values>=min(time.range) &
                             time.values<=max(time.range)))-start[d]+1
         result$dimensions[[d]]<-list('type'='time',
                     'values'=time.values[seq(start[d],start[d]+count[d]-1)])
         next
      }
      if(v$dim[[d]]$name=='nbnds') next # TWCR normals fudge
      stop('Custom dimensions not yet supported')
   }
   slab<-ncvar_get(f,v,start,count)
   result$data<-array(data=slab,dim=count)
   f<-nc_close(f)
   if(!is.null(cache.file.name)) save(result,file=cache.file.name)
   return(result)
}


#' Write to netCDF file
#'
#' Write a single field to a netCDF file.
#' Overwites anything already in the file.
#'
#' Always uses 'hours since 1800-01-01:00' in the 
#'  gregorian calendar for time fields.
#'
#' @export
#' @param f A GSDF field
#' @param file.name Output file name.
#' @param missval see \code{\link{ncvar_def}}
#' @param prec see \code{\link{ncvar_def}}
#' @param shuffle see \code{\link{ncvar_def}}
#' @param compression see \code{\link{ncvar_def}}
#' @param chunksizes see \code{\link{ncvar_def}}
#' @return 1 if successful, NULL for failure - invisibly.
GSDF.ncdf.write<-function(f,file.name,name='variable',
                          missval=NULL,prec='float',shuffle=FALSE,
                          compression=NA,chunksizes=NA) {

   nc.dims<-list()
   for(i in seq_along(f$dimensions)) {
      if(f$dimensions[[i]]$type=='time') {
         # convert into minutes from first value to store
         origin<-f$dimensions[[i]]$values[1]
         offsets<-GSDF.time.difference(GSDF.time(origin,f$meta$calendar),
                     GSDF.time(f$dimensions[[i]]$values,f$meta$calendar))
         nc.dims[[i]]<-ncdim_def(f$dimensions[[i]]$type,
            sprintf("minutes since %s",origin),
            offsets)
      }
      if(f$dimensions[[i]]$type=='lat') {
         nc.dims[[i]]<-ncdim_def(f$dimensions[[i]]$type,
            'degrees north',
            f$dimensions[[i]]$values,
            longname='latitude')
      }
      if(f$dimensions[[i]]$type=='lon') {
         nc.dims[[i]]<-ncdim_def(f$dimensions[[i]]$type,
            'degrees east',
            f$dimensions[[i]]$values,
            longname='longitude')
      }
      if(f$dimensions[[i]]$type=='ensemble') {
         nc.dims[[i]]<-ncdim_def(f$dimensions[[i]]$type,
            'member',
            f$dimensions[[i]]$values)
      }
      if(f$dimensions[[i]]$type!='ensemble' &&
         f$dimensions[[i]]$type!='lon' &&
         f$dimensions[[i]]$type!='lat' &&
         f$dimensions[[i]]$type!='time') {
         nc.dims[[i]]<-ncdim_def(f$dimensions[[i]]$type,
            'other',
            f$dimensions[[i]]$values)
      }
   }
   v<-tryCatch({
      ncvar_def(name,'',nc.dims,
                missval=missval,
                prec=prec,
                compression=compression,
                chunksizes=chunksizes)},
      warning=function(w) {
         warning('Problem creating nc4 variable',w)
      },
      error=function(e) {
         warning('Failed to create nc4 variable',e)
         return(invisible(NULL))
      })
         
   nf<-tryCatch({
     nc_create(file.name,v)},
      warning=function(w) {
         warning('Problem creating netCDF file',file.name,w)
      },
      error=function(e) {
         warning('Failed to create netCDF file',file.name,e)
         return(invisible(NULL))
      })
   np<-tryCatch({
      ncvar_put(nf,v,f$data)},
      warning=function(w) {
         warning('Problem writing to netCDF file',file.name,w)
      },
      error=function(e) {
         warning('Failed to write to netCDF file',file.name,e)
         return(invisible(NULL))
      })
  # Add the metadata
   for(i in names(f$meta)) {
       # A couple of these cause horrible problems - I don't know why
       #if(i=='missing_value') next
       #if(i=='_FillValue') next
       np<-tryCatch({
	  ncatt_put(nf,0,i,f$meta[[i]])},
	  warning=function(w) {
	     warning('Problem writing attribute to netCDF file',file.name,w)
	  },
	  error=function(e) {
	     warning('Failed to write attribute to netCDF file',file.name,e)
	     return(invisible(NULL))
	  })
   }
   nc_close(nf)
   return(invisible(1))
}

# TWCR uses different variable names in the .nc files from
#  in the names of the files (sometimes). So finding the
#  corect variable is not always trivial.
GSDF.ncdf.get.var<-function(f,variable) {
   v<-f$var[[variable]]
   if(!is.null(v)) return(v)
   v2<-gsub('\\..+$','',variable)
   v<-f$var[[v2]]
   if(is.null(v)) {
     if(v2=='prmsl') v<-f$var[['slp']]
     if(is.null(v)) stop(sprintf("Can't get variable %s",variable))
   }
   return(v)
}
         
# Want to be able to identify some special dimensions
GSDF.ncdf.is.lat <- function(dimension,lat.name) {
    if(!is.null(lat.name)) {
      if(regexpr(lat.name,dimension$name,ignore.case = T)>0) return(TRUE)
      return(FALSE)
    }
    if(regexpr('lat',dimension$name,ignore.case = T)>0 ||
       regexpr('YDim',dimension$name,ignore.case = T)>0) return(TRUE)
    return(FALSE)
}
GSDF.ncdf.is.lon <- function(dimension,lon.name) {
    if(!is.null(lon.name)) {
      if(regexpr(lon.name,dimension$name,ignore.case = T)>0) return(TRUE)
      return(FALSE)
    }
    if(regexpr('lon',dimension$name,ignore.case = T)>0 ||
       regexpr('XDim',dimension$name,ignore.case = T)>0) return(T)
    return(F)
}
GSDF.ncdf.is.height <- function(dimension,height.name) {
    if(!is.null(height.name)) {
      if(regexpr(height.name,dimension$name,ignore.case = T)>0) return(TRUE)
      return(FALSE)
    }
    if(regexpr('lev',dimension$name,ignore.case = T)>0 ||
       regexpr('Height',dimension$name,ignore.case = T)>0 ) return(T)
    return(F)
}
GSDF.ncdf.is.time <- function(dimension,time.name) {
    if(!is.null(time.name)) {
      if(regexpr(time.name,dimension$name,ignore.case = T)>0) return(TRUE)
      return(FALSE)
    }
    if(regexpr('time',dimension$name,ignore.case = T)>0) return(T)
    return(F)
}
GSDF.ncdf.is.ens <- function(dimension,ens.name) {
    if(!is.null(ens.name)) {
      if(regexpr(ens.name,dimension$name,ignore.case = T)>0) return(TRUE)
      return(FALSE)
    }
    if(regexpr('ens',dimension$name,ignore.case = T)>0) return(T)
    return(F)
}

# Given a variable, get the latitude values
# Pass an instance of class 'var.ncdf' (?var.def.ncdf)
GSDF.ncdf.get.lat <- function(variable,lat.name) {
   for(i in seq(1,variable$ndims)) {
      if(GSDF.ncdf.is.lat(variable$dim[[i]],lat.name)) return(i)
   }
   return(NULL)
}
# Same for longitude
GSDF.ncdf.get.lon <- function(variable,lon.name) {
   for(i in seq(1,variable$ndims)) {
      if(GSDF.ncdf.is.lon(variable$dim[[i]],lon.name)) return(i)
   }
   return(NULL)
}
# Same for pressure level
GSDF.ncdf.get.height <- function(variable,height.name) {
   for(i in seq(1,variable$ndims)) {
      if(GSDF.ncdf.is.height(variable$dim[[i]],height.name)) return(i)
   }
   return(NULL)
}
# Same for ensemble number
GSDF.ncdf.get.ens <- function(variable,ens.name) {
   for(i in seq(1,variable$ndims)) {
      if(GSDF.ncdf.is.ens(variable$dim[[i]],ens.name)) return(i)
   }
   return(NULL)
}
# Same for date/time
GSDF.ncdf.get.time <- function(variable,time.name) {
   for(i in seq(1,variable$ndims)) {
      if(GSDF.ncdf.is.time(variable$dim[[i]],time.name)) return(i)
   }
   return(NULL)
}

# Convert the time to chron
GSDF.ncdf.convert.time <- function(dim,default.calendar) {
    if(default.calendar=='raw') {
      return(dim$vals) # Special case - no conversion
    }
    units<-dim$units
    if(!is.null(dim$calendar)) {
       return(GSDF.ncdf.offset.to.date(dim$vals,dim$units,
				       dim$calendar))
    } else {
       return(GSDF.ncdf.offset.to.date(dim$vals,dim$units,
				       default.calendar))	
    }
}
# Convert the time to GSDF.time
GSDF.ncdf.convert.time2 <- function(dim,default.calendar) {
    if(default.calendar=='raw') {
      return(dim$vals) # Special case - no conversion
    }
    units<-dim$units
    if(!is.null(dim$calendar)) {
       return(GSDF.ncdf.offset.to.date2(dim$vals,dim$units,
				       dim$calendar))
    } else {
       return(GSDF.ncdf.offset.to.date2(dim$vals,dim$units,
				       default.calendar))	
    }
}  

# Turn a date offset, start point and calendar choice into a GSDF date
GSDF.ncdf.offset.to.date2<-function(offset,start,calendar) {
  step<-NA
  if(regexpr('hours since',start,ignore.case = T)>0) step<-'hours'
  if(regexpr('days* since',start,ignore.case = T)>0) step<-'days'
  if(regexpr('minutes* since',start,ignore.case = T)>0) step<-'minutes'
  if(is.na(step)) stop(paste("Unsupported date step",start))
  result<-GSDF.time.from.base.and.offset(offset,start,step,calendar)
  return(result$date)
}

# Turn a date offset, start point and calendar choice into a chron date
GSDF.ncdf.offset.to.date<-function(offset,start,calendar) {
  start.year<-as.integer(sub('\\D*(\\d+)\\D(\\d+)\\D(\\d+).*','\\1',start))
  if(is.na(start.year)) stop(paste("Unsupported date initial point",start))
  start.month<-as.integer(sub('\\D*(\\d+)\\D(\\d+)\\D(\\d+).*','\\2',start))
  start.day<-as.integer(sub('\\D*(\\d+)\\D(\\d+)\\D(\\d+).*','\\3',start))
  start.hour<-as.integer(sub('\\D*(\\d+)\\D(\\d+)\\D(\\d+)\\D(\\d+).*','\\4',start))
  if(is.na(start.hour)) start.hour<-0
  # start.minute<-as.integer(sub('\\D*(\\d+)\\D(\\d+)\\D(\\d+)\\D(\\d+)\\D(\\d+).*','\\5',start))
  # if(is.na(start.minute)) start.minute<-0 # Hours only - ignore minutes
  start.minute<-0
  step<-NA
  if(regexpr('hours since',start,ignore.case = T)>0) step<-'hours'
  if(regexpr('days* since',start,ignore.case = T)>0) step<-'days'
  if(regexpr('minutes* since',start,ignore.case = T)>0) step<-'minutes'
  if(step=='days') offset<-offset*24
  if(step=='minutes') offset<-offset/60
  if(is.na(step)) stop(paste("Unsupported date step",start))
  if(calendar=='360_day') {
    in.hours<-offset+start.year*24*30*12+(start.month-1)*24*30+(start.day-1)*24+
                     start.hour+start.minute/60
    year<-as.integer(in.hours/(24*30*12))
    in.hours<-in.hours-year*24*30*12
    month<-as.integer(in.hours/(24*30))+1
    in.hours<-in.hours-(month-1)*24*30
    day<-as.integer(in.hours/24)+1
    in.hours<-in.hours-(day-1)*24
    hour<-as.integer(in.hours)
    in.hours<-in.hours-hour
    minute<-as.integer(in.hours*60)
    in.hours<-in.hours-minute/60
    second<-as.integer(in.hours*60*60)
    print(sprintf("%02d/%02d/%04d %02d:%02d:%02d",
                   month,day,year,hour,minute,second))
    return(chron(sprintf("%02d/%02d/%04d %02d:%02d:%02d",
                   month,day,year,hour,minute,second)))    
  }
  if(calendar=='noleap' || calendar=='365_day') {
    m.length<-c(31,28,31,30,31,30,31,31,30,31,30,31)
    m.tostart<-c(0,31,59,90,120,151,181,212,243,273,304,334)
    in.hours<-offset+start.year*24*365+m.tostart[start.month]*24+(start.day-1)*24+
              start.hour+start.minute/60
    year<-as.integer(in.hours/(24*365))
    in.hours<-in.hours-year*24*365
    month<-rep(1,length(year))
    for(mt in seq(1,length(month))) {
       for(m in seq(2,12)) if(in.hours[mt]>m.tostart[m]*24) month[mt]<-m
    }
    in.hours<-in.hours-m.tostart[month]*24
    day<-as.integer(in.hours/24)+1
    in.hours<-in.hours-(day-1)*24
    hour<-as.integer(in.hours)
    in.hours<-in.hours-hour
    minute<-as.integer(in.hours*60)
    in.hours<-in.hours-minute/60
    second<-as.integer(in.hours*60*60)
    return(chron(sprintf("%02d/%02d/%04d %02d:%02d:%02d",
          month,day,year,hour,minute,second)))    
  }
  if(calendar=='gregorian' || calendar=='standard' || calendar=='julian') { # GFDL claims to be Julian - surely lying.
    offset<-offset+start.hour+start.minute/60
    base<-chron(as.numeric(as.Date(sprintf("%02d-%02d-%04d",start.day,start.month,start.year), format = "%d-%m-%Y")),
                sprintf("%02d:%02d:%02d",0,0,0),format = c(dates = "m/d/y", times = "h:m:s"))
    offset<-chron(as.numeric(base)+offset/24)
    return(offset)
  }
  if(calendar=='proleptic_gregorian') { 
    # Shift the calendar into the Gregorian era for processing, and then back for output
    shift<-0
    while(start.year+shift<1582) shift<-shift+400
    base<-chron(sprintf("%02d/%02d/%04d",start.month,start.day,start.year),
                sprintf("%02d:%02d:%02d",0,0,0),format = c(dates = "m/d/y", times = "h:m:s"))
    offset<-chron(as.numeric(base)+offset/24-400*365-97)
    return(offset)    
  }
  stop(paste("Unsupported calendar",calendar))
}


