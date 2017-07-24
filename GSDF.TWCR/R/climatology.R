# Functions to make (and access) climatologies from the 20CR data
#  Uses V3 (monthly) files.
# Uses ensemble data, not mean and SD.

#' TWCR get file name for hourly climatological data
#'
#' Get the file name for selected variable and date (hourly data)
#' Special normals-only function for miultiple normals over
#'  different date ranges
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any supported variable
#' @param first.year 1st year of climatology (NULL, default, for 1981)
#' @param last.year Last year of climatology (NULL, default, for 2010)
#' @return File containing the requested data 
TWCR.climatology.get.file.name<-function(variable,month,version,first.year=NULL,last.year=NULL) {
    base.dir<-TWCR.get.data.dir(version)
    dir.name<-sprintf("%s/normals/hourly/%02d",base.dir,month)
    if(!is.null(first.year) || !is.null(last.year)) {
      dir.name<-sprintf("%s/normals.%04d-%04d/hourly/%02d",base.dir,first.year,last.year,month)
    }
    file.name<-sprintf("%s/%s.nc",dir.name,variable)
    return(file.name)
}
#' Make (and store) a climatological average 
#'
#' Data are stored as .nc files in a parallel directory to the original
#'  data - same as original data exept only for 1 year.
#' Loads a whole month of data at once
#'
#' Chose to use 1981 as the storage year in the netcdf file.
#'
#' @export
#' @param variable  - 'air.2m', 'prmsl', 'prate', - 20CR names
#' @param month  - 1-12, month to make climatology for.
#' @param version - 
#'   If NULL (default) run for all months
#' @return Nothing - a climatological field will be created as a side effect.
TWCR.make.climatology<-function(variable,month=NULL,version,first.year=1981,last.year=2010) {

  for(mnth in seq(1,12)) {
    if(!is.null(month) && month != mnth) next
    
  # Load the data 1 year at a time.
  result<-NULL
  for(year in seq(first.year,last.year)) {

      mnth.file<-TWCR.hourly.members.get.file.name(variable,year,mnth,15,0,version=version)
      if(!file.exists(mnth.file)) {
        stop("Missing data for %s %04d-%02d",variable,year,mnth)
      }    
       t<-chron(sprintf("%04d/%02d/%02d",year,mnth,1),
                sprintf("%02d:00:00",0),
                format=c(dates='y/m/d',times='h:m:s'))
       nmd<-lubridate::days_in_month(as.POSIXct(t))
      if(nmd==29) nmd<-28 # death to leap years
       t2<-chron(sprintf("%04d/%02d/%02d",year,mnth,nmd),
                 sprintf("%02d:59:59",23),
                 format=c(dates='y/m/d',times='h:m:s'))
      if(version==2 || version=='3.2.1' || version=='2c' || version=='3.5.1') {
         t<-t+2 # Correct Julian->Gregorian calendar
         t2<-t2+2
      }
       ens.range=c(0,56)
       if(substr(version,1,1)=='4') ens.range=c(0,80)
       var<-variable
       if(substr(version,1,1)!='4' && variable=='air.2m') var<-'t9950'
       slab<-GSDF.ncdf.load(mnth.file,
                            var,
                            lat.range=c(-90,90),lon.range=c(0,360),
                            ens.range=ens.range,
                            time.range=c(t,t2))
    if(is.null(slab)) {
       stop("Data for climatology is not on disc.")
     }
    # Average over ensemble members
     slab<-GSDF.reduce.1d(slab,'ensemble',mean)
     gc(verbose=FALSE )
     if(is.null(result)) {
       result<-slab
     } else {
       result$data[]<-result$data+slab$data
     }
     gc(verbose=FALSE )

    }
    result$data[]<-result$data/(last.year-first.year+1)

      # set the year to 1981
      t.i<-GSDF.find.dimension(result,'time')
      if(version==2 || version=='3.2.1' || version=='2c' || version=='3.5.1') {
        # Back to the 
        result$dimensions[[t.i]]$values<-result$dimensions[[t.i]]$values-2
      }
      m<-stringr::str_match(as.POSIXlt(result$dimensions[[t.i]]$values,tz='GMT'),
                   "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
      result$dimensions[[t.i]]$values<-sprintf("1981-%s-%s:%s:%s",
                                               m[,3],m[,4],m[,5],m[,6])
      result$meta$calendar='gregorian'

      # Write the result to a disc location paralleling the data
      fn<-TWCR.climatology.get.file.name(variable,mnth,version,first.year,last.year)
      if(!file.exists(dirname(fn))) dir.create(dirname(fn),recursive=TRUE)

      GSDF.ncdf.write(result,fn,name=variable)
   }
}  

