# Functions to make (and access) climatologies from the ERA Interim data

#' ERAI get file name for hourly climatological data
#'
#' Get the file name for selected variable and date (hourly data)
#'
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any supported variable
#' @param first.year 1st year of climatology (NULL, default, for 1981)
#' @param last.year Last year of climatology (NULL, default, for 2010)
#' @return File containing the requested data 
ERAI.climatology.get.file.name<-function(variable,month,first.year=NULL,last.year=NULL) {
    base.dir<-ERAI.get.data.dir()
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
#'  data - same as original data except only for 1 year.
#' Loads a whole month of data at once
#'
#' Chose to use 1981 as the storage year in the netcdf file.
#'
#' @export
#' @param variable  - 'air.2m', 'prmsl', 'prate', - 20CR names
#' @param month  - 1-12, month to make climatology for.
#'   If NULL (default) run for all months
#' @param first.year 1st year of climatology (NULL, default, for 1981)
#' @param last.year Last year of climatology (NULL, default, for 2010)
#' @return Nothing - a climatological field will be created as a side effect.
ERAI.make.climatology<-function(variable,month=NULL,first.year=1981,last.year=2010) {

  for(mnth in seq(1,12)) {
    if(!is.null(month) && month != mnth) next
    
  # Load the data 1 year at a time.
  result<-NULL
  for(year in seq(first.year,last.year)) {

      mnth.file<-ERAI.hourly.get.file.name(variable,year,mnth,15,0)
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
       slab<-GSDF.ncdf.load(mnth.file,
                            ERAI.translate.for.variable.names(variable),
                            lat.range=c(-90,90),lon.range=c(0,360),
                            time.range=c(t,t2))
     if(is.null(slab)) {
       stop("Data for climatology is not on disc.")
     }
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
      m<-stringr::str_match(as.POSIXlt(result$dimensions[[t.i]]$values,tz='GMT'),
                   "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
      result$dimensions[[t.i]]$values<-sprintf("1981-%s-%s:%s:%s",
                                               m[,3],m[,4],m[,5],m[,6])
      result$meta$calendar='gregorian'

      # Write the result to a disc location paralleling the data
      fn<-ERAI.climatology.get.file.name(variable,mnth,first.year,last.year)
      if(!file.exists(dirname(fn))) dir.create(dirname(fn),recursive=TRUE)

      GSDF.ncdf.write(result,fn,name=ERAI.translate.for.variable.names(variable))
   }
}  

