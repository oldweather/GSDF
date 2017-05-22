# Functions to make (and access) climatologies from the CERA20C data


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
#'   If NULL (default) run for all months
#' @return Nothing - a climatological field will be created as a side effect.
CERA20C.make.climatology<-function(variable,month=NULL,first.year=1981,last.year=2010) {

  for(mnth in seq(1,12)) {
    if(!is.null(month) && month != mnth) next
    
  print(mnth)
  # Load the data 1 year at a time.
  result<-NULL
  for(year in seq(first.year,last.year)) {

     print(year)
      mnth.file<-CERA20C.hourly.get.file.name(variable,year,mnth,15,0)
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
                            CERA20C.translate.for.variable.names(variable),
                            lat.range=c(-90,90),lon.range=c(0,360),
                            ens.range=rep(0,9),ens.name='number',
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
    # Average over ensemble members
    result<-GSDF.reduce.1d(result,'ensemble',mean)
    result$data[]<-result$data/(last.year-first.year+1)

      # set the year to 1981
      t.i<-GSDF.find.dimension(result,'time')
      m<-stringr::str_match(as.POSIXlt(result$dimensions[[t.i]]$values,tz='GMT'),
                   "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")

      result$dimensions[[t.i]]$values<-sprintf("1981-%s-%s:%s:%s",
                                               m[,3],m[,4],m[,5],m[,6])
      result$meta$calendar='gregorian'

      # Write the result to a disc location paralleling the data
      fn<-CERA20C.hourly.get.file.name(variable,year,mnth,15,0,type='normal')
      if(!file.exists(dirname(fn))) dir.create(dirname(fn),recursive=TRUE)

      GSDF.ncdf.write(result,fn,name=CERA20C.translate.for.variable.names(variable))
   }
}  

