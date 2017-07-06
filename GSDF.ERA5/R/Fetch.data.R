# Scripts to retrieve ERA5 data from the ECMWF public data API.

# Pull down ERA5 data for a month from ECMWF (to $SCRATCH)
# Needs ECMWF account and python script access setup
# https://software.ecmwf.int/wiki/display/WEBAPI/Python+ERA-interim+examples

#' Get the data for one variable for one month
#'
#' Use 20CR variable names, store the data on $SCRATCH
#'
#'
#' @export
#' @param var - 20CR variable name
#' @param year
#' @param month
#' @param stream - 'ensda' for the lower resolution ensemble, otherwise 'oper' (default)
ERA5.fetch.data.for.month<-function(var,year,month,stream='oper') {
    if(var %in% ERA5.monolevel.analysis) {
        return(ERA5.fetch.analysis.data.for.month(var,year,month))
    }
    if(var %in% ERA5.monolevel.forecast) {
        return(ERA5.fetch.forecast.data.for.month(var,year,month))
    }
    stop(sprintf("Unsupported variable %s",var)) 
}

#' Translate to ERA5 variable names in the data access api
#'
#' I'm standardising on 20CR variable names - map to ERA5 choices
#'
#' ERA5 uses different names for the files and the variable in the file
#'  this function maps to ERA5 file names.
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
#' Get data for one analysis variable
#'
#' I'm packaging data by the month, a compromise
#'   between speed, space, & simplicity.
#'
#' Won't replace data already on disc - just
#'   returns immediately if the result file is already there.
#'
#' @export
#' @param var - 20CR variable name
#' @param year
#' @param month
#' @param stream - 'ensda' for the lower resolution ensemble, otherwise 'oper' (default)
ERA5.fetch.analysis.data.for.month<-function(var,year,month,stream='oper') {

    if(!var %in% ERA5.monolevel.analysis) {
        stop(sprintf("Unsupported analysis variable %s",var))
    }
    target.dir<-sprintf("%s/%s/hourly/%04d/%02d",ERA5.get.data.dir(),stream,year,month)
    if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)
    target.file<-(sprintf("%s/%s.nc",target.dir,var))
    if(file.exists(target.file) && file.info(target.file)$size>0) {
      return('Already done')
    }

    # Retrieval is done by Python script - put in a temporary file
    fname<-tempfile()
    sink(fname)
    cat('#!/usr/bin/env python\n')
    cat('from ecmwfapi import ECMWFDataServer\n')
    cat('server = ECMWFDataServer()\n')
    cat('server.retrieve({\n')
    cat('   \'dataset\'   : "era5",\n')
    cat(sprintf("   \'stream\'    : \"%s\",\n",stream))
    cat('   \'type\'      : "an",\n')
    cat('   \'levtype\'   : "sfc",\n')
    cat(sprintf("   \'param\'     : \"%s\",\n",
                ERA5.translate.for.file.names(var)))
    cat('   \'grid\'      : "0.25/0.25",\n')
    cat('   \'time\'      : "0/to/23/by/1",\n')
    cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                year,month,1,
                year,month,
                days_in_month(ymd(sprintf("%04d-%02d-01",year,month)))))
    cat('   \'format\'    : "netcdf",\n')
    cat(sprintf("   'target'    : \"%s\",\n",target.file))
    cat('})')
    sink()
    system(sprintf("python %s",fname))
    unlink(fname)
}


#' Get data for one forecast variable
#'
#' I'm packaging data by the month, a compromise
#'   between speed, space, & simplicity.
#'
#' Won't replace data already on disc - just
#'   returns immediately if the result file is already there.
#'
#' @export
#' @param var - 20CR variable name
#' @param year
#' @param month
#' @param stream - 'ensda' for the lower resolution ensemble, otherwise 'oper' (default)
ERA5.fetch.forecast.data.for.month<-function(var,year,month,stream='oper') {

    if(!var %in% ERA5.monolevel.forecast) {
        stop(sprintf("Unsupported forecast variable %s",var))
    }
    target.dir<-sprintf("%s/%s/hourly/%04d/%02d",ERA5.get.data.dir(),stream,year,month)
    if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)

    # Need two sets of forecast data - from the runs at 6 and 18
    for(start.hour in c(6,18)) {

        target.file<-(sprintf("%s/%s.%02d.nc",target.dir,var,start.hour))
        if(file.exists(target.file) && file.info(target.file)$size>0) {
          return('Already done')
        }

        # Retrieval is done by Python script - put in a temporary file
        fname<-tempfile()
        sink(fname)
        cat('#!/usr/bin/env python\n')
        cat('from ecmwfapi import ECMWFDataServer\n')
        cat('server = ECMWFDataServer()\n')
        cat('server.retrieve({\n')
        cat('   \'dataset\'   : "era5",\n')
        cat(sprintf("   \'stream\'    : \"%s\",\n",stream))
        cat('   \'type\'      : "fc",\n')
        cat('   \'step\'      : "0/to/18/by/1",\n')
        cat('   \'levtype\'   : "sfc",\n')
        cat(sprintf("   \'param\'     : \"%s\",\n",ERA5.translate.for.file.names(var)))
        cat('   \'grid\'      : "0.25/0.25",\n')
        cat(sprintf("   \'time\'      : \"%02d\",\n",start.hour))
        cat(sprintf("   'date'      : \"%04d-%02d-%02d/to/%04d-%02d-%02d\",\n",
                    year,month,1,
                    year,month,
                    days_in_month(ymd(sprintf("%04d-%02d-01",year,month)))))
        cat('   \'format\'    : "netcdf",\n')
        cat(sprintf("   'target'    : \"%s\",\n",target.file))
        cat('})')
        sink()
        system(sprintf("python %s",fname))
        unlink(fname)
        
    }
}
