# Scripts to retrieve ERA Interim data from the ECMWF public data API.

# Pull down ERA Interim data for a month from ECMWF (to $SCRATCH)
# Needs ECMWF account and python script access setup
# https://software.ecmwf.int/wiki/display/WEBAPI/Python+ERA-interim+examples

# Supported varibles - mapping between 20CR name and ERA code

# Analyis variables
ERAI.vars.a<-list(air.2m   = '167.128',
                  uwnd.10m = '165.128',
	          vwnd.10m = '166.128',
	          prmsl    = '151',
	          icec     = '31',
	          sst      = '34')

# Forecast variables
ERAI.vars.f<-list(prate    = '228')

#' Get the data for one variable for one month
#'
#' Use 20CR variable names, store the data on $SCRATCH
#'
#'
#' @export
#' @param var - 20CR variable name
#' @param year - Year for data
#' @param month - month for data (1-12)
ERAI.fetch.data.for.month<-function(var,year,month) {
    if(var %in% ERAI.monolevel.analysis) {
        return(ERAI.fetch.analysis.data.for.month(var,year,month))
    }
    if(var %in% ERAI.monolevel.forecast) {
        return(ERAI.fetch.forecast.data.for.month(var,year,month))
    }
    stop(sprintf("Unsupported variable %s",var)) 
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
#' @param year - Year for data
#' @param month - month for data (1-12)
ERAI.fetch.analysis.data.for.month<-function(var,year,month) {

    if(!var %in% ERAI.monolevel.analysis) {
        stop(sprintf("Unsupported analysis variable %s",var))
    }
    target.dir<-sprintf("%s/hourly/%04d/%02d",ERAI.get.data.dir(),year,month)
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
    cat('   \'dataset\'   : "interim",\n')
    cat('   \'stream\'    : "oper",\n')
    cat('   \'type\'      : "an",\n')
    cat('   \'step\'      : "0",\n')
    cat('   \'class\'     : "ei",\n')
    cat('   \'levtype\'   : "sfc",\n')
    cat(sprintf("   \'param\'     : \"%s\",\n",ERAI.vars.a[[var]]))
    cat('   \'grid\'      : "0.75/0.75",\n')
    cat('   \'time\'      : "00/06/12/18",\n')
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
#' @param year - Year for data
#' @param month - month for data (1-12)
ERAI.fetch.forecast.data.for.month<-function(var,year,month) {

    if(!var %in% ERAI.monolevel.forecast) {
        stop(sprintf("Unsupported forecast variable %s",var))
    }
    target.dir<-sprintf("%s/hourly/%04d/%02d",ERAI.get.data.dir(),year,month)
    if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)

    # Need two sets of forecast data - from the runs at 0 and 12
    for(start.hour in c(0,12)) {

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
        cat('   \'dataset\'   : "interim",\n')
        cat('   \'stream\'    : "oper",\n')
        cat('   \'type\'      : "fc",\n')
        cat('   \'step\'      : "3/6/9/12/15/18",\n')
        cat('   \'class\'     : "ei",\n')
        cat('   \'levtype\'   : "sfc",\n')
        cat(sprintf("   \'param\'     : \"%s\",\n",ERAI.vars.f[[var]]))
        cat('   \'grid\'      : "0.75/0.75",\n')
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
