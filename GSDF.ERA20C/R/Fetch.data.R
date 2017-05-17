# Scripts to retrieve ERA_20C data from the ECMWF public data API.

# Pull down ERA_20C data for a month from ECMWF (to $SCRATCH)
# Needs ECMWF account and python script access setup
# https://software.ecmwf.int/wiki/display/WEBAPI/Python+ERA-interim+examples

# Supported varibles - mapping between 20CR name and ERA code

# Analyis variables
ERA20C.vars.a<-list(air.2m   = '167.128',
                    uwnd.10m = '165.128',
	            vwnd.10m = '166.128',
	            prmsl    = '151.128',
	            icec     = '31',
	            sst      = '34')

# Forecast variables
ERA20C.vars.f<-list(prate    = '228.128')

#' Get the data for one variable for one month
#'
#' Use 20CR variable names, store the data on $SCRATCH
#'
#'
#' @export
#' @param var - 20CR variable name
#' @param year
#' @param month
ERA20C.fetch.data.for.month<-function(var,year,month) {
    if(!is.null(ERA20C.vars.a[[var]])) {
        return(ERA20C.fetch.analysis.data.for.month(var,year,month))
    }
    if(!is.null(ERA20C.vars.f[[var]])) {
        return(ERA20C.fetch.forecast.data.for.month(var,year,month))
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
#' @param year
#' @param month
ERA20C.fetch.analysis.data.for.month<-function(var,year,month) {

    if(is.null(ERA20C.vars.a[[var]])) {
        stop(sprintf("Unsupported analysis variable %s",var))
    }
    target.dir<-sprintf("%s/hourly/%04d/%02d",ERA20C.get.data.dir(),year,month)
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
    cat('   \'dataset\'   : "era20c",\n')
    cat('   \'stream\'    : "oper",\n')
    cat('   \'type\'      : "an",\n')
    cat('   \'step\'      : "0",\n')
    cat('   \'class\'     : "ei",\n')
    cat('   \'levtype\'   : "sfc",\n')
    cat('   \'grid\'      : "1.25/1.25",\n')
    cat('   \'time\'      : "00/03/06/09/12/15/18/21",\n')
    cat(sprintf("   \'param\'     : \"%s\",\n",ERA20C.vars.a[[var]]))
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
ERA20C.fetch.forecast.data.for.month<-function(var,year,month) {

    if(is.null(ERA20C.vars.f[[var]])) {
        stop(sprintf("Unsupported analysis variable %s",var))
    }
    target.dir<-sprintf("%s/hourly/%04d/%02d",ERA20C.get.data.dir(),year,month)
    if(!file.exists(target.dir)) dir.create(target.dir,recursive=TRUE)

    # Want 27 hours of forecast for each day (so we can interpolate over the seam),
    #  but the 27-hr forcast from one day has the same validity time as the 3-hr
    #  forecast from the next day - so need to download them separately and store

    # First 24-hours of forecast in main file
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
    cat('   \'dataset\'   : "era20c",\n')
    cat('   \'stream\'    : "oper",\n')
    cat('   \'type\'      : "fc",\n')
    cat('   \'step\'      : "3/6/9/12/15/18/21/24",\n')
    cat('   \'class\'     : "ei",\n')
    cat('   \'levtype\'   : "sfc",\n')
    cat('   \'grid\'      : "1.25/1.25",\n')
    cat('   \'time\'      : "06",\n')
    cat(sprintf("   \'param\'     : \"%s\",\n",ERA20C.vars.f[[var]]))
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
    
    target.file<-(sprintf("%s/%s.p1d.nc",target.dir,var))
    if(file.exists(target.file) && file.info(target.file)$size>0) return() # already done
    
    sink(fname)
    cat('#!/usr/bin/env python\n')
    cat('from ecmwfapi import ECMWFDataServer\n')
    cat('server = ECMWFDataServer()\n')
    cat('server.retrieve({\n')
    cat('   \'dataset\'   : "era20c",\n')
    cat('   \'stream\'    : "oper",\n')
    cat('   \'type\'      : "fc",\n')
    cat('   \'step\'      : "27",\n')
    cat('   \'class\'     : "ei",\n')
    cat('   \'levtype\'   : "sfc",\n')
    cat('   \'grid\'      : "1.25/1.25",\n')
    cat('   \'time\'      : "06",\n')
    cat(sprintf("   \'param\'     : \"%s\",\n",ERA20C.vars.f[[var]]))
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
