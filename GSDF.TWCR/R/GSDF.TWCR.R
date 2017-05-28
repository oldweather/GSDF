# Functions for getting data from the 20th Century Reanalysis

# names and classes of variables
TWCR.monolevel<-c('air.sig995','air.tropo','cape','cin','cldwtr.eatm','hgt.tropo',
                  'omega.sig995','pottmp.sig995','pr_wtr.eatm','pres.sfc','pres.tropo',
                  'prmsl','rhum.sig995','tco3.eatm','uwnd.sig995','uwnd.tropo',
                  'vwnd.sig995','vwnd.tropo')
TWCR.gaussian.monolevel<-c('air.2m','air.sfc','albedo','cprat','cwork.eatm','dlwrf.sfc',
                           'dswrf.sfc','gflux','hpbl','icec','icet','lhtfl','pevpr',
                           'pr_wtr.eatm','prate','press.sfc','runoff','shtfl','shum',
                           'snod','snowc','soilm','ssrunoff','tcdc.bndrylyr','tcdc.convcld',
                           'tcdc.eatm','tcdc.lowcld','tcdc.midcld','tcdc.topcld','tmax.2m',
                           'tmin.2m','trans','uflx','ugwd','ulwrf.ntat','ulwrf.sfc','uswrf.ntat',
                           'uswf.sfc','uwnd.10m','vflx','vgwd','vwnd.10m','weasd','cduvb')
TWCR.pressure.level<-c('air','hgt','omega','rhum','shum','uwnd','vwnd')
# Also subsurface soil - not currently supported

# Height of each level in hPa
TWCR.heights<-c(1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,
                 250,200,150,100,70,50,30,20,10)

#' TWCR show variables
#'
#' List the variables available from 20CR
#'
#' 3 Different grids: 'Monolevel' and 'Gaussian' are both
#' Lat x Lon x Time but with different grids, 'Pressure level'
#' is  Lat x Lon x Height x Time. No parameters or return value
#' prints out a list of available variables.
#' @export
TWCR.show.variables<-function() {
  print('Monolevel')
  print(TWCR.monolevel)
  print('Gaussian')
  print(TWCR.gaussian.monolevel)
  print('Pressure level')
  print(TWCR.pressure.level)
}

#' TWCR get data directory
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
TWCR.get.data.dir<-function(version=2) {

    if(file.exists(sprintf("%s/20CR/version_%s/",Sys.getenv('SCRATCH'),version))) {
            return(sprintf("%s/20CR/version_%s/",Sys.getenv('SCRATCH'),version))
    }	
    if(file.exists(sprintf("/project/projectdirs/m958/netCDF.data/20CR_v%s/",version))) {
            return(sprintf("/project/projectdirs/m958/netCDF.data/20CR_v%s/",version))
    }	
    return(NULL)
}

# Get class of variable: monolevel, pressure-level, gaussian
#  They have different locations on the openDAP server.
TWCR.get.variable.group<-function(variable) {
  if(length(which(TWCR.monolevel==variable))>0) return('monolevel')
  if(length(which(TWCR.gaussian.monolevel==variable))>0) return('gaussian')
  if(length(which(TWCR.pressure.level==variable))>0) return('pressure')
  stop(sprintf("Unrecognised variable: %s",variable))
}

#' TWCR get file name (hourly)
#'
#' Get the file name (or URL) for selected variable and date (hourly data)
#'
#' Called internally by \code{TWCR.get.slice.at.hour} but also useful
#'  called directly - you can then access the data with another tool.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any 20CR variable
#' @param type - 'mean', 'spread', 'normal', or 'standard.deviation'. Also
#'    'fg.mean' and 'fg.spread' for first guess fields.
#'  Note that standard deviations are not available over opendap.
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them),
#'  NULL (default) will use local files if available, and network if not.
#' @return File name or URL for netCDF file containing the requested data 
TWCR.hourly.get.file.name<-function(variable,year,month,day,hour,height=NULL,
                                      opendap=NULL,version=2,type='mean') {
   if(is.null(opendap) || opendap==FALSE) {
        base.dir<-TWCR.get.data.dir(version)
        if(!is.null(base.dir)) {
            name<-NULL
            if(type=='normal') {
                    name<-sprintf("%s/hourly/normals/%s.nc",base.dir,variable)
            }
            if(type=='standard.deviation') {
                    name<-sprintf("%s/hourly/standard.deviations/%s.nc",base.dir,variable)
            }
            if(type=='mean') {
               name<-sprintf("%s/hourly/%s/%s.%04d.nc",base.dir,
                           variable,variable,year)
            }
            if(type=='fg.mean') {
               name<-sprintf("%s/first.guess.hourly/%s/%s.%04d.nc",base.dir,
                           variable,variable,year)
            }
            if(type=='spread') {
               name<-sprintf("%s/hourly/%s/%s.%04d.spread.nc",base.dir,
                           variable,variable,year)
            }
            if(type=='fg.spread') {
               name<-sprintf("%s//first.guess.hourly/%s/%s.%04d.spread.nc",base.dir,
                           variable,variable,year)
            }
            if(is.null(name)) stop(sprintf("Unsupported data type %s",type))
            if(file.exists(name)) return(name)
            if(!is.null(opendap) && opendap==FALSE) stop(sprintf("No local file %s",name))
          }
      }
    if(version!=2 && version!='3.2.1') {
      stop(sprintf("No data accessible for %s %d %d %d %d",variable,year,month,day,hour))
    }
    base.dir<-'http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/20thC_ReanV2/'
    if(type=='mean') {
        if(TWCR.get.variable.group(variable)=='monolevel') {
          base.dir<-sprintf("%s/monolevel/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='gaussian') {
          base.dir<-sprintf("%s/gaussian/monolevel/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='pressure') {
          base.dir<-sprintf("%s/pressure/",base.dir)
        }
       return(sprintf("%s/%s.%04d.nc",base.dir,
                   variable,year))
    }
    if(type=='spread') {
        if(TWCR.get.variable.group(variable)=='monolevel') {
          base.dir<-sprintf("%s/monolevel_sprd/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='gaussian') {
          base.dir<-sprintf("%s/gaussian_sprd/monolevel/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='pressure') {
          base.dir<-sprintf("%s/pressure_sprd/",base.dir)
        }
        return(sprintf("%s/%s.%04d.nc",base.dir,
                   variable,year))
    }
    if(type=='normal') {
        if(TWCR.get.variable.group(variable)=='monolevel') {
          return(sprintf("%s/Derived/4Xdailies/monolevel/%s.4Xday.1981-2010.ltm.nc",base.dir,variable))
        } 
        if(TWCR.get.variable.group(variable)=='gaussian') {
          return(sprintf("%s/Derived/8Xdailies/gaussian/monolevel/%s.8Xday.1981-2010.ltm.nc",base.dir,variable))
        }
        if(TWCR.get.variable.group(variable)=='pressure') {
          return(sprintf("%s/Derived/4Xdailies/pressure/%s.4Xday.1981-2010.ltm.nc",base.dir,variable))
        } 
    }
    if(type=='standard.deviation') {
      if(version==2) version<-'3.2.1'
      if(month==2 && day==29) day<-28
      if(is.null(height)) {
         return(sprintf("http://s3.amazonaws.com/philip.brohan.org.20CR/version_%s/hourly/standard.deviations/%s/sd.%02d.%02d.%02d.rdata",
                    version,variable,month,day,hour))
      } else {
         return(sprintf("http://s3.amazonaws.com/philip.brohan.org.20CR/version_%s/hourly/standard.deviations/%s/sd.%04d.%02d.%02d.%02d.rdata",
                    version,variable,height,month,day,hour))
      }

    }
    stop(sprintf("Unsupported opendap data type %s",type))      
}

#' TWCR get file name (ensemble members, hourly)
#'
#' Get the file name (or URL) for selected variable and date (ensemble members hourly data)
#'
#' Called internally by \code{TWCR.get.members.slice.at.hour} but also useful
#'  called directly - you can then access the data with another tool.
#'
#' @export
#' @param variable 'prmsl', 'air.2m', prate, uwnd.10m and vwnd.10m. Others not currently available
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them),
#'  NULL (default) will use local files if available, and network if not.
#' @return File name or URL for netCDF file containing the requested data 
TWCR.hourly.members.get.file.name<-function(variable,year,month,day,hour,
                                              opendap=NULL,version=2) {
   if((version=='2c' || version=='3.5.1') &&
    file.exists('/project/projectdirs/20C_Reanalysis')) { # Nersc operational 2c
      base.name<-'/project/projectdirs/20C_Reanalysis/www/20C_Reanalysis_version2c_ensemble/'
      file.name<-list(
         'air.2m' = 'analysis/t9950/t9950',
         'prmsl' = 'analysis/prmsl/prmsl',
         'prate' = 'first_guess/prate/prate',
         'uwnd.10m' = 'first_guess/u10m/u10m',
         'vwnd.10m' = 'first_guess/v10m/v10m',
         'air.sfc' = 'first_guess/tsfc/tsfc')
      if(is.null(file.name[[variable]])) {
         stop(sprintf("Unavailable ensemble variable %s",variable))
      }
      return(sprintf("%s/%s_%04d.nc",base.name,file.name[[variable]],year))
   }
   if(is.null(opendap) || opendap==FALSE) {
        base.dir<-TWCR.get.data.dir(version)
               name<-sprintf("%s/ensembles/hourly/%s/%s.%04d.nc",base.dir,
                           variable,variable,year)
        if(file.exists(name)) return(name)
        if(!is.null(opendap) && opendap==FALSE) stop(sprintf("No local file %s",name))
      }
    if(version!=2 && version!='3.2.1') stop('Opendap only available for version 2')
    base.dir<-'http://portal.nersc.gov/pydap/20C_Reanalysis_ensemble'
        if(TWCR.get.variable.group(variable)=='monolevel') {
          base.dir<-sprintf("%s/analysis",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='gaussian') {
          base.dir<-sprintf("%s/first_guess",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='pressure') {
          stop("Ensembles of pressure level variables not available by openDAP")
        }
       if(variable=='air.2m') variable<-'t2m'
       if(variable=='uwnd.10m') variable<-'u10m'
       if(variable=='vwnd.10m') variable<-'v10m'
       return(sprintf("%s/%s/%s_%04d.nc",base.dir,variable,variable,year))
 }

#' TWCR get file name (monthly)
#'
#' Get the file name (or URL) for selected variable and date (monthly data)
#'
#' Called internally by \code{TWCR.get.slice.at.month} but also useful
#'  called directly - you can then access the data with another tool.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any 20CR variable
#' @param type - 'mean', 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them),
#'  NULL (default) will use local files if available, and network if not.
#' @return File name or URL for netCDF file containing the requested data 
TWCR.monthly.get.file.name<-function(variable,year,month,opendap=NULL,version=2,type='mean') {
   if(is.null(opendap) || opendap==FALSE) {
        base.dir<-TWCR.get.data.dir(version)
        if(!is.null(base.dir)) {
            name<-NULL
            if(type=='normal') {
                    name<-sprintf("%s/monthly/normals/%s.pp",base.dir,variable)
            }
            if(type=='standard.deviation') {
                    name<-sprintf("%s/monthly/standard.deviations/%s.pp",base.dir,variable)
            }
            if(type=='mean') {
               name<-sprintf("%s/monthly/%s.mon.mean.nc",base.dir,
                           variable)
            }
            if(type=='spread') {
               name<-sprintf("%s/monthly/%s.mon.spread.nc",base.dir,
                           variable)
             }
            if(is.null(name)) stop(sprintf("Unsupported data type %s",type))
            if(file.exists(name)) return(name)
            if(!is.null(opendap) && opendap==FALSE) stop(sprintf("No local file %s",name))
          }
      }
    if(version!=2 && version!='3.2.1') stop('Opendap only available for version 2')
    base.dir<-'http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/20thC_ReanV2/'
    if(type=='mean') {
        if(TWCR.get.variable.group(variable)=='monolevel') {
          base.dir<-sprintf("%s/Monthlies/monolevel/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='gaussian') {
          base.dir<-sprintf("%s/Monthlies/gaussian/monolevel/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='pressure') {
          base.dir<-sprintf("%s/Monthlies/pressure/",base.dir)
        }
       return(sprintf("%s/%s.mon.mean.nc",base.dir,
                   variable))
    }
    if(type=='spread') {
        if(TWCR.get.variable.group(variable)=='monolevel') {
          base.dir<-sprintf("%s/Monthlies/monolevel_sprd/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='gaussian') {
          base.dir<-sprintf("%s/Monthlies/gaussian_sprd/monolevel/",base.dir)
        }
        if(TWCR.get.variable.group(variable)=='pressure') {
          base.dir<-sprintf("%s/Monthlies/pressure_sprd/",base.dir)
        }
        return(sprintf("%s/%s.mon.mean.nc",base.dir,
                   variable))
    }
    if(type=='normal') {
        if(TWCR.get.variable.group(variable)=='monolevel') {
          return(sprintf("%s/Derived/Monthlies/monolevel/%s.1981-2010.ltm.nc",base.dir,variable))
        } 
        if(TWCR.get.variable.group(variable)=='gaussian') {
          return(sprintf("%s/Derived/Monthlies/gaussian/monolevel/%s.1981-2010.ltm.nc",base.dir,variable))
        }
        if(TWCR.get.variable.group(variable)=='pressure') {
          return(sprintf("%s/Derived/Monthlies/pressure/%s.1981-2010.ltm.nc",base.dir,variable))
        } 
    }
    stop(sprintf("Unsupported opendap data type %s",type))      
}

#' Get the observations from 1 prepbufr file
#'
#' All the observations used in one analysis run
#'  (0,6,12,18 hours each day).
#'
#' Specification of obs format is at
#' http://rda.ucar.edu/datasets/ds131.1/docs/ISPD_quick_assimilated_ascii_format.pdf
#' file access only - the observations feedback files are not online.
#' 
#' @export
#' @return A data frame - one row for each observation.
TWCR.get.obs.1file<-function(year,month,day,hour,version=2) {
    base.dir<-TWCR.get.data.dir(version)
    if(is.null(base.dir)) stop("No local TWCR files on this system")
    of.name<-sprintf(
                "%s/observations/%04d/prepbufrobs_assim_%04d%02d%02d%02d.txt",base.dir,
                year,year,month,day,hour)
    if(!file.exists(of.name)) stop("No obs file for given version and date")
    o<-read.fwf(file=of.name,na.strings=c('NA','*','***','*****','*******','**********',
                                          '-99','9999','-999','9999.99','10000.0',
                                          '-9.99','999999999999999999999999999999',
                                          '999999999999','9'),
                widths=c(19,-1,3,-1,1,-1,7,-1,6,-1,5,-1,5,-1,6,-1,7,-1,7,-1,7,
                         -1,10,-1,5,-1,5,-1,1,-1,1,-1,1,-1,1,-1,1,-1,10,-1,10,
                         -1,10,-1,10,-1,30,-1,14),
                col.names=c('UID','NCEP.Type','Variable','Longitude','Latitude',
                            'Elevation','Model.Elevation','Time.Offset',
                            'Pressure.after.bias.correction',
                            'Pressure.after.vertical.interpolation',
                            'SLP','Bias',
                            'Error.in.surface.pressure',
                            'Error.in.vertically.interpolated.pressure',
                            'Assimilation.indicator',
                            'Usability.check',
                            'QC.flag',
                            'Background.check',
                            'Buddy.check',
                            'Mean.first.guess.pressure.difference',
                            'First.guess.pressure.spread',
                            'Mean.analysis.pressure.difference',
                            'Analysis.pressure.spread',
                            'Name','ID'),
                  header=F,stringsAsFactors=F,fileEncoding="ISO-8859-1",
                  colClasses=c('character','integer','character',
                               rep('numeric',2),
                               rep('integer',2),
                               rep('numeric',7),
                               rep('integer',5),
                               rep('numeric',4),
                               rep('character',2)),
                  comment.char="")

    return(o)
}

#' TWCR get observations
#'
#' Retrieves observations from the obs. feedback (prepbufr) files
#' Gets all obs in +-range days around specified date
#'
#' Specification of obs format is at
#' http://rda.ucar.edu/datasets/ds131.1/docs/ISPD_quick_assimilated_ascii_format.pdf
#' File access only - the observations feedback files are not online.
#'
#' @seealso \code{TWCR.get.obs.1file} get the observations for a specific
#'  analysis run.
#' @export
#' @param range Date range (in days) to retrieve observations from - period is
#'  specified time +- range days (default is 0.5 - giving 1 day's obs).
#' @return A data frame - one row for each observation.
TWCR.get.obs<-function(year,month,day,hour,version=2,range=0.5) {
    base.dir<-TWCR.get.data.dir(version)
    if(is.null(base.dir)) stop("No local TWCR files on this system")
    today<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
          times=sprintf("%02d:%02d:00",as.integer(hour),as.integer(hour%%1*60)),
          format=c(dates='y/m/d',times='h:m:s'))
    result<-NULL
    for(hour2 in seq(today-range,today+range,1/24)) {
       hour2<-as.chron(hour2)
       of.name<-sprintf(
                "%s/observations/%04d/prepbufrobs_assim_%04d%02d%02d%02d.txt",base.dir,
                 as.integer(as.character(chron::years(hour2))),
                 as.integer(as.character(chron::years(hour2))),
                 months(hour2),chron::days(hour2),as.integer(chron::hours(hour2)))
        if(!file.exists(of.name)) next
        o<-TWCR.get.obs.1file(as.integer(as.character(chron::years(hour2))),
                               base::months(hour2),chron::days(hour2),as.integer(chron::hours(hour2)),version)
        odates<-chron(dates=sprintf("%04d/%02d/%02d",as.integer(substr(o$UID,1,4)),
                                                     as.integer(substr(o$UID,5,6)),
                                                     as.integer(substr(o$UID,7,8))),
                      times=sprintf("%02d:00:00",as.integer(substr(o$UID,9,10))),
                      format=c(dates='y/m/d',times='h:m:s'))
         o<-o[abs(today-odates)<=range,]
        if(is.null(result)) result<-o
        else result<-rbind(result,o)
     }
    return(result)
}

#' TWCR get fixed field.
#'
#' Get a field of non-time-varying data.
#'
#' Currently doesn't vary betwen versions.
#'
#' @export
#' @param variable - one of hgt.sfc, land, lsmask, soiltype, vegtype,
#' @return field of selected data.
TWCR.get.fixed.field<-function(variable) {
   if(variable != 'hgt.sfc' &&
      variable != 'land' &&
      variable != 'lsmask' &&
      variable != 'soiltype' &&
      variable != 'vegtype') {
     stop(sprintf("Unsupported fixed variable %s",variable))
   }
   fn<-sprintf("%s%s%s.nc","http://www.esrl.noaa.gov/psd/thredds/dodsC/",
                           "Datasets/20thC_ReanV2/gaussian/time_invariant/",
                           variable)
   fn.local<-sprintf("/project/projectdirs/m958/netCDF.data/20CR_v3.5.1/fixed/%s.nc",variable)
   if(file.exists(fn.local)) fn<-fn.local
   fn.local<-sprintf("/scratch/hadpb/20CR/version_3.5.1/fixed/%s.nc",variable)
   if(file.exists(fn.local)) fn<-fn.local
  
   if(variable=='lsmask') variable<-'land' # Wrong name in file
   v<-GSDF.ncdf.load(fn,variable,lat.range=c(-90,90),lon.range=c(0,360))
   return(v)  
}

TWCR.is.in.file<-function(variable,year,month,day,hour,type='mean') {
                #if(variable=='prate' && hour%%3==0) return(TRUE)
		if(hour%%6==0) return(TRUE)
		return(FALSE)
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
# Could do this directly, but it's vital to keep get.interpolation.times
# and is.in.file consistent.
TWCR.get.interpolation.times<-function(variable,year,month,day,hour,type='mean') {
	if(TWCR.is.in.file(variable,year,month,day,hour,type=type)) {
		stop("Internal interpolation failure")
	}
	ct<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
	          times=sprintf("%02d:%02d:00",as.integer(hour),
                                      as.integer(hour%%1*60)),
	          format=c(dates='y/m/d',times='h:m:s'))
	t.previous<-list()
        back.hours=0
	while(back.hours<24) {
		p.hour<-as.integer(hour-back.hours)
                p.year<-year
                p.month<-month
                p.day<-day
                if(p.hour<0) {
                  p.year<-as.numeric(as.character(chron::years(ct-1)))
                  p.month<-as.integer(base::months(ct-1))
                  p.day<-as.integer(chron::days(ct-1))
                  p.hour<-p.hour+24
                }
		if(TWCR.is.in.file(variable,p.year,p.month,p.day,p.hour,type=type)) {
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
		n.hour<-as.integer(hour+forward.hours)
                n.year<-year
                n.month<-month
                n.day<-day
                if(n.hour>23) {
                  n.year<-as.numeric(as.character(chron::years(ct+1)))
                  n.month<-as.integer(base::months(ct+1))
                  n.day<-as.integer(chron::days(ct+1))
                  n.hour<-n.hour-24
                }
		if(TWCR.is.in.file(variable,n.year,n.month,n.day,n.hour,type=type)) {
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
#' Interpolates to the selected height when the selected height is not that of a 20RC level.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any 20CR variable.
#'                 can also be 'sst' - will return 'air.sfc' over sea grids.
#' @param type - 'mean' (default), 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param height Height in hPa - leave NULL for monolevel
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them), NULL (default)
#'  will use local files if available and network otherwise.
#' @return A GSDF field with lat and long as extended dimensions
TWCR.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=NULL,version=2,type='mean') {
  if(variable=='sst') {
    v<-TWCR.get.slice.at.hour('air.sfc',year,month,day,hour,height=height,opendap=opendap,version=version,type=type)
    i<-TWCR.get.slice.at.hour('icec',year,month,day,hour,height=height,opendap=opendap,version=version,type=type)
    lm<-TWCR.get.fixed.field('lsmask')
    is.na(v$data[lm$data==1|i$data>0.01])<-TRUE # Mask out land and ice points
    return(v)
  }
  if(variable=='icec') {
    i<-TWCR.get.slice.at.level.at.hour('icec',year,month,day,hour,height=NULL,opendap=opendap,version=version,type=type)
    #lm<-TWCR.get.fixed.field('lsmask')
    #is.na(i$data[lm$data==1])<-TRUE # Mask out land points
    return(i)
  }
  if(TWCR.get.variable.group(variable)=='monolevel' ||
     TWCR.get.variable.group(variable)=='gaussian') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(TWCR.get.slice.at.level.at.hour(variable,year,month,day,hour,opendap=opendap,version=version,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(TWCR.heights>=height))
  if(height==TWCR.heights[level.below]) {
    return(TWCR.get.slice.at.level.at.hour(variable,year,month,day,hour,height=TWCR.heights[level.below],
                                         opendap=opendap,version=version,type=type))
  }
  below<-TWCR.get.slice.at.level.at.hour(variable,year,month,day,hour,height=TWCR.heights[level.below],
                                         opendap=opendap,version=version,type=type)
  above<-TWCR.get.slice.at.level.at.hour(variable,year,month,day,hour,height=TWCR.heights[level.below+1],
                                         opendap=opendap,version=version,type=type)
  above.weight<-(TWCR.heights[level.below]-height)/(TWCR.heights[level.below]-TWCR.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

TWCR.get.slice.at.level.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=NULL,version=2,type='mean') {
	#dstring<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
	# Is it from an analysis time (no need to interpolate)?
	if(TWCR.is.in.file(variable,year,month,day,hour,type=type)) {
        hour<-as.integer(hour)
        file.name<-TWCR.hourly.get.file.name(variable,year,month,day,hour,height=height,
                                                opendap=opendap,version=version,type=type)
           t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                    format=c(dates='y/m/d',times='h:m:s'))
           if(type=='standard.deviation') { 
               month<-as.integer(month) # Sometimes still a factor, why?
               day<-as.integer(day)
               if(month==2 && day==29) day<-28
	       t<-chron(sprintf("%04d/%02d/%02d",1981,month,day),sprintf("%02d:00:00",hour),
			       format=c(dates='y/m/d',times='h:m:s'))
           }   
           if(type=='normal' ) { # Normals are for year 1 (v2) or 1981 (otherwise), which chron can't handle, and have no Feb 29
               month<-as.integer(month) # Sometimes still a factor, why?
               day<-as.integer(day)
               if(month==2 && day==29) day<-28
              if(version==2 || version=='3.2.1') {
		  t<-chron(sprintf("%04d/%02d/%02d",-1,month,day),sprintf("%02d:00:00",hour),
			   format=c(dates='y/m/d',times='h:m:s'))
		  t<-chron(as.numeric(t)+729)
              } else {
		  t<-chron(sprintf("%04d/%02d/%02d",1981,month,day),sprintf("%02d:00:00",hour),
			   format=c(dates='y/m/d',times='h:m:s'))
              }
           }
           v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(0,360),
                             height.range=rep(height,2),time.range=c(t,t))
	   return(v)
	}
	# Interpolate from the previous and subsequent analysis times
	interpolation.times<-TWCR.get.interpolation.times(variable,year,month,day,hour,type=type)
	v1<-TWCR.get.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,interpolation.times[[1]]$month,
		                               interpolation.times[[1]]$day,interpolation.times[[1]]$hour,
                                               height=height,opendap=opendap,version=version,type=type)
	v2<-TWCR.get.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,interpolation.times[[2]]$month,
		                               interpolation.times[[2]]$day,interpolation.times[[2]]$hour,
                                               height=height,opendap=opendap,version=version,type=type)
	c1<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[1]]$year,
	                                         interpolation.times[[1]]$month,
	                                         interpolation.times[[1]]$day),
	          times=sprintf("%02d:00:00",as.integer(interpolation.times[[1]]$hour)),
	          format=c(dates='y/m/d',times='h:m:s'))
	c2<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[2]]$year,
	                                         interpolation.times[[2]]$month,
	                                         interpolation.times[[2]]$day),
	          times=sprintf("%02d:00:00",as.integer(interpolation.times[[2]]$hour)),
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

# Same, but for monthly data
#' Get slice at month.
#'
#' Get a 2D horizontal slice of a selected variable (as a GSDF field) for a given month.
#'
#' Interpolates to the selected height when the selected height is not that of a 20RC level.
#'
#' @export
#' @param variable 'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any 20CR variable
#' @param type - 'mean', 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param height Height in hPa - leave NULL for monolevel
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them).
#' @return A GSDF field with lat and long as extended dimensions
TWCR.get.slice.at.month<-function(variable,year,month,height=NULL,opendap=TRUE,version=2,type='mean') {
  if(TWCR.get.variable.group(variable)=='monolevel' ||
     TWCR.get.variable.group(variable)=='gaussian') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(TWCR.get.slice.at.level.at.month(variable,year,month,opendap=opendap,version=version,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<10) stop("Height must be between 10 and 1000 hPa")
  level.below<-max(which(TWCR.heights>=height))
  if(height==TWCR.heights[level.below]) {
    return(TWCR.get.slice.at.level.at.month(variable,year,month,height=TWCR.heights[level.below],
                                         opendap=opendap,version=version,type=type))
  }
  below<-TWCR.get.slice.at.level.at.month(variable,year,month,height=TWCR.heights[level.below],
                                         opendap=opendap,version=version,type=type)
  above<-TWCR.get.slice.at.level.at.month(variable,year,month,height=TWCR.heights[level.below+1],
                                         opendap=opendap,version=version,type=type)
  above.weight<-(TWCR.heights[level.below]-height)/(TWCR.heights[level.below]-TWCR.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}
TWCR.get.slice.at.level.at.month<-function(variable,year,month,height=NULL,opendap=FALSE,version=2,type='mean') {
    file.name<-TWCR.monthly.get.file.name(variable,year,month,opendap=opendap,version=version,type=type)
           t<-chron(sprintf("%04d/%02d/%02d",year,month,1),"00:00:00",
                    format=c(dates='y/m/d',times='h:m:s'))
           if(opendap && type=='normal') { # Online normals are for year 1, which chron can't handle, and have no Feb 29
              t<-chron(sprintf("%04d/%02d/%02d",-1,month,1),"00:00:00",
                       format=c(dates='y/m/d',times='h:m:s'))
              t<-chron(as.numeric(t)+729)
           }
           if(!opendap && (type=='normal' || type=='standard.deviation')) { # Local mean and sd files are for 1964
              t<-chron(sprintf("%04d/%02d/%02d",1964,month,day),"00:00:00",
                       format=c(dates='y/m/d',times='h:m:s'))
           }   
           v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(0,360),
                             height.range=rep(height,2),time.range=c(t,t+27))
     return(v)
}

#' Relative Entropy
#'
#' Estimates the Relative Entropy (Kullback-Liebler divergence)
#' between two fields, given the mean and standard deviation) or
#' each.
#'
#' This is a useful metric for 20CR - typical comparison is between
#' climatological and TWCR distributions. In principle we could extend
#' this to do multivariate comparisons (sets of fields) but this function
#' doesn't currently allow this.
#'
#' @export
#' @param old.mean Field of best estimates from climatology
#' @param old.sd Field of climatological standard deviations
#' @param new.mean Field of best estimates from TWCR
#' @param new.sd Field of standard deviations from TWCR
#' @return Field of relative entropy
TWCR.relative.entropy<-function(old.mean,old.sd,new.mean,
                                 new.sd) {
  result<-new.mean
  result$data[]<-(log((new.sd$data**2)/(old.sd$data**2)) +
                 (old.sd$data**2)/(new.sd$data**2) +
                 ((new.mean$data-old.mean$data)**2)/(new.sd$data**2) 
                   -1)*0.5
  return(result)
}

#' Relative Entropy spread.
#'
#' We only estimate the relative entropy, because we only have
#' estimates of the means and standard deviations. This
#' function estimates the spread of the relative entropy.
#'
#' Calculating a standard error for RE is hard, and it's an asymetric
#'  distribution so an exact sigma is of limited use anyway. This function
#'  estimates the value by perturbing the input means and standard
#'  deviations and combining the results in quadrature.
#'
#' @export
#' @param old.mean Field of best estimates from climatology
#' @param old.sd Field of climatological standard deviations
#' @param new.mean Field of best estimates from TWCR
#' @param new.sd Field of standard deviations from TWCR
#' @param new.n number of ensembles making up TWCR (default=56).
#' @return list with components upper (field of RE+n*sigma) and
#'   lower (field of RE-n*sigma).
TWCR.relative.entropy.spread<-function(old.mean,old.sd,new.mean,
                                       new.sd,new.n=56,
                                       perturbed.mean=NULL,
                                       perturbed.sd=NULL) {
  base.re<-TWCR.relative.entropy(old.mean,old.sd,new.mean,new.sd)
  if(is.null(perturbed.mean)) {
     perturbed.mean<-new.mean
     perturbed.mean$data[]<-new.mean$data*1.05
   }
  perturbed.mean.re<-TWCR.relative.entropy(old.mean,old.sd,perturbed.mean,new.sd)
  perturbed.mean$data[]<-perturbed.mean$data-new.mean$data
  perturbed.mean.re$data[]<-perturbed.mean.re$data-base.re$data
  if(is.null(perturbed.sd)) {
     perturbed.sd<-new.sd
     perturbed.sd$data[]<-new.sd$data*1.05
   }
  perturbed.sd.re<-TWCR.relative.entropy(old.mean,old.sd,new.mean,perturbed.sd)
  perturbed.sd$data[]<-perturbed.sd$data-new.sd$data
  perturbed.sd.re$data[]<-perturbed.sd.re$data-base.re$data
  mean.sd<-new.sd$data/sqrt(new.n)
  sd.sd<-TWCR.sds(new.sd$data,new.n)
  spread<-new.mean
  spread$data[]<-sqrt(((perturbed.mean.re$data/perturbed.mean$data)**2)*mean.sd**2+
                      ((perturbed.sd.re$data/perturbed.sd$data)**2)*sd.sd**2)
  return(spread)
}

# Estimate the standard deviation of the standard deviation
#  of a sample of n observations (assuming the distribution is normal).
TWCR.sds<-function(s,n){
  v1<-gamma(n/2)/gamma((n-1)/2)
  v2<-sqrt((n-1)/2 - v1**2)
  return(s*v2/v1)
}

#' Get the ensemble members instead of the mean, spread etc.
#' analogous to TWCR.get.slice.at.hour
#'
#' Currently only some 2d variables.
#'
#' No interpolation - must be at an analysis time
#'
#' @export
#' @param variable 20CR variable name, only 'prmsl', 'air.2m', 'prate', 'uwnd.10m', and 'vwnd.10m' supported.
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them).
#' @return A GSDF field with ensemble number, lat and long as extended dimensions
TWCR.get.members.slice.at.hour<-function(variable,year,month,day,hour,opendap=NULL,version='2c') {
  if(variable != 'prmsl' && variable != 'air.2m' &&
     variable != 'uwnd.10m' && variable != 'vwnd.10m' &&
     variable != 'prate') stop('Unsupported ensemble variable')
  if(!TWCR.is.in.file(variable,year,month,day,hour)) {
	interpolation.times<-TWCR.get.interpolation.times(variable,year,month,day,hour)
	v1<-TWCR.get.members.slice.at.hour(variable,interpolation.times[[1]]$year,interpolation.times[[1]]$month,
		                               interpolation.times[[1]]$day,interpolation.times[[1]]$hour,
                                               opendap=opendap,version=version)
	v2<-TWCR.get.members.slice.at.hour(variable,interpolation.times[[2]]$year,interpolation.times[[2]]$month,
		                               interpolation.times[[2]]$day,interpolation.times[[2]]$hour,
                                               opendap=opendap,version=version)
	c1<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[1]]$year,
	                                         interpolation.times[[1]]$month,
	                                         interpolation.times[[1]]$day),
	          times=sprintf("%02d:00:00",as.integer(interpolation.times[[1]]$hour)),
	          format=c(dates='y/m/d',times='h:m:s'))
	c2<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[2]]$year,
	                                         interpolation.times[[2]]$month,
	                                         interpolation.times[[2]]$day),
	          times=sprintf("%02d:00:00",as.integer(interpolation.times[[2]]$hour)),
	          format=c(dates='y/m/d',times='h:m:s'))
	c3<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
	          times=sprintf("%02d:%02d:00",as.integer(hour),as.integer(hour%%1*60)),
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
  # At calculation time - do the actual retrieval
  file.name<-TWCR.hourly.members.get.file.name(variable,year,month,day,hour,opendap=opendap,version=version)
  # Different variable names for official 2c data
  if(version=='2c' || version=='3.5.1' || version=='3.2.1') {
     if(variable=='air.2m') variable<-'t9950'
     if(variable=='uwnd.10m') variable<-'u10m'
     if(variable=='vwnd.10m') variable<-'v10m'
  }
  t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                      format=c(dates='y/m/d',times='h:m:s'))
  if(version==2 || version=='3.2.1' || version=='2c' || version=='3.5.1') {
     t<-t+2 # Kludge. Are dates 2 days ahead?
  }
  v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(0,360),
                           ens.range=c(0,56),time.range=c(t,t))
  return(v)  
}

#' Make a bootstrap version of an ensemble.
#'
#' Extract the ensemble with TWCR.get.members.slice.at.hour
#'
#' Makes a new ensemble subsampled with replacement from
#'  the original.
#'
#' @export
#' @param ensemble A GSDF field from TWCR.get.members.slice.at.hour
#' @return A similar field, but with a bootstrap sample of the ensemble members
TWCR.members.bootstrap<-function(ensemble) {
   if(ensemble$dimensions[[3]]$type != 'ensemble' ||
      length(ensemble$dimensions[[3]]$values) != 56) stop('Unsupported field format')
   s<-sample(56,replace=TRUE)
   result<-ensemble
   for(i in seq(1,56)) {
     result$data[,,i,]<-ensemble$data[,,s[i],]
   }
   return(result)
}

#' Make annual mean fields for each hour.
#'
#' Poor man's normals - just the mean for each hour (1:24) over a year
#'
#' Note that the indices are 1:24 not 0:23 as 0 can't be used as a list index.
#'
#' @export
#' @param variable 20CR variable name, only 2d variables will work.
#' @param year Year to make averages for.
#' @param version 20CR version number - defaults to 2.
#' @return A list (indexed 1:24) of fields, each of which is the annual average for that hour.
TWCR.annual.mean.hourly<-function(variable,year,version=2) {
   fn<-TWCR.hourly.get.file.name(variable,year,1,1,1,12,version=version)
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
#' Time must not span year boundaries (I.e. all in one file).
#'
#' @export
#' @param variable 20CR variable name, only 2d variables will work.
#' @param date.range A pair of text date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.
#' @param version 20CR version number - defaults to 2.
#' @param type - 'mean' (default), 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param height.range Bottom and top heights in hPa - leave NULL for monolevel
#' @param lat.range Min and max latitude in degrees - defaults to c(-90,90)
#' @param lon.range Min and max longitude in degrees - defaults to c(0,360)
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them), NULL (default)
#'  will use local files if available and network otherwise.
#' @return A GSDF field with the selected multidimensional data
TWCR.get.slab.from.hourly<-function(variable,date.range,
                                             height.range=NULL,
                                             lat.range=c(-90,90),
                                             lon.range=c(0,360),
                                    opendap=NULL,version=2,type='mean') {
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
        v<-TWCR.get.slab.from.hourly(variable,c(sprintf("%04d-%02d-%02d:%02d",
                                                          start.d$year,start.d$month,
                                                          start.d$day,start.d$hour),
                                                  sprintf("%04d-12-31:23",
                                                          start.d$year)),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,opendap=opendap,
                                       version=version,type=type)
        year<-start.d$year+1
        while(year<end.d$year) {
          v<-GSDF.concatenate(v,
                  TWCR.get.slab.from.hourly(variable,c(sprintf("%04d-01-01:00",
                                                          year),
                                                         sprintf("%04d-12-31:23",
                                                          year)),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,opendap=opendap,
                                       version=version,type=type),'time')
          year<-year+1
        }
          v<-GSDF.concatenate(v,
                  TWCR.get.slab.from.hourly(variable,c(sprintf("%04d-01-01:00",
                                                          end.d$year),
                                                         sprintf("%04d-%02d-%02d:%02d",
                                                          end.d$year,end.d$month,
                                                          end.d$day,end.d$hour),
                                                          end.d$year),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,opendap=opendap,
                                       version=version,type=type),'time')
        return(v)
     }

       file.name<-TWCR.hourly.get.file.name(variable,start.d$year,start.d$month,start.d$day,start.d$hour,
					    height=height.range[1],
					    opendap=opendap,version=version,type=type)
       if(type=='normal' || type =='standard.deviation' ) { 
          if(type=='normal' && (version==2 || version=='3.2.1')) { #V2 normals are for year 1, where chron is buggy
	      start.d$chron<-chron(sprintf("%04d/%02d/%02d",-1,start.d$month,start.d$day),
			  sprintf("%02d:00:00",start.d$hour),
			  format=c(dates='y/m/d',times='h:m:s'))
	      start.d$chron<-chron(as.numeric(start.d$chron)+728.999)
	      end.d$chron<-chron(sprintf("%04d/%02d/%02d",-1,end.d$month,end.d$day),
			  sprintf("%02d:00:00",end.d$hour),
			  format=c(dates='y/m/d',times='h:m:s'))
	      end.d$chron<-chron(as.numeric(end.d$chron)+729.001)
          } else {
	      start.d$chron<-chron(sprintf("%04d/%02d/%02d",1981,start.d$month,start.d$day),
			  sprintf("%02d:00:00",start.d$hour),
			  format=c(dates='y/m/d',times='h:m:s'))
	      end.d$chron<-chron(sprintf("%04d/%02d/%02d",1981,end.d$month,end.d$day),
			  sprintf("%02d:00:00",end.d$hour),
			  format=c(dates='y/m/d',times='h:m:s'))
          }     
       }
       v<-GSDF.ncdf.load(file.name,variable,lat.range=lat.range,lon.range=lon.range,
			 height.range=height.range,time.range=c(start.d$chron,end.d$chron))
       return(v)
}


#' Extract a hyperslab of data - from individual ensemble members
#'
#' Up to 5d (lat, lon, height, time, member).
#'
#' @export
#' @param variable 20CR variable name, only 2d variables will work.
#' @param date.range A pair of text date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.
#' @param version 20CR version number - defaults to 2.
#' @param type - 'mean' (default), 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param height.range Bottom and top heights in hPa - leave NULL for monolevel
#' @param lat.range Min and max latitude in degrees - defaults to c(-90,90)
#' @param lon.range Min and max longitude in degrees - defaults to c(0,360)
#' @param members.range Min and max ensemble member - defaults to c(0,56)
#' @param opendap TRUE for network retrieval, FALSE for local files (faster, if you have them), NULL (default)
#'  will use local files if available and network otherwise.
#' @return A GSDF field with the selected multidimensional data
TWCR.get.members.slab.from.hourly<-function(variable,date.range,
                                             height.range=NULL,
                                             lat.range=c(-90,90),
                                             lon.range=c(0,360),
                                             members.range=c(0,56),
                                             opendap=NULL,version=2) {
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
        v<-TWCR.get.members.slab.from.hourly(variable,c(sprintf("%04d-%02d-%02d:%02d",
                                                          start.d$year,start.d$month,
                                                          start.d$day,start.d$hour),
                                                  sprintf("%04d-12-31:23",
                                                          start.d$year)),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,members.range=members.range,
                                       opendap=opendap,
                                       version=version)
        year<-start.d$year+1
        while(year<end.d$year) {
          v<-GSDF.concatenate(v,
                  TWCR.get.members.slab.from.hourly(variable,c(sprintf("%04d-01-01:00",
                                                          year),
                                                         sprintf("%04d-12-31:23",
                                                          year)),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,members.range=members.range,
                                       opendap=opendap,
                                       version=version),'time')
          year<-year+1
        }
          v<-GSDF.concatenate(v,
                  TWCR.get.members.slab.from.hourly(variable,c(sprintf("%04d-01-01:00",
                                                          end.d$year),
                                                         sprintf("%04d-%02d-%02d:%02d",
                                                          end.d$year,end.d$month,
                                                          end.d$day,end.d$hour),
                                                          end.d$year),
                                       height.range=height.range,lat.range=lat.range,
                                       lon.range=lon.range,members.range=members.range,
                                       opendap=opendap,
                                       version=version),'time')
        return(v)
     }

       file.name<-TWCR.hourly.members.get.file.name(variable,start.d$year,start.d$month,start.d$day,start.d$hour,
					    opendap=opendap,version=version)
       v<-GSDF.ncdf.load(file.name,variable,lat.range=lat.range,lon.range=lon.range,
			 height.range=height.range,ens.range=members.range,
                         time.range=c(start.d$chron,end.d$chron))
       return(v)
}
