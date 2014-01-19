# Functions for getting data from the GFS forecast ensemble
# Opendap info http://nomads.ncep.noaa.gov:9090/dods/gens/gens20140118/gec00_00z.info
# http://nomads.ncep.noaa.gov/

# names and classes of variables
GFS.latname<-'lat'
GFS.lonname<-'lon'
GFS.hgtname<-'lev'
GFS.tname<-'time'
GFS.ensname<-'ens'
# Hourly monolevel
GFS.monolevel<-c('no4lftxsfc','no5wava500mb','no5wavh500mb','acpcpsfc','albdosfc','apcpsfc',
                 'capesfc','cape180_0mb','cfrzrsfc','cicepsfc','cinsfc','cin180_0mb','cpratsfc',
                 'crainsfc','csnowsfc','cwatclm','cworkclm','dlwrfsfc','dpt2m','dswrfsfc',
                 'gfluxsfc','gpa1000mb','gpa500mb','hgtsfc',


GFS.h.all<-c('absvprs',

GFS.h.trop<-c('clwmrprs',

GFS.h.strat<-c('o3mrprs',


# Height of each pressure level in hPa
MERRA.heights<-c(1000,970,950,925,900,875,850,825,800,775,750,725,700,650,600,550,500,450,
                 400,350,300,250,200,150,100,70,50,40,30,20,10,7,5,4,3,2,1,0.7,0.5,0.4,0.3,0.1)

#' Merra show variables
#' 
#' List all the variables available in the MERRA reanalyis.
#'
#' Two sorts of variables:
#' \describe{
#' \item{Monolevel} {Lat x Lon x Time}
#' \item{Pressure level} {Lat x Lon x Height x Time}
#' }
#' This function has no parmeters and returns no value
#' it prints a list of available variables.
#' @export
MERRA.show.variables<-function() {
  print('Monolevel')
  print(MERRA.MAT1NXSLV)
  print(MERRA.MAI1NXINT)
  print(MERRA.MAT1NXFLX)
  print(MERRA.MAT1NXINT)
  print(MERRA.MAT1NXLND)
  print(MERRA.MAT1NXRAD)
  print('Pressure level')
  print(MERRA.MAI3CPASM)
  print(MERRA.MAT3CPCLD)
  print(MERRA.MAT3CPMST)
  print(MERRA.MAT3CPODT)
  print(MERRA.MAT3CPQDT)
  print(MERRA.MAT3CPRAD)
  print(MERRA.MAT3CPTDT)
  print(MERRA.MAT3CPTRB)
  print(MERRA.MAT3CPUDT)
}

# Get group of variable.
#  They have different locations on the openDAP server.
MERRA.get.variable.group<-function(variable) {
  variable<-toupper(variable)
  if(length(which(MERRA.MAT1NXSLV==variable))>0) return('MAT1NXSLV')
  if(length(which(MERRA.MAI1NXINT==variable))>0) return('MAI1NXINT')
  if(length(which(MERRA.MAT1NXFLX==variable))>0) return('MAT1NXFLX')
  if(length(which(MERRA.MAT1NXINT==variable))>0) return('MAT1NXINT')
  if(length(which(MERRA.MAT1NXLND==variable))>0) return('MAT1NXLND')
  if(length(which(MERRA.MAT1NXRAD==variable))>0) return('MAT1NXRAD')
  if(length(which(MERRA.MAI3CPASM==variable))>0) return('MAI3CPASM')
  if(length(which(MERRA.MAT3CPCLD==variable))>0) return('MAT3CPCLD')
  if(length(which(MERRA.MAT3CPMST==variable))>0) return('MAT3CPMST')
  if(length(which(MERRA.MAT3CPODT==variable))>0) return('MAT3CPODT')
  if(length(which(MERRA.MAT3CPQDT==variable))>0) return('MAT3CPQDT')
  if(length(which(MERRA.MAT3CPRAD==variable))>0) return('MAT3CPRAD')
  if(length(which(MERRA.MAT3CPTDT==variable))>0) return('MAT3CPTDT')
  if(length(which(MERRA.MAT3CPTRB==variable))>0) return('MAT3CPTRB')
  if(length(which(MERRA.MAT3CPUDT==variable))>0) return('MAT3CPUDT')
  stop(sprintf("Unrecognised variable: %s",variable))
}

#' MERRA hourly get file name
#'
#' Get file name (URI) for MERRA variable
#' 
#' MERRA URIs depend on the variable group and the day, this function
#'  calculates them from the variable name and the date.
#'
#' @export
#' @param variable 'T2M', 'SLP', 'U10M', 'H500' - or any MERRA variable
#' @param type must be 'mean', (no spreads, normals etc available from MERRA)
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A file or URI containing the requested MERRA data
MERRA.hourly.get.file.name<-function(variable,year,month,day,hour,opendap=TRUE,type='mean') {
    if(opendap) {
        runId<-100
        if(year>=1993 && year<2001) runId<-200
        if(year>=2001) runId<-300
        if(type=='mean') {
            base.dir<-'http://nomads.ncep.noaa.gov:9090/dods/gens/gens20140118/gec00_00z'
            if(MERRA.get.variable.group(variable)=='MAT1NXSLV') {
              return(sprintf("%s/MAT1NXSLV.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_slv_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAI1NXINT') {
              return(sprintf("%s/MAI1NXINT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.inst1_2d_int_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXFLX') {
              return(sprintf("%s/MAT1NXFLX.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_flx_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXINT') {
              return(sprintf("%s/MAT1NXINT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_int_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXLND') {
              return(sprintf("%s/MAT1NXLND.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_lnd_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXRAD') {
              runId<-runId+1
              return(sprintf("%s/MAT1NXRAD.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_rad_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            base.dir<-'http://goldsmr3.sci.gsfc.nasa.gov:80/opendap/MERRA'
            if(MERRA.get.variable.group(variable)=='MAI3CPASM') {
              return(sprintf("%s/MAI3CPASM.5.2.0/%04d/%02d/MERRA%03d.prod.assim.inst3_3d_asm_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPCLD') {
              return(sprintf("%s/MAT3CPCLD.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_cld_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPMST') {
              return(sprintf("%s/MAT3CPMST.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_mst_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPODT') {
              return(sprintf("%s/MAT3CPODT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_odt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPQDT') {
              return(sprintf("%s/MAT3CPQDT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_qdt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPRAD') {
              return(sprintf("%s/MAT3CPRAD.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_rad_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPTDT') {
              return(sprintf("%s/MAT3CPTDT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_tdt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPTRB') {
              return(sprintf("%s/MAT3CPTRB.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_trb_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPUDT') {
              return(sprintf("%s/MAT3CPUDT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_udt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
          }
        stop(sprintf("Only mean values available from MERRA"))      
    }     
    else {
        stop(sprintf("MERRA only available via openDAP"))
   }
}


MERRA.is.in.file<-function(variable,year,month,day,hour,type='mean') {
   group<-MERRA.get.variable.group(variable)
   if(group=='MAT1NXSLV' || group=='MAI1NXINT' || group=='MAT1NXFLX' ||
      group=='MAT1NXINT' || group=='MAT1NXLND' || group=='MAT1NXRAD') {
      if(hour%%1==0) return(TRUE)
      return(FALSE)
   }
   if(group=='MAI3CPASM' || group=='MAT3CPCLD' || group=='MAT3CPMST' ||
      group=='MAT3CPODT' || group=='MAT3CPQDT' || group=='MAT3CPRAD' ||
      group=='MAT3CPTDT' || group=='MAT3CPTRB' || group=='MAT3CPUDT') {
      if(hour%%3==0) return(TRUE)
      return(FALSE)
   }
   stop("Internal error - unknown variable group")
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
# Could do this directly, but it's vital to keep get.interpolation.times
# and is.in.file consistent.
MERRA.get.interpolation.times<-function(variable,year,month,day,hour,type='mean') {
	if(MERRA.is.in.file(variable,year,month,day,hour,type=type)) {
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
                  p.month<-months(ct-1)
                  p.day<-days(ct-1)
                  p.hour<-p.hour+24
                }
		if(MERRA.is.in.file(variable,p.year,p.month,p.day,p.hour,type=type)) {
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
                  n.month<-months(ct+1)
                  n.day<-days(ct+1)
                  n.hour<-n.hour-24
                }
		if(MERRA.is.in.file(variable,n.year,n.month,n.day,n.hour,type=type)) {
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
#' Interpolates to the selected height when the selected height is not that of a MERRA level.
#'
#' @export
#' @param variable 'T2M', 'SLP', 'U10M', 'H500' - or any MERRA variable
#' @param type must be 'mean', (no spreads, normals etc available from MERRA)
#' @param height Height in hPa - leave NULL for monolevel
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A GSDF field with lat and long as extended dimensions
MERRA.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=TRUE,type='mean') {
   group<-MERRA.get.variable.group(variable)
   if(group=='MAT1NXSLV' || group=='MAI1NXINT' || group=='MAT1NXFLX' ||
      group=='MAT1NXINT' || group=='MAT1NXLND' || group=='MAT1NXRAD') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,opendap=opendap,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<0.1) stop("Height must be between 0.1 and 1000 hPa")
  level.below<-max(which(MERRA.heights>=height))
  if(height==MERRA.heights[level.below]) {
    return(MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,height=MERRA.heights[level.below],
                                          opendap=opendap,type=type))
  }
  below<-MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,height=MERRA.heights[level.below],
                                         opendap=opendap,type=type)
  above<-MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,height=MERRA.heights[level.below+1],
                                         opendap=opendap,type=type)
  above.weight<-(MERRA.heights[level.below]-height)/(MERRA.heights[level.below]-
                             MERRA.heights[level.below+1])
  below$data<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

MERRA.get.slice.at.level.at.hour<-function(variable,year,month,day,hour,
                                                   height=NULL,opendap=TRUE,type='mean') {
	dstring<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
	# Is it from an analysis time (no need to interpolate)?
	if(MERRA.is.in.file(variable,year,month,day,hour,type=type)) {
        file.name<-MERRA.hourly.get.file.name(variable,year,month,day,hour,opendap=opendap,type=type)
           t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                    format=c(dates='y/m/d',times='h:m:s'))
           v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(-180,180),
                             height.range=rep(height,2),time.range=c(t,t+1/24-0.001))
	   return(v)
	}
	# Interpolate from the previous and subsequent analysis times
	interpolation.times<-MERRA.get.interpolation.times(variable,year,month,day,hour,type=type)
	v1<-MERRA.get.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,
                                                  interpolation.times[[1]]$month,
		                                  interpolation.times[[1]]$day,
                                                  interpolation.times[[1]]$hour,
                                                  opendap=opendap,type=type,height=height)
	v2<-MERRA.get.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,
                                                  interpolation.times[[2]]$month,
		                                  interpolation.times[[2]]$day,
                                                  interpolation.times[[2]]$hour,
                                                  opendap=opendap,type=type,height=height)
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
    v$data<-v1$data*weight+v2$data*(1-weight)
    return(v)
}

