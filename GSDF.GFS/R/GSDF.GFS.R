# Functions for getting data from the GFS forecast ensemble
# Opendap info http://nomads.ncep.noaa.gov:9090/dods/gens/gens20140118/gec00_00z.info
# http://nomads.ncep.noaa.gov/

# names and classes of variables
GFS.latname<-'lat'
GFS.lonname<-'lon'
GFS.hgtname<-'lev'
GFS.tname<-'time'
GFS.ensname<-'ens'
# Everything is 6-Hourly

# Monolevel
GFS.monolevel<-c('no4lftxsfc','no5wava500mb','no5wavh500mb','acpcpsfc','albdosfc','apcpsfc',
                 'capesfc','cape180_0mb','cfrzrsfc','cicepsfc','cinsfc','cin180_0mb','cpratsfc',
                 'crainsfc','csnowsfc','cwatclm','cworkclm','dlwrfsfc','dpt2m','dswrfsfc',
                 'gfluxsfc','gpa1000mb','gpa500mb','hgtsfc','hgt2pv','hgtneg2pv','hgttop0c',
                 'hgttop0c','hgt0c','hgtmwl','hgttrop','hpblsfc','icecsfc','landsfc','lftxsfc',
                 'lhtflsfc','pevprsfc','potsig995','pratesfc','preslclb','preslclt','presmclb',
                 'presmclt','preshclb','preshclt','pressfc','pres2pv','presneg2pv','prescclb',
                 'prescclt','presmwl','prestrop','prmslmsl','pvort320k','pwatclm','rh2m',
                 'rhsg330_1000','rhsg440_1000','rhsg720_940','rhsg440_720','rhsig995','rh30_0mb',
                 'rhclm','rhtop0c','rh0c','shtflsfc','snodsfc','soilw0_10cm','soilw10_40cm',
                 'soilw40_100cm','soilw100_200cm','spfh2m','spfh30_0mb','sunsdsfc','tcdcclm',
                 'tcdcblcll','tcdclcll','tcdcmcll','tcdchcll','tcdcccll','tmax2m','tmin2m',
                 'tmplclt','tmpmclt','tmphclt','tmpsfc','tmp_1829m','tmp_2743m','tmp_3658m',
                 'tmp2m','tmpsig995','tmp0_10cm','tmp10_40cm','tmp40_100cm','tmp100_200cm',
                 'tmp30_0mb','tmp2pv','tmpneg2pv','tmpmwl','tmptrop','tozneclm','ugwdsfc',
                 'uflxsfc','ugrd_1829m','ugrd_2743m','ugrd_3658m','ugrd10m','ugrdsig995',
                 'ugrd30_0mb','ugrd2pv','ugrdneg2pv','ugrdmwl','ugrdtrop','ulwrfsfc','ulwrftoa',
                 'uswrfsfc','uswrftoa','vgwdsfc','vflxsfc','vgrd_1829m','vgrd_2743m',
                 'vgrd_3658m','vgrd10m','vgrdsig995','vgrd30_0mb','vgrd2pv','vgrdneg2pv',
                 'vgrdmwl','vgrdtrop','vvelsig995','vwsh2pv','vwshneg2pv','vwshtrop',
                 'watrsfc','weasdsfc')

# 4 different sets of heights
GFS.heights<-list()
                 
GFS.heights[['all']]<-c(1000,975,950,925,900,850,800,750,700,650,600,550,500,450, 
                         400,350,300,250,200,150,100,70,50,30,20,10)
GFS.h.all<-c('absvprs','hgtprs','tmpprs','ugrdprs','vgrdprs')

GFS.heights[['trop']]<-c(1000,975,950,925,900,850,800,750,700,650,600,550,500,450, 
                         400,350,300,250,200,150,100)
GFS.h.trop<-c('clwmrprs','rhprs','vvelprs')

GFS.heights[['trop2']]<-c(1000,925,850,700,500,300,250,200)
GFS.h.trop2<-c('spfhprs')

GFS.heights[['strat']]<-c(100,70,50,30,20,10)
GFS.h.strat<-c('o3mrprs')


#' GFS show variables
#' 
#' List all the variables available in the GFS ensemble.
#'
#' Two sorts of variables:
#' \describe{
#' \item{Monolevel} {Lat x Lon x Time}
#' \item{Pressure level} {Lat x Lon x Height x Time}
#' }
#' This function has no parmeters and returns no value
#' it prints a list of available variables.
#' @export
GFS.show.variables<-function() {
  print('Monolevel')
  print(GFS.monolevel)
  print('Pressure level')
  print(GFS.h.all)
  print(GFS.h.trop)
  print(GFS.h.trop2)
  print(GFS.h.strat)
}

# Get group of variable.
#  Distinguish between diferent sets of vertical heights 
GFS.get.variable.group<-function(variable) {
  variable<-tolower(variable)
  if(length(which(GFS.monolevel==variable))>0) return('monolevel')
  if(length(which(GFS.h.all==variable))>0) return('all')
  if(length(which(GFS.h.trop==variable))>0) return('trop')
  if(length(which(GFS.h.trop2==variable))>0) return('trop2')
  if(length(which(GFS.h.strat==variable))>0) return('strat')
 stop(sprintf("Unrecognised variable: %s",variable))
}

#' GFS hourly get file name
#'
#' Get file name (URI) for GFS variable
#' 
#' GFS URIs depend on the time of the run, this function
#'  calculates them from the variable name and the run date.
#'
#' @export
#' @param variable 'tmp2m', 'prmslmsl', 'ugrd10m', 'tmpprs' - or any GFS variable
#' @param type must be 'member' (everthing else derived from members).
#' @param member The member to use' - integer in the range 0-20.
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A URI containing the requested GFS data
GFS.hourly.get.file.name<-function(variable,year,month,day,hour,opendap=TRUE,type='member',member=0) {
    if(opendap) {
        if(type!='member') stop("Type must be 'member' for a file name")
        base.dir<-'http://nomads.ncep.noaa.gov:9090/dods/gens/gens'
        if(member==0) return(sprintf("%s%04d%02d%02d/gec00_%02dz",base.dir,year,month,day,hour))
        if(member<=20) return(sprintf("%s%04d%02d%02d/gep%02d_%02dz",base.dir,year,month,day,member,hour))
        stop("member must be in 1..20")
    }     
    else {
        stop(sprintf("GFS only available via openDAP"))
   }
}


GFS.is.in.file<-function(variable,year,month,day,hour,type='member',member=0,lead=0) {
      if(hour%%6==0) return(TRUE)
      return(FALSE)
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
# Could do this directly, but it's vital to keep get.interpolation.times
# and is.in.file consistent.
GFS.get.interpolation.times<-function(variable,year,month,day,hour,type='member',member=0) {
	if(GFS.is.in.file(variable,year,month,day,hour,type=type,member=member)) {
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
		if(GFS.is.in.file(variable,p.year,p.month,p.day,p.hour,type=type,member=member)) {
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
		if(GFS.is.in.file(variable,n.year,n.month,n.day,n.hour,type=type,member=member)) {
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
#' Interpolates to the selected hour when the data available are less than 6-hourly.
#' Interpolates to the selected height when the selected height is not that of a GFS level.
#'
#' @export
#' @param variable 'tmp2m', 'prmslmsl', 'ugrd10m', 'tmpprs' - or any GFS variable
#' @param (year,month,day,hour) Time at which the forecast was made.
#' @param lead number of hours ahead of forecast time (0-259, default=0)
#' @param type must be 'member', 'mean', or 'spread' (no normals or sds from GFS)
#' @param member The member to use' - integer in the range 0-20 (default=0, mean and sd use all members)
#' @param height Height in hPa - leave NULL for monolevel
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A GSDF field with lat and long as extended dimensions
GFS.get.member.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=TRUE,
                                       member=1,lead=0) {
   group<-GFS.get.variable.group(variable)
   if(group=='monolevel') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,opendap=opendap,
                                          member=member,lead=lead))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>max(GFS.heights[[group]]) || height<min(GFS.heights[[group]])) {
    stop("Height outside range for %s: %f to %f",variable,max(GFS.heights[[group]]),
                                                          min(GFS.heights[[group]]))
  }
  level.below<-max(which(GFS.heights[[group]]>=height))
  if(height==GFS.heights[[group]][level.below]) {
    return(GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,height=GFS.heights[[group]][level.below],
                                          opendap=opendap,member=member,lead=lead))
  }
  below<-GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,height=GFS.heights[[group]][level.below],
                                         opendap=opendap,member=member,lead=lead)
  above<-GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,height=GFS.heights[[group]][level.below+1],
                                         opendap=opendap,member=member,lead=lead)
  above.weight<-(GFS.heights[[group]][level.below]-height)/(GFS.heights[[group]][level.below]-
                             GFS.heights[[group]][level.below+1])
  below$data<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

GFS.get.member.slice.at.level.at.hour<-function(variable,year,month,day,hour,
                                                   height=NULL,opendap=TRUE,member=1,lead=0) {
	dstring<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
	# Is it from an analysis time (no need to interpolate)?
	if(GFS.is.in.file(variable,year,month,day,hour,type='member',member=member,lead=lead)) {
        file.name<-GFS.hourly.get.file.name(variable,year,month,day,hour,opendap=opendap,type='member')
           t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                    format=c(dates='y/m/d',times='h:m:s'))
           t<-t+2 # GFS timestamps seem to be two days out - why?
           v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(0,360),
                             height.range=rep(height,2),time.range=c(t+lead/24,t+lead/24+1/24-0.001),
                             ens.range=rep(member,2))
	   return(v)
	}
	# Interpolate from the previous and subsequent analysis times
	interpolation.times<-GSDF.get.interpolation.times(variable,year,month,day,hour,type=type)
	v1<-GFS.get.member.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,
                                                  interpolation.times[[1]]$month,
		                                  interpolation.times[[1]]$day,
                                                  interpolation.times[[1]]$hour,
                                                  opendap=opendap,type=type,height=height,
                                                  member=member,lead=lead)
	v2<-GFS.get.member.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,
                                                  interpolation.times[[2]]$month,
		                                  interpolation.times[[2]]$day,
                                                  interpolation.times[[2]]$hour,
                                                  opendap=opendap,type=type,height=height,
                                                  member=member,lead=lead)
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

