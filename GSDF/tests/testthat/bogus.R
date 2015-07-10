# Create some sample fields for tests.

checkers<-GSDF()
checkers$dimensions[[1]]<-list(type='lat',values=seq(-89.5,89.5))
checkers$dimensions[[2]]<-list(type='lon',values=seq(-179.5,179.5))
checkers$data<-array(data=seq(1,180*360),dim=c(180,360))
checkers$data[]<-sin(16*pi*as.integer(checkers$data/180)/360)*
                 sin(8*pi*(checkers$data%%180)/180)
w<-which(checkers$data>=0)
checkers$data[w]<-1
checkers$data[-w]<-0

circle<-GSDF()
circle$dimensions[[1]]<-list(type='lat',values=seq(-87.5,87.5,5))
circle$dimensions[[2]]<-list(type='lon',values=seq(2.5,357.5,5))
circle$data<-array(data=seq(1,36*72),dim=c(36,72))
x<-as.integer(circle$data/36)/36
y<-(circle$data%%36)/36
r<-sqrt((x-mean(x))**2+(y-mean(y))**2)
w<-which(r<0.4)
circle$data[w]<-1
circle$data[-w]<-0

uk<-GSDF()
uk$dimensions[[1]]<-list(type='lon',values=seq(-5.071,3.3125,0.0135))
uk$dimensions[[2]]<-list(type='lat',values=seq(-3.7707,7.1508,0.0135))
uk$data<-array(data=seq(1,622*810),dim=c(622,810))
y<-as.integer(uk$data/622)/810
x<-(uk$data%%622)/622
uk$data[]<-sin(2*pi*x)*sin(2*pi*y)
uk$meta<-list(pole.lat=37.5,pole.lon=177.5)

has.leap<-GSDF()
has.leap$dimensions[[1]]<-list(type='lat',values=c(1,2))
has.leap$dimensions[[2]]<-list(type='lon',values=c(3,4))
has.leap$dimensions[[3]]<-list(type='time',values=
  chron::chron(sprintf("%02d/%02d/%04d",c(2,2,3),c(28,29,1),c(2000,2000,2000)),
               sprintf("%02d:%02d:%02d",c(12,12,12),c(0,0,0),c(0,0,0)),
               format = c(dates = "m/d/y", times = "h:m:s"))
)
has.leap$data<-array(data=seq(1,2*2*3),dim=c(2,2,3))
