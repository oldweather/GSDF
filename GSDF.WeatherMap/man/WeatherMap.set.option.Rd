\name{WeatherMap.set.option}
\alias{WeatherMap.set.option}
\title{WeatherMap.option}
\usage{
  WeatherMap.set.option(Options = NULL, option = NULL,
    value = NULL)
}
\arguments{
  \item{Options}{list of options - if NULL, use defaults}

  \item{option}{name of option to set}

  \item{value}{value to set selected option to}
}
\value{
  new list of options
}
\description{
  Set or query the options controling the plot.
}
\details{
  The rendering of a map is controlled by a large number of
  options contained in a list. Option: Default: Effect:
  cores 1 Not currently used pole.lat 90 pole.lon 180 Pole
  location for map lon.min -180 lon.max 180 lat.min -90
  lat.max 90 Map range (around centre) show.wind=TRUE,
  show.precipitation=TRUE, show.mslp=TRUE,
  show.temperature=TRUE, show.ice=FALSE, show.fog=FALSE,
  show.obs=FALSE, show.ice.shelves=TRUE,
  precip.points=25000, # Bigger -> higher res precip
  precip.threshold=0.0025, # Only show where more than this
  precip.range=0.03, # Precip rate for max intensity
  precip.T.snow=273, # Show as snow where colder (K)
  precip.pch=21, # Graphics context for drawing precip
  precip.lty=1, precip.lwd=1, precip.scale=1, # Scaling for
  precip blob size precip.max.opacity=1,
  precip.colour=c(0,0,0), # Colour for intense precip
  wind.vector.fade.steps=1, # Increase for gradual fade
  in/out wind.vector.iterate=1, # Move streamlets n times
  before drawing wind.vector.seed=2, # Smaller -> more wind
  vectors wind.vector.arrow=NULL, # See ?arrow
  wind.vector.points=3, # Bigger -> smoother curves and
  slower wind.vector.scale=0.25, # Bigger -> longer vectors
  wind.vector.move.scale=1, # Bigger -> faster moving
  vectors wind.vector.decimate=0.2, # Bigger -> less vector
  clustering wind.vector.decimate.bandwidth=0.5, #
  wind.vector.decimate.gridsize=1000, # wind.vector.lwd=2,
  # Line width jitter=TRUE, # Jitter vector seed points?
  wind.palette=rev( brewer.pal(11,'RdBu')), # Interpolated
  blue red wind.palette.bias=1, # ?colorRamp
  wind.palette.opacity=1, # wind.palette.maxgrey=550, #
  Smaller -> white lines darker temperature.range=7, # T2m
  anomaly for max. colour mslp.base=101325, # Base value
  for anomalies mslp.range=10000, # Anomaly for max contour
  mslp.step=750, # Smaller -> more contours
  mslp.tpscale=2000, # Smaller -> contours less transparent
  mslp.lwd=1, background.resolution='low', # 'low' for
  fast, 'high' for pretty
  sea.colour=rgb(80*1.1,95*1.1,107*1.1,255,
  maxColorValue=255), # For background
  ice.colour=rgb(150,165,177,255, maxColorValue=255),
  merge.colour=rgb(110,110,110,255, maxColorValue=255), #
  Soften Wind colours merge.weight=1, # Amount of softening
  to apply ice.points=10000, # Bigger - higher res ice
  land.colour=rgb(123,121,117,255, maxColorValue=255),
  fog.colour=c(0.65,0.65,0.65), # 0-1, bigger -> lighter
  fog fog.min.transparency=0.85, # 0-1, bigger -> thicker
  fog fog.resolution=1, # Grid resolution in degrees
  obs.size=0.5, # In degrees obs.colour=rgb(255,215,0,100,
  maxColorValue=255), # For observations label='', # Label
  - the date is a good choice label.xp=0.97,label.yp=0.04 #
  Location, 'npc, units
}

