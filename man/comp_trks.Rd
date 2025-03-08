% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_trks.R
\name{comp_trks}
\alias{comp_trks}
\title{Compare grid cell counts}
\usage{
comp_trks(
  sim_trks,
  stations,
  land_barrier,
  vis_graph,
  multi.grid,
  HSgrid,
  snap_tolerance
)
}
\arguments{
\item{sim_trks}{Tracks to which modeled tracks should be compared}

\item{stations}{A sf object comprised of receiver station locations with a buffer around them that represents the range of the receiver (polygons)}

\item{land_barrier}{A sf polygon of a barrier object around which tracks should be re-routed}

\item{vis_graph}{A visibility graph created from the barrier object @seealso \link[pathroutr:prt_visgraph]{pathroutr::prt_visgraph}}

\item{multi.grid}{boolean}

\item{HSgrid}{When multi.grid = FALSE, a sf polygon grid; When multi.grid = TRUE, a list of sf polygon grids}

\item{snap_tolerance}{The tolerance (in meters) at which an intersection between a station and a track should snap to the station centroid. It is recommended that the snap_tolerance be equal to the station buffer size.}
}
\value{
A data frame with counts and differences by grid cell ID ("gid")
}
\description{
Compare grid cell counts
}
