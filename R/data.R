#'
#' @format An sf object with 1341 rows and 4 variables:
#' \describe{
#'   \item{ID}{transmitter identifier, a factor with 105 levels}
#'   \item{time}{POSIXct timestamp of detection, UTC time zone}
#'   \item{locType}{}
#'   \item{geometry}{Well-known text of the location of the identified transmitter
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"at_dly_locs"

#'
#' @format An sf object with 62358 rows and 3 variables:
#' \describe{
#'   \item{ID}{transmitter identifier, an integer ranging from 1-10}
#'   \item{time}{POSIXct timestamp of detection, local time zone}
#'   \item{geometry}{Well-known text of the location of the identified transmitter
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"demo_dets"

#'
"depth_data"

#'
"fit2.100km"

#' American coastal Atlantic states polygon
#'
#' @format An sf object with 435 rows and three columns:
#' \describe{
#'   \item{poly_uid}{Polygon ID, ranging from 1 to 434}
#'   \item{line_uid}{Values of 1 (States) or 2 (Washington, DC)}
#'   \item{geometry}{Well-known text POLYGON of the American Atlantic coastal shelf
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"fo_land_barrier"

#'
"fo_rel_site"

#'
"fo_stations"

#'
"fo_sts_pts"

#' American Atlantic coastal shelf to the 500m isobath from Florida to Cape Code, MA
#'
#' @format An sf object with 1 row and two columns:
#' \describe{
#'   \item{poly_uid}
#'   \item{geometry}{Well-known text POLYGON of the American Atlantic coastal shelf
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"fo_study_site"

#'
"rel_site"

#'
"res"

#'
"results"

#'
"site_depth"

#'
"study_site"

#'
"subset"
