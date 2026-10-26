#' Daily averaged acoustic telemetry locations
#'
#' @format An sf object with 1341 rows and 4 variables:
#' \describe{
#'   \item{ID}{transmitter identifier, a factor with 105 levels}
#'   \item{time}{POSIXct timestamp of detection, UTC time zone}
#'   \item{locType}{Type of location, one of "o" observed or "p" predicted}
#'   \item{geometry}{Well-known text of the location of the identified transmitter
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"at_dly_locs"

#' Detections for demonstration
#'
#' @format An sf object with 62358 rows and 3 variables:
#' \describe{
#'   \item{ID}{transmitter identifier, an integer ranging from 1-10}
#'   \item{time}{POSIXct timestamp of detection, local time zone}
#'   \item{geometry}{Well-known text of the location of the identified transmitter
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"demo_dets"

#' Depth data within study site
#'
#' @format An data frame with 809233 rows and 3 variables:
#' \describe{
#'   \item{latitude}{Numerical latitude coordinate in NAD 1983, EPSG: 4269}
#'   \item{longitude}{Numerical longitude coordinate in NAD 1983, EPSG: 4269}
#'   \item{altitude}{Numerical altitude measurements where negative values are below sea level}
#' }
"depth_data"

#' Model fit for 100 km x 100 km grid resolution
#'
#' @format A list of length 31:
#' \describe{
#'   \item{coefficients}{double vector of four elements}
#'   \item{residuals}{double vector of 92 elements}
#'   \item{fitted.values}{double vector of 92 elements}
#'   \item{effects}{double vector of 92 elements}
#'   \item{R}{matrix of four by four elements}
#'   \item{rank}{integer of one element, consisting of 4}
#'   \item{qr}{list of length 5}
#'   \item{family}{list of length 13}
#'   \item{linear.predictors}{double vector of 92 elements}
#'   \item{deviance}{double vector of one element, consisting of 59.38828}
#'   \item{aic}{double vector of one element, consisting of 67.38828}
#'   \item{null.deviance}{double vector of one element, consisting of 120.0902}
#'   \item{iter}{integer vector of one element, consisting of 7}
#'   \item{weights}{double vector of 92 elements}
#'   \item{prior.weights}{double vector of 92 elements, consisting of 1}
#'   \item{df.residual}{integer vector of one element, consisting of 88}
#'   \item{df.null}{integer vector of one element, consisting of 91}
#'   \item{y}{integer vector of 92 elements, consisting of 0 and 1}
#'   \item{converged}{logical vector of one element consisting of TRUE}
#'   \item{boundary}{logical vector of one element consisting of FALSE}
#'   \item{model}{date frame with 92 rows and 4 columns}
#'   \item{na.action}{integer vector of one element, "omit", consisting of 21}
#'   \item{call}{language vector of one element, consisting of glm(formula = g_fit ~ p_a +
#'   mean_depth + d_shore, family = binomial(), data = HS_100km_grid)}
#'   \item{formula}{formula vector of one element, consisting of g_fit ~ p_a + mean_depth + d_shore}
#'   \item{terms}{formula vector of one element, consisting of g_fit ~ p_a + mean_depth + d_shore}
#'   \item{data}{sf data frame with 93 rows and 13 columns}
#'   \item{method}{character vector with one element, consisting of glm.fit}
#'   \item{contrasts}{list of length 1}
#'   \item{xlevels}{list of length 1}
#' }
"fit2.100km"

#' American coastal Atlantic states polygon
#'
#' @format An sf object with 435 rows and three columns:
#' \describe{
#'   \item{poly_uid}{Polygon ID, ranging from 1 to 434}
#'   \item{line_uid}{Values of 1 (States) or 2 (Washington, DC)}
#'   \item{geometry}{Well-known text POLYGON of the United States Atlantic coastal shelf
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"fo_land_barrier"

#' Release site with false origin
#'
#' @format An sf object with 1 rows and 2 columns:
#' \describe{
#'   \item{FID}{Polygon ID, consisting of 0}
#'   \item{geometry}{Well-known text POLYGON of the release sites for male blacktip sharks, Carcharhinus limbatus,
#'    off the coast of Palm Beach County Florida in WGS84/Pseudo-Mercator, EPSG:3857, and shifted southeast using
#'    an undisclosed false origin}
#' }
"fo_rel_site"

#' Receiver station polygons with false origin
#'
#' @format An sf object with 1030 rows and 3 columns:
#' \describe{
#'   \item{poly_uid}{Polygon ID, consisting of 1}
#'   \item{line_uid}{Line ID, consisting of 1 to 1030}
#'   \item{geometry}{Well-known text POLYGON of the receiver stations with a 650 m buffer in WGS84/Pseudo-Mercator,
#'   EPSG:3857, and shifted southeast using an undisclosed false origin}
#' }
"fo_stations"

#' American Atlantic coastal shelf to the 500m isobath from Florida to Cape Code, MA
#'
#' @format An sf object with 1 row and two columns:
#' \describe{
#'   \item{poly_uid}{Polygon ID}
#'   \item{geometry}{Well-known text POLYGON of the American Atlantic coastal shelf
#'     in WGS84/Pseudo-Mercator, EPSG:3857, and shifted southeast using an undisclosed false origin}
#' }
"fo_study_site"

#' Release site
#'
#' @format An sf object with 1 rows and 2 columns:
#' \describe{
#'   \item{FID}{Polygon ID, consisting of 0}
#'   \item{geometry}{Well-known text POLYGON of the release sites for male blacktip sharks, Carcharhinus limbatus,
#'    off the coast of Palm Beach County Florida in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"rel_site"

#' Resolution grids
#'
#' @format A large list of length 4:
#' \describe{
#'   \item{[["1"]]}{A sf data frame with 93 rows and 13 columns}
#'    \itemize{
#'      \item{gid}{Polygon ID, consisting of 0}
#'      \item{res_name}{Character, name of grid resolution, one of 100km, 50km, 25km, 10km}
#'      \item{avg_dif}{Numeric average difference in unique IDs counted between simulated and modeled corresponding
#'   grid cell pairs}
#'      \item{se}{Numeric standard error in avg_dif}
#'      \item{g_fit}{Numeric good fit, consisting of 0 or 1}
#'      \item{mean_depth}{Numeric average depth across grid cell}
#'      \item{gid2}{Polygon ID before grid cells were clipped to study site}
#'      \item{d_shore}{Numeric distance to shore from centroid of grid cell}
#'      \item{poly_uid}{Polygon ID, consisting of 1}
#'      \item{count}{Numeric total of receivers per grid cell}
#'      \item{p_a}{Factor of 2 levels, consisting of "p" or "a", indicating whether receivers are present "p" or
#'   absent "a"}
#'      \item{den_rcs}{Numeric density of receivers per grid cell}
#'      \item{x}{Well-known text POLYGON of grid cells in WGS84/Pseudo-Mercator, EPSG:3857}
#'      }
#'   \item{[["2"]]}{A sf data frame with 292 rows and 13 columns}
#'    \itemize{
#'      \item{gid}{Polygon ID, consisting of 0}
#'      \item{res_name}{Character, name of grid resolution, one of 100km, 50km, 25km, 10km}
#'      \item{avg_dif}{Numeric average difference in unique IDs counted between simulated and modeled corresponding
#'   grid cell pairs}
#'      \item{se}{Numeric standard error in avg_dif}
#'      \item{g_fit}{Numeric good fit, consisting of 0 or 1}
#'      \item{mean_depth}{Numeric average depth across grid cell}
#'      \item{gid2}{Polygon ID before grid cells were clipped to study site}
#'      \item{d_shore}{Numeric distance to shore from centroid of grid cell}
#'      \item{poly_uid}{Polygon ID, consisting of 1}
#'      \item{count}{Numeric total of receivers per grid cell}
#'      \item{p_a}{Factor of 2 levels, consisting of "p" or "a", indicating whether receivers are present "p" or
#'   absent "a"}
#'      \item{den_rcs}{Numeric density of receivers per grid cell}
#'      \item{x}{Well-known text POLYGON of grid cells in WGS84/Pseudo-Mercator, EPSG:3857}
#'      }
#'  \item{[["3"]]}{A sf data frame with 986 rows and 13 columns}
#'    \itemize{
#'      \item{gid}{Polygon ID, consisting of 0}
#'      \item{res_name}{Character, name of grid resolution, one of 100km, 50km, 25km, 10km}
#'      \item{avg_dif}{Numeric average difference in unique IDs counted between simulated and modeled corresponding
#'   grid cell pairs}
#'      \item{se}{Numeric standard error in avg_dif}
#'      \item{g_fit}{Numeric good fit, consisting of 0 or 1}
#'      \item{mean_depth}{Numeric average depth across grid cell}
#'      \item{gid2}{Polygon ID before grid cells were clipped to study site}
#'      \item{d_shore}{Numeric distance to shore from centroid of grid cell}
#'      \item{poly_uid}{Polygon ID, consisting of 1}
#'      \item{count}{Numeric total of receivers per grid cell}
#'      \item{p_a}{Factor of 2 levels, consisting of "p" or "a", indicating whether receivers are present "p" or
#'   absent "a"}
#'      \item{den_rcs}{Numeric density of receivers per grid cell}
#'      \item{x}{Well-known text POLYGON of grid cells in WGS84/Pseudo-Mercator, EPSG:3857}
#'      }
#'  \item{[["4"]]}{A sf data frame with 5338 rows and 13 columns}
#'    \itemize{
#'      \item{gid}{Polygon ID, consisting of 0}
#'      \item{res_name}{Character, name of grid resolution, one of 100km, 50km, 25km, 10km}
#'      \item{avg_dif}{Numeric average difference in unique IDs counted between simulated and modeled corresponding
#'   grid cell pairs}
#'      \item{se}{Numeric standard error in avg_dif}
#'      \item{g_fit}{Numeric good fit, consisting of 0 or 1}
#'      \item{mean_depth}{Numeric average depth across grid cell}
#'      \item{gid2}{Polygon ID before grid cells were clipped to study site}
#'      \item{d_shore}{Numeric distance to shore from centroid of grid cell}
#'      \item{poly_uid}{Polygon ID, consisting of 1}
#'      \item{count}{Numeric total of receivers per grid cell}
#'      \item{p_a}{Factor of 2 levels, consisting of "p" or "a", indicating whether receivers are present "p" or
#'   absent "a"}
#'      \item{den_rcs}{Numeric density of receivers per grid cell}
#'      \item{x}{Well-known text POLYGON of grid cells in WGS84/Pseudo-Mercator, EPSG:3857}
#'      }
#' }
"resolutions"

#' Results of iterative simulation process
#'
#' @format Large list of length 1327 each element containing results for four grid resolutions with the following:
#' \describe{
#'   \item{gid}{Polygon ID, consisting of 1 to the maximum number of grid cells per grid resolution}
#'   \item{sim_count}{Numeric integer of unique simulated tracks that intersected each grid cell}
#'   \item{mod_count}{Numeric integer of unique modeled tracks that intersected each grid cell}
#'   \item{dif}{Numeric mathematical difference between mod_count and sim_count}
#' }
"results"

#' American Atlantic coastal shelf to the 500m isobath from Florida to Cape Code, MA
#'
#' @format An sf object with 1 row and two columns:
#' \describe{
#'   \item{poly_uid}{Polygon ID}
#'   \item{geometry}{Well-known text POLYGON of the American Atlantic coastal shelf
#'     in WGS84/Pseudo-Mercator, EPSG:3857}
#' }
"study_site"
