#'  @title Geodesic buffer around points (long, lat) using metric radius
#'  EXTRACTED FROM GEOBUFFER R PACKAGE
#'
#' @description Allows the possibility of creating geodesic buffers when the
#'   radius is given in metric units. A geodesic buffer is not affected by the
#'   distortions introduced by projected coordinate systems. This function is a
#'   wrapper of `geosphere::destPoint()`.
#'
#' @param xy One of the following: `SpatialPoints`, `SpatialPointsDataFrame`,
#'   points as `sf`, or two columns `matrix`, `data.frame` or `data.table`, with
#'   the first column containing unprojected longitudes and the second
#'   containing unprojected latitudes of your points around which you desire
#'   buffers.
#'
#' @param dist_m Distance in meters passed as `d` to `geosphere::destPoint()`.
#'   The distance must be a numeric vector. Its length must be either 1
#'   (assuming you want the same buffer radius for all points in `xy`), or the
#'   total number of points you have in `xy` (assuming you want a different
#'   buffer radius for each point).
#'
#' @param step_dg Step of bearings (directions) in degrees. Must be numeric of
#'   length 1. Defaults to 10. Dictates the point density of the buffer edge,
#'   therefore the buffer's shape. For example, the maximum allowed value of 120
#'   corresponds to 360/120 = 3 points on a circle, which will form a buffer as
#'   an equilateral triangle. For more circle-like shaped buffers, use a smaller
#'   step like 10, 5 dg or even smaller. However, the smaller the step, the more
#'   computational intensive the operations are. The smallest allowed value is 1
#'   dg.
#'
#' @param crs Character string of projection arguments. Defaults to
#'   `"+proj=longlat +ellps=WGS84 +datum=WGS84"`. The CRS must be the one
#'   corresponding to your points/coordinates. If you are unsure, then could be
#'   a safe bet to try the default value. For more details see `?sp::CRS`.
#'
#' @param output Dictates the type of output. Character vector with one of the
#'   following values: `"sp"`, `"sf"`, `"data.table"` or `"data.frame"`.
#'   Defaults to `"sp"`. If indicates a spatial object (`"sp"` or `"sf"`), then
#'   it returns the buffers as polygons around the given points. If indicates a
#'   table object (`"data.table"` or `"data.frame"`), then it returns the points
#'   that constitute the buffers as a 3 columns `data.table` or `data.frame`:
#'   `lon`, `lat`, `id`, where `id` is the id of each point in `xy`. This can be
#'   useful for plotting with `ggplot2`.
#'
#' @param ... Additional arguments passed to `geosphere::destPoint()`, like `a`
#'   and `f`.
#'
#' @return Depending on the value given to `output` (see above).
#'
#' @examples
#' \dontrun{
#' bucharest_500km <- geobuffer_pts(xy = data.frame(lon = 26.101390,
#'                                                  lat = 44.427764),
#'                                  dist_m = 500*10^3,
#'                                  output = "sf")
#' bucharest_500km
#' plot(bucharest_500km)
#'
#' library(mapview)
#' library(sf)
#' mapView(as(bucharest_500km, "Spatial"), alpha.regions = 0.2)
#' }
#' @author Valentin Stefan
#'
#' @references This function is a wrapper of `geosphere::destPoint()`. See also
#'   [Euclidean and Geodesic Buffering in R](https://gis.stackexchange.com/questions/250389/euclidean-and-geodesic-buffering-in-r)
#'   on gis.stackexchange. Also check [Understanding Geodesic Buffering](https://www.esri.com/news/arcuser/0111/geodesic.html).
#'
#' @importFrom methods as
#' @import sp sf geosphere data.table
#' @importFrom rlang .data
#'@importFrom data.table as.data.table setorder
#' @export
#' @keywords internal


geobuffer_pts <- function(xy,
                          dist_m,
                          step_dg = 10,
                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84",
                          output = "sp",
                          ...){

  id <- NULL
  # Validate the input and get the number of points in xy.
  tested <- .check_input(xy, dist_m, step_dg, crs, output)
  xy <- tested$xy
  n_points <- tested$n_points

  # A) Points at distance and bearing ---------------------------------------

  # Construct buffers as points at given distance and bearing.

  # A vector of bearings (follows a circle).
  dg <- seq(from = 0, to = 360, by = step_dg)

  # Construct equidistant points (the "buffer points"). Inspired from section
  # "Point at distance and bearing" from Robert J. Hijmans in "Introduction to
  # the 'geosphere' package" at:
  # https://cran.r-project.org/web/packages/geosphere/vignettes/geosphere.pdf
  buff_pts <- data.table::as.data.table(
    geosphere::destPoint(p = xy,
                         b = rep(dg, each = n_points),
                         d = dist_m,
                         ...)
  )

  # B) SpatialPolygon from points -------------------------------------------

  # Make polygon buffers from the points created above.

  # Add column which indicates to which point ID from n_points each buffer point
  # belongs to.
  buff_pts[, id := rep(1:n_points, times = length(dg))]
  # If the returns is desired as data.table or data.frame, then stop here.
  if(output == "data.table"){
    data.table::setorder(buff_pts, id)
    return(buff_pts)
  } else if(output == "data.frame"){
    data.table::setorder(buff_pts, id)
    return(as.data.frame(buff_pts))
  }

  # Split the "buffer points" by id.
  lst <- split(buff_pts, by = "id", keep.by = FALSE)

  # Make SpatialPolygons out of the list of coordinates.
  poly   <- lapply(lst, sp::Polygon, hole = FALSE)
  polys  <- lapply(list(poly), sp::Polygons, ID = NA)
  spolys <- sp::SpatialPolygons(Srl = polys, proj4string = sp::CRS(crs))
  # Disaggregate (split in individual polygons).
  spolys_buff <- sp::disaggregate(spolys)

  if(output == "sp"){
    return(spolys_buff)
  } else if(output == "sf"){
    return(sf::st_as_sf(spolys_buff))
  }
}


# Helper ------------------------------------------------------------------

# Helper hidden function to validate the input.

.check_input <- function(xy, dist_m, step_dg, crs, output){

  # Check `xy` --------------------------------------------------------------

  # Check if the xy argument is of expected class.
  expected <- c("SpatialPoints",
                "SpatialPointsDataFrame",
                "sf",
                "matrix",
                "data.frame",
                "data.table")
  if(!inherits(x = xy, what = expected))
    stop("For `xy`, expecting classs: ", paste(expected, collapse = ", "))

  if( inherits(xy, what = c("sf")) ){
    xy <- as(xy, "Spatial")
  } else if( inherits(xy, what = c("data.frame", "data.table")) ){
    xy <- base::as.matrix(xy)
  }

  # Depending on the class of xy, get the number of points and do some extra
  # tests.
  if( inherits(xy, what = c("SpatialPoints", "SpatialPointsDataFrame")) ){
    if (sp::is.projected(xy))
      stop(strwrap("The spatial object `xy` is projected.
                   Expecting unprojected coordinates,
                   i.e. longitude-latitude in decimal degrees.",
                   prefix = " ", initial = ""))
    n_points <- length(xy)
  } else if( inherits(xy, what = "matrix") ){
    if(dim(xy)[2] != 2)
      stop(strwrap("You have more than two columns in `xy`.
                   If you have one or more points, then expecting `xy` to be a
                   two columns matrix, data.frame or data.table
                   (column 1 as longitude and column 2 as latitude).",
                   prefix = " ", initial = ""))
    if( ! all(range(xy[, 1]) %between% c(-180, 180) &
              range(xy[, 2]) %between% c(-90, 90)) )
      stop(strwrap("Expecting unprojected coordinates in `xy` with
                   longitude between -180 & 180, and latitude between -90 & 90.",
                   prefix = " ", initial = ""))
    n_points <- dim(xy)[1]
  }

  # Check `dist_m` ----------------------------------------------------------

  if( ! inherits(dist_m, what = "numeric") )
    stop("`dist_m` must be numeric (meters).")
  if( ! length(dist_m) %in% c(1L, n_points) )
    stop(strwrap("The length of the `dist_m` numeric vector must be either 1
                 (assuming you want the same buffer radius for all points in `xy`),
                 or the total number of points you have in `xy`
                 (assuming you want a different buffer radius for each point).",
                 prefix = " ", initial = ""))

  # Check `step_dg` ---------------------------------------------------------

  if( ! inherits(step_dg, what = "numeric") )
    stop("`step_dg` must be numeric (degrees).")
  if( length(step_dg) != 1L)
    stop("`step_dg` must be of length 1.")
  # Maximum allowed is 120, corresponding two 3 bearings, so forming an
  # equilateral triangle. I do not encourage very small values between 0 and 1
  # because is using resources unnecessarily.
  if( ! step_dg %between% c(1, 120) )
    stop("`step_dg` must be numeric degrees, between 1 and 120")

  # # Check `crs` -----------------------------------------------------------

  for (arg in c("+proj", "+datum", "+ellps")){
    if( ! grepl(pattern = arg, x = crs, fixed = TRUE) )
      stop(strwrap(paste("In `crs`, missing the", arg, "argument.
                         Example of minimal expected CRS:
                         '+proj=longlat +ellps=WGS84 +datum=WGS84'",
                         prefix = " ", initial = "")))
  }

  # Check `output` ----------------------------------------------------------

  return_values <- c("sp", "sf", "data.table", "data.frame")
  if( ! output %in% return_values)
    stop("For `output`, expecting: ", paste(return_values, collapse = ", "))

  return(list(xy = xy, n_points = n_points))
}
