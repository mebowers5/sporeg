crw_in_polygon <- function(
  polyg,
  theta = c(0, 10),
  stepLen = 100,
  initPos = c(NA, NA),
  initHeading = NA,
  nsteps = 30,
  inputCRS = NA,
  cartesianCRS = NA,
  sp_out = TRUE,
  show_progress = TRUE
) {
  # Check input class
  if (
    !inherits(
      polyg,
      c(
        "data.frame",
        "sf",
        "sfc",
        "SpatialPolygonsDataFrame",
        "SpatialPolygons"
      )
    )
  ) {
    stop(
      "Input 'polyg' must be of class 'data.frame', 'sf', 'sfc', ",
      "'SpatialPolygonsDataFrame', or 'SpatialPolygons'."
    )
  }

  # Get input CRS and use CRS arg if missing
  crs_in <- sf::st_crs(polyg)
  if (is.na(crs_in)) {
    crs_in <- sf::st_crs(inputCRS)
  }

  # Get or set Cartesian CRS
  crs_cartesian <- sf::st_crs(cartesianCRS)

  # Set crs_cartesian = crs_input if Cartesian and cartesianCRS missing
  if (is.na(crs_cartesian) && isFALSE(crs_in$IsGeographic)) {
    crs_cartesian <- crs_in
  }

  # Check for Cartesian CRS
  if (isTRUE(crs_in$IsGeographic) && is.na(cartesianCRS)) {
    stop(
      "Coordinate reference system of input 'polyg' must be Cartesian ",
      "(projected) \nor 'cartesianCRS' must be specified."
    )
  }

  if (isTRUE(crs_cartesian$IsGeographic)) {
    stop(
      "Coordinate reference system specified by 'cartesianCRS' is not ",
      "Cartesian/projected."
    )
  }

  # Standardize polygon input to sfc
  if (inherits(polyg, c("sf", "sfc"))) {
    if (!any(c("MULTIPOLYGON", "POLYGON") %in% sf::st_geometry_type(polyg))) {
      stop(
        "Input object 'polyg' must contain geometry of type 'POLYGON' when ",
        "class is 'sf' or 'sfc'."
      )
    }
    polyg_sf <- sf::st_geometry(polyg)
  } else if (inherits(polyg, c("data.frame", "matrix"))) {
    if (!all(c("x", "y") %in% colnames(polyg))) {
      stop("Input 'polyg' must have columns named 'x' and 'y'.")
    }

    xy_mat <- as.matrix(polyg[, c("x", "y"), drop = FALSE])
    if (!identical(xy_mat[1L, ], xy_mat[nrow(xy_mat), ])) {
      xy_mat <- rbind(xy_mat, xy_mat[1L, ])
    }

    polyg_sf <- sf::st_sfc(sf::st_polygon(list(xy_mat)), crs = crs_in)
  } else {
    polyg_sf <- sf::st_as_sfc(polyg)
  }

  # Transform to Cartesian CRS once
  if (!is.na(crs_cartesian)) {
    polyg_sf <- sf::st_transform(polyg_sf, crs = crs_cartesian)
  }

  # Union multi-part polygons once to simplify distance and intersection checks
  polyg_union <- sf::st_union(polyg_sf)

  # Handle initial position
  if (any(is.na(initPos))) {
    init <- sf::st_sample(polyg_union, size = 1)
    init_xy <- as.vector(sf::st_coordinates(init))
  } else {
    init <- sf::st_sfc(sf::st_point(initPos[1:2]), crs = crs_in)
    if (!is.na(crs_in) && !is.na(crs_cartesian)) {
      init <- sf::st_transform(init, crs = crs_cartesian)
    }
    if (!sf::st_contains(polyg_union, init, sparse = FALSE)[1L, 1L]) {
      stop("initPos is outside polygon boundary.")
    }
    init_xy <- as.vector(sf::st_coordinates(init))
  }

  # Randomly select heading if not given
  if (is.na(initHeading)) {
    initHeading <- stats::runif(1, 0, 360)
  }

  # Preallocate plain numeric matrix for fast in-loop assignment
  path_fwd <- matrix(NA_real_, nrow = nsteps + 1L, ncol = 2L)
  path_fwd[1L, ] <- init_xy

  # Extract unified boundary once
  xl <- sf::st_boundary(polyg_union)

  # Initial distance and step window
  dist_i <- as.numeric(sf::st_distance(
    sf::st_sfc(sf::st_point(init_xy), crs = crs_cartesian),
    xl
  ))
  nsteps_i <- (dist_i %/% stepLen) + 1L
  rows_i <- seq.int(2L, min(nsteps + 1L, 1L + nsteps_i))

  # Counter for consecutive boundary failures
  k <- 0L

  # Initialize progress bar
  if (show_progress) {
    message("Simulating tracks...")
    pb <- utils::txtProgressBar(
      min = 0,
      max = nsteps + 1L,
      initial = 0,
      style = 3
    )
    on.exit(close(pb), add = TRUE)
  }

  while (length(rows_i) > 0L) {
    # Enlarge turn angle SD after consecutive boundary failures
    theta_i <- c(theta[1], theta[2] * (1 + 0.1 * k^2))

    # Simulate candidate chunk
    path_fwd_i <- as.matrix(glatos::crw(
      theta = theta_i,
      stepLen = stepLen,
      initPos = init_xy,
      initHeading = initHeading,
      nsteps = length(rows_i)
    ))

    # Combine current start point with candidate steps
    path_mat <- rbind(init_xy, path_fwd_i, deparse.level = 0)

    # Check if segments cross polygon boundary
    crosses <- crosses_boundary(path = path_mat, boundary = xl)

    # First step crossed boundary: retry chunk with wider turn angle SD
    if (crosses[1L]) {
      k <- k + 1L
      next
    }

    # Retain only contiguous steps prior to the first boundary crossing
    if (any(crosses)) {
      valid_len <- which(crosses)[1L] - 1L
      valid_idx <- seq_len(valid_len)
      rows_i <- rows_i[valid_idx]
      path_fwd_i <- path_fwd_i[valid_idx, , drop = FALSE]
    }

    k <- 0L
    last_row <- rows_i[length(rows_i)]

    # Store accepted steps and advance current position
    path_fwd[rows_i, ] <- path_fwd_i
    init_xy <- path_fwd[last_row, ]

    # Update progress bar
    if (show_progress) {
      utils::setTxtProgressBar(pb, last_row)
    }

    if (last_row >= nsteps + 1L) {
      break
    }

    # Smallest distance to boundary from new position
    dist_i <- as.numeric(sf::st_distance(
      sf::st_sfc(sf::st_point(init_xy), crs = crs_cartesian),
      xl
    ))

    # Calculate heading of final accepted step for start of next chunk
    initHeading <- glatos::vector_heading(
      path_fwd[(last_row - 1L):last_row, 1L],
      path_fwd[(last_row - 1L):last_row, 2L]
    )

    # Conservative estimate of steps before reaching nearest boundary
    nsteps_i <- (dist_i %/% stepLen) + 1L
    rows_i <- seq.int(last_row + 1L, min(nsteps + 1L, last_row + nsteps_i))
  }

  if (show_progress) {
    message("Done.")
  }

  crs_out <- if (!is.na(crs_in)) crs_in else sf::st_crs(NA)

  # Convert to sf or data.frame once at the end
  if (sp_out) {
    path_fwd_df <- as.data.frame(path_fwd)
    names(path_fwd_df) <- c("x", "y")
    path_fwd_sf <- sf::st_as_sf(
      path_fwd_df,
      coords = c("x", "y"),
      crs = crs_cartesian
    )
    if (!is.na(crs_cartesian)) {
      path_fwd_sf <- sf::st_transform(path_fwd_sf, crs = crs_out)
    }
    return(path_fwd_sf)
  }

  if (!is.na(crs_cartesian) && !is.na(crs_out) && crs_cartesian != crs_out) {
    pts <- sf::st_sfc(sf::st_multipoint(path_fwd), crs = crs_cartesian)
    pts <- sf::st_transform(pts, crs = crs_out)
    coords <- sf::st_coordinates(pts)[, 1:2, drop = FALSE]
  } else {
    coords <- path_fwd
  }

  path_fwd_df <- as.data.frame(coords)
  names(path_fwd_df) <- c("x", "y")
  return(path_fwd_df)
}


#' Check if path segments cross polygon boundary
#' @noRd
crosses_boundary <- function(path, boundary) {
  n <- nrow(path) - 1L
  segs <- lapply(seq_len(n), function(i) path[c(i, i + 1L), , drop = FALSE])
  segs_sfc <- sf::st_sfc(
    lapply(segs, sf::st_linestring),
    crs = sf::st_crs(boundary)
  )
  lengths(sf::st_intersects(segs_sfc, boundary)) > 0L
}
