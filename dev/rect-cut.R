# ============================================================
# Rectangle cut by two lines into four regions a, b, c, d
#
# Rectangle corners:
#   O = (0,0), A = (W,0), B = (W,H), C = (0,H)
#
# Region labels:
#   a = bottom-left
#   b = bottom-right
#   c = top-right
#   d = top-left
#
# Assumption:
#   total area is normalized to 1, so H = 1 / W
#
# Included:
#   1) General constrained oblique solver
#   2) Axis-parallel solver
#   3) Wrapper
#   4) Plotting function
# ============================================================


# ------------------------------------------------------------
# 1) General constrained oblique solver
#
# Lines:
#   L1: (0, v1) -> (W, v2)
#   L2: (u1, 0) -> (u2, H)
#
# Constraint:
#   v2 / (W - u1) = H / W
# Since H = 1/W, this becomes
#   v2 = (W - u1) / W^2
# ------------------------------------------------------------
cut_rectangle_lines_general <- function(a, b, c, d, W, tol = 1e-10) {
  vals <- c(a, b, c, d)
  if (any(vals < 0)) stop("Areas a, b, c, d must be nonnegative.")
  if (W <= 0) stop("W must be positive.")
  if (abs(sum(vals) - 1) > 1e-8) {
    stop("Areas must sum to 1 (normalized rectangle area).")
  }

  H <- 1 / W

  params_from_u1 <- function(u1) {
    v2 <- (W - u1) / W^2
    v1 <- 2 * (a + b) / W - v2
    u2 <- 2 * W * (a + d) - u1
    list(u1 = u1, u2 = u2, v1 = v1, v2 = v2)
  }

  intersection_xy <- function(u1) {
    p <- params_from_u1(u1)
    u2 <- p$u2
    v1 <- p$v1
    v2 <- p$v2

    # L1: y = v1 + m1*x
    m1 <- (v2 - v1) / W

    # L2: x = u1 + k*y
    k <- (u2 - u1) / H

    den <- 1 - k * m1
    if (abs(den) < 1e-12) return(c(x = NA_real_, y = NA_real_))

    x <- (u1 + k * v1) / den
    y <- v1 + m1 * x
    c(x = x, y = y)
  }

  f_root <- function(u1) {
    p <- params_from_u1(u1)
    xy <- intersection_xy(u1)
    if (any(is.na(xy))) return(NA_real_)

    area_a <- 0.5 * (u1 * xy["y"] + p$v1 * xy["x"])
    area_a - a
  }

  # Feasible interval
  lower <- max(
    0,
    2 * W * (a + d) - W,
    W * (1 - 2 * (a + b))
  )
  upper <- min(
    W,
    2 * W * (a + d),
    2 * W * (a + b)
  )

  if (lower > upper) {
    stop("No feasible solution interval for u1 under the given areas/constraint.")
  }

  grid <- seq(lower, upper, length.out = 1001)
  fvals <- sapply(grid, f_root)

  good <- is.finite(fvals)
  grid <- grid[good]
  fvals <- fvals[good]

  if (length(grid) < 2) {
    stop("Could not evaluate the root function on the feasible interval.")
  }

  idx0 <- which.min(abs(fvals))
  if (abs(fvals[idx0]) < 1e-10) {
    u1_sol <- grid[idx0]
  } else {
    idx <- which(fvals[-length(fvals)] * fvals[-1] <= 0)
    if (length(idx) == 0) {
      stop("No sign change found; areas may be incompatible with the oblique constraint.")
    }
    i <- idx[1]
    u1_sol <- uniroot(f_root, interval = c(grid[i], grid[i + 1]), tol = tol)$root
  }

  p <- params_from_u1(u1_sol)
  xy <- intersection_xy(u1_sol)

  u1 <- p$u1
  u2 <- p$u2
  v1 <- p$v1
  v2 <- p$v2
  xint <- xy["x"]
  yint <- xy["y"]

  # L1: y = b1 + m1*x
  m1 <- (v2 - v1) / W
  b1 <- v1

  # L2
  if (abs(u2 - u1) < 1e-12) {
    L2_yx <- NULL
    L2_xy <- list(intercept = u1, slope = 0)  # x = u1 + 0*y
    L2_type <- "vertical"
  } else {
    m2 <- H / (u2 - u1)
    b2 <- -H * u1 / (u2 - u1)
    L2_yx <- list(intercept = b2, slope = m2) # y = b2 + m2*x
    L2_xy <- list(intercept = u1, slope = (u2 - u1) / H) # x = u1 + slope*y
    L2_type <- "nonvertical"
  }

  area_check <- list(
    a = 0.5 * (u1 * yint + v1 * xint),
    b = 0.5 * ((W - u1) * yint + (W - xint) * v2),
    c = 0.5 * ((W - xint) * (H - v2) + (W - u2) * (H - yint)),
    d = 0.5 * (xint * (H - v1) + u2 * (H - yint))
  )

  list(
    method = "general",
    inputs = list(a = a, b = b, c = c, d = d, W = W, H = H),
    parameters = list(u1 = u1, u2 = u2, v1 = v1, v2 = v2),
    intersection = list(x = xint, y = yint),
    L1 = list(
      type = "nonvertical",
      endpoints = rbind(c(0, v1), c(W, v2)),
      y_as_function_of_x = list(intercept = b1, slope = m1),
      equation = sprintf("y = %.12f + %.12f x", b1, m1)
    ),
    L2 = list(
      type = L2_type,
      endpoints = rbind(c(u1, 0), c(u2, H)),
      y_as_function_of_x = L2_yx,
      x_as_function_of_y = L2_xy
    ),
    area_check = area_check
  )
}


# ------------------------------------------------------------
# 2) Axis-parallel solver
#
# u1 = u2 = u, so L2 is vertical: x = u
# v1 = v2 = v, so L1 is horizontal: y = v
#
# Area formulas:
#   a = u*v
#   b = (W-u)*v
#   c = (W-u)*(H-v)
#   d = u*(H-v)
#
# Compatibility condition:
#   a*c = b*d
# ------------------------------------------------------------
cut_rectangle_axis_parallel <- function(a, b, c, d, W, tol = 1e-10) {
  vals <- c(a, b, c, d)
  if (any(vals < 0)) stop("Areas a, b, c, d must be nonnegative.")
  if (W <= 0) stop("W must be positive.")
  if (abs(sum(vals) - 1) > tol) {
    stop("Areas must sum to 1 (normalized rectangle area).")
  }

  H <- 1 / W

  if (abs(a * c - b * d) > 1e-8) {
    stop("These areas are not compatible with one horizontal and one vertical cut: need a*c = b*d.")
  }

  v <- (a + b) / W
  u <- W * (a + d)

  area_check <- list(
    a = u * v,
    b = (W - u) * v,
    c = (W - u) * (H - v),
    d = u * (H - v)
  )

  list(
    method = "axis_parallel",
    inputs = list(a = a, b = b, c = c, d = d, W = W, H = H),
    parameters = list(u = u, v = v),
    intersection = list(x = u, y = v),
    L1 = list(
      type = "horizontal",
      equation = sprintf("y = %.12f", v),
      y_as_function_of_x = list(intercept = v, slope = 0),
      endpoints = rbind(c(0, v), c(W, v))
    ),
    L2 = list(
      type = "vertical",
      equation = sprintf("x = %.12f", u),
      y_as_function_of_x = NULL,
      x_as_function_of_y = list(intercept = u, slope = 0),
      endpoints = rbind(c(u, 0), c(u, H))
    ),
    area_check = area_check
  )
}


# ------------------------------------------------------------
# 3) Wrapper
# ------------------------------------------------------------
cut_rectangle_lines <- function(a, b, c, d, W,
                                method = c("general", "axis_parallel"),
                                tol = 1e-10) {
  method <- match.arg(method)

  if (method == "general") {
    cut_rectangle_lines_general(a, b, c, d, W, tol = tol)
  } else {
    cut_rectangle_axis_parallel(a, b, c, d, W, tol = tol)
  }
}


# ------------------------------------------------------------
# 4) Plotting function
# ------------------------------------------------------------
plot_cut_rectangle <- function(res,
                               col_fill = c(
                                 a = rgb(0.85, 0.90, 1.00, 0.8),
                                 b = rgb(1.00, 0.90, 0.80, 0.8),
                                 c = rgb(0.85, 1.00, 0.85, 0.8),
                                 d = rgb(1.00, 0.85, 0.92, 0.8)
                               ),
                               line_col = "black",
                               lwd = 2,
                               show_labels = TRUE) {
  W <- res$inputs$W
  H <- res$inputs$H

  plot(NA,
       xlim = c(0, W), ylim = c(0, H),
       asp = 1,
       xlab = "x", ylab = "y",
       main = paste("Rectangle cut:", res$method))

  # Rectangle border
  polygon(c(0, W, W, 0), c(0, 0, H, H), border = "black", col = NA, lwd = 2)

  if (res$method == "axis_parallel") {
    u <- res$parameters$u
    v <- res$parameters$v

    # Fill four rectangles
    polygon(c(0, u, u, 0), c(0, 0, v, v), col = col_fill["a"], border = NA)
    polygon(c(u, W, W, u), c(0, 0, v, v), col = col_fill["b"], border = NA)
    polygon(c(u, W, W, u), c(v, v, H, H), col = col_fill["c"], border = NA)
    polygon(c(0, u, u, 0), c(v, v, H, H), col = col_fill["d"], border = NA)

    # Draw lines
    segments(0, v, W, v, col = line_col, lwd = lwd)
    segments(u, 0, u, H, col = line_col, lwd = lwd)

    if (show_labels) {
      text(u / 2, v / 2, labels = "a", cex = 1.4)
      text((u + W) / 2, v / 2, labels = "b", cex = 1.4)
      text((u + W) / 2, (v + H) / 2, labels = "c", cex = 1.4)
      text(u / 2, (v + H) / 2, labels = "d", cex = 1.4)
    }

  } else {
    u1 <- res$parameters$u1
    u2 <- res$parameters$u2
    v1 <- res$parameters$v1
    v2 <- res$parameters$v2
    xP <- res$intersection$x
    yP <- res$intersection$y

    # Fill the four polygonal regions
    polygon(c(0, u1, xP, 0), c(0, 0, yP, v1), col = col_fill["a"], border = NA)
    polygon(c(u1, W, W, xP), c(0, 0, v2, yP), col = col_fill["b"], border = NA)
    polygon(c(xP, W, W, u2), c(yP, v2, H, H), col = col_fill["c"], border = NA)
    polygon(c(0, xP, u2, 0), c(v1, yP, H, H), col = col_fill["d"], border = NA)

    # Draw lines
    segments(0, v1, W, v2, col = line_col, lwd = lwd)
    segments(u1, 0, u2, H, col = line_col, lwd = lwd)

    if (show_labels) {
      text(mean(c(0, u1, xP, 0)), mean(c(0, 0, yP, v1)), labels = "a", cex = 1.4)
      text(mean(c(u1, W, W, xP)), mean(c(0, 0, v2, yP)), labels = "b", cex = 1.4)
      text(mean(c(xP, W, W, u2)), mean(c(yP, v2, H, H)), labels = "c", cex = 1.4)
      text(mean(c(0, xP, u2, 0)), mean(c(v1, yP, H, H)), labels = "d", cex = 1.4)
    }
  }

  # redraw border on top
  polygon(c(0, W, W, 0), c(0, 0, H, H), border = "black", col = NA, lwd = 2)
}


# ------------------------------------------------------------
# Example usage
# ------------------------------------------------------------

# Example 1: axis-parallel
# res1 <- cut_rectangle_lines(
#   a = 0.20, b = 0.30, c = 0.30, d = 0.20,
#   W = 2,
#   method = "axis_parallel"
# )
# print(res1$L1)
# print(res1$L2)
# print(res1$area_check)
# plot_cut_rectangle(res1)

# Example 2: general constrained case
# res2 <- cut_rectangle_lines(
#   a = 0.18, b = 0.27, c = 0.31, d = 0.24,
#   W = 2,
#   method = "general"
# )
# print(res2$L1)
# print(res2$L2)
# print(res2$area_check)
# plot_cut_rectangle(res2)