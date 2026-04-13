# Simple static histograms for the Chapter 12 "sliding distribution" idea.
# Change the values in `centers` to move the histograms manually.

make_hist <- function(center = 0,
                      observed = 6.05,
                      se = 3.38,
                      n = 1500,
                      xlim = c(-8, 20),
                      breaks = 28,
                      seed = 123) {
  set.seed(seed)
  x <- rnorm(n, mean = center, sd = se)
  hist(x,
       breaks = breaks,
       col = "#8DB3E2",
       border = "white",
       main = "",
       xlab = expression(b[1]),
       ylab = "",
       yaxt = "n",
       xlim = xlim)
  abline(v = observed, col = "#1F2937", lwd = 3)
  abline(v = center, col = "#D95550", lwd = 2, lty = 2)
  abline(v = center - 1.96 * se, col = "#D95550", lwd = 1.5, lty = 3)
  abline(v = center + 1.96 * se, col = "#D95550", lwd = 1.5, lty = 3)
  mtext(paste0("hypothesized beta1 = ", round(center, 2)), side = 3, line = 0.3, col = "#D95550")
  text(observed, par("usr")[4] * 0.95, labels = "observed b1 = 6.05", pos = 4, cex = 0.9)
}

# Option 1: show the three histograms in one window
centers <- c(0, 6.05, 12.76)
par(mfrow = c(1, 3), mar = c(4, 3, 3, 1))
for (i in seq_along(centers)) {
  make_hist(center = centers[i], seed = 100 + i)
}

# Option 2: save each histogram as a separate PNG for PowerPoint / Morph
for (i in seq_along(centers)) {
#  png(filename = paste0("hist_", i, ".png"), width = 1600, height = 750, res = 160)
  par(mar = c(4, 3, 3, 1))
  make_hist(center = centers[i], seed = 100 + i)
#  dev.off()
}
