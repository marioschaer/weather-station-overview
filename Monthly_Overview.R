# =============================================================================
# Monthly Overview
# Measurement data: temperature, air pressure, precipitation, wind, wind rose and
# comparison with the climate of a MeteoSwiss station.
#
# -> Edit the "Settings" section
#
# Input data: hourly data, comma-separated, at least 12 columns
#   date, hour, temp, temp.max, temp.min, relative humidity, dewpoint, pressure,
#   wind.mean, gusts, winddir, precip (°C, hPa, m/s, degrees, mm).
# =============================================================================

# Clean Environment
rm(list = ls())
graphics.off()

# ---- Settings -------------------------------------------------------------

month       <- 10       # Month (1-12)
year        <- 2026
ylimprecip  <- 30      # Y-axis upper limit for daily precipitation [mm]
save.pdf    <- FALSE   # TRUE = save as PDF
station  <- "Basel / Binningen"  # name as in the MeteoSwiss files for climate comparison

# =============================================================================
# ---- Read and prepare Data --------------------------------------------------
# =============================================================================
library(climaemet)
library(ggplot2)
library(grid)
library(gridBase)

# Example file is located in the "data" subfolder next to the script
data_file <- "data/example_data.txt"
 data_file <- file.choose()   # Use this line to pick your own file

data_month <- read.table(data_file, sep = ",")

colnames(data_month)[1:12] <- c("date", "hour", "temp", "temp.max", "temp.min", "rh",
                                "dewpoint", "pressure", "wind.mean", "gusts",
                                "winddir", "precip")
data_month$wind.mean <- data_month$wind.mean * 3.6
data_month$gusts <- data_month$gusts * 3.6

# Convert date
data_month$date <- as.Date(data_month$date, format = "%Y-%m-%d")

# Number of days in the month
daysmonth <- as.integer(format(seq(as.Date(sprintf("%d-%02d-01", year, month)),
                                   by = "month", length.out = 2)[2] - 1, "%d"))

# ---- Climate data of a MeteoSwiss station ------------------------------------
# (T = temperature [°C], P = precipitation [mm]; 6190 = 1961-1990, 9120 = 1991-2020)

# Download and unpack opendata
url <- "https://data.geo.admin.ch/ch.meteoschweiz.klima/normwerte/normwerte.zip"
zip <- file.path(tempdir(), "normwerte.zip")
download.file(url, zip, mode = "wb", quiet = TRUE)
files <- unzip(zip, exdir = tempdir())

# 12 monthly values (columns 5-16) of the station for one parameter and period
get_norm <- function(parameter, period) {
  f    <- files[grepl(parameter, files) & grepl(period, files)][1]
  rows <- strsplit(iconv(readLines(f, warn = FALSE), "latin1", "UTF-8"), "\t")  # files are Latin-1
  rows <- rows[sapply(rows, function(r) length(r) >= 16 && station %in% trimws(r[1:4]))]
  if (length(rows) != 1) stop("Station not found: ", station)
  as.numeric(gsub(",", ".", rows[[1]][5:16]))
}
# only 2m-Temperature and Precipitation data is needed
mch <- data.frame(month  = month.name,
                  T.6190 = get_norm("tre200m0", "1961-1990"),
                  T.9120 = get_norm("tre200m0", "1991-2020"),
                  P.6190 = get_norm("rre150m0", "1961-1990"),
                  P.9120 = get_norm("rre150m0", "1991-2020"))

# Normal values for the chosen month
mch.T90 <- mch$T.6190[month]
mch.T20 <- mch$T.9120[month]
mch.P   <- mch$P.9120[month]

# ---- Key Statistics ----------------------------------------------------------

# Temperature: 
T_mean     <- mean(data_month$temp, na.rm = TRUE)
T_mean.txt <- format(round(T_mean, 1), nsmall = 1)

# Precipitation: (days without rain = NA)
precip_total <- round(sum(data_month$precip, na.rm = TRUE),4)
daily_sum    <- colSums(matrix(data_month$precip, 24, daysmonth), na.rm = TRUE)
daily_sum[daily_sum == 0] <- NA

# Wind direction: if wind <= 0.1 m/s -> NA
winddir <- data_month$winddir
winddir[is.na(data_month$wind.mean) | data_month$wind.mean < 0.1] <- NA

# Position of the max / min value of each day (plot)
idx.max <- sapply(1:daysmonth, function(d) {
  ind <- ((d - 1) * 24 + 1):(d * 24)
  ind[which.max(data_month$temp.max[ind])]
})
idx.min <- sapply(1:daysmonth, function(d) {
  ind <- ((d - 1) * 24 + 1):(d * 24)
  ind[which.min(data_month$temp.min[ind])]
})

# =============================================================================
# ---- Plots ----------------------------------------------------------------
# =============================================================================
if (save.pdf) {
  pdf(sprintf("Grafik Monatsüberblick/Monatsüberblick%02d%02d.pdf", year %% 100, month),
      width = 6.8, height = 8.2424242424)
}

# Layout: 3 full time series on top, wind rose and comparison with climate below
layout(matrix(c(1, 1, 1, 1,
                2, 2, 2, 2,
                3, 3, 3, 3,
                4, 4, 5, 5), nrow = 4, byrow = TRUE),
       heights = c(1, 1, 1, 1), widths = c(0.73333, 0.73333, 1, 1))

par(mar = c(2, 4, 1.8, 5.2))


# ---- Plot 1: Temperature (red, left) and pressure (grey, right) ----

# Pressure
plot(1:nrow(data_month), data_month$pressure, axes = FALSE, xaxs = "i", type = "l",
     lty = 1, lwd = 0.5, ylab = "", col = "gray60",
     xlab = paste("Mean:", T_mean.txt, "°C   ", "Norm:", mch.T20, "°C"),
     mgp = c(1, 1, 2))
axis(4, las = 2, col.axis = "gray60")

# Temperature:
par(new = TRUE)
plot(1:nrow(data_month), data_month$temp, axes = FALSE, type = "n", xaxs = "i",
     ylab = "", xlab = "", main = paste(mch$month[month], year))
axis(2, las = 2, col.axis = "red3")
box(lwd = 1.25)
abline(v = seq(1, nrow(data_month), 24), lty = 1, lwd = 0.2, col = "grey")   # Days
abline(h = seq(-30, 45, 5), lty = 1, lwd = 0.2, col = "grey")
abline(h = T_mean, lty = 3, col = "red")                                      # Monthly mean
abline(h = mch.T20, lwd = 0.5, col = "red")                                    # Normal 1991-2020
abline(h = 0, lty = 1, lwd = 0.1)
lines(1:nrow(data_month), data_month$temp, col = "red4", lwd = 1.25)

# Daily maximum (red) and daily minimum (blue)
points(idx.max, data_month$temp.max[idx.max], pch = 19, col = "red3",  cex = 0.3)
points(idx.min, data_month$temp.min[idx.min], pch = 19, col = "blue3", cex = 0.3)

mtext("Pressure [hPa]",   side = 4, line = 3.5, cex = 0.7)
mtext("Temperature [°C]", side = 2, line = 2.5, cex = 0.7)


# ---- Plot 2: Precipitation---------------------------------------------------

# Scale and grid (left axis = daily sums)
plot(1:daysmonth, daily_sum, axes = FALSE, yaxs = "i", xaxs = "i", type = "n",
     ylim = c(0, ylimprecip), ylab = "", xlab = "")
abline(h = seq(0, 60, 5), lty = 1, lwd = 0.2, col = "grey")
box(lwd = 1.25)

# Monthly total (dotted line) and normal value (right axis)
par(new = TRUE)
plot(1:daysmonth, rep(precip_total, daysmonth), type = "l", lty = 3, axes = FALSE,
     xaxs = "i", yaxs = "i", ylab = "", ylim = c(0, 150),
     xlab = paste("Total:", precip_total, "mm   ", "Norm:", mch.P, "mm"),
     mgp = c(1, 1, 0))
abline(h = mch.P, lwd = 0.5)
axis(4, las = 2, at = seq(0, 250, 25), lwd = 1)

# Daily sums (bars)
par(new = TRUE)
barplot(daily_sum, axes = FALSE, yaxs = "i", xaxs = "i", ylim = c(0, ylimprecip),
        col = "dodgerblue3", border = "dodgerblue3", ylab = "", lwd = 3, xlab = "")
grid(ny = NA, nx = daysmonth, lty = 1, lwd = 0.2, col = "grey")
box()
axis(2, las = 2, at = seq(0, 60, 10), col.axis = "dodgerblue3")

mtext("Total Precipitation [mm]", side = 4, line = 3.5, cex = 0.7)
mtext("Precipitation [mm]",       side = 2, line = 2.5, cex = 0.7)

# ---- Plot 3: Wind -----------------------------------------------------------

days <- ((1:nrow(data_month)) - 1) / 24 + 1    # Time axis in days

plot(days, winddir, axes = FALSE, xaxs = "i", type = "p", cex = 0.5, pch = 19,
     col = "gray75", ylim = c(1, 360), ylab = "", xlab = "")
axis(4, at = c(1, 45, 90, 135, 180, 225, 270, 315, 360),
     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
     las = 2, col.axis = "gray60")
abline(v = seq(1, daysmonth, 1), lty = 1, lwd = 0.2, col = "grey")

par(new = TRUE)
plot(days, data_month$wind.mean, axes = FALSE, xaxs = "i", yaxs = "i",
     type = "n", ylim = c(0, 60), ylab = "", xlab = "")
abline(h = seq(0, 100, 10), lty = 1, lwd = 0.2, col = "grey")
lines(days, data_month$wind.mean, lty = 1, lwd = 0.75)    # Mean wind
lines(days, data_month$gusts, col = "green4")             # Gusts

axis(1, at = seq(1, nrow(data_month) / 24, 2), labels = seq(1, nrow(data_month) / 24, 2))
axis(2, las = 2, col.axis = "green4")
box(lwd = 1.25)

mtext("Wind Direction", side = 4, line = 3.5, cex = 0.7)
mtext(expression(paste("Wind [km h"^"-1" * "]")), side = 2, line = 2.5, cex = 0.7)


# ---- Plot 4: Wind rose ------------------------------------------------------
windrose <- ggwindrose(
  speed = data_month$wind.mean,
  direction = winddir,
  n_directions = 8,
  n_speeds = 5,
  calm_wind = 0.1
) +scale_fill_grey(start = .6, end = 0) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 7)
  )
plot.new()                     
vps <- baseViewports()
pushViewport(vps$figure)  
print(windrose, newpage = FALSE)
popViewport()


# ---- Plot 5: Comparison with climate data (1991-2020 and 1961-1990) ---------

par(mar = c(2.5, 4, 1.75, 5))

# Precipitation climate 1991-2020 (bars, right axis)
plot(mch$P.9120, lend = 1, type = "h", yaxs = "i", col = "grey80", lwd = 5,
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 500))
axis(4, las = 2, at = seq(0, 150, 25), col.axis = "grey70")
mtext("Precipitation [mm]", side = 4, line = 3.5, cex = 0.7)
points(month, precip_total, col = "dodgerblue3", cex = 2, lwd = 1, pch = "-")
box()

# Temperature climate (short lines: 1991-2020 and 1961-1990)
norm.T   <- list(mch$T.9120, mch$T.6190)
norm.col <- c("grey60", "grey85")
for (k in 1:2) {
  par(new = TRUE)
  plot(norm.T[[k]], axes = FALSE, yaxs = "i", lty = 1, type = "p", pch = "-", cex = 2,
       col = norm.col[k], ylab = "Temperature [°C]", ylim = c(-5, 25), xlab = "")
}
axis(1, cex.axis = 0.85, at = 1:12,
     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
axis(2, las = 2)

# Mean temperature of current month
points(month, T_mean, pch = "-", lwd = 1.5, col = "red3", cex = 2)

# Climate values (legend)
mtext(paste(mch.T20, "°C (91-20)"), side = 1, line = -10.2,   cex = 0.4, adj = 0.08)
mtext(paste(mch.T90, "°C (61-90)"), side = 1, line = -9.4, cex = 0.4, adj = 0.08)
mtext(paste(mch.P, "mm (91-20)"),   side = 1, line = -8.6,  cex = 0.4, adj = 0.08)


if (save.pdf) dev.off()