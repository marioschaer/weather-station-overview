# =============================================================================
# Wetterstation: Monatsüberblick
# Messdaten: Temperatur [°C], Luftdruck [hPa], Niederschlag [mm], Wind [km/h], Windrose und Vergleich mit den
# Normwerten von Basel-Binningen (MeteoSchweiz).
#
# Nur den Abschnitt "Einstellungen" anpassen, dann das ganze Skript ausführen.
#
# Eingabedaten: stündliche Daten, Komma-getrennt, mind. 12 Spalten
#   date, hour, temp, temp.max, temp.min, rh, dewpoint, pressure,
#   wind.mean, gusts, winddir, precip
# (°C, hPa, m/s, Grad, mm). Ab April 2021 folgt zusätzlich eine Spalte mit
# Solarstrahlung, sie wird hier nicht verwendet.
# =============================================================================

# Clean Environment
rm(list = ls())
graphics.off()


# ---- Einstellungen -------------------------------------------------------------

month       <- 10     # Monat (1-12)
year        <- 2026
ylimprecip  <- 30     # Y-Achse Niederschlag [mm]
save.pdf    <- FALSE   # TRUE = als PDF speichern, FALSE = im Grafikfenster zeigen

# ---- Daten einlesen ------------------------------------------------------------

# Beispieldatei liegt im Unterordner "data" neben dem Skript
data_file <- "data/Beispiel_Messdaten.txt"
# Eigene Datei auswählen: die Zeile darüber auskommentieren und diese aktivieren
# data_file <- file.choose()

data.month <- read.table(data_file, sep = ",")

colnames(data.month)[1:12] <- c("date", "hour", "temp", "temp.max", "temp.min", "rh",
                                "dewpoint", "pressure", "wind.mean", "gusts",
                                "winddir", "precip")
data.month$date <- format(strptime(data.month$date, format = "%Y-%m-%d"), format = "%Y/%m/%d")

# Anzahl Tage des Monats (aus Monat und Jahr berechnet)
daysmonth <- as.integer(format(seq(as.Date(sprintf("%d-%02d-01", year, month)),
                                   by = "month", length.out = 2)[2] - 1, "%d"))


# ---- Normwerte Basel-Binningen -------------------------------------------------
# T = Temperatur [°C], P = Niederschlag [mm]; Zahlen = Normperiode (z. B. 8110 = 1981-2010)

BB <- data.frame(
  Monat = c("January", "February", "March", "April", "May", "June", "July",
            "August", "September", "October", "November", "December"),
  T.6190 = c(0.9, 2.4, 5.6, 9.1, 13.1, 16.3, 18.5, 17.7, 14.8, 10.1, 4.9, 1.8),
  T.8110 = c(1.6, 2.7, 6.6, 10.0, 14.2, 17.4, 19.7, 19.1, 15.1, 10.9, 5.5, 2.6),
  T.9120 = c(2.2, 3.2, 7.0, 10.7, 14.6, 18.2, 20.2, 19.7, 15.4, 11.1, 6.0, 2.9),
  P.8110 = c(47, 45, 55, 64, 99, 86, 91, 80, 78, 73, 59, 66),
  P.9120 = c(48, 45, 50, 64, 98, 87, 89, 88, 70, 74, 65, 65)
)

# Normwerte des gewählten Monats
BB.T90 <- BB$T.6190[month]
BB.T10 <- BB$T.8110[month]
BB.T20 <- BB$T.9120[month]
BB.P   <- BB$P.9120[month]


# ---- Kennzahlen ----------------------------------------------------------------

# Temperatur: Mittel (Zahl) und Mittel als Text für die Beschriftung
T.mean     <- mean(data.month$temp, na.rm = TRUE)
T.mean.txt <- format(round(T.mean, 1), nsmall = 1)

# Niederschlag: Monatssumme und Tagessummen (Tage ohne Regen = NA)
Precip.total <- sum(data.month$precip, na.rm = TRUE)
Tagessumme   <- colSums(matrix(data.month$precip, 24, daysmonth), na.rm = TRUE)
Tagessumme[Tagessumme == 0] <- NA

# Windrichtung: nur gültig, wenn der mittlere Wind >= 0.1 m/s ist, sonst NA
winddir <- data.month$winddir
winddir[is.na(data.month$wind.mean) | data.month$wind.mean < 0.1] <- NA

# Index des Maximums / Minimums jedes Tages
idx.max <- sapply(1:daysmonth, function(d) {
  ind <- ((d - 1) * 24 + 1):(d * 24)
  ind[which.max(data.month$temp.max[ind])]
})
idx.min <- sapply(1:daysmonth, function(d) {
  ind <- ((d - 1) * 24 + 1):(d * 24)
  ind[which.min(data.month$temp.min[ind])]
})


# ---- Plots ---------------------------------------------------------------------

if (save.pdf) {
  pdf(sprintf("Grafik Monatsüberblick/Monatsüberblick%02d%02d.pdf", year %% 100, month),
      width = 6.8, height = 8.2424242424)
}

# Layout: 3 Zeitreihen übereinander, darunter Windrose und Normvergleich
layout(matrix(c(1, 1, 1, 1,
                2, 2, 2, 2,
                3, 3, 3, 3,
                4, 4, 5, 5), nrow = 4, byrow = TRUE),
       heights = c(1, 1, 1, 1), widths = c(0.73333, 0.73333, 1, 1))

par(mar = c(2, 4, 1.8, 5.2))


# ---- Plot 1: Temperatur (rot, links) und Druck (grau, rechts) ----

# Druck als Hintergrundlinie
plot(1:nrow(data.month), data.month$pressure, axes = FALSE, xaxs = "i", type = "l",
     lty = 1, lwd = 0.5, ylab = "", col = "gray60",
     xlab = paste("Mean:", T.mean.txt, "°C   ", "Norm:", BB.T20, "°C"),
     mgp = c(1, 1, 2))
axis(4, las = 2, col.axis = "gray60")

# Temperatur: erst Achse und Gitter, dann die Linie darüber
par(new = TRUE)
plot(1:nrow(data.month), data.month$temp, axes = FALSE, type = "n", xaxs = "i",
     ylab = "", xlab = "", main = paste(BB$Monat[month], year))
axis(2, las = 2, col.axis = "red3")
box(lwd = 1.25)
abline(v = seq(1, nrow(data.month), 24), lty = 1, lwd = 0.2, col = "grey")   # Tage
abline(h = seq(-30, 45, 5), lty = 1, lwd = 0.2, col = "grey")
abline(h = T.mean, lty = 3, col = "red")                                      # Monatsmittel
abline(h = BB.T20, lwd = 0.5, col = "red")                                    # Norm 1991-2020
abline(h = 0, lty = 1, lwd = 0.1)
lines(1:nrow(data.month), data.month$temp, col = "red4", lwd = 1.25)

# Tagesmaxima (rot) und Tagesminima (blau)
points(idx.max, data.month$temp.max[idx.max], pch = 19, col = "red3",  cex = 0.3)
points(idx.min, data.month$temp.min[idx.min], pch = 19, col = "blue3", cex = 0.3)

mtext("Pressure [hPa]",   side = 4, line = 3.5, cex = 0.7)
mtext("Temperature [°C]", side = 2, line = 2.5, cex = 0.7)


# ---- Plot 2: Niederschlag (Tagessummen als Balken, Monatssumme als Linie) ----

# Skala und Gitter (linke Achse = Tagessummen)
plot(1:daysmonth, Tagessumme, axes = FALSE, yaxs = "i", xaxs = "i", type = "n",
     ylim = c(0, ylimprecip), ylab = "", xlab = "")
abline(h = seq(0, 60, 5), lty = 1, lwd = 0.2, col = "grey")
box(lwd = 1.25)

# Monatssumme (gepunktete Linie) und Norm (rechte Achse)
par(new = TRUE)
plot(1:daysmonth, rep(Precip.total, daysmonth), type = "l", lty = 3, axes = FALSE,
     xaxs = "i", yaxs = "i", ylab = "", ylim = c(0, 150),
     xlab = paste("Total:", Precip.total, "mm   ", "Norm:", BB.P, "mm"),
     mgp = c(1, 1, 0))
abline(h = BB.P, lwd = 0.5)
axis(4, las = 2, at = seq(0, 250, 25), lwd = 1)

# Tagessummen als Balken
par(new = TRUE)
barplot(Tagessumme, axes = FALSE, yaxs = "i", xaxs = "i", ylim = c(0, ylimprecip),
        col = "dodgerblue3", border = "dodgerblue3", ylab = "", lwd = 3, xlab = "")
grid(ny = NA, nx = daysmonth, lty = 1, lwd = 0.2, col = "grey")
box()
axis(2, las = 2, at = seq(0, 60, 10), col.axis = "dodgerblue3")

mtext("Total Precipitation [mm]", side = 4, line = 3.5, cex = 0.7)
mtext("Precipitation [mm]",       side = 2, line = 2.5, cex = 0.7)


# ---- Plot 3: Wind (Richtung als Punkte rechts, mittlerer Wind und Böen links in km/h) ----

days <- ((1:nrow(data.month)) - 1) / 24 + 1    # Zeitachse in Tagen

plot(days, winddir, axes = FALSE, xaxs = "i", type = "p", cex = 0.5, pch = 19,
     col = "gray75", ylim = c(1, 360), ylab = "", xlab = "")
axis(4, at = c(1, 45, 90, 135, 180, 225, 270, 315, 360),
     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
     las = 2, col.axis = "gray60")
abline(v = seq(1, daysmonth, 1), lty = 1, lwd = 0.2, col = "grey")

par(new = TRUE)
plot(days, data.month$wind.mean * 3.6, axes = FALSE, xaxs = "i", yaxs = "i",
     type = "n", ylim = c(0, 60), ylab = "", xlab = "")
abline(h = seq(0, 100, 10), lty = 1, lwd = 0.2, col = "grey")
lines(days, data.month$wind.mean * 3.6, lty = 1, lwd = 0.75)    # mittlerer Wind
lines(days, data.month$gusts * 3.6, col = "green4")             # Böen

axis(1, at = seq(1, nrow(data.month) / 24, 2), labels = seq(1, nrow(data.month) / 24, 2))
axis(2, las = 2, col.axis = "green4")
box(lwd = 1.25)

mtext("Wind Direction", side = 4, line = 3.5, cex = 0.7)
mtext(expression(paste("Wind [km h"^"-1" * "]")), side = 2, line = 2.5, cex = 0.7)


# ---- Plot 4: Windrose ----

# Anzahl Stundenwerte pro 10°-Sektor: Sektor 1 = Nord (355°-5°), danach 5°-15°, 15°-25° usw.
occ <- numeric(36)
occ[1] <- sum(winddir >= 355 | winddir < 5, na.rm = TRUE)
for (i in 0:34) {
  occ[i + 2] <- sum(winddir >= (5 + i * 10) & winddir < (15 + i * 10), na.rm = TRUE)
}
freq <- occ * 100 / sum(occ)          # Häufigkeit in Prozent

rose.scale <- 2.75                    # Skalierung der Rose
rad   <- 2 * pi / 360
polyg <- cbind(sin(seq(0, 360, 10) * rad), cos(seq(0, 360, 10) * rad))
polyg <- polyg[-37, ]                 # letzter Punkt (360°) = erster Punkt (0°)
rose  <- cbind(polyg[, 1] * freq, polyg[, 2] * freq) * rose.scale

plot(c(-50, 50), c(-50, 50), type = "n", xlab = "", ylab = "", axes = FALSE)

# Ringe bei 5, 10 und 15 %
symbols(0, 0, circles = 15 * rose.scale, inches = FALSE, add = TRUE, lty = 1, lwd = 0.5, bg = "grey96")
symbols(0, 0, circles = 10 * rose.scale, inches = FALSE, add = TRUE, lty = 1, lwd = 0.5, bg = "grey92")
symbols(0, 0, circles =  5 * rose.scale, inches = FALSE, add = TRUE, lty = 1, lwd = 0.5, bg = "grey88")

# Himmelsrichtungen
text(0,  55, "N", xpd = TRUE, font = 2)
text(55,  0, "E", xpd = TRUE, font = 2)
text(0, -55, "S", xpd = TRUE, font = 2)
text(-55, 0, "W", xpd = TRUE, font = 2)
text(-55 * .7071068,  55 * .7071068, "NW", xpd = TRUE, cex = 0.8)
text( 55 * .7071068, -55 * .7071068, "SE", xpd = TRUE, cex = 0.8)
text( 55 * .7071068,  55 * .7071068, "NE", xpd = TRUE, cex = 0.8)
text(-55 * .7071068, -55 * .7071068, "SW", xpd = TRUE, cex = 0.8)

# Rose zeichnen, Achsenkreuz darüber, danach nochmals die farbige Rose
polygon(rose, col = "white")
lines(c(47, -47), c(0, 0), lty = 1, lwd = 0.5)
lines(c(0, 0), c(47, -47), lty = 1, lwd = 0.5)
lines(c(-15, 15) * rose.scale * .7071068, c(-15, 15) * rose.scale * .7071068, lty = 1, lwd = 0.5)
lines(c(-15, 15) * rose.scale * .7071068, c(15, -15) * rose.scale * .7071068, lty = 1, lwd = 0.5)
polygon(rose, col = "lightblue4", lty = 0)


# ---- Plot 5: Monatswerte im Vergleich zu den Normwerten ----

par(mar = c(2.5, 4, 1.75, 5))

# Niederschlagsnorm 1981-2010 (Balken im Hintergrund, rechte Achse)
plot(BB$P.8110, lend = 1, type = "h", yaxs = "i", col = "grey80", lwd = 5,
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 500))
axis(4, las = 2, at = seq(0, 150, 25), col.axis = "grey70")
mtext("Precipitation [mm]", side = 4, line = 3.5, cex = 0.7)

# Temperaturnormen als Striche: 1981-2010, 1991-2020 (dunkler), 1961-1990
norm.T   <- list(BB$T.8110, BB$T.9120, BB$T.6190)
norm.col <- c("grey85", "grey60", "grey85")
for (k in 1:3) {
  par(new = TRUE)
  plot(norm.T[[k]], axes = FALSE, yaxs = "i", lty = 1, type = "p", pch = "-", cex = 2,
       col = norm.col[k], ylab = "Temperature [°C]", ylim = c(-5, 25), xlab = "")
}
axis(1, cex.axis = 0.85, at = 1:12,
     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
axis(2, las = 2)

# Monatsmittel des aktuellen Monats (rot)
points(month, T.mean, pch = "-", lwd = 1.5, col = "red3", cex = 2)

# Normwerte des Monats als Text
mtext(paste(BB.T20, "°C (91-20)"), side = 1, line = -11,   cex = 0.4, adj = 0.08)
mtext(paste(BB.T10, "°C (81-10)"), side = 1, line = -10.2, cex = 0.4, adj = 0.08)
mtext(paste(BB.T90, "°C (61-90)"), side = 1, line = -9.4,  cex = 0.4, adj = 0.08)
mtext(paste(BB.P, "mm (91-20)"),   side = 1, line = -7.8,  cex = 0.4, adj = 0.08)

# Niederschlagsnorm 1991-2020 (Balken) und Monatssumme (blauer Strich)
par(new = TRUE)
plot(BB$P.9120, lend = 1, type = "h", yaxs = "i", col = "grey80", lwd = 5,
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 500))
points(month, Precip.total, col = "dodgerblue3", cex = 2, lwd = 1, pch = "-")
box()

if (save.pdf) dev.off()
