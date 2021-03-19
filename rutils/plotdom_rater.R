library(eixport)
library(ggmap)
library(raster)
geos <- list.files(pattern = "geo_em")


if(length(geos > 0)) cat("Detecting :", geos, "\n\n")

lg <- lapply(geos, wrf_grid, type = "geo", as_raster = T)


bb <- as.vector(bbox(lg[[1]]))
place <- c(left = bb[1], bottom = bb[2], right = bb[3], top = bb[4])
x <- get_stamenmap(place, zoom = 6)


if(length(lg) == 1) {
r1 <- as.data.frame(rasterToPoints(lg[[1]]))
p <- ggmap(x) +
geom_raster(data = r1, aes(x =x, y = y), alpha = 0.1) +
coord_equal()

} else if(length(lg) == 2) {
r1 <- as.data.frame(rasterToPoints(lg[[1]]))
r2 <- as.data.frame(rasterToPoints(lg[[2]]))

p <- ggmap(x) +
geom_raster(data = r1, aes(x =x, y = y), alpha = 0.1) +
geom_raster(data = r2, aes(x =x, y = y), alpha = 0.2) +
coord_equal()

} else if(length(lg) == 3) {
r1 <- as.data.frame(rasterToPoints(lg[[1]]))
r2 <- as.data.frame(rasterToPoints(lg[[2]]))
r3 <- as.data.frame(rasterToPoints(lg[[3]]))

p <- ggmap(x) +
geom_raster(data = r1, aes(x =x, y = y), alpha = 0.1) +
geom_raster(data = r2, aes(x =x, y = y), alpha = 0.2) +
geom_raster(data = r3, aes(x =x, y = y), alpha = 0.3) +
coord_equal()

} else if(length(lg) == 4) {
r1 <- as.data.frame(rasterToPoints(lg[[1]]))
r2 <- as.data.frame(rasterToPoints(lg[[2]]))
r3 <- as.data.frame(rasterToPoints(lg[[3]]))
r4 <- as.data.frame(rasterToPoints(lg[[4]]))

p <- ggmap(x) +
geom_raster(data = r1, aes(x =x, y = y), alpha = 0.1) +
geom_raster(data = r2, aes(x =x, y = y), alpha = 0.2) +
geom_raster(data = r3, aes(x =x, y = y), alpha = 0.3) +
geom_raster(data = r4, aes(x =x, y = y), alpha = 0.4) +
coord_equal()

}

f <-  "doms.png"
png(f, width = 600, height = 600, units = "px", res = 70)
print(p)
dev.off()

cat("Opening doms\n")
system(paste0("eog ", f, "  &"))

