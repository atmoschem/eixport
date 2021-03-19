library(sf)
library(eixport)
library(ggmap)
geos <- list.files(pattern = "geo_em")


if(length(geos > 0)) cat("Detecting :", geos, "\n\n")

lg <- lapply(geos, wrf_grid, type = "geo")
for(i in seq_along(geos)) lg[[i]] <- st_sf(geometry = st_as_sfc(st_bbox(lg[[i]])))

g <- do.call("rbind", lg)
g <- st_cast(g, "LINESTRING")
bb <- st_bbox(g)
place <- c(left = bb[["xmin"]], bottom = bb[["ymin"]], right = bb[["xmax"]], top = bb[["ymax"]])
x <- get_stamenmap(place, zoom = 6)

p <- ggmap(x) + geom_sf(data = g, inherit.aes = F) + coord_sf(crs = 4326)

f <-  "doms.png"
png(f, width = 600, height = 600, units = "px", res = 70)
print(p)
dev.off()

cat("Opening doms\n")
system(paste0("eog ", f, "  &"))
