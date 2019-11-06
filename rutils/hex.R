library(vein)
library(sf)
library(hexSticker)
data(net)
g2 <- st_make_grid(st_as_sfc(st_bbox(st_as_sf(net))),
                   cellsize = .055,
                   square = FALSE)
ge <- st_sf(id = 1:length(g2), geometry = st_sfc(g2))
netg <- emis_grid(spobj = sf::st_as_sf(net)[, "hdv"], g = ge)

sticker(~plot(netg["hdv"], axes = F, main = "", key.pos=NULL),
        package="eixport",
        s_x = 0.8,
        s_y = 0.7,
        s_width = 2,
        s_height = 2,
        p_x = 1,
        p_y = 1.6,
        p_color = "darkblue",
        p_family = "sans",
        p_size = 18,
        h_size = 1.2,
        h_fill = "white",
        h_color = "black",
        spotlight = FALSE,
        l_x = 1,
        l_y = 0.5,
        l_width = 3,
        l_height = 3,
        l_alpha = 0.4,
        url = "",
        u_x = 1,
        u_y = 0.08,
        u_color = "black",
        u_family = "Aller_Rg",
        u_size = 1.5,
        u_angle = 30,
        white_around_sticker = FALSE,
        filename = "man/figures/logo.png",
        asp = 1, dpi = 220)
