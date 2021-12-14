# Libraries ---------------------------------------------------------------

library(geomtextpath)
library(grid)
library(ragg)
library(here)
library(systemfonts)
library(usethis)

# Setup files -------------------------------------------------------------

logo_file <- here("man", "figures", "logo_original.png")

# Setup data --------------------------------------------------------------

# Quadrant of circle
t <- seq(pi, 1.5 * pi, length.out = 180)
path <- data.frame(x = cos(t), y = sin(t))

# Hexagon for outline and border
hex <- seq(1.5 * pi, -0.5 * pi, length.out = 7)
hex <- data.frame(
  x = cos(hex),
  y = sin(hex)
)

# Gridlines
gridlines <- data.frame(
  pos  = c(-0.5, 0, 0.5, seq(-0.75, 0.75, by = 0.25)),
  size = rep(c(1, 0.5), c(3, 7))
)

# Tickmarks
tickmarks <- data.frame(
  x    = c(rep(-0.50, 21), seq(-0.5, 0.5, by = 0.05)),
  xend = c(rep(-0.52, 21), seq(-0.5, 0.5, by = 0.05)),
  y    = c(seq(-0.5, 0.5, by = 0.05), rep(-0.50, 21)),
  yend = c(seq(-0.5, 0.5, by = 0.05), rep(-0.52, 21))
)

# Angular tickmarks
angleticks <- seq(1.5 * pi, 1 * pi, length.out = 10)
angleticks <- data.frame(
  x    = cos(angleticks) * 1.0 + 0.6,
  xend = cos(angleticks) * 0.8 + 0.6,
  y    = sin(angleticks) * 1.0 + 0.6,
  yend = sin(angleticks) * 0.8 + 0.6
)

# Setup plot constants ----------------------------------------------------

blues <- c("#4F65D8", "#3B51D4", "#2336A3")
shrink <- 0.8660254 # Let's border coincide with vertical gridlines

fontfamily <- system_fonts()
if ("Montserrat" %in% fontfamily$family) {
  fontfamily <- "Montserrat"
} else {
  fontfamily <- ""
}

# Setup plot --------------------------------------------------------------

p <- ggplot(mapping = aes(x, y)) +
  geom_polygon(data = hex, fill = blues[2]) +
  geom_polygon(data = hex, aes(x = x * shrink, y = y * shrink),
               fill = NA, colour = "#ffffff88", size = 2) +
  geom_segment(data = tickmarks, aes(xend = xend, yend = yend),
               colour = "white", alpha = 0.5) +
  geom_segment(data = angleticks, aes(xend = xend, yend = yend),
               colour = "white", alpha = 0.3) +
  geom_vline(data = gridlines, aes(xintercept = pos, size = I(size)),
             colour = "#ffffff22") +
  geom_hline(data = gridlines, aes(yintercept = pos, size = I(size)),
             colour = "#ffffff22") +
  geom_path(data = path, aes(x * 0.9 + 0.6, y * 0.9 + 0.6),
            colour = "white", size = 0,
            arrow = arrow(type = "closed", angle = 15, length = unit(0.5, "in"))) +
  geom_path(data = head(path, -5), aes(x * 0.9 + 0.6,  y * 0.9 + 0.6),
            colour = "white", size = 2) +
  geom_textpath(data = path, aes(x = x + 0.6, y + 0.6),
                label = "geomtextpath", linetype = 0, colour = "white",
                family = fontfamily, fontface = "bold", size = 18) +
  coord_equal(expand = FALSE) +
  theme_void()

# Edit plot ---------------------------------------------------------------

# Extract panel
gt  <- ggplotGrob(p)
row <- panel_rows(gt)
col <- panel_cols(gt)
gt  <- gt[row[1,1], col[1,1]]

# Find border polygon
nms  <- names(gt$grobs[[1]]$children)
poly <- grep("geom_polygon.polygon", nms)[1]

# Set radial fill
gt$grobs[[1]]$children[[poly]]$gp$fill <- radialGradient(blues)
gt$vp <- viewport(mask = gt$grobs[[1]]$children[[poly]])

grid.newpage(); grid.draw(gt)

# Save plot ---------------------------------------------------------------

agg_png(width = 4.39, height = 5.02, units = "cm", res = 300,
        filename = logo_file, scaling = 0.25, background = "transparent")
plot.new()
grid.draw(gt)

dev.off()

# Set as logo -------------------------------------------------------------

usethis::use_logo(logo_file)

