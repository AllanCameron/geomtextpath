df <- data.frame(x = rep(seq(0, 2*pi, length.out = 100), 2),
                 y = c(sin(seq(0, 2*pi, length.out = 100)),
                       cos(seq(0, 2*pi, length.out = 100))),
                 label = rep(c("My nice big long label",
                               "Another big long label"), each = 100))

ggplot(df, aes(x, y, label = label, color = label)) +
  geom_textpath(start_proportion = 0.5)

ggplot(df, aes(x = duration, color = monthly_run)) +
  geom_density() +
  geom_textpath(aes(label = monthly_run), stat = "density",
                vjust = - 0.3, start_proportion = 0.58)

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(alpha = 0.5) +
  geom_textpath(aes(label = Species,
                    color = stage(Species, after_scale = .darken(color, 1.5))),
                size = 5, stat = "smooth",
                start_proportion = 0.25, fontface = 2)

ggplot(iris, aes(x = Sepal.Length, color = Species)) +
  geom_density(alpha = 0.5) +
  geom_textpath(aes(label = as.character(Species),
                    color = stage(Species, after_scale = .darken(color, 1.5)),
                    vjust = -0.1),
                size = 5, stat = "density",
                start_proportion = 0.12, fontface = 2)

spiral <- data.frame(x = rev(sin(seq(0, 5*pi, length.out = 1000)) * 1000:1),
                     y = rev(cos(seq(0, 5*pi, length.out = 1000)) * 1000:1),
                     s = seq(1, 10, length.out = 1000),
                     z = paste("Like a circle in a spiral, like a",
                               "wheel within a wheel, never ending",
                               "or beginning on an ever spinning reel"))

ggplot(spiral, aes(x, y, label = z)) +
  geom_textpath(start_proportion = 0.2, size = 5.9, vjust=-0.4) +
  geom_path(color = "gray90") +
  coord_equal() +
  theme_void()


df <- data.frame(x = 1:1000, y = 1, z = "This is a perfectly flat label")

p <- ggplot(df, aes(x, y, label = z)) +
     geom_textpath(start_proportion = 0.25, size = 6)

p

p + coord_polar()
