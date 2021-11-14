# geomtextpath

## Create curved text in ggplot2

The existing text-based geom layers in ggplot2 (`geom_text` and `geom_label`) are ideal for the majority of plots, since typically textual annotations are short, straight and in line with the axes of the plot. However, there are some occasions when it is useful to have text follow a curved path. This may be to create or recreate a specific visual effect, or it may be to label a circular / polar plot in a more "natural" way.

There are limitations inherent in the plotting of text elements in ggplot due to the way that the underlying `grid` graphics handles text. A text string is dealt with as a zero-width object, and therefore the rotation and spacing of the letters making up the string can only be dealt with by treating each letter separately. Inevitably, this means that curved text paths have to be calculated based on the size and aspect ratio of the plotting device. Resizing the device after drawing a curved text path will therefore cause artefacts of spacing and rotation in the text.

It is important to realise that the letters are only rotated, and do not undergo any change in shape. Thus, for example, large text appearing on convex curves will not be deformed so that individual letters are narrower at the bottom and wider at the top. Doing so would require reinterpreting the letters as polygons.

Another issue is that we may wish to use a short curved label on a much longer path. Spacing the letters equally along the path would mean there is too much space between the letters for the label to remain legible. A single text string is therefore kept "together" according to the point size of the text in `geom_textpath`. This then leaves the problem of where on the path the text should be placed. This can be dealt with by the aesthetic mapping `hjust`, which allows the user to place the labels at the desired position along the path, including separate positions for each label.

A final point to note is that a path is usually a group-based geom (i.e. a path typically comprises x, y points from two columns over several rows of a data frame), whereas text labels can come from single rows in a data frame. This means that if we have a data frame with an x column, a y column and a grouping variable column, there can only be a single label for the group. Typically, this will be the grouping variable itself (see the examples, particularly those using the built-in iris data set.)


## Using `geom_textpath`

At the moment, the best way to install the package is with
```r
remotes::install_git("AllanCameron/geomtextpath")

```
Then, once installed, remember to do
``` r
library(geomtextpath)
#> Loading required package: ggplot2
```

Now you can use `geom_textpath` the same as you would use any other geom. It's best to show how it works with some examples.

## Examples

### Plot text along an arbitrary path

``` r
spiral <- data.frame(x = rev(sin(seq(0, 5*pi, length.out = 1000)) * 1000:1),
                     y = rev(cos(seq(0, 5*pi, length.out = 1000)) * 1000:1),
                     s = seq(1, 10, length.out = 1000),
                     z = paste("Like a circle in a spiral, like a",
                               "wheel within a wheel, never ending",
                               "or beginning on an ever spinning reel"))

ggplot(spiral, aes(x, y, label = z)) +
  geom_textpath(size = 7.1, vjust = 2, linewidth = 0) +
  coord_equal(xlim = c(-1500, 1500), ylim = c(-1500, 1500))
```

![](https://i.imgur.com/l3dthgK.png)

### Produce labelled density lines:

By default the paths are broken to allow the names in-line

``` r
 ggplot(iris, aes(x = Sepal.Length, color = Species)) +
  geom_textpath(aes(label = Species),
                size = 8, stat = "density",
                fontface = 2, hjust = 0.2)
```

![](https://i.imgur.com/QnKqs3A.png)

If the vjust parameter moves the text above or below the line, the line is automatically filled in:
``` r
ggplot(iris, aes(x = Sepal.Length, color = Species)) +
  geom_textpath(aes(label = Species), vjust = -0.1,
                size = 8, stat = "density",
                fontface = 2, hjust = 0.2)
```

![](https://i.imgur.com/T5Ux4hW.png)


# label groups of points along their trend line:
``` r
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(alpha = 0.1) +
  geom_textpath(aes(label = Species, color = Species),
                size = 8, stat = "smooth", linetype = 3,
                fontface = 2, linewidth = 3) +
  scale_color_manual(values = c("forestgreen", "deepskyblue4", "tomato4")) +
  theme_bw()
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](https://i.imgur.com/amDUSBs.png)

### Straight text paths in Cartesian Co-ordinates curve in Polar Co-ordinates
```r
df <- data.frame(x = 1:1000, y = 1, z = "This is a perfectly flat label")

p <- ggplot(df, aes(x, y, label = z)) +
   geom_textpath(size = 6)

p
```

![](https://i.imgur.com/cUurt23.png)

``` r
p + coord_polar()
```

![](https://i.imgur.com/ArstzYQ.png)


