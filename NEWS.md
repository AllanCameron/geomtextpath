# geomtextpath 0.1.5

* Improved handling of empty data (#76, #100).
* The default `alpha` is no longer `1` but `NA` (#95).
* Fixed bug where `show.legend` was not working in `geom_textabline()` (#96).
* Fixed some partially matching arguments (#98)
* Labels placed in polar coordinates at `r = 0` are now drawn horizontally
  instead of throwing errors (#104).

# geomtextpath 0.1.4

* Fix for compatibility with textshaping 0.4.0.

# geomtextpath 0.1.3

* Fix for compatibility with ggplot2 3.5.0.

# geomtextpath 0.1.2

* Fixed invalid html

# geomtextpath 0.1.1

* Fixed a bug in the windows graphics device when using arrows (#66).
* Fixed HTML5 bug by updating Roxygen2 

# geomtextpath 0.1.0

Initial version released on CRAN.

* Added a `NEWS.md` file to track changes to the package.
* Plot text atop curve for readability purposes.
* Support for `lineend`, `linejoin`, `linemitre` parameters.
* Letter angles should now be stable for regardless of aspect ratios and 
  recomputed when the plot device is resized (#6).
* Line breaks now implemented (#4).
* Added support for user-defined text spacing.
* Text angles are now correct for straight paths.
* Duplicated labels are now allowed.
* Multi-line text can be justified horizontally using the `halign` parameter.
* Added support for bidirectional text and composite Unicode glyphs (#).
* Added plotmath support (#25).
* Added richtext support (#39).
* Added `coord_curvedpolar` for curved polar axis labels (#5).
* Labels on single points will now be placed on paths to allow them to curve in 
  polar co-ordinates. This means `geom_textpath` can be used as a drop-in for
  `geom_text` in most circumstances.
* Added text and label equivalents of all line based geom layers.
* Added `geom_textsf` and `geom_labelsf` (#41)
* Added vignettes to cover use cases, aesthetics, polar co-ordinates and an 
  introduction to the package.
* Added smoothing options (#11, #21)
* Created website based on vignettes via `pkgdown`
