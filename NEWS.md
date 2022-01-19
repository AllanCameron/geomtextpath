# geomtextpath 0.1.0

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
* Added support for bidirectional text and composite Unicode glyphs.
* Added plotmath support.
* Added richtext support.
* Added `coord_curvedpolar` for curved polar axis labels.
* Labels on single points will now be placed on paths to allow them to curve in 
  polar co-ordinates. This means `geom_textpath` can be used as a drop-in for
  `geom_text` in most circumstances.
* Added text and label equivalents of all line based geom layers.
* Added `geom_textsf` and `geom_labelsf`
* Added vignettes to cover use cases, aesthetics, polar co-ordinates and an 
  introduction to the package.
* Created website based on vignettes via `pkgdown`
