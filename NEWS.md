# geomtextpath 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Plot text atop curve for readability purposes.
* Support for `lineend`, `linejoin`, `linemitre` parameters.
* Letter angles should now be stable for regardless of aspect ratios and 
  recomputed when the plot device is resized (#6)
* Text angles are now correct for straight paths.
* Duplicated labels are now allowed
