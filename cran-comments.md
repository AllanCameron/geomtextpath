Fixing CRAN note - 29 August 2022

Invalid html newly detected; this has been fixed by updating Roxygen2, which was
used to create the Rd files from which the html was generated.
Patched to new 0.1.1

```
devtools::check(remote = TRUE, manual = TRUE)

-- R CMD check results --------------------------------- geomtextpath 0.1.1 ----
Duration: 2m 4.5s

0 errors v | 0 warnings v | 0 notes v
```

---

Pre-submission checks - 21st January 2022

Initial incoming checks failed on 19th January due to:
1) Issue with systemfonts dependency not detecting any fonts on the Windows development computer. This has been fixed by including two open source / free to use font files which are registered with systemfonts via `.onLoad` as a fallback. The fonts should never be required except on the CRAN machine, since this problem seems to be peculiar to it as far as we can tell.
2) Non-standard formatting of MIT license - now changed
3) Failure of single test on Debian due to its standard Sans font not appearing to support Hebrew glyphs - this test has been skipped.
  
```
devtools::check_win_devel()

# 0 errors  | 0 warnings  | 1 note 
```

```
devtools::check(remote = TRUE, manual = TRUE)

# checking CRAN incoming feasibility ... NOTE
# Maintainer: 'Allan Cameron <Allan.Cameron@nhs.scot>'
# 
# New submission
#
# 0 errors  | 0 warnings  | 1 note 
```
