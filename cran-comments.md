Version 0.1.5 - 14th January 2025

Getting Additional Issues warnings on CRAN when using gcc-ASAN and clang-ASAN.
These appear to be due to memory allocation overlaps when the agg device is used to render graphics when building the vignettes.
There doesn't seem to be anything we can do at this end other than switch to the default png device for vignette graphics output.
There were no other problems found on any of the standard builds.
Also some patches to fix minor user-level bugs as documented in NEWS.md
Minor version bumped

R CMD check succeeded with 0 errors | 0 warnings | 0 notes

--------------------------------------------------------------------------------

Hotfix 0.1.4 - 11 June 2024

Bumped {textshaping} requirement for compatibility.
We checked 8 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


Fixing new CRAN error 12 March 2024

Failing test on some Linux machines due to different error message being produced on these machines when testing code for syntax errors

Patched and bumped version



Fixing CRAN errors - 8 March 2024

Multiple tests failing on latest R version which have been patched.
Dependencies updated to ggplot 3.5.0.
Version bumped to 0.1.2

```
devtools::check(remote = TRUE, manual = TRUE)

-- R CMD check results --------------------------------- geomtextpath 0.1.2 ----
Duration: 2m 32.4s

0 errors v | 0 warnings v | 0 notes v
```

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
