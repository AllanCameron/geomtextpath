First release:

* [X] `usethis::use_cran_comments()`
* [X] Update (aspirational) install instructions in README
* [X] Proofread `Title:` and `Description:`
* [X] Check that all exported functions have `@return` and `@examples`
* [X] Check that `Authors@R:` includes a copyright holder (role 'cph')
* [X] Check [licensing of included files](https://r-pkgs.org/license.html#code-you-bundle)
* [X] Review <https://github.com/DavisVaughan/extrachecks>

Prepare for release:

* [X] `devtools::build_readme()`
* [X] `urlchecker::url_check()`
* [X] `devtools::check(remote = TRUE, manual = TRUE)`
* [X] `devtools::check_win_devel()`
* [X] `rhub::check_for_cran()`
* [X] Review pkgdown reference index for, e.g., missing topics

Submit to CRAN:

* [ ] `usethis::use_version('minor')`
* [ ] `devtools::submit_cran()`
* [ ] Approve email

Wait for CRAN...

* [ ] Accepted :tada:
* [ ] `usethis::use_github_release()`
* [ ] `usethis::use_dev_version()`
* [ ] Finish blog post
* [ ] Tweet
* [ ] Add link to blog post in pkgdown news menu
