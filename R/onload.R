.onLoad <- function(libname, pkgname) {

  sans <- system.file("font", "Open_Sans_Regular.ttf", package = "geomtextpath")
  mono <- system.file("font", "Web_Hebrew_Monospace_Regular.ttf",
                      package = "geomtextpath")

  sans_type1 <- grDevices::postscriptFonts()$sans
  grDevices::postscriptFonts(fallback = sans_type1)
  grDevices::pdfFonts(fallback = sans_type1)
  # Provide fallback fonts in case unable to detect system fonts
  if (nrow(systemfonts::system_fonts()) == 0) {
    systemfonts::register_font("fallback", sans)
    systemfonts::register_font("mono", mono)
  } else {
    systemfonts::register_font("fallback", systemfonts::font_info("")$path[1])
  }

}
