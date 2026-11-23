# wrapping first and last labels works as expected

    Code
      axis_labels$textpath$label[[9]]$glyph
    Output
      [1] "1" "/" "1" "0"

---

    Code
      make_label(axis_labels$textpath$label[[9]]$glyph)
    Output
      [1] "1" "/" "1" "0"

