
# use google fonts to add atkinson hyperlegible font
library(showtext)
font_add_google("Atkinson Hyperlegible", "atkinson")
showtext_auto()

# define colors
did_text_blue <- "#BBDBEC"
did_background_blue <- "#2D6B97"
did_text_yellow <- "#FFCD00"

did_palette3 <- c(did_text_blue, did_background_blue, did_text_yellow)

# test readability color contrast
library(colorblindcheck)
palette_check(did_palette3)
