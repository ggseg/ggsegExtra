# Create hex logo for {REPO}
# This script creates a hex sticker matching the ggseg ecosystem style

library(ggplot2)
library(ggseg)
library(hexSticker)

# Load the atlas
load(here::here("data/{GGSEG}.rda"))

# Create the brain plot for the logo
p <- ggseg(
  atlas = {
    GGSEG
  },
  hemi = "left",
  view = "lateral",
  show.legend = FALSE,
  colour = "grey30",
  size = 0.2,
  mapping = aes(fill = region)
) +
  theme_void() +
  theme_transparent() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )

# Create output directory if needed
dir.create(here::here("man/figures"), recursive = TRUE, showWarnings = FALSE)

# Generate hex sticker in both PNG and SVG formats
for (ext in c("png", "svg")) {
  sticker(
    p,
    package = "{REPO}",
    filename = here::here(sprintf("man/figures/logo.%s", ext)),
    # Subplot positioning
    s_x = 1,
    s_y = 1.2,
    s_width = 1.5,
    s_height = 1.5,
    # Package name styling (ggseg ecosystem style)
    p_family = "mono",
    p_size = 8,
    p_color = "grey30",
    p_y = 0.55,
    # Hex styling (ggseg ecosystem style)
    h_fill = "white",
    h_color = "grey30",
    h_size = 1.2,
    # White background
    white_around_sticker = TRUE
  )
}

message("Hex logo created at man/figures/logo.png and man/figures/logo.svg")
