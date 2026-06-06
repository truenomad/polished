# Build the polisapi2 hex sticker.
# Writes man/figures/logo.png. Not part of the package build — see
# .Rbuildignore — this script lives here so the artifact is
# reproducible. Run from the package root with:
#   Rscript data-raw/build_logo.R

# install if missing
# pak::pak(c("hexSticker", "ggplot2", "tibble", "sysfonts", "showtext"))

# fonts ------------------------------------------------------------------
sysfonts::font_add_google("Inter", "inter")
showtext::showtext_auto()

# palette ----------------------------------------------------------------
polis_red <- "#a3122c"
charcoal <- "#1a1a1a"

# build node geometry ----------------------------------------------------
# inner ring: six vertices of a hexagon, scaled small
ring_coords <- function(n_nodes, radius) {
  angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[1:n_nodes] - pi / 2
  tibble::tibble(
    x = radius * cos(angles),
    y = radius * sin(angles)
  )
}

output_path <- here::here("man", "figures", "logo.png")
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

# right-skewed surveillance signal: rise to peak then slower decline
curve_x <- seq(0, 1, length.out = 200)
curve_raw <- dgamma(curve_x * 4, shape = 2.4, rate = 1.5)

epi_curve <- tibble::tibble(
  x = curve_x,
  y = curve_raw / max(curve_raw)
)

obs_x <- seq(0.05, 0.95, length.out = 7)
epi_points <- tibble::tibble(
  x = obs_x,
  y = dgamma(obs_x * 4, shape = 2.4, rate = 1.5) / max(curve_raw)
)

subplot_curve <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = epi_curve,
    ggplot2::aes(x = x, y = y),
    colour = polis_red,
    linewidth = 1.0
  ) +
  ggplot2::geom_point(
    data = epi_points,
    ggplot2::aes(x = x, y = y),
    colour = polis_red,
    size = 1.5
  ) +
  ggplot2::coord_cartesian(
    xlim = c(-0.05, 1.05),
    ylim = c(-0.05, 1.2)
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.background = ggplot2::element_blank())

# sticker call -----------------------------------------------------------
hexSticker::sticker(
  subplot = subplot_curve,
  package = "polisapi2",
  p_size = 40,
  p_color = charcoal,
  p_family = "inter",
  p_y = 1.4, # title above centre
  s_x = 1,
  s_y = 0.72, # curve sits in lower portion
  s_width = 1.5, # wider — curve is horizontal
  s_height = 0.9, # shorter — curve doesn't need vertical
  h_fill = "white", # solid white inside the hex
  h_color = polis_red,
  h_size = 1.2,
  white_around_sticker = FALSE, # transparent canvas around the hex
  filename = output_path,
  dpi = 600
)
