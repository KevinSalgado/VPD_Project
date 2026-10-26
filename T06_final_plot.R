# T06 - Rent as a share of the average salary, Portugal 2024
# Final visualization for the VPDA project.
#
# How to run: open this file from the project folder and run it top to bottom
# (or `Rscript T06_final_plot.R`). It reads two files from the data/ folder and
# writes the PNG next to this script.
#
# The design choices behind every number below are explained in design_decisions.md.

library(tidyverse)
library(sf)


# =============================================================================
# 1. Design settings - change colours, fonts and sizes here
# =============================================================================

# Font: Segoe UI is on every Windows PC (see design_decisions.md, round 4)
font <- "Segoe UI"

# Text sizes in points, as they will look printed on A3
size_name    <- 10    # region name next to each house
size_percent <- 16    # big percentage next to each house
size_legend  <- 10    # legend text
size_small   <- 9     # smallest text allowed

# Colours (round 3): one red hue in 4 steps, darker = more of the salary goes to rent
bin_breaks <- c(-Inf, 30, 40, 50, Inf)
bin_labels <- c("under 30%", "30-40%", "40-50%", "over 50%")
bin_colours <- c("#FBE3DA", "#F2B09B", "#E06A4E", "#A8200F")

ink         <- "#1E1E1E"   # main text
grey_text   <- "#555555"   # secondary text
grey_line   <- "#9A9A9A"   # leader lines
house_line  <- "#555555"   # house outline
house_roof  <- "#8C8C8C"   # roof (decoration, so it stays neutral)
accent      <- "#A8200F"   # same as the darkest bin: regions above 50%

# Our assumption: a typical rented home has 80 m2
home_size_m2 <- 80

# House size, in km of the map (the map is drawn in km, see section 3)
house_width  <- 40
house_height <- 26
roof_height  <- 11


# =============================================================================
# 2. Data: rent as % of the average monthly salary in 2024
# =============================================================================

all_years <- read_csv("data/datos_unidos.csv", col_types = cols(nuts_code = "c"),
                      show_col_types = FALSE)

regions <- all_years |>
  filter(year == 2024) |>
  mutate(
    # rent of an 80 m2 home divided by the average gross monthly salary
    rent_share = rent_m2 * home_size_m2 / wage_monthly * 100,
    colour     = bin_colours[cut(rent_share, bin_breaks, labels = FALSE)],
    above_half = rent_share > 50
  ) |>
  select(nuts_code, region, rent_share, colour, above_half)

# Shorter names so they fit next to the houses
short_names <- c(
  "Área Metropolitana do Porto" = "AM Porto",
  "Alto Tâmega e Barroso"       = "Alto Tâmega",
  "Terras de Trás-os-Montes"    = "Trás-os-Montes",
  "Beiras e Serra da Estrela"   = "Beiras e S. Estrela",
  "Região de Aveiro"            = "Aveiro",
  "Região de Coimbra"           = "Coimbra",
  "Região de Leiria"            = "Leiria",
  "Região Autónoma dos Açores"  = "Açores",
  "Região Autónoma da Madeira"  = "Madeira",
  "Península de Setúbal"        = "Setúbal")

regions <- regions |>
  mutate(name = if_else(region %in% names(short_names), short_names[region], region))

# Traits for the scatter panels (round 6). Each one is "per person" so that big
# and small regions can be compared.
traits <- all_years |>
  group_by(nuts_code) |>
  summarise(
    pop_growth  = (population[year == 2024] / population[year == 2021] - 1) * 100,
    foreign_pct = foreign_residents[year == 2023] / population[year == 2023] * 100,  # 2023 is the latest year
    nights_per_resident = tourist_nights[year == 2024] / population[year == 2024],
    young_pct   = share_20_34[year == 2024],   # already a percentage
    .groups = "drop") |>
  left_join(regions, by = "nuts_code")


# =============================================================================
# 3. Map shapes, moved to a simple km grid
# =============================================================================
# We work in km with the mainland's bottom-left corner at (0, 0).
# That way every position below (houses, insets, legend) is a plain number.

shapes <- st_read("data/nuts3_portugal_2024.geojson", quiet = TRUE) |>
  mutate(nuts_code = sub("^PT", "", NUTS_ID),
         nuts_code = recode(nuts_code, "200" = "20", "300" = "30")) |>
  left_join(regions, by = "nuts_code")

# Mainland: Portuguese official projection (in metres), then shift and convert to km
mainland <- shapes |> filter(!nuts_code %in% c("20", "30")) |> st_transform(3763)
corner   <- st_bbox(mainland)
st_geometry(mainland) <- (st_geometry(mainland) - c(corner$xmin, corner$ymin)) / 1000
mainland <- st_set_crs(mainland, NA)

mainland_width  <- (corner$xmax - corner$xmin) / 1000   # about 285 km
mainland_height <- (corner$ymax - corner$ymin) / 1000   # about 575 km

# Islands are far away, so we draw them as small framed insets in the ocean.
# This helper moves a shape so its centre lands on (x, y) and shrinks it to `width` km.
place_inset <- function(shape, x, y, width) {
  shape <- st_transform(shape, 3857)
  box <- st_bbox(shape)
  centre <- c((box$xmin + box$xmax) / 2, (box$ymin + box$ymax) / 2)
  scale  <- width / (box$xmax - box$xmin)
  st_geometry(shape) <- (st_geometry(shape) - centre) * scale + c(x, y)
  st_set_crs(shape, NA)
}

azores  <- shapes |> filter(nuts_code == "20")
madeira <- shapes |> filter(nuts_code == "30") |>
  st_crop(xmin = -17.5, xmax = -16.0, ymin = 32.5, ymax = 33.2)   # leave out the tiny Selvagens islands


# =============================================================================
# 4. Where each house goes
# =============================================================================
# Two columns of houses, left and right of the map. Each list goes from the top
# house to the bottom one. The order is roughly north to south, with a couple of
# swaps (Douro / Tâmega e Sousa, Beiras / Viseu) so that no leader lines cross.

left_order  <- c("111", "112", "11A", "191", "192", "193",
                 "1D1", "1D3", "1A0", "1B0", "1C1", "150")
right_order <- c("11B", "11E", "119", "11D", "11C", "196",
                 "194", "195", "1D2", "1C3", "1C4", "1C2")

row_top    <- mainland_height - 15   # first house
row_bottom <- 20                     # last mainland house
x_left     <- -95                    # centre of the left houses
x_right    <- mainland_width + 95    # centre of the right houses

# one point inside each region, where the leader line starts
inside_points <- st_coordinates(st_point_on_surface(mainland))

houses <- mainland |>
  st_drop_geometry() |>
  mutate(dot_x = inside_points[, 1], dot_y = inside_points[, 2],
         side  = if_else(nuts_code %in% left_order, "left", "right"),
         row   = if_else(side == "left", match(nuts_code, left_order), match(nuts_code, right_order)),
         y     = row_top - (row - 1) * (row_top - row_bottom) / 11,   # 12 rows, evenly spaced
         x     = if_else(side == "left", x_left, x_right))

# The islands get one more row at the bottom: Azores on the left, Madeira on the right
island_row <- -75
islands <- regions |>
  filter(nuts_code %in% c("20", "30")) |>
  mutate(side = if_else(nuts_code == "20", "left", "right"),
         x = if_else(side == "left", x_left, x_right), y = island_row,
         dot_x = NA, dot_y = NA)

houses <- bind_rows(houses, islands)

# Insets: Azores in the ocean to the south-west, Madeira to the south-east
azores_inset  <- place_inset(azores,  x = 45,  y = island_row - 5, width = 150)
madeira_inset <- place_inset(madeira, x = 235, y = island_row - 5, width = 55)
inset_frames <- tibble(
  xmin = c(-35, 200), xmax = c(125, 270),
  ymin = island_row - 48, ymax = island_row + 40,
  label = c("Açores (not to scale)", "Madeira (not to scale)"))


# =============================================================================
# 5. Drawing the houses
# =============================================================================
# Each house is the whole average salary. The coloured part, from the floor up,
# is the rent of an 80 m2 home. The dashed line marks half of the salary.

houses <- houses |>
  mutate(
    left   = x - house_width / 2,
    right  = x + house_width / 2,
    floor  = y - house_height / 2,
    top    = y + house_height / 2,
    rent   = floor + house_height * rent_share / 100,
    half   = floor + house_height / 2,
    # the 50% line is white on the dark houses, otherwise it would disappear
    half_colour = if_else(above_half, "white", grey_text),
    # text sits outside the house: right-aligned on the left side, left-aligned on the right
    text_x = if_else(side == "left", left - 6, right + 6),
    text_hjust = if_else(side == "left", 1, 0),
    label = sprintf("%.1f%%", rent_share),
    label_colour = if_else(above_half, accent, ink)
  )

# The roof is a triangle: three corners per house
roofs <- houses |>
  select(nuts_code, left, right, x, top) |>
  rowwise() |>
  reframe(nuts_code,
          px = c(left - 4, right + 4, x),
          py = c(top, top, top + roof_height))

# Leader lines go from the region to the inner edge of its house
lines <- houses |>
  filter(!is.na(dot_x)) |>
  mutate(end_x = if_else(side == "left", right, left))


# =============================================================================
# 6. Legend: an example house that explains how to read the others
# =============================================================================

legend_x <- -150   # left edge of the example house
legend_y <- mainland_height + 75
legend_house <- tibble(left = legend_x, right = legend_x + 50,
                       floor = legend_y - 18, top = legend_y + 18) |>
  mutate(rent = floor + (top - floor) * 0.55, half = floor + (top - floor) / 2)

legend_notes <- tibble(
  x = legend_x + 60,
  y = c(legend_house$top - 3, legend_house$half, legend_house$floor + 6),
  label = c("the whole house = average monthly salary (2024)",
            "- - - half of the salary",
            paste0("coloured part = rent of an ", home_size_m2, " m² home")))

# Colour bins, as a row of small boxes
bins <- tibble(
  label = bin_labels, colour = bin_colours,
  left = legend_x + 340 + (0:3) * 55, right = left + 50,
  floor = legend_y - 4, top = legend_y + 8)


# =============================================================================
# 7. The map panel
# =============================================================================

pt <- function(size) size / .pt   # ggplot text sizes are in mm; this converts points

p_map <- ggplot() +

  # regions, coloured by their 2024 bin
  geom_sf(data = mainland, aes(fill = colour), colour = "white", linewidth = 0.3) +
  geom_sf(data = azores_inset, aes(fill = colour), colour = house_line, linewidth = 0.2) +
  geom_sf(data = madeira_inset, aes(fill = colour), colour = house_line, linewidth = 0.2) +
  geom_rect(data = inset_frames, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, colour = grey_line, linewidth = 0.3) +
  geom_text(data = inset_frames, aes(x = xmin, y = ymax + 7, label = label),
            hjust = 0, family = font, size = pt(size_small), colour = grey_text) +

  # thin grey outline of the country, so the lightest regions don't melt into the white background
  geom_sf(data = st_union(mainland), fill = NA, colour = grey_line, linewidth = 0.3) +

  # leader lines and the small dot inside each region
  geom_segment(data = lines, aes(x = dot_x, y = dot_y, xend = end_x, yend = y),
               colour = grey_line, linewidth = 0.3) +
  geom_point(data = lines, aes(x = dot_x, y = dot_y), colour = ink, size = 0.9) +

  # houses: white walls, rent from the floor up, the 50% line, outline and roof
  geom_rect(data = houses, aes(xmin = left, xmax = right, ymin = floor, ymax = top),
            fill = "white") +
  geom_rect(data = houses, aes(xmin = left, xmax = right, ymin = floor, ymax = rent, fill = colour)) +
  geom_segment(data = houses, aes(x = left, xend = right, y = half, yend = half, colour = half_colour),
               linetype = "22", linewidth = 0.35) +
  geom_rect(data = houses, aes(xmin = left, xmax = right, ymin = floor, ymax = top),
            fill = NA, colour = house_line, linewidth = 0.4) +
  geom_polygon(data = roofs, aes(x = px, y = py, group = nuts_code),
               fill = house_roof, colour = house_line, linewidth = 0.4) +

  # region name and percentage next to each house
  geom_text(data = houses, aes(x = text_x, y = y + 8, label = name, hjust = text_hjust),
            family = font, fontface = "bold", size = pt(size_name), colour = ink) +
  geom_text(data = houses, aes(x = text_x, y = y - 6, label = label, hjust = text_hjust,
                               colour = label_colour),
            family = font, fontface = "bold", size = pt(size_percent)) +

  # legend: example house + notes
  geom_rect(data = legend_house, aes(xmin = left, xmax = right, ymin = floor, ymax = top), fill = "white") +
  geom_rect(data = legend_house, aes(xmin = left, xmax = right, ymin = floor, ymax = rent), fill = accent) +
  geom_segment(data = legend_house, aes(x = left, xend = right, y = half, yend = half),
               colour = "white", linetype = "22", linewidth = 0.35) +
  geom_rect(data = legend_house, aes(xmin = left, xmax = right, ymin = floor, ymax = top),
            fill = NA, colour = house_line, linewidth = 0.4) +
  geom_polygon(aes(x = c(legend_x - 4, legend_x + 54, legend_x + 25),
                   y = c(legend_house$top, legend_house$top, legend_house$top + 14)),
               fill = house_roof, colour = house_line, linewidth = 0.4) +
  geom_text(aes(x = legend_x, y = legend_house$top + 28, label = "How to read a house"),
            hjust = 0, family = font, fontface = "bold", size = pt(size_legend + 1), colour = ink) +
  geom_text(data = legend_notes, aes(x = x, y = y, label = label),
            hjust = 0, family = font, size = pt(size_legend), colour = grey_text) +

  # legend: colour bins (same colours as the map and the houses)
  geom_text(aes(x = legend_x + 340, y = legend_y + 22, label = "Rent as % of the salary, 2024"),
            hjust = 0, family = font, fontface = "bold", size = pt(size_legend + 1), colour = ink) +
  geom_rect(data = bins, aes(xmin = left, xmax = right, ymin = floor, ymax = top, fill = colour)) +
  geom_text(data = bins, aes(x = (left + right) / 2, y = floor - 10, label = label),
            family = font, size = pt(size_small), colour = grey_text) +

  scale_fill_identity() +
  scale_colour_identity() +
  coord_sf(xlim = c(-230, mainland_width + 230), ylim = c(-125, mainland_height + 140),
           expand = FALSE, clip = "off") +
  theme_void(base_family = font) +
  theme(plot.background = element_rect(fill = "white", colour = NA))


# =============================================================================
# 8. The dot strips: what do these regions have in common?
# =============================================================================
# One horizontal line per trait. Each dot is a region, coloured exactly like the
# map, so the reader only has to ask "where do the dark red dots sit on this line?".
# We first tried scatter plots (versions/v4_iteration2a_scatters.png), but two
# numeric axes are hard for a general audience, so we went with the simpler form
# used by The Times in the course examples ("Perfectly poised").

library(ggrepel)    # keeps the region names from overlapping
library(patchwork)  # puts several ggplots together

size_strip_title <- 13
size_axis        <- 10
porto_grey       <- "#444444"
track_grey       <- "#E6E6E6"   # the line the dots sit on

# Turns a correlation with the rent share into words a general reader understands.
# (Spearman's rho is reported in the report, not on the figure.)
link_in_words <- function(x) {
  rho <- cor(x, traits$rent_share, method = "spearman")
  case_when(abs(rho) >= 0.6 ~ "strong link with less affordable rent",
            abs(rho) >= 0.4 ~ "moderate link with less affordable rent",
            TRUE            ~ "weak link overall")
}

# Draws one strip. `x_var` is the column to use on the horizontal line.
make_strip <- function(x_var, title, x_label) {
  data <- traits |> mutate(x = .data[[x_var]])
  named <- data |> filter(above_half)            # the 4 regions above 50%
  porto <- data |> filter(nuts_code == "11A")    # reference point

  ggplot(data, aes(x = x, y = 0)) +
    geom_hline(yintercept = 0, colour = track_grey, linewidth = 3) +
    # the other regions: light outline so the palest dots stay visible on white
    geom_point(data = filter(data, !above_half), aes(fill = colour),
               shape = 21, colour = "#8C8C8C", size = 3.6, stroke = 0.3) +
    # the 4 regions above 50%: bigger, white outline, on top
    geom_point(data = named, aes(fill = colour), shape = 21, colour = "white", size = 4.8, stroke = 0.7) +
    geom_text_repel(data = named, aes(label = name), nudge_y = 0.55, direction = "x",
                    family = font, fontface = "bold", size = pt(size_small), colour = accent,
                    segment.colour = grey_line, min.segment.length = 0, seed = 1) +
    geom_text_repel(data = porto, aes(label = name), nudge_y = -0.55, direction = "x",
                    family = font, fontface = "bold", size = pt(size_small), colour = porto_grey,
                    segment.colour = grey_line, min.segment.length = 0, seed = 1) +
    scale_fill_identity() +
    scale_y_continuous(limits = c(-0.9, 1)) +
    labs(title = title, subtitle = paste0(x_label, "  ·  ", link_in_words(data$x))) +
    theme_minimal(base_family = font) +
    theme(
      plot.title       = element_text(face = "bold", size = size_strip_title, colour = ink),
      plot.subtitle    = element_text(size = size_axis, colour = grey_text),
      plot.title.position = "plot",
      axis.title       = element_blank(),
      axis.text.y      = element_blank(),
      axis.text.x      = element_text(size = size_small, colour = grey_text),
      panel.grid       = element_blank(),
      plot.margin      = margin(6, 12, 6, 8))
}

p_growth <- make_strip("pop_growth",
  title   = "Where the population grew the most",
  x_label = "Population growth, 2021-2024 (%)") +
  # Madeira is the exception: little growth, but heavy tourism (see the tourism strip)
  annotate("text", x = 1.3, y = -0.62, label = "Madeira: little growth,\nbut heavy tourism",
           hjust = 0, family = font, size = pt(size_small), colour = accent, lineheight = 0.9)

p_foreign <- make_strip("foreign_pct",
  title   = "Where more foreign residents live",
  x_label = "Foreign residents, 2023 (% of the population)")

p_tourism <- make_strip("nights_per_resident",
  title   = "Where tourists stay the most nights",
  x_label = "Tourist nights per resident, 2024")

p_young <- make_strip("young_pct",
  title   = "Where more young adults live",
  x_label = "People aged 20-34, 2024 (% of the population)")

# The four strips under their own block title. wrap_elements() turns the block into
# one piece, so its title stays with it when we put it next to the map.
p_traits <- wrap_elements(full =
  p_growth / p_foreign / p_tourism / p_young +
    plot_annotation(
      title    = "What do these regions have in common?",
      subtitle = "Each dot is a region, coloured like the map. Further right = more of that trait.",
      theme    = theme(
        plot.title    = element_text(family = font, face = "bold", size = 18, colour = ink),
        plot.subtitle = element_text(family = font, size = size_axis + 1, colour = grey_text))))


# =============================================================================
# 9. Put everything together on an A3 landscape page and save
# =============================================================================

n_above_half <- sum(regions$above_half)

title_text <- paste0("In ", n_above_half,
                     " regions, renting a home takes more than half of the average salary")
subtitle_text <- paste0(
  "Which Portuguese regions are least affordable to rent in on a local salary, and what do they have in common?\n",
  "Rent of a new ", home_size_m2, " m² home as a share of the average gross monthly earnings, ",
  "NUTS III regions, 2024.")
caption_text <- paste0(
  "Authors: Group T06 - Kevin Gomez, Lara Ribeiro, Maria Benedita Marques   |   ",
  "Created on ", format(Sys.Date(), "%d/%m/%Y"), "   |   ",
  "Sources: INE (rents of new leases, population, foreign residents, tourist nights); ",
  "MTSSS/GEP (earnings); Eurostat/GISCO (boundaries)")

# Map on the left (about 60% of the width), strips on the right (about 40%)
final_plot <- p_map + p_traits +
  plot_layout(widths = c(3, 2)) +
  plot_annotation(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    theme    = theme(
      plot.title      = element_text(family = font, face = "bold", size = 28, colour = ink),
      plot.subtitle   = element_text(family = font, size = 14, colour = grey_text,
                                     lineheight = 1.15, margin = margin(t = 4, b = 10)),
      plot.caption    = element_text(family = font, size = size_small, colour = grey_text,
                                     hjust = 0, margin = margin(t = 10)),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(18, 18, 12, 18)))

# A3 landscape, 300 dpi: ready to print
ggsave("T06_rent_affordability.png", final_plot, width = 42, height = 29.7, units = "cm",
       dpi = 300, bg = "white")
