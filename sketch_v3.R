# Sketch v3 (all text in English).
# Map and houses = share of salary spent on rent, time series (sale price; homes licensed and completed),
# a scatter of population growth vs the increase of the share, and small scatters for other traits.
# A second image, sketch_v3_detail.png, holds the detailed dot panel (appendix for the report).
# Run from the project folder: Rscript sketch_v3.R
suppressMessages({library(tidyverse); library(sf); library(giscoR)})

house_m2 <- 80          # reference home size (assumption)
sf_use_s2(FALSE)
W <- 120; H <- 240      # canvas size in "units" (x: 0-120, y: 0-240)

# ---------------------------------------------------------------- data
d  <- read_csv("data/datos_unidos.csv", na = "NA", show_col_types = FALSE)
ph <- read_csv("data/precios_historicos.csv", na = "NA", show_col_types = FALSE)
ch <- read_csv("data/construccion_historica.csv", na = "NA", show_col_types = FALSE)

short_names <- c(
  "PT" = "Portugal",
  "111" = "Alto Minho", "112" = "Cávado", "119" = "Ave", "11A" = "AM Porto",
  "11B" = "Alto Tâmega", "11C" = "Tâmega e Sousa", "11D" = "Douro", "11E" = "Trás-os-Montes",
  "150" = "Algarve", "191" = "Aveiro", "192" = "Coimbra", "193" = "Leiria",
  "194" = "Viseu Dão Lafões", "195" = "Beira Baixa", "196" = "Beiras e S. Estrela",
  "1A0" = "Grande Lisboa", "1B0" = "Setúbal", "1C1" = "Alentejo Litoral",
  "1C2" = "Baixo Alentejo", "1C3" = "Alto Alentejo", "1C4" = "Alentejo Central",
  "1D1" = "Oeste", "1D2" = "Médio Tejo", "1D3" = "Lezíria do Tejo",
  "20" = "Açores", "30" = "Madeira")

reg <- d |>
  group_by(nuts_code) |>
  summarise(
    e21        = rent_m2[year == 2021] * house_m2 / wage_monthly[year == 2021] * 100,
    e24        = rent_m2[year == 2024] * house_m2 / wage_monthly[year == 2024] * 100,
    price      = sale_price_m2[year == 2024],
    price21    = sale_price_m2[year == 2021],                     # start of the period
    tourism22  = tourist_nights[year == 2022] / population[year == 2022],   # first year with tourism data
    wage21     = wage_monthly[year == 2021],
    kids21     = share_0_14[year == 2021],
    young21    = share_20_34[year == 2021],
    old21      = share_65_79[year == 2021] + share_80plus[year == 2021],
    size21     = population[year == 2021] / 1000,
    lic_k      = mean(homes_licensed) / population[year == 2021] * 1000,   # permits per year, 2021-2024
    pop21      = population[year == 2021],
    pop22      = population[year == 2022],
    pop24      = population[year == 2024],
    pop_g      = (pop24 / pop21 - 1) * 100,
    mig_k      = sum(net_migration[year >= 2022]) / pop21 * 1000,
    young24    = share_20_34[year == 2024],
    young_chg  = share_20_34[year == 2024] - share_20_34[year == 2021],
    old24      = share_65_79[year == 2024] + share_80plus[year == 2024],
    dw_k       = first(dwellings_2022) / pop22 * 1000,
    rent_share = first(rented_households_2021) / first(households_2021) * 100,
    size       = pop24 / 1000,
    tourism    = tourist_nights[year == 2024] / pop24,
    rent_g     = (rent_m2[year == 2024] / rent_m2[year == 2021] - 1) * 100,
    wage_g     = (wage_monthly[year == 2024] / wage_monthly[year == 2021] - 1) * 100,
    .groups = "drop") |>
  mutate(name = short_names[nuts_code], delta = e24 - e21, critical = e24 > 50,
         group = case_when(critical ~ "critical", nuts_code == "11A" ~ "porto", TRUE ~ "other"))

# numbers written the English way: decimal point, comma between thousands
fmt <- function(x, digits = 0) formatC(round(x, digits), format = "f", digits = digits, big.mark = ",", decimal.mark = ".")

# ---------------------------------------------------------------- colours
paper <- "#FAF7F2"; ink <- "#1F1B16"; grey <- "#8A7F72"; red <- "#C1281A"; purple <- "#7A4FA3"
cream <- "#EFE7DA"; brown <- "#7C6A5A"; c_up <- "#6E1008"; c_down <- "#4C8C6B"
c_lic <- "#55627A"; c_comp <- "#D19A2B"; dot_grey <- "#B9AFA2"; line_grey <- "#CBC2B5"

# houses: the redder, the larger the share of the salary spent on rent (20% -> 60%)
red_ramp <- colorRamp(c("#F8E1D8", "#E9967F", "#D8412F"))
red_pal  <- function(e) { m <- red_ramp(pmin(pmax((e - 20) / 40, 0), 1)); rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255) }

# one fixed colour per highlighted region, used the same way in every chart below the map
reg_col <- c("PT" = ink, "1A0" = "#8E1B10", "1B0" = "#D8412F", "150" = "#E07B39", "30" = "#B8860B", "11A" = purple)

# ---------------------------------------------------------------- geometry
geo <- gisco_get_nuts(year = 2024, epsg = 4326, resolution = "03", country = "PT",
                      nuts_level = "3", cache_dir = file.path(Sys.getenv("TEMP"), "gisco")) |>
  mutate(nuts_code = recode(sub("^PT", "", NUTS_ID), "200" = "20", "300" = "30")) |>
  select(nuts_code)

# affine placement of a lon/lat geometry on the canvas (equirectangular)
place <- function(g, xc, y0, height = NULL, width = NULL) {
  bb <- st_bbox(g); latc <- mean(bb[c("ymin", "ymax")]); k <- cos(latc * pi / 180)
  dx <- (bb["xmax"] - bb["xmin"]) * k; dy <- bb["ymax"] - bb["ymin"]
  s <- if (!is.null(height)) height / dy else width / dx
  lonc <- mean(bb[c("xmin", "xmax")])
  out <- (st_geometry(g) - c(lonc, bb["ymin"])) * diag(c(k * s, s)) + c(xc, y0)
  list(geom = st_set_crs(out, NA), s = s, k = k, lonc = lonc, ymin = bb["ymin"], xc = xc, y0 = y0)
}

main <- geo |> filter(!nuts_code %in% c("20", "30"))
pm <- place(main, xc = 60, y0 = 152.4, height = 67.6)
main_c <- st_geometry(main)
cent <- st_coordinates(st_point_on_surface(main_c))
anchor <- st_coordinates(st_set_crs((st_point_on_surface(main_c) - c(pm$lonc, pm$ymin)) *
                                      diag(c(pm$k * pm$s, pm$s)) + c(pm$xc, pm$y0), NA))
main <- main |> mutate(lon = cent[, 1], lat = cent[, 2], ax = anchor[, 1], ay = anchor[, 2])
st_geometry(main) <- pm$geom

# ---------------------------------------------------------------- canvas helpers
reset_canvas <- function() {
  rects <<- list(); frames <<- list(); polys <<- list(); txts <<- list(); segs <<- list(); dots <<- list(); paths <<- list()
}
reset_canvas()
R <- function(xmin, xmax, ymin, ymax, fill) rects[[length(rects) + 1]] <<- tibble(xmin, xmax, ymin, ymax, fill)
FR <- function(xmin, xmax, ymin, ymax) frames[[length(frames) + 1]] <<- tibble(xmin, xmax, ymin, ymax)
PG <- function(x, y, fill) polys[[length(polys) + 1]] <<- tibble(x, y, fill, id = length(polys) + 1)
TX <- function(x, y, label, size = 3, colour = ink, face = "plain", hjust = 0, family = "sans", angle = 0)
  txts[[length(txts) + 1]] <<- tibble(x, y, label = as.character(label), size, colour, face, hjust, family, angle)
SG <- function(x, y, xend, yend, colour = "#A99D8F", lw = 0.25) segs[[length(segs) + 1]] <<- tibble(x, y, xend, yend, colour, lw)
DT <- function(x, y, fill, size, alpha = 1) dots[[length(dots) + 1]] <<- tibble(x, y, fill, size, alpha)
LN <- function(x, y, colour, lw = 0.4, lty = "solid") paths[[length(paths) + 1]] <<- tibble(x, y, colour, lw, lty, id = length(paths) + 1)

house <- function(xl, yc, e21, e24, w = 6.5, h = 4.0) {
  yb <- yc - 2.2; yt <- yb + h
  R(xl, xl + w, yb, yt, cream)
  lo <- min(e21, e24); hi <- max(e21, e24)
  R(xl, xl + w, yb, yb + h * lo / 100, red_pal(e24))                       # redder = larger share
  if (hi - lo >= 0.5) R(xl, xl + w, yb + h * lo / 100, yb + h * hi / 100, if (e24 >= e21) c_up else c_down)
  FR(xl, xl + w, yb, yt)
  PG(c(xl + w * .66, xl + w * .66 + .9, xl + w * .66 + .9, xl + w * .66), c(yt + .6, yt + .6, yt + 1.4, yt + 1.4), "#2E2A26")
  PG(c(xl - .5, xl + w + .5, xl + w / 2), c(yt, yt, yt + 1.6), brown)
}

card <- function(r, yc, side, xh, xt) {
  # side "L": text right-aligned before the house; "R": text after the house
  house(xh, yc, r$e21, r$e24)
  hj <- if (side == "L") 1 else 0
  dd <- round(r$e24 - r$e21)
  dl <- if (dd == 0) "no change" else sprintf("%s%d pp since 2021", if (dd > 0) "+" else "−", abs(dd))
  dc <- if (dd > 0) c_up else if (dd < 0) c_down else grey
  TX(xt, yc + 1.9, r$name, 3.1, if (r$nuts_code == "11A") purple else ink, "bold", hj)
  TX(xt, yc - 0.1, sprintf("%d%%", round(r$e24)), 5.2, if (r$critical) red else ink, "bold", hj)
  TX(xt, yc - 1.95, dl, 2.4, dc, "plain", hj)
}

# ---------------------------------------------------------------- mainland cards
main_df <- main |> st_drop_geometry() |> left_join(reg, by = "nuts_code")
main_df <- main_df |> mutate(side = if_else(rank(lon) <= 12, "L", "R")) |>
  group_by(side) |> arrange(desc(lat), .by_group = TRUE) |> mutate(row = row_number() - 1) |> ungroup() |>
  mutate(yc = 217 - 5.7 * row)

for (i in seq_len(nrow(main_df))) {
  r <- main_df[i, ]
  if (r$side == "L") {
    card(r, r$yc, "L", xh = 31, xt = 29.8)
    SG(37.5, r$yc - .2, r$ax, r$ay)
  } else {
    card(r, r$yc, "R", xh = 82.5, xt = 90.2)
    SG(82.5, r$yc - .2, r$ax, r$ay)
  }
  DT(r$ax, r$ay, ink, 2.6)
}

# ---------------------------------------------------------------- islands
isl <- geo |> filter(nuts_code %in% c("20", "30"))
isl_y <- 144.6
ma_main <- isl |> filter(nuts_code == "30") |>
  st_crop(xmin = -17.5, xmax = -16.0, ymin = 32.5, ymax = 33.2)   # drop the far-away Selvagens islands
az <- place(isl |> filter(nuts_code == "20"), xc = 59, y0 = isl_y - 1.8, width = 14)
ma <- place(ma_main, xc = 106, y0 = isl_y - 1, width = 6.5)
isl_geoms <- bind_rows(
  st_sf(nuts_code = "20", geometry = az$geom),
  st_sf(nuts_code = "30", geometry = ma$geom))

card(reg |> filter(nuts_code == "20"), isl_y, "R", xh = 22, xt = 29.8)
card(reg |> filter(nuts_code == "30"), isl_y, "R", xh = 72, xt = 79.8)
FRAME <- tibble(xmin = 18, xmax = 112, ymin = 139.5, ymax = 151)
TX(65, 149.5, "ISLANDS", 2.3, grey, "plain", 0.5)

# ---------------------------------------------------------------- header / legends
TX(4, 238.2, "In 4 regions, rent already takes more than half of the salary", 8.6, ink, "bold", 0, "serif")
TX(4, 234.9, "Each house is a region's average monthly salary; the coloured part is the share spent on the rent of a new 80 m² home in 2024. The map uses the same colour.", 3.2, "#4A433B")
TX(4, 232.8, "Rent weighs more where more people arrived and homes were already expensive. Madeira is the exception: little growth, but heavy tourism (see charts below).", 3.2, "#4A433B")

TX(4, 230.4, "Share of salary spent on rent, 2024 (map and houses, new 80 m² home)", 2.7, grey)
for (i in 0:29) R(4 + i * 0.8, 4 + (i + 1) * 0.8, 228.0, 229.3, red_pal(20 + i * 40 / 29))
TX(4, 226.7, "20%", 2.3, grey); TX(28, 226.7, "60%", 2.3, grey, "plain", 1)
TX(30, 228.65, "the redder, the bigger the share of the salary", 2.7, ink)
lg <- list(c(4, c_up, "increase since 2021 (dark band)"), c(42, c_down, "decrease since 2021"), c(72, cream, "rest of the salary"))
for (l in lg) {
  x <- as.numeric(l[1])
  R(x, x + 3.2, 224.0, 225.6, l[2]); FR(x, x + 3.2, 224.0, 225.6)
  TX(x + 4.2, 224.8, l[3], 2.9, ink)
}

# ---------------------------------------------------------------- time series 1: sale price 2019-2025
px0 <- 10; px1 <- 44; py0 <- 102; py1 <- 127
p_top <- ceiling(max(ph$sale_price_m2, na.rm = TRUE) / 1000) * 1000
xp <- function(y) px0 + (px1 - px0) * (y - 2019) / 6
yp <- function(v) py0 + (py1 - py0) * v / p_top
TX(4, 136.2, "Home sale prices, 2019–2025", 3.6, ink, "bold", 0, "serif")
TX(4, 134.0, "€ per m², median. Other regions in grey, Portugal dashed", 2.4, grey)
for (t in seq(0, p_top, 1000)) { SG(px0, yp(t), px1, yp(t), "#E8E0D4", 0.3); TX(px0 - 0.8, yp(t), fmt(t), 2.2, grey, "plain", 1) }
for (y in 2019:2025) TX(xp(y), py0 - 1.6, y, 2.2, grey, "plain", 0.5)
crit_codes <- reg$nuts_code[reg$critical]
for (cd in setdiff(unique(ph$nuts_code), c(crit_codes, "PT", "11A"))) { s <- ph |> filter(nuts_code == cd); LN(xp(s$year), yp(s$sale_price_m2), line_grey, 0.3) }
s <- ph |> filter(nuts_code == "PT"); LN(xp(s$year), yp(s$sale_price_m2), ink, 0.5, "dashed")
for (cd in c("11A", crit_codes)) { s <- ph |> filter(nuts_code == cd); LN(xp(s$year), yp(s$sale_price_m2), reg_col[[cd]], 0.8) }
# end labels pushed apart so they do not overlap (kept inside lo..hi)
spread <- function(y, gap, lo = -Inf, hi = Inf) {
  o <- order(y); ys <- y[o]; n <- length(ys)
  for (i in seq_len(n)[-1]) ys[i] <- max(ys[i], ys[i - 1] + gap)
  if (ys[n] > hi) { ys[n] <- hi; for (i in rev(seq_len(n - 1))) ys[i] <- min(ys[i], ys[i + 1] - gap) }
  y[o] <- pmax(ys, lo); y
}
ends <- ph |> filter(year == 2025, nuts_code %in% c(crit_codes, "PT", "11A")) |> mutate(y = spread(yp(sale_price_m2), 1.9))
for (i in seq_len(nrow(ends))) TX(px1 + 0.8, ends$y[i], paste0(short_names[ends$nuts_code[i]], " ", fmt(ends$sale_price_m2[i])), 2.4, reg_col[[ends$nuts_code[i]]], "bold")

# ---------------------------------------------------------------- time series 2: construction 2002-2025, 2 x 3 grid
pan_codes <- c("PT", "1A0", "1B0", "150", "30", "11A")
base_pop <- c(PT = sum(reg$pop21), setNames(reg$pop21, reg$nuts_code))   # population 2021 as fixed base
cons <- ch |> filter(nuts_code %in% pan_codes) |>
  mutate(lic_k = homes_licensed / base_pop[nuts_code] * 1000, comp_k = homes_completed / base_pop[nuts_code] * 1000)
cx0 <- 66; cx1 <- 103
TX(62, 136.2, "New home construction, 2002–2025", 3.6, ink, "bold", 0, "serif")
TX(62, 134.0, "per 1,000 residents (2021 population as a fixed base). Portugal dashed", 2.4, grey)
tick_years <- c(seq(2002, 2022, 4), 2025)
cons_charts <- list(
  list(var = "lic_k",  title = "Homes licensed (building permits)", y0 = 118.5, y1 = 129.5),
  list(var = "comp_k", title = "Homes completed (INE series starts in 2011)", y0 = 101.5, y1 = 112.5))
for (ccg in cons_charts) {
  y0 <- ccg$y0; y1 <- ccg$y1
  top <- ceiling(max(cons[[ccg$var]], na.rm = TRUE) / 2) * 2
  xc <- function(y) cx0 + (cx1 - cx0) * (y - 2002) / 23
  yc <- function(v) y0 + (y1 - y0) * v / top
  TX(cx0 - 4, y1 + 2.1, ccg$title, 2.8, ink, "bold")
  for (t in c(0, top / 2, top)) { SG(cx0, yc(t), cx1, yc(t), "#E8E0D4", 0.3); TX(cx0 - 0.6, yc(t), fmt(t), 2.1, grey, "plain", 1) }
  for (ty in tick_years) { SG(xc(ty), y0, xc(ty), y1, "#F0EADF", 0.25); TX(xc(ty), y0 - 1.6, ty, 2.1, grey, "plain", 0.5) }
  for (cd in c("11A", crit_codes, "PT")) {
    s <- cons |> filter(nuts_code == cd, !is.na(.data[[ccg$var]]))
    LN(xc(s$year), yc(s[[ccg$var]]), reg_col[[cd]], if (cd == "PT") 0.5 else 0.7, if (cd == "PT") "dashed" else "solid")
  }
  e <- cons |> filter(year == 2025) |> mutate(v = .data[[ccg$var]], y = spread(yc(v), 1.7, y0 - 0.5, y1 + 1))
  for (i in seq_len(nrow(e))) TX(cx1 + 0.8, e$y[i], paste0(short_names[e$nuts_code[i]], " ", fmt(e$v[i], 1)), 2.3, reg_col[[e$nuts_code[i]]], "bold")
}

# ---------------------------------------------------------------- scatter plots (shared helpers)
# same vertical axis everywhere: change of the share of salary spent on rent, 2021-2024 (pp)
y_lo <- min(0, floor(min(reg$delta))); y_hi <- ceiling(max(reg$delta))
y_ticks <- pretty(c(y_lo, y_hi), n = 5); y_ticks <- y_ticks[y_ticks >= y_lo & y_ticks <= y_hi]

# label placement: try 8 spots around a dot, keep the first one that hits no other label, dot or box
OBS <- matrix(numeric(0), ncol = 4)
add_obs <- function(b) OBS <<- rbind(OBS, b)
hits <- function(b) nrow(OBS) > 0 && any(b[1] < OBS[, 2] & b[2] > OBS[, 1] & b[3] < OBS[, 4] & b[4] > OBS[, 3])
text_box <- function(x, y, hj, txt, size, bold) {
  w <- nchar(txt) * size * if (bold) 0.2 else 0.185; h <- size * 0.3
  c(x - hj * w, x - hj * w + w, y - h / 2, y + h / 2)
}
dot_r <- function(size) 0.17 * size + 0.1
place_label <- function(px, py, r, txt, size, bold, bounds) {
  h <- size * 0.3; d <- r * 0.7
  cand <- list(c(r + .4, 0, 0), c(-(r + .4), 0, 1), c(0, r + h / 2 + .3, .5), c(0, -(r + h / 2 + .3), .5),
               c(d + .2, d + h / 2, 0), c(d + .2, -(d + h / 2), 0), c(-(d + .2), d + h / 2, 1), c(-(d + .2), -(d + h / 2), 1),
               c(0, r + h * 1.5 + .5, .5), c(0, -(r + h * 1.5 + .5), .5))   # a bit further up or down if all else is taken
  for (cc in cand) {
    b <- text_box(px + cc[1], py + cc[2], cc[3], txt, size, bold)
    if (b[1] >= bounds[1] && b[2] <= bounds[2] && b[3] >= bounds[3] && b[4] <= bounds[4] && !hits(b)) {
      add_obs(b); return(c(px + cc[1], py + cc[2], cc[3]))
    }
  }
  NULL
}

dot_fill <- function(i) if (reg$group[i] == "other") dot_grey else reg_col[[reg$nuts_code[i]]]
y_fmt <- function(t) ifelse(t > 0, paste0("+", fmt(t)), fmt(t))
# every dot has the same size: only position matters (x = the trait, y = rise of the share)
scatter_panel <- function(var, x0, x1, y0, y1, xfmt, y_labels = TRUE, nx = 5, tick_size = 2.5, log = FALSE) {
  vals <- reg[[var]]
  sizes <- ifelse(reg$group == "other", 2.6, 3.4)
  tf <- if (log) log10 else identity
  xt <- if (log) c(50, 100, 300, 1000, 3000) else pretty(range(c(vals, if (var == "pop_g") 0)), n = nx)
  xl <- tf(min(xt)); xh <- tf(max(xt))
  sx <- function(v) x0 + (x1 - x0) * (tf(v) - xl) / (xh - xl)
  sy <- function(v) y0 + (y1 - y0) * (v - y_lo) / (y_hi - y_lo)
  for (t in y_ticks) { SG(x0, sy(t), x1, sy(t), "#E8E0D4", 0.3); if (y_labels) TX(x0 - 0.8, sy(t), y_fmt(t), tick_size, grey, "plain", 1) }
  for (t in xt) { SG(sx(t), y0, sx(t), y1, "#F0EADF", 0.25); TX(sx(t), y0 - 1.9, xfmt(t), tick_size, grey, "plain", 0.5) }
  SG(x0, sy(0), x1, sy(0), "#A99D8F", 0.5)
  for (i in order(reg$group != "other")) DT(sx(vals[i]), sy(reg$delta[i]), dot_fill(i), sizes[i], if (reg$group[i] == "other") 0.8 else 1)
  OBS <<- matrix(numeric(0), ncol = 4)
  for (i in seq_len(nrow(reg))) { r <- dot_r(sizes[i]); add_obs(c(sx(vals[i]) - r, sx(vals[i]) + r, sy(reg$delta[i]) - r, sy(reg$delta[i]) + r)) }
  list(sx = sx, sy = sy, sizes = sizes, vals = vals)
}
# highlighted regions first (bold, own colour), then the others in grey if asked; returns the names left without a label
label_panel <- function(sp, bounds, size_hi, size_lo = NULL, skip = character(0)) {
  ord <- c(which(reg$group == "critical"), which(reg$group == "porto"), if (!is.null(size_lo)) which(reg$group == "other"))
  missing <- character(0)
  for (i in ord) {
    if (reg$nuts_code[i] %in% skip) next
    hi <- reg$group[i] != "other"
    pos <- place_label(sp$sx(sp$vals[i]), sp$sy(reg$delta[i]), dot_r(sp$sizes[i]), reg$name[i],
                       if (hi) size_hi else size_lo, hi, bounds)
    if (is.null(pos)) { missing <- c(missing, reg$name[i]); next }
    TX(pos[1], pos[2], reg$name[i], if (hi) size_hi else size_lo, if (hi) reg_col[[reg$nuts_code[i]]] else grey, if (hi) "bold" else "plain", pos[3])
  }
  missing
}

# ---------------------------------------------------------------- big scatter: population growth
TX(4, 94.0, "Where more people arrived, rent takes a bigger share of the salary", 5, ink, "bold", 0, "serif")
TX(4, 91.2, "Each dot is a region. The further right, the more its population grew; the higher, the more of the salary now goes to rent compared with 2021.", 2.9, "#4A433B")
sx0 <- 18; sx1 <- 108; sy0 <- 52; sy1 <- 86
sp <- scatter_panel("pop_g", sx0, sx1, sy0, sy1, function(t) paste0(fmt(t), "%"), nx = 7, tick_size = 2.6)
cor_s <- cor(reg$pop_g, reg$delta, method = "spearman")
# axis titles: big, next to their axis
TX(8.5, (sy0 + sy1) / 2, "Rise 2021–2024 in % of salary spent on rent (points)", 3.1, ink, "bold", 0.5, angle = 90)
TX((sx0 + sx1) / 2, sy0 - 5.2, "Population growth, 2021–2024 (%)", 3.4, ink, "bold", 0.5)
TX(sx0 + 0.6, sy1 + 1.8, "↑ more of the salary goes to rent", 2.5, grey)
TX(sx1, sy0 - 5.2, "more people →", 2.5, grey, "plain", 1)
TX(sx0, sy0 - 5.2, paste0("correlation ", fmt(cor_s, 2)), 2.2, grey)
TX(sx1, sp$sy(0) - 1.2, "no change", 2.4, grey, "plain", 1)
add_obs(text_box(sx1, sp$sy(0) - 1.2, 1, "no change", 2.4, FALSE))
# Madeira: label plus a short note, placed by hand
i <- which(reg$nuts_code == "30"); mx <- sp$sx(reg$pop_g[i]) + dot_r(sp$sizes[i]) + 0.6; my <- sp$sy(reg$delta[i])
TX(mx, my + 0.8, reg$name[i], 2.7, reg_col[["30"]], "bold"); add_obs(text_box(mx, my + 0.8, 0, reg$name[i], 2.7, TRUE))
note <- paste0("Little growth, but ", fmt(reg$tourism[i]), " tourist nights per resident")
TX(mx, my - 1.2, note, 2.4, reg_col[["30"]], "italic"); add_obs(text_box(mx, my - 1.2, 0, note, 2.4, FALSE))
# Alentejo Litoral sits almost on top of Algarve: name it by hand just below
i <- which(reg$nuts_code == "1C1"); al <- paste0(reg$name[i], " (next to Algarve)")
TX(sp$sx(reg$pop_g[i]) + 0.4, sp$sy(reg$delta[i]) - 2.2, al, 2.2, grey, "plain", 1)
add_obs(text_box(sp$sx(reg$pop_g[i]) + 0.4, sp$sy(reg$delta[i]) - 2.2, 1, al, 2.2, FALSE))
unlabelled <- label_panel(sp, c(sx0, sx1, sy0, sy1 + 1), 2.7, 2.2, skip = c("30", "1C1"))

# ---------------------------------------------------------------- small scatters: other traits, same vertical axis
TX(4, 40.5, "Which other traits go with a bigger increase?", 4.4, ink, "bold", 0, "serif")
TX(4, 37.8, "Vertical axis: how much the share of salary spent on rent rose from 2021 to 2024 (same as above). Horizontal axis: one trait of the regions per chart,", 2.6, "#4A433B")
TX(4, 35.8, "either how the region was AT THE START of the period (one year) or what happened DURING the period (2021–2024). The grey tag above each chart says which.", 2.6, "#4A433B")
pct <- function(t) paste0(fmt(t), "%")
traits <- list(
  list(var = "price21",    when = "START",  xlab = "Home sale price, 2021 (€/m²)",          f = fmt, more = "pricier regions rose more",  less = "cheaper regions rose more"),
  list(var = "wage21",     when = "START",  xlab = "Average salary, 2021 (€/month)",        f = fmt, more = "higher salaries, bigger rise", less = "lower salaries, bigger rise"),
  list(var = "rent_share", when = "START",  xlab = "Households that rent, 2021 (%)",        f = pct, more = "more renters, bigger rise",  less = "fewer renters, bigger rise"),
  list(var = "dw_k",       when = "START",  xlab = "Homes per 1,000 residents, 2022",       f = fmt, more = "more homes, bigger rise",    less = "fewer homes, bigger rise"),
  list(var = "lic_k",      when = "DURING", xlab = "Homes licensed per year per 1,000",     f = fmt, more = "more permits, bigger rise",  less = "fewer permits, bigger rise"),
  list(var = "size21",     when = "START",  xlab = "Residents, 2021 (log scale)",           f = function(t) if (t >= 1000) paste0(fmt(t / 1000), "M") else paste0(fmt(t), "k"),
       more = "bigger regions rose more", less = "smaller regions rose more", log = TRUE),
  list(var = "young21",    when = "START",  xlab = "Young adults 20–34, 2021 (%)",          f = pct, more = "younger regions rose more",  less = "fewer young, bigger rise"),
  list(var = "old21",      when = "START",  xlab = "People aged 65+, 2021 (%)",             f = pct, more = "older regions rose more",    less = "older regions rose less"),
  list(var = "tourism22",  when = "START",  xlab = "Tourist nights per resident, 2022",     f = fmt, more = "more tourism, bigger rise",  less = "less tourism, bigger rise"))
tw <- 30; tg <- 8; tx0 <- 12.5; row_step <- 37; ncol_t <- 3
cors <- c(pop_g = cor_s)
for (k in seq_along(traits)) {
  tr <- traits[[k]]; rr <- (k - 1) %/% ncol_t; cc <- (k - 1) %% ncol_t
  x0 <- tx0 + cc * (tw + tg); x1 <- x0 + tw
  ty1 <- 30 - rr * row_step; ty0 <- ty1 - 19.5
  if (cc == 0) TX(4.8, (ty0 + ty1) / 2, "Rise 2021–2024 in % of salary on rent", 2.5, ink, "bold", 0.5, angle = 90)
  TX(x0, ty1 + 2.6, if (tr$when == "START") "AT THE START" else "DURING 2021–2024", 2.2, if (tr$when == "START") grey else "#55627A", "bold")
  spk <- scatter_panel(tr$var, x0, x1, ty0, ty1, tr$f, y_labels = (cc == 0), nx = 4, tick_size = 2.3, log = isTRUE(tr$log))
  label_panel(spk, c(x0 - 1, min(x1 + 3, W - 1), ty0, ty1 + 1), 2.1)
  r <- cor(reg[[tr$var]], reg$delta, method = "spearman"); cors[tr$var] <- r
  a <- abs(r)
  line1 <- if (a < 0.3) "Almost no link" else paste0(if (a >= 0.5) "Clear link: " else "Weak link: ", if (r > 0) tr$more else tr$less)
  TX((x0 + x1) / 2, ty0 - 4.9, tr$xlab, 2.8, ink, "bold", 0.5)
  TX(x0, ty0 - 7.6, line1, 2.5, if (a >= 0.5) ink else if (a >= 0.3) "#4A433B" else grey, if (a >= 0.5) "bold" else "plain")
  TX(x0, ty0 - 9.6, paste0("(correlation ", fmt(r, 2), ")"), 2.2, grey)
}

fy <- 30 - (ceiling(length(traits) / ncol_t) - 1) * row_step - 19.5 - 14   # just below the last row
Y0 <- fy - 7    # the canvas goes below 0 to make room for the small scatters and the footer
TX(4, fy, paste0("Authors: Group [XX] — [names]  ·  Created on ", format(Sys.Date(), "%d/%m/%Y"), "  ·  Source: INE"), 2.5, grey)
TX(4, fy - 2.4, "SKETCH v3  ·  Rent effort = monthly rent of a new 80 m² home (assumption) ÷ average monthly salary. Per-resident rates use the INE yearly population estimates; renting households: Census 2021; homes: 2022 stock;", 2.2, grey)
TX(4, fy - 4.4, "construction per 1,000 residents uses the 2021 population as a fixed base; licensed = building permits, completed = homes finished; correlation = Spearman, 26 regions (1 = always together); pp = percentage points.", 2.2, grey)

# ---------------------------------------------------------------- assemble and save
make_plot <- function(Hc, map_sf = NULL, frame = NULL, ymin = 0) {
  p <- ggplot()
  if (!is.null(map_sf)) p <- p + geom_sf(data = map_sf, aes(fill = fill), colour = paper, linewidth = 0.3)
  if (!is.null(frame)) p <- p + geom_rect(data = frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                          fill = NA, colour = grey, linetype = "dashed", linewidth = 0.3)
  if (length(segs))   p <- p + geom_segment(data = bind_rows(segs), aes(x = x, y = y, xend = xend, yend = yend, colour = colour, linewidth = lw))
  if (length(paths))  p <- p + geom_path(data = bind_rows(paths), aes(x = x, y = y, group = id, colour = colour, linewidth = lw, linetype = lty), lineend = "round")
  if (length(rects))  p <- p + geom_rect(data = bind_rows(rects), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill))
  if (length(frames)) p <- p + geom_rect(data = bind_rows(frames), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                         fill = NA, colour = "#3B3128", linewidth = 0.3)
  if (length(polys))  p <- p + geom_polygon(data = bind_rows(polys), aes(x = x, y = y, group = id, fill = fill), colour = "#3B3128", linewidth = 0.3)
  if (length(dots))   p <- p + geom_point(data = bind_rows(dots), aes(x = x, y = y, fill = fill, size = size, alpha = alpha), shape = 21, colour = paper, stroke = 0.5)
  p <- p + geom_text(data = bind_rows(txts), aes(x = x, y = y, label = label, size = size, colour = colour, fontface = face, hjust = hjust, family = family, angle = angle),
                     lineheight = 0.9, vjust = 0.5) +
    scale_fill_identity() + scale_colour_identity() + scale_size_identity() +
    scale_linewidth_identity() + scale_alpha_identity() + scale_linetype_identity()
  p <- p + (if (!is.null(map_sf)) coord_sf(xlim = c(0, W), ylim = c(ymin, Hc), expand = FALSE, datum = NA)
            else coord_fixed(xlim = c(0, W), ylim = c(ymin, Hc), expand = FALSE))
  p + theme_void() + theme(plot.background = element_rect(fill = paper, colour = NA), plot.margin = margin(0, 0, 0, 0))
}

# map colour = the same share of the salary as the houses
main_sf <- main |> mutate(fill = red_pal(reg$e24[match(nuts_code, reg$nuts_code)])) |> select(fill)
isl_sf  <- isl_geoms |> mutate(fill = red_pal(reg$e24[match(nuts_code, reg$nuts_code)])) |> select(fill)
map_sf  <- bind_rows(main_sf, isl_sf)

ggsave("sketch_v3.png", make_plot(H, map_sf, FRAME, Y0), width = 14.4, height = 14.4 * (H - Y0) / W, dpi = 130, bg = paper)
cat("saved sketch_v3.png\n")
cat("Spearman correlation with the increase of the share:\n"); print(round(cors, 2))
cat("Regions without a label in the big scatter:", if (length(unlabelled)) paste(unlabelled, collapse = ", ") else "none", "\n")


# ---------------------------------------------------------------- detail image (appendix of the report)
reset_canvas()
Hd <- 100; ytop <- Hd - 3.5
TX(4, ytop, "What do the 4 critical regions have in common? The detail", 5, ink, "bold", 0, "serif")
DT(52, ytop - 2.9, dot_grey, 2.2, .6); TX(53.3, ytop - 2.9, "21 other regions", 2.6, ink)
DT(72, ytop - 2.9, red, 3, 1);        TX(73.3, ytop - 2.9, "the 4 critical regions", 2.6, ink)
DT(98, ytop - 2.9, purple, 3, 1);     TX(99.3, ytop - 2.9, "AM Porto", 2.6, ink)
SG(112, ytop - 3.8, 112, ytop - 2.0, ink, 0.5); TX(113, ytop - 2.9, "median", 2.2, grey)

panel <- tribble(
  ~group,      ~var,         ~title,                        ~unit_lab,                        ~suffix, ~digits, ~log,
  "DEMAND",    "pop_g",      "Population growth",           "2021–2024, %",                   "%",     1, FALSE,
  "DEMAND",    "mig_k",      "Net migration",               "2022–2024, per 1,000 residents", "",      0, FALSE,
  "DEMAND",    "young_chg",  "Increase of young adults",    "ages 20–34, 2021–2024, pp",      " pp",   1, FALSE,
  "AGE",       "kids21",     "Children 0–14",               "% of population, 2021",          "%",     1, FALSE,
  "AGE",       "young24",    "Young adults 20–34",          "% of population, 2024",          "%",     1, FALSE,
  "AGE",       "old24",      "People aged 65+",             "% of population, 2024",          "%",     1, FALSE,
  "SUPPLY",    "dw_k",       "Homes",                       "per 1,000 residents, 2022",      "",      0, FALSE,
  "EXPOSURE",  "rent_share", "Households that rent",       "% of households, Census 2021",   "%",     1, FALSE,
  "EXPOSURE",  "size",       "Size of the region",          "thousand residents, 2024 (log)", "k",     0, TRUE,
  "TOURISM",   "tourism",    "Tourist nights",              "per resident, 2024",             "",      1, FALSE,
  "MECHANISM", "rent_g",     "Increase in rent",            "2021–2024, %",                   "%",     0, FALSE,
  "MECHANISM", "wage_g",     "Increase in average salary",  "2021–2024, %",                   "%",     0, FALSE)

X0 <- 34; X1 <- 92
y <- ytop - 8.5; prev <- ""
for (k in seq_len(nrow(panel))) {
  p <- panel[k, ]
  if (p$group != prev) { if (prev != "") y <- y - 4.4; TX(4, y, p$group, 2.3, grey, "bold"); y <- y - 3.2; prev <- p$group } else y <- y - 5.6
  vals <- reg[[p$var]]
  if (p$log) {
    lo <- 60; hi <- 3000; ticks <- c(100, 300, 1000, 3000)
    xs <- function(v) X0 + (X1 - X0) * (log10(v) - log10(lo)) / (log10(hi) - log10(lo))
    tick_lab <- c("100k", "300k", "1M", "3M")
  } else {
    ticks <- pretty(c(min(0, min(vals)), max(vals)), n = 4); lo <- min(ticks); hi <- max(ticks)
    xs <- function(v) X0 + (X1 - X0) * (v - lo) / (hi - lo)
    tick_lab <- fmt(ticks, if (all(ticks == round(ticks))) 0 else 1)
  }
  TX(31, y + .5, p$title, 3, ink, "bold", 1); TX(31, y - 1.5, p$unit_lab, 2.3, grey, "plain", 1)
  SG(X0, y, X1, y, "#D9D0C2", 0.4)
  for (t in seq_along(ticks)) { SG(xs(ticks[t]), y - .5, xs(ticks[t]), y + .5, "#D9D0C2", 0.4); TX(xs(ticks[t]), y - 1.9, tick_lab[t], 2.1, grey, "plain", 0.5) }
  o <- reg |> filter(group == "other")
  for (v in o[[p$var]]) DT(xs(v), y, dot_grey, 2.2, .6)
  med <- median(o[[p$var]]); SG(xs(med), y - 1, xs(med), y + 1, ink, 0.6)
  cr <- reg |> filter(group == "critical") |> arrange(.data[[p$var]]); cx <- xs(cr[[p$var]])
  for (j in seq_along(cx)) DT(cx[j], y, red, 3.2, 1)
  pv <- reg[[p$var]][reg$group == "porto"]; DT(xs(pv), y, purple, 3.2, 1)
  cl <- cumsum(c(1, diff(cx) > 3))
  for (i in unique(cl)) TX(mean(cx[cl == i]), y + c(1.5, 2.7)[(i - 1) %% 2 + 1], paste(cr$name[cl == i], collapse = ", "), 2.5, red, "bold", 0.5)
  rng <- range(cr[[p$var]])
  TX(95.5, y + 1.6, paste0("The 4: ", fmt(rng[1], p$digits), " to ", fmt(rng[2], p$digits), p$suffix), 2.5, red, "italic")
  TX(95.5, y - 0.4, paste0("The other 21: ", fmt(med, p$digits), p$suffix), 2.5, "#4A433B", "italic")
  TX(95.5, y - 2.4, paste0("AM Porto: ", fmt(pv, p$digits), p$suffix), 2.5, purple, "italic")
}
TX(4, 3.2, paste0("Authors: Group [XX] — [names]  ·  Created on ", format(Sys.Date(), "%d/%m/%Y"), "  ·  Source: INE  ·  Each row has its own scale; pp = percentage points."), 2.3, grey)
ggsave("sketch_v3_detail.png", make_plot(Hd), width = 14.4, height = 14.4 * Hd / W, dpi = 130, bg = paper)
cat("saved sketch_v3_detail.png\n")
