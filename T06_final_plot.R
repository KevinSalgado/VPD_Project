# T06 — Rental affordability, Portugal, 2024
#
# Ficheiros necessários:
#   data/merged_data.csv
#   data/nuts3_portugal_2024.geojson
#
# A imagem é guardada na pasta atual.


# ---------------------------------------------------------------------
# 1. PACOTES, LETRA, TAMANHOS E CORES
# ---------------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggrepel)
library(patchwork)
library(showtext)

sysfonts::font_add_google(
  name = "Noto Sans",
  family = "noto",
  regular.wt = 400,
  bold.wt = 700
)

showtext_auto()

tipo_letra <- "noto"

tam_nome         <- 10
tam_perc         <- 16
tam_legenda      <- 10
tam_pequeno      <- 9

# Hierarquia do bloco da direita
tam_titulo_bloco <- 21
tam_tit_tira     <- 14
tam_numeros_tira <- 11

# Espaçamento do bloco da direita
# A margem superior baixa o título até à altura da legenda do mapa.
margem_topo_bloco <- 34
espaco_entre_tiras <- 0.28

comprimento_marcas <- grid::unit(2, "mm")
margem_numeros_pt  <- 4

limites_tramos <- c(-Inf, 30, 40, 50, Inf)

nomes_tramos <- c(
  "up to 30%",
  "30–40%",
  "40–50%",
  "over 50%"
)

cores_tramos <- c(
  "#FBE3DA",
  "#F2B09B",
  "#E06A4E",
  "#A8200F"
)

cor_texto      <- "#1E1E1E"
cinza_texto    <- "#555555"
cor_tit_tira   <- "#1E1E1E"
cinza_linha    <- "#9A9A9A"
cor_contorno   <- "#555555"
cor_telhado    <- "#8C8C8C"
cor_destaque   <- "#A8200F"
cinza_pista    <- "#E6E6E6"

area_casa <- 80

largura_casa   <- 40
altura_casa    <- 26
altura_telhado <- 11

em_pt <- function(tamanho) {
  tamanho / ggplot2::.pt
}


# ---------------------------------------------------------------------
# 2. DADOS
# ---------------------------------------------------------------------

dados_todos <- read_csv(
  "data/merged_data.csv",
  col_types = cols(nuts_code = col_character()),
  show_col_types = FALSE
)

colunas_necessarias <- c(
  "nuts_code",
  "region",
  "year",
  "rent_m2",
  "wage_monthly",
  "population",
  "foreign_residents_aima_2024",
  "tourist_nights",
  "homes_completed"
)

colunas_em_falta <- setdiff(
  colunas_necessarias,
  names(dados_todos)
)

if (length(colunas_em_falta) > 0) {
  stop(
    "Faltam estas colunas em merged_data.csv: ",
    paste(colunas_em_falta, collapse = ", ")
  )
}

dados_2024 <- dados_todos |>
  filter(year == 2024)

if (nrow(dados_2024) == 0) {
  stop("O ficheiro merged_data.csv não tem dados para 2024.")
}

if (anyDuplicated(dados_2024$nuts_code) > 0) {
  stop(
    "Existem regiões repetidas em 2024. ",
    "Deve existir apenas uma linha por região nesse ano."
  )
}

if (
  any(!is.finite(dados_2024$population)) ||
  any(dados_2024$population <= 0)
) {
  stop(
    "Existem valores de população em falta, ",
    "inválidos ou inferiores a 1."
  )
}

if (
  any(!is.finite(dados_2024$wage_monthly)) ||
  any(dados_2024$wage_monthly <= 0)
) {
  stop(
    "Existem valores de salário em falta, ",
    "inválidos ou inferiores a 1."
  )
}

if (
  any(!is.finite(dados_2024$rent_m2)) ||
  any(dados_2024$rent_m2 < 0)
) {
  stop(
    "Existem valores de renda em falta, ",
    "inválidos ou negativos."
  )
}

regioes <- dados_2024 |>
  mutate(
    esforco = rent_m2 * area_casa / wage_monthly * 100,
    
    cor_tramo = cores_tramos[
      cut(esforco, limites_tramos, labels = FALSE)
    ],
    
    acima_metade = esforco > 50
  ) |>
  select(
    nuts_code,
    region,
    esforco,
    cor_tramo,
    acima_metade
  )

nomes_curtos <- c(
  "Área Metropolitana do Porto" = "AM Porto",
  "Alto Tâmega e Barroso" = "Alto Tâmega",
  "Terras de Trás-os-Montes" = "Trás-os-Montes",
  "Beiras e Serra da Estrela" = "Beiras e S. Estrela",
  "Região de Aveiro" = "Aveiro",
  "Região de Coimbra" = "Coimbra",
  "Região de Leiria" = "Leiria",
  "Região Autónoma dos Açores" = "Açores",
  "Região Autónoma da Madeira" = "Madeira",
  "Península de Setúbal" = "Setúbal"
)

regioes <- regioes |>
  mutate(
    nome = if_else(
      region %in% names(nomes_curtos),
      nomes_curtos[region],
      region
    )
  )

caracteristicas <- dados_2024 |>
  transmute(
    nuts_code,
    populacao = population,
    
    estrang_pct =
      foreign_residents_aima_2024 / population * 100,
    
    noites_por_residente =
      tourist_nights / population,
    
    novas_habitacoes = homes_completed
  ) |>
  left_join(regioes, by = "nuts_code")


# ---------------------------------------------------------------------
# 3. FORMAS DO MAPA
# ---------------------------------------------------------------------

formas <- st_read(
  "data/nuts3_portugal_2024.geojson",
  quiet = TRUE
) |>
  mutate(
    nuts_code = sub("^PT", "", NUTS_ID),
    
    nuts_code = recode(
      nuts_code,
      "200" = "20",
      "300" = "30"
    )
  ) |>
  left_join(regioes, by = "nuts_code")

continente <- formas |>
  filter(!nuts_code %in% c("20", "30")) |>
  st_transform(3763)

canto <- st_bbox(continente)

st_geometry(continente) <- (
  st_geometry(continente) -
    c(canto$xmin, canto$ymin)
) / 1000

continente <- st_set_crs(continente, NA)

largura_continente <- (
  canto$xmax - canto$xmin
) / 1000

altura_continente <- (
  canto$ymax - canto$ymin
) / 1000

mover_ilha <- function(ilha, x, y, largura) {
  
  ilha <- st_transform(ilha, 3857)
  caixa <- st_bbox(ilha)
  
  centro <- c(
    (caixa$xmin + caixa$xmax) / 2,
    (caixa$ymin + caixa$ymax) / 2
  )
  
  escala <- largura / (
    caixa$xmax - caixa$xmin
  )
  
  st_geometry(ilha) <- (
    st_geometry(ilha) - centro
  ) * escala + c(x, y)
  
  st_set_crs(ilha, NA)
}

acores <- formas |>
  filter(nuts_code == "20")

madeira <- formas |>
  filter(nuts_code == "30") |>
  st_crop(
    xmin = -17.5,
    xmax = -16.0,
    ymin = 32.5,
    ymax = 33.2
  )


# ---------------------------------------------------------------------
# 4. POSIÇÃO DAS CASAS
# ---------------------------------------------------------------------

ordem_esq <- c(
  "111", "112", "11A", "191", "192", "193",
  "1D1", "1D3", "1A0", "1B0", "1C1", "150"
)

ordem_dir <- c(
  "11B", "11E", "119", "11D", "11C", "196",
  "194", "195", "1D2", "1C3", "1C4", "1C2"
)

y_topo  <- altura_continente - 15
y_fundo <- 20

x_esq <- -95
x_dir <- largura_continente + 95

pontos_dentro <- st_coordinates(
  st_point_on_surface(continente)
)

casas <- continente |>
  st_drop_geometry() |>
  mutate(
    xponto = pontos_dentro[, 1],
    yponto = pontos_dentro[, 2],
    
    lado = if_else(
      nuts_code %in% ordem_esq,
      "esq",
      "dir"
    ),
    
    linha = if_else(
      lado == "esq",
      match(nuts_code, ordem_esq),
      match(nuts_code, ordem_dir)
    ),
    
    ycasa = y_topo -
      (linha - 1) * (y_topo - y_fundo) / 11,
    
    xcasa = if_else(
      lado == "esq",
      x_esq,
      x_dir
    )
  )

y_ilhas <- -75

casas_ilhas <- regioes |>
  filter(nuts_code %in% c("20", "30")) |>
  mutate(
    lado = if_else(nuts_code == "20", "esq", "dir"),
    xcasa = if_else(lado == "esq", x_esq, x_dir),
    ycasa = y_ilhas,
    xponto = NA_real_,
    yponto = NA_real_
  )

casas <- bind_rows(casas, casas_ilhas)

acores_mapa <- mover_ilha(
  acores,
  x = 45,
  y = y_ilhas - 5,
  largura = 150
)

madeira_mapa <- mover_ilha(
  madeira,
  x = 235,
  y = y_ilhas - 5,
  largura = 55
)

molduras <- tibble(
  xmin = c(-35, 200),
  xmax = c(125, 270),
  ymin = y_ilhas - 48,
  ymax = y_ilhas + 40,
  
  texto = c(
    "Açores (not to scale)",
    "Madeira (not to scale)"
  )
)


# ---------------------------------------------------------------------
# 5. DESENHO DAS CASAS
# ---------------------------------------------------------------------

casas <- casas |>
  mutate(
    x0 = xcasa - largura_casa / 2,
    x1 = xcasa + largura_casa / 2,
    
    y0 = ycasa - altura_casa / 2,
    y1 = ycasa + altura_casa / 2,
    
    nivel_renda =
      y0 + altura_casa * esforco / 100,
    
    nivel_meio =
      y0 + altura_casa / 2,
    
    cor_meio = if_else(
      acima_metade,
      "white",
      cinza_texto
    ),
    
    xtexto = if_else(
      lado == "esq",
      x0 - 6,
      x1 + 6
    ),
    
    hj = if_else(lado == "esq", 1, 0),
    
    texto_pct = sprintf("%.1f%%", esforco),
    
    cor_pct = if_else(
      acima_metade,
      cor_destaque,
      cor_texto
    )
  )

telhados <- bind_rows(
  casas |>
    transmute(nuts_code, px = x0 - 4, py = y1),
  
  casas |>
    transmute(nuts_code, px = x1 + 4, py = y1),
  
  casas |>
    transmute(
      nuts_code,
      px = xcasa,
      py = y1 + altura_telhado
    )
)

ligacoes <- casas |>
  filter(!is.na(xponto)) |>
  mutate(
    xfim = if_else(lado == "esq", x1, x0)
  )


# ---------------------------------------------------------------------
# 6. LEGENDA
# ---------------------------------------------------------------------

leg_x       <- -200
leg_x_cores <- 190
leg_y       <- altura_continente + 75

casa_leg <- tibble(
  x0 = leg_x,
  x1 = leg_x + 50,
  y0 = leg_y - 18,
  y1 = leg_y + 18
) |>
  mutate(
    nivel_renda = y0 + (y1 - y0) * 0.55,
    nivel_meio = y0 + (y1 - y0) / 2
  )

notas_leg <- tibble(
  x = leg_x + 60,
  
  y = c(
    casa_leg$y1 - 3,
    casa_leg$nivel_meio,
    casa_leg$y0 + 6
  ),
  
  texto = c(
    "the whole house = average monthly salary (2024)",
    "- - - half of the salary",
    
    paste0(
      "coloured part = rent of an ",
      area_casa,
      " m² home"
    )
  )
)

caixas_tramos <- tibble(
  texto = nomes_tramos,
  cor = cores_tramos,
  
  x0 = leg_x_cores + (0:3) * 55,
  x1 = x0 + 50,
  
  y0 = leg_y - 4,
  y1 = leg_y + 8
)


# ---------------------------------------------------------------------
# 7. MAPA
# ---------------------------------------------------------------------

p_mapa <- ggplot() +
  
  geom_sf(
    data = continente,
    aes(fill = cor_tramo),
    colour = "white",
    linewidth = 0.3
  ) +
  
  geom_sf(
    data = acores_mapa,
    aes(fill = cor_tramo),
    colour = cor_contorno,
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = madeira_mapa,
    aes(fill = cor_tramo),
    colour = cor_contorno,
    linewidth = 0.2
  ) +
  
  geom_rect(
    data = molduras,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    ),
    fill = NA,
    colour = cinza_linha,
    linewidth = 0.3
  ) +
  
  geom_text(
    data = molduras,
    aes(x = xmin, y = ymax + 7, label = texto),
    hjust = 0,
    family = tipo_letra,
    size = em_pt(tam_pequeno),
    colour = cinza_texto
  ) +
  
  geom_sf(
    data = st_union(continente),
    fill = NA,
    colour = cinza_linha,
    linewidth = 0.3
  ) +
  
  geom_segment(
    data = ligacoes,
    aes(
      x = xponto, y = yponto,
      xend = xfim, yend = ycasa
    ),
    colour = cinza_linha,
    linewidth = 0.3
  ) +
  
  geom_point(
    data = ligacoes,
    aes(x = xponto, y = yponto),
    colour = cor_texto,
    size = 0.9
  ) +
  
  # Corpo e preenchimento das casas
  geom_rect(
    data = casas,
    aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = y1
    ),
    fill = "white"
  ) +
  
  geom_rect(
    data = casas,
    aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = nivel_renda,
      fill = cor_tramo
    )
  ) +
  
  geom_segment(
    data = casas,
    aes(
      x = x0, xend = x1,
      y = nivel_meio, yend = nivel_meio,
      colour = cor_meio
    ),
    linetype = "22",
    linewidth = 0.35
  ) +
  
  geom_rect(
    data = casas,
    aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = y1
    ),
    fill = NA,
    colour = cor_contorno,
    linewidth = 0.4
  ) +
  
  geom_polygon(
    data = telhados,
    aes(x = px, y = py, group = nuts_code),
    fill = cor_telhado,
    colour = cor_contorno,
    linewidth = 0.4
  ) +
  
  # Nomes e valores
  geom_text(
    data = casas,
    aes(
      x = xtexto, y = ycasa + 8,
      label = nome, hjust = hj
    ),
    family = tipo_letra,
    fontface = "bold",
    size = em_pt(tam_nome),
    colour = cor_texto
  ) +
  
  geom_text(
    data = casas,
    aes(
      x = xtexto, y = ycasa - 6,
      label = texto_pct, hjust = hj,
      colour = cor_pct
    ),
    family = tipo_letra,
    fontface = "bold",
    size = em_pt(tam_perc)
  ) +
  
  # Casa de exemplo
  geom_rect(
    data = casa_leg,
    aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = y1
    ),
    fill = "white"
  ) +
  
  geom_rect(
    data = casa_leg,
    aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = nivel_renda
    ),
    fill = cor_destaque
  ) +
  
  geom_segment(
    data = casa_leg,
    aes(
      x = x0, xend = x1,
      y = nivel_meio, yend = nivel_meio
    ),
    colour = "white",
    linetype = "22",
    linewidth = 0.35
  ) +
  
  geom_rect(
    data = casa_leg,
    aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = y1
    ),
    fill = NA,
    colour = cor_contorno,
    linewidth = 0.4
  ) +
  
  geom_polygon(
    aes(
      x = c(leg_x - 4, leg_x + 54, leg_x + 25),
      
      y = c(
        casa_leg$y1,
        casa_leg$y1,
        casa_leg$y1 + 14
      )
    ),
    fill = cor_telhado,
    colour = cor_contorno,
    linewidth = 0.4
  ) +
  
  geom_text(
    aes(
      x = leg_x,
      y = casa_leg$y1 + 28,
      label = "How to read a house"
    ),
    hjust = 0,
    family = tipo_letra,
    fontface = "bold",
    size = em_pt(tam_legenda + 1),
    colour = cor_texto
  ) +
  
  geom_text(
    data = notas_leg,
    aes(x = x, y = y, label = texto),
    hjust = 0,
    family = tipo_letra,
    size = em_pt(tam_legenda),
    colour = cinza_texto
  ) +
  
  # Legenda das cores
  geom_text(
    aes(
      x = leg_x_cores,
      y = leg_y + 22,
      label = "Rent as % of the salary, 2024"
    ),
    hjust = 0,
    family = tipo_letra,
    fontface = "bold",
    size = em_pt(tam_legenda + 1),
    colour = cor_texto
  ) +
  
  geom_rect(
    data = caixas_tramos,
    aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = y1,
      fill = cor
    )
  ) +
  
  geom_text(
    data = caixas_tramos,
    aes(
      x = (x0 + x1) / 2,
      y = y0 - 10,
      label = texto
    ),
    family = tipo_letra,
    size = em_pt(tam_pequeno),
    colour = cinza_texto
  ) +
  
  scale_fill_identity() +
  scale_colour_identity() +
  
  coord_sf(
    xlim = c(-230, largura_continente + 230),
    ylim = c(-125, altura_continente + 140),
    expand = FALSE,
    clip = "off"
  ) +
  
  theme_void(base_family = tipo_letra) +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )


# ---------------------------------------------------------------------
# 8. DOT PLOTS
# ---------------------------------------------------------------------

fazer_tira <- function(
    valores,
    titulo,
    unidade,
    precisao = 1
) {
  
  dados_tira <- caracteristicas |>
    mutate(x = valores)
  
  if (any(!is.finite(dados_tira$x))) {
    stop(
      "Existem valores em falta ou inválidos no painel: ",
      titulo
    )
  }
  
  destaque <- dados_tira |>
    filter(acima_metade)
  
  unidade_eixo <- grid::textGrob(
    label = unidade,
    
    x = grid::unit(1, "npc") +
      grid::unit(14, "pt"),
    
    y = grid::unit(0, "npc") -
      comprimento_marcas -
      grid::unit(margem_numeros_pt, "pt"),
    
    just = c("left", "top"),
    
    gp = grid::gpar(
      fontfamily = tipo_letra,
      fontsize = tam_numeros_tira,
      fontface = "plain",
      col = cinza_texto
    )
  )
  
  ggplot(dados_tira, aes(x = x, y = 0)) +
    
    geom_hline(
      yintercept = 0,
      colour = cinza_pista,
      linewidth = 3
    ) +
    
    geom_point(
      data = filter(dados_tira, !acima_metade),
      aes(fill = cor_tramo),
      shape = 21,
      colour = "#8C8C8C",
      size = 3.6,
      stroke = 0.3
    ) +
    
    geom_point(
      data = destaque,
      aes(fill = cor_tramo),
      shape = 21,
      colour = "white",
      size = 4.8,
      stroke = 0.7
    ) +
    
    geom_text_repel(
      data = destaque,
      aes(label = nome),
      nudge_y = 0.17,
      direction = "x",
      family = tipo_letra,
      fontface = "bold",
      size = em_pt(tam_pequeno),
      colour = cor_destaque,
      segment.colour = cinza_linha,
      min.segment.length = 0,
      box.padding = 0.35,
      point.padding = 0.25,
      max.overlaps = Inf,
      seed = 1
    ) +
    
    scale_fill_identity() +
    
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 4),
      
      labels = scales::label_number(
        accuracy = precisao,
        big.mark = ",",
        decimal.mark = "."
      )
    ) +
    
    scale_y_continuous(breaks = NULL) +
    
    coord_cartesian(
      ylim = c(-0.08, 0.30),
      expand = FALSE,
      clip = "off"
    ) +
    
    annotation_custom(grob = unidade_eixo) +
    
    labs(
      title = titulo,
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal(base_family = tipo_letra) +
    
    theme(
      # Títulos individuais mais pequenos e sem negrito
      plot.title = element_text(
        family = tipo_letra,
        face = "plain",
        size = tam_tit_tira,
        colour = cor_tit_tira,
        hjust = 0,
        margin = margin(b = 4)
      ),
      
      plot.title.position = "plot",
      
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      
      axis.text.x = element_text(
        size = tam_numeros_tira,
        face = "plain",
        colour = cinza_texto,
        vjust = 1,
        margin = margin(t = margem_numeros_pt)
      ),
      
      axis.line.x = element_line(
        colour = cinza_linha,
        linewidth = 0.3
      ),
      
      axis.ticks.x = element_line(
        colour = cinza_linha,
        linewidth = 0.3
      ),
      
      axis.ticks.y = element_blank(),
      axis.ticks.length = comprimento_marcas,
      
      panel.grid = element_blank(),
      
      plot.margin = margin(
        t = 4,
        r = 100,
        b = 8,
        l = 8
      )
    )
}

p_populacao <- fazer_tira(
  valores = caracteristicas$populacao / 1000000,
  titulo = "Resident population, 2024",
  unidade = "million residents",
  precisao = 0.1
)

p_estrangeiros <- fazer_tira(
  valores = caracteristicas$estrang_pct,
  titulo = "Share of foreign residents, 2024",
  unidade = "%"
)

p_turismo <- fazer_tira(
  valores = caracteristicas$noites_por_residente,
  titulo = "Tourist nights per resident, 2024",
  unidade = "nights/resident"
)

p_construcao <- fazer_tira(
  valores = caracteristicas$novas_habitacoes,
  titulo = "New homes completed, 2024",
  unidade = "homes"
)

# Painéis mais próximos
bloco_tiras <- wrap_plots(
  p_populacao,
  plot_spacer(),
  p_estrangeiros,
  plot_spacer(),
  p_turismo,
  plot_spacer(),
  p_construcao,
  
  ncol = 1,
  
  heights = c(
    1, espaco_entre_tiras,
    1, espaco_entre_tiras,
    1, espaco_entre_tiras,
    1
  )
) +
  
  plot_annotation(
    title = "What do these regions have in common?",
    
    theme = theme(
      text = element_text(
        family = tipo_letra
      ),
      
      plot.title = element_text(
        family = tipo_letra,
        face = "bold",
        size = tam_titulo_bloco,
        colour = cor_texto,
        hjust = 0,
        margin = margin(b = 12)
      ),
      
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      # Baixa todo o bloco para alinhar o título
      # com a legenda das cores do mapa
      plot.margin = margin(
        t = margem_topo_bloco,
        r = 0,
        b = 0,
        l = 8
      )
    )
  )

p_tiras <- wrap_elements(
  full = bloco_tiras
)


# ---------------------------------------------------------------------
# 9. COMPOSIÇÃO FINAL E EXPORTAÇÃO
# ---------------------------------------------------------------------

n_metade <- sum(regioes$acima_metade)

titulo_figura <- paste0(
  "In ",
  n_metade,
  " regions, renting a home takes more than half of the average salary"
)

subtitulo_figura <- paste0(
  "Which Portuguese regions are least affordable to rent in on a local salary, ",
  "and what do they have in common?\n",
  "Rent of a new ",
  area_casa,
  " m² home as a share of the average gross monthly earnings, ",
  "NUTS III regions, 2024."
)

rodape_figura <- paste0(
  "Authors: Group T06 - Kevin Gomez, Lara Ribeiro, Maria Benedita Marques",
  "   |   Created on ",
  format(Sys.Date(), "%d/%m/%Y"),
  "   |   Sources: INE (rents of new leases, population, tourist nights, ",
  "homes completed); AIMA (foreign residents, 2024); ",
  "MTSSS/GEP (earnings); Eurostat/GISCO (boundaries)"
)

figura_final <- p_mapa + p_tiras +
  
  plot_layout(
    widths = c(3, 2)
  ) +
  
  plot_annotation(
    title = titulo_figura,
    subtitle = subtitulo_figura,
    caption = rodape_figura,
    
    theme = theme(
      plot.title = element_text(
        family = tipo_letra,
        face = "bold",
        size = 28,
        colour = cor_texto
      ),
      
      plot.subtitle = element_text(
        family = tipo_letra,
        size = 14,
        colour = cinza_texto,
        lineheight = 1.15,
        margin = margin(t = 4, b = 10)
      ),
      
      plot.caption = element_text(
        family = tipo_letra,
        size = tam_pequeno,
        colour = cinza_texto,
        hjust = 0,
        margin = margin(t = 10)
      ),
      
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      plot.margin = margin(
        18, 18, 12, 18
      )
    )
  )

# Para mostrar no RStudio, retirar o #:
# print(figura_final)

showtext_opts(dpi = 300)

ggsave(
  filename = "T06_rent_affordability.png",
  plot = figura_final,
  width = 42,
  height = 29.7,
  units = "cm",
  dpi = 300,
  bg = "white"
)

message(
  "Imagem guardada em: ",
  file.path(
    getwd(),
    "T06_rent_affordability.png"
  )
)