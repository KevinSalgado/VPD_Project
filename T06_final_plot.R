# T06 - Renda como percentagem do salário médio, Portugal 2024
# Figura final do projeto de VPDA (mapa com casas + tiras de pontos).
#
# ficheiros necessários (na pasta data/):
#  - merged_data.csv  (feito no notebook merge_ine_data.ipynb; inclui os estrangeiros de 2024 da AIMA)
#  - nuts3_portugal_2024.geojson  (limites das regiões, Eurostat/GISCO)
#
# como correr: abrir este ficheiro a partir da pasta do projeto e correr tudo de cima para baixo
# (RStudio: Session > Set Working Directory > To Source File Location)
# setwd("caminho/para/a/pasta/do/projeto")

library(tidyverse)
library(sf)
library(ggrepel)    # para os nomes não ficarem uns por cima dos outros
library(patchwork)  # para juntar vários gráficos numa só figura


# ---------------------------------------------------------------------
# 1. DEFINIÇÕES (tipo de letra, tamanhos e cores)
# ---------------------------------------------------------------------

# tipo de letra de toda a figura (Noto Sans; registamos os ficheiros normal e negrito)
systemfonts::register_font(
  name  = "Noto Sans T06",
  plain = "C:/Windows/Fonts/NotoSans-Regular.ttf",
  bold  = "C:/Windows/Fonts/NotoSans-Bold.ttf"
)
tipo_letra <- "Noto Sans T06"

# tamanhos do texto, em pontos (como ficam impressos em A3)
tam_nome     <- 10   # nome da região ao lado da casa
tam_perc     <- 16   # percentagem grande ao lado da casa
tam_legenda  <- 10   # texto da legenda
tam_pequeno  <- 9    # o texto mais pequeno que usamos
tam_tit_tira <- 13   # título de cada tira de pontos
tam_eixo     <- 10   # texto dos eixos das tiras

# 4 tramos de vermelho: quanto mais escuro, maior a parte do salário que vai para a renda
limites_tramos <- c(-Inf, 30, 40, 50, Inf)
nomes_tramos   <- c("under 30%", "30-40%", "40-50%", "over 50%")
cores_tramos   <- c("#FEF0D9", "#FDCC8A", "#FC8D59", "#D7301F")

cor_texto     <- "#1E1E1E"   # texto principal
cinza_texto   <- "#555555"   # texto secundário
cinza_linha   <- "#9A9A9A"   # linhas que ligam a região à casa
cor_contorno  <- "#555555"   # contorno das casas
cor_telhado   <- "#8C8C8C"   # telhado das casas
cor_destaque  <- "#D7301F"   # igual ao tramo mais escuro: regiões acima de 50%
cinza_porto   <- "#444444"   # nome do AM Porto nas tiras
cinza_pista   <- "#E6E6E6"   # linha cinzenta onde ficam os pontos

# hipótese nossa: uma casa arrendada tem 80 m2
area_casa <- 80

# tamanho das casas (o mapa está desenhado em km, ver secção 3)
largura_casa  <- 40
altura_casa   <- 26
altura_telhado <- 11

# o ggplot mede o texto em mm; esta função passa de pontos para mm
em_pt <- function(tamanho) tamanho / .pt


# ---------------------------------------------------------------------
# 2. DADOS
# ---------------------------------------------------------------------
dados_todos <- read_csv(
  "data/merged_data.csv",
  col_types = cols(nuts_code = col_character()),
  show_col_types = FALSE
)

# só o ano 2024
# esforço = renda por m2 x área da casa / salário mensal x 100
regioes <- dados_todos |>
  filter(year == 2024) |>
  mutate(
    esforco      = rent_m2 * area_casa / wage_monthly * 100,
    cor_tramo    = cores_tramos[cut(esforco, limites_tramos, labels = FALSE)],
    acima_metade = esforco > 50
  ) |>
  select(nuts_code, region, esforco, cor_tramo, acima_metade)

# nomes mais curtos para caberem ao lado das casas
nomes_curtos <- c(
  "Área Metropolitana do Porto" = "AM Porto",
  "Alto Tâmega e Barroso"       = "Alto Tâmega",
  "Terras de Trás-os-Montes"    = "Trás-os-Montes",
  "Beiras e Serra da Estrela"   = "Beiras e S. Estrela",
  "Região de Aveiro"            = "Aveiro",
  "Região de Coimbra"           = "Coimbra",
  "Região de Leiria"            = "Leiria",
  "Região Autónoma dos Açores"  = "Açores",
  "Região Autónoma da Madeira"  = "Madeira",
  "Península de Setúbal"        = "Setúbal"
)

regioes <- regioes |>
  mutate(nome = if_else(region %in% names(nomes_curtos), nomes_curtos[region], region))

# características de cada região para as tiras de pontos
# (todas "por pessoa", para poder comparar regiões grandes e pequenas)
caracteristicas <- dados_todos |>
  group_by(nuts_code) |>
  summarise(
    cresc_pop  = (population[year == 2024] / population[year == 2021] - 1) * 100,
    estrang_pct = foreign_residents_aima_2024[year == 2024] / population[year == 2024] * 100,   # 2024, fonte AIMA
    noites_por_residente = tourist_nights[year == 2024] / population[year == 2024],
    jovens_pct = share_20_34[year == 2024],   # já vem em percentagem
    .groups = "drop"
  ) |>
  left_join(regioes, by = "nuts_code")


# ---------------------------------------------------------------------
# 3. FORMAS DO MAPA (em km)
# ---------------------------------------------------------------------
# o mapa fica em km, com o canto de baixo à esquerda do continente em (0, 0)
# assim todas as posições (casas, ilhas, legenda) são números simples

formas <- st_read("data/nuts3_portugal_2024.geojson", quiet = TRUE) |>
  mutate(
    nuts_code = sub("^PT", "", NUTS_ID),
    nuts_code = recode(nuts_code, "200" = "20", "300" = "30")   # no CSV as ilhas são "20" e "30"
  ) |>
  left_join(regioes, by = "nuts_code")

# continente: projeção oficial de Portugal (em metros), depois passa para km
continente <- formas |>
  filter(!nuts_code %in% c("20", "30")) |>
  st_transform(3763)

canto <- st_bbox(continente)
st_geometry(continente) <- (st_geometry(continente) - c(canto$xmin, canto$ymin)) / 1000
continente <- st_set_crs(continente, NA)

largura_continente <- (canto$xmax - canto$xmin) / 1000   # cerca de 285 km
altura_continente  <- (canto$ymax - canto$ymin) / 1000   # cerca de 575 km

# as ilhas estão longe, por isso desenhamo-las em quadrados pequenos, no oceano
# esta função põe o centro da ilha em (x, y) e encolhe-a para a largura pedida (em km)
mover_ilha <- function(ilha, x, y, largura) {
  ilha   <- st_transform(ilha, 3857)
  caixa  <- st_bbox(ilha)
  centro <- c((caixa$xmin + caixa$xmax) / 2, (caixa$ymin + caixa$ymax) / 2)
  escala <- largura / (caixa$xmax - caixa$xmin)
  st_geometry(ilha) <- (st_geometry(ilha) - centro) * escala + c(x, y)
  st_set_crs(ilha, NA)
}

acores  <- formas |> filter(nuts_code == "20")
madeira <- formas |>
  filter(nuts_code == "30") |>
  st_crop(xmin = -17.5, xmax = -16.0, ymin = 32.5, ymax = 33.2)   # tira as ilhas Selvagens, que são minúsculas


# ---------------------------------------------------------------------
# 4. ONDE FICA CADA CASA
# ---------------------------------------------------------------------
# duas colunas de casas, uma de cada lado do mapa, do topo para baixo
# a ordem é mais ou menos de norte para sul, com algumas trocas
# (Douro / Tâmega e Sousa e Beiras / Viseu) para as linhas não se cruzarem

ordem_esq <- c("111", "112", "11A", "191", "192", "193",
               "1D1", "1D3", "1A0", "1B0", "1C1", "150")
ordem_dir <- c("11B", "11E", "119", "11D", "11C", "196",
               "194", "195", "1D2", "1C3", "1C4", "1C2")

y_topo   <- altura_continente - 15   # primeira casa
y_fundo  <- 20                       # última casa do continente
x_esq    <- -95                      # centro das casas da esquerda
x_dir    <- largura_continente + 95  # centro das casas da direita

# um ponto dentro de cada região, de onde sai a linha
pontos_dentro <- st_coordinates(st_point_on_surface(continente))

casas <- continente |>
  st_drop_geometry() |>
  mutate(
    xponto = pontos_dentro[, 1],
    yponto = pontos_dentro[, 2],
    lado   = if_else(nuts_code %in% ordem_esq, "esq", "dir"),
    linha  = if_else(lado == "esq", match(nuts_code, ordem_esq), match(nuts_code, ordem_dir)),
    ycasa  = y_topo - (linha - 1) * (y_topo - y_fundo) / 11,   # 12 casas por coluna, à mesma distância
    xcasa  = if_else(lado == "esq", x_esq, x_dir)
  )

# as ilhas ficam numa linha à parte, em baixo: Açores à esquerda e Madeira à direita
y_ilhas <- -75

casas_ilhas <- regioes |>
  filter(nuts_code %in% c("20", "30")) |>
  mutate(
    lado   = if_else(nuts_code == "20", "esq", "dir"),
    xcasa  = if_else(lado == "esq", x_esq, x_dir),
    ycasa  = y_ilhas,
    xponto = NA,
    yponto = NA
  )

casas <- bind_rows(casas, casas_ilhas)

# mapas das ilhas: Açores no oceano a sudoeste, Madeira a sudeste
acores_mapa  <- mover_ilha(acores,  x = 45,  y = y_ilhas - 5, largura = 150)
madeira_mapa <- mover_ilha(madeira, x = 235, y = y_ilhas - 5, largura = 55)

molduras <- tibble(
  xmin  = c(-35, 200),
  xmax  = c(125, 270),
  ymin  = y_ilhas - 48,
  ymax  = y_ilhas + 40,
  texto = c("Açores (not to scale)", "Madeira (not to scale)")
)


# ---------------------------------------------------------------------
# 5. DESENHO DE CADA CASA
# ---------------------------------------------------------------------
# cada casa é o salário médio todo
# a parte colorida, a começar no chão, é a renda de uma casa de 80 m2
# a linha tracejada marca metade do salário

casas <- casas |>
  mutate(
    # cantos da casa (xcasa, ycasa é o centro)
    x0 = xcasa - largura_casa / 2,
    x1 = xcasa + largura_casa / 2,
    y0 = ycasa - altura_casa / 2,   # chão
    y1 = ycasa + altura_casa / 2,   # topo das paredes

    # até onde sobe a parte colorida (a renda) e onde fica metade do salário
    nivel_renda = y0 + altura_casa * esforco / 100,
    nivel_meio  = y0 + altura_casa / 2,

    # a linha dos 50% é branca nas casas escuras, senão não se via
    cor_meio = if_else(acima_metade, "white", cinza_texto),

    # o texto fica ao lado da casa (alinhado à direita na esquerda, à esquerda na direita)
    xtexto    = if_else(lado == "esq", x0 - 6, x1 + 6),
    hj        = if_else(lado == "esq", 1, 0),
    texto_pct = sprintf("%.1f%%", esforco),
    cor_pct   = if_else(acima_metade, cor_destaque, cor_texto)   # vermelho se passar de 50%
  )

# telhados: um triângulo por casa (3 pontos por casa)
telhados <- bind_rows(
  casas |> transmute(nuts_code, px = x0 - 4, py = y1),                       # canto esquerdo
  casas |> transmute(nuts_code, px = x1 + 4, py = y1),                       # canto direito
  casas |> transmute(nuts_code, px = xcasa,  py = y1 + altura_telhado)       # ponta
)

# linhas que ligam cada região à sua casa (as ilhas não têm linha)
ligacoes <- casas |>
  filter(!is.na(xponto)) |>
  mutate(xfim = if_else(lado == "esq", x1, x0))


# ---------------------------------------------------------------------
# 6. LEGENDA: UMA CASA DE EXEMPLO
# ---------------------------------------------------------------------
# a casa de exemplo explica como se leem as outras

leg_x <- -200                       # lado esquerdo da casa de exemplo
leg_x_cores <- -150 + 340           # onde começam os 4 tramos de cor
leg_y <- altura_continente + 75

casa_leg <- tibble(
  x0 = leg_x,
  x1 = leg_x + 50,
  y0 = leg_y - 18,
  y1 = leg_y + 18
) |>
  mutate(
    nivel_renda = y0 + (y1 - y0) * 0.55,
    nivel_meio  = y0 + (y1 - y0) / 2
  )

notas_leg <- tibble(
  x = leg_x + 60,
  y = c(casa_leg$y1 - 3, casa_leg$nivel_meio, casa_leg$y0 + 6),
  texto = c("the whole house = average monthly salary (2024)",
            "- - - half of the salary",
            paste0("coloured part = rent of an ", area_casa, " m² home"))
)

# os 4 tramos de cor, em caixinhas
caixas_tramos <- tibble(
  texto = nomes_tramos,
  cor   = cores_tramos,
  x0    = leg_x_cores + (0:3) * 55,
  x1    = x0 + 50,
  y0    = leg_y - 4,
  y1    = leg_y + 8
)


# ---------------------------------------------------------------------
# 7. O MAPA
# ---------------------------------------------------------------------
p_mapa <- ggplot() +

  # regiões, coloridas pelo tramo de 2024 (continente, Açores e Madeira)
  geom_sf(
    data = continente,
    aes(fill = cor_tramo),
    colour = "white", linewidth = 0.3
  ) +
  geom_sf(
    data = acores_mapa,
    aes(fill = cor_tramo),
    colour = cor_contorno, linewidth = 0.2
  ) +
  geom_sf(
    data = madeira_mapa,
    aes(fill = cor_tramo),
    colour = cor_contorno, linewidth = 0.2
  ) +

  # molduras das ilhas e o nome de cada uma
  geom_rect(
    data = molduras,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA, colour = cinza_linha, linewidth = 0.3
  ) +
  geom_text(
    data = molduras,
    aes(x = xmin, y = ymax + 7, label = texto),
    hjust = 0, family = tipo_letra, size = em_pt(tam_pequeno), colour = cinza_texto
  ) +

  # contorno cinzento fino do país (as regiões mais claras não se perdem no fundo branco)
  geom_sf(
    data = st_union(continente),
    fill = NA, colour = cinza_linha, linewidth = 0.3
  ) +

  # linhas e pontos
  geom_segment(
    data = ligacoes,
    aes(x = xponto, y = yponto, xend = xfim, yend = ycasa),
    colour = cinza_linha, linewidth = 0.3
  ) +
  geom_point(
    data = ligacoes,
    aes(x = xponto, y = yponto),
    colour = cor_texto, size = 0.9
  ) +

  # casa: 1) a casa toda (o salário), 2) a parte colorida (a renda)
  geom_rect(
    data = casas,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
    fill = "white"
  ) +
  geom_rect(
    data = casas,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = nivel_renda, fill = cor_tramo)
  ) +

  # linha tracejada a meio da casa (50% do salário)
  geom_segment(
    data = casas,
    aes(x = x0, xend = x1, y = nivel_meio, yend = nivel_meio, colour = cor_meio),
    linetype = "22", linewidth = 0.35
  ) +

  # contorno da casa
  geom_rect(
    data = casas,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
    fill = NA, colour = cor_contorno, linewidth = 0.4
  ) +

  # telhado
  geom_polygon(
    data = telhados,
    aes(x = px, y = py, group = nuts_code),
    fill = cor_telhado, colour = cor_contorno, linewidth = 0.4
  ) +

  # ao lado de cada casa: nome (em cima) e percentagem (em baixo)
  geom_text(
    data = casas,
    aes(x = xtexto, y = ycasa + 8, label = nome, hjust = hj),
    family = tipo_letra, fontface = "bold", size = em_pt(tam_nome), colour = cor_texto
  ) +
  geom_text(
    data = casas,
    aes(x = xtexto, y = ycasa - 6, label = texto_pct, hjust = hj, colour = cor_pct),
    family = tipo_letra, fontface = "bold", size = em_pt(tam_perc)
  ) +

  # legenda: a casa de exemplo (paredes, parte colorida, linha dos 50%, contorno e telhado)
  geom_rect(
    data = casa_leg,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
    fill = "white"
  ) +
  geom_rect(
    data = casa_leg,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = nivel_renda),
    fill = cor_destaque
  ) +
  geom_segment(
    data = casa_leg,
    aes(x = x0, xend = x1, y = nivel_meio, yend = nivel_meio),
    colour = "white", linetype = "22", linewidth = 0.35
  ) +
  geom_rect(
    data = casa_leg,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
    fill = NA, colour = cor_contorno, linewidth = 0.4
  ) +
  geom_polygon(
    aes(x = c(leg_x - 4, leg_x + 54, leg_x + 25),
        y = c(casa_leg$y1, casa_leg$y1, casa_leg$y1 + 14)),
    fill = cor_telhado, colour = cor_contorno, linewidth = 0.4
  ) +

  # legenda: título e notas ao lado da casa de exemplo
  geom_text(
    aes(x = leg_x, y = casa_leg$y1 + 28, label = "How to read a house"),
    hjust = 0, family = tipo_letra, fontface = "bold", size = em_pt(tam_legenda + 1), colour = cor_texto
  ) +
  geom_text(
    data = notas_leg,
    aes(x = x, y = y, label = texto),
    hjust = 0, family = tipo_letra, size = em_pt(tam_legenda), colour = cinza_texto
  ) +

  # legenda: os 4 tramos de cor (as mesmas cores do mapa e das casas)
  geom_text(
    aes(x = leg_x_cores, y = leg_y + 22, label = "Rent as % of the salary, 2024"),
    hjust = 0, family = tipo_letra, fontface = "bold", size = em_pt(tam_legenda + 1), colour = cor_texto
  ) +
  geom_rect(
    data = caixas_tramos,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = cor)
  ) +
  geom_text(
    data = caixas_tramos,
    aes(x = (x0 + x1) / 2, y = y0 - 10, label = texto),
    family = tipo_letra, size = em_pt(tam_pequeno), colour = cinza_texto
  ) +

  # as cores já vêm escritas nos dados, por isso usamos os valores tal como estão
  scale_fill_identity() +
  scale_colour_identity() +

  # zona que aparece (espaço dos lados para as casas e em baixo para as ilhas)
  coord_sf(
    xlim   = c(-230, largura_continente + 230),
    ylim   = c(-125, altura_continente + 140),
    expand = FALSE,
    clip   = "off"
  ) +

  theme_void(base_family = tipo_letra) +
  theme(plot.background = element_rect(fill = "white", colour = NA))


# ---------------------------------------------------------------------
# 8. AS TIRAS DE PONTOS: O QUE TÊM ESTAS REGIÕES EM COMUM?
# ---------------------------------------------------------------------
# uma linha horizontal por característica
# cada ponto é uma região, com a mesma cor do mapa
# assim, só é preciso ver onde estão os pontos vermelho escuro

# esta função desenha uma tira
#   valores  = o número de cada região, pela mesma ordem de "caracteristicas"
#   titulo   = título da tira
#   legenda_x = o que mede a linha (aparece por baixo do título)
fazer_tira <- function(valores, titulo, legenda_x) {

  dados_tira <- caracteristicas |> mutate(x = valores)
  destaque   <- dados_tira |> filter(acima_metade)      # as 4 regiões acima de 50%
  porto      <- dados_tira |> filter(nuts_code == "11A")  # o AM Porto serve de referência

  # força da relação com a renda, escrita em palavras (a correlação vai para o relatório)
  rho <- cor(valores, caracteristicas$esforco, method = "spearman")
  frase <- ifelse(abs(rho) >= 0.6, "strong link with less affordable rent",
           ifelse(abs(rho) >= 0.4, "moderate link with less affordable rent",
                                   "weak link overall"))

  ggplot(dados_tira, aes(x = x, y = 0)) +

    # a linha cinzenta onde ficam os pontos
    geom_hline(yintercept = 0, colour = cinza_pista, linewidth = 3) +

    # as outras regiões (contorno cinzento para os pontos mais claros se verem)
    geom_point(
      data = filter(dados_tira, !acima_metade),
      aes(fill = cor_tramo),
      shape = 21, colour = "#8C8C8C", size = 3.6, stroke = 0.3
    ) +

    # as 4 regiões acima de 50%: maiores, com contorno branco e por cima
    geom_point(
      data = destaque,
      aes(fill = cor_tramo),
      shape = 21, colour = "white", size = 4.8, stroke = 0.7
    ) +

    # nomes dessas regiões e do AM Porto
    geom_text_repel(
      data = destaque,
      aes(label = nome),
      nudge_y = 0.55, direction = "x",
      family = tipo_letra, fontface = "bold", size = em_pt(tam_pequeno), colour = cor_destaque,
      segment.colour = cinza_linha, min.segment.length = 0, seed = 1
    ) +
    geom_text_repel(
      data = porto,
      aes(label = nome),
      nudge_y = -0.55, direction = "x",
      family = tipo_letra, fontface = "bold", size = em_pt(tam_pequeno), colour = cinza_porto,
      segment.colour = cinza_linha, min.segment.length = 0, seed = 1
    ) +

    scale_fill_identity() +
    scale_y_continuous(limits = c(-0.9, 1)) +
    labs(title = titulo, subtitle = paste0(legenda_x, "  ·  ", frase)) +

    theme_minimal(base_family = tipo_letra) +
    theme(
      plot.title          = element_text(face = "bold", size = tam_tit_tira, colour = cor_texto),
      plot.subtitle       = element_text(size = tam_eixo, colour = cinza_texto),
      plot.title.position = "plot",
      axis.title          = element_blank(),
      axis.text.y         = element_blank(),
      axis.text.x         = element_text(size = tam_pequeno, colour = cinza_texto),
      panel.grid          = element_blank(),
      plot.margin         = margin(6, 12, 6, 8)
    )
}

# as 4 tiras
# Madeira é a exceção: pouco crescimento, mas muito turismo (ver a tira do turismo)
p_crescimento <- fazer_tira(
  caracteristicas$cresc_pop,
  titulo    = "Where the population grew the most",
  legenda_x = "Population growth, 2021-2024 (%)"
) +
  annotate(
    "text", x = 1.3, y = -0.62, label = "Madeira: little growth,\nbut heavy tourism",
    hjust = 0, family = tipo_letra, size = em_pt(tam_pequeno), colour = cor_destaque, lineheight = 0.9
  )

p_estrangeiros <- fazer_tira(
  caracteristicas$estrang_pct,
  titulo    = "Where more foreign residents live",
  legenda_x = "Foreign residents, 2024 (% of the population)"
)

p_turismo <- fazer_tira(
  caracteristicas$noites_por_residente,
  titulo    = "Where tourists stay the most nights",
  legenda_x = "Tourist nights per resident, 2024"
)

p_jovens <- fazer_tira(
  caracteristicas$jovens_pct,
  titulo    = "Where more young adults live",
  legenda_x = "People aged 20-34, 2024 (% of the population)"
)

# as 4 tiras juntas, com o título do bloco
# o wrap_elements() faz do bloco uma só peça, para o título ir com ele ao pôr ao lado do mapa
p_tiras <- wrap_elements(
  full = p_crescimento / p_estrangeiros / p_turismo / p_jovens +
    plot_annotation(
      title    = "What do these regions have in common?",
      subtitle = "Each dot is a region, coloured like the map. Further right = more of that trait.",
      theme    = theme(
        plot.title    = element_text(family = tipo_letra, face = "bold", size = 18, colour = cor_texto),
        plot.subtitle = element_text(family = tipo_letra, size = tam_eixo + 1, colour = cinza_texto)
      )
    )
)


# ---------------------------------------------------------------------
# 9. JUNTAR TUDO NUMA PÁGINA A3 E GUARDAR
# ---------------------------------------------------------------------

# quantas regiões gastam mais de metade do salário em renda (para o título)
n_metade <- sum(regioes$acima_metade)

titulo_figura <- paste0("In ", n_metade,
                        " regions, renting a home takes more than half of the average salary")

subtitulo_figura <- paste0(
  "Which Portuguese regions are least affordable to rent in on a local salary, and what do they have in common?\n",
  "Rent of a new ", area_casa, " m² home as a share of the average gross monthly earnings, ",
  "NUTS III regions, 2024."
)

# autores, data e fontes (pedido no enunciado)
rodape_figura <- paste0(
  "Authors: Group T06 - Kevin Gomez, Lara Ribeiro, Maria Benedita Marques   |   ",
  "Created on ", format(Sys.Date(), "%d/%m/%Y"), "   |   ",
  "Sources: INE (rents of new leases, population, tourist nights); ",
  "AIMA (foreign residents, 2024); ",
  "MTSSS/GEP (earnings); Eurostat/GISCO (boundaries)"
)

# mapa à esquerda (cerca de 60% da largura) e tiras à direita (cerca de 40%)
figura_final <- p_mapa + p_tiras +
  plot_layout(widths = c(3, 2)) +
  plot_annotation(
    title    = titulo_figura,
    subtitle = subtitulo_figura,
    caption  = rodape_figura,
    theme    = theme(
      plot.title      = element_text(family = tipo_letra, face = "bold", size = 28, colour = cor_texto),
      plot.subtitle   = element_text(family = tipo_letra, size = 14, colour = cinza_texto,
                                     lineheight = 1.15, margin = margin(t = 4, b = 10)),
      plot.caption    = element_text(family = tipo_letra, size = tam_pequeno, colour = cinza_texto,
                                     hjust = 0, margin = margin(t = 10)),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(18, 18, 12, 18)
    )
  )

# ver no RStudio
figura_final

# guardar em PNG: A3 na horizontal, 300 dpi (pronto a imprimir)
ggsave(
  "T06_rent_affordability.png", figura_final,
  width = 42, height = 29.7, units = "cm", dpi = 300, bg = "white"
)
