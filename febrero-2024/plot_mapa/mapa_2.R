library(tidyverse)
library(sf)
library(readxl)
library(santoku)
library(ggokabeito)
library(ggnewscale)

tablas <- paste0("Tabla ", 15:20)

df <- map(tablas, \(tabla){
  read_excel(
    path = "Tablas - IDS 2023-3.xlsx",
    sheet = tabla,
    range = "A7:J330",
    col_names = c("distrito","salud","participacion_electoral",
                  "seguridad","educacion","economico","ids_2023",
                  "quintil","posicion_nacionl","posicion_regional")
  ) |>
    mutate(
      canton = if_else(is.na(salud), distrito, NA_character_),
      .before = distrito
    ) |>
    mutate(
      fecha = 2023,
      canton = str_to_title(canton)
    ) |>
    fill(canton) |>
    drop_na() |>
    select(fecha,canton, distrito, salud, educacion, seguridad, economico, participacion_electoral)
}) |>
  list_rbind() |>
  summarise(
    across(where(is.numeric), mean),
    .by = c(fecha, canton)
  )


## dar formato a los datos ----


df <- df |>
  mutate(
    a = economico >= mean(economico),
    b = seguridad >= mean(seguridad),
    grupo = case_when(
      a & b   ~ "grupo 1",
      a & !b  ~ "grupo 2",
      !a & b  ~ "grupo 3",
      !a & !b ~ "grupo 4"
    )
  )

mapa <- cr_regions |>
  left_join(
    df,
    by = join_by(canton)
  )

title_color  <- "#332941"
subtitle_color <- "#61677A"

ggplot() +
  geom_sf(
    data = filter(mapa, grupo %in% c("grupo 1","grupo 2") ),
    mapping = aes(
      fill = grupo
    ),
    color = "white",
    linewidth = 0.4
    ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      ncol = 1,

    )
  ) +
  scale_fill_manual(
    values = c("#A2C579","#4F6F52","red"),
    labels=c('Seguridad mayor o igual \nal promedio (84.4)', 'Seguridad menor\nal promedio'),
    name = "Economía superior o igual\nal promedio (47.4)"
  ) +
  new_scale_fill() +
  geom_sf(
    data = filter(mapa, grupo %in% c("grupo 3","grupo 4") ),
    mapping = aes(
      fill = grupo
    ),
    color = "white",
    linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c("#C68484","#9B4444","red"),
    labels=c('Seguridad mayor o igual\nal promedio', 'Seguridad menor\nal promedio'),
    name = "Economía por debajo\ndel promedio nacional",
  ) +
  theme_void(
    base_family = "Domine"
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      ncol = 1
    )
  ) +
  theme(
    legend.position = "left",
    legend.title = element_text(size = 12),
    legend.key.size = unit(1.5, 'lines')
  )



