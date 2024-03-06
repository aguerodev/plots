
# dependencias ----------------------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(umap)
library(factoextra)
library(FactoMineR)
library(glue)
library(santoku)
## dependencias para los gráficos
library(ggrepel)
library(ggforce)
library(ggokabeito)
library(ggtext)
library(ggridges)


# lectura de los datos --------------------------------------------------------------------------------------------

## IDS de MIDEPLAN ----
## https://www.mideplan.go.cr/indice-desarrollo-social

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
      canton = str_to_title(canton)
    ) |>
    fill(canton) |>
    drop_na() |>
    select(canton, distrito, salud, educacion, seguridad,
           economico)
}) |>
  list_rbind() |>
  summarise(
    across(where(is.numeric), mean),
    .by = canton
  )

df <- df |>
  add_row(
    canton = "Salud +",
    salud = 100,
    educacion = mean(df$educacion),
    seguridad = mean(df$seguridad),
    economico = mean(df$economico)
  ) |>add_row(
    canton = "Salud -",
    salud = 0,
    educacion = mean(df$educacion),
    seguridad = mean(df$seguridad),
    economico = mean(df$economico)
  ) |>
  add_row(
    canton = "Educación +",
    salud = mean(df$salud),
    educacion = 100,
    seguridad = mean(df$seguridad),
    economico = mean(df$economico)
  )|>add_row(
    canton = "Educación -",
    salud = mean(df$salud),
    educacion = 0,
    seguridad = mean(df$seguridad),
    economico = mean(df$economico)
  ) |>
  add_row(
    canton = "Seguridad +",
    salud = mean(df$salud),
    educacion = mean(df$educacion),
    seguridad = 100,
    economico = mean(df$economico)
  ) |>
  add_row(
    canton = "Seguridad -",
    salud = mean(df$salud),
    educacion = mean(df$educacion),
    seguridad = 0,
    economico = mean(df$economico)
  ) |>
  add_row(
    canton = "Economía +",
    salud = mean(df$salud),
    educacion = mean(df$educacion),
    seguridad = mean(df$seguridad),
    economico = 100
  )|>
  add_row(
    canton = "Economía -",
    salud = mean(df$salud),
    educacion = mean(df$educacion),
    seguridad = mean(df$seguridad),
    economico = 0
  )

# UMAP ----
set.seed(2781)

.config <- umap.defaults
.config$n_neighbors <- 5
.config$n_components <- 2

# umap
res <- df |>
  select(where(is.numeric)) |>
  umap(config = .config)

# clustering
df_umap <- as_tibble(res$layout)
cj <- hcut(df_umap, k = 5)
df_umap$grupo <- as_factor(paste0("Grupo ",cj$cluster))
df_plot <- cbind(df,df_umap)

df_plot <- df_plot |>
  mutate(
    mostrar_canton = case_when(
      canton %in% {
        slice_head(df_plot, prop = 0.3, by = grupo) |>
          pull(canton)
      } ~ "Si",
      .default = "No"
    )
  )

df_groups <- df_plot |>
  summarise(
    across(salud:economico, mean),
    .by = grupo
  ) |>
  pivot_longer(-grupo)





# graficos ----

annotate_font_size <- 5
annotate_color <- "#7B8FA1"
geom_text_color <- "#404258"

ggplot(
  data = filter(df_plot, !canton %in% c(
    "Salud +","Salud -", "Seguridad +",
    "Seguridad -", "Educación +",
    "Educación -",
    "Economía +", "Economía -"
  )),
  mapping = aes(
    x = V1,
    y = V2
  )
) +
  geom_mark_ellipse(
    mapping = aes(
      fill = grupo
    ),
    color = "white",
    alpha = 0.3
  ) +
  geom_point(
    mapping = aes(
      alpha = mostrar_canton
    ),
    size = 2
  ) +
  geom_text_repel(
    data = filter(df_plot, mostrar_canton == "Si"),
    aes(label = canton),
    size = 5,
    color = geom_text_color,
    point.padding = 0, # additional padding around each point
    min.segment.length = 0, # draw all line segments
    max.time = 1, max.iter = 1e5
  )  +
  labs(
    x = "",
    y = "",
    subtitle = ""
  ) +
  annotate(
    geom = "text",
    x = 2.3,
    y = 4.8,
    label = "Seguridad +",
    size = annotate_font_size,
    color =  "#4F6F52",
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = 0,
    y = 4.8,
    label = "Salud -",
    size = annotate_font_size,
    color = "#750E21",
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = 1.5,
    y = -0.1,
    label = "Educación +",
    size = annotate_font_size,
    color =  "#4F6F52",
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = 2.3,
    y = -4,
    label = "Salud +",
    size = annotate_font_size,
    color =  "#4F6F52",
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = -4,
    y = -1,
    label = "Seguridad -",
    size = annotate_font_size,
    color = "#750E21",
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = -0.20,
    y = -6,
    label = "Economía +",
    size = annotate_font_size,
    color = "#4F6F52",
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = -4,
    y = 6.5,
    label = "Educación -",
    size = annotate_font_size,
    color = "#750E21",
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    x = -2.5,
    y = 4,
    label = "Economía -",
    size = annotate_font_size,
    color = "#750E21",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = palette_okabe_ito(1:6),
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c("Si" = 1 , "No" = 0.1),
    guide = "none"
  ) +
  scale_x_continuous(
    expand = c(0.1,0.1)
  ) +
  scale_y_continuous(
    expand = c(0.1,0.1)
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Roboto"
    ) +
  theme(
    plot.subtitle = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ),
    plot.background = element_rect(fill = "white", color = "white")
  )

ggsave(
  filename = "plotidsv1.png",
  dpi = 400,
  width = 9.36,
  height = 5.89
)

