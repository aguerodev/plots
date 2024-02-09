library(tidyverse)
library(ggrepel)

ids_cr <- read_csv(
  file = "data/ids_cr_2013-2023.csv",
  show_col_types = FALSE
  ) |> 
  drop_na()

glimpse(ids_cr)

df <- ids_cr |> 
  summarise(
    ids = mean(ids),
    .by = c(canton,version)
  ) |> 
  filter(version %in% c(2017,2023)) |> 
  mutate(
    diferencia = ids[2]-ids[1],
    cambio = if_else(
       diferencia >= 0,
      "Mejora",
      "Empeora"
    ),
    .by = canton
  ) |> 
  drop_na() |> 
  mutate(
    grupo = case_when(
      canton %in% {
        df |> 
          filter(version == 2023) |> 
          slice_min(diferencia, n = 5) |> 
          pull(canton)
      } ~ "Perdedores",
      canton %in% {
        df |> 
          filter(version == 2023) |> 
          slice_max(diferencia, n = 5) |> 
          pull(canton)
      } ~ "Ganadores",
      .default = "Otros"
    )
  )



ggplot(
  data = df,
  mapping = aes(
    x = as.factor(version),
    y = ids,
    color = grupo,
    group = canton,
    alpha = grupo
  )
)   +
  geom_line(linewidth = 1) +
  geom_text(
    data = {
      filter(
        df,
        version == 2017,
        grupo != "Otros",
        cambio == "Empeora")
    },
    mapping = aes(
      label = glue("{round(ids)}")
    ),
    nudge_x = -0.15,
    hjust = "left",
    fontface = "bold"
  ) +
  geom_text_repel(
    data = {
      filter(
        df,
        version == 2023,
        grupo != "Otros",
        cambio == "Empeora")
    },
    mapping = aes(
      label = glue("{canton} {round(diferencia)}")
    ),
    nudge_x = 0.8,
    direction = "y",
    hjust = "right",
    min.segment.length = 0.1,
    fontface = "bold"
  ) +
  
  geom_text(
    data = {
      filter(
        df,
        version == 2017,
        grupo != "Otros",
        cambio == "Mejora")
    },
    mapping = aes(
      label = glue("{round(ids)}")
    ),
    nudge_x = -0.05,
    hjust = "right",
    fontface = "bold"
  ) +
  
  geom_text_repel(
    data = {
      filter(
        df,
        version == 2023,
        grupo != "Otros",
        cambio == "Mejora")
    },
    mapping = aes(
      label = glue("{canton} +{round(diferencia)}")
    ),
    nudge_x = 0.65,
    direction = "y",
    hjust = "right",
    min.segment.length = 0.1,
    fontface = "bold"
  ) +
  facet_wrap(~cambio) +
  scale_alpha_manual(
    values = c(
      "Ganadores" = 1,
      "Perdedores" = 1,
      "Otros" = 0.2
    )
  ) + 
  labs(
    title = "Análisis del IDS en Costa Rica",
    subtitle = "Evaluación del Desarrollo Social a Nivel Cantonal (2013-2023)",
    caption = "Fuente: Ministerio de Planificación Nacional y Política Económica."
  ) +
  scale_color_manual(
    values = c(
      "Ganadores" = "#0056B9",
      "Perdedores" = "#A94438",
      "Otros" = "#7D7463"
    )
  ) +
  theme_void(
    base_size = 16,
    base_family = "Domine"
  ) +
  theme(
    plot.background = element_rect(fill = "#F5EEE6", color = "#F5EEE6"),
    plot.margin = margin(10,10,10,10),
    strip.placement = "outside",
    strip.clip = "off",
    strip.text = element_text(size = 16,margin = margin(b = 10, t = 10),family = "Roboto"),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(margin = margin(t = 5, b = 15), size =16),
    plot.caption = element_text(margin = margin(t = 20), hjust = 0, size = 10),
    text = element_text(),
    legend.position = "none",
    panel.grid.major.x = element_line(
      linewidth = 0.2,
      color = "#DED0B6"
    ),
    axis.text.x = element_text(family = "Roboto")
  )

ggsave(
  filename = "plot3.png",
  dpi = 400)
