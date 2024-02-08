library(tidyverse)
library(santoku)
library(ggrepel)

ids_cr <- read_csv(
  file = "ids_cr_2013-2023.csv",
  show_col_types = FALSE
  )

glimpse(ids_cr)

df <- ids_cr |> 
  summarise(
    ids = mean(ids),
    .by = c(canton,version)
  ) |> 
  filter(version %in% c(2013,2023)) |> 
  mutate(
    aumenta = if_else(
      (ids[1] - ids[2]) <= 0,
      "Mejora",
      "Empeora"
    ),
    d = ids[2] - ids[1] ,
    .by = canton
  ) |> 
  drop_na() |> 
  mutate(
    d = case_when(
      between(d, -10,-5) ~ "Perdedores",
      between(d, 10,15) ~ "Ganadores",
      .default = "Otros"
    )
  )


ggplot(
  data = df,
  mapping = aes(
    x = as.factor(version),
    y = ids,
    color = d,
    group = canton,
    alpha = d
  )
)   +
  geom_line(linewidth = 1) +
  geom_text_repel(
    data = filter(df, version == 2023, d != "Otros"),
    mapping = aes(
      label = canton
    ),
    nudge_x = 0.05,
    direction = "y",
    hjust = "left",
    min.segment.length = 0.1,
    fontface = "bold"
  ) +
  facet_wrap(~aumenta) +
  scale_alpha_manual(
    values = c(
      "Ganadores" = 1,
      "Perdedores" = 1,
      "Otros" = 0.4
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
      linewidth = 0.7,
      color = "#DED0B6"
    ),
    axis.text.x = element_text(family = "Roboto")
  )

ggsave(
  filename = "plot2.png",
  dpi = 400)
