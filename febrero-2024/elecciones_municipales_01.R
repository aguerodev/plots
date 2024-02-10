library(tidyverse)
library(ggbump)
library(ggokabeito)

df <- tibble::tribble(
               ~"name", ~"2002", ~"2006", ~"2010", ~"2016", ~"2020", ~"2024",
             "PLN", 27L, 59L, 59L, 50L, 43L, 29L,
            "PUSC", 48L, 11L,  9L, 14L, 15L, 20L,
             "PAC",  1L,  5L,  6L,  6L,  4L,  0L,
             "PNG",  0L,  0L,  0L,  3L,  4L,  4L,
      "Cantonales",  4L,  5L,  2L,  5L,  13L, 12L,
              "ML",  0L,  1L,  2L,  0L,  0L,  0L,
            "PASE",  0L,  0L,  2L,  1L,  0L,  0L,
             "PRC",  1L,  0L,  1L,  0L,  0L,  0L,
            "PRSC",  0L,  0L,  0L,  1L,  2L,  2L,
  "Unidos Podemos",  0L,  0L,  0L,  0L,  1L,  9L,
             "PLP",  0L,  0L,  0L,  0L,  0L,  3L,
              "FA",  0L,  0L,  0L,  1L,  0L,  1L,
            "PPSD",  0L,  0L,  0L,  0L,  0L,  2L,
             "PNR",  0L,  0L,  0L,  0L,  0L,  2L
  )

# verificar la cantidad de cantones entre 81 y 84
colSums(df[,-1])

df <- df |>
  pivot_longer(
    cols = -name,
    names_to = "year",
    values_to = "value",
    names_transform = \(x) as.numeric(x)
  )

# partidos con alcaldias en 2002

names_2002 <- df |>
  filter(year == 2002, value >0) |>
  pull(name)

df <- df |>
  mutate(
    name = case_when(
      !name %in% names_2002 ~ "Otros",
      .default = name
    )
  ) |>
  # sumar por nombre y año
  summarise(
    value = sum(value),
    .by = c(name,year)
  ) |>
  mutate(
    rank = row_number(value),
    .by = c(year)
  )

ggplot(df, aes(year, rank, color = name)) +
  geom_point(size = 5, shape = "|") +
  geom_text(
    mapping = aes(x = year, label = value),
    size = 5,
    vjust = -1,
    family = "Roboto"
  ) +
  geom_text(
    data = df %>% filter(year == min(year)),
    mapping = aes(x = year, label = name),
    size = 5,
    hjust = 1,
    nudge_x = -0.3,
    family = "Roboto",
    fontface = "bold"
  ) +
  geom_text(
    data = df %>% filter(year == max(year)),
    mapping = aes(x = year, label = name),
    size = 5,
    hjust = 0,
    nudge_x = 0.3,
    family = "Roboto",
    fontface = "bold"
  ) +
  geom_bump(linewidth = 2) +
  scale_x_continuous(
    limits = c(1999, 2027),
    breaks = c(2002,2006,2010,2016,2020,2024)
  ) +
  scale_y_continuous(
    breaks = seq(1,6,1),
    limits = c(0.3,6.7)
    ) +
  scale_color_manual(
    values = c(
      "PLN" = "#009E73",
      "PUSC" = "#0072B2",
      "PAC" = "#d9ce3b",
      "PNR" = "#D55E00",
      "Cantonales" = "#56B4E9",
      "Otros" = "#E69F00"
    )
  ) +
  annotate(
    x = 2001.5,
    y = 6.6,
    geom = "text",
    label = "Más alcaldías",
    hjust = 1,
    color = "#c9bda7"
  ) +
  annotate(
    x = 2001.5,
    y = 0.4,
    geom = "text",
    label = "Menos alcaldías",
    hjust = 1,
    color = "#c9bda7"
  ) +
  annotate(
    x = 2024.5,
    y = 6.6,
    geom = "text",
    label = "Más alcaldías",
    hjust = 0,
    color = "#c9bda7"
  ) +
  annotate(
    x = 2024.5,
    y = 0.4,
    geom = "text",
    label = "Menos alcaldías",
    hjust = 0,
    color = "#c9bda7"
  ) +
  theme_void(
    base_size = 16,
    base_family = "Domine"
    ) +
  labs(
    title = "¿Qué ha cambiado en los últimos 22 años?",
    subtitle = "Evolución y tendencias electorales a nivel municipal (2002-2024)",
    caption = "El conjunto de partidos denominado 'Otros' está conformado por los partidos que no obtuvieron alcaldías en las elecciones\nde 2002. El orden de los partidos está determinado por la cantidad de alcaldías obtenidas para cada uno de los años.\nFuente de los datos: Tribunal Supremo Electoral (TSE)."
  ) +
  theme(
    plot.background = element_rect(fill = "#F5EEE6", color = "#F5EEE6"),
    plot.margin = margin(10,10,10,10),
    plot.title = element_text(),
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.caption = element_text(margin = margin(t = 20), hjust = 0, size = 12),
    text = element_text(),
    legend.position = "none",
    panel.grid.major.x = element_line(
      linetype = "dashed",
      color = "#DED0B6"
    ),
    axis.text.x = element_text(family = "Roboto")
  )

ggsave(
  filename = "plot1v2.png",
  dpi = 400,
  width = 10,
  height = 5.89
)
