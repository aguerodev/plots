# paquetes
library(tidyverse)
library(scales)

# lectura de los datos ----

datos <- read_csv("data/pelis_1937_2023.csv")
glimpse(datos)

top_ingresos <- datos |>
  filter(fecha_estreno >= "2023-01-01") |>
  arrange(desc(ingresos)) |>
  select(titulo, director, ingresos) |>
  head(5)

top_ingresos


ggplot(
  data = top_ingresos,
  mapping = aes(
    x = ingresos,
    y = str_wrap(titulo, 20) |>
      fct_reorder(ingresos),
    label = comma(ingresos/1000000),
    fill = titulo == "Barbie"
  )
) +
  geom_col() +
  geom_text(
    size = 6,
    hjust = 1.2,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("#35374B","#E0218A"),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = comma_format(scale = 0.000001),
    expand = expand_scale(mult  = c(0.01,0.1))
  ) +
  labs(
    title = "Barbie el mayor éxito en 2023",
    subtitle = "Las 5 películas con mayor ingreso (millones USD)*",
    y = "",
    x = "",
    caption = "Fuente: The Movie Database (TMDB)\n*Actualizado a enero 2024"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      color = "#E0218A"
    )
  )

ggsave("plot_barbie.jpg", dpi = 400)
