library(tidyverse)
library(ggbump)
library(extrafont)

df <- tibble::tribble(
               ~"name", ~"2002", ~"2006", ~"2010", ~"2016", ~"2020", ~"2024",
             "PLN", 27L, 59L, 59L, 50L, 43L, 29L,
            "PUSC", 48L, 11L,  9L, 14L, 15L, 20L,
             "PAC",  1L,  5L,  6L,  6L,  4L,  0L,
      "Cantonales",  0L,  0L,  0L,  3L,  4L,  4L,
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


df

df <- df |> 
  pivot_longer(cols = -name, names_to = "year", values_to = "value",
               names_transform = \(x) as.numeric(x))

df

df <- df |> 
  mutate(
    name = case_when(
      !name %in% c("PLN","PUSC","PAC","ML","PNR") ~ "Otros",
      .default = name
    )
  ) |> 
  summarise(
    value = sum(value), 
    .by = c(name,year)
  ) |> 
  arrange(year,desc(value)) |> 
  mutate(
    rank = row_number(),
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
    limits = c(2000, 2026),
    breaks = c(2002, 2006, 2010, 2016, 2020, 2024)
  ) +
  scale_y_reverse(limits = c(5.5, 0.5)) +
  scale_color_manual(
    values = c(
      "PLN" = "#508D69",
      "PUSC" = "#11235A",
      "PAC" = "#FFD800",
      "ML" = "#D04848",
      "PNR" = "#59B4C3",
      "Otros" = "#BEADFA"
    )
  ) +
  theme_void(
    base_size = 16,
    base_family = "Domine"
    ) +
  labs(
    title = "Distribución de alcaldías por partidos políticos",
    subtitle = "Evolución y tendencias electorales a nivel municipal (2002-2024)",
    caption = "El conjunto de partidos llamado 'Otros' esta conformado por: partidos cantonales, PASE, PRC, PRSC, Unidos Podemos, PLP,\nFA y PPSD. Fuente de los datos: Tribunal Supremo Electoral (TSE)."
  ) +
  theme(
    plot.background = element_rect(fill = "#F5EEE6", color = "#F5EEE6"),
    plot.margin = margin(10,10,10,10),
    plot.title = element_text(),
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 12),
    text = element_text(),
    legend.position = "none",
    panel.grid.major.x = element_line(
      linetype = "dashed",
      color = "#DED0B6"
    ),
    axis.text.x = element_text(family = "Roboto")
  )

ggsave(filename = "plot1.png", dpi = 400)
