library(sf)
library(rmapshaper)
library(httr2)
library(ows4R)
library(janitor)
library(rlang)
library(tidyverse)
library(santoku)


# descargar datos del IGN (Costa Rica) ----------------------------------------------------------------------------

wfs_regions <- "https://geos.snitcr.go.cr/be/IGN_5_CO/wfs?"
regions_client <- WFSClient$new(wfs_regions,
                                serviceVersion = "2.0.0")
url <- url_parse(wfs_regions)
url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "IGN_5_CO:limitecantonal_5k",
                  srsName = "EPSG:4326"
)
request <- url_build(url)
cr_regions <- read_sf(request) |>
  clean_names()


## optimizar el mapa ----

# simplificar mapa
cr_regions <- ms_simplify(
  input = cr_regions,
  keep = 0.05,
  keep_shapes = FALSE
)

# eliminar la isla del coco
cr_regions <- ms_filter_islands(
  input = cr_regions,
  min_area = 24000000
)

cr_regions <- cr_regions |>
  select(
    codigo_provincia = codigo_de_provincia
    , provincia, codigo_canton, canton
  ) |>
  mutate(
    canton = str_to_title(canton)
  )

# write_rds(cr_regions, "sf_cantones_cr.rds")
# recomiendo guardar el archivo como rds y asi no tener que
# repetir este proceso
# cr_regions <- read_rds("sf_cantones_cr.rds")

ggplot(
  data = cr_regions
) +
  geom_sf()

# leer datos del IDS desde excel ----------------------------------------------------------------------------------
# se hace la lectura de los archivos xlsx publicados
# por https://www.mideplan.go.cr/indice-desarrollo-social

## datos 2023 ----
# los datos estan separados cada region
# en una hoja distinta 🎉😒
tablas <- paste0("Tabla ", 15:20)

ids_2023 <- map(tablas, \(tabla){
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
    select(fecha,canton, distrito, salud, educacion, seguridad, economico, participacion_electoral) |>
    summarise(
      across(where(is.numeric), mean),
      .by = c(fecha, canton)
    )
}) |>
  list_rbind()


## dar formato a los datos ----

ids_2023 <- ids_2023 |>
  pivot_longer(
    cols = -canton,
    names_to = "indicador",
    values_to = "valor"
  ) |>
  mutate(
    indicador = str_to_title(indicador)
  )


# primeros mapas --------------------------------------------------------------------------------------------------

## plot datos 2023 ----

df <- ids_2023 |>
  filter(
    indicador == "Salud"
  ) |>
  select(canton, valor)

mapa <- cr_regions |>
  left_join(
    df,
    by = join_by(canton)
  )

ggplot(
  data = mapa,
  mapping = aes(
    fill = valor
  )
) +
  geom_sf()


## Ahora un poco mas de cariño ----

df <- ids_2023 |>
  filter(
    indicador == "Seguridad"
  ) |>
  mutate(
    grupo = ordered(chop_mean_sd(valor))
  )

mapa <- cr_regions |>
  left_join(
    df,
    by = join_by(canton)
  )

ggplot(
  data = mapa,
  mapping = aes(
    fill = grupo
  )
) +
  geom_sf(color = "white", linewidth = 0.4) +
  scale_fill_manual(
    values = c("#C35E34","#E6A777","#F1ECD7","#A7C2A4","#448C82","#346b64"),
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_void()

ggsave("plot_mapa.png", dpi = 400)
