library(sf)
library(rmapshaper)
library(httr2)
library(tidyverse)
library(ows4R)
library(janitor)


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

cr_regions |>
  select(
    codigo_provincia = codigo_de_provincia
    , provincia, codigo_canton, canton
  ) |>
  write_rds("data/sf_cantones_cr.rds")

localidades <- read_xlsx(
  path = "data/localidades_cr.xlsx",
  range = "A6:X11964"
) |>
  clean_names()

localidades <- localidades |>
  select(
    dta = dta_ii,
    id_localidad,
    codigo_provincia,
    provincia = nombre_provincia,
    codigo_canton,
    canton = nombre_canton,
    codigo_distrito,
    distrito = nombre_distrito,
    localidad = nombre_localidad,
    nombre_oficial,
    nombre_compuesto,
    urbanidad,
    tipologia,
    region,
    latitud,longitud
  ) |>
  mutate(
    longitud = parse_number(longitud, locale = locale(decimal_mark = ",")),
    latitud = parse_number(latitud, locale = locale(decimal_mark = ","))
  )

DT_sf = st_as_sf(localidades, coords = c("longitud", "latitud"),
                 crs = 4326)

ggplot(
  data = DT_sf
) +
  geom_sf()

write_rds(DT_sf,"data/localidades_cr.rds")
write_rds(cr_regions, "data/mapa_cantones.rds")

localidades |>
  select(
    dta,
    codigo_provincia,
    provincia,
    codigo_canton,
    canton,
    codigo_distrito,
    distrito
  ) |>
  write_rds("data/catalogo_dta_cr.rds")
