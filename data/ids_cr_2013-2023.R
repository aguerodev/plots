library(tidyverse)
library(readxl)

localidades <- read_xlsx(
  path = here("data/localidades_cr.xlsx"),
  range = "A6:F11964"
  ) |> 
  clean_names() |> 
  select(dta_ii, contains("nombre")) |> 
  filter(!duplicated(dta_ii)) |> 
  mutate(
    dta_ii = as.character(dta_ii)
  )


files <- list.files(
  path = here("data"),
  pattern = ".rds"
  )

df <- map(files, read_rds) |> 
  list_rbind()

df |> 
  left_join(
    localidades,
    by = join_by(codigo_distrito == dta_ii)
  ) |> 
  select(
    dta = codigo_distrito,
    provincia = nombre_provincia,
    canton = nombre_canton,
    distrito = distrito, ids, version
  ) |> 
  write_csv("ids_cr_2013-2023.csv")
