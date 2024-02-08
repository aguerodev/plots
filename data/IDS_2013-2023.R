library(tidyverse)
library(readxl)
library(here)

# IDS 2013 --------------------------------------------------------------------------------------------------------


df <- read_xlsx(
  path = here("data/IDS_2013.xlsx"),
  sheet = "IDS distrital por regiones",
  range = "A7:C504"
)
colnames(df) <- c("codigo_distrito","distrito","ids")

df <- df |> 
  mutate(
    codigo_distrito = as.character(codigo_distrito),
    ids = round(as.numeric(ids), 2),
    version = 2013
  ) 
  df
write_rds(df, "ids_2013.rds")



# IDS 2017 --------------------------------------------------------------------------------------------------------


df <- read_xlsx(
  path = here("data/IDS_2017.xlsx"),
  range = "A3:D483"
)
df <- clean_names(df)
df <- df |> 
  mutate(
    codigo_pcd = as.character(codigo_pcd),
    version = 2017
  ) |> 
  rename(
    codigo_distrito = codigo_pcd,
    ids = valor
  ) |> 
  select(
    codigo_distrito, distrito, ids, version
  )

write_rds(df, "ids_2017.rds")


# IDS 2023 --------------------------------------------------------------------------------------------------------

df <- read_xlsx(
  path = here("data/IDS_2023.xlsx"),
  range = "A5:D495",
  sheet = "Tabla 4"
)
df <- clean_names(df)
df <- df |> 
  mutate(
    codigo_pcd = as.character(codigo_pcd),
    version = 2023
  ) |> 
  rename(
    codigo_distrito = codigo_pcd,
    ids = valor
  ) |> 
  select(
    codigo_distrito, distrito, ids, version
  )

write_rds(df, "ids_2023.rds")
