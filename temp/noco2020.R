pacman::p_load(dplyr, terra,sf )
# city data 
s1 <- terra::vect("data/top200_simple.gpkg")
df <- readr::read_csv("data/top200.csv")

View(df)
nchar(s2[[1]])
# top 200 


# largest area 
s1$area <- terra::expanse(x = s1, unit = "km")
# greenest city 

# high pop 20 and younger 
df <- df %>%
  mutate(
    percent_less_than_35 = ((popOver20_2023 - popOver35_2023) / popOver20_2023) * 100,
    percent_older_than_55 = (popOver55_2023 / popOver20_2023) * 100
  )


# highest pop 55 and older 

# Do fort collins greely longmont and loveland make the list 

# state with no cities in top 200 

# state with most cities 
df2 <- df |> 
  dplyr::group_by(state)|>
  dplyr::summarise(
    avePop = mean(popOver20_2023),
    count = n()
  )
# complex city 
s2 <- terra::vect("data/top200.gpkg")
s2$area <- terra::expanse(s2, unit = "km")
sg <- geom(s2, wkt = TRUE) 
s2$length <- purrr::map(sg, .f = nchar)
# ugliest city 

# bonus : pizza city 

