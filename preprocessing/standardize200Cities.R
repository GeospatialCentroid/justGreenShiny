pacman::p_load(sf, dplyr, readr, rmapshaper, tmap, stringr)
tmap_mode("view")

# read in spatial object 
s1 <- sf::st_read("preprocessing/data/top200Cities.gpkg") |> 
  st_make_valid()

# read in csv for reference 
c1 <- st_read("preprocessing/data/allCities_2023_morDemStroke_with10percentAdjust.csv")
# convert values from chr to numberic 
colCovert <- names(c1)[5:103]
c1 <- c1 |> 
  mutate(across(.cols = all_of(colCovert), .fns = as.numeric))|>
  dplyr::mutate("NDVI_plus10" = (meanNDVI * 0.1) + meanNDVI)
#reformat a new city option 
c1 <- c1 |>
  dplyr::mutate(
    cityFormat = trimws(gsub("CDP|City", "", city)),
    fullCity = paste0(cityFormat, ", ",state)
  )
# fitler to the columns needed by the application 
c1 <- c1 |>
  dplyr::select(
    fullCity,
    meanNDVI,
    ls_Mortality_Rate,
    ls_Stroke_Rate,
    ls_Dementia_Rate,
    popOver20_2023,
    popOver35_2023,
    popOver55_2023,
    geoid,
    city,
    state,
    countyGEOID ,
    standardDevNDVI
  )
# gather average values 
allCities <- data.frame(
  fullCity = "all cities",
  meanNDVI = mean(c1$meanNDVI, na.rm = TRUE),
  ls_Mortality_Rate = abs(mean(c1$ls_Mortality_Rate, na.rm = TRUE)),
  ls_Stroke_Rate= abs(mean(c1$ls_Stroke_Rate, na.rm = TRUE)),
  ls_Dementia_Rate= abs(mean(c1$ls_Dementia_Rate, na.rm = TRUE)),
  popOver20_2023= sum(c1$popOver20_2023, na.rm = TRUE),
  popOver35_2023= sum(c1$popOver35_2023, na.rm = TRUE),
  popOver55_2023= sum(c1$popOver55_2023, na.rm = TRUE),
  geoid = NA,
  city = "all cities",
  state = NA, 
  countyGEOID = NA,
  standardDevNDVI = mean(c1$standardDevNDVI, na.rm = TRUE)
)



# filter names 
s2 <- s1 |>
  dplyr::select("GEOID","geom") |> 
  st_as_sf(coords = "geom") |>
# ms_simplify is droping geometies so experiment 
  ms_simplify(
    # keep = 0.25,
    # keep_shapes = TRUE, 
    sys_mem = 32)
# running the st make valid before the ms_simply help resolve lots of errors, 
# possible due from generating this data from a CSV.... 

# join 
s3 <- dplyr::left_join(x = s2, y = c1 , by = c("GEOID" = "geoid"))|>
  dplyr::mutate(
    ls_Mortality_Rate = abs(ls_Mortality_Rate),
    ls_Stroke_Rate = abs(ls_Stroke_Rate),
    ls_Dementia_Rate = abs(ls_Dementia_Rate)
  )
View(s3)
qtm(s3)



# assign the popup elements 
s3$popup <- paste0(
  "<strong>City: </strong>", s3$city, "<br>",
  "<strong>Mean NDVI: </strong>", round(s3$meanNDVI, 3),"<br>",
  # "<strong>Mean NDVI: </strong>", round(s3$meanNDVI, 3),"<br>",
  "<strong>Total Population Over 20: </strong>", format(s3$popOver20_2023,  big.mark = ",")
)

# generate the centroid object 
cityCentroid <- sf::st_centroid(s3)


# export 
st_write(obj = s3, dsn = "data/top200_simple.gpkg", delete_layer = TRUE)
st_write(obj = cityCentroid, dsn = "data/top200_centroid.gpkg", delete_layer = TRUE)

# generate a non spatial object for selection 
# bind in the all cities data 
s4 <- bind_rows(as.data.frame(s3), allCities) |>
  dplyr::select(names(allCities))
readr::write_csv(s4, "data/top200.csv")

