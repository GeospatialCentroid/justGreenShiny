pacman::p_load(dplyr, sf, readr)

# reference data
cityHealth <- read_csv(
  "~/trueNAS/work/justGreen/data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
)
ctHealth <- read_csv(
  "~/trueNAS/work/justGreen/data/products/healthMeasures/allCT_2023_morDemStroke_with10percentAdjust_svi.csv"
)
totalPop <- read_csv(
  "~/trueNAS/work/justGreen/data/processed/top200_2023/totalPopCities.csv"
)
# spatial data
cityFiles <- list.files(
  "~/trueNAS/work/justGreen/data/processed/top200_2023",
  pattern = ".gpkg",
  full.names = TRUE
)
ctFiles <- list.files(
  "~/trueNAS/work/justGreen/data/processed/censusGeographies",
  pattern = "ct.gpkg",
  full.names = TRUE
)
#

cities <- unique(totalPop$NAME)
city <- cities[3]

processPage2Data <- function(city) {
  # replace all spaces with underscore in city name
  # cName <- stringr::str_replace_all(
  #   string = city,
  #   pattern = " ",
  #   replacement = "_"
  # )

  # need this to be the geoid of the city
  export1 <- paste0("data/ctFiles/", city, "_ct.gpkg")
  export2 <- paste0("data/ctFiles/", city, "_ct.csv")

  if (!file.exists(export1)) {
    # pop data
    tPop <- totalPop[totalPop$NAME == city, ] |> pull(totalPopulation)
    # define state
    state <- totalPop[totalPop$NAME == city, ] |> pull(State)

    # filter health data to city
    cH <- cityHealth[cityHealth$city == city, ]
    ctH <- ctHealth[ctHealth$city == city, ]
    # filter spatial data to state and city
    ## city level
    cS <- terra::vect(cityFiles[grepl(pattern = state, x = cityFiles)]) |>
      terra::makeValid()
    cS <- cS[cS$NAME == city, ]
    ## census tract
    ctS <- terra::vect(ctFiles[grepl(pattern = state, x = ctFiles)]) |>
      terra::makeValid()
    ctS <- ctS[ctS$GEOID %in% ctH$geoid, ]
    ctS$geoid <- ctS$GEOID
    # join health data
    ctS <- terra::merge(x = ctS, y = ctH, by = "geoid")

    # crop the ct to the
    crop_ct <- terra::crop(ctS, cS)
    # select the census tracts
    ct_select <- ctS[ctS$GEOID %in% crop_ct$GEOID, ]
    # spilt into a csv and spatial data file to join on GEOID
    geom <- ct_select[, "GEOID"]
    values <- terra::values(ct_select)
    #export
    terra::writeVector(x = geom, filename = export1, overwrite = TRUE)
    readr::write_csv(x = values, file = export2)
  }
}
#issues
## 18,32,120,129,135,172
## only went to 195?
for (i in 173:length(cities)) {
  print(i)
  city <- cities[[i]]
  processPage2Data(city = city)
}
