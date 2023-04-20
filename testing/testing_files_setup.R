# create testing files for Onondaga County

ot <- tigris::tracts("NY", "Onondaga", year = 2010)

obg <- tigris::block_groups("NY", "Onondaga", year = 2010)

ow <- tigris::area_water("NY", "Onondaga", year = 2015)
om <- tigris::county_subdivisions("NY", "Onondaga", year = 2010)
ol <- tigris::landmarks("NY", "point", year = 2015)
t <- sf::st_contains(oc, ol) |> unlist()
  ol <- ol[t, ]
op <- tigris::landmarks("NY", "area", year = 2015)
  t <- sf::st_contains(oc, op) |> unlist()
  op <- op[t, ]

sf::write_sf(ot, dsn = "testing", driver = "ESRI shapefile",
             layer = "onondaga_tracts")
sf::write_sf(obg, dsn = "testing", driver = "ESRI shapefile",
             layer = "onondaga_blockgroups")
sf::write_sf(ow, dsn = "testing", driver = "ESRI shapefile",
             layer = "onondaga_water")
sf::write_sf(om, dsn = "testing", driver = "ESRI shapefile",
             layer = "onondaga_mcd")
sf::write_sf(ol, dsn = "testing", driver = "ESRI shapefile",
             layer = "onondaga_landmarkpoints")
sf::write_sf(op, dsn = "testing", driver = "ESRI shapefile",
             layer = "onondaga_landmarkpolygons")


# no native areas in Onondaga County
# n <- tigris::states(year = 2010) |>
#   dplyr::filter(STATEFP10 == 36)
# or <- tigris::native_areas(year = 2015)
# t <- sf::st_contains(n, or) |> unlist()
# or <- or[t, ]


