require(stars)
require(starsTileServer)
s5p <- system.file(
  "sentinel5p/S5P_NRTI_L2__NO2____20180717T120113_20180717T120613_03932_01_010002_20180717T125231.nc",
  package = "starsdata"
)
nit <- read_stars(
  s5p,
  along = NA,
  sub = c(
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_total_column",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_total_column_precision",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_total_column_precision_kernel",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/number_of_iterations",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/number_of_spectral_points_in_retrieval",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/oxygen_oxygen_dimer_slant_column_density",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/oxygen_oxygen_dimer_slant_column_density_precision",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ozone_slant_column_density",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ozone_slant_column_density_precision",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/processing_quality_flags",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ring_coefficient",
    "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/ring_coefficient_precision"
  ),
  curvilinear = c("//PRODUCT/longitude", "//PRODUCT/latitude"),
  driver = NULL
)
names(nit) <-
  sub("//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/", "", names(nit))
for (i in seq(length(names(nit)))) {
  nit[[i]][nit[[i]] > 9e+36] <- NA
}
st_crs(nit) <- 4326

server <- starsTileServer$new(nit)
# we save the server here as there should only be one version (sampling of color scales would otherwise result in differently colored tiles)
saveRDS(server, "server.rds")
