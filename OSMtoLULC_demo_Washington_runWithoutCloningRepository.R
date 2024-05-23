#################################
#   OSM for ecology - demo   ####
#   Gelmi-Candusso et al.    ####
#       for MEE review       ####
#################################

#### Notes

# Quick demo to generate an LULC map from the OSM database and integrate it into a global landcover map. 
# Cropped global landcover map is provided due to storage limitations

# In the demo we will generate an OSM-enhanced LULC map for Washington, which has is available as a stored available for the city alone, making computational requirements suitable for this demo.
# In the event of requiring a specific city, we recommend downloading the regional or continental OSM database from geofabrik and specifying a bounding box, as we describe in the manuscript, replace Step 1 below with script from "Alt_Step1_with_predownloaded_database"


## libraries

library(osmextract)
library(tidyterra)
library(dplyr)
library(terra)
library(sf)
library(readr)
library(devtools)
library(ggplot2)
devtools::source_url(
  "https://raw.githubusercontent.com/tgelmi-candusso/OSM_for_Ecology/main/OSM_to_LULC_functions.R"
)
#source("OSM_to_LULC_functions.R")

#==================================
# Step 1: Download/Load OSM database
#==================================

# Load the characterized key values (Table S4) we provided in the manuscript so 
#  those features are all downloaded by the oe_get() or oe_read() orfunction

#table with the key-value pairs to be extracted 
osm_kv <- read_csv(
  "https://raw.githubusercontent.com/tgelmi-candusso/OSM_for_Ecology/main/urban_features/osm_key_values.csv"
) 
osm_kv <- osm_kv %>% 
  filter(!is.na(key))
keys <- unique(osm_kv$key)

# Download database for both polygons and lines if you
#  have not done so already, otherwise read in
#  the already saved files.

if(
  !file.exists("OSM_polygon_features.rds")
){
  pol_feat <- osmextract::oe_get(
    "Washington",
    provider="geofabrik",
    layer = "multipolygons", 
    extra_tags=keys
  )
  saveRDS(
    pol_feat,
    "OSM_polygon_features.rds"
  )
} else {
  pol_feat <- readRDS(
    "OSM_polygon_features.rds"
  )
}

if(
  !file.exists("OSM_linear_features.rds")
) {
  lin_feat <- osmextract::oe_get(
    "Washington",
    layer = "lines", 
    extra_tags=keys
  )
  saveRDS(
    lin_feat,
    "OSM_linear_features.rds"
  )
} else {
  lin_feat <- readRDS(
    "OSM_linear_features.rds"
  )
}

#================================
# Step 2: Categorize OSM features
#================================

# Filter OSM features from Table S4 and categorize into classes. 
# These classes will represent the classes in our LULC map
#The code within the function here can be used to extract OSM features 
# relevant to ones research topic

vlayers <- OSMtoLULC_vlayers(
  OSM_polygon_layer = pol_feat, 
  OSM_line_layer = lin_feat
)

#=======================================
# Step 3: Convert all classes to rasters
#=======================================

extent <- as.vector(ext(pol_feat))

#run function
rlayers <- OSMtoLULC_rlayers(
  OSM_LULC_vlayers = vlayers,
  study_area_extent = extent
)


#=================================
# Step 4: Stack & collapse rasters
#=================================
# Collapse list of individual rasters based on order of elements in list.

OSM_only_map <- merge_OSM_LULC_layers(
  OSM_raster_layers = rlayers
)

plot(OSM_only_map)

#=========================================================
# Step 5: Integrate OSM features into Global landcover map 
#=========================================================

#load the global landcover map raster
url <- "https://github.com/tgelmi-candusso/OSM_for_Ecology/raw/main/global_landcover_maps/Global_LULC_map_CEC_cropped_Washington.img"
download.file(
  url,
  destfile = "Global_LULC_map_CEC_cropped_Washington.img",
  mode='wb'
)
CEC_map <- rast(
  "Global_LULC_map_CEC_cropped_Washington.img"
) 
plot(CEC_map)

#generate reclassification table, consisting of two columns, 
# one with the global lulc code, and one with the corresponding OSM lulc code
reclass_values <- read_csv(
  "https://raw.githubusercontent.com/tgelmi-candusso/OSM_for_Ecology/main/reclass_tables/reclass_cec_2_mcsc.csv"
)

CEC_to_OSM_table <- reclass_values %>% 
  dplyr::select(cec_value, osm_value)

OSM_enhanced_LULC_map <- integrate_OSM_to_globalLULC(
  OSM_lulc_map = OSM_only_map,
  global_lulc_map = CEC_map, 
  reclass_table = CEC_to_OSM_table
)


#plot OSM-enhanced landcover map

plot(OSM_enhanced_LULC_map)

ggplot(data = OSM_enhanced_LULC_map) +
  geom_raster(aes(x = x, y = y, fill = first)) +
  scale_fill_manual(values=c("#843438","#df919a",	"#F88A50", "#EC5C3B","#FEF3AC",
                             "#D4ED88","#AFDC70", "#83C966", "#51B25D","#d19c5f", "#1A9850",
                             "#088da5",
                             "#b0b0b0", "#000000",
                             "#ff580f", "#ce7e00",
                             "#ffde1a","#ffce00","#ffa700","#ff8d00", 
                             "#ff7e26", "#ff7400",
                             "#FDB768", "#783F04",
                             "#FEF3AC", "#AD6A24",
                             "#FDDB87", "#400000"),
                    labels=c("industrial", "commercial", "institutional","residential","landuse_railway",
                             "open green", "protected area", "resourceful green area","heterogeneous green area", "barren soil","dense green area",
                             "water",
                             "parking surface", "building",
                             "roads (v.h. traffic)",
                             "sidewalks",
                             "roads_na",
                             "roads (v.l. traffic)",
                             "roads (l. traffic)",
                             "roads (m. traffic)",
                             "roads (h.t.l.s)",
                             "roads (h.t.h.s)",
                             "trams/streetcars",
                             "hiking trails",
                             "railways",
                             "unused linear feature",
                             "barriers",
                             "developed_na"
                    )) +
  theme_void() +
  theme(legend.position = "right")+
  coord_equal() 
freq(OSM_enhanced_LULC_map)
terra::writeRaster(
  OSM_enahnced_LULC_map,
  "Washington_OSM-enhanced_lcover_map.tif",
  overwrite=TRUE
)

