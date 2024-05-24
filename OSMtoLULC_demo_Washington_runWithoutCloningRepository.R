#################################
#   OSM for ecology - demo   ####
#   Gelmi-Candusso et al.    ####
#       for MEE review       ####
#################################

#### Notes

# Demo to characterize the landscape using the OSM database. 
# The demo follows the steps described in the manuscript to characterize 
# the landscape of Washington DC, US, named in the manuscript as an OSM-enhanced LULC map.

# Our repository includes another demo script to run this map for any study area using a specified bounding box.


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
#source("OSM_to_LULC_functions.R") #use this if repository has been cloned

#==================================
# Step 1: Download/Load OSM database
#==================================

# Load the characterized key values we provide in our manuscript (Table S4) 
# in order to include all features required, 
# to generate a complete landscape characterization, 
# during the conversion from database file to vector file.

#table with the key-value pairs to be extracted 
osm_kv <- read_csv(
  "https://raw.githubusercontent.com/tgelmi-candusso/OSM_for_Ecology/main/urban_features/osm_key_values.csv"
) 
osm_kv <- osm_kv %>% 
  filter(!is.na(key))
keys <- unique(osm_kv$key)

# Download and convert database for both polygon and linear features 
# if you have not done so already, 
# otherwise read in the already saved files. 

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
# These classes will represent the classes in our OSM-enhanced map
# Note: The code within the function here can be used to extract OSM features 
# relevant to specific research topic (e.g. extract (filter for) golf courses or cemeteries only). 
# See Figure 1, Step 2, for details on how to filter other specific OSM features and Table S4 for specific features and their key-value pairs.

vlayers <- OSMtoLULC_vlayers(
  OSM_polygon_layer = pol_feat, 
  OSM_line_layer = lin_feat
)

#=======================================
# Step 3: Convert all classes to rasters
#=======================================

#here we convert all the filtered OSM features into raster layers. 
# We do this for each layer separately. 
# Within the function We convert linear features into polygons 
# using a buffer function and the specific buffer size described in Table S3.
# To rasterize we generate a raster template using the extent of the study area downloaded in step 1.

#define the extent of study area
extent <- as.vector(ext(pol_feat))

#run function
rlayers <- OSMtoLULC_rlayers(
  OSM_LULC_vlayers = vlayers,
  study_area_extent = extent
)


#=================================
# Step 4: Stack & collapse rasters
#=================================

#Here we merge all layers into one raster layer, by overlaying rasters following their priority. 
# We defined priority of each layer to represent movement barriers for wildlife. 
# E.g. road features over water features to maintain bridges in the landscape

OSM_only_map <- merge_OSM_LULC_layers(
  OSM_raster_layers = rlayers
)

# This map includes ONLY OSM features, 
# this is the reference map we used in the manuscript to analyze completeness of our framework
plot(OSM_only_map) 

#=========================================================
# Step 5: Integrate OSM features into Global landcover map 
#=========================================================

# The OSM database is community based in current expansion, and may still have gaps of information in certain areas. 
# We assess gaps in information in the manuscript in our completeness analysis. 
# To ensure we dont have any gaps in the final output of the framework we integrate the 
# OSM-only map on to a global or continental land cover map, based on the cities used. 
# In this case we use the CEC land cover map as a background, by reclassifying it into our classification system
# and filling any NA cells with the information provided in the reclassified CEC map.

#load the global or continental landcover map raster
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

# generate reclassification table, consisting of two columns, 
# one with the global lulc code, and one with the corresponding 
# land cover class in our framework.
# Here we load a table containing this values and 
# select the columns required for the reclassification.

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

#===========================================================
# Framework output: OSM-enhanced landscape characterization 
#===========================================================

# this is the final output of our framework. 
# We used in the manuscript to analyze accuracy of our framework
plot(OSM_enhanced_LULC_map)

#Here we provide code to plot the OSM-enhanced map using a color map
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
                    breaks = c(1:28),
                    labels=c("industrial", "commercial", "institutional","residential","landuse_railway",
                             "open green", "protected area", "resourceful green area","heterogeneous green area", "barren soil","dense green area",
                             "water",
                             "parking surface", "buildings",
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
                             "developed_na")) +
  theme_void() +
  theme(legend.position = "right")+
  coord_equal() 

#Export the raster for further use, 
# for example as the basis for our urbanization index, 
# or for habitat selection analysis, connectivity assessments and other potential applications described in our manuscript.

terra::writeRaster(
  OSM_enahnced_LULC_map,
  "Washington_OSM-enhanced_lcover_map.tif",
  overwrite=TRUE
)

