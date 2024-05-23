#################################
#   OSM for ecology - demo   ####
#  Gelmi-Candusso and Rodriguez #
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
source("OSM_to_LULC_functions.R")

#==================================
# Step 1: Download/Load OSM database
#==================================

# Load the characterized key values (Table S4) we provided in the manuscript so those features are all downloaded by the oe_get() or oe_read() orfunction

osm_kv <- read.csv("urban_features/osm_key_values.csv") #table with the key-value pairs to be extracted I nded up doing it by hand though, so we can hard code it
osm_kv <- osm_kv %>% filter(!is.na(key))
keys <- unique(osm_kv$key)

# Download database for both polygons and lines.
# This function queries and downloads a database file based on the result of that query. 
# The query can be either through a city name, a region, or a bounding box 
# If the specific name queried is not found, the function will suggest the closest result found, and suggest a database file which it will download. This result might not be the one you meant. 
# For regions queries like "us/illinois", "us/washington" work well. 
# An alternative is to predownload the database file from geofabrik, store it and call it directly using the oe_get() function, the code for this is in the "Alt_Step1_with_predownloaded_database.R" script. 
# For using bounding boxes as query I will leave here, and comment out an example to download Chicago.

#crop study area directly here using the 'clipsrc' boundary type

#Chicago
bbox_crs <- 4269
study_area_bbox <- st_bbox(ext(c(-88.38422408, -87.45238124, 41.49280305, 42.34485453)), crs=st_crs(bbox_crs)) #3857
#change crs of bbox to 4326
study_area_bbox <- study_area_bbox %>%
  st_as_sfc() %>%
  st_transform(crs = 4326) %>%
  st_bbox()

pol_feat <- osmextract::oe_get(place = "us/illinois", #can also use place = study_area_bbox to query for databases containing the study area
                               boundary = study_area_bbox,
                               boundary_type = 'clipsrc',
                               #boundary = sf::st_bbox(c(xmin = 11.23602, ymin = 47.80478, xmax = 11.88867, ymax = 48.24261)), #must be in crs=4326 or specified in the st_bbox object
                               provider ="geofabrik",
                               layer = "multipolygons",
                               stringsAsFactors = FALSE, 
                               quiet = FALSE,
                              #OSMEXT_DOWNLOAD_DIRECTORY=/path/to/osm/data
                              extra_tags=keys)

lin_feat <- osmextract::oe_get("us/illinois",
                                layer = "lines", 
                               boundary = study_area_bbox,
                               boundary_type = 'clipsrc',
                               stringsAsFactors = FALSE, 
                               extra_tags=keys)

#save rds so you dont have to download each time
saveRDS(pol_feat, "OSM_polygon_features.rds")
saveRDS(lin_feat, "OSM_linear_features.rds")


# pol_feat <- readRDS("OSM_polygon_features.rds")
# lin_feat <- readRDS("OSM_linear_features.rds")

#================================
# Step 2: Categorize OSM features
#================================

#filter OSM features from Table S4 and categorize into classes. These classes will represent the classes in our LULC map
#The code within the function here can be used to extract OSM features relevant to ones research topic

#read function

vlayers <- OSMtoLULC_vlayers(OSM_polygon_layer = pol_feat, 
                             OSM_line_layer = lin_feat)

#=======================================
# Step 3: Convert all classes to rasters
#=======================================

#extent <- st_bbox(ext(pol_feat))

#run function
rlayers <- OSMtoLULC_rlayers(OSM_LULC_vlayers = vlayers,
                             study_area_extent = study_area_bbox)



#=================================
# Step 4: Stack & collapse rasters
#=================================
# Collapse list of individual rasters based on order of elements in list.

OSM_only_map <- merge_OSM_LULC_layers(OSM_raster_layers = rlayers)

plot(OSM_only_map==14)

#=========================================================
# Step 5: Integrate OSM features into Global landcover map 
#=========================================================

#load the global landcover map raster
CEC_map <- rast("E:/cec_v2/Land_cover_2015v2_30m_TIF/NA_NALCMS_landcover_2015v2_30m/data/NA_NALCMS_landcover_2015v2_30m.tif")
#CEC_map <- rast("global_landcover_maps/Global_LULC_map_CEC_cropped_Washington.tif") 

#generate reclassification table, consisting of two columns, one with the global lulc code, and one with the corresponding OSM lulc code
reclass_values <- read.csv("reclass_tables/reclass_cec_2_mcsc.csv") #extract only column values and convert as a matrix to use for reclassifying the global map into our landcover classes.
CEC_to_OSM_table <- reclass_values %>% dplyr::select(cec_value, osm_value)

OSM_enhanced_LULC_map <- integrate_OSM_to_globalLULC(OSM_lulc_map = OSM_only_map,
                                                     global_lulc_map = CEC_map, 
                                                     reclass_table = CEC_to_OSM_table)


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
                             "#FDDB87", "#36454f"),
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
                             "barriers"
                    )) +
  theme_void() +
  theme(legend.position = "right")+
  coord_equal() 

terra::writeRaster(OSM_enahnced_LULC_map, "augmented_cec_lcover.tif", overwrite=TRUE)
#plot(r6, type="classes")


