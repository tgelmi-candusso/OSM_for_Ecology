#function to convert the output of our framework into a wildlife-specific urbanization index


LULCtoUI <- function(osm_enhanced_LULC){
  
  osm_landcover <- OSM_enhanced_LULC_map
  
  osm_landcover <- project(osm_landcover, "EPSG: 5070")
  #proportion of building polygons 
  buildings <- osm_landcover == 14
  built_prop <- aggregate(buildings, fact=15, fun="mean") #500m
  
  #proportion of wild areas
  forest <- osm_landcover == 11
  wild_prop <- aggregate(forest, fact=30, fun="mean") #fact = factor of multiplication for cell size e.g. 10 = 10*30m = 300x300m
  wild_prop_neg <- 1-wild_prop
  wild_prop_neg <- disagg(wild_prop_neg, fact=2)
  wild_prop_neg1 <-project(wild_prop_neg, built_prop)
  
  #proportion of green bushy areas
  bushy <- osm_landcover == 9
  bushy_prop <- aggregate(bushy, fact=30, fun="mean") #fact = factor of multiplication for cell size e.g. 10 = 10*30m = 300x300m
  bushy_prop_neg <- (1-bushy_prop)/2
  bushy_prop_neg <- disagg(bushy_prop_neg, fact=2)
  bushy_prop_neg1 <-project(bushy_prop_neg, built_prop)
  
  #proportion of green open areas
  grass <- osm_landcover == 6
  grass_prop <- aggregate(grass, fact=30, fun="mean") #fact = factor of multiplication for cell size e.g. 10 = 10*30m = 300x300m
  grass_prop_neg <- (1 - grass_prop)/4
  grass_prop_neg <- disagg(grass_prop_neg, fact=2)
  grass_prop_neg1 <-project(grass_prop_neg, built_prop)
  
  #
  #proportion of roads to check road classes
  roads <- osm_landcover == c(15,21) #c(13:25) #all classes
  road_density <- aggregate(roads, fact=10, fun="mean") #aggregate proportion fact = xcell size
  road_density <-project(road_density, built_prop)
  road1<-sum(road_density)*100 #resistance proportional to the density of roads  in the cell
  plot(road1)
  #
  
  #distance to water
  water_class <- c(12)
  water_na <-terra::ifel(osm_landcover %in% water_class, 1, NA)
  dist_to_water<-terra::distance(water_na)
  dist_to_wat<-project(dist_to_water,built_prop) #reproject for distances
  nx <- minmax(dist_to_wat)    #standardize part 1
  dist_water1 <- (dist_to_wat - nx[1,]) / (nx[2,] - nx[1,]) #standardize part 2
  
  #distance to roads
  class <- c(15,21) #linear_features
  class_na <-terra::ifel(osm_landcover %in% class, 1, NA)
  dist_to_class<-terra::distance(class_na)
  
  #reproject and mask water before standardization
  dist_to_class1<-project(dist_to_class,built_prop) #reproject for standardizing
  water_1 <-terra::ifel(osm_landcover == 12, NA, 1)
  water_1 <- project(water_1, built_prop)
  dist_to_class2 <- mask(dist_to_class1, water_1)
  dist_to_class2<- project(dist_to_class2,built_prop)
  nx <- minmax(dist_to_class2)    
  dist_lft <- (dist_to_class - nx[1,]) / (nx[2,] - nx[1,])
  mask_open_water <- dist_lft >1
  msk <-ifel(dist_lft >1, NA, dist_lft)
  dist_to_mainroads_neg <- 1-msk
  dist_to_mainroads_neg1<-(resample(dist_to_mainroads_neg, dist_water1))
  
  #distance to railways
  class <- c(25) #rail_features
  class_na <-terra::ifel(osm_landcover %in% class, 1, NA)
  dist_to_class<-terra::distance(class_na)
  dist_to_class1<-project(dist_to_class,built_prop) #reproject for standardizing
  #mask water before standardization
  water_1 <-terra::ifel(osm_landcover == 12, NA, 1)
  water_1 <- project(water_1, built_prop)
  dist_to_class2 <- mask(dist_to_class1, water_1)
  dist_to_class2<- project(dist_to_class2,built_prop)
  # standardize 
  nx <- minmax(dist_to_class2)    
  dist_lft <- (dist_to_class - nx[1,]) / (nx[2,] - nx[1,])
  mask_open_water <- dist_lft >1
  msk <-ifel(dist_lft >1, NA, dist_lft)
  dist_to_railways_neg <- 1-msk
  dist_to_railways_neg1<-(resample(dist_to_railways_neg, dist_water1))
  
  
  #compile urbanization index
  HIF_tgc1<-(dist_to_mainroads_neg1 + dist_water1 + built_prop + wild_prop_neg1 + grass_prop_neg1  + bushy_prop_neg1 + dist_to_railways_neg1)
  water_0 <-terra::ifel(osm_landcover == 12, 0, NA)
  water_0 <- project(water_0, HIF_tgc1)
  HIF_tgc1<-terra::cover(water_0, HIF_tgc1)
  HIF_tgc1 <-terra::ifel(HIF_tgc1 <= 0, NA, HIF_tgc1)
  HIF_tgc1st <- (HIF_tgc1 - 1) / (5 - 1) #changed min from Chicago values to avoid negatives across cities - 1.8) / (5 - 1.8) #these ranges were from Chicago and may on negatives 
  HIF_tgc1st<- HIF_tgc1st*100
  
  return(HIF_tgc1st)
}


