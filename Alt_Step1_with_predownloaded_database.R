#Alternative Step 1 when database has been pre-downloaded

lin_feat <- oe_read("D:/geofabrik files/north-america-latest.osm.pbf",
                    layer = "lines", 
                    extra_tags=keys,
                    boundary = st_bbox(c(xmin=-8898187.3461,ymin=5386652.6503,xmax=-8807991.6527,ymax=5454509.2032), crs=st_crs(3857)), #Toronto WGs EPSG 3857, got the boundaries from: http://bboxfinder.com/
                    boundary_type = "spat")


pol_feat <- oe_read("D:/geofabrik files/north-america-latest.osm.pbf",
                       layer = "multipolygons", 
                       extra_tags=keys,
                       boundary = st_bbox(c(xmin=-8898187.3461,ymin=5386652.6503,xmax=-8807991.6527,ymax=5454509.2032), crs=st_crs(3857)), #Toronto WGs EPSG 3857, got the boundaries from: http://bboxfinder.com/
                       boundary_type = "spat")
