pkgs <- c("tidyverse", "XML", "sf", "basemaps", "tmap", "ggplot2")

lapply(pkgs, library, character.only = TRUE, verbose = FALSE)

# Read in DPO regions
rnames <- read.csv("PDO_EU_id.csv")
regions <- st_read("EU_PDO.gpkg") %>%
  left_join(rnames[, c("Country", "PDOid", "PDOnam")], by = "PDOid") %>%
  filter(Country %in% c("FR", "IT"))

# Read in stage names
stages <- read.csv("2024 Stages.csv")

r.proj <- st_transform(regions, 3035)

# Download TDF stage route
for (ii in 1:21) {
  download.file(
    paste0("https://cdn.cyclingstage.com/images/tour-de-france/2024/stage-",
    ii,
    "-route.gpx"),
    paste0("stage-", ii, "-route.gpx"))
}

# Convert gpx to spatial lines
route <- lapply(1:21, function(s) {
  coords <- xpathSApply(doc = htmlTreeParse(paste0("stage-", s, "-route.gpx"),
                                            useInternalNodes = TRUE),
                        path = "//trkpt",
                        fun = xmlAttrs)
  df <- data.frame(
    x = coords["lon", ],
    y = coords["lat", ])
  
  sf <- st_as_sf(df, coords = c("x", "y")) %>%
    st_set_crs(4326) %>%
    st_transform(st_crs(regions)) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  return(sf)
})

# Identify DPO's on stage route
route.dpo <- lapply(route, function(sf) {
  int <- st_intersection(sf, regions)
  
  if (nrow(int) == 0) {
    sf.buff <- st_transform(sf, st_crs(r.proj)) %>%
      st_buffer(10000)
    int <- st_intersection(sf.buff, r.proj)
  } 
  
  if (nrow(int) == 0) {
    sf.buff <- st_transform(sf, st_crs(r.proj)) %>%
      st_buffer(30000)
    int <- st_intersection(sf.buff, r.proj)
  } 
  
  if (nrow(int) == 0) {
    sf.buff <- st_transform(sf, st_crs(r.proj)) %>%
      st_buffer(50000)
    int <- st_intersection(sf.buff, r.proj)
  }
  
  if (nrow(int) == 0) {
    sf.buff <- st_transform(sf, st_crs(r.proj)) %>%
      st_buffer(80000)
    int <- st_intersection(sf.buff, r.proj)
  }
  return(int)
})

# Attaching stage number to results
names(route.dpo) <- paste("Stage", 1:21)

for (ii in 1:21) {
  route.dpo[[ii]]$stage <- ii
}

# Mapping DPOs of each stage
lapply(1:21, function(ii) { 
  # Transforming route to match basemap projection
  r <- route[[ii]] %>%
    st_transform(3857)
  
  # Creating bbox for basemap creation
  x <- st_as_sfc(st_bbox(r)) %>%
    st_transform(st_crs(r.proj)) %>%
    st_buffer(30000)
  x <- st_as_sfc(st_bbox(x)) %>%
    st_transform(3857)
  
  # Downloading basemaps
  bm <- basemap_stars(ext = x,
                      map_service = "carto",
                      map_type = "voyager")
  bml <- basemap_stars(ext = x,
                       map_service = "carto",
                       map_type = "voyager_only_labels")
  
  # Subsetting DPO region polygons for those in the route area
  p <- regions %>%
    filter(PDOid %in% route.dpo[[ii]]$PDOid) %>%
    st_transform(3857) %>%
    st_crop(x)
  
  # Plot route and DPOs over basemap
  m <- tm_shape(bm) +
    tm_rgb() +
    tm_shape(r, bbox = st_bbox(x)) +
    tm_lines() +
    tm_shape(p) +
    tm_fill("PDOnam", 
            title = "Protected Designation of Origins",
            alpha = 0.5) +
    tm_borders(col = "grey50") +
    tm_shape(r, bbox = st_bbox(x)) +
    tm_lines() +
    tm_shape(bml) +
    tm_rgb() +
    tm_layout(main.title = paste("Étage", ii),
              main.title.position = c("center", "top"),
              title = stages[ii, 2],
              title.position = c("center", "top"),
              legend.outside = TRUE,
              legend.position = c("center", "bottom"))
  
  png(filename = paste0("Stage ", ii,".png"),
      height = 8,
      width = 8,
      res = 300,
      units = "in")
  print(m)
  dev.off()
  }
)




