pkgs <- c("tidyverse", "XML", "sf", "sfheaders", "basemaps", "tmap", "ggplot2")

lapply(pkgs, library, character.only = TRUE, verbose = FALSE)

# Read in AOC regions (France only)
aoc <- st_read("2024-06-10_delim-parcellaire-aoc-shp.shp") %>%
  st_transform(3035)

# # Aggregating to 'denom' column geometry
# aoc.agg <- aoc %>%
#   group_by(denom) %>%
#   summarize(do_union = TRUE)

aoc.app <- split(aoc, f = aoc$app)
aoc.app <- lapply(seq_along(aoc.app), function(ii) {
  x <- aoc.app[[ii]]
  
  y <- list()
  for (jj in seq_len(nrow(x))) {
    y[[jj]] <- x[jj, ] %>%
      st_buffer(50) %>%
      sf_remove_holes(close = TRUE)
  }
  z <- do.call(rbind, y) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(app = names(aoc.app)[ii]) %>%
    rename(geometry = x)
    
  return(z)
})

aoc.app <- bind_rows(aoc.app)

# Read in stage names
stages <- read.csv("2025 Stages.csv")

# # 2024 .gpx file source
# route <- 
# 
# temp <- tempfile()
# download.file(paste0("https://cdn.cyclingstage.com/images/tour-de-france/2025/stage-",
#                      ii,
#                      "-route.gpx"),
#               temp)

# Download, read, and transform 2025 stage routes
route <- lapply(seq_len(nrow(stages)), function(s) {
  return(
    st_read(paste0("https://www.visugpx.com/download.php?id=mRD0shgw6d&t=", s), layer = "tracks") %>%
      st_set_crs(4326) %>%
      st_transform(st_crs(aoc.app))
  )
  # Adding sleep time to avoid 503 error
  Sys.sleep(10)
})

# Identify AOCs on stage route
route.aoc <- lapply(route, function(sf) {
  int <- st_intersection(sf, aoc.app) %>%
    mutate(dist = 0)
  
  if (nrow(int) == 0) {
    sf.buff <- st_buffer(sf, 10000)
    int <- st_intersection(sf.buff, aoc.app) %>%
      mutate(dist = 10)
  } 
  
  if (nrow(int) == 0) {
    sf.buff <- st_buffer(sf, 30000)
    int <- st_intersection(sf.buff, aoc.app) %>%
      mutate(dist = 30)
  } 
  
  if (nrow(int) == 0) {
    sf.buff <- st_buffer(sf, 50000)
    int <- st_intersection(sf.buff, aoc.app) %>%
      mutate(dist = 50)
  }
  
  if (nrow(int) == 0) {
    sf.buff <- st_buffer(sf, 80000)
    int <- st_intersection(sf.buff, aoc.app) %>%
      mutate(dist = 80)
  }
  return(int)
})

# Attaching stage number to results
names(route.aoc) <- paste("Stage", seq_len(nrow(stages)))

# Checking how many AOCs cross each stage
table(unlist(lapply(route.aoc, nrow)))

maps25 <- lapply(seq_along(route), function(ii) {
  r <- route[[ii]] %>%
    st_transform(3857)
  
  coord <- st_coordinates(r)
  
  # Points of departure and arrival
  dep <- st_sfc(st_point(coord[1, ]), crs = st_crs(r))
  arr <- st_sfc(st_point(coord[nrow(coord), ]), crs = st_crs(r))
  
  # Creating bbox for basemap creation
  if (nrow(route.aoc[[ii]]) > 0) {
    x <- st_as_sfc(st_bbox(r)) %>%
      st_buffer(max(route.aoc[[ii]]$dist) * 1000 + 10000) 
  } else {
    x <- st_as_sfc(st_bbox(r)) %>%
      st_buffer(10000)
  }
  
  # Downloading basemaps
  bm <- basemap_stars(ext = x,
                      map_service = "carto",
                      map_type = "voyager")
  bml <- basemap_stars(ext = x,
                       map_service = "carto",
                       map_type = "voyager_only_labels")
  
  # Subsetting DPO region polygons for those in the route area
  p <- aoc.app %>%
    filter(app %in% route.aoc[[ii]]$app) %>%
    st_transform(3857) %>%
    st_crop(x)
  
  if (nrow(p) > 0) {
    # Plot route and DPOs over basemap
    m <- tm_shape(bm, unit = "mi") +
      tm_rgb() +
      tm_shape(r, bbox = st_bbox(x)) +
      tm_lines(lwd = 2) +
      tm_shape(p) +
      tm_polygons(fill = "app",
                  fill.scale = tm_scale_categorical(),
                  col = NULL,
                  fill_alpha = 0.9,
                  fill.legend = tm_legend(title = "Appellation d'Origen Contrôllée",
                                          position = tm_pos_out("right", "center"))) +
      tm_shape(r, bbox = st_bbox(x)) +
      tm_lines() +
      tm_shape(dep) +
      tm_symbols(shape = 21, fill = "green4") +
      tm_shape(arr) +
      tm_symbols(shape = 21, fill = "red3") +
      tm_shape(bml) +
      tm_rgb() +
      tm_scalebar(position = tm_pos_out("right", "center")) +
      tm_title(paste("Étage", ii, ": ", stages$Cities[ii]),
               position = tm_pos_out("center", "top"))
  } else {
    # Plot route and DPOs over basemap
    m <- tm_shape(bm, unit = "mi") +
      tm_rgb() +
      tm_shape(r, bbox = st_bbox(x)) +
      tm_lines(lwd = 2) +
      tm_shape(r, bbox = st_bbox(x)) +
      tm_lines() +
      tm_shape(dep) +
      tm_symbols(shape = 21, fill = "green4") +
      tm_shape(arr) +
      tm_symbols(shape = 21, fill = "red3") +
      tm_shape(bml) +
      tm_rgb()  +
      tm_add_legend(type = "polygons",
                    fill = "white",
                    title = "Appellation d'Origen Contrôllée",
                    labels = "None",
                    position = tm_pos_out("right", "center")) +
      tm_scalebar(position = tm_pos_out("right", "center")) +
      tm_title(paste("Étage", ii, ": ", stages$Cities[ii]),
               position = tm_pos_out("center", "top"))
  }
  
  tmap_save(m,
            paste0("2025 Maps/Stage ", ii,".png"),
            width = 8, height = 8, dpi = 300, units = "in")
  
  return(m)
  })

saveRDS(maps25, "TDF2025_maps.rda")
maps25 <- readRDS("TDF2025_maps.rda")

tmap_mode("view")

maps25[[6]]
