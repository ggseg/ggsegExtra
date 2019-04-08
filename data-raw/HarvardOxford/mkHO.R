# Experiments turning raster images into polygons
# Need to go back and split it left/right
library(tidyverse)
library(raster)
library(stars)
library(sf)
library(rmapshaper)

#pics <- list.files(pattern=".+_1\\.png", path="PicsDesikan/", full.names = TRUE)
pics <- list.files(pattern="^.h_.+\\.tif", path="PicsHarvardOxford/", full.names = TRUE)

region <- basename(pics)
region <- stringr::str_remove(region, "\\.tif")
origlabel <- region

hemi <- stringr::str_extract(region, "^.h")
region <- stringr::str_remove(region, "^.h_")
side <- stringr::str_extract(region, "...$")
region <- stringr::str_remove(region, "_...$")
origlabel <- stringr::str_remove(origlabel, "_...$")
ho.df <- tibble(area=region, hemi=hemi, side=side, label=origlabel)
ho.df <- mutate(ho.df, area=stringr::str_replace_all(area, "\\.+", " "))

rasterobjs <- map(pics, raster)
map_dbl(rasterobjs, cellStats, stat=max)

## check the maximum value
cellStats(rasterobjs[[1]], stat = max)
mkContours <- function(rstobj){
  mx <- cellStats(rstobj, stat=max)
  # Filter out the blank images
  if (mx < 200) {
    return(NULL)
  }
  tmp.rst <- rstobj
  tmp.rst[tmp.rst == 0] <- NA
  
  ## levels = 50 is to remove the occasional edge point that has
  ## non zero hue.
  #cntr <- raster::rasterToPolygons(rstobj, fun = function(X)X>100, dissolve=TRUE)
  g <- st_as_sf(st_as_stars(tmp.rst), merge=TRUE, connect8=TRUE)
  ## Is it a multipolygon? Keep the biggest bit
  ## Small parts are usually corner connected single voxels
  if (nrow(g)>1) {
    gpa <- st_area(g)
    biggest <- which.max(gpa)
    g <- g[biggest,]
  }
  g <-st_sf(g)
  names(g)[[1]] <- "region"
  g$region <- names(rstobj)
  return(g)

}


contourobjs <- map(rasterobjs, mkContours)
kp <- !map_lgl(contourobjs, is.null)

contourobjsDF <- do.call(rbind, contourobjs)


ho.df <- filter(ho.df, kp)
ho.df <- bind_cols(contourobjsDF, ho.df)
## Now we need to place them into their own panes
## Bounding box for all
bball <- st_bbox(ho.df)
ho.df <-  mutate(ho.df, geometry=geometry - bball[c("xmin", "ymin")])

## ifelse approach doesn't seem to work, so split it up
ho.dfA <- ho.df %>% 
  filter(hemi=="lh", side=="med") %>% 
  mutate(geometry=geometry+c(600,0))

ho.dfB <- ho.df %>% 
  filter(hemi=="rh", side=="med") %>% 
  mutate(geometry=geometry+c(2*600,0))

ho.dfC <- ho.df %>% 
  filter(hemi=="rh", side=="lat") %>% 
  mutate(geometry=geometry+c(3*600,0))

ho.dfD <- ho.df %>% 
  filter(hemi=="lh", side=="lat")
  
ho.df.panes <- rbind(ho.dfD, ho.dfA, ho.dfB, ho.dfC)
#ho.df.panes.simple <- st_simplify(ho.df.panes, preserveTopology = TRUE, dTolerance=0.75)
ho.df.panes.simple <- rmapshaper::ms_simplify(ho.df.panes)

plot(ho.df.panes.simple)

library(ggseg)
library(ggsegExtra)

## Not sure whether the range of values really matters. The other atlases look like they
## may be giving the coordinates in physical units of some sort.
## Lets pretend each picture is 10cm square. Divide point values by 60 at the end.

ho.df.final <- mutate(ho.df.panes.simple, 
                      id=1:nrow(ho.df.panes.simple),
                      coords = map(geometry, ~(st_coordinates(.x)[, c("X", "Y")])),
                      coords = map(coords, as.tibble),
                      coords = map(coords, ~mutate(.x, order=1:nrow(.x))))
ho.df.final$geometry <- NULL
ho.df.final <- unnest(ho.df.final, .drop=TRUE)
ho.df.final <- rename(ho.df.final, long=X, lat=Y)
ggseg(atlas=ho.df.final, mapping=aes(fill=area), color="white") + theme(legend.position = "none")

save(ho.df.panes.simple, ho.df.final, file="ho_atlases.Rda")
