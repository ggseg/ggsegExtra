library(magick)
library(raster)
library(stars)
library(sf)
library(rmapshaper)
library(neurosurf)
library(rgl)
library(here)
library(tidyverse)

setwd(paste0(here::here(), "/data-raw/SchaeferYeo"))

## The number of parcels in the atlas. Set to change to 400, 600, etc.
nparcels <- 200

## the amount of buffering to expand polygons slightly to close gaps
buffer_dist <- 1

## load left and right freesurfer6 inflated atlases
geometry_lh = neurosurf::read_surf("fsaverage6/lh.inflated")
geometry_rh = neurosurf::read_surf("fsaverage6/rh.inflated")

## read in left and right annotations for 17 network with nparcels
annot_lh=neurosurf::read_freesurfer_annot(
  paste0("annot/lh.Schaefer2018_", nparcels, "Parcels_17Networks_order.annot"), geometry_lh)
annot_rh=neurosurf::read_freesurfer_annot(
  paste0("annot/rh.Schaefer2018_", nparcels, "Parcels_17Networks_order.annot"), geometry_rh)


## function to convert ROI rasters to polygons
mkContours <- function(rst){
  rstobj = rst$rstobj
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
  g$hemi <- rst$hemi
  g$side <- rst$side
  return(g)

}


## function to create tiff images of ROIs
do_snapshots <- function(hemi, side) {
  if (hemi == "left") {
    geom  <- neurosurf::read_surf(paste0("fsaverage6/lh.inflated"))
    annot <- neurosurf::read_freesurfer_annot(paste0("annot/lh.Schaefer2018_", nparcels, "Parcels_17Networks_order.annot"), geom)
  } else {
    geom  <- neurosurf::read_surf("fsaverage6/rh.inflated")
    annot <- neurosurf::read_freesurfer_annot(paste0("annot/rh.Schaefer2018_", nparcels, "Parcels_17Networks_order.annot"), geom)
  }

  ids <- sort(unique(annot@data))
  ret <- vector(length(ids), mode="list")

  for (id in ids) {
    ## create an roi conisting of all surfaces nodes == id
    roi = annot == id
    neurosurf::plot(roi, irange=c(0,1), threshold=c(-100,.3), bgcol="seashell4", cmap = "#00FF00FF", view=side)
    fname1 <- tempfile(fileext=".png")

    rgl.snapshot(filename=fname1)
    rgl.clear()
    im=magick::image_read(fname1)

    ## converting and thresholding
    im <- im %>% image_convert(colorspace="HSL") %>% image_channel("1") %>% image_threshold(threshold="10%")

    tiff <- tempfile()
    magick::image_write(im, path = tiff, format = 'tiff')
    rstobj <- raster::brick(tiff)

    lab <- stringr::str_remove(annot@labels[id], "17Networks_[RL]H_")
    names(rstobj) <- lab

    ret[[id]] <- list(
      hemi=hemi,
      side=side,
      id=id,
      region=annot@labels[id],
      rstobj=rstobj
    )

  }

  rgl.close()

  ret
}

ras_lat_lh <- do_snapshots("left", "lateral")
ras_med_lh <- do_snapshots("left", "medial")
ras_lat_rh <- do_snapshots("right", "lateral")
ras_med_rh <- do_snapshots("right", "medial")

raslis <- c(ras_lat_lh, ras_med_lh, ras_lat_rh, ras_med_rh)
contourobjs <- map(raslis, ~ mkContours(.))

kp <- !map_lgl(contourobjs, is.null)
contourobjs <- contourobjs[kp]
scontourobjs <- map(contourobjs, ~ smoothr::smooth(., method="ksmooth", smoothness=3.5))
contourobjsDF <- do.call(rbind, scontourobjs)

## Now we need to place them into their own panes
## Bounding box for all
bball <- st_bbox(contourobjsDF)
contourobjsDF <-  mutate(contourobjsDF, geometry=geometry - bball[c("xmin", "ymin")])

## ifelse approach doesn't seem to work, so split it up
dfA <- contourobjsDF %>%
  filter(hemi=="left", side=="medial") %>%
  mutate(geometry=geometry+c(240,0))

dfB <- contourobjsDF %>%
  filter(hemi=="right", side=="medial") %>%
  mutate(geometry=geometry+c(2*240,0))

dfC <- contourobjsDF %>%
  filter(hemi=="right", side=="lateral") %>%
  mutate(geometry=geometry+c(3*240,0))

dfD <- contourobjsDF %>%
  filter(hemi=="left", side=="lateral")

dfpanes <- rbind(dfD, dfA, dfB, dfC)
dfpanes_simple <- rmapshaper::ms_simplify(dfpanes)
dfpanes_simple <- st_buffer(dfpanes_simple, dist=buffer_dist)

#dfpanes_simple <- st_buffer(dfpanes_simple, .2)
#dfpanes_simple <- st_difference(dfpanes_simple,dfpanes_simple)

dfpanes_simple <- dfpanes_simple %>% as_tibble() %>% dplyr::select(region,hemi,side, geometry)

## Not sure whether the range of values really matters. The other atlases look like they
## may be giving the coordinates in physical units of some sort.
## Lets pretend each picture is 10cm square. Divide point values by 60 at the end.

## ggseg .long  .lat   .id .order
df_final <- mutate(dfpanes_simple,
                      id=1:nrow(dfpanes_simple),
                      ggseg = map(geometry, ~(st_coordinates(.x)[, c("X", "Y")])),
                      ggseg = map(ggseg, as.tibble),
                      ggseg = map(ggseg, ~mutate(.x, .long=.x$X, .lat=.x$Y, .order=1:nrow(.x)) %>% dplyr::select(-X,-Y)))



df_final <- df_final %>% mutate(atlas="SchaeferYeo400") %>%
  dplyr::select(-geometry) %>% dplyr::rename(area=region)

if (nparcels == 200) {
  Schaefer17_200 <- ggseg::as_ggseg_atlas(df_final)
  usethis::use_data(Schaefer17_200, internal = FALSE, overwrite = TRUE, compress = "xz")
} else if (nparcels == 400) {
  Schaefer17_400 <- as_ggseg_atlas(df_final)
  usethis::use_data(Schaefer17_400, internal = FALSE, overwrite = TRUE, compress = "xz")
}
##ggseg(atlas=Schaefer17_200, color="black", size=.5, position="stacked",mapping=aes(fill=area, alpha=.5)) +
##  theme(legend.position = "none") + theme_darkbrain()
#ggseg(atlas=glasser, color="black", size=.5, position="stacked",mapping=aes(fill=area)) +
#  theme(legend.position = "none") + theme_darkbrain()


#saveRDS(df_final, file=paste0("Schaefer-Yeo-17Networks", nparcels, ".rds"))
