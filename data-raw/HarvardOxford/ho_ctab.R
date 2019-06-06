## make an annotation file for the harvard-oxford cortical atlas
library(tidyverse)
library(xml2)

ho <- xml2::read_xml(file.path(Sys.getenv("FSLDIR"), "/data/atlases/HarvardOxford-Cortical.xml"))

ll <- as_list(ho)

labels <- map_chr(ll$atlas$data, 1)
names(labels) <- NULL

labels <- c("unknown", labels)

HO <- tibble(idx=0:(length(labels)-1), labels=labels)

FS <- readr::read_table2(file.path(Sys.getenv("FREESURFER_HOME"), "FreeSurferColorLUT.txt"), skip=4, col_names=FALSE)
colnames(FS) <- c("idx", "label", "R", "G", "B", "A")

## choose 48 rows from here for colour
start <- which.max(FS$label=="ctx-lh-Unknown")

colours <- slice(FS, start:(start+nrow(HO)-1))

HO <- bind_cols(HO, select(colours, R, G, B, A))

HO <- mutate(HO, labels=make.names(labels))
readr::write_delim(HO, "../label/ho.annot.ctab", col_names = FALSE)

## Want the colours to be distinct - do any editing here.
## HLS transform could be an option.
