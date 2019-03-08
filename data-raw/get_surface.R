
# Function to grab all the data and create a nested tibble
get_surface = function(folder, atlasname){
  
  surfs = list.dirs(folder, full.names = T, recursive = F)
  
  hemi = sapply(surfs, list.files, full.names = T) %>% as.character()
  
  files = sapply(hemi, list.files, pattern="*roi*", full.names = T) %>% as.character()
  
  mesh = lapply(files, geomorph::read.ply, ShowSpecimen = F)
  
  annots = sapply(hemi, list.files, pattern="annot*", full.names = T) %>%
    as.character() %>%
    lapply(readr::read_csv) %>%
    dplyr::bind_rows()
  
  
  data = data.frame(files=files) %>%
    tidyr::separate(files,
             c(NA, NA, NA, "atlas",NA, NA, "hemi", "surf",NA , "roi", "DELX"),
             remove=F) %>%
    tidyr::separate(files, remove=F,
             c("Del1", "DEL2", "DEL3", "atlas","DEL5", "DEL7", "hemi", "surf", "DEL6", "roi", "DELX")) %>%
    tidyr::separate(files, c("DEL1","DEL2","DEL#","DEL4","DEL5","DEL6","filename"), sep="/") %>%
    dplyr::select(-dplyr::contains("DEL")) %>%
    dplyr::left_join(annots, by="filename") %>%
    dplyr::mutate(annot = ifelse(annot == "unknown", "medialwall", annot)) %>%
    dplyr::mutate(label = paste(hemi, annot, sep="_"),
           hemi = ifelse(hemi =="lh", "left", "right"),
           atlas = atlasname) %>%
    dplyr::distinct()
  
  for(i in 1:length(mesh)){
    data$mesh[[i]] = list(vb=mesh[[i]]$vb,
                          it=mesh[[i]]$it
    )
  }
  
  data %>%
    dplyr::select(-filename, -ply) %>%
    dplyr::group_by(atlas, surf, hemi) %>%
    tidyr::nest()
}

rgb2hex = function(r = 0, g = 0, b = 0){
  df = data.frame(r, g, b)
  check = function(x, ix = NULL){
    nm = deparse(substitute(x))
    ix = as.character({ix %||% ''})
    if(!is.numeric(x))  stop(sprintf("'%s%s' must be numeric",             nm,ix),call. = FALSE)
    if(length(x) != 1)  stop(sprintf("'%s%s' must be scalar",              nm,ix),call. = FALSE)
    if(!is.finite(x))   stop(sprintf("'%s%s' must be finite",              nm,ix),call. = FALSE)
    if(x < 0 | x > 255) stop(sprintf("'%s%s' must be in the range [0,255]",nm,ix),call. = FALSE)
  }
  nr = nrow(df)
  sapply( c(1:nr), function(ix){
    n = if(nr > 1){ ix }else{ NULL }
    r = df$r[ix]; check(r,n)
    g = df$g[ix]; check(g,n)
    b = df$b[ix]; check(b,n)
    sprintf("#%.2x%.2x%.2x",r,g,b) 
  })
}
