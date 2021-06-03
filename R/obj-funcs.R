
read_obj <- function(path){
  
  obj <- readLines(path)
  vs <- obj[grep("^v", obj)]
  vs <- strsplit(vs, " ")
  vs <- do.call(cbind, vs)  
  vs <- as.matrix(vs[-1, ])
  vs <- apply(vs, 2, as.numeric)
  
  fc <- obj[grep("^f", obj)]
  fc <- strsplit(fc, " ")
  fc <- do.call(cbind, fc)  
  fc <- as.matrix(fc)[-1,]
  fc <- apply(fc, 2, as.numeric)
  
  fmt <- grep("usemtl", obj)
  fcc <- c()
  if(length(fmt) > 0){
    for(i in 1:length(fmt)){
      mx <- fmt[i+1]-1
      if(is.na(mx)) mx <- length(obj)
      ff <- strsplit(obj[(fmt[i]+1):mx], " ")
      fcc <- c(fcc, rep(strsplit(obj[fmt[i]], " ")[[1]][2],
                        length(ff)))
    }
    
    mtl <- obj[grepl("mtllib", obj)]
    mtl <- file.path(dirname(path),
                     strsplit(mtl, " ")[[1]][2])
    mtl <- readLines(mtl)
    
    kd <- mtl[grep("Kd", mtl)]
    kd <- do.call(rbind, strsplit(kd, " "))[,-1]
    
    cols <- data.frame(
      mtl = gsub("newmtl ", "", mtl[grep("newmtl", mtl)]),
      hex = apply(kd, 1, function(x) grDevices::rgb(x[1], x[2], x[3]))
    )
    
    fcc <- cols$hex[match(fcc, cols$mtl)]
  }
  
  list(
    vertices = vs,
    faces = fc,
    facecolour = fcc
  )
}


# obj <- read_obj("~/Downloads/octa-color.obj")
# plot_ly(type = "mesh3d",
#         x = obj$vertices[1,],
#         y = obj$vertices[2,],
#         z = obj$vertices[3,],
#         i = obj$faces[1,]-1,
#         j = obj$faces[2,]-1,
#         k = obj$faces[3,]-1, 
#         facecolor = obj$facecolour)

