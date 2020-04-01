#' Split surface file into separate plys
#' 
#' Function to splot up a surface ply
#' into several ply's based on a freesurfer
#' label file.
#' 
#' Script adapted to R and ggseg based on scripts
#' from Anderson M. Winkler: http://brainder.org
#'
#' @param srf_ply Surface ply path
#' @param label_path label dpv file
#' @param prefix output prefix
#' @template output_dir
#' @template verbose
#'
#' @return list of ply meshes
surfsplit <- function(srf_ply, 
                      label_path, 
                      prefix = "test", 
                      output_dir = dirname(label_path), 
                      verbose = TRUE){
  
  dirs <- lapply(c("ascii", "ply"), function(x) file.path(output_dir, x))
  j <- lapply(dirs[!sapply(dirs, dir.exists)], dir.create, recursive = TRUE)
  
  surf <- get_mesh(srf_ply, ShowSpecimen = FALSE)  
  
  dpx <- utils::read.table(label_path)
  
  nV <- nrow(surf$vertices)

  if(is.null(dpx)){
    
    # TODO: currently this part is not working,
    # because we are missing the dmperm function in R
    # Was given a tip by twitter user @pachamaltese
    # that Rcpparmadillo has a similar function that
    # might work using the schur decomposition:
    # http://arma.sourceforge.net/docs.html#schur
    
    # see you twitter thread:
    # https://twitter.com/DrMowinckels/status/1242137452913086467
    
    # Two vertices are connected if they share an edge.
    # adj  = sparse( ...
    #                [ ...
    #                  fac(:,1); fac(:,1);  ...
    #                  fac(:,2); fac(:,2);  ...
    #                  fac(:,3); fac(:,3)], ...
    #                [ ...
    #                  fac(:,2); fac(:,3);  ...
    #                  fac(:,1); fac(:,3);  ...
    #                  fac(:,1); fac(:,2)],1,nV,nV);
    # 
    # % Partition the adjacency with the Dulmage-Mendelsohn algorithm.
    # % See the MATLAB help for details.
    # [p,~,r] = dmperm(adj);
    # dpx     = zeros(size(adj,1),1);
    # clear adj;
    # for j = 1:numel(r)-1
    # dpx(p(r(j):r(j+1)-1)) = j;
    # end
    # 
    # % Save label indices
    # fname = sprintf('%s.labels.dpx',srfprefix);
    # dpxwrite(fname,dpx);
    # 
    # % For each unique label index
    # U = unique(dpx);
    # for lab = 1:numel(U),
    # idx  = dpx == U(lab);
    # vtxu = vtx(idx,:);
    # nV2  = size(vtxu,1);
    # idx  = double(idx);
    # idx(~~idx) = (1:nV2)';
    #       facu = idx(fac);
    #       facu = facu(all(facu,2),:);
    #       
    #       % Save the resulting surface
    #       fname = sprintf('%s.%0.4d.srf',srfprefix,lab);
    #       srfwrite(vtxu,facu,fname);
    #   end
    
  }else{
    
    facewise <- is_facewise(dpx, surf, verbose)
    
    # Make sure data contains only integers (labels)
    if(!is.integer(dpx$V1)){
      cat('The DPV or DPF file must contain only integers\nAborting.\n')
      stop(call. = FALSE)
    }
    
    # Make a short list of labels, with no repetitions, and index using
    # integers only, monotonically growing and with no intervals
    udpx <- unique(dpx$V5)   # Unique labels
    udpx <- udpx[order(udpx)]
    uidx <- 1:length(udpx)          # Unique corresponding indices
    
    if(verbose) cat('The number of unique labels is',length(udpx), '\n')
    
    dpxidx = rep(0, nrow(dpx))
    
    for(lab in uidx){
      dpxidx[dpx$V5 == udpx[lab]] <- lab # Replace labels by indices
    }
    
    # Vars to store vtx and fac for each label
    vtxL <- lapply(uidx, prep_n_data, type = "vertex")
    facL <- lapply(uidx, prep_n_data, type = "face")
    
    if(facewise){
      # If facewise data, simply take the faces and assign
      # them to the corresponding labels
      for(lab in uidx){
        facL[[lab]] = surf$faces[dpxidx == lab,]
      }
      
    } else {
      
      if(verbose){
        cat("Re-indexing faces\n")
        pb <- utils::txtProgressBar(min = 1, max = nrow(surf$faces), style = 3)
      } 
      
      # If vertexwise data
      for(f in 1:nrow(surf$faces)){
        
        # Current face & labels
        Cfac <- surf$faces[f,]
        Cidx = dpxidx[unlist(Cfac)]
        
        # Depending on how many vertices of the current face
        # are in different labels, behave differently
        if(length(unique(Cidx)) == 1){ # If all vertices share same label
          
          # Add the current face to the list of faces of the
          # respective label, and don't create new faces
          facL[[unique(Cidx)]] <- rbind(facL[[unique(Cidx)]], Cfac)
          
        }else{ # If 2 or 3 vertices are in different labels
          # Define 4 new faces, with care preserve normals (all CCW)
          Cfac_l <- unlist(Cfac)
          facnew <- data.frame(
            i = c(Cfac_l[1], nV+1, nV+3, nV+1),
            j = c(nV+1, Cfac_l[2], nV+2, nV+2),
            k = c(nV+3, nV+2, Cfac_l[3], nV+3)
          )
          
          if(is.null(getmode(Cidx))) print(paste0(f, ": mode is ", getmode(Cidx)))
          
          # Add the new faces to their respective labels
          facL[[Cidx[1]]] <- dplyr::bind_rows(facL[[Cidx[1]]], facnew[1,])
          facL[[Cidx[2]]] <- dplyr::bind_rows(facL[[Cidx[2]]], facnew[2,])
          facL[[Cidx[3]]] <- dplyr::bind_rows(facL[[Cidx[3]]], facnew[3,])
          facL[[getmode(Cidx)]] <- dplyr::bind_rows(facL[[getmode(Cidx)]], facnew[4,]) # central face
          
          # Create 3 new vertices at the midpoints of the 3 edges
          vtxCfac <- surf$vertices[Cfac_l,]
          vtxnew <- (vtxCfac + vtxCfac[c(2, 3, 1), ]) / 2
          surf$vertices <- dplyr::bind_rows(surf$vertices, vtxnew)
          
          nV <- nrow(surf$vertices) # Update nV for the next loop
        } # if length(unique(Cidx)) == 1
        
        if(verbose) utils::setTxtProgressBar(pb, f)
      } #for f
      close(pb)
      
      # Having defined new faces and assigned all faces to labels, now
      # select the vertices and redefine faces to use the new vertex indices

      if(verbose){
        cat("Writing dpv\n")
        pb <- utils::txtProgressBar(min = 1, max = length(uidx), style = 3)
      } 
      
      fname <- rep(NA_character_, length(uidx))
      for(lab in uidx){
        
        # Vertices for the current label
        unl_fc <- unlist(facL[[lab]])
        
        vidx  <- unique(unl_fc[order(unl_fc)])
        
        vtxL[[lab]] <- surf$vertice[vidx,]
        
        # Reindex the faces
        tmp <- rep(0, nV)
        tmp[vidx] <- 1:length(vidx)
        
        facL[[lab]] <- matrix(tmp[unl_fc],
                              nrow = nrow(facL[[lab]]), 
                              ncol = ncol(facL[[lab]])
        )
        facL[[lab]] <- as.data.frame(facL[[lab]])
        names(facL[[lab]]) <- c("i", "j", "k")               
        
        # Save the resulting surface
        fname[lab] = file.path(output_dir,"ascii", sprintf("%s.%04d.dpv", prefix, lab))
        write_dpv(fname[lab], vtxL[[lab]], facL[[lab]]);
        closeAllConnections()
        
        if(verbose) utils::setTxtProgressBar(pb, lab)
        
      } # for lab
      
    } # if facewise
  } # if empty dpx
  
  plys <- sapply(fname, function(x) asc2ply(x,
                 output_file = gsub("\\.dpv", "\\.ply", gsub("ascii", "ply", x)))
  )
  closeAllConnections()
  
  plys <- lapply(gsub("\\.dpv", "\\.ply", gsub("ascii", "ply", fname)), 
                      get_mesh, ShowSpecimen = FALSE)
  closeAllConnections()

  names(plys) <- gsub(paste0(prefix, "\\.|\\.dpv"), "", basename(fname))

  return(plys)
}



#' prep data for verteces/faces
#'
#' @param n unnused arg, need for apply
#' @param type vertex or face
prep_n_data <- function(n, type = "vertex"){
  
  type <- match.arg(type, c("vertex", "face"))
  nms <- switch(type,
                "vertex" = c("x", "y", "z"),
                "face" = c("i", "j", "k"))
  
  x <- matrix(NA, nrow = 0, ncol = 3)
  x <- as.data.frame(x)
  names(x) <- nms
  
  x
}
