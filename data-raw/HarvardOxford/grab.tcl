# surfparc_makeimages.tcl

set subjectsdir $home
set subjectdir "$home/$subject"
set imagedir "$subjectdir/screencaps"

puts "Subject Dir: $subjectdir"
puts "Hemi: $hemi"

#----------------------------------
# Prep
#----------------------------------
# 0 Main vertices 
# 1 Inflated vertices 
# 2 White vertices 
# 3 Pial vertices 
# 4 Orig vertices 

# Supposed to load into pial vertex slots, but appears to load into main instead
# read_pial_vertex_coordinates

read_surface_vertex_set 1 inflated
read_surface_vertex_set 2 white
read_surface_vertex_set 3 pial 

# note: following command wants side omitted
#labl_import_annotation "aparc.annot"

# read the parameters via environment variables
set inlabelfile $::env(INLABFILE) 
set regname $::env(REGNAME)
set TARGFOLDER $::env(DEST)

labl_load $inlabelfile

set rotmult 1

if {$hemi == "rh"} {
  set rotmult -1
}

labl_select -1
labl_set_color 0 255 0 0

#---------------------------
# Iterate surfaces
#---------------------------
# was {2 3}
foreach surfix {1} {
  set surfname "ERROR"

  switch -exact -- $surfix {
      1 { set surfname infl }
      2 { set surfname wh }
      3 { set surfname pl }
  }

  #show_surf vertexSet

  set_current_vertex_set $surfix

  #---------------------------
  # Iterate views
  #---------------------------
  # was  {$viewix0 < 6} 
  for {set viewix0 0} {$viewix0 < 2} {incr viewix0 1} {

    switch -exact -- $viewix0 {
      0 { set rot   0                   ; set axis a ; set viewname lat ; set zoom 1.25 }
      1 { set rot 180                   ; set axis y ; set viewname med ; set zoom 1.25 }
      2 { set rot [expr -90 * $rotmult] ; set axis y ; set viewname ant ; set zoom 1.8  }
      3 { set rot [expr  90 * $rotmult] ; set axis y ; set viewname pst ; set zoom 1.8  }
      4 { set rot -90                   ; set axis x ; set viewname sup ; set zoom 1.35 }
      5 { set rot  90                   ; set axis x ; set viewname inf ; set zoom 1.35 }
    }

    make_lateral_view

    switch -exact -- $axis {
      x { rotate_brain_x $rot }
      y { rotate_brain_y $rot }
    }

    scale_brain $zoom
    redraw
     
    # Use braces to avoid tcl's utterly brain-dead catenation inadequacies...
    #set imagepath "${imagedir}/${subject}_${surfname}_${hemi}_${viewname}.tif"
    set imagepath "${TARGFOLDER}/${subject}_${surfname}_${hemi}_${regname}_${viewname}.tif"
    save_tiff $imagepath
  }
}

# will cause FS to exit
exit 0
