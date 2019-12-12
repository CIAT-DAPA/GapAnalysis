#main_code

Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
run_version="v1"
species_list <- c(
  "Cucurbita_cordata",
  "Cucurbita_digitata",
  "Cucurbita_foetidissima",
  "Cucurbita_palmata"
)
run_version <-"v1"
bufferType="G" # "G" or "total"
buff_dist=50000 #50km radius
species <- species_list[[1]]
###PRE-ANALYSIS
x <- create_folder_structure(species_list=species_list,dir="E:/CIAT/workspace/Workspace_test",run_version = "v1")
x <- clean_records(Workspace,species_csv,species_list,run_version)
x <- create_buffers(species,Workspace,bufferType,buff_dist,run_version)
###EX SITU ANALYSIS
x <-  srs_exsitu(species,Workspace,run_version)
x <- grs_exsitu(species,Workspace,run_version)
x <- ers_exsitu(species,Workspace,run_version)
x <- fcs_exsitu(species,Workspace,run_version)
###IN SITU ANALYSIS
x <- srs_insitu(species,Workspace,run_version)
x <- insitu_grs(species,Workspace,run_version)
x <- ers_insitu(species,Workspace,run_version)
x <- fcs_insitu(species,Workspace,run_version)
###COMBINED ANALYSIS
x <- fcs_combine(species,Workspace,run_version)
###EOO-AOO ANALYSIS
x <- eooAoo(species,Workspace,run_version)
  