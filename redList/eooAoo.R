eooAoo <-function(species,Workspace,run_version) {

  suppressMessages(require(redlistr))
  suppressMessages(require(sp))
  
  
  occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
  cleanPoints <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
  colnames(cleanPoints) <- c("lon","lat")
  coordinates(cleanPoints) <- ~lon + lat
  sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
  
  
  if(file.exists(paste0(sp_dir, "/gap_analysis/redList/listingValues.csv"))){
    print('completed, moving on')
  }else{
    wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    worldEqualArea <- crs("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")
    cleanPoints2 <- sp::SpatialPoints(coords = cleanPoints@coords, proj4string = wgs84)
    spAllPro <- sp::spTransform(cleanPoints2, worldEqualArea)
    EOO.polygon <- redlistr::makeEOO(spAllPro)
    # then calcualte the area of the bounding box
    EOO.area <- redlistr::getAreaEOO(EOO.polygon)
    #determine status based on area
    if (EOO.area >= 45000) {blo <- "Least Concern (LC)"}
    if (EOO.area < 45000) {blo <- "Possible Near Threatened (NT)"}
    if (EOO.area < 20000) {blo <- "Vulnerable (VU)"} # 20000
    if (EOO.area < 5000) {blo <- "Endangered (EN)"} # 5000
    if (EOO.area < 100) {blo <- "Critically Endangered (CR)"} # 100
    if (EOO.area == "NA") {blo <- "Critically Endangered (CR)"}
    
    #EOO.area
    # this value is then use in the develop of other criteria in the sebcriterion B1
    
    ### Subcriterion B2 (calculating AOO)
    # create a 10 x 10 grid of to overlay on distribution.
    
    AOO.grid <- redlistr::makeAOOGrid(spAllPro, grid.size = 10000,
                            min.percent.rule = F)
    #plot(AOO.grid)
    n.AOO <- length(AOO.grid)
    AOOarea <- n.AOO* 100
    if (AOOarea >= 4500) {AOO_cat <- "Least Concern (LC)"} # <
    if (AOOarea < 4500) {AOO_cat <- "Possible Near Threatened (NT)"}
    if (AOOarea < 2000) {AOO_cat <- "Vulnerable (VU)"} # < 2000
    if (AOOarea < 500) {AOO_cat <- "Endangered (EN)"}# < 500
    if (AOOarea < 10) {AOO_cat  <- "Critically Endangered (CR)"}# < 10
    if (AOOarea == "NA") {AOO_cat <- "Critically Endangered (CR)"}
    
    
    #n.AOO
    # so the length is just the number of grid cells that overlay this environment
    # because the position of the grid cells can potential change the number of cells
    # a randomized process is used to determine a minimun number of grids.
    
    gU.results <- redlistr::gridUncertainty(spAllPro, 10000,
                                  n.AOO.improvement = 5,
                                  min.percent.rule = F)
    
    df <- data.frame(matrix(data = NA, nrow = 1, ncol = 6))
    colnames(df) <- c("taxon", "EOO Area km2","EOO Status", "AOO", "AOO adjusted Minimum", "AOO Status")
    
    df$taxon <- species
    df$`EOO Area km2` <- EOO.area
    df$`EOO Status`<- blo
    df$AOO <- n.AOO * 100
    df$`AOO adjusted Minimum` <- gU.results$min.AOO.grid$AOO.number * 100
    df$`AOO Status` <- AOO_cat
    
    #still run them with the html but do not display in docs
    # DT::datatable(df)
    write.csv(x = df, file = paste0(sp_dir, '/redList/listingValues.csv'),quote = F,row.names = F)
  }
 return(df)
}

Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
run_version="v1"
species_list <- c(
  "Cucurbita_cordata",
  "Cucurbita_digitata",
  "Cucurbita_foetidissima",
  "Cucurbita_palmata"
)
run_version <-"v1"

lapply(1:length(species_list),function(i){
  species <- species_list[[i]]
  x <- eooAoo(species,Workspace,run_version)
  cat(paste0(species," DONE!"),"\n")
})