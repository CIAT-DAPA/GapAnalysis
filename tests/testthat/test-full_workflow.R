test_that("Full GapAnalysis workflow runs successfully", {
  # 1. Setup data (from the package)
  data(CucurbitaData)
  data(CucurbitaRasts)
  data(ProtectedAreas)
  data(ecoregions)
  
  taxon <- "Cucurbita_cordata"
  sdm <- terra::unwrap(CucurbitaRasts)$cordata
  occurrenceData <- CucurbitaData
  protectedAreas <- terra::unwrap(ProtectedAreas)
  ecos <- terra::vect(ecoregions)
  
  # 2. Ex-situ workflow
  srs_exsitu <- SRSex(taxon = taxon, occurrenceData  = occurrenceData)
  expect_s3_class(srs_exsitu, "data.frame")
  expect_true("SRS exsitu" %in% names(srs_exsitu))
  
  gBuffer <- generateGBuffers(taxon = taxon, occurrenceData = occurrenceData, bufferDistM = 50000)
  expect_type(gBuffer, "list")
  
  grs_exsitu <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  expect_type(grs_exsitu, "list")
  expect_true("GRS exsitu" %in% names(grs_exsitu$results))
  
  ers_exsitu <- ERSex(
    taxon = taxon, sdm = sdm, occurrenceData = occurrenceData, 
    gBuffer = gBuffer, ecoregions = ecos, idColumn = "ECO_NAME", 
    limitByPoints = FALSE
  )
  expect_type(ers_exsitu, "list")
  expect_true("ERS exsitu" %in% names(ers_exsitu$results))
  
  fcs_exsitu <- FCSex(taxon = taxon, srsex = srs_exsitu, grsex = grs_exsitu, ersex = ers_exsitu)
  expect_s3_class(fcs_exsitu, "data.frame")
  expect_true("FCS exsitu" %in% names(fcs_exsitu))
  
  # 3. In-situ workflow
  srs_insitu <- SRSin(taxon = taxon, sdm = sdm, occurrenceData = occurrenceData, protectedAreas = protectedAreas)
  expect_type(srs_insitu, "list")
  expect_true("SRS insitu" %in% names(srs_insitu$results))
  
  grs_insitu <- GRSin(taxon = taxon, sdm = sdm, protectedAreas = protectedAreas)
  expect_type(grs_insitu, "list")
  expect_true("GRS insitu" %in% names(grs_insitu$results))
  
  ers_insitu <- ERSin(
    taxon = taxon, sdm = sdm, occurrenceData = occurrenceData, 
    protectedAreas = protectedAreas, ecoregions = ecos, idColumn = "ECO_NAME", 
    limitByPoints = FALSE
  )
  expect_type(ers_insitu, "list")
  expect_true("ERS insitu" %in% names(ers_insitu$results))
  
  fcs_insitu <- FCSin(taxon = taxon, srsin = srs_insitu, grsin = grs_insitu, ersin = ers_insitu)
  expect_s3_class(fcs_insitu, "data.frame")
  expect_true("FCS insitu" %in% names(fcs_insitu))
  
  # 4. Combine conservation score
  fsc_combine <- FCSc_mean(taxon = taxon, fcsin = fcs_insitu, fcsex = fcs_exsitu)
  expect_s3_class(fsc_combine, "data.frame")
  expect_true("FCSc_mean" %in% names(fsc_combine))
  
  # 5. Test new parameter functionality: limitByPoints = TRUE
  ers_exsitu_limited <- ERSex(
    taxon = taxon, sdm = sdm, occurrenceData = occurrenceData, 
    gBuffer = gBuffer, ecoregions = ecos, idColumn = "ECO_NAME", 
    limitByPoints = TRUE
  )
  
  ers_insitu_limited <- ERSin(
    taxon = taxon, sdm = sdm, occurrenceData = occurrenceData, 
    protectedAreas = protectedAreas, ecoregions = ecos, idColumn = "ECO_NAME", 
    limitByPoints = TRUE
  )
  
  # The limited evaluations should have filtered out ecoregions without points,
  # resulting in fewer or equal total ecoregions considered compared to the legacy (FALSE) evaluation.
  expect_true(
    ers_exsitu_limited$results[["Ecoregions with records"]] <= ers_exsitu$results[["Ecoregions with records"]]
  )
  
  expect_true(
    ers_insitu_limited$results[["Ecoregions within model"]] <= ers_insitu$results[["Ecoregions within model"]]
  )
})