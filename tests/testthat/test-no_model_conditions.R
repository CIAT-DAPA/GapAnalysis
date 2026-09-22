# Tests for the edge conditions introduced in 2.1.1:
#   * a taxon with records but no usable coordinates (no SDM can be built)
#   * a taxon with coordinates but no SDM supplied (sdm = NULL)
#   * a taxon with a model but no G (germplasm) records
#   * the noModel argument to FCSex / FCSin
#
# GRSex and GRSin require a SpatRaster and are expected to be skipped by the
# caller when no model exists, so they are not exercised in the no-model paths.

# ---- shared fixtures ---------------------------------------------------------

data(CucurbitaData)
data(CucurbitaRasts)
data(ProtectedAreas)
data(ecoregions)

protectedAreas <- terra::unwrap(ProtectedAreas)
ecos <- terra::vect(ecoregions)
sdmCordata <- terra::unwrap(CucurbitaRasts)$cordata

# A taxon with 10 records (all H) and no coordinates at all
noCoordTaxon <- "Cucurbita_nocoords"
noCoordRows <- CucurbitaData[CucurbitaData$species == "Cucurbita_cordata", ][1:10, ]
noCoordRows$species <- noCoordTaxon
noCoordRows$latitude <- NA_real_
noCoordRows$longitude <- NA_real_

# A taxon with coordinates and a model but no G records
noGTaxon <- "Cucurbita_noG"
noGRows <- CucurbitaData[CucurbitaData$species == "Cucurbita_cordata" &
                           CucurbitaData$type == "H", ]
noGRows$species <- noGTaxon

occ <- rbind(CucurbitaData, noCoordRows, noGRows)

# ---- taxon with no coordinates and no model ----------------------------------

test_that("SRSex counts records without coordinates and returns SRS = 0 when no G", {
  srs <- SRSex(taxon = noCoordTaxon, occurrenceData = occ)
  expect_s3_class(srs, "data.frame")
  expect_equal(srs[["Total records"]], 10)
  expect_equal(srs[["Total with coordinates"]], 0)
  expect_equal(srs[["Total G records"]], 0)
  expect_equal(srs[["SRS exsitu"]], 0)
})

test_that("generateGBuffers reports no G points when the taxon has none with coordinates", {
  gb <- generateGBuffers(taxon = noCoordTaxon, occurrenceData = occ, bufferDistM = 50000)
  expect_type(gb, "list")
  expect_true(is.character(gb$data))
})

test_that("ERSex runs without a model for a taxon with no coordinates", {
  gb <- generateGBuffers(taxon = noCoordTaxon, occurrenceData = occ, bufferDistM = 50000)

  ers <- suppressWarnings(ERSex(
    taxon = noCoordTaxon, sdm = NULL, occurrenceData = occ,
    gBuffer = gb, ecoregions = ecos, idColumn = "ECO_NAME",
    limitByPoints = FALSE
  ))
  expect_type(ers, "list")
  expect_equal(ers$results[["ERS exsitu"]], 0)
  expect_true(is.na(ers$results[["Ecoregions within G buffer"]]))
  # with no model every considered ecoregion is reported as a gap
  expect_equal(ers$results[["Ecoregions with records"]], nrow(ers$ecoGaps))
  expect_s3_class(ers$map, "leaflet")

  # limitByPoints = TRUE leaves no ecoregions; must not error
  ersLimited <- suppressWarnings(ERSex(
    taxon = noCoordTaxon, sdm = NULL, occurrenceData = occ,
    gBuffer = gb, ecoregions = ecos, idColumn = "ECO_NAME",
    limitByPoints = TRUE
  ))
  expect_equal(ersLimited$results[["ERS exsitu"]], 0)
  expect_equal(ersLimited$results[["Ecoregions with records"]], 0)
  expect_s3_class(ersLimited$map, "leaflet")
})

test_that("SRSin runs without a model for a taxon with no coordinates", {
  srs <- suppressWarnings(SRSin(
    taxon = noCoordTaxon, sdm = NULL, occurrenceData = occ,
    protectedAreas = protectedAreas
  ))
  expect_type(srs, "list")
  expect_equal(srs$results[["Total Observations"]], 10)
  expect_true(is.na(srs$results[["Total records in SDM"]]))
  expect_equal(srs$results[["Records in Protected areas"]], 0)
  expect_equal(srs$results[["SRS insitu"]], 0)
  expect_s3_class(srs$map, "leaflet")
})

test_that("ERSin runs without a model for a taxon with no coordinates", {
  for (limit in c(FALSE, TRUE)) {
    ers <- suppressWarnings(ERSin(
      taxon = noCoordTaxon, sdm = NULL, occurrenceData = occ,
      protectedAreas = protectedAreas, ecoregions = ecos,
      idColumn = "ECO_NAME", limitByPoints = limit
    ))
    expect_type(ers, "list")
    expect_equal(ers$results[["Ecoregions within model"]], 0)
    expect_equal(ers$results[["Ecoregions with protected areas"]], 0)
    expect_equal(ers$results[["ERS insitu"]], 0)
    expect_equal(nrow(ers$missingEcos), 0)
    expect_s3_class(ers$map, "leaflet")
  }
})

test_that("Full no-coordinate, no-model workflow chains through to FCSc_mean", {
  srsex <- SRSex(taxon = noCoordTaxon, occurrenceData = occ)
  srsin <- suppressWarnings(SRSin(
    taxon = noCoordTaxon, sdm = NULL, occurrenceData = occ,
    protectedAreas = protectedAreas
  ))

  fcsex <- FCSex(taxon = noCoordTaxon, srsex = srsex, grsex = NULL, ersex = NULL,
                 noModel = TRUE)
  fcsin <- FCSin(taxon = noCoordTaxon, srsin = srsin, grsin = NULL, ersin = NULL,
                 noModel = TRUE)

  expect_equal(fcsex[["FCS exsitu"]], 0)
  expect_equal(fcsex[["FCS exsitu score"]], "UP")
  expect_equal(fcsin[["FCS insitu"]], 0)
  expect_equal(fcsin[["FCS insitu score"]], "UP")

  comb <- FCSc_mean(taxon = noCoordTaxon, fcsin = fcsin, fcsex = fcsex)
  expect_equal(comb$FCSc_mean, 0)
  expect_equal(comb$FCSc_mean_class, "UP")
})

# ---- taxon with coordinates but no model supplied ----------------------------

test_that("SRSin without a model uses every occurrence point", {
  withModel <- SRSin(taxon = "Cucurbita_cordata", sdm = sdmCordata,
                     occurrenceData = occ, protectedAreas = protectedAreas)
  noModel <- SRSin(taxon = "Cucurbita_cordata", sdm = NULL,
                   occurrenceData = occ, protectedAreas = protectedAreas)

  expect_equal(noModel$results[["Total Observations"]],
               withModel$results[["Total Observations"]])
  expect_true(is.na(noModel$results[["Total records in SDM"]]))
  expect_equal(nrow(noModel$points), noModel$results[["Total Observations"]])
  expect_equal(
    noModel$results[["SRS insitu"]],
    noModel$results[["Records in Protected areas"]] /
      noModel$results[["Total Observations"]] * 100
  )
})

test_that("ERSex and ERSin without a model return zero for a taxon with coordinates", {
  gb <- generateGBuffers(taxon = "Cucurbita_cordata", occurrenceData = occ,
                         bufferDistM = 50000)
  ersex <- ERSex(taxon = "Cucurbita_cordata", sdm = NULL, occurrenceData = occ,
                 gBuffer = gb, ecoregions = ecos, idColumn = "ECO_NAME",
                 limitByPoints = TRUE)
  expect_equal(ersex$results[["ERS exsitu"]], 0)
  expect_gt(ersex$results[["Ecoregions with records"]], 0)
  expect_true(is.na(ersex$results[["Ecoregions within G buffer"]]))

  ersin <- ERSin(taxon = "Cucurbita_cordata", sdm = NULL, occurrenceData = occ,
                 protectedAreas = protectedAreas, ecoregions = ecos,
                 idColumn = "ECO_NAME", limitByPoints = TRUE)
  expect_equal(ersin$results[["ERS insitu"]], 0)
  expect_equal(ersin$results[["Ecoregions within model"]], 0)
})

# ---- taxon with a model but no G records -------------------------------------

test_that("GRSex and ERSex return zero when a modelled taxon has no G records", {
  gb <- generateGBuffers(taxon = noGTaxon, occurrenceData = occ, bufferDistM = 50000)
  expect_true(is.character(gb$data))

  grs <- GRSex(taxon = noGTaxon, sdm = sdmCordata, gBuffer = gb)
  expect_equal(grs$results[["GRS exsitu"]], 0)
  expect_equal(grs$results[["G buffer areas in model km2"]], 0)
  expect_gt(grs$results[["Area of model km2"]], 0)

  ers <- ERSex(taxon = noGTaxon, sdm = sdmCordata, occurrenceData = occ,
               gBuffer = gb, ecoregions = ecos, idColumn = "ECO_NAME",
               limitByPoints = TRUE)
  expect_equal(ers$results[["ERS exsitu"]], 0)
  expect_equal(ers$results[["Ecoregions within G buffer"]], 0)
  expect_gt(ers$results[["Ecoregions with records"]], 0)
  expect_equal(nrow(ers$ecoGaps), ers$results[["Ecoregions with records"]])

  srs <- SRSex(taxon = noGTaxon, occurrenceData = occ)
  fcs <- FCSex(taxon = noGTaxon, srsex = srs, grsex = grs, ersex = ers)
  expect_equal(fcs[["FCS exsitu"]], 0)
  expect_equal(fcs[["FCS exsitu score"]], "UP")
})

# ---- FCSex / FCSin noModel behaviour -----------------------------------------

test_that("FCSex noModel assigns 0 to GRS/ERS and averages the three metrics", {
  # Follows Khoury et al. (2019): taxa without a model receive GRS = ERS = 0
  # and FCS is the mean of the three metrics, i.e. SRS / 3
  srs <- dplyr::tibble(Taxon = "t", "SRS exsitu" = 60)
  out <- FCSex(taxon = "t", srsex = srs, grsex = NULL, ersex = NULL, noModel = TRUE)
  expect_equal(out[["GRS exsitu"]], 0)
  expect_equal(out[["ERS exsitu"]], 0)
  expect_equal(out[["FCS exsitu"]], 60 / 3)
  expect_equal(out[["FCS exsitu score"]], "UP")

  # SRS of 100 (G records but no H records) scores 33.3 -> HP, not LP
  srs100 <- dplyr::tibble(Taxon = "t", "SRS exsitu" = 100)
  out100 <- FCSex(taxon = "t", srsex = srs100, grsex = NULL, ersex = NULL, noModel = TRUE)
  expect_equal(out100[["FCS exsitu"]], 100 / 3)
  expect_equal(out100[["FCS exsitu score"]], "HP")

  # no G records: SRS is 0 so FCS is 0
  srs0 <- dplyr::tibble(Taxon = "t", "SRS exsitu" = 0)
  out0 <- FCSex(taxon = "t", srsex = srs0, grsex = NULL, ersex = NULL, noModel = TRUE)
  expect_equal(out0[["GRS exsitu"]], 0)
  expect_equal(out0[["FCS exsitu"]], 0)
  expect_equal(out0[["FCS exsitu score"]], "UP")
})

test_that("FCSin noModel assigns 0 to GRS/ERS and averages the three metrics", {
  srsin <- list(results = dplyr::tibble(Taxon = "t", "SRS insitu" = 90))
  out <- FCSin(taxon = "t", srsin = srsin, grsin = NULL, ersin = NULL, noModel = TRUE)
  expect_equal(out[["GRS insitu"]], 0)
  expect_equal(out[["ERS insitu"]], 0)
  expect_equal(out[["FCS insitu"]], 90 / 3)
  expect_equal(out[["FCS insitu score"]], "HP")
})

test_that("FCSex and FCSin default (noModel = FALSE) ignore the noModel branch", {
  srsex <- dplyr::tibble(Taxon = "t", "SRS exsitu" = 30)
  grsex <- list(results = dplyr::tibble("GRS exsitu" = 60))
  ersex <- list(results = dplyr::tibble("ERS exsitu" = 90))
  out <- FCSex(taxon = "t", srsex = srsex, grsex = grsex, ersex = ersex)
  expect_equal(out[["FCS exsitu"]], 60)
  expect_equal(out[["FCS exsitu score"]], "MP")

  srsin <- list(results = dplyr::tibble("SRS insitu" = 30))
  grsin <- list(results = dplyr::tibble("GRS insitu" = 60))
  ersin <- list(results = dplyr::tibble("ERS insitu" = 90))
  outIn <- FCSin(taxon = "t", srsin = srsin, grsin = grsin, ersin = ersin)
  expect_equal(outIn[["FCS insitu"]], 60)
  expect_equal(outIn[["FCS insitu score"]], "MP")
})

test_that("FCS priority categories follow the 25/50/75 thresholds", {
  category <- function(fcs) {
    srsin <- list(results = dplyr::tibble("SRS insitu" = fcs))
    grsin <- list(results = dplyr::tibble("GRS insitu" = fcs))
    ersin <- list(results = dplyr::tibble("ERS insitu" = fcs))
    FCSin(taxon = "t", srsin = srsin, grsin = grsin, ersin = ersin)[["FCS insitu score"]]
  }
  expect_equal(category(0), "UP")
  expect_equal(category(24.99), "UP")
  expect_equal(category(25), "HP")
  expect_equal(category(49.99), "HP")
  expect_equal(category(50), "MP")
  expect_equal(category(74.99), "MP")
  expect_equal(category(75), "LP")
  expect_equal(category(100), "LP")
})
