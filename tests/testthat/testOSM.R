#'
#' Example of meta-model for OSM and an FMU in Bas-Saint-Laurent
#'

rm(list=ls())

library(OSMWebAPI4R)

variantList <- OSMGetVariantList()

test_that("Check if variant list is populated", {
  expect_equal("Acadian" %in% variantList, T)
})

variant <- "Acadian"

speciesConiferous <- OSMGetVariantSpecies(variant, "Coniferous")
speciesBroadleaved <- OSMGetVariantSpecies(variant, "Broadleaved")
test_that("Check if coniferous and broadleaved species lists", {
  expect_equal(length(speciesBroadleaved), 48)
  expect_equal(length(speciesConiferous), 16)
})

variantFields <- OSMGetVariantFields(variant)
test_that("Check variant fields", {
  expect_equal(length(variantFields), 2)
})

outputRequestTypes <- OSMGetOutputRequestTypes()
test_that("Check output request type", {
  expect_equal("AliveVolume" %in% outputRequestTypes, T)
})

outputRequestList <- CFSCommonGYModelWebAPI4R::new_OutputRequestList()
outputRequestList$addOutputRequest(outputRequestTypes[1], list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

df <- OSMSimulate(OSMWebAPI4R::OSMThreeStandList, outputRequestList, variant, 80, 5)
ds <- df$dataSet
test_that("Check simulation results", {
  expect_equal(df$nbPlots, 3)
  expect_equal(nrow(ds), 34)
  expect_equal(max(ds$timeSinceInitialDateYear), 80)
})

