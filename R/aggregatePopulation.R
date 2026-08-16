#' Aggregate population for reporting
#'
#' Population can use a different regional resolution from the other reporting
#' variables. Aggregate only the detailed regions covered by the supplied map,
#' and calculate World from the unaggregated population to avoid double counting.
#'
#' @param population Population data in long format.
#' @param regSubsetMap Mapping with `region` and `aggrReg` columns.
#'
#' @returns Population data containing the original, aggregate, and World regions.
#' @keywords internal
#' @noRd

aggregatePopulation <- function(population, regSubsetMap) {
  region <- value <- NULL

  populationBase <- copy(population)
  populationWorld <- populationBase[, .(value = sum(value)),
                                    by = setdiff(names(populationBase), c("region", "value"))]
  populationWorld[, region := "World"]

  mapRegions <- unique(regSubsetMap$region)
  populationSubset <- populationBase[region %chin% mapRegions]

  populationAggregated <- NULL
  if (length(mapRegions) > 0L &&
        setequal(unique(populationSubset$region), mapRegions)) {
    populationAggregated <- as.data.table(
      aggregate_map(populationSubset, regSubsetMap, by = "region")
    )
  }

  return(rbindlist(
    list(populationBase, populationAggregated, populationWorld),
    use.names = TRUE,
    fill = TRUE
  ))
}
