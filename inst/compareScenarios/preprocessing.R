# reference models for historical ----

# Sometimes it is necessary to choose a single model for the historical data,
# e.g., calculating per capita variables. These reference models are defined here.

histRefModel <- c(
  "Population" = "WDI",
  "GDP|PPP pCap" = "James_IMF"
)

options(mip.histRefModel = histRefModel)

# load custom plotting function ----

showLinePlotsByVariable <- function(
    data, vars, xVar, scales = "free_y",
    showHistorical = FALSE,
    mainReg = getOption("mip.mainReg"),
    histRefModel = getOption("mip.histRefModel"),
    yearsByVariable = getOption("mip.yearsBarPlot")) {
  data <- as.quitte(data)

  # Validate function arguments.
  stopifnot(is.character(vars))
  stopifnot(is.character(xVar) && length(xVar) == 1)
  stopifnot(is.character(scales) && length(scales) == 1)
  stopifnot(identical(showHistorical, TRUE) || identical(showHistorical, FALSE))
  stopifnot(is.null(yearsByVariable) || is.numeric(yearsByVariable))
  checkGlobalOptionsProvided(c("mainReg", "histRefModel"))
  stopifnot(is.character(mainReg) && length(mainReg) == 1)
  stopifnot(is.character(histRefModel) && !is.null(names(histRefModel)))
  stopifnot(xVar %in% names(histRefModel))

  dy <- data %>%
    filter(.data$variable %in% .env$vars)
  dx <- data %>%
    filter(.data$variable %in% .env$xVar) %>%
    filter(.data$scenario != "historical" | .data$model == .env$histRefModel[.env$xVar])
  d <- dy %>%
    left_join(dx, by = c("scenario", "region", "period"), suffix = c("", ".x")) %>%
    drop_na(.data$value, .data$value.x) %>%
    arrange(.data$period) %>%
    droplevels()
  dMainScen <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario != "historical") %>%
    droplevels()
  dMainHist <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario == "historical") %>%
    droplevels()
  dRegiScen <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario != "historical") %>%
    droplevels()
  dRegiHist <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario == "historical") %>%
    droplevels()

  regions <- levels(dRegiScen$region)

  warnMissingVars(dMainScen, vars)

  if (NROW(dMainScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  unitlabel <- ifelse(length(levels(d$unit)) == 0, "", paste0(" [", paste0(levels(d$unit), collapse = ","), "]"))
  label <- paste0(paste0(vars, collapse = ","), unitlabel)
  xLabel <- paste0(xVar, " [", paste0(levels(d$unit.x), collapse = ","), "]")


  p1 <- dRegiScen %>%
    ggplot(aes(.data$value.x, .data$value, color = .data$scenario)) +
    geom_line(aes(linetype = .data$scenario)) +
    facet_wrap(vars(.data$region), scales = scales) +
    theme_minimal() +
    expand_limits(y = 0) +
    ylab(label) +
    xlab(xLabel)
  if (showHistorical) {
    p1 <- p1 +
      geom_point(data = dRegiHist, aes(shape = .data$model)) +
      geom_line(data = dRegiHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5)
  }
  # Add markers for certain years.
  if (length(yearsByVariable) > 0) {
    p1 <- p1 +
      geom_point(
        data = dRegiScen %>%
          filter(.data$period %in% .env$yearsByVariable) %>%
          mutate(year = factor(.data$period)),
        mapping = aes(.data$value.x, .data$value, shape = .data$year)
      )
  }

  # Show plots.
  print(p1)
  cat("\n\n")

  return(invisible(NULL))
}

# calculate pCap variables ----

# For all variables in following table, add a new variable to data with the name
# "OldName pCap". Calculate its value by OldValue * conversionFactor and set its unit to newUnit.
# The new variable "OldName pCap" will be available in the plot sections.

pCapVariables <- tribble(
  ~variable, ~newUnit, ~conversionFactor,
  "GDP|PPP", "kUS$2017", 1e6,
  "ES|Transport edge|Pass", "pkm/yr", 1e9,
  "ES|Transport|Pass|Aviation", "pkm/yr", 1e9,
  "ES|Transport|Bunkers|Pass|International Aviation", "pkm/yr", 1e9,
  "ES|Transport|Pass|Domestic Aviation", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|Bus", "pkm/yr", 1e9,
  "ES|Transport|Pass|Non-motorized|Walk", "pkm/yr", 1e9,
  "ES|Transport|Pass|Non-motorized|Cycle", "pkm/yr", 1e9,
  "ES|Transport|Pass|Rail|non-HSR", "pkm/yr", 1e9,
  "ES|Transport|Pass|Rail|HSR", "pkm/yr", 1e9,
  "ES|Transport edge|Freight", "tkm/yr", 1e9,
  "ES|Transport|Bunkers|Freight|International Shipping", "tkm/yr", 1e9,
  "ES|Transport|Freight|Road", "tkm/yr", 1e9,
  "ES|Transport|Freight|Domestic Shipping", "tkm/yr", 1e9,
  "ES|Transport|Freight|Rail", "tkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV|Four Wheelers", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV|Two Wheelers", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV|BEV", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV|FCEV", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV|Gases", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV|Hybrid electric", "pkm/yr", 1e9,
  "ES|Transport|Pass|Road|LDV|Liquids", "pkm/yr", 1e9,
  "ES|Transport|Freight with bunkers", "tkm/yr", 1e9,
  "ES|Transport|Freight|Road|BEV", "pkm/yr", 1e9,
  "ES|Transport|Freight|Road|FCEV", "pkm/yr", 1e9,
  "ES|Transport|Freight|Road|Gases", "pkm/yr", 1e9,
  "ES|Transport|Freight|Road|Liquids", "pkm/yr", 1e9
)

dataPop <- data %>%
  filter(variable == "Population") %>%
  filter(scenario != "historical" | model == histRefModel["Population"]) %>%
  select(scenario, region, period, value) %>%
  mutate(
    population = value * 1e6, # unit originally is million, now is 1
    value = NULL
  )

dataPCap <- data %>%
  inner_join(pCapVariables, "variable") %>%
  left_join(dataPop, c("scenario", "region", "period")) %>%
  mutate(
    value = value / population * conversionFactor,
    variable = paste0(variable, " pCap"),
    unit = newUnit,
    newUnit = NULL, conversionFactor = NULL, population = NULL
  )

data <- data %>%
  bind_rows(dataPCap)
