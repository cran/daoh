#' Load a built-in example dataset
#'
#' Reads one of the three synthetic example datasets bundled with the
#' package as CSV files in `inst/extdata/`. This is the preferred way
#' to access examples without needing to run the data-generation script.
#'
#' @param name Character. One of:
#'   \describe{
#'     \item{`"daystay"`}{Single day-stay admission: illustrates the 1-day
#'       difference between nights and days algorithms (30 vs 29 DAOH over
#'       30 days).}
#'     \item{`"death"`}{Four admissions with in-period death: reproduces the
#'       Figure 1 worked example, covering all seven DAOH variants.}
#'     \item{`"population"`}{500-patient synthetic cohort suitable for
#'       Bland-Altman, ICC, and reclassification demonstrations.}
#'   }
#' @return A named list with elements `events` and `index_dates`, both
#'   data.frames ready to pass directly to [calc_daoh()].
#'
#' @examples
#' ex <- load_example("daystay")
#' calc_daoh(ex$events, ex$index_dates, period = 30, method = "nights")
#' calc_daoh(ex$events, ex$index_dates, period = 30, method = "days")
#'
#' @export
load_example <- function(name = c("daystay", "death", "population")) {
  name <- match.arg(name)
  base <- system.file("extdata", package = "daoh")

  read_csv <- function(file) {
    df <- read.csv(file.path(base, file), stringsAsFactors = FALSE)
    # Convert date columns
    for (col in c("admission", "discharge", "indexDate")) {
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    }
    if ("dod" %in% names(df)) {
      df$dod <- ifelse(df$dod == "" | is.na(df$dod), NA_character_, df$dod)
      df$dod <- as.Date(df$dod)
    }
    df
  }

  list(
    events      = read_csv(paste0("example_", name, "_events.csv")),
    index_dates = read_csv(paste0("example_", name, "_index.csv"))
  )
}
