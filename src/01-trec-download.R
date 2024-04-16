source("src/common.R")

if(!exists(".TREC_USER")) stop("Set variable .TREC_USER to you know what")
if(!exists(".TREC_PASS")) stop("Set variable .TREC_PASS to you know what")

for(year in .WEB_YEAR) {
  print(year)
  trec_edition <- year-1992+1

  path <- glue("scratch/01-trec-download/web{year}/")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  d <- rio::import(glue("data/web{year}.csv"))

  future_sapply(d$sys, function(sys) {
    if(!file.exists(glue("{path}/{sys}.gz")))
      download.file(glue("https://{.TREC_USER}:{.TREC_PASS}@trec.nist.gov/results/trec{trec_edition}/web/input.{sys}.gz"),
                    destfile = glue("{path}/{sys}.gz"), quiet = TRUE)

  },
  future.globals = c(".TREC_USER", ".TREC_PASS", "trec_edition", "path"),
  future.packages = "glue")
}
