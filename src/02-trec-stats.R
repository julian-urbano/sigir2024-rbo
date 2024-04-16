source("src/common.R")

for(year in .WEB_YEAR) {
  print(year)
  # Read runs
  d <- read_inputs(glue("scratch/01-trec-download/web{year}/"), breaker = "docid", topk = 1000)
  meta <- rio::import(glue("data/web{year}.csv"))

  s <- d |>
    group_by(sys, topic) |>
    summarize({
      if(all(!is.nan(score) & !is.na(score))) {
        x <- table(score)
        # how many ties
        n = n()
        n_u <- sum(x == 1) # untied
        n_t <- sum(x[x > 1]) # tied

        # size of tie groups
        avg <- mean(x)
        avg_t <- mean(x[x > 1]) # only groups with ties

        data.frame(n, n_u, n_t,
                   avg, avg_t)
      }else{
        NA
      }
    }) |>
    mutate(year = year, .before = sys) |>
    left_join(meta) |>
    relocate(group, .before = sys)

  # Save
  path <- glue("output/trec-stats/")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  rio::export(s, glue("{path}/web{year}.csv"))
}
