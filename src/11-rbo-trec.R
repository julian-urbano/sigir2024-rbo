source("src/common.R")
source("rbo/rbo.R")

for(breaker in .TIE_BREAKER) {
  for(year in .WEB_YEAR) {
    print(c(breaker, year))

    set.seed(0)

    # Read runs
    d <- read_inputs(glue("scratch/01-trec-download/web{year}/"), breaker = breaker, topk = 1000)
    meta <- rio::import(glue("data/web{year}.csv"))
    stopifnot(all(unique(meta$sys) %in% unique(d$sys)) && all(unique(d$sys) %in% unique(meta$sys)))#TODO
    topics <- sort(unique(d$topic))

    # For every pair of runs within the same group, compute RBO scores

    # First, precompute a data frame with all the comparisons' metadata
    # (will be easier to paralellize)
    r <- lapply(unique(meta$group), function(g) {
      group_syss <- meta |> filter(group == g) |> pull(sys) # runs from the same group
      if(length(group_syss) > 1) {
        lapply(utils::combn(group_syss, 2, simplify = FALSE), function(syss) {
          data.frame(sys1 = syss[1], sys2 = syss[2], topic = topics)
        }) |> bind_rows()
      }
    }) |> bind_rows()

    # And then proceed with actual calculations
    r <- future_lapply(1:nrow(r), function(i) {
      sys1 <- r$sys1[i]
      sys2 <- r$sys2[i]
      top <- r$topic[i]

      run1 <- d |> filter(sys == sys1, topic == top)
      run2 <- d |> filter(sys == sys2, topic == top)

      # Only calculate RBOs when having valid data (no missing or non-numeric scores)
      if(all(!is.na(run1$score)) && all(!is.na(run2$score)) &&
        all(!is.nan(run1$score)) && all(!is.nan(run2$score))) {

        lapply(.P, function(p) {
          run1ties <- extract_ranking(run1$doc, run1$score)
          run2ties <- extract_ranking(run2$doc, run2$score)

          data.frame(sys1, sys2, topic = top, p = p,
                     rbo = rbo(run1$doc, run2$doc, p, "w", "ext"),
                     rbow = rbo(run1ties, run2ties, p, "w", "ext"),
                     rboa = rbo(run1ties, run2ties, p, "a", "ext"),
                     rbob = rbo(run1ties, run2ties, p, "b", "ext"))
        })
      }else{
        data.frame(sys1, sys2, topic = top, p = NA,
                   rbo = NA,
                   rbow = NA,
                   rboa = NA,
                   rbob = NA)
      }
    }) |> bind_rows()

    # Save all
    path <- glue("output/rbo-trec-{breaker}/")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    rio::export(r, glue("{path}/web{year}.csv"))
  }
}
