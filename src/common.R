.N_CPU <- parallelly::availableCores()
future::plan(future::multisession(workers = .N_CPU))

.TIE_BREAKER <- c("docid", "random")
.WEB_YEAR <- 2009:2014
.P <- c(.8, .9, .95)

read_inputs <- function(path, breaker = c("docid", "random"), topk = NULL) {
  breaker <- match.arg(breaker)

  f <- list.files(path, pattern = "\\.gz$", full.names = TRUE)

  future_lapply(f, function(ff) {
    d <- read.table(gzfile(ff), header = FALSE) |>
      rename_with(~c("topic","q0","doc","rank","score","sys")) |>
      select(-q0,-rank) |>
      mutate(sys = tools::file_path_sans_ext(basename(ff)))
    if(breaker == "docid"){
      d <- d |>
        arrange(sys, topic, desc(score), doc)
    }else{
      d <- d |>
        mutate(rank = sample(seq(n()))) |>
        arrange(sys, topic, desc(score), rank) |>
        select(-rank)
    }

    if(!is.null(topk)) {
      d <- d |>
        group_by(sys, topic) |>
        slice_head(n = topk) |>
        ungroup()
    }
    d
  }) |> bind_rows()
}
