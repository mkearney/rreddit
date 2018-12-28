

non_recs <- function(x) {
  x[!sapply(x, is.recursive)]
}

formate_createds <- function(d) {
  if ("created" %in% names(d)) {
    d$created <- as.POSIXct(d$created, origin = "1970-01-01")
  }
  if ("created_utc" %in% names(d)) {
    d$created_utc <- as.POSIXct(d$created_utc, origin = "1970-01-01", tz = "UTC")
  }
  d
}

docall_rbind <- function(...) {
  dfs <- list(...)
  if (length(dfs) == 1L && is.list(dfs[[1]]) &&
      is.data.frame(dfs[[1]][[1]])) {
    dfs <- dfs[[1]]
  }
  nms <- unlist(lapply(dfs, names))
  nms <- table(nms)
  max_n <- max(nms, na.rm = TRUE)
  nms <- names(nms)[nms == max_n]
  dfs <- lapply(dfs, function(.x) .x[nms])
  dfs <- do.call("rbind", dfs, quote = TRUE)
  dfs <- dfs[!duplicated(dfs$id), ]
  dfs
}
