
#' Get data from reddit.com
#'
#' Reads/parses data directly from reddit.com
#'
#' @param subreddit Name of subreddit from which to get data. Defaults to "all".
#' @param n Number of submission/posts to return. Defaults to 100.
#' @param after Optional, the parameter from which to start the next search.
#' @param before Optional, the parameter from which to start the next search.
#' @return A data frame of reddit data.
#' @export
get_reddit_com <- function(subreddit = "all", n = 100, after = NULL, before = NULL) {
  n <- ceiling(n / 100)
  r <- vector("list", n)
  count <- 0
  for (i in seq_along(r)) {
    r[[i]] <- get_subreddit_(subreddit, after = after, before = before, count = count)
    r[[i]] <- parse_reddit_com_json(r[[i]])
    count <- count + nrow(r[[i]])
    after <- get_after(r[[i]])
    before <- NULL
    if (length(after) == 0 || after %in% c("0", "-1")) break
    tfse::print_complete(
      "#", i, ": collected ", nrow(r[[i]]), " posts!"
    )
  }
  d <- docall_rbind(r)
  r <- r[lengths(r) > 0]
  after <- sapply(r, get_after)
  before <- sapply(r, get_before)
  attr(d, "after") <- after
  attr(d, "before") <- before
  d
}

parse_reddit_com_json <- function(r) {
  if ("after" %in% names(r$data)) {
    after <- r$data$after
  } else {
    after <- NULL
  }
  if ("before" %in% names(r$data)) {
    before <- r$data$before
  } else {
    before <- NULL
  }
  d <- as_tbl(non_recs(r$data$children$data))
  d <- formate_createds(d)
  if (!"author_cakeday" %in% names(d)) {
    d$author_cakeday <- rep(FALSE, nrow(d))
  }
  if (!"crosspost_parent" %in% names(d)) {
    d$crosspost_parent <- rep(NA_character_, nrow(d))
  }
  attr(d, "before") <- before
  attr(d, "after") <- after

  d
}

get_after <- function(x) attr(x, "after")

get_before <- function(x) attr(x, "before")



get_subreddit_ <- function(subreddit,
  before = NULL,
  after = NULL,
  count = 100,
  sort = c("relevance", "hot", "top", "new", "comments")) {
  sort <- match.arg(sort)
  sort <- "new"
  url <- paste0(
    "https://www.reddit.com/r/",
    subreddit,
    ".json?limit=100&count=",
    count,
    "&sort=",
    sort
  )
  if (!is.null(before)) {
    url <- paste0(url, "&before=", before)
  }
  if (!is.null(after)) {
    url <- paste0(url, "&after=", after)
  }
  jsonlite::fromJSON(url)
}
