#' Get data from pushshift.io
#'
#' Reads/parses reddit data from api.pushshift.io
#'
#' @param subreddit Name of subreddit from which to get data. Defaults to "all".
#' @param n Number of submission/posts to return. Defaults to 1000.
#' @param after Optional, the date-time from which to start the next search.
#' @param before Optional, the date-time from which to start the next search.
#' @return A data frame of reddit data.
#' @details Column descriptions are provided below
#'
#' \itemize{
#'   \item author Name of author of post
#'   \item author_flair_css_class Flair class attribute
#'   \item author_flair_text Flair text
#'   \item created_utc Date-time in UTC timezone
#'   \item domain Domain
#'   \item full_link Full URL
#'   \item id Unique identifier string
#'   \item is_self Whether is self
#'   \item link_flair_css_class Flair class attribute for link
#'   \item link_flair_text Flair text for link
#'   \item num_comments Number of comments
#'   \item over_18 Whether material is intended for people over 18
#'   \item permalink Stable link
#'   \item post_hint Type of post
#'   \item retrieved_on Date-time when data was originally received
#'   \item subreddit Name of subreddit
#'   \item score Reddit score
#'   \item selftext Text
#'   \item stickied Whether it's been stickied
#'   \item subreddit Name of subreddit
#'   \item subreddit_id Unique identifier of subreddit
#'   \item thumbnail Path to post thumbnail
#'   \item title Title of post
#'   \item url URL
#' }
#'
#' @export
get_r_reddit <- function(subreddit = "all", n = 1000, after = NULL) {
  n <- ceiling(n / 1000)
  x <- vector("list", n)
  for (i in seq_along(x)) {
    url <- "https://api.pushshift.io/reddit/search/submission/?size=1000"
    if (!identical(subreddit, "all")) {
      url <- paste0(url, "&subreddit=", subreddit)
    }
    if (!is.null(after)) {
      url <- paste0(url, "&before=", as.numeric(after))
    }
    r <- httr::GET(url)
    j <- httr::content(r, as = "text", encoding = "UTF-8")
    j <- jsonlite::fromJSON(j)
    x[[i]] <- tbltools::as_tbl(non_recs(j$data))
    if (!"created_utc" %in% names(x[[i]])) break
    x[[i]] <- formate_createds(x[[i]])
    after <- x[[i]]$created_utc[nrow(x[[i]])]
    if (length(after) == 0) break
    tfse::print_complete(
      "#", i, ": collected ", nrow(x[[i]]), " posts"
    )
  }
  tryCatch(docall_rbind(x),
    error = function(e) x)
}


#' Get comments from pushshift.io
#'
#' Reads/parses reddit data from api.pushshift.io
#'
#' @param subreddit Name of subreddit from which to get data. Defaults to "all".
#' @param author Restrict results to author.
#' @param n Number of submission/posts to return. Defaults to 1000.
#' @param after Optional, the date-time from which to start the next search.
#' @param before Optional, the date-time from which to start the next search.
#' @return A data frame of reddit data.
#' @details Column descriptions are provided below
#'
#' \itemize{
#'   \item author Name of author of post
#'   \item author_flair_css_class Flair class attribute
#'   \item author_flair_text Flair text
#'   \item body Text of the comment
#'   \item created_utc Date-time in UTC timezone
#'   \item domain Domain
#'   \item full_link Full URL
#'   \item id Unique identifier string
#'   \item is_self Whether is self
#'   \item link_flair_css_class Flair class attribute for link
#'   \item link_flair_text Flair text for link
#'   \item num_comments Number of comments
#'   \item over_18 Whether material is intended for people over 18
#'   \item permalink Stable link
#'   \item post_hint Type of post
#'   \item retrieved_on Date-time when data was originally received
#'   \item subreddit Name of subreddit
#'   \item score Reddit score
#'   \item selftext Text
#'   \item stickied Whether it's been stickied
#'   \item subreddit Name of subreddit
#'   \item subreddit_id Unique identifier of subreddit
#'   \item thumbnail Path to post thumbnail
#'   \item title Title of post
#'   \item url URL
#' }
#'
#' @export
get_comment_reddit <- function(subreddit = "all", author = NULL, n = 1000, after = NULL) {
  n <- ceiling(n / 1000)
  x <- vector("list", n)
  for (i in seq_along(x)) {
    url <- "https://api.pushshift.io/reddit/search/comment/?size=1000"
    if (!identical(subreddit, "all")) {
      url <- paste0(url, "&subreddit=", subreddit)
    }
    if (!is.null(author)) {
      url <- paste0(url, "&author=", author)
    }
    if (!is.null(after)) {
      url <- paste0(url, "&before=", as.numeric(after))
    }
    r <- httr::GET(url)
    j <- httr::content(r, as = "text", encoding = "UTF-8")
    j <- jsonlite::fromJSON(j)
    x[[i]] <- tbltools::as_tbl(non_recs(j$data))
    if (!"created_utc" %in% names(x[[i]])) break
    x[[i]] <- formate_createds(x[[i]])
    after <- x[[i]]$created_utc[nrow(x[[i]])]
    if (length(after) == 0) break
    tfse::print_complete(
      "#", i, ": collected ", nrow(x[[i]]), " posts"
    )
  }
  tryCatch(docall_rbind(x),
           error = function(e) x)
}

