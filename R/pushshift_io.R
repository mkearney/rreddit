#' Get data from pushshift.io
#'
#' Reads/parses reddit data from api.pushshift.io
#'
#' @param subreddit Name of subreddit from which to get data. Defaults to "all".
#' @param q Query term for comments and submissions.
#' @param title Search in title only.
#' @param selftext Search in selftext (main body) only.
#' @param author Restrict results to author - use "!" to negate, comma delimited for multiples.
#' @param is_video Boolean - Restrict results based on if submission is video.
#' @param is_self Boolean - Restrict results based on if submission is a self post.
#' @param is_original_content Boolean - Restrict results based on if submission is original content.
#' @param is_reddit_media_domain Boolean - Is Submission hosted on Reddit Media.
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
get_r_reddit <- function(subreddit = "all", q = NULL, title = NULL, selftext = NULL, author = NULL,
                         is_video = NULL, is_self = NULL, is_original_content = NULL, is_reddit_media_domain = NULL,
                         domain = NULL, link_url = NULL, n = 1000, after = NULL, before = NULL, verbose = FALSE) {
  n <- ceiling(n / 1000)
  x <- vector("list", n)
  for (i in seq_along(x)) {
    url <- "https://api.pushshift.io/reddit/submission/search/?size=1000"
    if (!identical(subreddit, "all")) url <- paste0(url, "&subreddit=", subreddit)
    if (!is.null(q)) url <- paste0(url, "&q=", urltools::url_encode(q))
    if (!is.null(title)) url <- paste0(url, "&title=", urltools::url_encode(title))
    if (!is.null(selftext)) url <- paste0(url, "&selftext=", urltools::url_encode(selftext))
    if (!is.null(author)) url <- paste0(url, "&author=", author)
    if (!is.null(is_video)) url <- paste0(url, "&is_video=", tolower(is_video))
    if (!is.null(is_self)) url <- paste0(url, "&is_self=", tolower(is_self))
    if (!is.null(is_original_content)) url <- paste0(url, "&is_original_content=", tolower(is_original_content))
    if (!is.null(is_reddit_media_domain)) url <- paste0(url, "&is_reddit_media_domain=", tolower(is_reddit_media_domain))
    if (!is.null(domain)) url <- paste0(url, "&domain=", urltools::url_encode(domain))
    if (!is.null(link_url)) url <- paste0(url, "&url=", urltools::url_encode(link_url))
    if (!is.null(before)) url <- paste0(url, "&before=", as.numeric(before))
    if (!is.null(after)) url <- paste0(url, "&after=", as.numeric(after))
    if(verbose) message(url)
    r <- httr::GET(url)
    j <- httr::content(r, as = "text", encoding = "UTF-8")
    j <- jsonlite::fromJSON(j)
    x[[i]] <- as_tbl(non_recs(j$data))
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
get_comment_reddit <- function(subreddit = "all", n = 1000, after = NULL, before = NULL, verbose = FALSE) {
  n <- ceiling(n / 1000)
  x <- vector("list", n)
  for (i in seq_along(x)) {
    url <- "https://api.pushshift.io/reddit/search/comment/?size=1000"
    if (!identical(subreddit, "all")) {
      url <- paste0(url, "&subreddit=", subreddit)
    }
    if (!is.null(before)) {
      url <- paste0(url, "&before=", as.numeric(before))
    }
    if (!is.null(after)) {
      url <- paste0(url, "&after=", as.numeric(after))
    }
    if(verbose) message(url)
    r <- httr::GET(url)
    j <- httr::content(r, as = "text", encoding = "UTF-8")
    j <- jsonlite::fromJSON(j)
    x[[i]] <- dplyr::as_tibble(non_recs(j$data))
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

