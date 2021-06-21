get_community_stats <- function() {
  categories_data <- get_categories()

  total_users <-
    jsonlite::fromJSON(
      "https://community.rstudio.com/directory_items.json?period=all&order=posts_read"
    )[["meta"]][["total_rows_directory_items"]]

  total_tags <-
    jsonlite::fromJSON("https://community.rstudio.com/tags.json")$tags %>%
    dplyr::pull(n_distinct(id)) %>%
    length()

  list(
    total_posts = sum(categories_data$post_count),
    total_topics = sum(categories_data$topic_count),
    total_users = total_users,
    total_tags = total_tags
  )
}

get_categories <- function() {
  jsonlite::fromJSON("https://community.rstudio.com/categories.json")[[1]]$categories
}

get_top_users <- function(pages = 1) {
  base_url <-
    "https://community.rstudio.com/directory_items.json?period=all&order=likes_received&page="

  n_pages <- seq(from = 0, to = pages)

  urls <- stringr::str_glue("{base_url}{n_pages}")

  purrr::map_dfr(urls,
                 ~ jsonlite::fromJSON(.x, flatten = TRUE)$directory_items) %>%
    dplyr::select(
      id,
      user.username,
      user.name,
      user.avatar_template,
      user.title,
      likes_received,
      post_count
    ) %>%
    dplyr::as_tibble()
}
