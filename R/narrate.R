#' Generate Documentation from data.txt
#'
#' Generate a set of markdown files based on a `data.txt` file.
#'
#' @param datatxt URL of the `data.txt` file.
#' @param format Output format, only `"markdown"` is currently supported.
#' @param output_dir Output directory of documentation articles.
#'
#' @export
narrate <- function(datatxt, format = "markdown", output_dir = "content/dataset") {
  temp_board_name <- basename(tempfile())
  pins::board_register_datatxt(temp_board_name, datatxt)
  yml <- yaml::read_yaml(datatxt)
  narratives <- yml %>%
    purrr::map(narrate_dataset, temp_board_name, dirname(datatxt)) %>%
    stats::setNames(yml %>% purrr::map("name"))

  fs::dir_create(output_dir)
  narratives %>%
    purrr::iwalk(~ cat(.x, file = fs::path(output_dir, paste0(.y, ".md"))))
}

narrate_dataset <- function(dataset, board, base_url) {

  yml <- try_parse_datatxt(dataset$path, base_url)

  df <- pins::pin_get(dataset$name, board = board)
  skim_summary <- utils::capture.output(skimr::kable(skimr::skim(df))) %>%
    glue::glue_collapse(sep = "\n")

  title <- dataset$title %||% dataset$name

  result <- glue::glue(
    "
---
title: {title}
---

## Description

{dataset$description}
    ")

  if (!is.null(yml$columns)) {
    col_content <- yml$columns %>%
      purrr::map(function(column) {
        type <- if (!is.null(column$type)) glue::glue("&lt;{column$type}&gt; ") else ""
        glue::glue("- `{column$name}` ", type, "{column$description}")
      }) %>%
      glue::glue_collapse("\n")

    result <- glue::glue(
      result,
      "

## Columns

{col_content}
")
  }
  result <- glue::glue(
    result,
    "

## Summary

{skim_summary}
")

  if (!is.null(yml$source)) {
    result <- glue::glue(
      result,
"

## Source

{yml$source}
")
  }

  result
}

try_parse_datatxt <- function(path, base_url) {
  path <- if (grepl("^http(s?)://", path)) path else paste0(base_url, "/", path)
  splits <- strsplit(paste0(path, "/data.txt"), "(?:(?<=http://)|(?<=https://))",
                     perl = TRUE)[[1]]
  datatxt <- paste0(splits[[1]], gsub("//", "/", splits[[2]]))
  purrr::possibly(yaml::read_yaml, NULL)(datatxt)
}
