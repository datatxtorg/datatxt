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
  site_name <- urltools::domain(datatxt)
  pins::board_register_datatxt(site_name, datatxt)
  yml <- yaml::read_yaml(datatxt)
  narratives <- yml %>%
    purrr::map(narrate_dataset, site_name) %>%
    stats::setNames(yml %>% purrr::map("name"))

  fs::dir_create(output_dir)
  narratives %>%
    purrr::iwalk(~ cat(.x, file = fs::path(output_dir, paste0(.y, ".md"))))
}

narrate_dataset <- function(dataset, board) {

  df <- pins::pin_get(dataset$name, board = board)
  skim_summary <- utils::capture.output(skimr::kable(skimr::skim(df))) %>%
    glue::glue_collapse(sep = "\n")

  title <- dataset$title %||% dataset$name
  glue::glue(
    "
---
title: {title}
---

## Description

{dataset$description}

## Summary

{skim_summary}
")
}
