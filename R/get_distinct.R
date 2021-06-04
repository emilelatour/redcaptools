


#' @title
#' Get distinct values on different rows
#'
#' @description
#' A wrapper around `dplyr::group_by %>% dplyr::fill %>% dplyr::distinct %>%
#' dplyr::ungroup()`. I use this all the time when I am working with data from
#' REDCap. This is meant to make my life easier and clean up my code.
#'
#' @param data A data.frame or tibble.
#' @param id Unquoted name of grouping variable. Typically a subject or record
#'   id.
#' @param ... A selection of columns. If empty, nothing happens. You can supply
#'   bare variable names, select all variables between x and z with x:z, exclude
#'   y with -y. For more selection options, see the `dplyr::select()`
#'   documentation.
#' @param fill_direction Direction in which to fill missing values. Currently
#'   either "down" (the default), "up", "downup" (i.e. first down and then up)
#'   or "updown" (first up and then down).
#'
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom tidyr fill
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#'
#' @return
#' A tbl_df
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' df <- tibble::tribble(
#'   ~id,            ~form, ~age,     ~sex,               ~ethnicity, ~trt,
#'   1L,            "age",  25L,       NA,                       NA,   NA,
#'   1L,            "sex",   NA,   "Male",                       NA,   NA,
#'   1L, "race_ethnicity",   NA,       NA,     "Hispanic or Latino",   NA,
#'   1L,      "treatment",   NA,       NA,                       NA,  "A",
#'   2L,            "age",  32L,       NA,                       NA,   NA,
#'   2L,            "sex",   NA, "Female",                       NA,   NA,
#'   2L, "race_ethnicity",   NA,       NA, "Not Hispanic or Latino",   NA,
#'   2L,      "treatment",   NA,       NA,                       NA,  "B"
#' )
#'
#' df
#'
#' df %>%
#'   get_distinct(data = .,
#'                id = id,
#'                age, sex, ethnicity, trt)
#' df %>%
#'   get_distinct(data = .,
#'                id = id,
#'                dplyr::starts_with("ag"))
#'
#' df %>%
#'   get_distinct(data = .,
#'                id = id,
#'                dplyr::one_of("age", "sex"))

get_distinct <- function(data,
                         id,
                         ...,
                         fill_direction = "downup") {

  # id <- rlang::enquo(id)
  # vars <- rlang::enquos(...)
  #
  # data %>%
  #   dplyr::group_by(!! id) %>%
  #   tidyr::fill(data = .,
  #               !!! vars,
  #               .direction = fill_direction) %>%
  #   dplyr::distinct(!! id,
  #                   !!! vars) %>%
  #   dplyr::ungroup()

  id <- rlang::enquo(id)

  data %>%
    dplyr::group_by(!! id) %>%
    tidyr::fill(data = .,
                ...,
                .direction = fill_direction) %>%
    dplyr::select(!! id,
                  ...) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

}

