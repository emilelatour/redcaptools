
#' @title
#' Count checkbox responses
#'
#' @description
#' Get summaary counts easily from checkbox style variables/columns in a REDCap
#' data set.
#'
#' @param data A tbl_df or data.frame
#' @param ... Checkbox column names. Supports `dplyr` `select_helpers`
#' @param check_text Character string of the response to count. Default is
#'   "Checked"
#' @param negate Boolean. If `TRUE`, the count is `!= check_text`. Default is
#'   `FALSE`
#' @param key_text Character string to name result column of the columns
#' @param value_text Character string to name the result column of counts
#' @param group Column name to `group_by`
#'
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr summarise_at
#' @importFrom dplyr vars
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang quo_is_null
#' @importFrom rlang quo_name
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#'
#' @return a tble_df
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble::tibble(
#'   id = c(1:100),
#'   group = sample(c("A", "B", "C"), size = 100, replace = TRUE),
#'   preferred_color___1 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___2 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___3 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___4 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___5 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE))
#'
#' count_checkboxes(data = df,
#'                  preferred_color___1:preferred_color___5)
#'
#' count_checkboxes(data = df,
#'                  preferred_color___1, preferred_color___5)
#'
#' count_checkboxes(data = df,
#'                  dplyr::starts_with("preferred_color"))
#'
#' count_checkboxes(data = df,
#'                  preferred_color___1:preferred_color___5,
#'                  check_text = "Unchecked",
#'                  key_text = "checkbox",
#'                  value_text = "count")
#'
#' count_checkboxes(data = df,
#'                  preferred_color___1:preferred_color___5,
#'                  group = group)
#'
#' count_checkboxes(data = df,
#'                  preferred_color___1:preferred_color___5,
#'                  negate = TRUE,
#'                  group = group)
count_checkboxes <- function(data,
                             ...,
                             check_text = "Checked",
                             negate = FALSE,
                             key_text = "field_name",
                             value_text = "n",
                             group = NULL) {

  vars <- rlang::enquos(...)
  group <- rlang::enquo(group)

  if (rlang::quo_is_null(group)) {

    data %>%
      summarise_at(.vars = vars(!!! vars),
                   .funs = list(~ check_count_func(., check_text, negate))) %>%
      tidyr::gather(.,
                    key = !! key_text,
                    value = !! value_text,
                    !!! vars) %>%
      mutate(nn = dplyr::pull(dplyr::count(data)))

  } else {

    data %>%
      group_by(!! group) %>%
      summarise_at(.vars = vars(!!! vars),
                   .funs = list(~ check_count_func(., check_text, negate))) %>%
      tidyr::gather(.,
                    key = !! key_text,
                    value = !! value_text,
                    !!! vars) %>%
      dplyr::left_join(.,
                       data %>%
                         group_by(!! group) %>%
                         dplyr::count(!! group, name = "nn"),
                       by = rlang::quo_name(group)
      )

  }
}


#### Helper function --------------------------------

check_count_func <- function(x,
                             check_text,
                             negate) {
  if (!negate) {
    sum(x == check_text, na.rm = TRUE)
  } else {
    sum(x != check_text, na.rm = TRUE)
  }
}
