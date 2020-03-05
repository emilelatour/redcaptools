
#' @title
#' Count checkbox responses
#'
#' @description
#' Get summary counts easily from checkbox style variables/columns in a REDCap
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
#' @param data_labels A data frame or tibble with columns, `field_name` and
#'   `checkbox_choice`. Used to replace variable names with checkbox labels.
#'
#' @importFrom dplyr count
#' @importFrom dplyr everything
#' @importFrom dplyr group_by
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr summarise_at
#' @importFrom dplyr ungroup
#' @importFrom dplyr vars
#' @importFrom purrr map
#' @importFrom purrr map_int
#' @importFrom purrr pluck
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
#' #### Example 1 --------------------------------
#'
#' library(dplyr)
#' library(tibble)
#' library(tidyr)
#'
#' df <- tibble::tibble(
#'   id = c(1:100),
#'   group = sample(c("A", "B", "C"), size = 100, replace = TRUE),
#'   preferred_color___1 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___2 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___3 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___4 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___5 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE)) %>%
#'   mutate(group = factor(group,
#'                         levels = c("A", "B", "C")))
#'
#'
#' var_labels <- tibble::tribble(
#'   ~field_name, ~checkbox_choice,
#'   "preferred_color___1",           "Blue",
#'   "preferred_color___2",          "Green",
#'   "preferred_color___3",         "Orange",
#'   "preferred_color___4",            "Red",
#'   "preferred_color___5",         "Yellow"
#' )
#'
#' var_labels
#'
#' # Different ways of selecting variables
#' count_checkboxes(data = df,
#'                  preferred_color___1:preferred_color___5)
#'
#' count_checkboxes(data = df,
#'                  preferred_color___1, preferred_color___5)
#'
#' count_checkboxes(data = df,
#'                  dplyr::starts_with("preferred_color"))
#'
#'
#' # Different ways to select the "Unchecked" text instead
#' count_checkboxes(data = df,
#'                  preferred_color___1:preferred_color___5,
#'                  check_text = "Unchecked",
#'                  key_text = "checkbox",
#'                  value_text = "count")
#'
#'
#' df %>%
#'   count_checkboxes(data = .,
#'                    preferred_color___1:preferred_color___5,
#'                    negate = TRUE)
#'
#' # Can handle grouping
#' df %>%
#'   group_by(group) %>%
#'   count_checkboxes(data = .,
#'                    preferred_color___1:preferred_color___5)
#'
#' # Can also apply labels instead of variable names
#' count_checkboxes(data = df,
#'                  preferred_color___1:preferred_color___5,
#'                  negate = TRUE,
#'                  data_labels = var_labels)
#'
#'
#'
#' #### Example 2 --------------------------------
#'
#' library(dplyr)
#' library(tibble)
#' library(tidyr)
#'
#' df2 <- tibble::tibble(
#'   record_id = c(1:100),
#'   gender = sample(c("Female", "Male"),
#'                   size = 100,
#'                   replace = TRUE),
#'   gender_other = NA,
#'   age = sample(c(18:85),
#'                size = 100,
#'                replace = TRUE),
#'   education = sample(c("High-school", "College", "Graduate school"),
#'                      size = 100,
#'                      replace = TRUE),
#'   ethnicity = sample(c("Hispanic", "Non-hispanic"),
#'                      size = 100,
#'                      replace = TRUE),
#'   key = sample(c("race___1", "race___2", "race___3",
#'                  "race___4", "race___5", "race___6", "race___98"),
#'                size = 100,
#'                replace = TRUE),
#'   value = "Checked",
#'   income = sample(c(20000:120000),
#'                   size = 100,
#'                   replace = TRUE),
#'   marital_status = sample(c("Married", "Single"),
#'                           size = 100,
#'                           replace = TRUE),
#'   survey_complete = sample(c("Complete", "Not complete"),
#'                            size = 100,
#'                            replace = TRUE)) %>%
#'   mutate(key = factor(key,
#'                       levels = c("race___1", "race___2", "race___3",
#'                                  "race___4", "race___5", "race___6",
#'                                  "race___98"))) %>%
#'   tidyr::spread(.,
#'                 key = "key",
#'                 value = "value") %>%
#'   mutate_at(.vars = vars(race___1:race___98),
#'             .funs = list(~ tidyr::replace_na(., "Unchecked")))
#'
#'
#' data_labels <- tibble::tribble(
#'         ~field_name,                      ~checkbox_choice,
#'         "record_id",                                    NA,
#'            "gender",                                    NA,
#'      "gender_other",                                    NA,
#'               "age",                                    NA,
#'         "education",                                    NA,
#'         "ethnicity",                                    NA,
#'          "race___1",                               "White",
#'          "race___2",           "Black or African-American",
#'          "race___3",    "American Indian or Alaska Native",
#'          "race___4",                               "Asian",
#'          "race___5", "Native Hawaiian or Pacific Islander",
#'          "race___6",                               "Other",
#'         "race___98",                "Prefer not to answer",
#'        "race_other",                                    NA,
#'            "income",                                    NA,
#'    "marital_status",                                    NA,
#'   "survey_complete",                                    NA
#'   )
#'
#'
#'
#'
#' count_checkboxes(data = df2,
#'                  race___1:race___98)
#'
#' count_checkboxes(data = df2,
#'                  dplyr::starts_with("race"))
#'
#' count_checkboxes(data = df2,
#'                  dplyr::starts_with("race___"))
#'
#' df2 %>%
#'   group_by(gender) %>%
#'   count_checkboxes(data = .,
#'                    race___1:race___98)
#'
#' df2 %>%
#'   group_by(gender, education) %>%
#'   count_checkboxes(data = .,
#'                    race___1:race___98)
#'
#'
#'
#' # With labels
#'
#' count_checkboxes(data = df2,
#'                  race___1:race___98,
#'                  data_labels = data_labels)
#'
#' df2 %>%
#'   group_by(gender, education) %>%
#'   count_checkboxes(data = .,
#'                    race___1:race___98,
#'                    data_labels = data_labels)

count_checkboxes <- function(data,
                             ...,
                             check_text = "Checked",
                             negate = FALSE,
                             key_text = "field_name",
                             value_text = "n",
                             data_labels = NULL) {

  vars <- rlang::enquos(...)

  #### Count the check boxes --------------------------------

  cbox_cnt <- data %>%
    # tidyr::nest(.data = .,
    #             data = dplyr::everything()) %>%
    tidyr::nest(.data = .) %>%
    mutate(nn = purrr::map_int(.x = data,
                               .f = ~ dim(.x)[[1]])) %>%
    mutate(res = purrr::map(.x = data,
                            .f = ~ .wrap_count(.x,
                                               vars,
                                               check_text,
                                               negate,
                                               key_text,
                                               value_text))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols = c(res)) %>%
    dplyr::select(-nn,
                  nn) %>%
    mutate(percent = !! rlang::sym(value_text) / nn)


  if (dplyr::is_grouped_df(cbox_cnt)) {
    cbox_cnt <- dplyr::ungroup(cbox_cnt)
  }


  #### Apply labels if avaiable --------------------------------

  if (is.null(data_labels)) {

    cbox_cnt

  } else {

    new_col_name <- gsub("___.*$", "", purrr::pluck(cbox_cnt,
                                                    # !! rlang::sym(key_text),
                                                    key_text,
                                                    1))

    cbox_cnt <- .adorn_checkbox_labels(cbox_cnt,
                                       data_labels,
                                       new_col_name) %>%
      dplyr::select(dplyr::group_vars(data),
                    dplyr::everything())
  }

  return(cbox_cnt)

}


#### Helper function --------------------------------

## check_count_func ----------------
# Helper function to count the text in the column that represents a "check".
# Typically from REDCap this is just "Checked" or "Unchecked".
.check_count_func <- function(x,
                              check_text,
                              negate) {
  if (!negate) {
    sum(x == check_text, na.rm = TRUE)
  } else {
    sum(x != check_text, na.rm = TRUE)
  }
}

## wrap_count ----------------
# A wrapper to help counting checkboxes go a little more smoothly. An extra
# step, but makes things easier.
.wrap_count <- function(df,
                        vars,
                        check_text,
                        negate,
                        key_text,
                        value_text) {

  df %>%
    summarise_at(.vars = vars(!!! vars),
                 .funs = list(~ .check_count_func(., check_text, negate))) %>%
    tidyr::gather(.,
                  key = !! key_text,
                  value = !! value_text,
                  !!! vars)
}

## adorn_checkbox_labels ----------------
# If there are labels provided to the function then it will replace the variable
# names with the appropriate labels.
# Requires a data frame or tibble with two columns: (1) field_name and (2)
# checkbox_choice

.adorn_checkbox_labels <- function(cbox_cnt,
                                   data_labels,
                                   new_col_name) {

  if (!"field_name" %in% names(data_labels)) {

    stop("data_labels must contain a column named field_name")

  } else if (!"checkbox_choice" %in% names(data_labels)) {

    stop("data_labels must contain a column named checkbox_choice")

  } else {

    cbox_cnt %>%
      dplyr::left_join(.,
                       data_labels[, c("field_name", "checkbox_choice")],
                       by = "field_name") %>%
      dplyr::select(checkbox_choice,
                    dplyr::everything(),
                    -field_name
      ) %>%
      dplyr::rename(
        !! rlang::sym(new_col_name) := "checkbox_choice"
      )

  }

}




#### Old version --------------------------------

# count_checkboxes <- function(data,
#                              ...,
#                              check_text = "Checked",
#                              negate = FALSE,
#                              key_text = "field_name",
#                              value_text = "n",
#                              group = NULL) {
#
#   vars <- rlang::enquos(...)
#   group <- rlang::enquo(group)
#
#   if (rlang::quo_is_null(group)) {
#
#     data %>%
#       summarise_at(.vars = vars(!!! vars),
#                    .funs = list(~ check_count_func(., check_text, negate))) %>%
#       tidyr::gather(.,
#                     key = !! key_text,
#                     value = !! value_text,
#                     !!! vars) %>%
#       mutate(nn = dplyr::pull(dplyr::count(data)))
#
#   } else {
#
#     data %>%
#       group_by(!! group) %>%
#       summarise_at(.vars = vars(!!! vars),
#                    .funs = list(~ check_count_func(., check_text, negate))) %>%
#       tidyr::gather(.,
#                     key = !! key_text,
#                     value = !! value_text,
#                     !!! vars) %>%
#       dplyr::left_join(.,
#                        data %>%
#                          group_by(!! group) %>%
#                          dplyr::count(!! group, name = "nn"),
#                        by = rlang::quo_name(group)
#       )
#
#   }
# }
