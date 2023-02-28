
#' @title
#' Cobine checkbox style variables
#'
#' @description
#' In REDCap, checkbox style variables are ones where more than one choice is
#' allowed and the responses appear over multiple columns with values "Checked"
#' or "Unchecked" that have names with a similar prefix. This function takes the
#' columns specified by the prefix and combine them into a single column.
#'
#' @param data A tbl_df or data.frame
#' @param id_var Name of the column for each subject/participant/observational
#'   unit
#' @param prefix Character string to select variables that start with this
#'   pattern
#' @param check_text Character string of the response to count. Default is
#'   "Checked"
#' @param sep Character string to collapse the responses by. Default is ", "
#' @param data_labels A data frame or tibble with columns, `field_name` and
#'   `checkbox_choice`. Used to replace variable names with checkbox labels.
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom glue glue
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
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
#'
#' combine_checkboxes(data = df,
#'                    id_var = id,
#'                    prefix = "preferred_color___")
#'
#' combine_checkboxes(data = df,
#'                    id_var = id,
#'                    prefix = "preferred_color___",
#'                    data_labels = var_labels)
#'
#' combine_checkboxes(data = df,
#'                    id_var = id,
#'                    prefix = "preferred_color___",
#'                    data_labels = var_labels,
#'                    sep = "-")
#'
#'
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
#' df2
#'
#' combine_checkboxes(data = df2,
#'                    id_var = record_id,
#'                    prefix = "race___")
#'
#' combine_checkboxes(data = df2,
#'                    id_var = record_id,
#'                    prefix = "race___",
#'                    data_labels = data_labels)

combine_checkboxes <- function(data,
                               id_var,
                               prefix,
                               check_text = "Checked",
                               sep = ", ",
                               data_labels = NULL) {

  id_var <- rlang::enquo(id_var)

  col_name <- glue::glue("{prefix}combined")

  value <- NULL

  ids_df <- data %>%
    dplyr::select(!! id_var)

  # Select the id variable and the checkboxes
  check_boxes <- data %>%
    dplyr::select(!! id_var, dplyr::starts_with(prefix)) %>%
    tidyr::pivot_longer(data = .,
                        cols = c(dplyr::starts_with(prefix)),
                        names_to = "field_name",
                        values_to = "value")

  if (is.null(data_labels)) {

    check_boxes %>%
      mutate(value = dplyr::if_else(value == "Checked",
                                    value,
                                    NA_character_)) %>%
      dplyr::distinct(!! id_var, field_name, value, .keep_all = TRUE) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::select(-value) %>%
      group_by(!! id_var) %>%
      summarise(!! col_name := stringr::str_c(field_name,
                                              collapse = sep),
                .groups = "drop") %>%
      dplyr::left_join(ids_df,
                       .,
                       by = rlang::quo_name(id_var))


  } else {

    if (!"field_name" %in% names(data_labels)) {

      stop("data_labels must contain a column named field_name")

    } else if (!"checkbox_choice" %in% names(data_labels)) {

      stop("data_labels must contain a column named checkbox_choice")

    } else {

      # Join with the data_labels
      check_boxes <- data_labels %>%
        dplyr::select(field_name, checkbox_choice) %>%
        dplyr::filter(stringr::str_detect(field_name, prefix)) %>%
        dplyr::right_join(check_boxes,
                          .,
                          by = "field_name")

      check_boxes %>%
        mutate(value = dplyr::if_else(value == "Checked",
                                      checkbox_choice,
                                      NA_character_)) %>%
        dplyr::distinct(!! id_var, field_name, value, .keep_all = TRUE) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::select(-field_name) %>%
        group_by(!! id_var) %>%
        summarise(!! col_name := stringr::str_c(value,
                                                collapse = sep),
                  .groups = "drop") %>%
        dplyr::left_join(ids_df,
                         .,
                         by = rlang::quo_name(id_var))

    }



  }
}
