
#' @title
#' Parse labels from REDCap academic
#'
#' @description
#' Take the field names and field labels that are given in the script from
#' REDCap academic and parse them into a `tbl_df` with columns for all of the
#' information. This is usefule because later on these can be used to make
#' pretty (or human readable) field names and feild labels in tables with the
#' REDCap variables. Also, parses the checkbox choices from radio style
#' variables in the data base.
#'
#' Best pracitce is to copy the text from the REDCap script and paste it as a
#' veritcal vector using the `datapasta` package by Miles McBain.
#' (\url{https://cran.r-project.org/web/packages/datapasta/index.html}). See
#' example.
#'
#' @param data A data frame or tibble with a single column with the text from
#'   the R file that is exported from REDCap.
#' @param text Name of the column. Default is `text`.
#' @param checkbox_text Character string of text to remove from the labels for
#'   checkbox style variables. Default is `c("Choose all that apply.", "Mark all
#'   that apply.")`
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom stringr str_locate
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_trim
#' @importFrom tibble tibble
#'
#' @return
#' A `tbl_df` with the following columns:
#' \describe{
#'   \item{field_name}{REDCap variable names}
#'   \item{field_label}{REDCap labels for the variables}
#'   \item{clean_label}{REDCap labels that have been cleaned to remove extra text}
#'   \item{checkbox_choice}{Options for checkbox type variables}
#'   \item{is_checkbox}{TRUE/FALSE is this variable a checkbox}
#' }
#'
#' @export
#'
#' @examples
#' #### Step 1. Paste labels --------------------------------
#'
#' # Copy the labels from the REDCap script.
#' # Using the data pasta add-in, paste as a vertical vector.
#' # Add the necessary syntax to make this a tbl_df.
#'
#' data_labels <- tibble::tibble(
#'   text = # Paste as vertical vector
#'     c("label(data$record_id)=\"Record ID\"",
#'       "label(data$gender)=\"What is your gender?\"",
#'       "label(data$gender_other)=\"Prefer to self-describe:\"",
#'       "label(data$age)=\"What is your age?\"",
#'       "label(data$education)=\"What is the highest level of education you have completed? Mark only one.\"",
#'       "label(data$ethnicity)=\"Would you describe yourself as being of Hispanic or Latino/a origin or descent?\"",
#'       "label(data$race___1)=\"How would you describe your race? Mark all that apply. (choice=White)\"",
#'       "label(data$race___2)=\"How would you describe your race? Mark all that apply. (choice=Black or African-American)\"",
#'       "label(data$race___3)=\"How would you describe your race? Mark all that apply. (choice=American Indian or Alaska Native)\"",
#'       "label(data$race___4)=\"How would you describe your race? Mark all that apply. (choice=Asian)\"",
#'       "label(data$race___5)=\"How would you describe your race? Mark all that apply. (choice=Native Hawaiian or Pacific Islander)\"",
#'       "label(data$race___6)=\"How would you describe your race? Mark all that apply. (choice=Other)\"",
#'       "label(data$race___98)=\"How would you describe your race? Mark all that apply. (choice=Prefer not to answer)\"",
#'       "label(data$race_other)=\"Other (tell us):\"",
#'       "label(data$income)=\"What was your total household income (before taxes) last year (2018)? Your best estimate is fine.\"",
#'       "label(data$marital_status)=\"What is your marital status?\"",
#'       "label(data$survey_complete)=\"Complete?\"")
#'
#' )
#'
#' #### Step 2. Make the tbl_df with the labels --------------------------------
#'
#' data_labels <- data_labels %>%
#'   parse_field_labels_academic(.)
#'
#' data_labels

parse_field_labels_academic <- function(data,
                                        text = text,
                                        checkbox_text = c("Choose all that apply.",
                                                          "Mark all that apply.")) {

  # Data = the tbl_df with the text from REDCap
  # text = the name of the column with the text/information to parse

  text <- rlang::enquo(text)
  checkbox_text <- paste0(checkbox_text, collapse = "|")

  # Separate the field_name and the field_label
  res_df <- data %>%
    mutate(field_label = .right(!! text, "$"),
           field_name = .left(field_label, "\\)="),
           field_label = .right(field_label, "\\)="),
           field_label = .right(field_label, "="),
           field_label = stringr::str_sub(field_label,
                                          start = 2,
                                          end = -2),
           field_label = stringr::str_trim(field_label, side = "both"),
           NULL)

  # Identify checkbox variables
  res_df <- res_df %>%
    # mutate(is_checkbox = dplyr::if_else(stringr::str_detect(field_label,
    #                                                         "\\(choice="),
    #                                     1, 0))
    mutate(is_checkbox = stringr::str_detect(field_label,
                                             "\\(choice="))

  # Clean up the text in field_label
  res_df <- res_df %>%
    mutate(clean_label = dplyr::if_else(is_checkbox,
                                        .make_clean_label(field_label,
                                                          checkbox_text),
                                        NA_character_))

  # Extract the checkbox text
  res_df <- res_df %>%
    mutate(checkbox_choice = dplyr::if_else(is_checkbox,
                                        .make_checkbox_text(field_label),
                                        NA_character_))

  res_df %>%
    dplyr::select(field_name,
                  field_label,
                  clean_label,
                  checkbox_choice,
                  is_checkbox)

}

#### User defined functions --------------------------------

## Right ----------------
# Extracts the string to the right of a given character

.right <- function(string, char) {

  special <- pattern <- "/|:|\\?|<|>|\\|\\\\|\\*\\."

  if (grepl(char, special)) {
    esc <- "\\"
  } else {
    esc <- ""
  }

  # substr(string,
  #        start = unlist(gregexpr(paste0(esc, char), string)) + 1,
  #        stop = nchar(string))

  stringr::str_sub(string,
                   start = stringr::str_locate(string,
                                               paste0(esc, char))[, 1] + 1,
                   end = nchar(string))

}

## Left ----------------
# Extracts the string to the left of a given character

.left <- function(string, char) {

  special <- pattern <- "/|:|\\?|<|>|\\|\\\\|\\*\\."

  if (grepl(char, special)) {
    esc <- "\\"
  } else {
    esc <- ""
  }

  # substr(string,
  #        start = 1,
  #        stop = unlist(gregexpr(paste0(esc, char), string)) - 1)

  stringr::str_sub(string,
                   start = 1,
                   end = stringr::str_locate(string,
                                             paste0(esc, char))[, 1] - 1)
}

## Clean the labels ----------------
# Used to remove the extra text from the field_label
.make_clean_label <- function(x,
                              checkbox_text) {

  .left(x, "\\(choice=") %>%
    stringr::str_replace_all(.,
                             checkbox_text, "") %>%
    stringr::str_trim(.,
                      side = "both")
}

## Checkbox text ----------------
# Used to clean up the text in checkboxes

.make_checkbox_text <- function(x) {

  .right(x, "\\(choice=")%>%
    stringr::str_trim(., side = "both")%>%
    stringr::str_sub(.,
                     start = 8,
                     end = -2)
}
