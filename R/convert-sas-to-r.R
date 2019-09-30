#' @title
#' Convert REDCap cloud SAS code to R script
#'
#' @description
#' REDCap cloud only provides SAS code along with a csv as of the creation of
#' this script. I want to use R like with REDCap academic. Relying heavily on
#' the package
#' \href{https://cran.r-project.org/web/packages/datapasta/vignettes/how-to-datapasta.html}{datapasta}
#' by Miles McBain, this function lets you take the SAS code, provide it to the
#' function, and get a data frame of paste-able R script to use with a csv.
#' Also, makes important use of the
#' \href{https://cran.r-project.org/web/packages/clipr/index.html}{clipr}
#' package.
#'
#' @param sas_code_text A string vector of SAS code that has been pasted using
#'   \href{https://cran.r-project.org/web/packages/datapasta/vignettes/how-to-datapasta.html}{datapasta}
#'   package "Paste as vector" or "Paste as vector (vertical)".
#'
#' @importFrom clipr write_clip
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom graphics text
#' @importFrom purrr map
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom tibble tibble
#' @importFrom tidyr fill
#' @importFrom tidyr nest
#' @importFrom tidyr separate
#' @importFrom utils data
#'
#' @return A tbl_df
#' @export
#'
#' @examples \dontrun{
#'
#' library(clipr)
#' library(datapasta)
#' library(dplyr)
#'
#' #### Step 1 --------------------------------
#'
#' # Copy the text to your clipboard from the SAS script that REDCap cloud gives
#' # you, i.e. highlight and Control+C or Command+C.
#'
#' #### Step 2 --------------------------------
#'
#' # Use the datapasta package to paste the text in the clipboard as a vector or as
#' # a vertical vector.
#'
#' # datapasta::vector_paste_vertical() or datapasta::vector_paste()
#'
#' text_from_sas <- c("value $ SEX_ 1=\"Female\"",
#'                    "2=\"Male\";",
#'                    "value $ RACE_ 1=\"American Indian or Alaska Native\"",
#'                    "2=\"Asian\"",
#'                    "3=\"Black of African American\"",
#'                    "4=\"Native Hawaiian or other Pacific Islander\"",
#'                    "5=\"Multiracial\"",
#'                    "6=\"White\"",
#'                    "999=\"Unknown\"",
#'                    "9999=\"Not Reported\";",
#'                    "value $ ETHNICITY_ 1=\"Hispanic or Latino\"",
#'                    "2=\"Not Hispanic or Latino\"",
#'                    "999=\"Unknown\";",
#'                    "value $ REG_ELG_IND_ 1=\"Yes\"",
#'                    "2=\"No\";")
#'
#' #### Step 3 --------------------------------
#'
#' # Use the function to convert the SAS code to R
#'
#' text_for_rscript <- convert_sas_to_r(text_from_sas)
#'
#' text_for_rscript
#'
#' #### Step 4 --------------------------------
#'
#' # Pick a column and use the clipr package to copy it to the clipboard
#'
#' text_for_rscript %>%
#'   dplyr::pull(fct_and_lvl_txt) %>%
#'   clipr::write_clip()
#'
#' # Ctrl+V
#' # data$SEX = factor(data$SEX,levels=c("1", "2"))
#' # data$RACE = factor(data$RACE,levels=c("1", "2", "3", "4", "5", "6", "999", "9999"))
#' # data$ETHNICITY = factor(data$ETHNICITY,levels=c("1", "2", "999"))
#' # data$REG_ELG_IND = factor(data$REG_ELG_IND,levels=c("1", "2"))
#'
#' text_for_rscript %>%
#'   dplyr::pull(fct_txt) %>%
#'   clipr::write_clip()
#'
#' # Ctrl+V
#' # data$SEX = factor(data$SEX)
#' # data$RACE = factor(data$RACE)
#' # data$ETHNICITY = factor(data$ETHNICITY)
#' # data$REG_ELG_IND = factor(data$REG_ELG_IND)
#'
#' text_for_rscript %>%
#'   dplyr::pull(lvl_txt) %>%
#'   clipr::write_clip()
#'
#' # Ctrl+V
#' # levels(data$SEX)=c("Female", "Male"))
#' # levels(data$RACE)=c("American Indian or Alaska Native", "Asian", "Black of African American", "Native Hawaiian or other Pacific Islander", "Multiracial", "White", "Unknown", "Not Reported"))
#' # levels(data$ETHNICITY)=c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown"))
#' # levels(data$REG_ELG_IND)=c("Yes", "No"))
#'
#' }

convert_sas_to_r <- function(sas_code_text) {

  #### Take the vector and make a tibble --------------------------------

  fct_txt <- tibble::tibble(text = sas_code_text)

  #### Remove escape character and ; --------------------------------

  fct_txt <- fct_txt %>%
    mutate(
      # Remove escape character
      text = stringr::str_replace_all(string = text,
                                      pattern = '\"',
                                      replacement = ""),
      # Remove ;
      text = stringr::str_replace_all(string = text,
                                      pattern = ';',
                                      replacement = ""),
      # Remove value $
      text = stringr::str_replace_all(string = text,
                                      pattern = "value \\$",
                                      replacement = ""),
      # Trim whitespace
      text = stringr::str_trim(text))


  #### Split text --------------------------------

  fct_txt <- fct_txt %>%
    tidyr::separate(data =.,
                    col = text,
                    into = c("text", "label"),
                    sep = "=") %>%
    tidyr::separate(data =.,
                    col = text,
                    into = c("var_name", "value"),
                    sep = " ",
                    fill = "left")


  #### Fill in var_names --------------------------------

  fct_txt <- fct_txt %>%
    tidyr::fill(.,
                var_name,
                .direction = "down") %>%
    mutate(var_name = gsub("_$", "", var_name),
           var_name = gsub("_$", "", var_name))


  #### Make levels and labels --------------------------------

  fct_txt <- fct_txt %>%
    group_by(var_name) %>%
    tidyr::nest() %>%
    mutate(lvls = purrr::map(.x = data,
                             .f = ~ dplyr::pull(.x,
                                                value)),
           lbls = purrr::map(.x = data,
                             .f = ~ dplyr::pull(.x,
                                                label))) %>%
    mutate(fct_and_lvl_txt = paste0("data$", var_name, " = factor(data$", var_name, ",levels=", lvls, ")"),
           fct_txt = paste0("data$", var_name, " = factor(data$", var_name, ")"),
           lvl_txt = paste0("levels(data$", var_name, ")=", lbls, ")")) %>%
    ungroup() %>%
    dplyr::select(fct_and_lvl_txt, fct_txt, lvl_txt)


  #### End of function --------------------------------

  fct_txt

}




