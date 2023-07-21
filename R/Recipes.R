
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @title A function that shows filtered vegan recipes
#' @param sweets A named list of recipes.
#' @return A character vector indicating "yes" if the specific ingredients are found, and "no" otherwise.
#' @export
#'
#' @example
#' vegan_recipes(sweets)


library(dplyr)
library(tidyverse)



sweets <-   #Auswahl an Rezepten/Snackideen unter sweets
  sweets <- c(brownie = (c("eggs", "flour", "sugar", "dark chocolate", "cacao powder", "butter", "salt", "vanilla")),
              vegan_brownie = (c("butternut pumkin", "melted chocolate", "coconut oil")),
              filled_dates = (c("dates", "nut butter")),
              black_bean_brownie = (c("eggs", "black beans", "melted chocolate", "cane sugar", "cacao powder", "baking powder", "coconut oil")),
              acai_bowl = (c("frozen acai", "apple juice", "bananas", "strawberries")),
              muffins = (c("flour", "sugar", "milk", "vanilla", "baking powder", "butter", "dried cherries"))
  )
#Lösung aus Suche, Funktion 1: rausfiltern, welche Rezepte vegan sind
vegan_recipes_filter <- function(sweets) {
  sweets_tbl <- enframe(sweets, name = "treat", value = "ingredients")
  vegan_recipes <- sweets_tbl %>%
    filter(!grepl("eggs", ingredients, ignore.case = TRUE) &
             !grepl("butter", ingredients, ignore.case = TRUE) &
             !grepl("milk", ingredients, ignore.case = TRUE))
  return(vegan_recipes)
}


#' @title A function that shows the needed ingredients for the choosen recipe
#' @param sweets A named list of recipes.
#' @return A character vector indicating "yes" if the specific ingredients are found, and "no" otherwise.
#' @export
#'
#' @example
#' needed_ingredients("filled_dates", sweets)


needed_ingredients <- function(name_of_recipe, sweets) {
  # Convert input to lowercase for case-insensitive search
  name_of_recipe <- tolower(name_of_recipe)

  # Check if the recipe name exists
  if (name_of_recipe %in% names(sweets)) {
    return(sweets[[name_of_recipe]])
  } else {
    return(NULL)
  }
}

