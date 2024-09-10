

get_var_type <- function(df, x, cat_threshold=10) {
  sym_x <- sym(x)
  mods <- df %>% 
    filter(!is.na(!!sym_x)) %>% 
    distinct(!!sym_x) %>% 
    pull()
  
  n_mods <- mods %>% 
    length()
  
  if (n_mods <= 2) {
    if (1 %in% mods | TRUE %in% mods | "TRUE" %in% mods) {
      return("boolean")
    }
    else {
      return("dichotomous")
    }
  }
  else if (n_mods <= cat_threshold) {
    return("categorical")
  }
  
  else {
    return("continuous")
  }
}