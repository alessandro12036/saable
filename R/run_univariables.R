run_univariables <- function(df,
                             vars_of_interest,
                             save=T,
                             save_path="./univariable_regressions.docx") {
  starting <- T
  for (var_ in vars_of_interest) { # handles variables with one factor due to stratification
    if (length(unique(temp_df[[var_]]))<=1) {
      next
    }
    str_form <- glue("{outcome}~{var_}")
    m <- do.call("glm", list(as.formula(str_form), data=quote(df), family="binomial"))
    summ_m <- summary(m)
    coef_mat <- summ_m$coefficients
    var_names <- rownames(coef_mat)[2:dim(coef_mat)[1]]
    colnames_ <- colnames(coef_mat)
    temp_out_df <- coef_mat[2:dim(coef_mat)[1], 1:dim(coef_mat)[2]]
    row_names <- names(temp_out_df)

    if (is.null(dim(temp_out_df)[1])) {
      temp_out_df <- temp_out_df %>%
        as_tibble() %>%
        mutate(variable=row_names) %>%
        pivot_wider(names_from=variable,
                    values_from="value")
    }

    else {
      temp_out_df <- temp_out_df %>%
        as_tibble()
    }

    temp_out_df <- temp_out_df %>%
      mutate(OR=exp(Estimate)) %>%
      mutate(ci_low=exp(Estimate+qnorm(0.025)*`Std. Error`),
             ci_high=exp(Estimate+qnorm(0.975)*`Std. Error`)) %>%
      mutate(OR=glue("{round(OR, 2)} ({round(ci_low, 2)}-{round(ci_high, 2)})")) %>%
      select(-c(ci_low, ci_high)) %>%
      relocate(OR, .before=Estimate) %>%
      mutate(Estimate=round(Estimate, 2),
             `Std. Error`=round(`Std. Error`, 2),
             `z value`=round(`z value`, 2),
             `Pr(>|z|)`=round(`Pr(>|z|)`, 3))

    if (var_ %in% conts) {
      temp_out_df <- temp_out_df %>%
        mutate(Variable=var_names) %>%
        relocate(Variable, 1)
    }
    else {
      temp_out_df <- temp_out_df %>%
        mutate(Variable=var_names) %>%
        mutate(Variable=sapply(Variable, FUN=function(x) str_match(x, glue("{var_}(.*)"))[2])) %>%  # leaves only factor as the name
        relocate(Variable, 1)
    }

    if (starting) {
      out_df <- temp_out_df
      starting <- F
    }
    else {
      out_df <- out_df %>%
        rbind(temp_out_df)
    }
  }

  out_t <- out_df %>%
    gt() %>%
    tab_style(style=cell_text(weight="bold"),
              location=cells_body(columns=c("Variable", "Pr(>|z|)"),
                                  rows=`Pr(>|z|)`<0.05))

  if (save) {
    out_t %>%
      gtsave(filename = save_path)
  }

  return(list("table"=out_t,
              "df"=out_df))
}
