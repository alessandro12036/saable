# TODO: add comparison mechanism
# TODO: add mean (sd) option for continuous

library(glue)
library(gt)
library(tidyverse)
library(knitr)
library(kableExtra)

# need this for correctly displaying latex output
# \usepackage{lmodern}
# \usepackage[T1]{fontenc}

summ_table_maker <- function(df,
                             vars_,
                             var_types,
                             refs,
                             factors=NULL,
                             labels=NULL,
                             strat_var=NULL,
                             cat_threshold=10,
                             show_null=F, # not sure it's worth it
                             prec=2,
                             comparisons=F,
                             var_new_names=NULL,
                             save_as_latex=F,
                             file_path=".",
                             file_name="summary_table",
                             table_package="gt",
                             missing_value_not="--",
                             gt_format="tex",
                             indent_size="0.5cm"){
  starting_flag <- T
  rows_to_indent <- c()
  row_counter <- 1

  n_tot <- dim(df)[1]

  if (!base::missing(strat_var)) {

    sym_strat_var <- sym(strat_var)
    sym_strat_type <- get_var_type(df, sym_strat_var)

    # just to get consistent logical booleans
    if (sym_strat_type=="boolean") {
      df <- df %>%
        mutate(!!sym_strat_var:=as.logical(!!sym_strat_var)) %>%
        mutate(!!sym_strat_var:=case_when(!!sym_strat_var~"Yes",
                                          !!sym_strat_var==F~"No",
                                          .default="Missing"))
    }

    tots_strat_var <- df %>%
      group_by(!!sym_strat_var) %>%
      summarise(n=n()) %>%
      pivot_wider(names_from=strat_var,
                  values_from=n) %>%
      mutate(Total=n_tot)

    tot_row <- tots_strat_var %>%
      mutate(Characteristics="Totals")
  }

  for (var_ in vars_) {
    print(var_)
    if (!var_ %in% names(var_types)) {
      var_types[[var_]] <- get_var_type(df, var_, cat_threshold)
    }
    var_type <- var_types[[var_]]
    sym_var <- sym(var_)

    if (!base::missing(var_new_names) & var_ %in% names(var_new_names)) {
      var_new_name <- var_new_names[[var_]]
      sym_var_new_name <- sym(var_new_name)
    }

    else {
      var_new_name <- var_
      sym_var_new_name <- sym_var
    }

    if (var_type %in% c("boolean", "dichotomous", "categorical")) {

      if (!base::missing(strat_var) & comparisons) {
        temp_table <- table(df[[strat_var]], df[[var_]])
        out_test <- chisq.test(temp_table)
        p_value <- round(out_test$p.value, prec)
      }

      temp_df <- df %>%
        group_by(!!sym_var) %>%
        summarise(n=n()) %>%
        mutate(perc_n=round(n/n_tot*100, prec)) %>%
        mutate(label=ifelse(is.na(n),
                            NA,
                            glue("{n} ({perc_n}%)"))) %>%
        select(!!sym_var, label) %>%
        rename(Characteristics=!!sym_var,
               Value=label)

      if (!base::missing(strat_var)) {
        temp_df_strat <- df %>%
          group_by(!!sym_strat_var, !!sym_var) %>%
          summarise(n=n()) %>%
          rowwise() %>%
          mutate(perc_n=round(n/tots_strat_var[[!!sym_strat_var]]*100, prec)) %>%
          ungroup() %>%
          mutate(label=ifelse(is.na(n),
                              NA,
                              glue("{n} ({perc_n}%)"))) %>%
          select(!!sym_strat_var, !!sym_var, label) %>%
          pivot_wider(names_from=strat_var,
                      values_from=label) %>%
          rename(Characteristics=!!sym_var)

        temp_df <- temp_df_strat %>%
          left_join(temp_df, by="Characteristics") %>%
          rename(Total=Value) #%>%
        #rbind(tot_row)

        if (!base::missing(strat_var) & comparisons) {
          temp_df <- temp_df %>%
            mutate(P=p_value)
        }
      }

      if (!show_null) {
        temp_df <- temp_df %>%
          filter(Characteristics!="NA")
      }
      if (var_type=="boolean") {
        temp_df <- temp_df %>%
          mutate(Characteristics = as.logical(Characteristics)) %>%
          mutate(Characteristics=case_when(Characteristics~"Yes",
                                           Characteristics==F~"No",
                                           .default=NA)) %>%
          #filter(Characteristics != F) %>%
          filter(Characteristics != "No")

        if (!base::missing(strat_var) & comparisons) {
          temp_df <- temp_df %>%
            mutate(P=p_value)
        }
      }

      else if (var_type=="dichotomous"){
        temp_df <- temp_df %>%
          filter(Characteristics != refs[[var_]])

        if (!base::missing(strat_var) & comparisons) {
          temp_df <- temp_df %>%
            mutate(P=p_value)
        }
      }

      else {

        first_row <- data.frame(temp_1=var_new_name,
                                temp_2="") %>%
          as_tibble()
        colnames(first_row) <- c("Characteristics", "Value")

        if (!base::missing(strat_var)) {
          for (mod in names(tots_strat_var)[1:(length(tots_strat_var)-1)]) {
            sym_mod <- sym(mod)
            first_row <- first_row %>%
              mutate(!!sym_mod:="")
          }
          first_row <- first_row %>%
            rename(Total=Value) %>%
            relocate(Total, .after=last_col())
        }

        if (!base::missing(strat_var) & comparisons) {
          first_row <- first_row %>%
            mutate(P=round(p_value, prec))

          temp_df <- temp_df %>%
            mutate(P=missing_value_not)
        }

        temp_df <- first_row %>%
          rbind(temp_df)

        rows_to_indent <- c(rows_to_indent, (row_counter+1):(row_counter+dim(temp_df)[1]-1))

        if (!base::missing(factors) & var_ %in% names(factors)) {
          factors[[var_]] <- c(var_new_name, factors[[var_]])
        }
        if (!base::missing(labels) & var_ %in% names(labels)) {
          labels[[var_]] <- c(var_new_name, labels[[var_]])
        }
      }
    }

    else {

      if (!base::missing(strat_var) & comparisons) {
        out_test <- t.test(df[[var_]]~df[[strat_var]])
        p_value <- round(out_test$p.value, prec)
      }

      temp_df <- df %>%
        summarise(median_var=median(!!sym_var, na.rm=T),
                  low_quart=quantile(!!sym_var, 0.25, na.rm=T),
                  upper_quart=quantile(!!sym_var, 0.75, na.rm=T)) %>%
        mutate(Value=glue("{median_var} [{low_quart}-{upper_quart}]"),
               Characteristics=var_) %>%
        select(Characteristics, Value)

      if (!base::missing(strat_var)) {
        temp_df_strat <- df %>%
          group_by(!!sym_strat_var) %>%
          summarise(median_var=median(!!sym_var, na.rm=T),
                    low_quart=quantile(!!sym_var, 0.25, na.rm=T),
                    upper_quart=quantile(!!sym_var, 0.75, na.rm=T)) %>%
          mutate(Value=glue("{median_var} [{low_quart}-{upper_quart}]"),
                 Characteristics=var_) %>%
          select(Characteristics, Value, !!sym_strat_var) %>%
          pivot_wider(names_from=!!sym_strat_var,
                      values_from=Value)

        temp_df <- temp_df_strat %>%
          left_join(temp_df, by="Characteristics") %>%
          rename(Total=Value) %>%
          mutate(Characteristics=var_new_name)
      }

      if (!base::missing(strat_var) & comparisons) {
        temp_df <- temp_df %>%
          mutate(P=p_value)
      }
    }
    row_counter <- row_counter + dim(temp_df)[1]

    if (!base::missing(factors) & var_ %in% names(factors)) {
      if (!base::missing(labels) & var_ %in% names(labels)) {
        temp_df <- temp_df %>%
          mutate(Characteristics=factor(Characteristics,
                                        levels=factors[[var_]],
                                        labels=labels[[var_]])) %>%
          arrange(Characteristics)
      }
      else {
        temp_df <- temp_df %>%
          mutate(Characteristics=factor(Characteristics,
                                        levels=factors[[var_]])) %>%
          arrange(Characteristics)
      }
    }

    if (var_type %in% c("dichotomous", "boolean")) {
      temp_df <- temp_df %>%
        mutate(Characteristics=glue("{var_new_name} - {Characteristics}"))
    }

    if (!base::missing(strat_var) & comparisons) {
      temp_df <- temp_df %>%
        relocate(P, .before=Total) %>%
        rename(`p-value`=P)
    }

    if (starting_flag) {
      summ_df <- temp_df
      starting_flag <- F
    }

    else {
      summ_df <- summ_df %>%
        rbind(temp_df)
    }
  }


  if (table_package == "gt") {
    table_summ <- summ_df %>%
      mutate_all(str_replace_all, ">=", "/geq") %>%
      mutate_all(str_replace_all, "<=", "/leq") %>%
      gt() %>%
      cols_align(columns="Characteristics",
                 align="left") %>%
      tab_style(style=cell_text(indent = indent_size),
                location=cells_body(rows = rows_to_indent,
                                    columns=c("Characteristics")))

    if (!base::missing(strat_var) & comparisons) {
      table_summ <- table_summ %>%
        tab_style(style=cell_text(align="left"),
                  location=list(cells_body(columns=`p-value`),
                                cells_column_labels(columns=`p-value`)))
    }

    table_summ %>%
      gtsave(glue("{file_path}/{file_name}.{gt_format}"))
  }

  else if (table_package=="kable") {
    table_summ <- summ_df %>%
      mutate_all(str_replace_all, ">=", "/geq") %>%
      mutate_all(str_replace_all, "<=", "/leq") %>%
      kable(format = "latex", booktabs = TRUE,
            linesep="") %>%
      add_indent(positions = rows_to_indent)

    save_kable(table_summ, glue("{file_path}/{file_name}.tex"))
  }

  return_obj <- list("tidy_df"=summ_df,
                     "table_summ"=table_summ,
                     "rows_to_indent"=rows_to_indent,
                     "labels"=labels)
}

