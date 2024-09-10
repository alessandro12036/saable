# TODO: add comparison mechanism
# TODO: add mean (sd) option for continuous
# TODO: add way to clean labels
library(glue)
library(gt)
library(tidyverse)

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
                             var_new_names=NULL){
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
      }

      else if (var_type=="dichotomous"){
        temp_df <- temp_df %>%
          filter(Characteristics != refs[[var_]])
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

    if (starting_flag) {
      summ_df <- temp_df
      starting_flag <- F
    }

    else {
      summ_df <- summ_df %>%
        rbind(temp_df)
    }
  }

  gt_summ <- summ_df %>%
    gt() %>%
    cols_align(columns="Characteristics",
               align="left") %>%
    tab_style(style=cell_text(indent = pct(10)),
              location=cells_body(rows = rows_to_indent,
                                  columns=c("Characteristics")))

  return_obj <- list("tidy_df"=summ_df,
                     "gt_summ"=gt_summ,
                     "rows_to_indent"=rows_to_indent,
                     "labels"=labels)
}

