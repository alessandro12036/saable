library(tidyverse)
library(readr)

source("R/summ_table_maker.R")
source("R/get_var_type.R")

df <- read_csv2("../../Spallanzani/lets_test/data/preprocessed/final_df.csv") %>%
  rename(id=`__id`,
         conoscenza_u_u=u_u,
         conoscenza_u_u_appropriata=u_u_appropriata)

age_levels <- c("<24", "25-34", "35-44", "45-59", ">60")
ist_cols <- df %>%
  select(starts_with("ist_")) %>%
  names()

df <- df %>%
  mutate(ist=ifelse(is.na(ist) | ist=="Non noto",
                    NA,
                    ist=="Si"),
         no_condom_3m=(anal_insert_pre_no_condom_3m=="Si")|
           (anal_rice_pre_no_condom_3m=="Si")|
           (vag_insert_pre_no_condom_3m=="Si")|
           (vag_rice_pre_no_condom_3m=="Si")|
           (oral=="Sì"),
         data_test_hiv_prec=ymd_hms(data_test_hiv_prec),
         anno_test_hiv_prec=year(data_test_hiv_prec)) %>%
  mutate(anno_test_hiv_prec=ifelse(anno_test_hiv_prec>=2015 & anno_test_hiv_prec<2020,
                                   "2015-2019",
                                   ifelse(anno_test_hiv_prec >= 2010 & anno_test_hiv_prec < 2015,
                                          "2010-2014",
                                          ifelse(anno_test_hiv_prec<2010,
                                                 "<2010",
                                                 ifelse(anno_test_hiv_prec>=2024,
                                                        "2024",
                                                        anno_test_hiv_prec)))),
         eta_primo_rapporto_cat=case_when(is.na(eta_primo_rapporto)~NA,
                                          eta_primo_rapporto>=10 & eta_primo_rapporto<=14 ~ "10-14",
                                          eta_primo_rapporto>=15 & eta_primo_rapporto<=17 ~ "15-17",
                                          eta_primo_rapporto>=18 & eta_primo_rapporto<=24 ~ "18-24",
                                          eta_primo_rapporto>=25 ~ "25+"),
         titolo_studio_=ifelse(titolo_studio_=="Diploma Media Inferiore" | titolo_studio_=="Elementari", # modifiche categorie
                               "Senza diploma superiore",
                               titolo_studio_),
         eta=ifelse(eta=="18-24",
                    "<24",
                    eta)) %>%
  rowwise() %>%
  # taking the most recent one
  mutate(`ist_Papilloma virus (HPV)`=ifelse(is.na(`ist_Papilloma virus (HPV)`) & is.na(ist_Condilomi),
                                            NA,
                                            max(`ist_Papilloma virus (HPV)`, ist_Condilomi, na.rm=T))) %>%
  mutate(most_recent_ist_year=max(c_across(all_of(ist_cols)), na.rm=T)) %>%
  mutate(most_recent_ist_year=ifelse(abs(most_recent_ist_year)==Inf,
                                     NA,
                                     most_recent_ist_year)) %>%
  mutate(less_than_5_years_ist=(year(today())-most_recent_ist_year)<=5) %>%
  ungroup() %>%
  mutate(less_than_5_years_ist=ifelse(is.na(less_than_5_years_ist),
                                      F,
                                      less_than_5_years_ist)) %>%
  mutate(eta=ifelse(eta=="<18",
                    "<24",
                    eta),
         eta=factor(eta, levels=age_levels),
         cat_giulia=case_when((rapporti_con_ %in% c("Solo maschi", "Sia maschi che femmine")
                               & sesso_nascita=="M"
                               & identita_genere_=="Cisgender")~"GBMSM",
                              sesso_nascita=="M" & identita_genere_=="Cisgender"
                              & rapporti_con_=="Solo femmine" ~ "Cis-Heterosexual Male",
                              sesso_nascita=="F" & identita_genere_=="Cisgender"
                              & rapporti_con_=="Solo maschi" ~ "Cis-Heterosexual Female",
                              sesso_nascita=="M" & identita_genere_=="Transgender"~"TGW",
                              sesso_nascita=="F" & identita_genere_=="Transgender"~"TGM",
                              .default="Altro")) %>%
  mutate(cat_giulia=as.factor(cat_giulia)) %>%
  select(-ist_Condilomi)

out_obj <- summ_table_maker(df, vars_=c("eta",
                                        "eta_primo_rapporto",
                                        "conoscenza_u_u_appropriata",
                                        "stato_relazionale_"),
                            var_types = list("eta"="categorical",
                                             "eta_primo_rapporto"="continuous",
                                             "conoscenza_u_u_appropriata"="boolean",
                                             "stato_relazionale_"="categorical"),
                            refs = list("sesso_nascita"="M"),
                            strat_var="sesso_nascita",
                            factors=list("stato_relazionale_"=c("Single",
                                                                "Relazione esclusiva",
                                                                "Relazione non esclusiva")),
                            labels=list("stato_relazionale_"=c("A", "B", "C")))

out_obj
