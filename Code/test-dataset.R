# Synthetic data
# Since we cannot share the real dataset (not got permission from IAO), going to use synthpop to generate a dataset with the same distribution and characteristics of the existing one.

library(tidyverse)
library(synthpop)


df <- read.csv('Data/final_wi_dataset-with_rp_id_2020-05-26.csv', stringsAsFactors = F) %>%
#df <- read.csv('Data/final_no_wi_dataset-2020-03-05.csv', stringsAsFactors = F) %>%
  mutate(
    intervention = ifelse(monthsSinceIntervention > 0, "Post", "Pre"),
    rot_paras = ifelse(rp == 1, "Rotational Paramedics", "Control"),
    age = ordered(age, levels = c("[0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]", "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", "(55,60]", "(60,65]","(65,70]","(70,75]", "(75,80]", "(80,85]", "(85,90]", "(90,95]", "(95,100]", "(100,105]", "(105,110]")),
    risk = ordered(risk, levels=c("Low", "Low-Medium","Medium", "High")),
    los = ordered(los, levels=c("<1 year", "1 to 5 years", ">5 years")),
    ltc = ordered(ltc, levels=c("[0,4]", "(4,8]", "(8,12]","(12,16]")),
    `non-conveyed` = ifelse(conveyed == 1, 0, 1),
    `safe non-conveyance` = case_when(
      conveyed == 1 ~ 0,
      recontact == 1 ~ 0,
      TRUE ~ 1
    ),
    newTimeElapsed = ifelse(timeElapsedMonths < 13, timeElapsedMonths + 1, timeElapsedMonths - 1)
  ) %>% filter(newTimeElapsed < 25) %>%
 dplyr::select("safe non-conveyance", "non-conveyed", "season", "ooh", "call_cat","rural_urban", "imd_decile",  "ltc", "age", "sex", "risk" , "wi", "los", "intervention", "rot_paras", "rp", "newTimeElapsed", "conveyed", "recontact", "recontact_conveyed", numbRRV, numAmb, conveyed, recontact, recontact_RRV, recontact_Amb, rp_id)
#  dplyr::select("safe non-conveyance", "non-conveyed", "season", "ooh", "call_cat","rural_urban", "imd_decile",  "ltc", "age", "sex", "risk" , "los", "intervention", "rot_paras", "rp", "newTimeElapsed", "conveyed", "recontact", "recontact_conveyed", numbRRV, numAmb, conveyed, recontact, recontact_RRV, recontact_Amb) %>%
 # mutate_if(is.integer, as.factor)

 # No working impression numbers 2059, 4780
# Working impression numbers 1901, 3297
syn_df <- syn.strata(data = df, strata = c("intervention", "rp"), seed = 123)
compare(syn_df, df)


# No working impression ----------------

# Target numbers as 2059 pre-placement and 4780 after

fake_df_no_wi <- syn_df$syn

fake_df_no_wi %>% count(intervention, rp)

# intervention rp    n
# 1         Post  0 2005
# 2         Post  1 2060
# 3          Pre  0 4760
# 4          Pre  1 4853

set.seed(246)

df2_no_wi <- fake_df_no_wi %>%
  mutate(
    rowid = row_number()
  )

add_pre_placement_control <- df2_no_wi %>%
  filter(intervention == "Pre", rp == 0) %>%
  sample_n(20)

subtract_pre_placement_intervention <- df2_no_wi %>%
  filter(intervention == "Pre", rp == 1) %>%
  sample_n(73) %>%
  pull(rowid)

add_post_placement_control <- df2_no_wi %>%
  filter(intervention == "Post", rp == 0) %>%
  sample_n(54)

subtract_post_placement_intervention <- df2_no_wi %>%
  filter(intervention == "Post", rp == 1) %>%
  sample_n(1) %>%
  pull(rowid)

final_no_wi_df <- df2_no_wi %>%
  filter(!(rowid %in% subtract_pre_placement_intervention)) %>%
  filter(!(rowid %in% subtract_post_placement_intervention)) %>%
  bind_rows(add_pre_placement_control) %>%
  bind_rows(add_post_placement_control)


final_no_wi_df %>% count(intervention, rp)  

saveRDS(final_no_wi_df %>% dplyr::select(-rowid), 'Data/synthetic-dataset-no-wi.rds')

# Working impression ----------------------

# Working impression taget numbers post-placement 1901, pre-placement 3297

fake_df_with_wi <- syn_df$syn

fake_df_with_wi %>% count(intervention, rp)

# intervention rp    n
# 1         Post  0 1849  - add 52
# 2         Post  1 1888 - add 13
# 3          Pre  0 3297 - nada
# 4          Pre  1 3362 - remove 65


set.seed(246)

df2_with_wi <- fake_df_with_wi %>%
  mutate(
    rowid = row_number()
  )


subtract_pre_placement_intervention <- df2_with_wi %>%
  filter(intervention == "Pre", rp == 1) %>%
  sample_n(65) %>%
  pull(rowid)

add_post_placement_control <- df2_with_wi %>%
  filter(intervention == "Post", rp == 0) %>%
  sample_n(52)

add_post_placement_intervention <- df2_with_wi %>%
  filter(intervention == "Post", rp == 1) %>%
  sample_n(13) 


final_with_wi_df <- df2_with_wi %>%
  filter(!(rowid %in% subtract_pre_placement_intervention)) %>%
  bind_rows(add_post_placement_intervention, add_post_placement_control)

final_with_wi_df %>% count(intervention, rp)


saveRDS(final_with_wi_df, 'Data/synthetic-dataset-with-wi.rds')
