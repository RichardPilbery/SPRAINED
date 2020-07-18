# Data for Appendix
# Sensitivity analysis with WI not included.

# Setup -----------------------------------

#synth_data = FALSE
#format = 'html'

library(tidyverse)
library(stringr)
library(tableone)
library(nlme)
library(boot)

if(synth_data == FALSE) {
df_no_wi <- read_csv('Data/final_no_wi_dataset-2020-03-05.csv') %>%
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
    dplyr::select("safe non-conveyance", "non-conveyed", "season", "ooh", "call_cat","rural_urban", "imd_decile",  "ltc", "age", "sex", "risk" , "los", "intervention", "rot_paras", "rp", "newTimeElapsed", "conveyed", "recontact", "recontact_conveyed", conveyed, recontact, recontact_RRV, recontact_Amb)
}

# Synthetic data
if(synth_data == TRUE) {
  
  print("Synthetic dataset")
  
  df_no_wi <- readRDS('SynData/synthetic-dataset-no-wi.rds') %>%
    mutate(
      call_cat = as.character(call_cat),
      age = ordered(age, levels = c("[0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]", "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", "(55,60]", "(60,65]","(65,70]","(70,75]", "(75,80]", "(80,85]", "(85,90]", "(90,95]", "(95,100]", "(100,105]", "(105,110]")),
      los = ordered(los, levels=c("<1 year", "1 to 5 years", ">5 years")),
      rp = as.numeric(levels(rp))[rp]
    )
  
}


matchedTbl_pre_post_control_rp_no_wi <- CreateTableOne(
  vars = c("safe non-conveyance", "non-conveyed", "season", "ooh", "call_cat","rural_urban", "imd_decile",  "ltc", "age", "sex", "risk" ,  "los"),
  strata = c("intervention","rot_paras"),
  data = df,
  test = F,
  factorVars = c("safe non-conveyance", "non-conveyed", "season", "call_cat", "ooh", "risk", "los", "age", "sex", "imd_decile", "rural_urban", "ltc")
)

matrix_tbl_no_wi <- print(matchedTbl_pre_post_control_rp_no_wi)
col1_no_wi <- rownames(matrix_tbl_no_wi)

## Synthetic data only
# if(synth_data == TRUE) {
#   # The synthetic data does not include 2 factor levels that the main data set has
#   # However, this breaks the summary table, so need to add them in first
# 
#   new_matrix_tbl_no_wi <- matrix_tbl_no_wi[1:54,] %>%
#     rbind(c("   0 ( 0.0) ", "   0 ( 0.0) ", "   0 ( 0.0) ", "   0 ( 0.0) " )) %>%
#     rbind(matrix_tbl_no_wi[55:106,])
# 
#   matrix_tbl_no_wi <- new_matrix_tbl_no_wi
# 
#   col1_no_wi <- c(col1[1:54], "(105,110]", col1[55:106])
# 
# 
#   ##
# }


# Formatting of headings

col1_no_wi[2] <- "Appropriately not conveyed n (%)"
col1_no_wi[3] <- "Not conveyed n (%)"
col1_no_wi[4] <- "Yearly quarter n (%)"
col1_no_wi[9] <- "Out-of-hours n (%)"
col1_no_wi[10] <- "Call category"
col1_no_wi[16] <- "Urban location n (%)"
col1_no_wi[17] <- "IMD decile n (%)"
col1_no_wi[28] <- "Prevalence of Long-term conditions n (%)"
col1_no_wi[33] <- "Patient age in years n (%)"
col1_no_wi[56] <- "Patient sex n (%)"
col1_no_wi[60] <- "NEWS category n (%)"
# This is a problem since it only has los > 5 as the other is 1 to 5 years (no values < 1 year)
# Probably need to solve as per synthetic dataset
#col1[65] <- "Years registered as a paramedic"

# This will come in handy for Table 1 headers
#list_of_cols <- c(4, 10, 17, 28, 33, 56, 60, 65, 104)


df_summary_no_wi <- bind_cols(data.frame(Measure = col1_no_wi, stringsAsFactors = F), as_tibble(matrix_tbl_no_wi)) %>%
  dplyr::select(Measure, `Pre:Control`, `Pre:Rotational Paramedics`, `Post:Control`, `Post:Rotational Paramedics`)

colnames(df_summary_no_wi) <- c("Measure", "Control", "Rotation", "Control", "Rotation")



# fig2 ------------------------------


fig2_no_wi <- ggplot(df_no_wi %>% filter(rp == 1), aes(x = newTimeElapsed)) +
  geom_histogram(binwidth =1, color="black") +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Number of incidents", expand = c(0,0), breaks=seq(0, 650, 50), limits = c(0,650)) +
  geom_vline(xintercept = 12.5, size=2) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 2)) +
  scale_fill_viridis_d(name="SP ID", option = "D") +
  annotate("text", label = "Pre-rotation", x = 2.8, y = 600, size = 4, colour = "black") +
  annotate("text", label = "Post-rotation", x = 15, y = 600, size = 4, colour = "black")


# fig3 ---------------------------

df1a_no_wi <- df_no_wi %>%
  dplyr::select(newTimeElapsed, call_cat, risk, intervention)

levels(df1a_no_wi$call_cat) <- c("category 1", "category 2", "category 3", "category 4", "category 5")

df1a_no_wi <- df1a_no_wi %>%
  mutate(
    call_cat = case_when(
      call_cat == "cat1" ~ "category 1",
      call_cat == "cat2" ~ "category 2",
      call_cat == "cat3" ~ "category 3",
      call_cat == "cat4" ~ "category 4",
      TRUE ~ "category 5"
    )
  )


fig3_no_wi <- ggplot(df1a_no_wi, aes(x = newTimeElapsed, fill = call_cat)) +
  geom_histogram(binwidth =1, position="fill", color="black") +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Percentage of incidents", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  labs(fill="Call Category") +
  theme(legend.position = "bottom", axis.title.x = element_text(vjust = -1)) +
  geom_vline(xintercept = 12.5, size = 1.5) +
  scale_fill_viridis_d() +
  annotate("label", label = "Pre-rotation", x = 2.8, y = 0.9, size = 4, colour = "black") +
  annotate("label", label = "Post-rotation", x = 15, y = 0.9, size = 4, colour = "black")


# fig4 ------------------------------------


fig4_no_wi <- ggplot(df_no_wi, aes(x = newTimeElapsed, fill = risk)) +
  geom_histogram(binwidth =1, position="fill", color="black") +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Percentage of incidents", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  scale_fill_brewer("NEWS risk category") +
  theme(legend.position = "bottom", axis.title.x = element_text(vjust = -1)) +
  geom_vline(xintercept = 12.5, size = 2) +
  annotate("label", label = "Pre-rotation", x = 2.8, y = 0.9, size = 4, colour = "black") +
  annotate("label", label = "Post-rotation", x = 15, y = 0.9, size = 4, colour = "black")



# Time Series -------------------------


ts_df_no_wi <- df_no_wi %>%
  group_by(rot_paras, newTimeElapsed) %>%
  summarise(
    n = n(),
    propnonConveyed = sum(conveyed == 0)/n,
    propnonRecontact = sum(`safe non-conveyance` == 1)/n,
    intervention = first(intervention),
    rp = first(rp)
  ) %>%
  ungroup() %>%
  mutate(
    intervention = ifelse(intervention == "Pre", 0, 1),
    timeSinceIntervention = ifelse(intervention == 0, 0, newTimeElapsed - 12)
  ) %>%
  rename(
    level = intervention,
    trend = timeSinceIntervention,
    time = newTimeElapsed
  ) %>%
  mutate(
    rptime = ifelse(rp == 1, time, 0),
    rplevel = ifelse(rp == 1, level, 0),
    rptrend = ifelse(rp == 1, trend, 0)
  )

ts_df_no_18_no_wi <- ts_df_no_wi %>%
  filter(!(rp == 1 & time == 18))


# fig5 -------------------------

step4its_gg_no_wi <- ts_df_no_wi %>%
  mutate(
    outcome = round(propnonRecontact, 2)  )

cols2 <- c("control"="#0000ff", "intervention"="#ff0000")

fig5_no_wi <- ggplot(data = step4its_gg_no_wi, aes(x = time, y = outcome)) +
  # Prepare graph
  theme_bw() +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of patients appropriately non-conveyed", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  geom_vline(xintercept = 12.5, color="black") +
  #annotate("rect", xmin=0.5, xmax= 12.5, ymin=0, ymax=Inf, alpha=0.1, fill="blue") +
  #annotate("rect", xmin=12.5, xmax=24.5, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
  annotate("text", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("text", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black") +

  # Prep control/intervention data
  geom_point(data=step4its_gg_no_wi %>% filter(rp == 0), aes(color="control")) +
  geom_line(data=step4its_gg_no_wi %>% filter(rp == 0), aes(color="control"), linetype="solid") +
  geom_point(data=step4its_gg_no_wi %>% filter(rp == 1), aes(color="intervention")) +
  geom_line(data=step4its_gg_no_wi %>% filter(rp == 1), aes(color="intervention"), linetype="solid") +

  scale_color_manual(name="Group", values = cols2) +
  theme(legend.position = "bottom")

#fig5
# cits-workflow ----------------------------------


library(tidymodels)

lm_mod <- linear_reg() %>%
  set_engine('lm')

lm_fit_no_wi <-
  lm_mod %>%
  fit(propnonRecontact ~ time + rp + rptime + level + trend + rplevel + rptrend, data = ts_df_no_wi)

mean_pred_no_wi <- predict(lm_fit_no_wi, new_data = ts_df_no_wi)

plot_data_no_wi <-
  ts_df_no_wi %>%
  bind_cols(mean_pred_no_wi)


lm_fit_no_18_no_wi <-
  lm_mod %>%
  fit(propnonRecontact ~ time + rp + rptime + level + trend + rplevel + rptrend, data = ts_df_no_18_no_wi)


# Intercept is the initial value of the control group
# time is existing trend in control group
# rp is the difference in levels between control and intervention
# rptime is the trend of hte intervention with reference to control
# level is post-pilot control change in level of non-conveyance
# trend is post-pilot control change in trend on non-conveyance
# rplevel shows post-pilot intervention group change in level
# rptrend is the post-pilot intervention group change in trend


dw_test_no_wi <- tidy(lmtest::dwtest(lm(propnonRecontact ~ time + rp + rptime + level + trend + rplevel + rptrend, data = ts_df_no_wi),iterations=12,alternative="two.sided"))


# cits-table-prep -----------------------------



f <- function(x) {
  y <- sprintf("%.1f", x * 100,1)
  return(y)
}

cits_tidy_no_wi <- tidy(lm_fit_no_wi, conf.int = T) %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), f) %>%
  mutate(
    ci = glue::glue("{conf.low}--{conf.high}"),
    p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )


cits_tidy_no_18_no_wi <- tidy(lm_fit_no_18_no_wi, conf.int = T) %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), f) %>%
  mutate(
    ci = glue::glue("{conf.low}--{conf.high}"),
    p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )


ols_table_no_wi <- tidy(lm_fit_no_wi, conf.int = T) %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), f) %>%
  mutate(
    ` ` = case_when(
      term == "(Intercept)" ~ "Initial control group level",
      term == "time" ~ "Pre-rotation control group trend",
      term == "rp" ~ "Difference in level between control and intervention groups",
      term == "rptime" ~ "Rotation group trend relative to control group",
      term == "level" ~ "Post-rotation change in control group level",
      term == "trend" ~ "Post-rotation change in control group trend",
      term == "rplevel" ~ "Post-rotation intervention group change in level relative to control group",
      term == "rptrend" ~ "Post-rotation intervention group change in trend relative to control group"
    ),
    `Coefficient (%)` = estimate,
    `95% CI` = glue::glue("{conf.low} to {conf.high}"),
    `P value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  dplyr::select(` `, `Coefficient (%)`, `95% CI`, `P value`)


# econ_1 -----------------------------


# Calculate the per incident cost for an SP
# 10 weeks 

ten_weeks_salary = (31121/52) * 10

# 10 weeks salary for 10 SPs
ten_for_ten = ten_weeks_salary * 10

# SPs attended 2059 cases (including missing wi) in the post-rotation phase

salary_per_inc = round(ten_for_ten / 2059, 2)

# £29.07 per incident.

econ_df_no_wi <- df_no_wi %>%
  #filter(n < 3) %>%
  dplyr::select(conveyed, recontact, recontact_RRV, recontact_Amb, recontact_conveyed, intervention, rot_paras, `safe non-conveyance`)

# Calculate the total cost for each incident based on the unit costs
econ_df_no_wi$cost <- mapply(function(conveyed, recontact, recontact_conveyed, rp)
{
  
  call_cost <- 7.33
  S_and_T <- 209.38
  S_T_and_C <- 257.34
  A_E <- 135
  
  cost = 0
  
  # Add placement cost if rp
  
  if(rp == "Rotational Paramedics") {
    cost = cost + salary_per_inc
  }
  
  # Add 999 call
  
  cost = cost + call_cost
  
  # If no ambulances and RRV conveyed, then need to ST and C
  
  if(conveyed == 1) {
    cost = cost + S_T_and_C + A_E
  } else {
    cost = cost + S_and_T
  }
  
  # If not conveyed but there is a recontact
  # Then need to add these costs
  
  if(conveyed == 0 & recontact == 1) {
    
    # Add 999 call
    
    cost = cost + call_cost
    
    # If no ambulances and RRV conveyed, then need to ST and C
    
    if(recontact_conveyed == 1) {
      cost = cost + S_T_and_C + A_E
    } else {
      cost = cost + S_and_T
    }
    
  }
  
  return(cost)
  
},
conveyed = econ_df_no_wi$conveyed, recontact = econ_df_no_wi$recontact, recontact_conveyed = econ_df_no_wi$recontact_conveyed, rp = econ_df_no_wi$rot_paras)



cost_convey_df_no_wi <- econ_df_no_wi %>%
  filter(intervention == "Post") %>%
  dplyr::select(cost, `safe non-conveyance`, rot_paras) %>%
  group_by(rot_paras) %>%
  summarise(
    n = n(),
    mean_cost = mean(cost),
    tot_cost = sum(cost),
    num_nc = sum(`safe non-conveyance`),
    cost_per_convey = sum(cost)/num_nc
  )



# econ_rp -----------------------------


boot_mean <- function(orig, resamp) {
  mean(orig$cost[resamp])
}

boot_convey <- function(orig, resamp) {
  sum(orig$cost[resamp])/sum(orig$`safe non-conveyance`[resamp])
}

mean_rp_df_no_wi <- econ_df_no_wi %>%
  filter(
    rot_paras == "Rotational Paramedics",
    intervention == "Post"
  ) %>%
  dplyr::select(cost, `safe non-conveyance`)

mean_results_rp_no_wi <- boot(mean_rp_df_no_wi, boot_mean, R = 6000)

mean_results_rp_sc_no_wi <- boot(mean_rp_df_no_wi, boot_convey, R = 6000)


# econ_control --------------------------

mean_control_df_no_wi <- econ_df_no_wi %>%
  filter(
    rot_paras == "Control",
    intervention == "Post"
  ) %>%
  dplyr::select(cost, `safe non-conveyance`)

set.seed(123)
mean_results_control_no_wi <- boot(mean_control_df_no_wi, boot_mean, R = 6000)
# broom::tidy(mean_results_control)

mean_results_control_sc_no_wi <- boot(mean_control_df_no_wi, boot_convey, R = 6000)


# mean_cost_per_unit ---------------------------


ci_mean_rp_no_wi <- boot.ci(mean_results_rp_no_wi)
ci_mean_control_no_wi <- boot.ci(mean_results_control_no_wi)


# mean_cost_per_sc -----------------------

ci_mean_rp_sc_no_wi <- boot.ci(mean_results_rp_sc_no_wi)
ci_mean_control_sc_no_wi <- boot.ci(mean_results_control_sc_no_wi)


boot_diff_convey <- function(orig, resamp) {
  
  control <- orig %>%
    filter(row_number() %in% resamp, rot_paras == "Control")
  control_mean <- sum(control$cost)/sum(control$`safe non-conveyance`)
  
  intervention <- orig %>%
    filter(row_number() %in% resamp, rot_paras == "Rotational Paramedics")
  int_mean <- sum(intervention$cost)/sum(intervention$`safe non-conveyance`)
  
  # print(control_mean - int_mean)
  
  return(control_mean - int_mean)
}

mean_diff_df_no_wi <- econ_df_no_wi %>% 
  filter(
    intervention == "Post"
  ) %>%
  dplyr::select(cost, rot_paras, `safe non-conveyance`)

set.seed(123)

mean_results_diff_no_wi <- boot(mean_diff_df_no_wi, boot_diff_convey, R = 6000)

ci_diff_no_wi <- boot.ci(mean_results_diff_no_wi)


