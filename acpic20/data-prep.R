# Prep figures for ACPIC20 presentation

library(tidyverse)
library(stringr)
library(tableone)
library(nlme)
library(boot)
library(flextable)

synth_data = FALSE

# Load in data -------------
df <- read_csv('Data/final_wi_dataset-with_rp_id_2020-05-26.csv') %>%
  mutate(
    intervention = ifelse(monthsSinceIntervention > 0, "Post", "Pre"),
    rot_paras = ifelse(rp == 1, "Rotational Paramedics", "Control"),
    rp_table = ifelse(rp == 1, "Intervention", "Control"),
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
  dplyr::select("safe non-conveyance", "non-conveyed", "season", "ooh", "call_cat","rural_urban", "imd_decile",  "ltc", "age", "sex", "risk" , "wi", "los", "intervention", "rot_paras", "rp", "rp_table", "newTimeElapsed", "conveyed", "recontact", "recontact_conveyed", conveyed, recontact, recontact_RRV, recontact_Amb, rp_id)


# Figure 1 ---------------


ggplot(df %>% filter(rp == 1), aes(x = newTimeElapsed)) +
  geom_histogram(binwidth =1, color="black", size = 0.1, fill = "lightblue") +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Number of incidents", expand = c(0,0), breaks=seq(0, 400, 50), limits = c(0,400)) +
  geom_vline(xintercept = 12.5, size=2) +
  theme_minimal() +
  #theme(legend.position = "bottom", axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 2)) +
  theme( axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 2)) +
  scale_fill_viridis_d(name="SP ID", option = "D") +
  annotate("text", label = "Pre-placement", x = 4, y = 385, size = 4, colour = "black") +
  annotate("text", label = "Post-placement", x = 16, y = 385, size = 4, colour = "black")

# Figure 2 -----------------

df1a <- df %>%
  select(newTimeElapsed, call_cat, risk, intervention)

levels(df1a$call_cat) <- c("category 1", "category 2", "category 3", "category 4", "category 5")

df1a <- df1a %>%
  mutate(
    call_cat = case_when(
      call_cat == "cat1" ~ "category 1",
      call_cat == "cat2" ~ "category 2",
      call_cat == "cat3" ~ "category 3",
      call_cat == "cat4" ~ "category 4",
      TRUE ~ "category 5"
    )
  )


ggplot(df1a, aes(x = newTimeElapsed, fill = call_cat)) +
  geom_histogram(binwidth =1, position="fill", color="black", size = 0.1) +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Percentage of incidents", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  labs(fill="Call Category") + 
  theme(legend.position = "bottom", axis.title.x = element_text(vjust = -1)) + 
  geom_vline(xintercept = 12.5, size = 1.5) +
  scale_fill_viridis_d() +
  annotate("label", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("label", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black")


# Figure 3 ---------------

ggplot(df, aes(x = newTimeElapsed, fill = risk)) +
  geom_histogram(binwidth =1, position="fill", color="black", size = 0.1) +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Percentage of incidents", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  scale_fill_brewer("NEWS risk category") +
  theme(legend.position = "bottom", axis.title.x = element_text(vjust = -1)) + 
  geom_vline(xintercept = 12.5, size = 2) +
  annotate("label", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("label", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black")

# CITS prep ----------------

ts_df <- df %>%
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

ts_df_no_18 <- ts_df %>%
  filter(!(rp == 1 & time == 18))

step4its_gg <- ts_df %>%
  mutate(
    outcome = round(propnonRecontact, 2)  )

cols2 <- c("control"="#0000ff", "intervention"="#ff0000")

library(tidymodels)

lm_mod <- linear_reg() %>%
  set_engine('lm') 

lm_fit <- 
  lm_mod %>% 
  fit(propnonRecontact ~ time + rp + rptime + level + trend + rplevel + rptrend, data = ts_df)

mean_pred <- predict(lm_fit, new_data = ts_df)

plot_data <- 
  ts_df %>% 
  bind_cols(mean_pred)


lm_fit_no_18 <- 
  lm_mod %>% 
  fit(propnonRecontact ~ time + rp + rptime + level + trend + rplevel + rptrend, data = ts_df_no_18)


# Intercept is the initial value of the control group
# time is existing trend in control group
# rp is the difference in levels between control and intervention
# rptime is the trend of hte intervention with reference to control
# level is post-pilot control change in level of non-conveyance
# trend is post-pilot control change in trend on non-conveyance
# rplevel shows post-pilot intervention group change in level
# rptrend is the post-pilot intervention group change in trend


# Alternating control and rp
new_data_points <- data.frame(
  time = c(1, 1, 12, 12, 13, 13, 24, 24),
  rp = rep(c(0, 1), 4),
  rptime = c(0, 1, 0, 12, 0, 13, 0, 24),
  level = c(0, 0, 0, 0, 1, 1, 1, 1),
  trend = c(0, 0, 0, 0, 1, 1, 12, 12),
  rplevel = c(0, 0, 0, 0, 0, 1, 0, 1),
  rptrend = c(0, 0, 0, 0, 0, 1, 0, 12),
  stringsAsFactors = F
)

# Control then rp
cf_data_points <- data.frame(
  time = c(13, 24, 13, 24),
  rp = c(0, 0, 1, 1),
  rptime = c(0, 0, 13, 24),
  level = c(0, 0, 1, 1),
  trend = c(0, 0, 12, 12),
  rplevel = c(0, 0, 0, 0),
  rptrend = c(0,0, 0, 0),
  stringsAsFactors = F
)

# 12 months rp non-conveyance

rp_12_months_data_points <- data.frame(
  time = c(12, 24),
  rp = c(1, 1),
  rptime = c(12, 24),
  level = c(1, 1),
  trend = c(12, 12),
  rplevel = c(1, 1),
  rptrend = c(1, 12),
  stringsAsFactors = F
)

rp_12_months_pred <- predict(lm_fit, new_data = rp_12_months_data_points)
rp_12_months_pred_ci <- predict(lm_fit, new_data = rp_12_months_data_points, type = "conf_int")

mean_pred <- predict(lm_fit, new_data = new_data_points)
cf_mean_pred <- predict(lm_fit, new_data = cf_data_points)

plot_data <- new_data_points %>%
  bind_cols(mean_pred)

cf_plot_data <- cf_data_points %>%
  bind_cols(cf_mean_pred)

cols2 <- c("control"="#0000ff", "intervention"="#ff0000")


# Figure 4a -------------------

# Need to split figure 6 into smaller chunks

ggplot(data = ts_df, aes(x = time, y = propnonRecontact)) + 
  # Prepare graph
  theme_bw() +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of patients appropriately non-conveyed", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  geom_vline(xintercept = 12.5, color="black") +
  annotate("text", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("text", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black") +
  
  # Plot actual data points
  geom_point(data=ts_df %>% filter(rp == 0, level == 0), aes(color="control"), alpha = 0.2) +

  # Plot control lines
  geom_line(data=plot_data %>% filter(rp == 0, level == 0), aes(y = .pred, color="control"), linetype="solid") +
  
  # Plot counterfactual control lines
  geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +

  scale_color_manual(name="Group", values = cols2) +
  theme(legend.position = "bottom")


# Figure 4b -------------------------


ggplot(data = ts_df, aes(x = time, y = propnonRecontact)) + 
  # Prepare graph
  theme_bw() +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of patients appropriately non-conveyed", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  geom_vline(xintercept = 12.5, color="black") +
  annotate("text", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("text", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black") +
  
  # Plot actual data points
  geom_point(data=ts_df %>% filter(rp == 0), aes(color="control"), alpha = 0.2) +
  
  # Plot control lines
  geom_line(data=plot_data %>% filter(rp == 0, level == 0), aes(y = .pred, color="control"), linetype="solid") +
  geom_line(data=plot_data %>% filter(rp == 0, level == 1), aes(y = .pred, color="control"), linetype="solid") +
  
  # Plot counterfactual control lines
  geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +
  #geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +
  
  scale_color_manual(name="Group", values = cols2) +
  theme(legend.position = "bottom")


# Figure 4c --------------------

ggplot(data = ts_df, aes(x = time, y = propnonRecontact)) + 
  # Prepare graph
  theme_bw() +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of patients appropriately non-conveyed", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  geom_vline(xintercept = 12.5, color="black") +
  annotate("text", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("text", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black") +
  
  # Plot actual data points
  geom_point(data=ts_df %>% filter(rp == 0), aes(color="control"), alpha = 0.2) +
  geom_point(data=ts_df %>% filter(rp == 1, level == 0), aes(color="intervention"), alpha = 0.2) +
  
  # Plot rp lines
  geom_line(data=plot_data %>% filter(rp == 1, level == 0), aes(y = .pred, color="intervention"), linetype="solid") +
  
  # Plot counterfactual rp lines
  #geom_line(data=cf_plot_data %>% filter(rp == 1), aes(y = .pred, color="intervention"), linetype="dashed") +

  
  # Plot control lines
  geom_line(data=plot_data %>% filter(rp == 0, level == 0), aes(y = .pred, color="control"), linetype="solid") +
  geom_line(data=plot_data %>% filter(rp == 0, level == 1), aes(y = .pred, color="control"), linetype="solid") +
  
  # Plot counterfactual control lines
  geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +

  scale_color_manual(name="Group", values = cols2) +
  theme(legend.position = "bottom")

# Figure 4d ----------------------

ggplot(data = ts_df, aes(x = time, y = propnonRecontact)) + 
  # Prepare graph
  theme_bw() +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of patients appropriately non-conveyed", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  geom_vline(xintercept = 12.5, color="black") +
  annotate("text", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("text", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black") +
  
  # Plot actual data points
  geom_point(data=ts_df %>% filter(rp == 0), aes(color="control"), alpha = 0.2) +
  geom_point(data=ts_df %>% filter(rp == 1, level == 0), aes(color="intervention"), alpha = 0.2) +
  
  # Plot rp lines
  geom_line(data=plot_data %>% filter(rp == 1, level == 0), aes(y = .pred, color="intervention"), linetype="solid") +
  #geom_line(data=plot_data %>% filter(rp == 1, level == 1), aes(y = .pred, color="intervention"), linetype="solid") +
  
  # Plot counterfactual rp lines
  geom_line(data=cf_plot_data %>% filter(rp == 1), aes(y = .pred, color="intervention"), linetype="dashed") +
  geom_line(data=cf_plot_data %>% filter(rp == 1), aes(y = .pred, color="intervention"), linetype="dashed") +
  
  # Plot control lines
  geom_line(data=plot_data %>% filter(rp == 0, level == 0), aes(y = .pred, color="control"), linetype="solid") +
  geom_line(data=plot_data %>% filter(rp == 0, level == 1), aes(y = .pred, color="control"), linetype="solid") +
  
  # Plot counterfactual control lines
  geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +
  geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +
  
  scale_color_manual(name="Group", values = cols2) +
  theme(legend.position = "bottom")



# Figure 4e --------------------

ggplot(data = ts_df, aes(x = time, y = propnonRecontact)) + 
  # Prepare graph
  theme_bw() +
  scale_x_continuous(name="Months elapsed", breaks = seq(1,24,1), labels = seq(1,24,1), expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of patients appropriately non-conveyed", expand = c(0,0), breaks=seq(0,1,0.1), limits = c(0,1), labels=scales::percent) +
  geom_vline(xintercept = 12.5, color="black") +
  annotate("text", label = "Pre-placement", x = 4, y = 0.9, size = 4, colour = "black") +
  annotate("text", label = "Post-placement", x = 16, y = 0.9, size = 4, colour = "black") +
  
  # Plot actual data points
  geom_point(data=ts_df %>% filter(rp == 0), aes(color="control"), alpha = 0.2) +
  geom_point(data=ts_df %>% filter(rp == 1), aes(color="intervention"), alpha = 0.2) +
  
  # Plot rp lines
  geom_line(data=plot_data %>% filter(rp == 1, level == 0), aes(y = .pred, color="intervention"), linetype="solid") +
  geom_line(data=plot_data %>% filter(rp == 1, level == 1), aes(y = .pred, color="intervention"), linetype="solid") +
  
  # Plot counterfactual rp lines
  geom_line(data=cf_plot_data %>% filter(rp == 1), aes(y = .pred, color="intervention"), linetype="dashed") +
  geom_line(data=cf_plot_data %>% filter(rp == 1), aes(y = .pred, color="intervention"), linetype="dashed") +
  
  # Plot control lines
  geom_line(data=plot_data %>% filter(rp == 0, level == 0), aes(y = .pred, color="control"), linetype="solid") +
  geom_line(data=plot_data %>% filter(rp == 0, level == 1), aes(y = .pred, color="control"), linetype="solid") +
  
  # Plot counterfactual control lines
  geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +
  geom_line(data=cf_plot_data %>% filter(rp == 0), aes(y = .pred, color="control"), linetype="dashed") +
  
  scale_color_manual(name="Group", values = cols2) +
  theme(legend.position = "bottom")

# Table 2 CITS -----------------



f <- function(x) {
  y <- sprintf("%.1f", x * 100,1)
  return(y)
}

cits_tidy <- tidy(lm_fit, conf.int = T) %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), f) %>%
  mutate(
    ci = glue::glue("{conf.low}--{conf.high}"),
    p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )


cits_tidy_no_18 <- tidy(lm_fit_no_18, conf.int = T) %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), f) %>%
  mutate(
    ci = glue::glue("{conf.low}--{conf.high}"),
    p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )


ols_table <- tidy(lm_fit, conf.int = T) %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), f) %>%
  mutate(
    ` ` = case_when(
      term == "(Intercept)" ~ "Initial control group level",
      term == "time" ~ "Pre-placement control group trend",
      term == "rp" ~ "Difference in level between control and rotating SP",
      term == "rptime" ~ "Rotating SP trend relative to control",
      term == "level" ~ "Post-placement change in control level",
      term == "trend" ~ "Post-placement change in control trend",
      term == "rplevel" ~ "Post-placement rotating SP change in level relative to control",
      term == "rptrend" ~ "Post-placement rotating SP change in trend relative to control"
    ),
    `Coefficient (%)` = estimate,
    `95% CI` = glue::glue("{conf.low} to {conf.high}"),
    `P value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  dplyr::select(` `, `Coefficient (%)`, `95% CI`, `P value`)

saveRDS(ols_table,'./acpic20/ols_table_df.rds')
