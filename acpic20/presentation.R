library(tidyverse)
library(officer)
library(flextable)
library(tidymodels)
library(emo)

my_pres <- read_pptx("./acpic20/ACPIC20 presentation template.pptx")

layout_properties(my_pres, layout = "Blank")

# Title slide ----------
my_pres <- add_slide(my_pres, layout = "Title Slide", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "The SPRAINED study", location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(my_pres, value = "The effect of a specialist paramedic primary care rotation on appropriate non-conveyance decisions: a controlled interrupted time series analysis\n Richard Pilbery\nAndrew Hodge\nProfessor Tracey Young", location = ph_location_type(type = "subTitle"))


# COI ----------

# Professor in Health Economics and Outcome Measurement, co-lead of the Health Economics theme for the Yorkshire and Humber NIHR Applied Research Collaboration.
# The views expressed here are those of the author(s) and not necessarily those of the National Institute for Health Research or the Department of Health and Social Care.

ul <- unordered_list(
  level_list = c(1, 2, 1, 2, 1, 2),
  str_list = c("Richard Pilbery", "YAS employee, not involved in rotating pilot", "Andrew Hodge", "YAS employee, involved with pilot, not involved in data analysis", "Prof. Young", "Time funded by NIHR ARC"))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Conflicts of interest", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = ul, location = ph_location_type(type = "body"))

# The challenge --------

# The National Health Service (NHS) in the United Kingdom is facing a 5% year-on-year increase in demand for urgent and emergency care services [@national_audit_office_nhs_2017]. In 2018/19, ambulance services in England provided a face-to-face assessment to nearly 7.9 million incidents, of which 5.4 million were conveyed to hospital [@nhs_england_ambulance_2019]. This non-conveyance rate of nearly 69% is occurring despite an increase in non-emergency cases and continues to place increasing demands on already crowded emergency departments (EDs), leading to decreased availability of ambulances as turnaround times at hospitals increase . ED overcrowding is a significant issue for patients, resulting in poorer quality of care, increased healthcare costs and potentially, increased mortality

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "The Challenge", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/ramping.jpg", width = 7, height = 4.7), location = ph_location_type(type = "body"), use_loc_size = F)

# Yorkshire -----------------------

# YAS provides 24-hour emergency and health care services for the county of Yorkshire in the north of England. The county has a population of approximately five million, spread over almost 6000 square miles of varied terrain, included isolated moors and dales, coastline and heavily populated urban areas. In 2018/19 YAS received more than 998,500 emergency calls and responded to 798,968 incidents by either sending clinicians to scene or by providing assessment and advice over the telephone.

# Yorkshire Ambulance Service NHS Trust (YAS) have been early adopters of initiatives to respond appropriately to the increase in non-emergency cases, with the introduction of advanced paramedics working in the role of Emergency Care Practitioners (ECP) since 2004. Even without selective dispatching, ECPs consistently have non-conveyance rates double that of other paramedics in the Trust. Since 2015, the specialist paramedic (SP) role has been introduced in YAS, with education comprising of a 1 year university program. However, during the time period of this study (2017--2019), YAS SP non-conveyance rates were around 37.5%, compared to the overall non-conveyance rate of 30%. In contrast, ECPs had non-conveyance rates of over 59% for all call categories.

#YAS employs approximately 118 SPs ?about 8% of paramedic workforce and 33 ECPs.
# 2019 there were 1736 paramedics (although included student paramedics)

my_pres <- add_slide(my_pres, layout = "Two Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Yorkshire", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c("6000 square miles (~15,500 sq km)", "Population: 5 million", "YAS responds to ~800,000 incidents each year"), location = ph_location_left())
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/yorkshire.png", height = 5, width = 3), location = ph_location_right(), use_loc_size = F)


# Opportunity  ---------------
# Rotating paramedics
# Clinicians

#YAS employs approximately 118 SPs and 33 ECPs. 

# In 2018, Health Education England (HEE) funded a pilot scheme to rotate paramedics into a range of healthcare settings, with the aim of improving patient care and relieving pressures on primary care, ambulance services and other parts of the NHS in a sustainable way [9]. A subsequent economic evaluation estimated that the rotating paramedics could save in the region of £275,000 per year in avoidable conveyance and subsequent admission to hospital compared to historic controls. However, data from Yorkshire included 5 ECPs who participated in the pilot and the analysis did not adjust for the difference in patient acuity between the pre- and post-placement phases [10].
# The HEE pilot also presented an opportunity to further develop the decision-making of SPs, with the potential to deliver patient and cost-benefits that were anticipated when the role was created. 

# As the name implies - rotating paras into primary care, EOC, MDT/999

# In the Leeds area, 5 ECPs and 10 SPs worked with a number of primary care organisations. The ECPs and SPs rotated between providing a home visiting service for 15 GP surgeries for 10 weeks, followed by front line operations, either responding to 999 calls or working in the emergency operations centre (EOC) staffing a dedicated dispatch desk for SPs. Occasional days rotating back into GP

# 10 SPs 5 June, 3 August, 2 Oct (all 2018)
# Give numbers of cases

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Opportunity", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c("Rotating paramedics pilot", "Funded by Health Education England", "Opportunity for SP development"), location = ph_location_type(type = "body"))


# Aims/objectives -------

# This study aimed to evaluate whether a primary care placement appropriately increases the level and trend of non-conveyance decisions made by SPs compared to a matched control population of YAS paramedics in a cost-effective manner.
# The primary objective was to determine the change and trend in proportion of appropriate non-conveyance decisions by specialist paramedics who have completed a 10-week placement in a General Practitioner (GP) practice. The secondary objective was to compare the cost-effectiveness of specialist paramedics who have completed a 10-week placement in a GP practice compared to a matched control group of YAS paramedics.

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Aims and objectives", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c("Aim: Evaluate whether primary care placement appropriately increases level and trend of non-conveyance", "Primary objective: Determine change and trend in proportion of appropriate non-conveyance decisions", "Secondary objective: Compare cost-effectiveness of GP placement SPs vs control"), location = ph_location_type(type = "body"))


# Definition of appropriate conveyance
# Since the ambulance service does not routinely capture outcome data for all patients, we pragmatically defined appropriate non-conveyance as any patient episode where the patient was not transferred to hospital and no further calls were made to the ambulance service in the following 72 hours.

# Method --------
 
## Primary outcome analysis
# We conducted a retrospective analysis of appropriate non-conveyance before and after the GP placement, using segmented regression as part of a CITS design [14]. Since the SP placements were staggered, the actual month and year was not utilised. Instead, the number of months before and after the placement were used, so that month 1 was the month that occurred 12 months prior to the GP placement for all SPs and month 24, the month that occurred 12 months after the placement. It was anticipated that this would remove or reduce any autocorrelation. However, we checked for auto-regression and moving averages by performing the Durbin-Watson test and by plotting autocorrelation function and partial autocorrelation function plots. Coefficients from the model were used to predict the absolute change and trend in appropriate non-conveyance following the GP placement.


my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Primary outcome analysis", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c("Before/After design", "Controlled Interrupted Time Series", "24 months (12 months either side of primary care placement)"), location = ph_location_type(type = "body"))

## Secondary outcome analysis
# Salary costs were calculated for the 10 week GP placement and divided by the number of incidents attended by SPs to calculate a per-incident cost. SPs were assumed to be salaried at NHS Agenda for Change mid-band 6, which was £31,121 for 2018/19. Education costs were not included since all SPs had already undertaken the education component prior to the HEE pilot commencing. The resource use related to the 999 call handling, dispatch of an ambulance, cost of conveyance and admission to the ED was calculated using reference costs published by NHS Improvement [15] (Table 1). For example, a non-conveyance would comprise the cost of answering a 999 call and cost of sending an ambulance (see and treat). In the event that a recall was made within 72 hours, the total cost for the non-conveyance would be the 999 call, ambulance (see and treat) plus the cost of the second call i.e. 999 call and ambulance (see and treat or see, treat and convey plus ED attendance).

# Bootstrapping was used to estimate uncertainty (reported as 95% bootstrapped confidence intervals) around cost estimates. Costs of those patients seen by intervention group SPs was compared with matched controls and the results presented as the cost per appropriate conveyance and cost-effectiveness ratio.

cc <- data.frame(
  item = c("999 call", "Ambulance: see and treat", "Ambulance: see, treat and convey", "ED attendance"),
  cost = c("£7.33", "£209.38", "£257.34", "£135.00"),
  stringsAsFactors = F
)

tablecc <- flextable(cc)
tablecc <- fontsize(tablecc, size = 20, part = "all")
tablecc <- align(tablecc, align = "right", part = "all")
tablecc <- color(tablecc, color = "#183558", part = "all")
#tablecc <- bg(tablecc, i = 3, j = c(2, 4), bg = "lightgray")
tablecc <- autofit(tablecc)

a <- ji("check")

my_pres <- add_slide(my_pres, layout = "Two Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Secondary outcome analysis", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c(paste0("Salary costs ", a), paste0("Education costs ", ji("x")), "Unit costs", "Bootstrapping"), location = ph_location_left())
my_pres <- ph_with(my_pres, value = tablecc, location = ph_location_right())


# Participants -------

# Patients

# We hypothesised that appropriate non-conveyance was likely to increase following the 10-week placement, but needed to ensure that we took account of factors previously identified as being important when pre-hospital clinicians make non-conveyance decisions [@ocathain_understanding_2018]. To achieve this, we aimed to match the control and intervention groups on the following variables:
#   
#   + Patient:
#   + Age (5 year increments)
# + Sex
# + Working impression (as determined by paramedic on scene)
# + Time of call (in-hours/out-of-hours)
# + Call category
# + Call month and year
# + Lowest National Early Warning Score (NEWS) threshold
# + Location (lower super output area, LSOA):
#   + Urban/rural
# + Index of multiple deprivation decile
# + Proportion of population within LSOA with a long-term physical or mental illness (0--4%, 4--8%, 8--12%)
# + Paramedics
# + Years registered as a paramedic (<1 year, 1–5 years, >5 years)
# + Role designation at time of incident.

#  Genetic algorithms are a subgroup of evolutionary computing which as the name suggests, imitate biological processes of reproduction and natural selection to solve according to 'fitness'. The 'Matching' package uses this algorithm to find the optimal balance between groups by examining the cumulative probability distribution functions of a variety of standardised statistics such as t-tests and Kolmogorov-Smirnov tests.

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Matching", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c("Patient", "Location", "Paramedics"), location = ph_location_type(type = "body"))


# Results --------

# 
# Between 1st June 2017 and 31st December, 2019 there were 8849 incidents attended by one of the intervention group SPs. Once data was adjusted to remove any cases during the 10 week GP placement, and outside of the 12 months prior to the start of the rotation and 12 months after the end of the rotation, 7349 cases remained. A further 6 had no sex recorded, 15 had no age recorded and 8 had no post code, leaving 7326/7349 (99.7%) cases. A further 4 cases were excluded due to a missing index of multiple deprivation decile (3 cases), rural urban classification (3 cases) and/or prevalence of missing long-term condition data (4 cases), leaving 7322/7349 (99.6%) cases. Finally, no working impression was included in 1785 cases, resulting in a final dataset of 5537/7349 (75.3%) cases for inclusion in the final analysis. Due to the high number of missing working impressions, a sensitivity analysis was performed excluding the working impression as a variable

# The matching algorithm utilised 5198/5537 (93.9%) cases. Overall, the control group was closely matched to the rotational paramedic (intervention group) incidents (defined as less than 10% in standardised mean difference). Only the NEWS risk category and prevalence of long-term conditions were outside this limit.

ul <- unordered_list(
  level_list = c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1),
  str_list = c("Data from 1st June 2017 to 31st December 2019",
               "7349 cases",
               "Missing data",
                  "1785 : No working impression",
                  "15 : No age",
                  "8 : No postcode",
                  "6 : No sex",
                  "4 : No IMD/rural urban class/LTC",
               "5537/7349 (75.3%) eligible",
               "5198/5537 (93.9%) utilised by matching algorithm"))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Results", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = ul, location = ph_location_type(type = "body"))

# Demographic figures1 ------

# Operational activity was higher pre-placement since intervention group SPs had to undertake a range of additional activities in the post-placement phase, including staffing a dedicated SP dispatch desk in EOC and working in GP practices as part of the HEE pilot (Table \@ref(tab:activity)). Post-placement, there were also differences in triage call category (a marker of perceived acuity following telephone triage of the call), and physiological acuity based on the NEWS risk category that the SPs were tasked to attend.

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure1.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "Monthly totals of incidents attended by rotating SPs", location = ph_location_label(ph_label = "Text Placeholder 2"))

# Activity table ----------------

activity_df <- readRDS('SynData/summary_activity_table.rds') %>%
  rename(
    Activity = activity,
  )

table1 <- flextable(activity_df)
table1 <- fontsize(table1, size = 20, part = "all")
table1 <- align(table1, align = "right", part = "all")
table1 <- color(table1, color = "#183558", part = "all")
table1 <- bg(table1, i = 3, j = c(2, 4), bg = "lightgray")
table1 <- autofit(table1)

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Rotating SP daily activity", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = table1, location = ph_location_type(type = "body"))

# Demographic figures2 ------

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure2.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "AMPDS call category pre- and post-placement", location = ph_location_label(ph_label = "Text Placeholder 2"))

# Demographic figure3 -----------

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure3.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "NEWS risk category pre- and post-placement", location = ph_location_label(ph_label = "Text Placeholder 2"))

# Demographic figure4a -----------

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure4a.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "CITS model: Control group", location = ph_location_label(ph_label = "Text Placeholder 2"))

# Demographic figure4b -----------

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure4b.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "CITS model: Control group", location = ph_location_label(ph_label = "Text Placeholder 2"))

# Demographic figure4c -----------

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure4c.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "CITS model: Rotating SP group", location = ph_location_label(ph_label = "Text Placeholder 2"))

# Demographic figure4d -----------

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure4d.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "CITS model: Rotating SP group", location = ph_location_label(ph_label = "Text Placeholder 2"))

# Demographic figure4e -----------

my_pres <- add_slide(my_pres, layout = "Blank", master = "Office Theme")
my_pres <- ph_with(my_pres, value = external_img("./acpic20/images/figure4e.png", width = 9, height = 6), location = ph_location_type(type = "pic"), use_loc_size = F)
my_pres <- ph_with(my_pres, value = "Final fitted CITS model", location = ph_location_label(ph_label = "Text Placeholder 2"))


# CITS table ------------

ols_table_df <- readRDS('./acpic20/ols_table_df.rds')

table2 <- flextable(ols_table_df)
table2 <- fontsize(table2, size = 16, part = "all")
table2 <- align(table2, align = "left", part = "all")
table2 <- color(table2, color = "#183558", part = "all")
table2 <- bg(table2, i = c(7,8), j = 2, bg = "lightgray")
table2 <- autofit(table2)

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Segmented regression analysis", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = table2, location = ph_location_type(type = "body"))

# Economics ---------


ul <- unordered_list(
  level_list = c(1, 2, 2, 1, 2, 1, 2, 2),
  str_list = c("Mean cost per appropriate non-conveyance", "Rotating SP: £509.42 (95% bootstrapped CI £485.94–£535.41)", "Control: £1124.41 (95% bootstrapped CI £1041.89–£1218.31)", "Mean saving of £615 per appropriate non-conveyance", "95% bootstrapped CI £545.31–£686.69", "Cost-effectiveness ratio", "£1758.89 per percentage increase in appropriate non-conveyance", "95% bootstrapped CI £1477.76–£2133.08"))

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Let's talk money...", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = ul, location = ph_location_type(type = "body"))

# Limitations ---------

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Limitations", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c("Routine observational data vs RCT", "Re-contact to 999 only", "Patient re-identification", "Missing working impressions", "Determining paramedic roles"), location = ph_location_type(type = "body"))

# Conclusion ----------

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Conclusion", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = c("Clinically important and statistically significant increase in appropriate non-conveyance rates", "Persisted for the 12-month period following the rotation", "Demonstrated cost savings compared to usual care"), location = ph_location_type(type = "body"))

# Thank you ---------------

my_pres <- add_slide(my_pres, layout = "Title Slide", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Questions?", location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(my_pres, value = "Pre-print URL: bit.ly/SPRAINED", location = ph_location_type(type = "subTitle"))




print(my_pres, target = "./acpic20/acpic20-presentation.pptx")

