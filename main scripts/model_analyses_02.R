# keep private-sector observed in at least two waves
data <- private_sector %>%
  group_by(respondentid) %>%
  filter(n_distinct(wave) >= 2) %>%
  mutate(n_waves = n_distinct(wave)) %>%
  ungroup()

# keep variables used
variables <- data[, c(
  "wave","respondentid","wght","qsize","sector","onlinea","supplyrisk",
  "exercise","incidman","bgovern1","bgovern2","boardtrainfreq","bdiscss",
  "bengagec","devices","rulesa","rulesb","rulesc","rulesd","rulese",
  "rulesf","rulesg","rulesh","rulesi","rulesj","identa","identb","identc",
  "identd","gova","govb","govc","govd","gove","improvea","improveb",
  "improvec","improved","improvee","improvef","improveg",
  "incident12", "incident13","incidenti","incidenta","incidentc","incidentd",
  "incidente","incidentf","incidentg","incidenth","incidentj","incidentk"
)]

# recode analysis variables
variables <- variables %>%
  mutate(
    size = ifelse(qsize == 6, NA_real_, qsize),

    cloud_storage = case_when(
      onlinea == 1 ~ 1,
      onlinea == 2 ~ 0,
      onlinea == 3 ~ NA_real_
    ),

    supplierrisk_mgmt = case_when(
      supplyrisk == 1 ~ 1,
      supplyrisk == 2 ~ 0,
      supplyrisk == 3 ~ NA_real_
    ),

    incident_plan = case_when(
      incidman == 1 ~ 1,
      incidman == 2 ~ 0,
      incidman == 3 ~ NA_real_
    ),

    incident_exercise = case_when(
      exercise == 1 ~ 1,
      exercise == 2 ~ 0,
      exercise == 3 ~ NA_real_,
      is.na(exercise) & incidman == 2 ~ 0,
      is.na(exercise) & incidman == 3 ~ NA_real_,
      TRUE ~ NA_real_
    ),

    byod_access = case_when(
      devices == 1 ~ 1,
      devices == 2 ~ 0,
      devices == 3 ~ NA_real_
    ),

    dv = case_when(
      incident12 == 1 ~ 1,
      incident12 == 2 ~ 0
    )
  )

# incident response preparedness index
variables <- variables %>%
  mutate(
    incident_preparedness = case_when(
      incident_plan == 0 ~ 0,
      incident_plan == 1 & incident_exercise == 0 ~ 1,
      incident_plan == 1 & incident_exercise == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )

# item sets for cyber management measures
rules_items <- c("rulesa", "rulesb", "rulesc", "rulesd", "rulese",
                 "rulesf", "rulesg", "rulesh", "rulesi", "rulesj")

ident_items <- c("identa", "identb", "identc", "identd")

gov_items <- c("gova", "govb", "govc", "govd", "gove")

improve_items <- c("improvea", "improveb", "improvec", "improved",
                   "improvee", "improvef", "improveg")

# cybersecurity rules count
variables <- variables %>%
  mutate(
    cybersecurity_rules = if_else(
      if_all(all_of(rules_items), ~ is.na(.x) | .x %in% c(3, 4)),
      NA_real_,
      rowSums(across(all_of(rules_items), ~ .x == 1),na.rm = TRUE)
    )
  )

table(variables$cybersecurity_rules, useNA = "always")

# cyber risk identification count
variables <- variables %>%
  mutate(
    cyberrisk_identification = if_else(
      if_all(all_of(ident_items), ~ is.na(.x) | .x %in% c(3, 4)),
      NA_real_,
      rowSums(across(all_of(ident_items), ~ .x == 1),na.rm = TRUE)
    )
  )

table(variables$cyberrisk_identification, useNA = "always")

# cyber risk documentation count
variables <- variables %>%
  mutate(
    cyberrisk_documentation = if_else(
      if_all(all_of(gov_items), ~ is.na(.x) | .x %in% c(3, 4)),
      NA_real_,
      rowSums(across(all_of(gov_items), ~ .x == 1),na.rm = TRUE)
    )
  )

table(variables$cyberrisk_documentation, useNA = "always")

# cybersecurity improvement count
variables <- variables %>%
  mutate(
    cyberrisk_improvement = if_else(
      if_all(all_of(improve_items), ~ is.na(.x) | .x %in% c(3, 4, 5)),
      NA_real_,
      rowSums(across(all_of(improve_items), ~ .x == 1),na.rm = TRUE)
    )
  )

table(variables$cyberrisk_improvement, useNA = "always")

# cyber management factor
cyber_management_items <- c("cybersecurity_rules","cyberrisk_identification",
                            "cyberrisk_documentation","cyberrisk_improvement")

# prepare data for measurement invariance tests
cfa_data1 <- variables %>%
  select(respondentid,wave, all_of(cyber_management_items)) %>%
  filter(if_any(all_of(cyber_management_items), ~ !is.na(.x))) %>%
  mutate(
    wave_f = factor(
      as.numeric(wave),
      levels = 1:5,
      labels = c("wave1","wave2","wave3","wave4","wave5")
    )
  )

cyber_management_model <- '
  cyber_management_factor =~ cybersecurity_rules + cyberrisk_identification +
  cyberrisk_documentation + cyberrisk_improvement
'

# configural invariance
fit_configural1 <- cfa(
  model = cyber_management_model,
  data = cfa_data1,
  group = "wave_f",
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)
summary(fit_configural1,fit.measures = TRUE,standardized = TRUE)

# metric invariance
fit_metric1 <- cfa(
  model = cyber_management_model,
  data = cfa_data1,
  group = "wave_f",
  group.equal = "loadings",
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)
summary(fit_metric1,fit.measures = TRUE,standardized = TRUE)

# scalar invariance
fit_scalar1 <- cfa(
  model = cyber_management_model,
  data = cfa_data1,
  group = "wave_f",
  group.equal = c(
    "loadings",
    "intercepts"
  ),
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)
summary(fit_scalar1,fit.measures = TRUE,standardized = TRUE)

lavTestLRT(fit_configural1,fit_metric1,fit_scalar1)

# extract cyber management factor scores
latent_scores1 <- lavPredict(fit_scalar1, type = "lv", assemble = TRUE)

cyber_management_scores <- cfa_data1 %>%
  mutate(
    cyber_management_latent = latent_scores1[, "cyber_management_factor"]
  ) %>%
  select(respondentid, wave, cyber_management_latent)

variables <- variables %>%
  left_join(cyber_management_scores, by = c("respondentid", "wave"))

summary(variables$cyber_management_latent)
table(is.na(variables$cyber_management_latent))

# add wave 1 board training item
w1_btrain <- csls1 %>%
  mutate(wave = 1) %>%
  rename(respondentid = id) %>%
  select(respondentid, wave, btrain)

# recode board governance measures
variables <- variables %>%
  left_join(w1_btrain, by = c("respondentid", "wave")) %>%
  mutate(
    boardtrain_raw = case_when(
      wave %in% 2:5 ~ boardtrainfreq,
      wave == 1 & btrain == 1 ~ 1,
      wave == 1 & btrain == 2 ~ 5,
      wave == 1 & btrain == -97 ~ 6,
      TRUE ~ NA_real_
    ),

    boardtrain_bin = case_when(
      boardtrain_raw %in% 1:4 ~ 1,
      boardtrain_raw == 5 ~ 0,
      boardtrain_raw %in% c(6, 7) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    boarddiscussion_ord = case_when(
      bdiscss == 1 ~ 0,
      bdiscss %in% c(2, 3, 4) ~ 1,
      bdiscss %in% c(5, 6, 7, 8) ~ 2,
      bdiscss %in% c(9, 10) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    boardintegration_bin = case_when(
      bengagec %in% c(1, 2) ~ 1,
      bengagec %in% c(3, 4, 5) ~ 0,
      bengagec %in% c(6, 7) ~ NA_real_,
      is.na(bengagec) & bdiscss == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# board governance factor
board_items <- c("bgovern1", "bgovern2", "boardtrain_bin",
                 "boarddiscussion_ord", "boardintegration_bin")

# prepare board items for measurement invariance tests
cfa_data <- variables %>%
  select(respondentid, wave, all_of(board_items)) %>%
  filter(if_any(all_of(board_items), ~ !is.na(.x))) %>%
  mutate(
    wave_f = factor(
      as.numeric(wave),
      levels = 1:5,
      labels = c("wave1", "wave2", "wave3", "wave4", "wave5")
    )
  )

board_model <- '
  board_factor =~ bgovern1 + bgovern2 + boardtrain_bin +
                  boarddiscussion_ord + boardintegration_bin
'

# configural invariance
fit_configural2 <- cfa(
  model          = board_model,
  data           = cfa_data,
  group          = "wave_f",
  ordered        = board_items,
  estimator      = "PML",
  std.lv         = TRUE,
  missing        = "available.cases"
)
summary(fit_configural2, fit.measures = TRUE, standardized = TRUE)

# metric invariance
fit_metric2 <- cfa(
  model       = board_model,
  data        = cfa_data,
  group       = "wave_f",
  group.equal = c("loadings"),
  ordered     = board_items,
  estimator   = "PML",
  std.lv      = TRUE,
  missing     = "available.cases"
)
summary(fit_metric2, fit.measures = TRUE, standardized = TRUE)

# scalar invariance
fit_scalar2 <- cfa(
  model          = board_model,
  data           = cfa_data,
  group          = "wave_f",
  group.equal    = c("loadings", "thresholds"),
  ordered        = board_items,
  estimator      = "PML",
  std.lv         = TRUE,
  missing        = "available.cases"
)
summary(fit_scalar2, fit.measures = TRUE, standardized = TRUE)

# extract board governance factor scores
latent_scores2 <- lavPredict(fit_scalar2, type = "lv", assemble = TRUE)

board_scores <- cfa_data %>%
  mutate(board_cyber_governance_latent = latent_scores2[, "board_factor"]) %>%
  select(respondentid, wave, board_cyber_governance_latent)

variables <- variables %>%
  left_join(board_scores, by = c("respondentid", "wave"))

summary(variables$board_cyber_governance_latent)
sum(is.na(variables$board_cyber_governance_latent))

# time-varying predictors
tv_vars <- c("cloud_storage","supplierrisk_mgmt","incident_preparedness",
             "board_cyber_governance_latent","byod_access","cyber_management_latent")

# mundlak within between decomposition
# between
variables <- variables %>%
  group_by(respondentid) %>%
  mutate(
    across(all_of(tv_vars), ~ if (all(is.na(.x))) {NA_real_}
           else {mean(.x, na.rm = TRUE)},
           .names = "{.col}_between"
    )
  ) %>%
  ungroup()

# within
for (v in tv_vars) {
  variables[[paste0(v, "_within")]] <- variables[[v]] - variables[[paste0(v, "_between")]]
}

# flag predictors
flag_data <- variables %>%
  arrange(respondentid, wave) %>%
  group_by(respondentid) %>%
  summarise(
    size = first(na.omit(size)),
    sector = first(na.omit(sector)),
    .groups = "drop"
  )

flag_data <- flag_data %>%
  mutate(
    size_250_499 = if_else(size == 3, 1, 0),
    size_500_999 = if_else(size == 4, 1, 0),
    size_1000plus = if_else(size == 5, 1, 0),

    sector_industrial_operational_infrastructure = if_else(sector %in% c(1, 2, 3, 5), 1, 0),
    sector_consumer_facing_services = if_else(sector %in% c(4, 6, 14), 1, 0),
    sector_property_administrative_membership_services = if_else(sector %in% c(9, 11, 15), 1, 0),
    sector_education_health_social_services = if_else(sector %in% c(12, 13), 1, 0)
  )

flag_vars <- c("size_250_499","size_500_999","size_1000plus",
               "sector_industrial_operational_infrastructure",
               "sector_consumer_facing_services",
               "sector_property_administrative_membership_services",
               "sector_education_health_social_services")

# convert to wide format
wide_dv <- variables %>%
  select(respondentid, wave, dv) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = dv,
    names_glue = "dv_w{wave}"
  )

riar_data <- wide_dv %>%
  left_join(
    flag_data %>%
      select(respondentid,all_of(flag_vars)),by = "respondentid") %>%
  left_join(panel_weights,by = "respondentid")


# model 1: test flags only
m1 <- '
  # random intercept component
  RI_dv =~ 1*dv_w1 + 1*dv_w2 + 1*dv_w3 + 1*dv_w4 + 1*dv_w5

  # within-business components
  W_dv1 =~ 1*dv_w1
  W_dv2 =~ 1*dv_w2
  W_dv3 =~ 1*dv_w3
  W_dv4 =~ 1*dv_w4
  W_dv5 =~ 1*dv_w5

  dv_w1 ~~ 0*dv_w1
  dv_w2 ~~ 0*dv_w2
  dv_w3 ~~ 0*dv_w3
  dv_w4 ~~ 0*dv_w4
  dv_w5 ~~ 0*dv_w5

  RI_dv ~~ 0*W_dv1
  RI_dv ~~ 0*W_dv2
  RI_dv ~~ 0*W_dv3
  RI_dv ~~ 0*W_dv4
  RI_dv ~~ 0*W_dv5

  # time-invariant covariates
  RI_dv ~ size_250_499 +
          size_500_999 +
          size_1000plus +
          sector_industrial_operational_infrastructure +
          sector_consumer_facing_services +
          sector_property_administrative_membership_services +
          sector_education_health_social_services
'

fit1 <- sem(
  model = m1,
  data = riar_data,
  estimator = "MLR",
  missing = "fiml",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# model 2: test flags and direct boost
m2 <- '
  RI_dv =~ 1*dv_w1 + 1*dv_w2 + 1*dv_w3 + 1*dv_w4 + 1*dv_w5

  W_dv1 =~ 1*dv_w1
  W_dv2 =~ 1*dv_w2
  W_dv3 =~ 1*dv_w3
  W_dv4 =~ 1*dv_w4
  W_dv5 =~ 1*dv_w5

  dv_w1 ~~ 0*dv_w1
  dv_w2 ~~ 0*dv_w2
  dv_w3 ~~ 0*dv_w3
  dv_w4 ~~ 0*dv_w4
  dv_w5 ~~ 0*dv_w5

  RI_dv ~~ 0*W_dv1
  RI_dv ~~ 0*W_dv2
  RI_dv ~~ 0*W_dv3
  RI_dv ~~ 0*W_dv4
  RI_dv ~~ 0*W_dv5

  RI_dv ~ size_250_499 +
          size_500_999 +
          size_1000plus +
          sector_industrial_operational_infrastructure +
          sector_consumer_facing_services +
          sector_property_administrative_membership_services +
          sector_education_health_social_services

  # autoregressive effect (direct boost)
  W_dv2 ~ boost*W_dv1
  W_dv3 ~ boost*W_dv2
  W_dv4 ~ boost*W_dv3
  W_dv5 ~ boost*W_dv4
'

fit2 <- sem(
  model = m2,
  data = riar_data,
  estimator = "MLR",
  missing = "fiml",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit2,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)
lavInspect(fit2, "converged")

# prepare within- and between-business predictors for model 3
within_vars <- paste0(tv_vars, "_within")
between_vars <- paste0(tv_vars, "_between")

# between-business predictors
between_data <- variables %>%
  group_by(respondentid) %>%
  summarise(across(all_of(between_vars),first),
            .groups = "drop"
  )

# within-business predictors per wave
within_data <- variables %>%
  select(respondentid,wave,all_of(within_vars)) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = all_of(within_vars),
    names_glue = "{.value}_w{wave}"
  )

riar_data <- riar_data %>%
  left_join(between_data,by = "respondentid") %>%
  left_join(within_data,by = "respondentid")

# model 3: flags, direct boost and organisational cs predictors
# autoregressive model with within-between predictors
m3 <- '
  RI_dv =~ 1*dv_w1 + 1*dv_w2 + 1*dv_w3 + 1*dv_w4 + 1*dv_w5

  W_dv1 =~ 1*dv_w1
  W_dv2 =~ 1*dv_w2
  W_dv3 =~ 1*dv_w3
  W_dv4 =~ 1*dv_w4
  W_dv5 =~ 1*dv_w5

  dv_w1 ~~ 0*dv_w1
  dv_w2 ~~ 0*dv_w2
  dv_w3 ~~ 0*dv_w3
  dv_w4 ~~ 0*dv_w4
  dv_w5 ~~ 0*dv_w5

  RI_dv ~~ 0*W_dv1
  RI_dv ~~ 0*W_dv2
  RI_dv ~~ 0*W_dv3
  RI_dv ~~ 0*W_dv4
  RI_dv ~~ 0*W_dv5

  RI_dv ~ size_250_499 +
          size_500_999 +
          size_1000plus +
          sector_industrial_operational_infrastructure +
          sector_consumer_facing_services +
          sector_property_administrative_membership_services +
          sector_education_health_social_services +
          cloud_storage_between +
          supplierrisk_mgmt_between +
          incident_preparedness_between +
          board_cyber_governance_latent_between +
          byod_access_between +
          cyber_management_latent_between

  W_dv2 ~ boost*W_dv1 +
          cloud*cloud_storage_within_w1 +
          supplier*supplierrisk_mgmt_within_w1 +
          preparedness*incident_preparedness_within_w1 +
          board*board_cyber_governance_latent_within_w1 +
          byod*byod_access_within_w1 +
          cyber_management*cyber_management_latent_within_w1

  W_dv3 ~ boost*W_dv2 +
          cloud*cloud_storage_within_w2 +
          supplier*supplierrisk_mgmt_within_w2 +
          preparedness*incident_preparedness_within_w2 +
          board*board_cyber_governance_latent_within_w2 +
          byod*byod_access_within_w2 +
          cyber_management*cyber_management_latent_within_w2

  W_dv4 ~ boost*W_dv3 +
          cloud*cloud_storage_within_w3 +
          supplier*supplierrisk_mgmt_within_w3 +
          preparedness*incident_preparedness_within_w3 +
          board*board_cyber_governance_latent_within_w3 +
          byod*byod_access_within_w3 +
          cyber_management*cyber_management_latent_within_w3

  W_dv5 ~ boost*W_dv4 +
          cloud*cloud_storage_within_w4 +
          supplier*supplierrisk_mgmt_within_w4 +
          preparedness*incident_preparedness_within_w4 +
          board*board_cyber_governance_latent_within_w4 +
          byod*byod_access_within_w4 +
          cyber_management*cyber_management_latent_within_w4
'

fit3 <- sem(
  model = m3,
  data = riar_data,
  estimator = "MLR",
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit3,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)
lavInspect(fit3, "converged")

# model 4: moderation effects
# model 4a: Cloud storage moderation
m4_cloud <- paste0(
  m3,
  '
  W_dv2 ~ cloud_between_mod*W_dv1:cloud_storage_between +
          cloud_within_mod*W_dv1:cloud_storage_within_w1

  W_dv3 ~ cloud_between_mod*W_dv2:cloud_storage_between +
          cloud_within_mod*W_dv2:cloud_storage_within_w2

  W_dv4 ~ cloud_between_mod*W_dv3:cloud_storage_between +
          cloud_within_mod*W_dv3:cloud_storage_within_w3

  W_dv5 ~ cloud_between_mod*W_dv4:cloud_storage_between +
          cloud_within_mod*W_dv4:cloud_storage_within_w4
  '
)

fit4_cloud <- modsem_pi(
  model.syntax = m4_cloud,
  data = riar_data,
  method = "dblcent",
  res.cov.method = "none",
  estimator = "MLR",
  na.rm = FALSE,
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE,
  em.h1.iter.max = 10000
)
lav4_cloud <- extract_lavaan(fit4_cloud)

# summary(lav4_cloud,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_cloud, "converged")
lavInspect(lav4_cloud, "post.check")

# model 4b: supplier cyber-risk management moderation
m4_supplier <- paste0(
  m3,
  '
  W_dv2 ~ supplier_between_mod*W_dv1:supplierrisk_mgmt_between +
          supplier_within_mod*W_dv1:supplierrisk_mgmt_within_w1

  W_dv3 ~ supplier_between_mod*W_dv2:supplierrisk_mgmt_between +
          supplier_within_mod*W_dv2:supplierrisk_mgmt_within_w2

  W_dv4 ~ supplier_between_mod*W_dv3:supplierrisk_mgmt_between +
          supplier_within_mod*W_dv3:supplierrisk_mgmt_within_w3

  W_dv5 ~ supplier_between_mod*W_dv4:supplierrisk_mgmt_between +
          supplier_within_mod*W_dv4:supplierrisk_mgmt_within_w4
  '
)

fit4_supplier <- modsem_pi(
  model.syntax = m4_supplier,
  data = riar_data,
  method = "dblcent",
  res.cov.method = "none",
  estimator = "MLR",
  na.rm = FALSE,
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE,
  em.h1.iter.max = 10000
)
lav4_supplier <- extract_lavaan(fit4_supplier)

# summary(lav4_supplier,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_supplier, "converged")
lavInspect(lav4_supplier, "post.check")

# model 4c: incident preparedness moderation
m4_preparedness <- paste0(
  m3,
  '
  W_dv2 ~ preparedness_between_mod*W_dv1:incident_preparedness_between +
          preparedness_within_mod*W_dv1:incident_preparedness_within_w1

  W_dv3 ~ preparedness_between_mod*W_dv2:incident_preparedness_between +
          preparedness_within_mod*W_dv2:incident_preparedness_within_w2

  W_dv4 ~ preparedness_between_mod*W_dv3:incident_preparedness_between +
          preparedness_within_mod*W_dv3:incident_preparedness_within_w3

  W_dv5 ~ preparedness_between_mod*W_dv4:incident_preparedness_between +
          preparedness_within_mod*W_dv4:incident_preparedness_within_w4
  '
)

fit4_preparedness <- modsem_pi(
  model.syntax = m4_preparedness,
  data = riar_data,
  method = "dblcent",
  res.cov.method = "none",
  estimator = "MLR",
  na.rm = FALSE,
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE,
  em.h1.iter.max = 10000
)
lav4_preparedness <- extract_lavaan(fit4_preparedness)

# summary(lav4_preparedness,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_preparedness, "converged")
lavInspect(lav4_preparedness, "post.check")

# model 4d: board cybersecurity governance moderation
m4_board <- paste0(
  m3,
  '
  W_dv2 ~ board_between_mod*W_dv1:board_cyber_governance_latent_between +
          board_within_mod*W_dv1:board_cyber_governance_latent_within_w1

  W_dv3 ~ board_between_mod*W_dv2:board_cyber_governance_latent_between +
          board_within_mod*W_dv2:board_cyber_governance_latent_within_w2

  W_dv4 ~ board_between_mod*W_dv3:board_cyber_governance_latent_between +
          board_within_mod*W_dv3:board_cyber_governance_latent_within_w3

  W_dv5 ~ board_between_mod*W_dv4:board_cyber_governance_latent_between +
          board_within_mod*W_dv4:board_cyber_governance_latent_within_w4
  '
)

fit4_board <- modsem_pi(
  model.syntax = m4_board,
  data = riar_data,
  method = "dblcent",
  res.cov.method = "none",
  estimator = "MLR",
  na.rm = FALSE,
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE,
  em.h1.iter.max = 10000
)
lav4_board <- extract_lavaan(fit4_board)

# summary(lav4_board,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_board, "converged")
lavInspect(lav4_board, "post.check")

# model 4e: personal-device access moderation
m4_byod <- paste0(
  m3,
  '
  W_dv2 ~ byod_between_mod*W_dv1:byod_access_between +
          byod_within_mod*W_dv1:byod_access_within_w1

  W_dv3 ~ byod_between_mod*W_dv2:byod_access_between +
          byod_within_mod*W_dv2:byod_access_within_w2

  W_dv4 ~ byod_between_mod*W_dv3:byod_access_between +
          byod_within_mod*W_dv3:byod_access_within_w3

  W_dv5 ~ byod_between_mod*W_dv4:byod_access_between +
          byod_within_mod*W_dv4:byod_access_within_w4
  '
)

fit4_byod <- modsem_pi(
  model.syntax = m4_byod,
  data = riar_data,
  method = "dblcent",
  res.cov.method = "none",
  estimator = "MLR",
  na.rm = FALSE,
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE,
  em.h1.iter.max = 10000
)
lav4_byod <- extract_lavaan(fit4_byod)

# summary(lav4_byod,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_byod, "converged")
lavInspect(lav4_byod, "post.check")

# model 4f: cs management practices moderation
m4_management <- paste0(
  m3,
  '
  W_dv2 ~ cyber_management_between_mod*W_dv1:cyber_management_latent_between +
          cyber_management_within_mod*W_dv1:cyber_management_latent_within_w1

  W_dv3 ~ cyber_management_between_mod*W_dv2:cyber_management_latent_between +
          cyber_management_within_mod*W_dv2:cyber_management_latent_within_w2

  W_dv4 ~ cyber_management_between_mod*W_dv3:cyber_management_latent_between +
          cyber_management_within_mod*W_dv3:cyber_management_latent_within_w3

  W_dv5 ~ cyber_management_between_mod*W_dv4:cyber_management_latent_between +
          cyber_management_within_mod*W_dv4:cyber_management_latent_within_w4
  '
)

fit4_management <- modsem_pi(
  model.syntax = m4_management,
  data = riar_data,
  method = "dblcent",
  res.cov.method = "none",
  estimator = "MLR",
  na.rm = FALSE,
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE,
  em.h1.iter.max = 10000
)
lav4_management <- extract_lavaan(fit4_management)

# summary(lav4_management,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_management, "converged")
lavInspect(lav4_management, "post.check")

save(
  fit1,
  fit2,
  fit3,
  lav4_cloud,
  lav4_supplier,
  lav4_preparedness,
  lav4_board,
  lav4_byod,
  lav4_management,

  file = here(
    "results",
    "main_models.RData"
  )
)

# check interaction results
interaction_results <- bind_rows(
  parameterEstimates(lav4_cloud),
  parameterEstimates(lav4_supplier),
  parameterEstimates(lav4_preparedness),
  parameterEstimates(lav4_board),
  parameterEstimates(lav4_byod),
  parameterEstimates(lav4_management)
) %>%
  filter(
    label %in% c(
      "cloud_between_mod", "cloud_within_mod",
      "supplier_between_mod", "supplier_within_mod",
      "preparedness_between_mod", "preparedness_within_mod",
      "board_between_mod", "board_within_mod",
      "byod_between_mod", "byod_within_mod",
      "cyber_management_between_mod", "cyber_management_within_mod"
    )
  ) %>%
  distinct(label, .keep_all = TRUE) %>%
  mutate(
    sig = case_when(
      pvalue < .001 ~ "***",
      pvalue < .01  ~ "**",
      pvalue < .05  ~ "*",
      pvalue < .10  ~ "+",
      TRUE          ~ ""
    ),
    formatted = sprintf(
      "%.3f%s (%.3f)",
      est,
      sig,
      se
    )
  )

interaction_results %>%
  select(
    label,
    est,
    se,
    pvalue,
    sig,
    formatted
  )
