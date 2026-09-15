# sensitivity analysis: victimisation excluding phishing
variables_nophish <- variables %>%
  mutate(
    dv_nophish = case_when(
      incident13 == 1 ~ 1,
      incident13 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(variables_nophish$dv_nophish, useNA = "always")

wide_dv_nophish <- variables_nophish %>%
  select(respondentid, wave, dv_nophish) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = dv_nophish,
    names_glue = "dv_nophish_w{wave}"
  )

riar_data_nophish <- wide_dv_nophish %>%
  left_join(
    flag_data %>%
      select(respondentid, all_of(flag_vars)),
    by = "respondentid"
  ) %>%
  left_join(panel_weights, by = "respondentid") %>%
  left_join(between_data, by = "respondentid") %>%
  left_join(within_data, by = "respondentid")

# model 1: test flags only
m1_nophish <- '
  # random intercept component
  RI_dv =~ 1*dv_nophish_w1 + 1*dv_nophish_w2 + 1*dv_nophish_w3 +
           1*dv_nophish_w4 + 1*dv_nophish_w5

  # within-business components
  W_dv1 =~ 1*dv_nophish_w1
  W_dv2 =~ 1*dv_nophish_w2
  W_dv3 =~ 1*dv_nophish_w3
  W_dv4 =~ 1*dv_nophish_w4
  W_dv5 =~ 1*dv_nophish_w5

  dv_nophish_w1 ~~ 0*dv_nophish_w1
  dv_nophish_w2 ~~ 0*dv_nophish_w2
  dv_nophish_w3 ~~ 0*dv_nophish_w3
  dv_nophish_w4 ~~ 0*dv_nophish_w4
  dv_nophish_w5 ~~ 0*dv_nophish_w5

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

fit1_nophish <- sem(
  model = m1_nophish,
  data = riar_data_nophish,
  estimator = "MLR",
  missing = "fiml",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit1_nophish, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# model 2: test flags and direct boost
# random intercept autoregressive model
m2_nophish <- '
  RI_dv =~ 1*dv_nophish_w1 + 1*dv_nophish_w2 + 1*dv_nophish_w3 +
           1*dv_nophish_w4 + 1*dv_nophish_w5

  W_dv1 =~ 1*dv_nophish_w1
  W_dv2 =~ 1*dv_nophish_w2
  W_dv3 =~ 1*dv_nophish_w3
  W_dv4 =~ 1*dv_nophish_w4
  W_dv5 =~ 1*dv_nophish_w5

  dv_nophish_w1 ~~ 0*dv_nophish_w1
  dv_nophish_w2 ~~ 0*dv_nophish_w2
  dv_nophish_w3 ~~ 0*dv_nophish_w3
  dv_nophish_w4 ~~ 0*dv_nophish_w4
  dv_nophish_w5 ~~ 0*dv_nophish_w5

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

fit2_nophish <- sem(
  model = m2_nophish,
  data = riar_data_nophish,
  estimator = "MLR",
  missing = "fiml",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit2_nophish, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
lavInspect(fit2_nophish, "converged")

# model 3: flags, direct boost and organisational cs predictors
# autoregressive model with within-between predictors
m3_nophish <- '
  RI_dv =~ 1*dv_nophish_w1 + 1*dv_nophish_w2 + 1*dv_nophish_w3 +
           1*dv_nophish_w4 + 1*dv_nophish_w5

  W_dv1 =~ 1*dv_nophish_w1
  W_dv2 =~ 1*dv_nophish_w2
  W_dv3 =~ 1*dv_nophish_w3
  W_dv4 =~ 1*dv_nophish_w4
  W_dv5 =~ 1*dv_nophish_w5

  dv_nophish_w1 ~~ 0*dv_nophish_w1
  dv_nophish_w2 ~~ 0*dv_nophish_w2
  dv_nophish_w3 ~~ 0*dv_nophish_w3
  dv_nophish_w4 ~~ 0*dv_nophish_w4
  dv_nophish_w5 ~~ 0*dv_nophish_w5

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

fit3_nophish <- sem(
  model = m3_nophish,
  data = riar_data_nophish,
  estimator = "MLR",
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit3_nophish, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
lavInspect(fit3_nophish, "converged")

# model 4: moderation effects
# model 4a: cloud storage moderation
m4_cloud_nophish <- paste0(
  m3_nophish,
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

fit4_cloud_nophish <- modsem_pi(
  model.syntax = m4_cloud_nophish,
  data = riar_data_nophish,
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
lav4_cloud_nophish <- extract_lavaan(fit4_cloud_nophish)

summary(lav4_cloud_nophish,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

# model 4b: supplier cyber-risk management moderation
m4_supplier_nophish <- paste0(
  m3_nophish,
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

fit4_supplier_nophish <- modsem_pi(
  model.syntax = m4_supplier_nophish,
  data = riar_data_nophish,
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
lav4_supplier_nophish <- extract_lavaan(fit4_supplier_nophish)

summary(lav4_supplier_nophish,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

# model 4c: incident preparedness moderation
m4_preparedness_nophish <- paste0(
  m3_nophish,
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

fit4_preparedness_nophish <- modsem_pi(
  model.syntax = m4_preparedness_nophish,
  data = riar_data_nophish,
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

lav4_preparedness_nophish <- extract_lavaan(fit4_preparedness_nophish)

summary(lav4_preparedness_nophish,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

# model 4d: board cybersecurity governance moderation
m4_board_nophish <- paste0(
  m3_nophish,
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

fit4_board_nophish <- modsem_pi(
  model.syntax = m4_board_nophish,
  data = riar_data_nophish,
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
lav4_board_nophish <- extract_lavaan(fit4_board_nophish)

summary(lav4_board_nophish,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

# model 4e: personal-device access moderation
m4_byod_nophish <- paste0(
  m3_nophish,
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

fit4_byod_nophish <- modsem_pi(
  model.syntax = m4_byod_nophish,
  data = riar_data_nophish,
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

lav4_byod_nophish <- extract_lavaan(fit4_byod_nophish)

summary(lav4_byod_nophish,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

# model 4f: cybersecurity management moderation
m4_management_nophish <- paste0(
  m3_nophish,
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

fit4_management_nophish <- modsem_pi(
  model.syntax = m4_management_nophish,
  data = riar_data_nophish,
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
lav4_management_nophish <- extract_lavaan(fit4_management_nophish)

summary(lav4_management_nophish,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)