# sensitivity analysis: phishing-only victimisation
variables_phishing <- variables %>%
  mutate(
    dv_phishing = case_when(
      incidenti == 1 ~ 1,
      incidenti == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(variables_phishing$dv_phishing, useNA = "always")

wide_dv_phishing <- variables_phishing %>%
  select(respondentid, wave, dv_phishing) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = dv_phishing,
    names_glue = "dv_phishing_w{wave}"
  )

riar_data_phishing <- wide_dv_phishing %>%
  left_join(
    flag_data %>%
      select(respondentid, all_of(flag_vars)),by = "respondentid") %>%
  left_join(panel_weights, by = "respondentid") %>%
  left_join(between_data, by = "respondentid") %>%
  left_join(within_data, by = "respondentid")


# model 3: flags, direct boost and organisational cs predictors
m3_phishing <- '
  RI_dv =~ 1*dv_phishing_w1 + 1*dv_phishing_w2 + 1*dv_phishing_w3 +
           1*dv_phishing_w4 + 1*dv_phishing_w5

  W_dv1 =~ 1*dv_phishing_w1
  W_dv2 =~ 1*dv_phishing_w2
  W_dv3 =~ 1*dv_phishing_w3
  W_dv4 =~ 1*dv_phishing_w4
  W_dv5 =~ 1*dv_phishing_w5

  dv_phishing_w1 ~~ 0*dv_phishing_w1
  dv_phishing_w2 ~~ 0*dv_phishing_w2
  dv_phishing_w3 ~~ 0*dv_phishing_w3
  dv_phishing_w4 ~~ 0*dv_phishing_w4
  dv_phishing_w5 ~~ 0*dv_phishing_w5

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

fit3_phishing <- sem(
  model = m3_phishing,
  data = riar_data_phishing,
  estimator = "MLR",
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit3_phishing, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
lavInspect(fit3_phishing, "converged")

# model 4: moderation effects
# model 4a: cloud storage moderation
m4_cloud_phishing <- paste0(
  m3_phishing,
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

fit4_cloud_phishing <- modsem_pi(
  model.syntax = m4_cloud_phishing,
  data = riar_data_phishing,
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

lav4_cloud_phishing <- extract_lavaan(fit4_cloud_phishing)

# summary(lav4_cloud_phishing, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# model 4b: supplier cyber-risk management moderation
m4_supplier_phishing <- paste0(
  m3_phishing,
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

fit4_supplier_phishing <- modsem_pi(
  model.syntax = m4_supplier_phishing,
  data = riar_data_phishing,
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

lav4_supplier_phishing <- extract_lavaan(fit4_supplier_phishing)

# summary(lav4_supplier_phishing, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# model 4c: incident preparedness moderation
m4_preparedness_phishing <- paste0(
  m3_phishing,
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

fit4_preparedness_phishing <- modsem_pi(
  model.syntax = m4_preparedness_phishing,
  data = riar_data_phishing,
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

lav4_preparedness_phishing <- extract_lavaan(fit4_preparedness_phishing)

# summary(lav4_preparedness_phishing, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# model 4d: board cybersecurity governance moderation
m4_board_phishing <- paste0(
  m3_phishing,
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

fit4_board_phishing <- modsem_pi(
  model.syntax = m4_board_phishing,
  data = riar_data_phishing,
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

lav4_board_phishing <- extract_lavaan(fit4_board_phishing)

# summary(lav4_board_phishing, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# model 4e: personal-device access moderation
m4_byod_phishing <- paste0(
  m3_phishing,
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

fit4_byod_phishing <- modsem_pi(
  model.syntax = m4_byod_phishing,
  data = riar_data_phishing,
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

lav4_byod_phishing <- extract_lavaan(fit4_byod_phishing)

# summary(lav4_byod_phishing, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# model 4f: cs management practices moderation
m4_management_phishing <- paste0(
  m3_phishing,
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

fit4_management_phishing <- modsem_pi(
  model.syntax = m4_management_phishing,
  data = riar_data_phishing,
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

lav4_management_phishing <- extract_lavaan(fit4_management_phishing)

# summary(lav4_management_phishing, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


save(
  fit3_phishing,
  lav4_cloud_phishing,
  lav4_supplier_phishing,
  lav4_preparedness_phishing,
  lav4_board_phishing,
  lav4_byod_phishing,
  lav4_management_phishing,

  file = here(
    "results",
    "phishing_models.RData"
  )
)

# check interaction results
interaction_results_phishing <- bind_rows(
  parameterEstimates(lav4_cloud_phishing),
  parameterEstimates(lav4_supplier_phishing),
  parameterEstimates(lav4_preparedness_phishing),
  parameterEstimates(lav4_board_phishing),
  parameterEstimates(lav4_byod_phishing),
  parameterEstimates(lav4_management_phishing)
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

interaction_results_phishing %>%
  select(
    label,
    est,
    se,
    pvalue,
    sig,
    formatted
  )

