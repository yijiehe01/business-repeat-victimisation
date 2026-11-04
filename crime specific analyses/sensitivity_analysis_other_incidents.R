# sensitivity analysis:other cyber incidents
variables_other_incident <- variables %>%
  mutate(
    dv_other_incident = case_when(
      incidentk == 1 ~ 1,
      incidentk == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(variables_other_incident$dv_other_incident,useNA = "always")

wide_dv_other_incident <- variables_other_incident %>%
  select(respondentid, wave, dv_other_incident) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = dv_other_incident,
    names_glue = "dv_other_incident_w{wave}"
  )

riar_data_other_incident <- wide_dv_other_incident %>%
  left_join(
    flag_data %>%
      select(respondentid, all_of(flag_vars)),
    by = "respondentid"
  ) %>%
  left_join(panel_weights, by = "respondentid") %>%
  left_join(between_data, by = "respondentid") %>%
  left_join(within_data, by = "respondentid")


# model 3
m3_other_incident <- '
  RI_dv =~ 1*dv_other_incident_w1 +
           1*dv_other_incident_w2 +
           1*dv_other_incident_w3 +
           1*dv_other_incident_w4 +
           1*dv_other_incident_w5

  W_dv1 =~ 1*dv_other_incident_w1
  W_dv2 =~ 1*dv_other_incident_w2
  W_dv3 =~ 1*dv_other_incident_w3
  W_dv4 =~ 1*dv_other_incident_w4
  W_dv5 =~ 1*dv_other_incident_w5

  dv_other_incident_w1 ~~ 0*dv_other_incident_w1
  dv_other_incident_w2 ~~ 0*dv_other_incident_w2
  dv_other_incident_w3 ~~ 0*dv_other_incident_w3
  dv_other_incident_w4 ~~ 0*dv_other_incident_w4
  dv_other_incident_w5 ~~ 0*dv_other_incident_w5

  RI_dv ~~ 0*W_dv1
  RI_dv ~~ 0*W_dv2
  RI_dv ~~ 0*W_dv3
  RI_dv ~~ 0*W_dv4
  RI_dv ~~ 0*W_dv5

  # between-business predictors of stable victimisation propensity
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

  # within-business lagged predictors
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

fit3_other_incident <- sem(
  model = m3_other_incident,
  data = riar_data_other_incident,
  estimator = "MLR",
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit3_other_incident,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(fit3_other_incident, "converged")
lavInspect(fit3_other_incident, "post.check")

# model 4a
m4_cloud_other_incident <- paste0(
  m3_other_incident,
  '
  W_dv2 ~
    cloud_between_mod*W_dv1:cloud_storage_between +
    cloud_within_mod*W_dv1:cloud_storage_within_w1

  W_dv3 ~
    cloud_between_mod*W_dv2:cloud_storage_between +
    cloud_within_mod*W_dv2:cloud_storage_within_w2

  W_dv4 ~
    cloud_between_mod*W_dv3:cloud_storage_between +
    cloud_within_mod*W_dv3:cloud_storage_within_w3

  W_dv5 ~
    cloud_between_mod*W_dv4:cloud_storage_between +
    cloud_within_mod*W_dv4:cloud_storage_within_w4
  '
)

fit4_cloud_other_incident <- modsem_pi(
  model.syntax = m4_cloud_other_incident,
  data = riar_data_other_incident,
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

lav4_cloud_other_incident <- extract_lavaan(
  fit4_cloud_other_incident
)

# summary(lav4_cloud_other_incident,fit.measures = TRUE, standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_cloud_other_incident, "converged")
lavInspect(lav4_cloud_other_incident, "post.check")

# model 4b
m4_supplier_other_incident <- paste0(
  m3_other_incident,
  '
  W_dv2 ~
    supplier_between_mod*W_dv1:supplierrisk_mgmt_between +
    supplier_within_mod*W_dv1:supplierrisk_mgmt_within_w1

  W_dv3 ~
    supplier_between_mod*W_dv2:supplierrisk_mgmt_between +
    supplier_within_mod*W_dv2:supplierrisk_mgmt_within_w2

  W_dv4 ~
    supplier_between_mod*W_dv3:supplierrisk_mgmt_between +
    supplier_within_mod*W_dv3:supplierrisk_mgmt_within_w3

  W_dv5 ~
    supplier_between_mod*W_dv4:supplierrisk_mgmt_between +
    supplier_within_mod*W_dv4:supplierrisk_mgmt_within_w4
  '
)

fit4_supplier_other_incident <- modsem_pi(
  model.syntax = m4_supplier_other_incident,
  data = riar_data_other_incident,
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

lav4_supplier_other_incident <- extract_lavaan(
  fit4_supplier_other_incident
)

# summary(lav4_supplier_other_incident,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_supplier_other_incident, "converged")
lavInspect(lav4_supplier_other_incident, "post.check")

# model 4c
m4_preparedness_other_incident <- paste0(
  m3_other_incident,
  '
  W_dv2 ~
    preparedness_between_mod*W_dv1:incident_preparedness_between +
    preparedness_within_mod*W_dv1:incident_preparedness_within_w1

  W_dv3 ~
    preparedness_between_mod*W_dv2:incident_preparedness_between +
    preparedness_within_mod*W_dv2:incident_preparedness_within_w2

  W_dv4 ~
    preparedness_between_mod*W_dv3:incident_preparedness_between +
    preparedness_within_mod*W_dv3:incident_preparedness_within_w3

  W_dv5 ~
    preparedness_between_mod*W_dv4:incident_preparedness_between +
    preparedness_within_mod*W_dv4:incident_preparedness_within_w4
  '
)

fit4_preparedness_other_incident <- modsem_pi(
  model.syntax = m4_preparedness_other_incident,
  data = riar_data_other_incident,
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

lav4_preparedness_other_incident <- extract_lavaan(
  fit4_preparedness_other_incident
)

# summary(lav4_preparedness_other_incident,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_preparedness_other_incident, "converged")
lavInspect(lav4_preparedness_other_incident, "post.check")

# model 4d
m4_board_other_incident <- paste0(
  m3_other_incident,
  '
  W_dv2 ~
    board_between_mod*W_dv1:board_cyber_governance_latent_between +
    board_within_mod*W_dv1:board_cyber_governance_latent_within_w1

  W_dv3 ~
    board_between_mod*W_dv2:board_cyber_governance_latent_between +
    board_within_mod*W_dv2:board_cyber_governance_latent_within_w2

  W_dv4 ~
    board_between_mod*W_dv3:board_cyber_governance_latent_between +
    board_within_mod*W_dv3:board_cyber_governance_latent_within_w3

  W_dv5 ~
    board_between_mod*W_dv4:board_cyber_governance_latent_between +
    board_within_mod*W_dv4:board_cyber_governance_latent_within_w4
  '
)

fit4_board_other_incident <- modsem_pi(
  model.syntax = m4_board_other_incident,
  data = riar_data_other_incident,
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

lav4_board_other_incident <- extract_lavaan(
  fit4_board_other_incident
)

# summary(lav4_board_other_incident,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_board_other_incident, "converged")
lavInspect(lav4_board_other_incident, "post.check")

# model 4e
m4_byod_other_incident <- paste0(
  m3_other_incident,
  '
  W_dv2 ~
    byod_between_mod*W_dv1:byod_access_between +
    byod_within_mod*W_dv1:byod_access_within_w1

  W_dv3 ~
    byod_between_mod*W_dv2:byod_access_between +
    byod_within_mod*W_dv2:byod_access_within_w2

  W_dv4 ~
    byod_between_mod*W_dv3:byod_access_between +
    byod_within_mod*W_dv3:byod_access_within_w3

  W_dv5 ~
    byod_between_mod*W_dv4:byod_access_between +
    byod_within_mod*W_dv4:byod_access_within_w4
  '
)

fit4_byod_other_incident <- modsem_pi(
  model.syntax = m4_byod_other_incident,
  data = riar_data_other_incident,
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

lav4_byod_other_incident <- extract_lavaan(
  fit4_byod_other_incident
)

# summary(lav4_byod_other_incident,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_byod_other_incident, "converged")
lavInspect(lav4_byod_other_incident, "post.check")


# model 4f
m4_management_other_incident <- paste0(
  m3_other_incident,
  '
  W_dv2 ~
    cyber_management_between_mod*W_dv1:cyber_management_latent_between +
    cyber_management_within_mod*W_dv1:cyber_management_latent_within_w1

  W_dv3 ~
    cyber_management_between_mod*W_dv2:cyber_management_latent_between +
    cyber_management_within_mod*W_dv2:cyber_management_latent_within_w2

  W_dv4 ~
    cyber_management_between_mod*W_dv3:cyber_management_latent_between +
    cyber_management_within_mod*W_dv3:cyber_management_latent_within_w3

  W_dv5 ~
    cyber_management_between_mod*W_dv4:cyber_management_latent_between +
    cyber_management_within_mod*W_dv4:cyber_management_latent_within_w4
  '
)

fit4_management_other_incident <- modsem_pi(
  model.syntax = m4_management_other_incident,
  data = riar_data_other_incident,
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

lav4_management_other_incident <- extract_lavaan(
  fit4_management_other_incident
)

# summary(lav4_management_other_incident,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_management_other_incident, "converged")
lavInspect(lav4_management_other_incident, "post.check")

save(
  fit3_other_incident,
  lav4_cloud_other_incident,
  lav4_supplier_other_incident,
  lav4_preparedness_other_incident,
  lav4_board_other_incident,
  lav4_byod_other_incident,
  lav4_management_other_incident,

  file = here(
    "results",
    "other_incident_models.RData"
  )
)


interaction_results_other <- bind_rows(
  parameterEstimates(lav4_cloud_other_incident),
  parameterEstimates(lav4_supplier_other_incident),
  parameterEstimates(lav4_preparedness_other_incident),
  parameterEstimates(lav4_board_other_incident),
  parameterEstimates(lav4_byod_other_incident),
  parameterEstimates(lav4_management_other_incident)
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
  select(label, est, se, pvalue)

# multiple testing correction
interaction_tests_other <- interaction_results_other %>%
  mutate(
    p_BH = p.adjust(pvalue, method = "BH"),
    sig_BH = case_when(
      p_BH < .001 ~ "***",
      p_BH < .01  ~ "**",
      p_BH < .05  ~ "*",
      p_BH < .10  ~ "+",
      TRUE        ~ ""
    ),
    formatted_BH = sprintf(
      "%.3f%s (%.3f)",
      est,
      sig_BH,
      se
    )
  )


interaction_tests_other %>%
  select(
    label,
    est,
    se,
    pvalue,
    p_BH,
    sig_BH,
    formatted_BH
  )
