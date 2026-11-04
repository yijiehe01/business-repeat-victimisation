# sensitivity analysis:denial-of-service attacks only
variables_dos <- variables %>%
  mutate(
    dv_dos = case_when(
      incidente == 1 ~ 1,
      incidente == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(variables_dos$dv_dos, useNA = "always")

wide_dv_dos <- variables_dos %>%
  select(respondentid, wave, dv_dos) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = dv_dos,
    names_glue = "dv_dos_w{wave}"
  )

riar_data_dos <- wide_dv_dos %>%
  left_join(
    flag_data %>%
      select(respondentid, all_of(flag_vars)),
    by = "respondentid"
  ) %>%
  left_join(panel_weights, by = "respondentid") %>%
  left_join(between_data, by = "respondentid") %>%
  left_join(within_data, by = "respondentid")

# model 3
m3_dos <- '
  RI_dv =~ 1*dv_dos_w1 +
           1*dv_dos_w2 +
           1*dv_dos_w3 +
           1*dv_dos_w4 +
           1*dv_dos_w5

  W_dv1 =~ 1*dv_dos_w1
  W_dv2 =~ 1*dv_dos_w2
  W_dv3 =~ 1*dv_dos_w3
  W_dv4 =~ 1*dv_dos_w4
  W_dv5 =~ 1*dv_dos_w5

  dv_dos_w1 ~~ 0*dv_dos_w1
  dv_dos_w2 ~~ 0*dv_dos_w2
  dv_dos_w3 ~~ 0*dv_dos_w3
  dv_dos_w4 ~~ 0*dv_dos_w4
  dv_dos_w5 ~~ 0*dv_dos_w5

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

fit3_dos <- sem(
  model = m3_dos,
  data = riar_data_dos,
  estimator = "MLR",
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit3_dos,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(fit3_dos, "converged")
lavInspect(fit3_dos, "post.check")

# model 4a
m4_cloud_dos <- paste0(
  m3_dos,
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

fit4_cloud_dos <- modsem_pi(
  model.syntax = m4_cloud_dos,
  data = riar_data_dos,
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

lav4_cloud_dos <- extract_lavaan(fit4_cloud_dos)

# summary(lav4_cloud_dos,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_cloud_dos, "converged")
lavInspect(lav4_cloud_dos, "post.check")

# model 4b
m4_supplier_dos <- paste0(
  m3_dos,
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

fit4_supplier_dos <- modsem_pi(
  model.syntax = m4_supplier_dos,
  data = riar_data_dos,
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

lav4_supplier_dos <- extract_lavaan(fit4_supplier_dos)

# summary(lav4_supplier_dos,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_supplier_dos, "converged")
lavInspect(lav4_supplier_dos, "post.check")

# model 4c
m4_preparedness_dos <- paste0(
  m3_dos,
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

fit4_preparedness_dos <- modsem_pi(
  model.syntax = m4_preparedness_dos,
  data = riar_data_dos,
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

lav4_preparedness_dos <- extract_lavaan(
  fit4_preparedness_dos
)

# summary(lav4_preparedness_dos,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_preparedness_dos, "converged")
lavInspect(lav4_preparedness_dos, "post.check")

# model 4d
m4_board_dos <- paste0(
  m3_dos,
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

fit4_board_dos <- modsem_pi(
  model.syntax = m4_board_dos,
  data = riar_data_dos,
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

lav4_board_dos <- extract_lavaan(fit4_board_dos)

# summary(lav4_board_dos,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_board_dos, "converged")
lavInspect(lav4_board_dos, "post.check")

# model 4e
m4_byod_dos <- paste0(
  m3_dos,
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

fit4_byod_dos <- modsem_pi(
  model.syntax = m4_byod_dos,
  data = riar_data_dos,
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

lav4_byod_dos <- extract_lavaan(fit4_byod_dos)

# summary(lav4_byod_dos,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_byod_dos, "converged")
lavInspect(lav4_byod_dos, "post.check")

# model 4f
m4_management_dos <- paste0(
  m3_dos,
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

fit4_management_dos <- modsem_pi(
  model.syntax = m4_management_dos,
  data = riar_data_dos,
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

lav4_management_dos <- extract_lavaan(
  fit4_management_dos
)

# summary(lav4_management_dos,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_management_dos, "converged")
lavInspect(lav4_management_dos, "post.check")


save(
  fit3_dos,
  lav4_cloud_dos,
  lav4_supplier_dos,
  lav4_preparedness_dos,
  lav4_board_dos,
  lav4_byod_dos,
  lav4_management_dos,

  file = here(
    "results",
    "dos_models.RData"
  )
)

# multiple testing correction
interaction_tests_dos <- bind_rows(
  parameterEstimates(lav4_cloud_dos),
  parameterEstimates(lav4_supplier_dos),
  parameterEstimates(lav4_preparedness_dos),
  parameterEstimates(lav4_board_dos),
  parameterEstimates(lav4_byod_dos),
  parameterEstimates(lav4_management_dos)
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

interaction_tests_dos %>%
  select(
    label,
    est,
    se,
    pvalue,
    p_BH,
    sig_BH,
    formatted_BH
  )
