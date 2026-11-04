# sensitivity analysis: rare incident composite combing ransomware, hacking, unauthorised conference/message listening

# create composite outcome using the least three prevalent incidents
variables_afj <- variables %>%
  mutate(
    dv_afj = case_when(
      incidenta == 1 | incidentf == 1 | incidentj == 1 ~ 1,
      incidenta == 2 & incidentf == 2 & incidentj == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(variables_afj$dv_afj,useNA = "always")

wide_dv_afj <- variables_afj %>%
  select(
    respondentid,
    wave,
    dv_afj
  ) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = dv_afj,
    names_glue = "dv_afj_w{wave}"
  )

riar_data_afj <- wide_dv_afj %>%
  left_join(
    flag_data %>%
      select(respondentid,all_of(flag_vars)),by = "respondentid") %>%
  left_join(panel_weights,by = "respondentid") %>%
  left_join(between_data,by = "respondentid") %>%
  left_join(within_data,by = "respondentid")

# model 3
m3_afj <- '
  RI_dv_afj =~ 1*dv_afj_w1 + 1*dv_afj_w2 + 1*dv_afj_w3 + 1*dv_afj_w4 + 1*dv_afj_w5

  W_dv_afj1 =~ 1*dv_afj_w1
  W_dv_afj2 =~ 1*dv_afj_w2
  W_dv_afj3 =~ 1*dv_afj_w3
  W_dv_afj4 =~ 1*dv_afj_w4
  W_dv_afj5 =~ 1*dv_afj_w5

  dv_afj_w1 ~~ 0*dv_afj_w1
  dv_afj_w2 ~~ 0*dv_afj_w2
  dv_afj_w3 ~~ 0*dv_afj_w3
  dv_afj_w4 ~~ 0*dv_afj_w4
  dv_afj_w5 ~~ 0*dv_afj_w5

  RI_dv_afj ~~ 0*W_dv_afj1
  RI_dv_afj ~~ 0*W_dv_afj2
  RI_dv_afj ~~ 0*W_dv_afj3
  RI_dv_afj ~~ 0*W_dv_afj4
  RI_dv_afj ~~ 0*W_dv_afj5

  RI_dv_afj ~ size_250_499 +
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

  W_dv_afj2 ~ boost_afj*W_dv_afj1 +
    cloud_afj*cloud_storage_within_w1 +
    supplier_afj*supplierrisk_mgmt_within_w1 +
    preparedness_afj*incident_preparedness_within_w1 +
    board_afj*board_cyber_governance_latent_within_w1 +
    byod_afj*byod_access_within_w1 +
    cyber_management_afj*cyber_management_latent_within_w1

  W_dv_afj3 ~ boost_afj*W_dv_afj2 +
    cloud_afj*cloud_storage_within_w2 +
    supplier_afj*supplierrisk_mgmt_within_w2 +
    preparedness_afj*incident_preparedness_within_w2 +
    board_afj*board_cyber_governance_latent_within_w2 +
    byod_afj*byod_access_within_w2 +
    cyber_management_afj*cyber_management_latent_within_w2

  W_dv_afj4 ~ boost_afj*W_dv_afj3 +
    cloud_afj*cloud_storage_within_w3 +
    supplier_afj*supplierrisk_mgmt_within_w3 +
    preparedness_afj*incident_preparedness_within_w3 +
    board_afj*board_cyber_governance_latent_within_w3 +
    byod_afj*byod_access_within_w3 +
    cyber_management_afj*cyber_management_latent_within_w3

  W_dv_afj5 ~ boost_afj*W_dv_afj4 +
    cloud_afj*cloud_storage_within_w4 +
    supplier_afj*supplierrisk_mgmt_within_w4 +
    preparedness_afj*incident_preparedness_within_w4 +
    board_afj*board_cyber_governance_latent_within_w4 +
    byod_afj*byod_access_within_w4 +
    cyber_management_afj*cyber_management_latent_within_w4
'

fit3_afj <- sem(
  model = m3_afj,
  data = riar_data_afj,
  estimator = "MLR",
  missing = "fiml.x",
  sampling.weights = "longitudinal_weight",
  sampling.weights.normalization = "none",
  meanstructure = TRUE,
  fixed.x = TRUE,
  auto.cov.lv.x = FALSE
)

summary(fit3_afj,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(fit3_afj, "converged")
lavInspect(fit3_afj, "post.check")

# model 4a
m4_cloud_afj <- paste0(
  m3_afj,
  '
  W_dv_afj2 ~
    cloud_between_mod_afj* W_dv_afj1:cloud_storage_between +
    cloud_within_mod_afj* W_dv_afj1:cloud_storage_within_w1

  W_dv_afj3 ~
    cloud_between_mod_afj* W_dv_afj2:cloud_storage_between +
    cloud_within_mod_afj*W_dv_afj2:cloud_storage_within_w2

  W_dv_afj4 ~
    cloud_between_mod_afj*W_dv_afj3:cloud_storage_between +
    cloud_within_mod_afj*W_dv_afj3:cloud_storage_within_w3

  W_dv_afj5 ~
    cloud_between_mod_afj*W_dv_afj4:cloud_storage_between +
    cloud_within_mod_afj*W_dv_afj4:cloud_storage_within_w4
  '
)

fit4_cloud_afj <- modsem_pi(
  model.syntax = m4_cloud_afj,
  data = riar_data_afj,
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
  em.h1.iter.max = 10000,
  suppress.warnings.lavaan = FALSE
)

lav4_cloud_afj <- extract_lavaan(fit4_cloud_afj)

# summary(lav4_cloud_afj,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_cloud_afj, "converged")
lavInspect(lav4_cloud_afj, "post.check")

# model 4b
m4_supplier_afj <- paste0(
  m3_afj,
  '
  W_dv_afj2 ~
    supplier_between_mod_afj*W_dv_afj1:supplierrisk_mgmt_between +
    supplier_within_mod_afj*W_dv_afj1:supplierrisk_mgmt_within_w1

  W_dv_afj3 ~
    supplier_between_mod_afj*W_dv_afj2:supplierrisk_mgmt_between +
    supplier_within_mod_afj*W_dv_afj2:supplierrisk_mgmt_within_w2

  W_dv_afj4 ~
    supplier_between_mod_afj*W_dv_afj3:supplierrisk_mgmt_between +
    supplier_within_mod_afj*W_dv_afj3:supplierrisk_mgmt_within_w3

  W_dv_afj5 ~
    supplier_between_mod_afj*W_dv_afj4:supplierrisk_mgmt_between +
    supplier_within_mod_afj*W_dv_afj4:supplierrisk_mgmt_within_w4
  '
)

fit4_supplier_afj <- modsem_pi(
  model.syntax = m4_supplier_afj,
  data = riar_data_afj,
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
  em.h1.iter.max = 10000,
  suppress.warnings.lavaan = FALSE
)

lav4_supplier_afj <- extract_lavaan(fit4_supplier_afj)

# summary(lav4_supplier_afj,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_supplier_afj, "converged")
lavInspect(lav4_supplier_afj, "post.check")

# model 4c
m4_preparedness_afj <- paste0(
  m3_afj,
  '
  W_dv_afj2 ~
    preparedness_between_mod_afj*W_dv_afj1:incident_preparedness_between +
    preparedness_within_mod_afj*W_dv_afj1:incident_preparedness_within_w1

  W_dv_afj3 ~
    preparedness_between_mod_afj*W_dv_afj2:incident_preparedness_between +
    preparedness_within_mod_afj*W_dv_afj2:incident_preparedness_within_w2

  W_dv_afj4 ~
    preparedness_between_mod_afj*W_dv_afj3:incident_preparedness_between +
    preparedness_within_mod_afj*W_dv_afj3:incident_preparedness_within_w3

  W_dv_afj5 ~
    preparedness_between_mod_afj*W_dv_afj4:incident_preparedness_between +
    preparedness_within_mod_afj*W_dv_afj4:incident_preparedness_within_w4
  '
)

fit4_preparedness_afj <- modsem_pi(
  model.syntax = m4_preparedness_afj,
  data = riar_data_afj,
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
  em.h1.iter.max = 10000,
  suppress.warnings.lavaan = FALSE
)

lav4_preparedness_afj <- extract_lavaan(fit4_preparedness_afj)

# summary(lav4_preparedness_afj,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_preparedness_afj, "converged")
lavInspect(lav4_preparedness_afj, "post.check")

# model 4d
m4_board_afj <- paste0(
  m3_afj,
  '
  W_dv_afj2 ~
    board_between_mod_afj*W_dv_afj1:board_cyber_governance_latent_between +
    board_within_mod_afj*W_dv_afj1:board_cyber_governance_latent_within_w1

  W_dv_afj3 ~
    board_between_mod_afj*W_dv_afj2:board_cyber_governance_latent_between +
    board_within_mod_afj*W_dv_afj2:board_cyber_governance_latent_within_w2

  W_dv_afj4 ~
    board_between_mod_afj*W_dv_afj3:board_cyber_governance_latent_between +
    board_within_mod_afj*W_dv_afj3:board_cyber_governance_latent_within_w3

  W_dv_afj5 ~
    board_between_mod_afj*W_dv_afj4:board_cyber_governance_latent_between +
    board_within_mod_afj*W_dv_afj4:board_cyber_governance_latent_within_w4
  '
)

fit4_board_afj <- modsem_pi(
  model.syntax = m4_board_afj,
  data = riar_data_afj,
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
  em.h1.iter.max = 10000,
  suppress.warnings.lavaan = FALSE
)

lav4_board_afj <- extract_lavaan(fit4_board_afj)

# summary(lav4_board_afj,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_board_afj, "converged")
lavInspect(lav4_board_afj, "post.check")

# model 4e
m4_byod_afj <- paste0(
  m3_afj,
  '
  W_dv_afj2 ~
    byod_between_mod_afj*W_dv_afj1:byod_access_between +
    byod_within_mod_afj*W_dv_afj1:byod_access_within_w1

  W_dv_afj3 ~
    byod_between_mod_afj*W_dv_afj2:byod_access_between +
    byod_within_mod_afj*W_dv_afj2:byod_access_within_w2

  W_dv_afj4 ~
    byod_between_mod_afj*W_dv_afj3:byod_access_between +
    byod_within_mod_afj*W_dv_afj3:byod_access_within_w3

  W_dv_afj5 ~
    byod_between_mod_afj*W_dv_afj4:byod_access_between +
    byod_within_mod_afj*W_dv_afj4:byod_access_within_w4
  '
)

fit4_byod_afj <- modsem_pi(
  model.syntax = m4_byod_afj,
  data = riar_data_afj,
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
  em.h1.iter.max = 10000,
  suppress.warnings.lavaan = FALSE
)

lav4_byod_afj <- extract_lavaan(fit4_byod_afj)

# summary(lav4_byod_afj,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_byod_afj, "converged")
lavInspect(lav4_byod_afj, "post.check")

# model 4f
m4_management_afj <- paste0(
  m3_afj,
  '
  W_dv_afj2 ~
    cyber_management_between_mod_afj*W_dv_afj1:cyber_management_latent_between +
    cyber_management_within_mod_afj*W_dv_afj1:cyber_management_latent_within_w1

  W_dv_afj3 ~
    cyber_management_between_mod_afj*W_dv_afj2:cyber_management_latent_between +
    cyber_management_within_mod_afj*W_dv_afj2:cyber_management_latent_within_w2

  W_dv_afj4 ~
    cyber_management_between_mod_afj*W_dv_afj3:cyber_management_latent_between +
    cyber_management_within_mod_afj*W_dv_afj3:cyber_management_latent_within_w3

  W_dv_afj5 ~
    cyber_management_between_mod_afj*W_dv_afj4:cyber_management_latent_between +
    cyber_management_within_mod_afj*W_dv_afj4:cyber_management_latent_within_w4
  '
)

fit4_management_afj <- modsem_pi(
  model.syntax = m4_management_afj,
  data = riar_data_afj,
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
  em.h1.iter.max = 10000,
  suppress.warnings.lavaan = FALSE
)

lav4_management_afj <- extract_lavaan(fit4_management_afj)

# summary(lav4_management_afj,fit.measures = TRUE,standardized = TRUE,rsquare = TRUE)

lavInspect(lav4_management_afj, "converged")
lavInspect(lav4_management_afj, "post.check")

save(
  fit3_afj,
  lav4_cloud_afj,
  lav4_supplier_afj,
  lav4_preparedness_afj,
  lav4_board_afj,
  lav4_byod_afj,
  lav4_management_afj,

  file = here(
    "results",
    "rare_incident_composite_models.RData"
  )
)

# multiple testing correction: rare-incident composite (AFJ)
interaction_tests_afj <- bind_rows(
  parameterEstimates(lav4_cloud_afj),
  parameterEstimates(lav4_supplier_afj),
  parameterEstimates(lav4_preparedness_afj),
  parameterEstimates(lav4_board_afj),
  parameterEstimates(lav4_byod_afj),
  parameterEstimates(lav4_management_afj)
) %>%
  filter(
    label %in% c(
      "cloud_between_mod_afj", "cloud_within_mod_afj",
      "supplier_between_mod_afj", "supplier_within_mod_afj",
      "preparedness_between_mod_afj", "preparedness_within_mod_afj",
      "board_between_mod_afj", "board_within_mod_afj",
      "byod_between_mod_afj", "byod_within_mod_afj",
      "cyber_management_between_mod_afj",
      "cyber_management_within_mod_afj"
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

interaction_tests_afj %>%
  select(
    label,
    est,
    se,
    pvalue,
    p_BH,
    sig_BH,
    formatted_BH
  )
