# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("here")
# install.packages("haven")
# install.packages("lavaan")
# install.packages("survey")
# install.packages("modsem")
# install.packages("tibble")
library(dplyr)
library(tidyr)
library(here)
library(haven)
library(lavaan)
library(survey)
library(modsem)
library(tibble)

csls1_5 <- read_sav(here("data", "csls_waves_1-5.sav"))
csls1   <- read_sav(here("data", "csls1.sav"))

# keep private-sector sample
private_sector <- csls1_5 %>%
  filter(qtypdum == 1) %>%
  drop_na(respondentid)

participation <- private_sector %>%
  distinct(respondentid, wave) %>%
  mutate(observed = 1) %>%
  pivot_wider(
    id_cols = respondentid,
    names_from = wave,
    values_from = observed,
    names_prefix = "wave",
    values_fill = 0
  )

# identify each business's entry wave
participation <- participation %>%
  mutate(
    entry_wave = case_when(
      wave1 == 1 ~ 1,
      wave2 == 1 ~ 2,
      wave3 == 1 ~ 3,
      wave4 == 1 ~ 4,
      wave5 == 1 ~ 5,
      TRUE ~ NA_real_
    )
  )

# remove businesses entering at wave 5
participation <- participation %>%
  filter(entry_wave < 5)

# create possible follow-up transitions
transition_data <- participation %>%
  crossing(to_wave = 2:5)

# keep follow-up waves occurring after entry
transition_data <- transition_data %>%
  filter(to_wave > entry_wave)

transition_data <- transition_data %>%
  mutate(from_wave = to_wave - 1)

# identify response at the next follow-up wave
transition_data <- transition_data %>%
  mutate(
    next_wave_response = case_when(
      to_wave == 2 ~ wave2,
      to_wave == 3 ~ wave3,
      to_wave == 4 ~ wave4,
      to_wave == 5 ~ wave5,
      TRUE ~ NA_real_
    )
  )

# keep transitions where the business responded at the preceding wave
transition_data <- transition_data %>%
  mutate(
    observed_previous = case_when(
      from_wave == 1 ~ wave1,
      from_wave == 2 ~ wave2,
      from_wave == 3 ~ wave3,
      from_wave == 4 ~ wave4
    )
  ) %>%
  filter(observed_previous == 1)

predictor_data <- private_sector %>%
  mutate(from_wave = wave)

# recode business size and sector
predictor_data <- predictor_data %>%
  mutate(
    sector_group = case_when(
      sector %in% c(1, 2, 3, 5) ~ "production_manufacturing_construction_transport",
      sector %in% c(4, 6, 14) ~ "consumer_facing_services",
      sector %in% c(7, 8, 10) ~ "information_financial_professional_services",
      sector %in% c(9, 11, 15) ~ "property_administrative_membership_services",
      sector %in% c(12, 13) ~ "education_health_social_services",
      TRUE ~ NA_character_
    ),

    size_group = case_when(
      qsize %in% c(1, 2) ~ "under_250",
      qsize == 3 ~ "250_499",
      qsize == 4 ~ "500_999",
      qsize == 5 ~ "1000plus",
      TRUE ~ NA_character_
    )
  )

predictor_data <- predictor_data %>%
  mutate(
    sector_group = factor(
      sector_group,
      levels = c( "information_financial_professional_services",
                  "production_manufacturing_construction_transport",
                  "consumer_facing_services",
                  "property_administrative_membership_services",
                  "education_health_social_services")
    ),

    size_group = factor(
      size_group,levels = c("under_250","250_499","500_999","1000plus")
    )
  )

# recode businesses' cyber victimisation
predictor_data <- predictor_data %>%
  mutate(
    victimisation = case_when(
      incident12 == 1 ~ 1,
      incident12 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# recode whether business adhere to the cyber essentials standard
predictor_data <- predictor_data %>%
  mutate(
    cyber_essentials = case_when(
      complyx2 == 1 ~ 1,
      complyx2 == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# count don't know responses across all cyber risk identification items
predictor_data <- predictor_data %>%
  mutate(
    ident_dk_count = rowSums(
      cbind(identa == 3,identb == 3,identc == 3,identd == 3), na.rm = TRUE
    )
  )

table(predictor_data$ident_dk_count, useNA = "always")

predictor_data <- predictor_data %>%
  mutate(
    ident_dk = case_when(
      ident_dk_count == 0 ~ "0",
      ident_dk_count == 1 ~ "1",
      ident_dk_count >= 2 ~ "2+",
      TRUE ~ NA_character_
    ),
    ident_dk = factor(
      ident_dk,levels = c("0", "1", "2+")
    )
  )

# recode whether businesses assess cyber risk presented by their suppliers
predictor_data <- predictor_data %>%
  mutate(
    supplier_risk = case_when(
      supplyrisk == 1 ~ "yes",
      supplyrisk == 2 ~ "no",
      supplyrisk == 3 ~ "dont_know",
      TRUE ~ NA_character_
    ),

    supplier_risk = factor(
      supplier_risk,
      levels = c("no", "yes", "dont_know")
    ),

    # recode whether businesses have cyber insurance
    cyber_insurance = case_when(
      insurex1 == 1 ~ "yes",
      insurex1 == 2 ~ "no",
      insurex1 == 3 ~ "dont_know",
      TRUE ~ NA_character_
    ),

    cyber_insurance = factor(
      cyber_insurance,levels = c("no", "yes", "dont_know")
    )
  )

table(predictor_data$supplier_risk, useNA = "always")
table(predictor_data$cyber_insurance, useNA = "always")

# recode whether board members receive cs training
w1_btrain <- csls1 %>%
  transmute(
    respondentid = id,
    wave = 1,
    btrain
  )

predictor_data <- predictor_data %>%
  left_join(w1_btrain, by = c("respondentid","wave"))

predictor_data <- predictor_data %>%
  mutate(
    board_train_dk = case_when(
      wave %in% 2:5 & boardtrainfreq == 6 ~ 1,
      wave %in% 2:5 & boardtrainfreq %in% c(1,2,3,4,5,7) ~ 0,

      wave == 1 & btrain == -97 ~ 1,
      wave == 1 & btrain %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    ),

    # recode whether board members receive updates on organisational cs
    board_updates_dk = case_when(
      bdiscss == 9 ~ 1,
      bdiscss %in% 1:8 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(predictor_data$board_updates_dk, useNA = "always")
table(predictor_data$board_train_dk, useNA = "always")

# predictors
transition_data <- transition_data %>%
  left_join(
    predictor_data %>%
      select(
        respondentid,
        from_wave,
        size_group,
        sector_group,
        victimisation,
        cyber_essentials,
        ident_dk,
        supplier_risk,
        cyber_insurance,
        board_updates_dk,
        board_train_dk
      ),
    by = c("respondentid", "from_wave")
  )

# keep the cross-sectional weight at each business's entry wave
entry_weights <- private_sector %>%
  group_by(respondentid) %>%
  slice_min(wave, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(respondentid,entry_weight = wght)

transition_data <- transition_data %>%
  left_join(entry_weights, by = "respondentid")

transition_model_data <- transition_data %>%
  drop_na(
    next_wave_response,
    entry_weight,
    entry_wave,
    from_wave,
    size_group,
    sector_group,
    victimisation,
    cyber_essentials,
    ident_dk,
    supplier_risk,
    cyber_insurance,
    board_updates_dk,
    board_train_dk
  )

# numerator model for stabilised weights
numerator_model <- glm(
  next_wave_response ~
    factor(entry_wave) +
    factor(from_wave) +
    size_group +
    sector_group,
  family = binomial(),
  data = transition_model_data
)

# denominator model for stabilised weights
denominator_model <- glm(
  next_wave_response ~
    factor(entry_wave) +
    factor(from_wave) +
    size_group +
    sector_group +
    victimisation +
    cyber_essentials +
    ident_dk +
    supplier_risk +
    cyber_insurance +
    board_updates_dk +
    board_train_dk,
  family = binomial(),
  data = transition_model_data
)

# summary(denominator_model)

transition_model_data <- transition_model_data %>%
  mutate(
    p_denominator = predict(
      denominator_model,
      type = "response"
    ),

    p_numerator = predict(
      numerator_model,
      type = "response"
    ),

    transition_factor = p_numerator / p_denominator
  )

transition_weights <- transition_model_data %>%
  filter(next_wave_response == 1) %>%
  arrange(respondentid, to_wave) %>%
  group_by(respondentid) %>%
  mutate(
    retention_weight = cumprod(transition_factor), # accumulate weights across observed transitions
    longitudinal_weight = entry_weight * retention_weight
  ) %>%
  ungroup()

# one final longitudinal weight for each business
panel_weights <- transition_weights %>%
  group_by(respondentid) %>%
  slice_max(order_by = to_wave, n=1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    respondentid,
    weight_wave = to_wave,
    longitudinal_weight = longitudinal_weight
  )

summary(panel_weights$longitudinal_weight)
