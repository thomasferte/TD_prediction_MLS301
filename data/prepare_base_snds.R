library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(dagitty)
library(stringr)

set.seed(1)

# Outcomes ----------------------------------------------------------------

## comorbidités
# Age
# Sexe
# HTA : I10-I15
# FA : I48
# CI - Cardiopathie ischémique : I20-I25
# DBT - Diabète : E10-E14
# AIT - Antécédent d’accident ischémique transitoire : G45
# IRC - Insuffisance rénale chronique : N18

## DAG

g1 <- dagitty::dagitty(
'dag {
"AVC M0-M12" [pos="0.144,-0.631"]
"Age & Sexe" [pos="-2.104,-0.413"]
"Décès M0-M12" [pos="0.248,-0.509"]
"Délai de PEC AVC" [pos="-1.080,-1.013"]
"IDM M0-M12" [pos="0.153,-0.412"]
"Localisation AVC" [pos="-0.451,-1.162"]
"MACE M0-M12" [pos="0.814,-0.490"]
"NIHSS M12" [pos="0.970,-0.819"]
"NIHSS Sortie Hospit" [pos="-0.107,-0.823"]
"NIHSS initial" [pos="-0.733,-0.846"]
"Taille AVC" [pos="-0.683,-1.088"]
AIT [pos="-1.342,-0.751"]
CI [pos="-1.703,-0.627"]
DBT [pos="-2.079,-0.657"]
FA [pos="-1.427,-0.568"]
HTA [pos="-1.937,-0.546"]
IRC [pos="-1.902,-0.742"]
"AVC M0-M12" -> "Décès M0-M12"
"AVC M0-M12" -> "MACE M0-M12"
"AVC M0-M12" -> "NIHSS M12"
"Age & Sexe" -> "AVC M0-M12"
"Age & Sexe" -> "Décès M0-M12"
"Age & Sexe" -> "IDM M0-M12"
"Age & Sexe" -> "NIHSS M12"
"Age & Sexe" -> "NIHSS initial"
"Age & Sexe" -> AIT
"Age & Sexe" -> CI
"Age & Sexe" -> DBT
"Age & Sexe" -> FA
"Age & Sexe" -> HTA
"Age & Sexe" -> IRC
"Décès M0-M12" -> "MACE M0-M12"
"Délai de PEC AVC" -> "NIHSS M12"
"Délai de PEC AVC" -> "NIHSS Sortie Hospit"
"Délai de PEC AVC" -> "NIHSS initial"
"Délai de PEC AVC" -> "Taille AVC"
"IDM M0-M12" -> "Décès M0-M12"
"IDM M0-M12" -> "MACE M0-M12"
"Localisation AVC" -> "NIHSS M12"
"Localisation AVC" -> "NIHSS Sortie Hospit"
"Localisation AVC" -> "NIHSS initial"
"NIHSS Sortie Hospit" -> "NIHSS M12"
"NIHSS initial" -> "NIHSS Sortie Hospit"
"Taille AVC" -> "NIHSS M12"
"Taille AVC" -> "NIHSS Sortie Hospit"
"Taille AVC" -> "NIHSS initial"
AIT -> "AVC M0-M12"
AIT -> "NIHSS M12"
AIT -> "NIHSS Sortie Hospit"
AIT -> "NIHSS initial"
CI -> "Décès M0-M12"
CI -> "IDM M0-M12"
CI -> FA
DBT -> "AVC M0-M12"
DBT -> "Décès M0-M12"
DBT -> "IDM M0-M12"
DBT -> "NIHSS M12"
DBT -> "NIHSS Sortie Hospit"
DBT -> "NIHSS initial"
DBT -> "Taille AVC"
DBT -> AIT
DBT -> CI
DBT -> IRC
FA -> "AVC M0-M12"
FA -> "Décès M0-M12"
FA -> "NIHSS M12"
FA -> "NIHSS Sortie Hospit"
FA -> "NIHSS initial"
FA -> "Taille AVC"
FA -> AIT
HTA -> "AVC M0-M12"
HTA -> "Décès M0-M12"
HTA -> "IDM M0-M12"
HTA -> "NIHSS M12"
HTA -> "NIHSS Sortie Hospit"
HTA -> "NIHSS initial"
HTA -> "Taille AVC"
HTA -> AIT
HTA -> CI
HTA -> IRC
IRC -> "AVC M0-M12"
IRC -> "Décès M0-M12"
IRC -> "IDM M0-M12"
IRC -> "NIHSS initial"
IRC -> CI
}
')
plot(graphLayout(g1))


##### Prédiction
## Question TD
# Prédiction d'une normalisation du score NIHSS à 0/42 (asymptomatique) à 1 an après AVC

## Question exam
# Prédiction d'un nouvel événement cardiovasculaire majeur ou décès à 1 an

##### Explication
## Question TD
# Etude de la mortalité après AVC : âge, sexe, comorbidités
# Etude de la récupération du NIHSS à 12 mois après un AVC

## Question exam
# Facteurs causant un plus grand risque de récidive d'AVC

# Connection --------------------------------------------------------------

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "synthetic_snds",
  host = "localhost",
  port = 5432,
  user = "tf1",
  password = "tf1"
)

# list tables
dbGetQuery(
  con,
  "SELECT table_name
   FROM information_schema.tables;"
)


tab_mso_ass_dgn <- tbl(
  con,
  in_schema("synthetic_snds", "tab_mso_ass_dgn")
)

tab_patient <- tbl(
  con,
  in_schema("synthetic_snds", "tab_patient")
)

tab_hospitalisation <- tbl(
  con,
  in_schema("synthetic_snds", "tab_hospitalisation")
)


# extraction des données --------------------------------------------------

# date max de suivi
max_date <- tab_hospitalisation |> 
  select(hosp_end_date) |> 
  filter(hosp_end_date == max(hosp_end_date)) |> 
  distinct() |> 
  pull()
# date pour avoir au moins 365 jours de suivi
max_debut_follow_up = max_date - 365

# selection des patients
included_patients <- tab_hospitalisation |> 
  filter(grepl(pattern = "^I63",
               x = hosp_main_dgn),
         hosp_start_date <= max_debut_follow_up) |> 
  group_by(pat_id) |> 
  slice_min(hosp_start_date) |> 
  select(pat_id, eta_num, rsa_num, stroke_date = hosp_start_date)

date_of_birth_and_death <- included_patients |> 
  left_join(
    tab_patient |> 
      select(pat_id,
             pat_birth_date,
             pat_sex_cod,
             pat_death_date),
    by = "pat_id"
  ) |> 
  mutate(Age_inclusion = (stroke_date - pat_birth_date)/365.25,
         delai_deces = pat_death_date - stroke_date,
         deces_M12 = if_else(delai_deces > 365 | is.na(delai_deces), 0, 1)) |> 
  select(pat_id, stroke_date, pat_sex_cod, Age_inclusion, deces_M12)

tbl_date_of_birth_and_death <- date_of_birth_and_death |> collect() |> ungroup()

# récupération des outcomes
outcomes <- date_of_birth_and_death |> 
  select(pat_id, stroke_date) |> 
  left_join(tab_hospitalisation |> 
              select(pat_id, hosp_start_date, hosp_main_dgn),
            by = "pat_id") |> 
  mutate(diff_date = hosp_start_date - stroke_date) |> 
  filter(diff_date <= 365 & diff_date > 0) |> 
  mutate(AVC = str_detect(hosp_main_dgn, "^I63"),
         IDM = str_detect(hosp_main_dgn, "^I2[12]")) |>
  filter(!(AVC & diff_date < 30)) |> 
  group_by(pat_id) |> 
  summarise(AVC_M0_M12 = any(AVC),
            IDM_M0_M12 = any(IDM))

tbl_outcomes <- outcomes |> 
  collect()

# récupération des comorbidités
icd_pattern <- "^(I1[0-5]|I48|I2[0-5]|E1[0-4]|G45|N18)"

# HTA : I10-I15
# FA : I48
# CI - Cardiopathie ischémique : I20-I25
# DBT - Diabète : E10-E14
# AIT - Antécédent d’accident ischémique transitoire : G45
# IRC - Insuffisance rénale chronique : N18

main_diagnosis <- date_of_birth_and_death |> 
  select(pat_id, stroke_date) |> 
  left_join(tab_hospitalisation |> 
              select(pat_id, hosp_start_date, hosp_main_dgn),
            by = "pat_id") |> 
  filter(hosp_start_date < stroke_date,
         grepl(x = hosp_main_dgn, pattern = icd_pattern)) |> 
  select(pat_id, comorb = hosp_main_dgn)

associated_diagnosis <- date_of_birth_and_death |> 
  select(pat_id, stroke_date) |> 
  left_join(tab_hospitalisation |> 
              select(pat_id, rsa_num, hosp_start_date),
            by = "pat_id") |> 
  filter(hosp_start_date <= stroke_date) |> 
  left_join(tab_mso_ass_dgn |> 
              select(rsa_num, dgn_ass),
            by = "rsa_num") |> 
  filter(grepl(x = dgn_ass, pattern = icd_pattern)) |> 
  select(pat_id, comorb = dgn_ass)

tbl_comorbidites <- bind_rows(main_diagnosis |> collect(),
                              associated_diagnosis |> collect()) |> 
  mutate(
    HTA = str_detect(comorb, "^I1[0-5]"),
    FA  = str_detect(comorb, "^I48"),
    CI  = str_detect(comorb, "^I2[0-5]"),
    DBT = str_detect(comorb, "^E1[0-4]"),
    AIT = str_detect(comorb, "^G45"),
    IRC = str_detect(comorb, "^N18")
  ) |> 
  group_by(pat_id) |> 
  summarise(across(.cols = c("HTA", "FA", "CI", "DBT", "AIT", "IRC"),
                   .fns = any))

tbl_snds <- tbl_date_of_birth_and_death |> 
  left_join(tbl_comorbidites,
            by = "pat_id") |> 
  left_join(tbl_outcomes,
            by = "pat_id") |> 
  mutate(across(.cols = c("AVC_M0_M12", "IDM_M0_M12", "HTA", "FA", "CI", "DBT", "AIT", "IRC"),
                .fns = ~ if_else(is.na(.x) | !.x, 0, 1))) |> 
  select(-stroke_date)

# RastaRocket::desc_var(tbl_snds |> select(-pat_id))

# Make data more realistic ------------------------------------------------

tbl_snds_makeup <- tbl_snds |> 
  mutate(
    # number of chronic conditions
    n_comorb = HTA + FA + CI + DBT + AIT + IRC,
    
    # log-odds model for 12-month mortality
    logit_death =
      -5.5 +                         # baseline mortality
      0.06 * (Age_inclusion - 60) +   # age effect
      0.8 * IRC +                     # renal disease
      0.6 * DBT +                     # diabetes
      0.7 * FA +                      # atrial fibrillation
      0.5 * CI +                      # ischemic heart disease
      1.5 * AVC_M0_M12 +              # stroke
      1.8 * IDM_M0_M12 +              # acute MI
      0.15 * n_comorb,                # multimorbidity
    
    # convert logit to probability
    p_death = plogis(logit_death),
    
    # simulate death
    deces_M12 = rbinom(n(), 1, p_death)
  ) |>
  mutate(NIHSS_initial = pmin(
    42,
    rpois(n(), lambda = 
            5 +
            3 * (Age_inclusion/60) +
            2 * FA +
            2 * DBT +
            1.5 * IRC +
            3 * CI +
            2 * n_comorb + 
            5 * FA * DBT
    )
  ),
  lambda_NIHSS_discharge = 6 -
    3 * (Age_inclusion/60) -
    2 * DBT -
    1.5 * IRC -
    2 * n_comorb,
  lambda_NIHSS_discharge = if_else(lambda_NIHSS_discharge < 0, 0, lambda_NIHSS_discharge),
  NIHSS_discharge = pmax(
    0,
    NIHSS_initial - rpois(
      n(),
      lambda = lambda_NIHSS_discharge
      ) +
      rbinom(n(), 1, 0.15) * rpois(n(), 5)
  ),
  NIHSS_M12 = pmax(
      0,
      NIHSS_discharge - rpois(
        n(),
        lambda = 5
      )
    ),
  NIHSS_M12 = if_else(
    deces_M12 == 0,
    NIHSS_M12,
    NA_real_
  ),
  # Delay from symptom onset to hospital arrival (hours)
  delay_care_hours = round(
    rgamma(
      n(),
      shape = 1.5,
      rate = 0.25
    )
  ),
  
  # Stroke localisation
  stroke_location = sample(
    c(
      "MCA",
      "Lacunar",
      "PCA",
      "Vertebro_basilar",
      "ACA"
    ),
    n(),
    replace = TRUE,
    prob = c(
      0.48,
      0.22,
      0.12,
      0.12,
      0.06
    )
  ),
  # stroke size
  stroke_size = case_when(
    stroke_location == "Lacunar" ~ "Small",
    
    stroke_location == "MCA" &
      NIHSS_initial >= 15 ~ "Large",
    
    NIHSS_initial >= 10 ~ sample(
      c("Medium", "Large"),
      n(),
      replace = TRUE,
      prob = c(0.8, 0.2)
    ),
    
    NIHSS_initial >= 9 ~ "Medium",
    
    TRUE ~ "Small"
  ),
  # sexe
  sexe = if_else(pat_sex_cod == 1, "Male", "Female")
  ) |> 
  tibble::rowid_to_column(var = "patient_id") |> 
  select(
    patient_id,
    sex = sexe,
    age_at_inclusion = Age_inclusion,
    hypertension = HTA,
    atrial_fibrillation = FA,
    ischemic_heart_disease = CI,
    diabetes = DBT,
    transient_ischemic_attack = AIT,
    chronic_kidney_disease = IRC,
    time_to_care_hours = delay_care_hours,
    stroke_location,
    stroke_size,
    NIHSS_admission = NIHSS_initial,
    NIHSS_discharge,
    NIHSS_12_months = NIHSS_M12,
    stroke_within_12_months = AVC_M0_M12,
    myocardial_infarction_within_12_months = IDM_M0_M12,
    death_within_12_months = deces_M12
  )

# RastaRocket::desc_var(tbl_snds_makeup |> select(-patient_id))

write.csv(tbl_snds_makeup, file = "tbl_stroke.csv", row.names = FALSE)

# Test models -------------------------------------------------------------

## Question TD
# Prédiction d'une normalisation du score NIHSS à 0/42 (asymptomatique) à 1 an après AVC

## Question exam
# Prédiction d'un nouvel événement cardiovasculaire majeur ou décès à 1 an

##### Explication
## Question TD
# Etude de la mortalité après AVC : âge, sexe, comorbidités
# Etude de la récupération du NIHSS à 12 mois après un AVC

## Question exam
# Facteurs causant un plus grand risque de récidive d'AVC

# model1 <- glm(formula = NIHSS_12_months ~ sex + age_at_inclusion + hypertension +
#       atrial_fibrillation + ischemic_heart_disease + diabetes +
#       transient_ischemic_attack + chronic_kidney_disease,
#     data = tbl_snds_makeup,
#     family = "gaussian")
# 
# gtsummary::tbl_regression(model1)

