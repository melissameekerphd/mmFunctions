################################################################################
# Name: simulate_ed_dataset
# Purpose: Simulate a data set of ED encounters
#
# Arguments:
#   n_encounters - the number of ED encounters
#   n_patients - the number of patients; needs to be less than the number of
#                encounters
#   seed - the seed for random number generation
################################################################################

simulate_ed_dataset <- function(n_encounters = 10000, n_patients = 8500, seed = 42) {

  if(n_patients>=n_encounters){
    print("Number of patients needs to be less than the number of encounters.")
    return(NULL)
  }

  set.seed(seed)

  # Patient Data
  unique_pat_ids <- sprintf("P%07d", seq_len(n_patients))

  age = round(rnorm(n = n_patients, mean = 48, sd = 20))
  age[which(age<=0)] = runif(n = length(age[which(age<=0)]), min = 1, max = 17)
  age[which(age>=100)] = runif(n = length(age[which(age>=100)]), min = 65, max = 99)
  sex = sample(c("Female", "Male"), size = n_patients, replace = TRUE)
  language = sample(c("English", "Non-English"), size = n_patients, replace = TRUE)

  pat_data = as.data.frame(cbind(unique_pat_ids, age, sex, language))

  # Setting up encounters
  encounter_id = sprintf("P%07d", seq_len(n_encounters))
  patient_id = sample(unique_pat_ids, size = n_encounters, replace = TRUE)

  encounter_data = as.data.frame(cbind(encounter_id, patient_id))
  encounter_data = merge(encounter_data, pat_data, by.x = "patient_id", by.y = "unique_pat_ids")

  # ESI:
  encounter_data$esi_prob = rnorm(n = n_encounters, mean = 0.5, sd = 0.1) -
                            0.050 * (encounter_data$age>=18) -
                            0.100 * (encounter_data$age>=65) +
                            0.025 * (encounter_data$sex=="Female") +
                            0.050 * (encounter_data$language=="Non-English")

  esi_quantile_noise = rnorm(n = 4, mean = 0, sd = 0.05)
  esi_quantiles = c(0.1, 0.4, 0.7, 0.85) + esi_quantile_noise
  esi_cutoffs = quantile(encounter_data$esi_prob, probs = esi_quantiles)

  encounter_data$esi = ifelse(encounter_data$esi_prob< esi_cutoffs[1], 1,
                              ifelse(encounter_data$esi_prob< esi_cutoffs[2], 2,
                                     ifelse(encounter_data$esi_prob< esi_cutoffs[3], 3,
                                            ifelse(encounter_data$esi_prob< esi_cutoffs[4], 4,5))))

  # Time to antibiotic:
  encounter_data$time_to_abx = rnorm(n = n_encounters, mean = 120, sd = 20) -
                                rnorm(n = n_encounters, mean = 20, sd = 2) * (encounter_data$esi==1) -
                                rnorm(n = n_encounters, mean = 10, sd = 2) * (encounter_data$esi%in% c(2, 3)) -
                                rnorm(n = n_encounters, mean = 15, sd = 2) * (encounter_data$age>=18) -
                                rnorm(n = n_encounters, mean = 15, sd = 2) * (encounter_data$age>=65) +
                                rnorm(n = n_encounters, mean = 20, sd = 5) * (encounter_data$sex=="Female") +
                                rnorm(n = n_encounters, mean = 30, sd = 5) * (encounter_data$language=="Non-English")
  encounter_data$time_to_abx = ifelse(encounter_data$time_to_abx<0, 1, encounter_data$time_to_abx)

  encounter_data = encounter_data[,c("encounter_id", "patient_id",
                                     "age", "sex", "language",
                                     "esi", "time_to_abx")]

  return(encounter_data)
}

