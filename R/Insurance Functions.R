#' Categorize Insurance
#'
#' `categorize_insurance` categorizes the unique combinations of a patient's
#' payor and benefit plan as one of the following: Commercial, Medicare,
#' Medicaid, MassHealth Limited, Auto, Workers' Comp, Other, or Missing.
#'
#' @param insurance_list the vector of patients' insurance. The values must be
#' formatted as
#' "Payor - insert_payor_name_here; Plan - insert_benefit_plan_name_here". For
#' example, "Payor -  AETNA; Plan - AETNA PPO" or
#' "Payor -  CIGNA; Plan - CIGNA LOCAL PLUS".
#' @export

categorize_insurance = function(insurance_list){

  data = as.data.frame(insurance_list)

  dictionary = insurance_codebook

  dictionary$categorization = as.character(dictionary$categorization)

  insurance_categorization = apply(data, 1, function(x) dictionary[which(dictionary$insurance==x[,"insurance_list"]),]$categorization)
  insurance_categorization = factor(insurance_categorization, levels = c("1", "2", "3", "7", "4", "5", "6", "8"), labels = c("Commercial", "Medicare", "Medicaid", "MassHealth Limited", "Auto", "Workers' Comp", "Other", "Missing"))

  return(insurance_categorization)

}


