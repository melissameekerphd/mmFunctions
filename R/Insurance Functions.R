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
  dictionary = as.data.frame(dictionary)

  dictionary$categorization = as.character(dictionary$categorization)

  insurance_categorization = rep(NA, times = nrow(data))

  for(i in 1:length(insurance_categorization)){

    id = which(dictionary$insurance==data[i,"insurance_list"])

    if(length(id)==1){
      insurance_categorization[i] = dictionary[id,]$categorization
    }

  }

  insurance_categorization = factor(insurance_categorization, levels = c("1", "2", "3", "7", "4", "5", "6", "8"), labels = c("Commercial", "Medicare", "Medicaid", "MassHealth Limited", "Auto", "Workers' Comp", "Other", "Missing"))

  return(insurance_categorization)

}


