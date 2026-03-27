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
  print("Hi1")
  print(head(data))
  dictionary = insurance_codebook
  dictionary = as.data.frame(dictionary)
  print("Hi2")
  print(head(dictionary))
  dictionary$categorization = as.character(dictionary$categorization)
  print("Hi3")
  insurance_categorization = rep(NA, times = nrow(data))
  print(length(insurance_categorization))
  for(i in 1:length(insurance_categorization)){
    print(i)
    if(i<10)(print(data[i,"insurance_list"]))
    insurance_categorization[i] = dictionary[which(dictionary$insurance==data[i,"insurance_list"]),]$categorization
  }
  #insurance_categorization = apply(data, 1, function(x) dictionary[which(dictionary$insurance==x["insurance_list"]),]$categorization)
  print("Hi4")
  print(insurance_categorization[1:10])
  insurance_categorization = factor(insurance_categorization, levels = c("1", "2", "3", "7", "4", "5", "6", "8"), labels = c("Commercial", "Medicare", "Medicaid", "MassHealth Limited", "Auto", "Workers' Comp", "Other", "Missing"))
  print("Hi5")
  print(insurance_categorization[1:10])
  return(insurance_categorization)

}


