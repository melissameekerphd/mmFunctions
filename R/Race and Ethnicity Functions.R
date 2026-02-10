#' Function to Clean Binary Ethnicity Variable
#'
#' `clean_binary_ethnicity` takes values of "Not Hispanic", "No Non Hispanic",
#' "Portugese", "Madagascar", "Hispanic", "Yes Hispanic",
#' "Prefer not to say/Decline", "Declined", "Unavailable",
#' "Patient doesn't know", and "NULL" and returns values in (Non-Hispanic,
#' Hispanic, Declined, and Unavailable)
#'
#' @param x the variable "ETHNIC_GROUP_DSC" from EPIC
#' @export

clean_binary_ethnicity = function(x){
  binary_ethnicity = x["ETHNIC_GROUP_DSC"]
  cleaned = NA

  if(binary_ethnicity %in% c("Not Hispanic", "No Non Hispanic", "Portugese", "Madagascar")){
    cleaned = "Non-Hispanic"
  } else if(binary_ethnicity %in% c("Hispanic", "Yes Hispanic")){
    cleaned = "Hispanic"
  } else if(binary_ethnicity %in% c("Prefer not to say/Decline", "Declined")){
    cleaned = "Declined"
  } else if(binary_ethnicity %in% c("Unavailable", "Patient doesn't know", "NULL")){
    cleaned = "Unavailable"
  }

  return(cleaned)
}

#' Helper Function to Categorize Race
#'
#' `clean_race_variable` Applies the definitions for taking the race components
#' and assigning one value.
#'
#' @param x the race variable components from 'categorize_race'
#' @param version version chooses which version to return. The default "v1"
#' cleans race such that White + Other for example, returns 'More Than 1 Race'.
#' The other option, "v2", cleans race such that White + Other for example,
#' returns 'White'.

clean_race_variable = function(x, version){

  race_vars = c("race_white",
                "race_black_or_african_american",
                "race_asian",
                "race_native_hawaiian_or_other_pacific_islander",
                "race_american_indian_or_alaska_native")

  race_label = c("White",
                 "Black or African American",
                 "Asian",
                 "Native Hawaiian or Other Pacific Islander",
                 "American Indian or Alaska Native")

  cleaned = NA

  race_n = as.numeric(x["race_n"])

  #Ex ("White; Other" --> "White")
  if(version == "v1"){
    if(is.na(x["race_list"])){
      cleaned = "Unknown"
    } else if(race_n==0 & (x["race_other"]==TRUE|x["race_hispanic"]==TRUE)){
      cleaned = "Other"
    } else if(race_n==0 & (x["race_unknown"]==TRUE)){
      cleaned = "Unknown"
    } else if(race_n==1){
      cleaned = race_label[which(x[race_vars]==1)]
    } else if(race_n>1){
      cleaned = "More Than 1 Race"
    }
  }

  #Ex ("White; Other" --> "More Than 1 Race")
  if(version == "v2"){
    if(is.na(x["race_list"])){
      cleaned = "Unknown"
    } else if(race_n==0 & (x["race_other"]==TRUE|x["race_hispanic"]==TRUE)){
      cleaned = "Other"
    } else if(race_n==0 & x["race_unknown"]==TRUE){
      cleaned = "Unknown"
    } else if(race_n==1 & as.logical(x["race_other"])==FALSE){
      cleaned = race_label[which(x[race_vars]==1)]
    } else if(race_n==1 & as.logical(x["race_other"])==TRUE){
      cleaned = "More Than 1 Race"
    } else if(race_n>1){
      cleaned = "More Than 1 Race"
    }
  }


  return(cleaned)

}

#' Return Factor of a Cleaned Race Variable
#'
#' `create_race_list` returns a cleaned race variable of the following
#' potential values: "White", "Black or African American", "Asian", "American
#' Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander", "More
#' Than 1 Race", "Other", "Unknown"
#'
#' @param race_list the initial race list containing these potential values
#' separated by the specified delimiter: white, black or african american,
#' asian, native hawaiian or other pacific islander, american indian or alaska
#' native, hispanic or latino, other, unavailable, declined, patient doesn't
#' know, or race not listed
#' @param delimeter the delimiter the separates the races in each value of
#' 'race_list'
#' @param version version chooses which version to return. The default "v1"
#' cleans race such that White + Other for example, returns 'White'.
#' The other option, "v2", cleans race such that White + Other for example,
#' returns 'More Than 1 Race'.
#' @export

categorize_race = function(race_list, version = "v1", delimeter = ','){

  if(version != "v1" & version != "v2"){
    cat(red(bold("The argument \"version\" must be equal to \"v1\" or \"v2\".")))
  } else {
    #UPDATING MISSING VALUES FROM EPIC PULL
    race_list = ifelse(race_list=='NULL', NA, race_list)

    #MAKING BINARY VARIABLES OUT OF RACE VARIABLES
    race_vars = mtabulate(strsplit(race_list, delimeter))
    colnames(race_vars) = paste0("race_", tolower(str_replace_all(colnames(race_vars), " ", "_")))

    race_vars = as.data.frame(apply(race_vars, 2, function(x) ifelse(x>=1,1,0)))

    all_vars = c("race_white",
                 "race_black_or_african_american",
                 "race_asian",
                 "race_native_hawaiian_or_other_pacific_islander",
                 "race_american_indian_or_alaska_native",
                 "race_hispanic_or_latino",
                 "race_other",
                 "race_unavailable",
                 "race_declined",
                 "race_patient_doesn't_know",
                 "race_race_not_listed")

    for(v in all_vars){
      if(!(v %in% colnames(race_vars))){
        toadd = as.data.frame(matrix(0, nrow = nrow(race_vars), ncol = 1))
        colnames(toadd) = v
        race_vars = cbind(race_vars, toadd)
      }
    }

    data = cbind(race_list, race_vars)
    remove(race_vars)

    data = data %>% mutate(race_n = race_white+
                             race_black_or_african_american+
                             race_asian+
                             race_native_hawaiian_or_other_pacific_islander+
                             race_american_indian_or_alaska_native,
                           race_hispanic = race_hispanic_or_latino==1,
                           race_other = race_other==1|
                             race_race_not_listed==1|
                             race_unavailable==1,
                           race_unknown = is.na(race_list)|
                             `race_patient_doesn't_know`==1|
                             race_declined==1)

    data$race_clean = apply(data[,c("race_list",
                                    "race_n",
                                    "race_hispanic",
                                    "race_other",
                                    "race_unknown",
                                    "race_white",
                                    "race_black_or_african_american",
                                    "race_asian",
                                    "race_native_hawaiian_or_other_pacific_islander",
                                    "race_american_indian_or_alaska_native")], 1, function(x) clean_race_variable(x, version = version))

    data$race_factor = factor(data$race_clean, levels = c("White",
                                                          "Black or African American",
                                                          "Asian",
                                                          "American Indian or Alaska Native",
                                                          "Native Hawaiian or Other Pacific Islander",
                                                          "More Than 1 Race",
                                                          "Other",
                                                          "Unknown"))

    return(data$race_factor)
  }


}

#' Helper function to format race list
#'
#' `clean_list` returns a nicely formatted list of races
#'
#' @param x the race data
#' @param race_vars the race variables
#' @param race_label the corresponding labels

clean_list = function(x, race_vars, race_label){
  marked = x[which(x==1)]
  marked = names(marked)

  list = race_label[which(race_vars %in% marked)]

  clean_list = paste(list, collapse = "; ")

  return(clean_list)
}

#' Return List of Races Separated by Delimiter
#'
#' `create_race_list` returns a string that is a list of races (American Indian
#' or Alaska Native, Asian, Black or African American, Native Hawaiian or Other
#' Pacific Islander, White, Other, Unknown) separated by the specified delimiter
#'
#' @param race_list the initial race list containing these potential values
#' separated by the specified delimiter: white, black or african american,
#' asian, native hawaiian or other pacific islander, american indian or alaska
#' native, hispanic or latino, other, unavailable, declined, patient doesn't
#' know, or race not listed
#' @param delimeter the delimiter the separates the races in each value of
#' 'race_list'
#' @export

create_race_list = function(race_list, delimeter = ','){
  #UPDATING MISSING VALUES FROM EPIC PULL
  race_list = ifelse(race_list=='NULL', NA, race_list)

  #MAKING BINARY VARIABLES OUT OF RACE VARIABLES
  race_vars = mtabulate(strsplit(race_list, delimeter))
  colnames(race_vars) = paste0("race_", tolower(str_replace_all(colnames(race_vars), " ", "_")))

  race_vars = as.data.frame(apply(race_vars, 2, function(x) ifelse(x>=1,1,0)))

  all_vars = c("race_white",
               "race_black_or_african_american",
               "race_asian",
               "race_native_hawaiian_or_other_pacific_islander",
               "race_american_indian_or_alaska_native",
               "race_hispanic_or_latino",
               "race_other",
               "race_unavailable",
               "race_declined",
               "race_patient_doesn't_know",
               "race_race_not_listed")

  for(v in all_vars){
    if(!(v %in% colnames(race_vars))){
      toadd = as.data.frame(matrix(0, nrow = nrow(race_vars), ncol = 1))
      colnames(toadd) = v
      race_vars = cbind(race_vars, toadd)
    }
  }

  data = cbind(race_list, race_vars)
  remove(race_vars)

  data = data %>% mutate(race_other = race_other==1|
                           race_race_not_listed==1|
                           race_unavailable==1,
                         race_unknown = is.na(race_list)|
                           `race_patient_doesn't_know`==1|
                           race_hispanic_or_latino==1|
                           race_declined==1)

  data$race_other = ifelse(data$race_other, 1, 0)
  data$race_unknown = ifelse(data$race_unknown, 1, 0)

  race_vars = c("race_american_indian_or_alaska_native",
                "race_asian",
                "race_black_or_african_american",
                "race_native_hawaiian_or_other_pacific_islander",
                "race_white",
                "race_other",
                "race_unknown")

  race_label = c("American Indian or Alaska Native",
                 "Asian",
                 "Black or African American",
                 "Native Hawaiian or Other Pacific Islander",
                 "White",
                 "Other",
                 "Unknown")

  data$race_list_clean = apply(data[,c(race_vars)], 1, function(x) clean_list(x, race_vars, race_label))

  return(data$race_list_clean)

}


#' Return cleaned ethnicity helper variables
#'
#' `return_ethnicity_vars` returns ethnicity helper variables used in
#' categorize_ethnicity
#'
#' @param ethnicity_list the country of origin data
#' @param delimeter the delimiter the separates the country of origins in each
#' value of ethnicity_list

return_ethnicity_vars = function(ethnicity_list, delimeter = ','){

  #UPDATING MISSING VALUES FROM EPIC PULL
  ethnicity_list = ifelse(ethnicity_list=='NULL', NA, ethnicity_list)

  #MAKING BINARY VARIABLES OUT OF ETHNICITY VARIABLES
  ethnicity_vars = mtabulate(sapply(strsplit(ethnicity_list, delimeter), unique))
  colnames(ethnicity_vars) = paste0("ethn_", trimws(colnames(ethnicity_vars)))

  return(ethnicity_vars)

}


#' Identify Country of Origin as Hispanic or Non-Hispanic
#'
#' `categorize_ethnicity` cleans ETHNICITY_LIST and passes the data to another
#' function called 'hispanic_detailed' to categorize as Hispanic or Non-Hispanic
#'
#' @param ethnicity_list the country of origin data
#' @param def the Hispanic Dictionary to use; default is 'ombp' and
#' other options include 'omb' and 'c'.
#' @param delimeter the delimiter the separates the country of origins in each
#' value of ethnicity_list
#' @param file the file pathway of an old ethnicity codebook, only if needed
#' @export

categorize_ethnicity = function(ethnicity_list, def = "ombp", delimeter = ',', file = NULL){

  #UPDATING MISSING VALUES FROM EPIC PULL
  ethnicity_list = ifelse(ethnicity_list=='NULL', NA, ethnicity_list)

  #MAKING BINARY VARIABLES OUT OF ETHNICITY VARIABLES
  ethnicity_vars = mtabulate(sapply(strsplit(ethnicity_list, delimeter), unique))
  colnames(ethnicity_vars) = paste0("ethn_", trimws(colnames(ethnicity_vars)))
  ethn = cbind(ethnicity_list, ethnicity_vars)

  # UNAVAILABLE OR DECLINED VARIABLES
  decl = c("ethn_Declined")
  unavail = c("ethn_Unavailable", "ethn_Other-Specify", "ethn_Ethnicity not listed-Specify", "ethn_Patient doesn't know")

  # DEFINING UNAVAILABLE AND MISSING FLAGS
  ethn$ethn_list_decl = ifelse(apply(as.data.frame(ethn[,decl[which(decl %in% colnames(ethn))]]),1, function(x) any(x==1)),1,0)
  ethn$ethn_list_unavail = ifelse(apply(as.data.frame(ethn[,unavail[which(unavail %in% colnames(ethn))]]),1, function(x) any(x==1)),1,0)
  ethn$ethn_list_na = ifelse(is.na(ethn$ethnicity_list),1,0)

  result = hispanic_detailed(ethn, def, file)

}


#' Identify Country of Origin as Hispanic or Non-Hispanic
#'
#' `hispanic_detailed` determines if a country of origin is Hispanic or
#' Non-Hispanic given a specific definition (one of ombp, omb, or c).
#'
#' @param data the country of origin data
#' @param definition the Hispanic Dictionary to use; default is 'ombp' and
#' other options include 'omb' and 'c'.
#' @param f the file path to another iteration of the codebook

hispanic_detailed = function(data, definition = "ombp", f){

  if(is.null(f)){
    dictionary = ethnicity_codebook
  } else {
    ethnicity_dictionary_file = f
    dictionary = read_csv(ethnicity_dictionary_file)

  }

  dictionary[is.na(dictionary)] = 0
  dictionary = dictionary[which(dictionary$ethnicity!="Unavailable"),]

  if(definition == "c"){

    dictionary = dictionary[,c("ethnicity", "complete")]
    colnames(dictionary) = c("ethnicity", "hispanic_flg")

  } else if(definition == "ombp"){

    dictionary = dictionary[,c("ethnicity", "omb_plus")]
    colnames(dictionary) = c("ethnicity", "hispanic_flg")

  } else if(definition == "omb"){

    dictionary = dictionary[,c("ethnicity", "omb")]
    colnames(dictionary) = c("ethnicity", "hispanic_flg")

  } else if(definition == "cp"){

    dictionary = dictionary[,c("ethnicity", "complete_plus")]
    colnames(dictionary) = c("ethnicity", "hispanic_flg")

  } else {
    return("Definition can be 'c', 'ombp', or 'omb' (or 'cp' if using old file).")
  }

  # CATEGORIZING ETHNICITY VARIABLE NAMES AS HISPANIC OR NON HISPANIC
  hispanic_vars = paste0("ethn_", dictionary[which(dictionary$hispanic_flg==1),]$ethnicity)
  nonhispanic_vars = paste0("ethn_", dictionary[which(dictionary$hispanic_flg==0),]$ethnicity)

  all_vars = paste0("ethn_", trimws(dictionary$ethnicity))

  for(v in all_vars){
    if(!(v %in% colnames(data))){
      #data = data %>% mutate(!!v:=0)

      toadd = as.data.frame(matrix(0, nrow = nrow(data), ncol = 1))
      colnames(toadd) = v
      data = cbind(data, toadd)
    }
  }

  # CATEGORIZING PATIENT ETHNICITY AS HISPANIC OR NON HISPANIC
  hispanic = ifelse(apply(data[,hispanic_vars],1, function(x) any(x==1)),1,0)
  nonhispanic = ifelse(apply(data[,nonhispanic_vars],1, function(x) any(x==1)),1,0)

  categorization = ifelse(hispanic==1, "Hispanic",
                          ifelse(nonhispanic==1, "Non-Hispanic",
                                 ifelse(data$ethn_list_decl==1, "Declined",
                                        ifelse(data$ethn_list_na==1|data$ethn_list_unavail==1, "Unavailable/Missing", NA))))

  categorization = factor(categorization, levels = c("Hispanic", "Non-Hispanic", "Declined", "Unavailable/Missing"))
  return(categorization)
}

