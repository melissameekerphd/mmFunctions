#' LINEAR REGRESSION
#'
#' `calculate_lm_output` organizes linear regression model output into table
#' for publication. Note: Always evaluate output from summary of model, as well.
#'
#' @param lm_model the linear regression model object
#' @param decimal the number of decimal places to be used in output
#' @export

calculate_lm_output = function(lm_model, decimal = 2){
  summary = summary(lm_model)

  coef = as.data.frame(summary$coefficients)

  coef$pvalue = ifelse(coef$`Pr(>|t|)`<0.001, "<0.001", round(coef$`Pr(>|t|)`, 3))

  coef$lower = coef$Estimate-1.96*coef$`Std. Error`
  coef$upper = coef$Estimate+1.96*coef$`Std. Error`

  coef$conf_int = paste0("[", format(round(coef$lower,decimal), nsmall = decimal), ", ",
                                format(round(coef$upper,decimal), nsmall = decimal), "]")

  coef = coef[-1,]

  #coef = cbind(coef, olsrr::ols_vif_tol(lm_model))

  coef = coef[,c("Estimate", "conf_int", "pvalue")]
  #coef = coef[,c("Estimate", "conf_int", "pvalue", "VIF")]

  return(coef)
}

#' GENERALIZED LINEAR REGRESSION (GLM)
#'
#' `calculate_glm_output` organizes genearlized linear regression model output
#' (binomial, quasipoisson, and poisson) into table for publication. Binomial
#' for binary outcome data; poisson for count outcome data; quasipoisson when
#' there is overdispersion.Note: Always evaluate output from summary of model,
#' as well.
#'
#' @param glm_model the generalized linear regression model object
#' @param decimal the number of decimal places to be used in output
#' @export


calculate_glm_output = function(glm_model){

  summary = summary(glm_model)
  glm_results = as.data.frame(coef(summary))

  if(summary$family$family=="binomial"){
    glm_results$pvalue = ifelse(glm_results$`Pr(>|z|)`<0.001, "<0.001", round(glm_results$`Pr(>|z|)`, 3))

    glm_results$lower = glm_results$Estimate-1.96*glm_results$`Std. Error`
    glm_results$upper = glm_results$Estimate+1.96*glm_results$`Std. Error`

    glm_results$odds_ratio = exp(glm_results$Estimate)
    glm_results$odds_lower = exp(glm_results$lower)
    glm_results$odds_upper = exp(glm_results$upper)

    glm_results$conf_int = paste0("[", format(round(glm_results$odds_lower,2), nsmall = 2, scientific = F), ", ",
                                  format(round(glm_results$odds_upper,2), nsmall = 2, scientific = F), "]")


    glm_results = glm_results[,c("odds_ratio", "conf_int", "pvalue")]
  } else if(summary$family$family=="quasipoisson"){
    glm_results$pvalue = ifelse(glm_results$`Pr(>|t|)`<0.001, "<0.001", round(glm_results$`Pr(>|t|)`, 3))

    glm_results$lower = glm_results$Estimate-1.96*glm_results$`Std. Error`
    glm_results$upper = glm_results$Estimate+1.96*glm_results$`Std. Error`

    glm_results$rate_ratio = exp(glm_results$Estimate)
    glm_results$rr_lower = exp(glm_results$lower)
    glm_results$rr_upper = exp(glm_results$upper)

    glm_results$conf_int = paste0("[", format(round(glm_results$rr_lower,2), nsmall = 2, scientific = F), ", ",
                                  format(round(glm_results$rr_upper,2), nsmall = 2, scientific = F), "]")


    glm_results = glm_results[,c("rate_ratio", "conf_int", "pvalue")]
  } else if(summary$family$family=="poisson"){
    glm_results$pvalue = ifelse(glm_results$`Pr(>|z|)`<0.001, "<0.001", round(glm_results$`Pr(>|z|)`, 3))

    glm_results$lower = glm_results$Estimate-1.96*glm_results$`Std. Error`
    glm_results$upper = glm_results$Estimate+1.96*glm_results$`Std. Error`

    glm_results$rate_ratio = exp(glm_results$Estimate)
    glm_results$rr_lower = exp(glm_results$lower)
    glm_results$rr_upper = exp(glm_results$upper)

    glm_results$conf_int = paste0("[", format(round(glm_results$rr_lower,2), nsmall = 2, scientific = F), ", ",
                                  format(round(glm_results$rr_upper,2), nsmall = 2, scientific = F), "]")


    glm_results = glm_results[,c("rate_ratio", "conf_int", "pvalue")]
  }

  return(glm_results)
}

#' LINEAR MIXED EFFECT MODEL
#'
#' `calculate_lmer_output` organizes linear mixed effect regression model output
#' into table for publication. Note: Always evaluate output from summary of
#' model, as well.
#'
#' @param model the linear mixed effect regression model object
#' @param decimal the number of decimal places to be used in output
#' @export

calculate_lmer_output = function(model, decimal = 0){
  #summary = summary(model, ddf = "Satterthwaite")#link: https://link.springer.com/article/10.3758/s13428-016-0809-y
  for_lmerTest = lmerTest::as_lmerModLmerTest(model)
  summary = summary(for_lmerTest)
  coefs = summary$coefficients
  coefs = as.data.frame(coefs)
  coefs = coefs[2:nrow(coefs),]

  conf = confint(model)
  conf = as.data.frame(conf)
  conf = conf[(which(rownames(conf)=="(Intercept)")+1):nrow(conf),]

  results = cbind(coefs, conf)
  results$confint = paste0("[", round(results$`2.5 %`,decimal), ", ", round(results$`97.5 %`, decimal), "]")
  results$pvalue = ifelse(results$`Pr(>|t|)`<0.001, "<0.001", round(results$`Pr(>|t|)`, 3))
  results$estimate = round(results$Estimate, decimal)
  results = results[,c("estimate", "pvalue", "confint")]

  return(results)
}

#' GENERALIZED LINEAR MIXED EFFECT MODEL
#'
#' `calculate_glmer_output` organizes generalized linear mixed effect
#' regression model (binomial and poisson) output into table for publication.
#' Note: Always evaluate output from summary of model, as well.
#'
#' @param model the generalized linear mixed effect regression model object
#' @param decimal the number of decimal places to be used in output
#' @export

calculate_glmer_output = function(model, decimal = 2){

  family = model@resp$family$family

  summary = summary(model)
  coef = as.data.frame(summary$coefficients)

  coef$pvalue = ifelse(coef$`Pr(>|z|)`<0.001, "<0.001", round(coef$`Pr(>|z|)`, 3))
  coef$ll = coef$Estimate - 1.96*coef$`Std. Error`
  coef$ul = coef$Estimate + 1.96*coef$`Std. Error`

  coef_exp = exp(coef[,c("Estimate", "ll", "ul")])
  coef_exp$confint = paste0("[", format(round(coef_exp$ll,decimal), nsmall = 2), ", ", format(round(coef_exp$ul, decimal), nsmall = 2), "]")

  coef_exp = cbind(coef_exp[,c("Estimate", "confint")], coef[,c("pvalue")])

  if(family == "binomial"){
    colnames(coef_exp) = c("odds ratio", "95% CI", "p-value")
  } else if(family == "poisson"){
    colnames(coef_exp) = c("rate ratio", "95% CI", "p-value")
  }

  return(coef_exp[-1,])
}

#' GENERALIZED LINEAR MIXED EFFECT MODEL: Modified Poisson
#'
#' `calculate_glmer_modified_poisson_output` organizes modified poisson mixed
#' effect model output into table for publication. Applies standard errors to
#' be able to use poisson model for binary outcome.
#' Note: Always evaluate output from summary of model, as well.
#'
#' @param model the generalized linear mixed effect regression model object
#' @param decimal the number of decimal places to be used in output
#' @export

calculate_glmer_modified_poisson_output = function(model, decimal = 2){
  summary = summary(model)
  coef = as.data.frame(summary$coefficients)

  sand = sandwich(model)
  std_errs = sqrt(diag(sand)[1:nrow(coef)])

  coef = cbind(coef, std_errs)

  coef$robust_z = coef$Estimate/coef$std_errs
  coef$pvalue = 2 * (1-pnorm(abs(coef$robust_z)))
  coef$pvalue = ifelse(coef$pvalue<0.001, "<0.001", round(coef$pvalue, 3))
  coef$ll = coef$Estimate - 1.96*coef$std_errs
  coef$ul = coef$Estimate + 1.96*coef$std_errs

  coef_exp = exp(coef[,c("Estimate", "ll", "ul")])
  coef_exp$confint = paste0("[", format(round(coef_exp$ll,decimal), nsmall = 2), ", ", format(round(coef_exp$ul,decimal), nsmall = 2), "]")

  coef_exp = cbind(coef_exp[,c("Estimate", "confint")], coef[,c("pvalue")])
  colnames(coef_exp) = c("rate ratio", "95% CI", "p-value")

  return(coef_exp[-1,])
}

#' GENERALIZED ESTIMATING EQUATION MODELS
#'
#' `calculate_gee_output` organizes GEE model (Gaussian, Binomial, and Poisson)
#' output into table for publication. Note: Always evaluate output from summary
#' of model, as well.
#'
#' @param model the GEE model object
#' @param decimal the number of decimal places to be used in output
#' @export

calculate_gee_output = function(model, decimal = 2){

  summary = summary(model)
  gee_results = as.data.frame(coef(summary))

  gee_results$pvalue = 2 * (1-pnorm(abs(gee_results$`Robust z`)))
  gee_results$pvalue = ifelse(gee_results$pvalue<0.001, "<0.001", round(gee_results$pvalue, 3))


  gee_results$lower = gee_results$Estimate-1.96*gee_results$`Robust S.E.`
  gee_results$upper = gee_results$Estimate+1.96*gee_results$`Robust S.E.`

  if(summary$model$varfun=="Gaussian"){

    gee_results$conf_int = paste0("[", format(round(gee_results$lower,decimal), nsmall = decimal), ", ",
                                  format(round(gee_results$upper,decimal), nsmall = decimal), "]")

    gee_results = gee_results[,c("Estimate", "conf_int", "pvalue")]

  } else if(summary$model$varfun=="Binomial"){

    #Exponentiate for odds
    gee_results$odds_ratio = exp(gee_results$Estimate)
    gee_results$odds_lower = exp(gee_results$lower)
    gee_results$odds_upper = exp(gee_results$upper)

    gee_results$conf_int = paste0("[", format(round(gee_results$odds_lower,decimal), nsmall = decimal), ", ",
                                  format(round(gee_results$odds_upper,decimal), nsmall = decimal), "]")

    gee_results = gee_results[,c("odds_ratio", "conf_int", "pvalue")]
  } else if(summary$model$varfun=="Poisson"){

    #Exponentiate for odds
    gee_results$ratio = exp(gee_results$Estimate)
    gee_results$odds_lower = exp(gee_results$lower)
    gee_results$odds_upper = exp(gee_results$upper)

    gee_results$conf_int = paste0("[", format(round(gee_results$odds_lower,decimal), nsmall = decimal), ", ",
                                  format(round(gee_results$odds_upper,decimal), nsmall = decimal), "]")

    gee_results = gee_results[,c("ratio", "conf_int", "pvalue")]
  }

  return(gee_results)
}

#' GLM (BINOMIAL) output for forest plot
#'
#' `glm_output_for_forest_plot` organizes GLM (BINOMIAL/LOGISTIC) output to
#' help with a forest plot. Note: Always evaluate output from summary of model,
#' as well.
#'
#' @param model the logistic regression (GLM) model object
#' @param decimal the number of decimal places to be used in output
#' @export

glm_output_for_forest_plot = function(model, decimal = 2){

  summary = summary(model)
  glm_results = as.data.frame(coef(summary))

  glm_results$pvalue = glm_results$`Pr(>|t|)`

  glm_results$lower = glm_results$Estimate-1.96*glm_results$`Std. Error`
  glm_results$upper = glm_results$Estimate+1.96*glm_results$`Std. Error`

  glm_results$odds_ratio = exp(glm_results$Estimate)
  glm_results$odds_lower = exp(glm_results$lower)
  glm_results$odds_upper = exp(glm_results$upper)

  glm_results$conf_int = paste0("[", format(round(glm_results$odds_lower,decimal), nsmall = decimal), ", ",
                                format(round(glm_results$odds_upper,decimal), nsmall = decimal), "]")


  glm_results = glm_results[,c("odds_ratio", "odds_lower", "odds_upper", "conf_int")]
  colnames(glm_results) = c("OR", "LL", "UL", "CI")

  return(glm_results)
}
