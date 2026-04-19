# pkonfound_applicability.R
# Helpers used by pkonfound() to emit an informational message when
# users supply arguments that are not applicable to the analysis
# they have selected. Messages use message(), not warning()/stop(),
# so existing scripts keep running.

# ---------------------------------------------------------------
# Lookup: which arguments are meaningful under each dispatch branch.
# "universal" applies to every branch; each named branch entry lists
# everything else that branch actually uses.
# ---------------------------------------------------------------
.pkonfound_applicable <- list(
    universal = c("to_return", "alpha", "index", "model_type"),
    
    RIR_t = c("est_eff", "std_err", "n_obs", "n_covariates",
              "tails", "nu",
              "upper_bound", "lower_bound",
              "scale"),
    
    RIR_r_lm = c("est_eff", "std_err", "n_obs", "n_covariates",
                 "tails", "nu",
                 "upper_bound", "lower_bound",
                 "scale"),
    
    RIR_r_glm = c("est_eff", "std_err", "n_obs", "n_covariates",
                  "tails", "nu",
                  "upper_bound", "lower_bound",
                  "scale", "link"),
    
    ols_IT = c("est_eff", "std_err", "n_obs", "n_covariates",
               "tails", "nu",
               "upper_bound", "lower_bound",
               "sdx", "sdy", "R2",
               "far_bound", "eff_thr"),
    
    COP = c("est_eff", "std_err", "n_obs", "n_covariates",
            "tails",
            "sdx", "sdy", "R2",
            "eff_thr", "FR2max", "FR2max_multiplier"),
    
    PSE = c("est_eff", "std_err", "n_obs", "n_covariates",
            "sdx", "sdy", "R2",
            "eff_thr"),
    
    cRIR = c("est_eff", "std_err", "n_obs", "n_covariates",
             "tails", "R2"),
    
    VAM = c("est_eff", "n_obs",
            "replace_stu", "peer_effect_pi", "eff_thr"),
    
    logistic = c("est_eff", "std_err", "n_obs", "n_covariates",
                 "tails", "nu",
                 "n_treat", "switch_trm", "replace",
                 "raw_treatment_success"),
    
    two_by_two = c("a", "b", "c", "d", "two_by_two_table",
                   "test", "switch_trm", "replace")
)

# ---------------------------------------------------------------
# Determine which dispatch branch pkonfound() will take. Mirrors
# the if/else chain inside pkonfound(); keep in sync when adding
# new dispatch branches.
# ---------------------------------------------------------------
.determine_pkonfound_branch <- function(index, model_type,
                                        n_treat, a, two_by_two_table,
                                        scale, link) {
    if (index == "COP")  return("COP")
    if (index == "PSE")  return("PSE")
    if (index == "cRIR") return("cRIR")
    if (index == "VAM")  return("VAM")
    if (model_type == "logistic" && !is.null(n_treat)) return("logistic")
    if (!is.null(a))                return("two_by_two")
    if (!is.null(two_by_two_table)) return("two_by_two")
    if (index == "RIR") {
        if (!is.null(link))        return("RIR_r_glm")
        if (identical(scale, "r")) return("RIR_r_lm")
        return("RIR_t")
    }
    if (model_type == "ols") return("ols_IT")
    NA_character_
}

# ---------------------------------------------------------------
# Emit a single combined message if any user-supplied arguments
# are not applicable to the dispatched branch. Silent otherwise.
# ---------------------------------------------------------------
.check_pkonfound_applicability <- function(user_args, branch) {
    if (is.na(branch)) return(invisible(NULL))
    applicable <- c(
        .pkonfound_applicable$universal,
        .pkonfound_applicable[[branch]]
    )
    irrelevant <- setdiff(user_args, applicable)
    if (length(irrelevant) > 0) {
        message(sprintf(
            paste0("Note: argument(s) not applicable to %s and ignored: %s.\n",
                   "  See ?pkonfound for which arguments apply to each analysis."),
            branch,
            paste(irrelevant, collapse = ", ")
        ))
    }
    invisible(NULL)
}