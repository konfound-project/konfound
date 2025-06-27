# test_VAM: beta version of VAM function with exact output format and spacing
test_VAM <- function(
    est_eff,             # numeric: teacher's observed VAM
    replace_stu,         # numeric: hypothetical "average" student score
    n_obs,               # integer: number of students in the class
    eff_thr,             # numeric: effectiveness threshold
    peer_effect_pi = 0.5, # numeric in [0,0.5]: proportion of students exerting peer effects
    to_return
) {
  # 1) Input validation
  if (!is.numeric(est_eff)   || length(est_eff)   != 1) stop("est_eff must be a single numeric value.")
  if (!is.numeric(replace_stu) || length(replace_stu) != 1) stop("replace_stu must be a single numeric value.")
  if (!is.numeric(n_obs)     || length(n_obs)     != 1 || n_obs <= 0) stop("n_obs must be a single positive integer.")
  if (!is.numeric(eff_thr)   || length(eff_thr)   != 1) stop("eff_thr must be a single numeric value.")
  if (!is.numeric(peer_effect_pi) || length(peer_effect_pi) != 1 ||
      peer_effect_pi < 0 || peer_effect_pi > 0.5) stop("peer_effect_pi must be between 0 and 0.5.")
  if ((est_eff >= replace_stu) & (replace_stu > eff_thr)) stop("Undefined in this scenario because the resulting RIR_perc >= 1.")
  
  # 2) Replacement proportion π and direction
  below <- est_eff < eff_thr
  if (below) {
    pi_replace <- (eff_thr - est_eff) / (replace_stu - est_eff)
    direction_text <- "below"   # “VAM score is below the threshold”
    action_text    <- "increase" # “must replace … to increase the VAM above…”
    move_text      <- "above"
  } else {
    pi_replace <- (est_eff - eff_thr) / (replace_stu - eff_thr)
    direction_text <- "above"
    action_text    <- "reduce"
    move_text      <- "below"
  }
  rir_count <- round(n_obs * pi_replace)
  
  # 3) One unified peer‐effect value with sign
  raw_peer  <- if (below) (eff_thr - est_eff) / (n_obs * peer_effect_pi * (1 - peer_effect_pi)) 
  else                (est_eff  - eff_thr)/ (n_obs * peer_effect_pi * (1 - peer_effect_pi))
  peer_signed <- if (below) -abs(raw_peer) else abs(raw_peer)
  
  if (to_return == "print") {
  # 4) Header and narrative
  cat("This is beta version of the VAM function.\n")
  cat("\n")
  cat(sprintf(
    "The reported VAM score is %.3f with evaluation threshold of %.2f. ",
    est_eff, eff_thr
  ))
  cat(sprintf(
    "The VAM score is %s the threshold. RIR indicates replacement required to %s the VAM %s the threshold.\n",
    direction_text, action_text, move_text
  ))
  cat("\n")
  
  # 5) Replacement scenario summary
  if (below) {
      cat(sprintf(
          "If there are no peer effects, then %d (%d%%) students must be replaced with students whose score is %.2f (as specified) to move the VAM %s the threshold (RIR = %d%% * %d = %d).\n",
          rir_count, round(100*pi_replace), replace_stu, move_text,
          round(100*pi_replace), n_obs, rir_count
      ))
  } else {
      cat(sprintf(
          "If there are no peer effects, then %d (%d%%) students whose score is %.2f (as specified) must be replaced with students whose score is at the threshold level %.2f to move the VAM %s the threshold (RIR = %d%% * %d = %d).\n",
          rir_count, round(100*pi_replace), 
          replace_stu, eff_thr, 
          move_text,
          round(100*pi_replace), n_obs, rir_count
      ))
  }
  cat("\n")
  
  # 6) Peer‐effect scenario summary
  cat(sprintf(
    "If all of the bias comes from peer spillover effects, and we assume %d%% (as specified) students are %s the others, then a peer effect of %.3f is needed to change the evaluation. Each replaced student must have a %+.3f effect (compared to their replacements) on each of the non-replaced students to cross the threshold for evaluation.\n",
    round(100*peer_effect_pi),
    if (below) "distracting" else "supporting",
    abs(peer_signed),
    peer_signed
  ))
  cat("\n")
  
  # 7) Additional narrative
  # cat("Each replaced student had a ", sprintf("%.3f", abs(peer_signed)), 
  #    " effect (compared to their replacements) on each of the non-replaced students.\n", sep="")
  # cat("\n")
  cat("See the figure for combinations of size of peer effect by proportion to be replaced (pi) to change the evaluation. The red point marks the value reported above.\n")
  cat("\n")
  cat("The calculations and interpretation depend on your VAM model specification and estimation. See the paper for more details.\n\n")
  
  # 8) Plot with revised axis labels
  p_seq <- seq(0.01, 0.4999, length.out = 200)
  y_seq <- if (below) (eff_thr - est_eff) / (n_obs * p_seq * (1 - p_seq)) 
  else                (est_eff  - eff_thr)/ (n_obs * p_seq * (1 - p_seq))
  
  old_mar <- par("mar")
  par(mar = c(4,4,2,1))
  plot(
    p_seq, abs(y_seq), type = "l",
    xlab = "Proportion to be replaced",
    ylab = "Absolute value of the peer effect",
    main = "Size of peer effect by proportion to be replaced (pi)"
  )
  points(peer_effect_pi, abs(peer_signed), col = "red", pch = 19)
  par(mar = old_mar)
  
  invisible(NULL)
  } else if (to_return == "raw_output") {
      output <- list(
          RIR = rir_count,
          RIR_perc = pi_replace,
          peer_effect = peer_signed
      )
      return(output)
  }
}


