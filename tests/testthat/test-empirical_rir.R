context("Checking empirical RIR pipeline")

# Shared setup. mtcars (n=32) keeps tests fast.
# wt has a large, clearly significant negative coefficient in this model.
m_full  <- lm(mpg ~ wt + cyl, data = mtcars, model = TRUE)
m_no_mf <- lm(mpg ~ wt + cyl, data = mtcars, model = FALSE)

# ============================================================================
# refit_like()
# Correctness contract: refit on the same data must reproduce the original
# coefficients exactly. If this drifts, every downstream numeric result is
# suspect because almost all replacements go through this path.
# ============================================================================

test_that("refit_like() reproduces the same coefficients as a direct lm() call", {
    mf   <- m_full$model
    fit2 <- refit_like(m_full, formula = formula(m_full), data = mf)
    expect_equal(coef(fit2), coef(m_full), tolerance = 1e-10)
})

# ============================================================================
# prep_empirical_rir()
# Dimension contract: r_x, r_y, pred_red, res_red, hat, cooks must all be
# length n. Almost every downstream function indexes into these by position,
# so a length mismatch produces silent wrong results rather than an error.
# ============================================================================

test_that("prep_empirical_rir() vectors are all length n", {
    prep <- prep_empirical_rir(m_full, "wt")
    n <- nrow(mtcars)
    expect_equal(prep$n,             n)
    expect_length(prep$r_x,          n)
    expect_length(prep$r_y,          n)
    expect_length(prep$pred_red,     n)
    expect_length(prep$res_red,      n)
    expect_length(prep$hat,          n)
    expect_length(prep$cooks,        n)
})

test_that("prep_empirical_rir() errors when model frame is absent", {
    expect_error(
        prep_empirical_rir(m_no_mf, "wt"),
        regexp = "model=TRUE"
    )
})

# ============================================================================
# label_cases()
# ci_share is the normalization used in every composition-tracking fraction
# (frac_supporter, frac_high_infl, etc.). If it does not sum to 1, all those
# fractions are silently wrong and the tracker output is meaningless.
# ============================================================================

test_that("label_cases() ci_share sums to 1", {
    info <- label_cases(m_full, focal_var = "wt")
    expect_equal(sum(info$ci_share), 1, tolerance = 1e-10)
})

test_that("label_cases() vectors have length n and label is a 4-level factor", {
    info <- label_cases(m_full, focal_var = "wt")
    n <- nrow(mtcars)
    expect_length(info$ci_share, n)
    expect_length(info$label,    n)
    expect_s3_class(info$label, "factor")
    expect_equal(nlevels(info$label), 4L)
})

# ============================================================================
# find_k_star()
# Bounded-output contract: k_star must lie in [0, n]. This is the core
# invariant of the binary search. A value outside this range means the
# search overshot and the distribution is invalid.
# ============================================================================

test_that("find_k_star() k_star is finite and in [0, n]", {
    set.seed(7)
    prep <- prep_empirical_rir(m_full, "wt")
    n    <- prep$n
    y0   <- prep$data[[prep$y_name]]
    
    stat_obs <- abs(coef(m_full)[["wt"]]) /
        summary(m_full)$coefficients["wt", "Std. Error"]
    crit     <- qt(0.975, df = m_full$df.residual)
    k_cf_local <- as.integer(round((1 - crit / stat_obs) * n))
    
    res <- find_k_star(
        prep,
        k_cf               = k_cf_local,
        index_order        = sample.int(n),
        replacement_y_full = generate_replacement_y(prep, seq_len(n), y0)
    )
    
    expect_true(is.finite(res$k_star))
    expect_gte(res$k_star, 0L)
    expect_lte(res$k_star, n)
})

# ============================================================================
# konfound_empdist()
# Reproducibility: if the same seed does not reproduce k_star, the randomness
# plumbing is broken and no result from this function can be trusted.
# ============================================================================

test_that("konfound_empdist() returns a konfound_empdist object", {
    out <- konfound_empdist(m_full, "wt",
                            reps = 50, method = "search",
                            seed = 42, progress = FALSE)
    expect_s3_class(out, "konfound_empdist")
})

test_that("konfound_empdist() same seed gives identical k_star vector", {
    out1 <- konfound_empdist(m_full, "wt",
                             reps = 30, method = "search",
                             seed = 99, progress = FALSE)
    out2 <- konfound_empdist(m_full, "wt",
                             reps = 30, method = "search",
                             seed = 99, progress = FALSE)
    expect_equal(out1$k_star, out2$k_star)
})

# Non-significant guard: a non-significant model has no valid k_cf anchor,
# so the search loop would silently produce garbage. The error is the only
# protection -- there is no fallback.
test_that("konfound_empdist() errors when effect is not significant", {
    set.seed(1)
    d    <- data.frame(y = rnorm(50), x = rnorm(50))
    m_ns <- lm(y ~ x, data = d, model = TRUE)
    if (summary(m_ns)$coefficients["x", "Pr(>|t|)"] >= 0.05) {
        expect_error(
            konfound_empdist(m_ns, "x", reps = 10, method = "search"),
            regexp = "not currently significant"
        )
    } else {
        skip("Randomly generated data happened to be significant.")
    }
})