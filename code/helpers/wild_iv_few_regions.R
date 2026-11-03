# Wang and Zhang (2024), Algorithm 1 / equations (7)-(11).
# https://arxiv.org/pdf/2108.13707
# One endogenous regressor and one excluded IV; exhaustive Rademacher signs.
# The SD of unrestricted-centred draws is descriptive bootstrap dispersion.
# Inference uses the null-imposed studentized bootstrap p-value, not SE stars.
wild_iv_few_regions <- function(sample, outcome, controls, reference_beta) {
  w <- model.matrix(as.formula(paste("~", controls)), sample)
  qw <- qr(w)
  residualize <- function(x) qr.resid(qw, x)
  d <- sample$IMTI
  y <- sample[[outcome]]
  z <- residualize(sample$E_is)
  group <- droplevels(factor(sample$region))
  groups <- nlevels(group)
  signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), groups)))

  # Scalar FWL IV slope and unadjusted cluster sandwich (paper equation 5).
  fit_iv <- function(y_value, d_value) {
    denominator <- sum(z * d_value)
    stopifnot(abs(denominator) > 1e-10)
    beta <- sum(z * y_value) / denominator
    error <- residualize(y_value - beta * d_value)
    scores <- rowsum(z * error, group, reorder = TRUE)
    se <- sqrt(sum(scores^2)) / abs(denominator)
    c(beta = beta, se = se)
  }
  original <- fit_iv(y, d)
  stopifnot(abs(original["beta"] - reference_beta) < 1e-9)
  u <- residualize(y - reference_beta * d)

  # Interacted residualized IVs preserve heterogeneous regional first stages.
  interacted_z <- model.matrix(~ group - 1) * as.numeric(z)
  augmented <- lm.fit(cbind(w, interacted_z, u), d)
  coefficients <- augmented$coefficients
  coefficients[is.na(coefficients)] <- 0
  first_stage_fit <- drop(cbind(w, interacted_z) %*%
    coefficients[seq_len(ncol(w) + groups)])
  v <- d - first_stage_fit

  draws <- lapply(seq_len(nrow(signs)), function(index) {
    weight <- signs[index, as.integer(group)]
    d_star <- first_stage_fit + weight * v
    # Test H0: beta=0 using null-restricted structural residuals.
    null_residual <- residualize(y)
    y_null <- y - null_residual + weight * null_residual
    null_fit <- fit_iv(y_null, d_star)
    # Centre at unrestricted beta solely to describe coefficient dispersion.
    y_unrestricted <- reference_beta * d_star +
      (y - reference_beta * d - u) + weight * u
    unrestricted_fit <- fit_iv(y_unrestricted, d_star)
    tibble::tibble(
      pattern = index, null_beta = null_fit["beta"],
      null_se = null_fit["se"],
      null_t = abs(null_fit["beta"] / null_fit["se"]),
      unrestricted_beta = unrestricted_fit["beta"]
    )
  }) |> dplyr::bind_rows()
  all_positive <- which(rowSums(signs == 1) == groups)
  stopifnot(abs(draws$null_beta[all_positive] - reference_beta) < 1e-9)
  observed_t <- abs(original["beta"] / original["se"])
  p <- mean(draws$null_t >= observed_t - 1e-10 * max(1, observed_t))
  dispersion <- sqrt(mean((draws$unrestricted_beta -
    mean(draws$unrestricted_beta))^2))
  list(
    stats = tibble::tibble(
      wild_boot_se = dispersion, wild_boot_p = p,
      wild_patterns = nrow(signs)
    ), draws = draws, signs = signs
  )
}
