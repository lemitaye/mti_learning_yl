# Small-G checks for Table 2; sourced locally by 07_table_main.R.
# CR2/Satterthwaite: https://jepusto.github.io/clubSandwich/
# Wild AR score sign changes: Wang & Zhang (2024), section 2.3.2 / Theorem 6.1
# https://arxiv.org/pdf/2108.13707
# No method here guarantees exact finite-sample inference with 3--4 regions.
table2_cluster_checks <- function(sample, outcome, controls, reference_beta) {
  group <- droplevels(factor(sample$region))
  G <- nlevels(group)
  stopifnot(G %in% c(3L, 4L), !anyNA(group))
  iv <- AER::ivreg(as.formula(paste(outcome, '~ IMTI +', controls,
                                    '| E_is +', controls)), data = sample)
  stopifnot(abs(coef(iv)['IMTI'] - reference_beta) < 1e-9)
  test <- function(model, coefficient) {
    clubSandwich::coef_test(model, vcov = 'CR2', cluster = group,
                            test = 'Satterthwaite', coefs = coefficient)
  }
  ci <- test(iv, 'IMTI')
  fs <- lm(as.formula(paste('IMTI ~ E_is +', controls)), data = sample)
  cf <- test(fs, 'E_is')
  ar <- lm(as.formula(paste(outcome, '~ E_is +', controls)), data = sample)
  ca <- test(ar, 'E_is')
  # Weak-IV robust score test of beta=0. Remove controls from the instrument
  # separately in each region, ensuring the nuisance-orthogonality condition
  # within every cluster (Wang-Zhang Assumption 1(i)). This changes the AR test
  # moments, NOT the reported 2SLS estimator: it discards between-region variation.
  # It requires conditional instrument exogeneity within regions, independent
  # regions and approximately symmetric/CLT cluster scores. It does not permit
  # unrestricted common regional factor shocks or repair the exclusion restriction.
  X <- model.matrix(as.formula(paste('~', controls)), data = sample)
  z_local <- numeric(nrow(sample))
  for (g in levels(group)) {
    idx <- which(group == g)
    z_local[idx] <- lm.fit(X[idx, , drop = FALSE], sample$E_is[idx])$residuals
    stopifnot(max(abs(crossprod(X[idx, , drop = FALSE], z_local[idx]))) < 1e-7)
  }
  u0 <- lm.fit(X, sample[[outcome]])$residuals
  scores <- as.numeric(rowsum(z_local * u0, group, reorder = TRUE))
  observed <- abs(sum(scores))
  signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), G)))
  bootstrap <- abs(drop(signs %*% scores))
  # Exhaustive enumeration includes both all-positive and all-negative patterns.
  # Include ties with a scale-relative tolerance; no Monte Carlo +1 adjustment.
  p_sign <- mean(bootstrap >= observed - 1e-10 * max(1, observed))
  stopifnot(p_sign >= 2 / 2^G)
  informative <- as.numeric(rowsum(z_local^2, group)) > 1e-12
  # Tigray has constant E_is here: local orthogonalization leaves no signal.
  # Zero-information clusters duplicate patterns and further coarsen p-values.
  effective_min <- 2 / 2^sum(informative)
  audit <- data.frame(region = levels(group), n = as.integer(table(group)),
                      instrument_residual_SS = as.numeric(rowsum(z_local^2, group)),
                      null_score = scores)
  list(stats = data.frame(
    clusters = G, iv_se_CR2 = ci$SE, iv_df_CR2 = ci$df_Satt,
    iv_p_CR2 = ci$p_Satt,
    iv_ci_low_CR2 = ci$beta - qt(.975, ci$df_Satt) * ci$SE,
    iv_ci_high_CR2 = ci$beta + qt(.975, ci$df_Satt) * ci$SE,
    first_stage_F_CR2 = cf$tstat^2, first_stage_df_CR2 = cf$df_Satt,
    first_stage_p_CR2 = cf$p_Satt,
    AR_p_CR2_beta0 = ca$p_Satt, AR_df_CR2 = ca$df_Satt,
    AR_p_sign_beta0 = p_sign, sign_patterns = nrow(signs),
    sign_p_min = 2 / 2^G, informative_regions = sum(informative),
    sign_p_min_effective = effective_min), audit = audit)
}
