# Run from the repository root: Rscript temp/tests/check_table2_clustering.R
# Research regression checks against the existing Table 2 models and diagnostics.
suppressPackageStartupMessages({library(readr); library(dplyr)})
source('code/helpers/cluster_inference.R')
a <- read.csv('output/tables/tableII_mod_diagnostics.csv')
d <- read_csv('data/non_aa_samp.csv', show_col_types = FALSE) %>%
  mutate_if(is.character, as.factor) %>% filter(raw_lang != 0)
x <- paste('chsex + zbfa + stunting + caredu_r1 + careage_r1 + factor(caresex_r1)',
 '+ hhsize + wi_new + hq_new + cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3)')
stopifnot(identical(a$n, c(437L,279L,334L,192L)))
original_beta <- c(.0431234443600741,.0292047939479791,.0423263595234968,.0288607533982841)
stopifnot(max(abs(a$iv_beta - original_beta)) < 1e-12)
for (i in 1:4) {
  s <- if (i > 2) filter(d, region != 'Amhara') else d
  v <- all.vars(as.formula(paste(a$outcome[i], '~ IMTI + E_is +', x)))
  s <- s[complete.cases(s[,v]), ]
  f <- lfe::felm(as.formula(paste(a$outcome[i], '~',x,'|0|(IMTI~E_is)')),data=s)
  stopifnot(abs(tail(coef(f),1)-a$iv_beta[i])<1e-12)
  r <- table2_cluster_checks(s, a$outcome[i], x, a$iv_beta[i])
  # Permuting rows and relabelling clusters must not change inference.
  shuffled <- s[rev(seq_len(nrow(s))), ]
  shuffled$region <- factor(paste0('group_', match(shuffled$region, sort(unique(s$region)))))
  rr <- table2_cluster_checks(shuffled, a$outcome[i], x, a$iv_beta[i])
  stopifnot(isTRUE(all.equal(r$stats, rr$stats, tolerance=1e-7)))
  t <- a$iv_beta[i] / r$stats$iv_se_CR2
  stopifnot(abs(2*pt(-abs(t),r$stats$iv_df_CR2)-a$iv_p_CR2[i])<1e-10)
  # Independent enumeration from recorded region scores, including tied zeros.
  scores <- r$audit$null_score
  extremes <- 0L
  for (mask in 0:(2^length(scores)-1)) {
    weights <- ifelse(as.integer(intToBits(mask))[seq_along(scores)]==1,1,-1)
    extremes <- extremes + (abs(sum(weights*scores)) >= abs(sum(scores))-1e-10)
  }
  stopifnot(extremes/2^length(scores) == a$AR_p_sign_beta0[i],
            a$iv_ci_low_CR2[i]<0, a$iv_ci_high_CR2[i]>0,
            a$AR_p_CR2_beta0[i]>.05)
}
cat('PASS: original estimates/N; row-order/cluster-label invariance; Satterthwaite p-values; exhaustive sign enumeration; confidence intervals and AR conclusions.\n')
