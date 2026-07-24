# Stratified regression estimator (from author replication_final.Rmd)
strat_reg <- function(formula, data, stratum) {
  counts <- data |> dplyr::group_by({{ stratum }}) |> dplyr::summarize(N = dplyr::n(), .groups = "drop")

  stratum_regs <- data |>
    dplyr::group_by({{ stratum }}) |>
    dplyr::group_map(~ estimatr::lm_robust(formula = formula, data = .x))

  sample_sizes <- unlist(lapply(stratum_regs, function(x) x$nobs))
  sample_shares <- sample_sizes / sum(sample_sizes)

  point_est <- sapply(stratum_regs, function(x) x$coefficients)
  if (!is.matrix(point_est) && is.vector(point_est)) {
    point_est <- t(as.matrix(point_est))
  }
  if (sum(apply(point_est, 1, function(x) sum(is.na(x)))) > 0) {
    print(counts[apply(point_est, 2, function(x) sum(is.na(x))) != 0, , drop = FALSE])
    stop("Error: NAs in stratified point estimates")
  }

  stratum_dfs <- data |> dplyr::group_by({{ stratum }}) |> dplyr::group_split()
  var_est <- mapply(function(reg, df) {
    v <- abs(diag(stats::vcov(reg)))
    if (any(is.na(v))) {
      m <- stats::lm(formula, data = df)
      X <- stats::model.matrix(m)
      e <- stats::residuals(m)
      h <- stats::hatvalues(m)
      w <- e^2 / (1 - h)
      w[!is.finite(w)] <- 0
      bread <- solve(crossprod(X))
      v <- abs(diag(bread %*% (t(X) %*% (X * w)) %*% bread))
    }
    v
  }, stratum_regs, stratum_dfs)

  if (!is.matrix(var_est) && is.vector(var_est)) {
    var_est <- t(as.matrix(var_est))
  }
  if (sum(apply(var_est, 1, function(x) sum(is.na(x)))) > 0) {
    print(counts[apply(var_est, 2, function(x) sum(is.na(x))) != 0, , drop = FALSE])
    stop("Error: NAs in stratified variance estimates")
  }

  point_combined <- apply(point_est, 1, function(x) sum(x * sample_shares))
  se_combined <- apply(var_est, 1, function(x) sqrt(sum(x * sample_shares^2)))

  char_names <- if (length(point_combined) < 2) c("(Mean)") else names(point_combined)

  out_results <- data.frame(
    term = as.character(char_names),
    estimate = point_combined,
    std.error = se_combined,
    stringsAsFactors = FALSE
  )
  out_results$statistic <- out_results$estimate / out_results$std.error
  out_results$conf.low <- out_results$estimate - stats::qnorm(.975) * out_results$std.error
  out_results$conf.high <- out_results$estimate + stats::qnorm(.975) * out_results$std.error
  out_results
}
