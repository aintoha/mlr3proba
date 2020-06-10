meas_ill = function(alive, distribution, unique_times, eps) {
  # if a patient is alive at t then find the survival, otherwise find cdf
  surv = transpose(1 - distribution$cdf(unique_times))
  ll = (surv * alive) + ((1 - surv) * (1 - alive))
  # set prediction to be very small but non-zero then find negative log
  ll[ll == 0] = eps

  -log(ll)
}

meas_ibs = function(alive, distribution, unique_times) {
  # ibs at time t* as G(t*) = (I(t > t*) - S(t*))^2
  (alive - transpose(1 - distribution$cdf(unique_times)))^2
}

regr_logloss = function(truth, distribution, times, ...) {
  prob_score(truth, distribution, times, meas_ill, eps = eps, weight = FALSE)
}

regr_ibs = function(truth, distribution, times, ...) {
  prob_score(truth, distribution, times, meas_ibs, weight = FALSE)
}

weighted_logloss = function(truth, distribution, times, eps = 1e-15, ...) {
  prob_score(truth, distribution, times, meas_ill, eps = eps)
}

weighted_graf = function(truth, distribution, times, ...) {
  prob_score(truth, distribution, times, meas_ibs)
}

integrated_score = function(score, integrated, method) {
  if (ncol(score) == 1) {
    integrated = FALSE
  }

  if (integrated) {
    if (method == 1) {
      return(mean(as.numeric(score), na.rm = TRUE))
    } else if (method == 2) {
      times = as.numeric(colnames(score))
      lt = ncol(score)
      score = as.numeric(colMeans(score, na.rm = TRUE))
      return((diff(times) %*% (score[1:(lt - 1)] + score[2:lt])) / (2 * (max(times) - min(times))))
    }
  } else {
    return(colMeans(score, na.rm = TRUE))
  }
}

integrated_se = function(score, integrated) {
  if (integrated) {
    return(sqrt(sum(stats::cov(score)) / (nrow(score) * ncol(score)^2)))
  } else {
    return(apply(score, 2, function(x) stats::sd(x) / sqrt(nrow(score))))
  }
}

