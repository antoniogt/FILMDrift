#' Selects the top‑p model indices by a metric
#' @export
best_models <- function(model, metric_max, metrics, x, y, p,
                        positive_label = "SI", negative_label = "NO") {
  model_performance <- numeric(length(model))
  for (j in seq_along(model)) {
    results_class <- as.factor(FILM::predict_film(ensemble = model[[j]], x = x, y = y, type = "class"))
    results_prob  <- FILM::predict_film(ensemble = model[[j]], x = x, y = y, type = "prob")
    results <- FILM::metric_probs(data.frame(
      obs      = as.factor(y),
      pred     = results_class,
      prob     = results_prob,
      obs.prob = as.numeric(dplyr::recode(as.factor(y),
                                          !!negative_label := 1,
                                          !!positive_label := 0))
    ))[metrics]
    model_performance[j] <- as.double(results[metric_max])
  }
  order(model_performance, decreasing = TRUE)[seq_len(p)]
}
