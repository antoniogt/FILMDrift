#' Internal helper: majority vote for one ensemble
#' @keywords internal
prediction <- function(ensemble, x, q = 0.75,
                       positive_label = "SI",
                       negative_label = "NO") {
  pred <- matrix(nrow = nrow(x), ncol = 0)
  for (modelo in ensemble) pred <- cbind(pred, predict(modelo, x))
  p_neg <- apply(pred, 1, function(z) prop.table(table(z))[negative_label])
  ifelse(is.na(p_neg) | p_neg < q, positive_label, negative_label)
}

#' Internal helper: probability prediction for one ensemble
#' @keywords internal
prediction_prob <- function(ensemble, x, q = 0.75) {
  pred <- data.frame(matrix(nrow = nrow(x), ncol = 0))
  for (modelo in ensemble) pred <- cbind(pred, predict(modelo, x, type = "prob")$NO)
  rowSums(pred)
}

#' Internal helper: final class prediction for a list of ensembles
#' @keywords internal
prediction.final <- function(ensemble, x, q = 0.5) {
  pred <- as.data.frame(lapply(ensemble, function(e) prediction(e, x)))
  pred <- apply(pred, 1, function(z) prop.table(table(z))["NO"])
  ifelse(is.na(pred) | pred < q, "SI", "NO")
}

#' Internal helper: final probability prediction for a list of ensembles
#' @keywords internal
prediction.final.prob <- function(ensemble, x, q = 0.5) {
  pred <- as.data.frame(lapply(ensemble, function(e) prediction_prob(e, x)))
  pred <- rowSums(pred) / sum(unlist(lapply(ensemble, length)))
  pred
}
