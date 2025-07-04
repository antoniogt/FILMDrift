#' Internal helper: balanced metrics
#' @keywords internal
metrics <- function(data,
                     positive_label = "SI",
                     negative_label = "NO",
                     lev = levels(as.factor(data$obs)),
                     model = NULL) {
  c(
    ACCURACY = MLmetrics::Accuracy(data[["pred"]], data[["obs"]]),
    SENS     = MLmetrics::Sensitivity(data[["pred"]], data[["obs"]],
                                      positive = positive_label),
    SPEC     = MLmetrics::Specificity(data[["pred"]], data[["obs"]],
                                      positive = positive_label),
    PPV      = MLmetrics::Precision(data[["pred"]], data[["obs"]],
                                    positive = positive_label),
    NPV      = MLmetrics::NPV(data[["pred"]], data[["obs"]],
                              positive = positive_label),
    KAPPA    = psych::cohen.kappa(cbind(data[["obs"]], data[["pred"]]))$kappa,
    BAL_ACC  = (MLmetrics::Sensitivity(data[["pred"]], data[["obs"]],
                                       positive = positive_label) +
                  MLmetrics::Specificity(data[["pred"]], data[["obs"]],
                                         positive = positive_label)) / 2,
    MCC      = mltools::mcc(data[["pred"]], data[["obs"]]),
    GEOM     = sqrt(
      MLmetrics::Sensitivity(data[["pred"]], data[["obs"]],
                             positive = positive_label) *
        MLmetrics::Specificity(data[["pred"]], data[["obs"]],
                               positive = positive_label)
    )
  )
}
