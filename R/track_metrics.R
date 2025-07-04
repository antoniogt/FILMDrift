#' Sequential metric tracking for IPIP results
#'
#' Produces cumulative performance metrics chunk by chunk for every unique
#' `p_min` value found in the supplied results data frame. Optionally runs the
#' per‑`p_min` computations in parallel.
#'
#' @param results_df   Data frame created by `ipip_drift`, with columns
#'   `real`, `pred`, `pred_prob`, `real_prob`, `p_min`, and `chunk`.
#' @param metric_names Character vector naming the metrics to keep from
#'   `metrics_fun`.
#' @param metrics_fun  Function that returns all metrics given a data frame
#'   with the columns `obs`, `pred`, `prob`, `obs.prob`. Defaults to
#'   `metricas.todas`.
#' @param parallel     Logical flag; if `TRUE`, use a PSOCK cluster.
#' @param n_clusters   Integer number of worker processes when `parallel = TRUE`.
#'
#' @return A data frame with cumulative metrics for every chunk and every
#'   `p_min` value.
#' @export
#'
#' @importFrom parallel makeCluster stopCluster parLapply
track_metrics <- function(results_df,
                          metric_names,
                          metrics_fun  = metricas.todas,
                          parallel     = FALSE,
                          n_clusters   = 2) {

  results_df$real <- factor(results_df$real, levels = c("NO", "SI"))
  results_df$pred <- factor(results_df$pred, levels = c("NO", "SI"))

  evaluate_one <- function(p_val) {
    subset_p <- results_df[results_df$p_min == p_val, ]
    subset_p <- subset_p[order(subset_p$chunk), ]

    base_rows   <- subset_p[subset_p$chunk == 1, ]
    follow_rows <- subset_p[subset_p$chunk > 1, ]

    metrics_tracking <- data.frame(
      t(as.data.frame(
        metrics_fun(data.frame(
          obs       = base_rows$real,
          pred      = base_rows$pred,
          prob      = base_rows$pred_prob,
          obs.prob  = base_rows$real_prob
        ))[metric_names]
      )),
      p_min = p_val,
      chunk = 1,
      row.names = NULL
    )

    for (z in seq_len(nrow(follow_rows))) {
      all_obs  <- c(base_rows$real,      follow_rows[seq_len(z), ]$real)
      all_pred <- c(base_rows$pred,      follow_rows[seq_len(z), ]$pred)
      all_prob <- c(base_rows$pred_prob, follow_rows[seq_len(z), ]$pred_prob)
      all_obs_prob <- c(base_rows$real_prob, follow_rows[seq_len(z), ]$real_prob)

      metrics_tracking <- rbind(
        metrics_tracking,
        cbind(
          t(as.data.frame(
            metrics_fun(data.frame(
              obs      = all_obs,
              pred     = all_pred,
              prob     = all_prob,
              obs.prob = all_obs_prob
            ))[metric_names]
          )),
          p_min = p_val,
          chunk = follow_rows[z, ]$chunk
        )
      )
    }
    metrics_tracking
  }

  unique_p <- unique(results_df$p_min)

  if (parallel) {
    cl <- parallel::makeCluster(n_clusters)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(
      cl,
      varlist = c("results_df", "metric_names", "metrics_fun", "evaluate_one"),
      envir   = environment()
    )

    result_list <- parallel::parLapply(cl, unique_p, function(x) evaluate_one(x))
  } else {
    result_list <- lapply(unique_p, evaluate_one)
  }

  do.call(rbind, result_list)
}
