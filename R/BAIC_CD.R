#' BAIC index for sequential concept‑drift evaluation
#'
#' Computes the Balanced Adaptive Index of Concordance (BAIC, formerly UIC) for
#' every row of the cumulative‑metrics data frame returned by
#' \code{track_metrics()}.
#' For each chunk index \eqn{n}, metric weights are obtained by:
#'   1.  Calculating the Pearson correlation between each metric and
#'       \code{p_min} across all parameter values at that same \eqn{n}.
#'   2.  Mapping those correlations through a Gaussian kernel
#'       (\code{gauss_fun}).
#' The BAIC value of row \eqn{n_k} uses the weight vector from chunk
#' \eqn{n_k-1} (or chunk 1 when \eqn{n_k = 1}).
#'
#' @param tracking_df  Data frame produced by \code{track_metrics()}.
#' @param metric_names Character vector with the metric columns to include.
#' @param sigma        Standard deviation for the Gaussian kernel (default 0.15).
#' @param gauss_fun    Gaussian kernel; defaults to \code{FILM::f_gauss}.
#'
#' @return Numeric vector of BAIC values in the same order as \code{tracking_df}.
#' @export
#'
#' @import dplyr
BAIC_CD <- function(tracking_df,
                    metric_names,
                    sigma     = 0.15,
                    gauss_fun = FILM::f_gauss) {

  tracking_df <- tracking_df %>%
    dplyr::arrange(p_min, chunk) %>%
    dplyr::group_by(p_min) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    dplyr::ungroup()

  max_n <- max(tracking_df$n)

  #------------------------------------------------------------------
  # Step 1: build a weight matrix (rows = chunk index, cols = metrics)
  #------------------------------------------------------------------
  weight_list <- vector("list", max_n)

  for (i in seq_len(max_n)) {
    sub_i <- tracking_df %>%
      dplyr::filter(n == i) %>%
      dplyr::select(dplyr::all_of(c(metric_names, "p_min")))

    cor_mat   <- cor(sub_i, method = "pearson")
    cor_p_min <- as.double(cor_mat["p_min", metric_names])

    weight_i <- gauss_fun(cor_p_min, 1, 0, sigma)
    weight_list[[i]] <- weight_i
  }

  weights_df <- as.data.frame(do.call(rbind, weight_list))
  names(weights_df) <- metric_names
  weights_df$chunk  <- seq_len(max_n)

  #------------------------------------------------------------------
  # Step 2: compute BAIC for each row of tracking_df
  #------------------------------------------------------------------
  BAIC <- numeric(nrow(tracking_df))

  for (j in seq_len(nrow(tracking_df))) {
    row_n <- tracking_df$n[j]
    weight_row <- if (row_n == 1) {
      weights_df[weights_df$chunk == 1, metric_names]
    } else {
      weights_df[weights_df$chunk == (row_n - 1), metric_names]
    }
    BAIC[j] <- sum(as.double(tracking_df[j, metric_names]) *
                     as.double(weight_row))
  }

  BAIC
}
