#' Update an existing IPIP ensemble with a new data chunk
#'
#' Trains fresh sub‑ensembles on the incoming chunk and merges them with the
#' current ensemble, optionally pruning to the top‑`p_keep` performers.
#'
#' @param ipip           List of ensembles produced by \code{ipip_drift()}.
#' @param new_chunk      Data frame containing the next chunk.
#' @param chunk_col      Column that identifies the chunk (set \code{NULL} if absent).
#' @param target_col     Response‑variable column.
#' @param positive_label Positive‑class label.
#' @param negative_label Negative‑class label.
#' @param prop_majority  Majority‑class proportion for balanced subsampling.
#' @param metric_max     Metric used for best‑model ranking.
#' @param metrics        Character vector of metrics to feed into
#'   \code{best_models()}.
#' @param p_keep         Number of ensembles to retain after merging.  Defaults
#'   to the size of the incoming batch.
#' @param seed           Random seed.
#' @param ...            Extra arguments forwarded to \code{caret::train}.
#'
#' @return The updated IPIP ensemble (list of ensembles).
#' @export
#'
#' @import dplyr
#' @importFrom caret createDataPartition train trainControl
update_ipip_cd <- function(ipip,
                           new_chunk,
                           chunk_col       = NULL,
                           target_col      = "hosp",
                           positive_label  = "SI",
                           negative_label  = "NO",
                           prop_majority   = 0.55,
                           metric_max      = "BAL_ACC",
                           metrics         = c("BAL_ACC"),
                           p_keep          = NULL,
                           seed            = 1234,
                           ...) {

  if (!is.null(chunk_col))
    new_chunk <- new_chunk %>% dplyr::select(-all_of(chunk_col))

  set.seed(seed)
  idx <- caret::createDataPartition(new_chunk[[target_col]], p = 0.80)
  train_set <- new_chunk[idx[[1]], ]
  test_set  <- new_chunk[-idx[[1]], ]

  discharge <- dplyr::filter(train_set, .data[[target_col]] == negative_label)
  expired   <- dplyr::filter(train_set, .data[[target_col]] == positive_label)

  np <- round(nrow(expired) * 0.75)
  p  <- ceiling(log(.01) / (log(1 - 1 / nrow(expired)) * np))
  b  <- ceiling(log(.01) / (log(1 - 1 / np) * np))

  mt_local <- function(n) ceiling((b - n) / 3)

  dfs <- vector("list", p)
  for (k in seq_len(p)) {
    set.seed(seed)
    id_exp <- sample(seq_len(nrow(expired)),   np, replace = TRUE)
    id_dis <- sample(seq_len(nrow(discharge)),
                     round(np * prop_majority / (1 - prop_majority)),
                     replace = TRUE)
    dfs[[k]] <- rbind(discharge[id_dis, ], expired[id_exp, ])
  }

  new_ensembles <- vector("list", p)

  for (k in seq_len(p)) {
    Ek <- list()
    i  <- 0
    df <- dfs[[k]]

    while (length(Ek) <= b && i < mt_local(length(Ek))) {
      set.seed(seed)
      rf <- caret::train(
        x = df %>% dplyr::select(-all_of(target_col)),
        y = df[[target_col]],
        method     = "ranger",
        trControl  = caret::trainControl(
          summaryFunction = metricas,
          method          = "cv",
          number          = 10,
          classProbs      = TRUE,
          allowParallel   = TRUE
        ),
        metric     = "BAL_ACC",
        maximize   = TRUE,
        num.trees  = 200,
        ...
      )

      metric_prev <- if (length(Ek) == 0) {
        setNames(-Inf, "BAL_ACC")
      } else {
        metricas(
          data.frame(
            obs  = test_set[[target_col]],
            pred = factor(
              prediccion(Ek,
                         test_set %>%
                           dplyr::select(-all_of(target_col)),
                         positive_label = positive_label,
                         negative_label = negative_label),
              levels = c(negative_label, positive_label)
            )
          )
        )
      }

      Ek[[length(Ek) + 1]] <- rf

      metric_new <- metricas(
        data.frame(
          obs  = test_set[[target_col]],
          pred = factor(
            prediccion(Ek,
                       test_set %>% dplyr::select(-all_of(target_col)),
                       positive_label = positive_label,
                       negative_label = negative_label),
            levels = c(negative_label, positive_label)
          )
        )
      )

      if (metric_new["BAL_ACC"] <= metric_prev["BAL_ACC"]) {
        i <- i + 1
        Ek[[length(Ek)]] <- NULL
      } else {
        i <- 0
      }
    }
    new_ensembles[[k]] <- Ek
  }

  merged <- c(ipip, new_ensembles)

  if (is.null(p_keep)) p_keep <- length(ipip)

  best_idx <- best_models(
    model       = merged,
    metric_max  = metric_max,
    metrics     = metrics,
    x           = test_set %>% dplyr::select(-all_of(target_col)),
    y           = test_set[[target_col]],
    p           = p_keep,
    positive_label = positive_label,
    negative_label = negative_label
  )

  merged[best_idx]
}
