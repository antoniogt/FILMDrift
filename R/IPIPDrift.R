#' Passive concept‑drift learner based on IPIP ensembles
#'
#' @description Executes the IPIP workflow but lets you specify column names so it works on any data set.
#'
#' @param data_list A list of data frames.
#' @param chunk_col Chunk‑identifier column.
#' @param target_col Response‑variable column.
#' @param positive_label Label of the positive class.
#' @param negative_label Label of the negative class.
#' @param prop_majority Majority‑class proportion in each balanced subsample.
#' @param save_dir Directory where models and results are written.
#' @param seed Random seed.
#' @param ... Extra arguments forwarded to `caret::train`.
#'
#' @return The prediction data frame (invisibly). Also saved as
#'   `results_IPIP_NSCD.rds` in `save_dir`.
#' @export
#'
#' @importFrom rlang sym
#' @import dplyr
#' @importFrom caret createDataPartition train trainControl
ipip_drift <- function(data_list,
                       chunk_col       = "chunk",
                       target_col      = "hosp",
                       positive_label  = "SI",
                       negative_label  = "NO",
                       prop_majority   = 0.55,
                       save_dir        = tempdir(),
                       seed            = 1234,
                       ...) {
  chunk_sym  <- rlang::sym(chunk_col)
  target_sym <- rlang::sym(target_col)

  chunk_vector <- pm_vector <- numeric(0)
  real <- pred <- real_prob <- pred_prob <- character(0)

  for (data_i in seq_along(data_list)) {
    data   <- data_list[[data_i]]
    chunks <- split(data, data[[chunk_col]])
    max_chunk <- length(chunks)

    for (t in 1:(max_chunk - 1)) {
      current_chunk <- chunks[[t]] %>% dplyr::select(-!!chunk_sym)

      set.seed(seed)
      train_id <- caret::createDataPartition(current_chunk[[target_col]], p = 0.80)
      train    <- current_chunk[train_id[[1]], ]
      test     <- current_chunk[-train_id[[1]], ]

      discharge <- train %>% dplyr::filter(!!target_sym == negative_label)
      expired   <- train %>% dplyr::filter(!!target_sym == positive_label)

      np <- round(nrow(expired) * 0.75)
      p  <- ceiling(log(.01) / (log(1 - 1 / nrow(expired)) * np))
      b  <- ceiling(log(.01) / (log(1 - 1 / np) * np))

      dfs <- vector("list", p)
      for (k in seq_len(p)) {
        set.seed(seed)
        id.exp <- sample(seq_len(nrow(expired)),   np, replace = TRUE)
        id.dis <- sample(seq_len(nrow(discharge)),
                         round(np * prop_majority / (1 - prop_majority)),
                         replace = TRUE)
        dfs[[k]] <- rbind(discharge[id.dis, ], expired[id.exp, ])
      }

      E <- vector("list", p)

      for (k in seq_len(p)) {
        Ek <- list()
        i  <- 0
        df <- dfs[[k]]

        while (length(Ek) <= b && i < mt(length(Ek))) {
          set.seed(seed)
          rf <- caret::train(
            x  = df %>% dplyr::select(-!!target_sym),
            y  = df[[target_col]],
            method     = "ranger",
            trControl  = caret::trainControl(
              summaryFunction = metrics,
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

          metrics.ensemble <- if (length(Ek) == 0) {
            setNames(-Inf, "BAL_ACC")
          } else {
            metrics(
              data.frame(
                obs  = test[[target_col]],
                pred = factor(
                  prediction(Ek,
                             test %>% dplyr::select(-!!target_sym),
                             positive_label = positive_label,
                             negative_label = negative_label),
                  levels = c(negative_label, positive_label)
                )
              )
            )
          }

          Ek[[length(Ek) + 1]] <- rf

          metrics.ensemble.2 <- metrics(
            data.frame(
              obs  = test[[target_col]],
              pred = factor(
                prediction(Ek,
                           test %>% dplyr::select(-!!target_sym),
                           positive_label = positive_label,
                           negative_label = negative_label),
                levels = c(negative_label, positive_label)
              )
            )
          )

          if (metrics.ensemble.2["BAL_ACC"] <= metrics.ensemble["BAL_ACC"]) {
            i <- i + 1
            Ek[[length(Ek)]] <- NULL
          } else {
            i <- 0
          }
        }
        E[[k]] <- Ek
      }

      ipip <- E

      next_chunk   <- chunks[[t + 1]] %>% dplyr::select(-!!chunk_sym)
      next_chunk_x <- next_chunk %>% dplyr::select(-!!target_sym)

      results_class <- factor(
        prediction.final(ipip,
                         next_chunk_x,
                         positive_label = positive_label,
                         negative_label = negative_label),
        levels = c(negative_label, positive_label)
      )
      results_prob  <- prediction.final.prob(ipip, next_chunk_x)

      real       <- c(real, next_chunk[[target_col]])
      pred       <- c(pred, results_class)
      pred_prob  <- c(pred_prob, results_prob)
      real_prob  <- c(real_prob,
                      as.numeric(dplyr::recode(next_chunk[[target_col]],
                                               !!negative_label := 1,
                                               !!positive_label := 0)))
      chunk_vector <- c(chunk_vector, rep(t, nrow(next_chunk)))
      pm_vector    <- c(pm_vector,   rep(prop_majority, nrow(next_chunk)))

      saveRDS(ipip,
              file = file.path(save_dir,
                               sprintf("IPIP_NSCD.chunk%s.rds", t)))
    }
  }

  df_final_pred <- data.frame(
    real       = real,
    pred       = pred,
    pred_prob  = pred_prob,
    real_prob  = real_prob,
    prop_maj   = pm_vector,
    chunk      = chunk_vector,
    stringsAsFactors = FALSE
  )
  saveRDS(df_final_pred,
          file = file.path(save_dir, "results_IPIP_NSCD.rds"))
  invisible(df_final_pred)
}
