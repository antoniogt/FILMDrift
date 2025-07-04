# FILMDrift

<!-- badges: start -->
<!-- badges: end -->

**FILMDrift** brings the FILM philosophy to **chunked data**.  
It implements the Identical Partitions for Imbalance Problems (IPIP) (See https://arxiv.org/abs/2503.04370) ensemble for passive concept‑drift learning, together with utilities to monitor performance and to update models as new chunks arrive.

The package have the following main functions:

* **`ipip_drift()`** – fit an IPIP ensemble sequentially, chunk by chunk.  
* **`track_metrics()`** – build a cumulative metric table that grows with every processed observation.  
* **`BAIC_CD()`** – compress that table into a single drift‑aware score
  (Balanced Adaptive Index of Concordance).  
* **`update_ipip_cd()`** – inject a new chunk into an existing IPIP ensemble, retaining only the best sub‑models.

---

## Installation

``` r
# install.packages("devtools")  # if you don't have it
devtools::install_github("antoniogt/FILMDrift")
library(FILMDrift)
```

## Prerequisites
You need the following R packages:

- rlang
- dplyr
- caret
- ranger
- MLmetrics
- mltools
- psych
- parallel

## Quick workflow
Below is an end‑to‑end example.
Assume each element of stream_list is a dataframe with a chunk column that labels the batch and a binary target called hosp.

``` r
## 1 Initial training

model_out <- ipip_drift(
  data_list  = stream_list,
  chunk_col  = "chunk",
  target_col = "hosp"
)

## 2 Cumulative metrics (parallel, 4 workers)

metric_tbl <- track_metrics(
  results_df   = model_out,
  metric_names = c("BAL_ACC", "SENS", "SPEC"),
  parallel     = TRUE,
  n_clusters   = 4
)

## 3 Compute BAIC 

baic_vec <- BAIC_CD(
  tracking_df  = metric_tbl,
  metric_names = c("BAL_ACC", "SENS", "SPEC")
)

## 4 Online update when a new chunk arrives

model_out$ensemble <- update_ipip_cd(
  ipip         = model_out$ensemble,
  new_chunk    = newest_chunk,
  chunk_col    = "chunk",
  target_col   = "hosp"
)
```

## Function overview

- _ipip_drift()_ creates random‑forest ensemble, one per balanced bootstrap (See https://arxiv.org/abs/2503.04370), and stores predictions for every processed chunk we can reuse them.

- _track_metrics()_ process those predictions in chronological order, updating standard metrics each time a new ground‑truth label becomes available. It can be used in parallel.

- _BAIC_CD()_ compresses the cumulative metric table into a single drift‑aware score per observation. Metric weights derive from their Pearson correlation with p<sub>min</sub> and are passed through a Gaussian kernel to down‑weight biased metrics with the proportion of the minority class.

- _update_ipip_cd()_ retrains ipip on an incoming chunk and then selects only the top ensemble performers, keeping model size bounded while adapting to new data.

## About
FILMDrift was developed by Antonio Guillén-Teruel and the CALM research group (Universidad de Murcia, https://www.um.es/web/aike/calm) to bring the FILM methodology to non‑stationary chunk-based problems.
