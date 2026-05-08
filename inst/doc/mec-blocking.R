## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(automatedRecLin)
library(data.table)

options("text2vec.mc.cores" = 1L)

## ----data---------------------------------------------------------------------
data("census", package = "automatedRecLin")
data("cis", package = "automatedRecLin")
setDT(census)
setDT(cis)

NROW(cis)
NROW(census)

## ----true-matches-------------------------------------------------------------
cis[is.na(cis)] <- ""
census[is.na(census)] <- ""

cis[, pername1 := gsub("-", "", pername1)]
census[, pername1 := gsub("-", "", pername1)]

true_matches <- merge(
  x = cis[, .(a = .I, person_id)],
  y = census[, .(b = .I, person_id)],
  by = "person_id"
)[, .(a, b)]

NROW(true_matches)

## ----model-specification------------------------------------------------------
variables <- c(
  "pername1", "pername2", "sex",
  "dob_day", "dob_mon", "dob_year"
)

comparators <- list(
  "pername1" = jarowinkler_complement(),
  "pername2" = jarowinkler_complement()
)

methods <- list(
  "pername1" = "continuous_parametric",
  "pername2" = "continuous_parametric"
)

blocking_variables <- c(variables, "enumcap", "enumpc")

## ----mec-blocking-------------------------------------------------------------
set.seed(1)

result <- mec_blocking(
  A = cis,
  B = census,
  variables = variables,
  comparators = comparators,
  methods = methods,
  blocking_variables = blocking_variables,
  blocking_sep = "",
  controls_blocking = list(seed = 1, n_threads = 1),
  min_training_pairs = 1000,
  min_training_nonmatches = 1000,
  block_sampling_seed = 1,
  nonmatch_sample_size = 100000,
  nonmatch_sampling_seed = 1,
  true_matches = true_matches
)

result

## ----results, echo = FALSE----------------------------------------------------
data.table(
  step = c("Training", "Blocking", "Linkage"),
  result = c(
    paste0(
      result$training_rule, " on ",
      format(NROW(result$training_blocks), big.mark = ","),
      " blocks"
    ),
    paste0(
      format(result$blocking_eval[["preserved_matches"]], big.mark = ","),
      " of ",
      format(result$blocking_eval[["true_matches"]], big.mark = ","),
      " known links retained"
    ),
    paste0(
      "FLR = ", sprintf("%.2f%%", 100 * result$eval_metrics[["FLR"]]),
      "; MMR = ", sprintf("%.2f%%", 100 * result$eval_metrics[["MMR"]])
    )
  )
)

