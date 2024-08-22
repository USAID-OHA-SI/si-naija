#
# This is replica of the `LQAS Sampling Plan Calculator` based on Hypergeometric model
# SOURCE: https://brixtonhealth.com/hyperLQAS.html
#

break_while <- function(x, p) {
  x <- x
  while (x) {
    print(x)
    x <- x - 1
    if (x < p) break
  }
}

#' @title Round list element to 100%
#'
#'
equal_parts <- function(x, percent = T) {

  df_x <- tibble(value = x)

  if (percent != TRUE | all(x < 1) == TRUE) {
    df_x <- df_x %>%
      mutate(value = value * 100)
  }

  df_x <- df_x %>%
    mutate(flr = floor(value),
           ext = value - flr,
           rnd = round(value, 0),
           idx = row_number())

  t <- df_x %>% pull(rnd) %>% sum()
  r <- 0
  ad <- 1

  if (t < 100) {
    r <- 100 - t
    ad <- 1
  }
  else if (t > 100) {
    ad <- -1
  }
  else if (t == 100) {
    ad <- 0
  }

  df_x <- df_x %>%
    arrange(desc(ext)) %>%
    mutate(
      rnd = case_when(
        row_number() <= r ~ rnd + ad,
        TRUE ~ rnd
      )
    ) %>%
    arrange(idx)

  return(df_x$rnd)
}

#' @title Get Quantile
#'
#'
get_quantiles <- function(vals,
                          probs = seq(0, 1, 0.25),
                          labels = NULL) {

  breaks <- probs %>% map_dbl(~.x * 100) %>% paste0("Q", .)
  lbls <- ifelse(is.null(labels), c("<25", "25-50", "50-75", "75+"), labels)

  qqs <- vals %>%
    quantile(probs = probs) %>%
    as.list() %>%
    as.list() %>%
    as_tibble() %>%
    set_names(nm = breaks) %>%
    pivot_longer(cols = everything(), names_to = "breaks", values_to = "value")

  print(qqs)

  return(qqs)
}


#' @title Weighted distribution
#'
#' @param s   Sampled size to be distributed
#' @param wts Distribution weights
#'
distribute_sample <- function(s, wts) {
  #round(s * wts / sum(wts, na.rm = T))
  ds <- s * wts / sum(wts, na.rm = T)

  # Redistribute excess to highest decimal point
  ds_f <- floor(ds)
  ds_r <- round(ds)
  ds_d <- sum(ds_r) - s

  # Get the index of sorted values
  ds_oi <- ds %>%
    set_names(x = ., nm = 1:length(.)) %>%
    sort(decreasing = T) %>%
    names() %>%
    as.integer()

  # If there is access, redistribute it to the highest only
  if (ds_d > 0) {

    1:ds_d %>%
      walk(function(.x){
        ds_f[ds_oi[.x]] <<- ds_f[ds_oi[.x]] + 1
      })

    return(ds_f)
  }
  else {
    round(ds)
  }
}

#' @title Calculate the binomial coefficient
#' @note "n-choose-k" (given n and k)
#'
#' @param n Sample size
#' @param k Number of cases in the sample
#'
coef_nCk <- function(n, k) {
  # initiate params
  i = 0
  x = 1

  if (k == 0) return(1)

  for (.k in 1:k) {
    x <- x * (n - .k + 1) / .k
  }

  return(x)
}

#' @title Calculate hypergeometric probabilities
#'
#' @param k Number of cases in the sample
#' @param m Number of cases in the population
#' @param n Sample size
#' @param N Population size
#'
hyperProb <- function(k, m, n, N) {
  # Initiate params
  x = 0.0

  x = (coef_nCk(m, k) * coef_nCk(N - m, n - k)) / coef_nCk(N, n)

  return(x)
}

#' @title Calculate cumulative hypergeometric probabilities
#'
#' @param k Number of cases in the sample
#' @param m Number of cases in the population
#' @param n Sample size
#' @param N Population size
#' @param tail Tail, options are: 'lower' (default) or 'upper'
#'
hyperTail <- function (k, m, n, N, tail = 'lower') {
  # Initiate params
  i <- 0
  x <- 0.0

  # probability
  for(.k in 0:k) {
    x <- x + hyperProb(.k, m, n, N)
  }

  # Adjust for tail
  if(tail == 'upper') {
    x = 1.0 - x
  }

  return(x)
}

#' @title Find sampling plan for a pop size
#'
#' @param N Population size
#' @param upperP Upper threshold
#' @param lowerP Lower threshold
#' @param tAlpha Maximum tolerable alpha error
#' @param tBeta  Maximum tolerable beta error
#'
hyperPlan <- function(N,
                      upperP = .95,
                      lowerP = .85,
                      tAlpha = .05,
                      tBeta = .1) {

  usethis::ui_info("Sampling params: \nPop = {N} \nUpper Threshold = {upperP} \nLower threshold = {upperP} \nAlpha Error = {tAlpha} \nBeta Error = {tBeta}")

  # Validate entered data
  if(N < 1) {
    usethis::ui_stop('Population size is too small.')
  }

  if(lowerP < 0 | lowerP > 1) {
    usethis::ui_stop('Lower threshold must be between 0 and 1.')
  }

  if(upperP < 0 | upperP > 1) {
    usethis::ui_stop('Upper threshold must be between 0 and 1.')
  }

  if(upperP <= lowerP) {
    usethis::ui_stop('Upper threshold must be above Lower theshold.')
  }

  if(tAlpha < 0 | tAlpha > 1 | tBeta < 0 | tBeta > 1) {
    usethis::ui_stop('Errors must be between 0 and 1.')
  }

  # Case numbers in population at high and low prevalences

  H = round(upperP * N)
  L = round(lowerP * N)

  # Starting values for sampling plan

  d <- 0             # Acceptance number
  n <- d + 1         # Sample size
  oAlpha <- 1.0      # Alpha error for candidate sampling plan
  oBeta <- 1.0       # Beta error for candidate sampling plan

  # Search for a solution

  while(oAlpha > tAlpha & n < N) {

    while(oBeta <= tBeta & n < N) {

      n <- n + 1;
      oAlpha <- hyperTail(d, H, n, N, tail = 'lower')
      oBeta <- hyperTail(d, L, n, N, tail = 'upper')

      if (oAlpha <= tAlpha) {
        break
      }
    }

    if(oAlpha <= tAlpha & oBeta <= tBeta) {
      break
    }

    d <- d + 1

    oAlpha <- hyperTail(d, H, n, N, tail = 'lower')
    oBeta <- hyperTail(d, L, n, N, tail = 'upper')
  }

  usethis::ui_info("End values: \nd = {d}, n = {n}, oAlpha = {oAlpha}, oBeta = {oBeta}")

  # Report results
  # Check that we have a real result and report it

  res <- list(
    'pop' = N,
    'sample' = "NO PLAN FOUND",
    'dRule' = "NO PLAN FOUND",
    'aError' = "NO PLAN FOUND",
    'bError' = "NO PLAN FOUND"
  )

  if(n < N) {
    res$pop <- N
    res$sample <- n
    res$dRule <- d
    res$aError <- round(oAlpha, digits = 4)
    res$bError <- round(oBeta, digits = 4)
  }

  return(res)
}


## Test Cases - ICHSSA 1 @ AKwa Ibom

#sites <- 31
#ovc_serv = 151243

#hyperPlan(N = sites)
#hyperPlan(N = ovc_serv)
