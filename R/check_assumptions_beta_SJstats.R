#this is from github, a newer version of check_assumptions used within the assumptions function of trafo package.
#retrieved from https://github.com/strengejacke/sjstats/commit/33395ecd2d3e1c18a2dfe4c270186496f0cd353a

check_assumptions <- function(x, as.logical = FALSE) {
  # first, check outliers
  models <- suppressMessages(outliers(x))
  
  if (is.null(models))
    result <- tibble::tibble()
  else
    result <- models$result
  
  # check assumptions for original
  hn <- suppressMessages(heteroskedastic(x)$heteroskedastic)
  mn <- suppressMessages(multicollin(x)$multicollin)
  nn <- suppressMessages(normality(x)$non.normality)
  an <- suppressMessages(autocorrelation(x)$autocorrelation)
  
  # check whether user wants p-values or logical values
  if (as.logical) {
    hn <- hn < 0.05
    nn <- nn < 0.05
    an <- an < 0.05
  }
  
  # if we have an updated model w/o outliers, check assumptions for
  # this model as well
  if (!is.null(models)) {
    hu <- suppressMessages(heteroskedastic(models$updated.model)$heteroskedastic)
    mu <- suppressMessages(multicollin(models$updated.model)$multicollin)
    nu <- suppressMessages(normality(models$updated.model)$non.normality)
    au <- suppressMessages(autocorrelation(models$updated.model)$autocorrelation)
    
    # check whether user wants p-values or logical values
    if (as.logical) {
      hu <- hu < 0.05
      nu <- nu < 0.05
      au <- au < 0.05
    }
    
    result <- tibble::add_column(
      result,
      heteroskedasticity = c(hn, hu),
      multicollinearity = c(mn, mu),
      non.normal.resid = c(nn, nu),
      autocorrelation = c(an, au)
    )
  } else {
    result <- tibble::tibble(
      heteroskedasticity = hn,
      multicollinearity = mn,
      non.normal.resid = nn,
      autocorrelation = an
    )
  }
  
  result
}


environment(check_assumptions)<-asNamespace("sjstats")
