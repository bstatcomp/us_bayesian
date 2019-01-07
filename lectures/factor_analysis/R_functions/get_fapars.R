get_fapars <- function (stan_mod, type = "median", par_names) {
  ext  <- extract(stan_mod)
  pars <- list()
  for (nm in par_names) {
    tmp                      <- ext[[nm]]
    pars[[length(pars) + 1]] <- apply(tmp, 2:length(dim(tmp)), type)
  }
  names(pars) <- par_names
  return (pars)
}