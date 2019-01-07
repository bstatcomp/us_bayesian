get_stanmod <- function (mod_name) {
  mdl_file   <- paste("./stan_models/", mod_name, ".stan", sep = "")
  obj_file   <- paste("./stan_models/", mod_name, ".rds", sep = "")
  if (file.exists(obj_file)) {
    model <- readRDS(obj_file)
  } else {
    model <- stan_model(file = mdl_file)
    saveRDS(model, file = obj_file)
  }
  return (model)
}