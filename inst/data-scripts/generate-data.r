library("here")
library("purrr")
library("fitR")

exampleDir <- here::here("inst", "examples")
dataFiles <- list.files(exampleDir, pattern = "^example-")

safeSource <- purrr::safely(source)
purrr::walk(dataFiles, \(x) safeSource(file.path(exampleDir, x)))

models <- gsub("-", "_",
  sub("^example-(.+)\\.r", "\\1", dataFiles)
)

purrr::walk(models, \(x) save(
  list = x, file = here::here("data", paste0(x, ".rdata"))
))
