library(bladdr)
library(mop)

mtt <- get_spectramax(
  "mtt/2022-09-20_upfl1-erda-pemi-gefi.txt",
  user = "aragaki-kai"
) |>
  read_spectramax()

usethis::use_data(mtt, overwrite = TRUE)
