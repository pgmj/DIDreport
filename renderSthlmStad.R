library(glue)
library(quarto)
library(purrr)
library(readxl)
library(dplyr)

### Credit to https://github.com/Pecners/sra_pullout/blob/main/render.R for inspiration

### Parallelization doesn't work with the settings (commented) below, but haven't experimented much.
# library(furrr)
# plan(multisession, workers = 8) # set up parallel processing, using 8 cpu cores
# furrr_options(seed = TRUE)
# then use future_walk() instead of walk()

# read file with parameters
DIDparams <- read_excel("DIDreportParameters.xls", sheet = 2)

walk(1:nrow(DIDparams), function(i) {
  this <- DIDparams[i,]

  outfile <- glue("DID_{this$fokusKommun}.html")

  quarto_render(input = "DIDreport3_2024sthlm.qmd",
                execute_params = list("fokusKommun" = this$fokusKommun,
                                      "jmfKommun" = this$jmfKommun %>% strsplit(", ") %>% unlist(),
                                      "years" = this$years %>% strsplit(",") %>% unlist() %>% as.numeric()
                                      ),
                output_file = outfile,
                output_format = "html")
})
