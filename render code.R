
library(rmarkdown)
rmarkdown::render('./Vignette covid data.Rmd',
                  output_format = 'github_document',
                  output_file = 'README.md',
                  output_dir = './',
                  output_options = list(
                    df_print = 'default',
                    html_preview = FALSE # to remove .html file creation
                  )
)
