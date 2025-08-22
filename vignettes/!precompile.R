# vignettes that depend on Internet access need to be precompiled and take a
# while to run
library(knitr)
library(here)
library(devtools)

install() # ensure we're building with the latest version of the package

knit(
    input = "vignettes/small_plot_analysis.Rmd.orig",
    output = "vignettes/small_plot_analysis.Rmd"
)

purl(
    "vignettes/small_plot_analysis.Rmd.orig",
    output = "vignettes/small_plot_analysis.R"
)

# move image files
figs <-
    list.files(here("figure/"), pattern = ".png$", full.names = TRUE)
file.copy(
    from = figs,
    to = paste0(here("vignettes/"), basename(figs)),
    overwrite = TRUE
)
file.remove(figs)
file.remove(here("figure"))

# remove file path such that vignettes will build with figures
small_plot_analysis_replace <- readLines("vignettes/small_plot_analysis.Rmd")
small_plot_analysis_replace <- gsub(
    "\\(figure/",
    "\\(",
    small_plot_analysis_replace
)

small_plot_analysis_file_conn <- file("vignettes/small_plot_analysis.Rmd")
writeLines(small_plot_analysis_replace, small_plot_analysis_file_conn)
close(small_plot_analysis_file_conn)

build_vignettes()

# move resource files to /docs
resources <-
    list.files("vignettes/", pattern = ".png$", full.names = TRUE)
file.copy(
    from = resources,
    to = here("doc"),
    overwrite = TRUE
)
