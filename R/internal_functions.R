is_asreml_installed <- function() {
    if (!requireNamespace("asreml", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg asreml} is required but not installed.",
        )
    }
    return(TRUE)
}
