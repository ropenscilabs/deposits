# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    if (!"atom4R" %in% search ()) {
        attachNamespace("atom4R")
    }
}
# nocov end
