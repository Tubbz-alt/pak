
local({
  # This is not set when pkgload::load_all() runs
  pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")
  if (pkgdir != "") {
    lib <- file.path(pkgdir, "library")
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    install.packages("callr", lib = lib, quiet = TRUE)
  }
})
