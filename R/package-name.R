
#' @export

pkg_name_check <- function(
    nm, checks = c("valid", "cran", "bioc", "github", "bad-word",
    "abbrev", "wikipedia", "wiktionary", "urban-dict", "sentiment")) {

  remote(
    function(...) {
      get("pkg_name_check_internal", asNamespace("pak"))(...)
    },
    list(nm = nm, checks = checks)
  )
}

pkg_name_check_internal <- function(
    nm, checks = c("valid", "cran", "bioc", "github", "bad-word",
    "abbrev", "wikipedia", "wiktionary", "urban-dict", "sentiment")) {

  bad <- ! checks %in% names(pkg_name_check_functions)
  if (any(bad)) {
    stop("Unknown package name checks: ", paste0(nm[bad], collapse = ", "))
  }

  todo <- pkg_name_check_functions[checks]

  done <- 0
  tick <- function() {
    done <<- done + 1
    cat(done, "/", length(todo), "is done\n")
  }

  ans <- synchronize({
    pms <- lapply(todo, function(fun) {
      async_timeout(function() fun(nm), 10)$
        catch(error = function(err) list(err, attr(fun, "msg")))$
        finally(function() tick())
    })
    when_all(.list = pms)
  })

  attr(ans, "package") <- nm
  pkg_name_check_print(ans)
  
  invisible(ans)
}

pkg_name_check_print <- function(chks) {
  pkg <- attr(chks, "package")
  cli::cli_h2("Investigating {.emph {pkg}} as a package name")
  for (chk in chks) {
    if (inherits(chk[[1]], "error")) {
      cli::cli_alert_warning(chk[[2]])
    } else if (identical(chk[[1]], "info")) {
      cli::cli_alert_info(chk[[2]])
    } else if (isTRUE(chk[[1]])){
      cli::cli_alert_success(utils::head(chk[[2]], 1))
    } else {
      cli::cli_alert_danger(utils::tail(chk[[2]], 1))
    }
  }
}

pkg_name_check_function <- function(msg, fun) {
  attr(fun, "msg") <- msg
  class(fun) <- c("pkg_name_check_function", class(fun))
  fun
}

pkg_name_check_valid <- pkg_name_check_function(
  "Could not check if package_name is valid",
  function(nm) {
    async_constant({
      valid <- pkgdepends::is_valid_package_name(nm)
      list(
        c(valid),
        c("Valid package name.", 
          paste0("Invalid package name: ", attr(valid, "reason"))
        )
      )
    })
  }
)

# TODO: this should be case insensitive, so a completely different
# test is needed.

pkg_name_check_cran <- pkg_name_check_function(
  "Could not check if available on CRAN",
  function(nm) {
    url <- glue::glue("https://cran.rstudio.com/package={nm}")
    http_get(url)$
      then(function(resp) {
        list(
          resp$status_code == 404,
          c("Available on CRAN", "Not available on CRAN.")
        )
      })
  }
)

pkg_name_check_bioc <- pkg_name_check_function(
  "Could not check if available on Bioconductor",
  function(nm) {
    async_constant(list(
      TRUE,
      c("Available on Bioconductor", "Not available on Bioconductor")
    ))
  }
)

pkg_name_check_github <- pkg_name_check_function(
  "Could not check if available on GitHub",
  function(nm) {
    async_constant(list(
      TRUE,
      c("Available on GitHub", "Not available on GitHub")
    ))
  }
)

pkg_name_check_bad_word <- pkg_name_check_function(
  "Could not check if it is a bad word.",
  function(nm) {
    async_constant(list(
      TRUE,
      c("Not a bad word", "It is a bad word")
    ))
  }
)

pkg_name_check_abbrev <- pkg_name_check_function(
  "Could not reach abbreviations.com",
  function(nm) {
    async_constant(list(
      "info",
      "TODO"
    ))
  }
)

pkg_name_check_wikipedia <- pkg_name_check_function(
  "Could not reach Wikipedia",
  function(nm) {
    async_constant(list(
      "info",
      "TODO"
    ))
  }
)

pkg_name_check_wiktionary <- pkg_name_check_function(
  "Could not reach Wiktionary",
  function(nm) {
    async_constant(list(
      "info",
      "TODO"
    ))
  }
)

pkg_name_check_urban_dict <- pkg_name_check_function(
  "Could not reach Urban Dictionary",
  function(nm) {
    async_constant(list(
      "info",
      "TODO"
    ))
  }
)

pkg_name_check_sentiment <- pkg_name_check_function(
  "Could not perform sentiment check",
  function(nm) {
    async_constant(list(
      TRUE,
      "TODO"
    ))
  }
)

# TODO: OmegaHat: http://www.omegahat.net/R/src/contrib/
# TODO: R-Forge: http://download.r-forge.r-project.org/src/contrib/
# TODO: RForge: http://www.rforge.net/src/contrib/PACKAGES
# TODO: CRANextra: http://www.stats.ox.ac.uk/pub/RWin/src/contrib/
# TODO: INLA is forbidden?
#   https://inla.r-inla-download.org/R/stable/src/contrib/
#   https://github.com/gaborcsardi/R-dev-web/blob/88554a1e197875abc3c90ce9bbcec770b877d4fa/trunk/CRAN/QA/BDR/blackswan/packages/reinstall.R#L16

pkg_name_check_functions <- list(
  valid = pkg_name_check_valid,
  cran = pkg_name_check_cran,
  bioc = pkg_name_check_bioc,
  github = pkg_name_check_github,
  "bad-word" = pkg_name_check_bad_word,
  abbrev = pkg_name_check_abbrev,
  wikipedia = pkg_name_check_wikipedia,
  wiktionary = pkg_name_check_wiktionary,
  "urban-dict" = pkg_name_check_urban_dict,
  sentiment = pkg_name_check_sentiment
)
