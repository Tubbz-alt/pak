
synchronise <- synchronize <- function(...) {
  asNamespace("pkgcache")$synchronise(...)
}

async_reflect <- function(...) {
  asNamespace("pkgcache")$async_reflect(...)
}

async_constant <- function(...) {
  asNamespace("pkgcache")$async_constant(...)
}

async_timeout <- function(...) {
  asNamespace("pkgcache")$async_timeout(...)
}

http_get <- function(...) {
  asNamespace("pkgcache")$http_get(...)
}

when_all <- function(...) {
  asNamespace("pkgcache")$when_all(...)
}
