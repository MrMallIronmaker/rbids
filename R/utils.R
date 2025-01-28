anonymize <- function(x, salt, algo = "sha256", chars = 8) {

  parallel_digest <- function(entry) {
    digest::digest(entry, algo = algo)
  }

  result <- substr(sapply(paste0(x, salt), parallel_digest), 1, chars)

  if (length(unique(result)) != length(unique(x))) {
    warning("Collisions occurred in the call to `anonymize`. Consider increasing
            the number of characters retained with the `chars` argument.")
  }
  result
}