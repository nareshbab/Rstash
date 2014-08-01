require(RCurl)

genNonce.sni <- function(len = 15L + sample(1:16, 1L)) {
  ## Get a random sequence of characters.
  ## Nonce - number used only once.
  els <- c(letters, LETTERS, 0:9, "_")
  paste(sample(els, len, replace = TRUE), collapse = "")
}

encodeURI.sni <- function(URI, ...) {
  if (!is.character(URI)) {
    URI
  } else {
    OK <- "[^-A-Za-z0-9_.~]"
    x <- strsplit(URI, "")[[1L]]
    z <- grep(OK, x)
    if (length(z)) {
      y <- sapply(x[z], function(x) paste("%", toupper(as.character(charToRaw(x))),
                                          sep = "", collapse = ""))
      x[z] <- y
    }
      paste(x, collapse = "")
  }
}

## cf. http://tools.ietf.org/html/rfc5849#section-3.4.1.3.2
normalizeParams.sni <- function(params, escapeFun) {
  ## we escape the values of the parameters in a special way that escapes
  ## the resulting % prefix in the escaped characters, e.g. %20 becomes
  ## %2520 as %25 is the escape for %
  names(params) <- sapply(names(params), escapeFun, post.amp = TRUE)
  params <- sapply(params, escapeFun, post.amp = TRUE)
  ## If two or more parameters share the same name, they are sorted by their value.
  params <- params[order(names(params), params)]
  return(paste(names(params), params, sep = "=", collapse = "&"))
}
  
signRequest.sni <- function(url, params, consumerKey, consumerSecret,
						oauthKey = "", oauthSecret = "", httpMethod = "GET",
						signMethod = "HMAC", nonce = genNonce.sni(),
						timestamp = Sys.time(),
						escapeFun = encodeURI.sni,
						handshakeComplete=TRUE) {
  ## Sign an request made up of the URL, the parameters as a named character
  ## vector the consumer key and secret and the token and token secret.
  httpMethod <- toupper(httpMethod)
  signMethod <- toupper(signMethod)
  params["oauth_nonce"] <- nonce
  params["oauth_timestamp"] <- as.integer(timestamp)

  token <- oauthKey
  if(!is.null(token) && !is.na(token) && token != "")
  	params["oauth_token"] <- token

  params["oauth_consumer_key"] <- consumerKey
  params["oauth_signature_method"] <- switch(signMethod,
												HMAC = 'HMAC-SHA1',
												RSA = 'RSA-SHA1',
												text = 'PLAINTEXT',
												stop("Unsupported signature method: ", signMethod))
  params["oauth_version"] <- '1.0'
  args <- escapeFun(normalizeParams.sni(params, escapeFun), post.amp = TRUE)
  if(is.null(oauthSecret))
	oauthSecret <- ""
  okey <- consumerSecret
  ## note that we don't escape the args string again.
  odat <- paste(c(sapply(c(httpMethod, url), escapeFun), args),	collapse = "&")
  sig <- signString.sni(odat, okey, signMethod)
  params["oauth_signature"] <- sig
  return(params)
}

signString.sni <- function(str, key, method) {
  ## Perform the actual computation to get the signature of the data
  sigFunc <- switch(toupper(method),
                    HMAC = signWithHMAC,
                    RSA = signWithRSA.sni,
                    text = signWithPlaintext,
                    stop("No signature method for ", method))
  sigFunc(key, str)
}

signWithRSA.sni <- function(key, data) {
  sig <- PKI.sign.PEM(data, key, hash="SHA1")
  return(base64(sig)[1])
}

parseResponse.sni <- function(response) {
  pairs <- sapply(strsplit(response, '&')[[1]], strsplit, '=')
  out <- sapply(pairs, function(x) x[2])
  names(out) <- sapply(pairs, function(x) x[1])
  out
}
