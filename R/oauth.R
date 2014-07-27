require(PKI)

authenticate.sni <- function(consumerKey, rsa_key, reqURL) {
  cSecret <- key <- PKI.load.private.pem(rsa_key)
  cKey <- consumerKey
  resp <- oauthPOST.sni(url = reqURL,
						consumerKey = cKey,
						consumerSecret = cSecret,
						oauthKey = NULL,
						oauthSecret = NULL,
						signMethod = signMethod,
						handshakeComplete=handshakeComplete)
  vals <- parseResponse.sni(resp)
  if (!all(c('oauth_token', 'oauth_token_secret') %in%
             names(vals))) {
    stop("Invalid response from site, please ",
         "check your consumerKey and consumerSecret",
         " and try again.")
  }
  return(vals)
}
