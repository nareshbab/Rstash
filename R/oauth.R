require(PKI)
require(ROAuth)


authenticate.sni <- function(consumerKey, rsa_key) {

  reqURL = 'http://atlassian.client.research.att.com:7990/plugins/servlet/oauth/request-token'
  authURL = 'http://atlassian.client.research.att.com:7990/plugins/servlet/oauth/authorize'
  accessURL = 'http://atlassian.client.research.att.com:7990/plugins/servlet/oauth/access-token'
  cSecret <- key <- PKI.load.private.pem(rsa_key)
  cKey <- consumerKey
  
  credentials <- OAuthFactory$new(consumerKey=cKey, consumerSecret=cSecret, requestURL=reqURL, accessURL=accessURL, authURL=authURL, needsVerifier=FALSE,
  signMethod="RSA")
  
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
