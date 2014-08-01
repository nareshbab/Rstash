require(PKI)
require(RCurl)
.token <- new.env(parent=emptyenv())

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
  .token$variables <- vals
  return(vals)
}

oauthGET.sni <- function(url, consumerKey, consumerSecret,
                     oauthKey, oauthSecret, params=character(), customHeader = NULL,
                     curl = getCurlHandle(), signMethod='RSA', ..., .opts = list(...)) {
  if(is.null(curl))
    curl <- getCurlHandle()
  
  params <- signRequest.sni(url, params, consumerKey, consumerSecret,
                        oauthKey=oauthKey, oauthSecret=oauthSecret,
                        httpMethod="GET", signMethod='RSA')
  
  getForm(url, .params = params, curl = curl, .opts = c(httpget = TRUE,  list(...)))
}

oauthPOST.sni <- function(url, consumerKey, consumerSecret,
						oauthKey, oauthSecret, params=character(), customHeader = NULL,
						curl = getCurlHandle(), signMethod='RSA', handshakeComplete=TRUE,...) {
  if(is.null(curl))
    curl <- getCurlHandle()
  params <- signRequest.sni(url, params, consumerKey, consumerSecret,
						oauthKey=oauthKey, oauthSecret=oauthSecret,
						httpMethod="POST", signMethod='RSA',
						handshakeComplete=handshakeComplete)
  opts <- list(...)
  ## post ,specify the method
  postForm(url, .params = params, curl = curl,.opts = opts, style = "POST")
}
  

POST <- function(access_url, config, body) {
  cSecret <- PKI.load.private.pem(body$client_secret)
  url.split <- strsplit(access_url, "/")[[1]][-6]
  url.split[7] <- "access-token"
  accessURL <- paste(url.split, collapse="/")
  params <- c(oauth_verifier=.token$variables[[1]])
  resp <- oauthPOST.sni(accessURL, consumerKey = body$client_id, consumerSecret = cSecret,
    oauthKey = .token$variables[[1]], oauthSecret = .token$variables[[2]], signMethod=signMethod,
    curl=getCurlHandle(), params=params, handshakeComplete=handshakeComplete)
  result <- list(access_token=paste0(strsplit(strsplit(resp, "&")[[1]][1], "=")[[1]][2],"//",strsplit(strsplit(resp, "&")[[1]][2], "=")[[1]][2]),expires_in=strsplit(strsplit(resp, "&")[[1]][3], "=")[[1]][2], session_handle=strsplit(strsplit(resp, "&")[[1]][4], "=")[[1]][2], authorization_expires_in=strsplit(strsplit(resp, "&")[[1]][5], "=")[[1]][2])
  rjson::toJSON(result)
}

content <- function(json) {
  rjson::fromJSON(json)
}
