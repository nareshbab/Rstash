.token <- new.env(parent=emptyenv())

authenticate.sni <- function(consumerKey, rsa_key, baseurl) {
  cSecret <- PKI::PKI.load.private.pem(rsa_key)
  cKey <- consumerKey
  reqURL <- paste0(baseurl, "plugins/servlet/oauth/request-token")
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

verify.token <- function(token, client_id, client_secret, api_url) {
  ctx <- list(token=token, client_id=client_id, client_secret=client_secret, api_url=api_url)
  data.json <- paste0('{ "name" : "sample","description":"sample", "isVisible": true,"isPublic": true ,"files" : [{ "name" : "scratch.R", "content" : "#keep snippets here while working with your notebook cells" }] }')
  status <- tryCatch(
					{ 
					response <- sni.post.request(ctx, data.json)
					sni.delete.request(fromJSON(response)$guid, ctx)
					}, error=function(e) 
					          print("error")
					)
  if(status=="error") {
    return(list(status="error"))
  } else {
    return(list(status="success"))
  }
}

oauthGET.sni <- function(url, consumerKey, consumerSecret,
                     oauthKey, oauthSecret, params=character(), customHeader = NULL,
                     curl = getCurlHandle(), signMethod='RSA', ..., .opts = list(...)) {
  if(is.null(curl))
    curl <- getCurlHandle()
  
  params <- signRequest.sni(url, params, consumerKey, consumerSecret,
                        oauthKey=oauthKey, oauthSecret=oauthSecret,
                        httpMethod="GET", signMethod='RSA')
  
  RCurl::getForm(url, .params = params, curl = curl, .opts = c(httpget = TRUE,  list(...)))
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
  RCurl::postForm(url, .params = params, curl = curl,.opts = opts, style = "POST")
}
  

POST <- function(access_url, config, body) {
  cSecret <- PKI::PKI.load.private.pem(body$client_secret)
  accessURL <- paste0(strsplit(access_url, "/")[[1]][1], '//', strsplit(access_url, "/")[[1]][3], "/plugins/servlet/oauth/access-token")
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
