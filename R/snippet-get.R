require(RCurl)
require(PKI)

## for GET request in snippet API
sni.get.request <- function(id , ctx) {
  cKey <- ctx$client_id
  cSecret <- PKI.load.private.pem(ctx$client_secret)
  url <- paste0(ctx$api_url,"/",id)
  if(!is.null(ctx$token)) {
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(ctx$token, "//")[[1]][1] , oauthSecret= strsplit(ctx$token, "//")[[1]][2], httpMethod="GET", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
    curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  } else {
    curl <- curlSetOpt(.opts=list(httpheader=c('Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  }
  getURL(url,curl=curl)
}
