require(RCurl)
require(PKI)

## for POST and PUT request in snippet API
sni.post.request <- function(ctx, data.json, id = NULL) {
  cKey <- ctx$client_id
  cSecret <- PKI.load.private.pem(ctx$client_secret)
  if(is.null(id)) {
    url <- ctx$api_url
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(ctx$token, "//")[[1]][1] , oauthSecret= strsplit(ctx$token, "//")[[1]][2], httpMethod="POST", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- postForm(url, curl=curl, style="POST")
  } else {
    url <- paste0(ctx$api_url ,"/",id)
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(ctx$token, "//")[[1]][1] , oauthSecret= strsplit(ctx$token, "//")[[1]][2], httpMethod="PUT", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- httpPUT(url, curl=curl, style="PUT")
  }
}
