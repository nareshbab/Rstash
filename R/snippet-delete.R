require(RCurl)
require(PKI)

## for DELETE request in snippet API
sni.delete.request<- function(id, ctx) {
  cKey <- ctx$client_id
  cSecret <- PKI.load.private.pem(ctx$client_secret)
  url <- paste0(ctx$api_url,"/",id)
  params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(ctx$token, "//")[[1]][1] , oauthSecret= strsplit(ctx$token, "//")[[1]][2], httpMethod="DELETE", signMethod='RSA', handshakeComplete=handshakeComplete)
  params <- lapply(params, encodeURI.sni)
  auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
  curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  httpDELETE(url,curl=curl)
}
