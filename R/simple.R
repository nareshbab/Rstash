require(rredis)
require(rjson)
require(httr)
require(PKI)
redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)

create.snippet <- function(content, ctx = NULL){
  
  require(rredis)
  redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  data <- fromJSON(content)
  data.json <- paste0('{ "name" : "',data$description,'","description":"',data$description,'" ,"files" : [{ "name" : "scratch.R", "content" : "#keep snippets here while working with your notebook cells" }] }')
  token <- redisGet("access_token")
  response <- sni.post.request(data.json, token)
  res <- redisGet("notebook_res")
  res$content$id <- fromJSON(response)$guid
  redisLPush(fromJSON(response)$guid, fromJSON(response)[1])
  res$content$description <- fromJSON(response)$name
  res$content$user$id <- fromJSON(response)$userId
  res  
}

get.snippet <- function(id, version = NULL, ctx = NULL){
  require(rredis)
  redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet("access_token")
  if(!is.null(version)) {
    res <- sni.get.request(version, token)
    res <- .get.git.res(res)
    res$content$id <- id
    res
  }else { 
    res <- sni.get.request(id, token)
    if(length(grep("No such snippet", res)) == 0) {
      res <- .get.git.res(res)
      res
    } else {
      content <- '{"description":"Notebook1"}'
      res <- create.snippet(content, ctx = NULL)
      res   
    }
  }
}

modify.snippet <- function(id, content, ctx = NULL){
  snippet <- .get.snippet.res(id)
  if(length(grep("No such snippet", snippet)) == 0) {
    snippet.list <- fromJSON(snippet)
    files <- list()
    for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
    }
    fns <- as.vector(sapply(files, function(o) o$name))
    if(is.list(content)){
      updated.parts <- grep(names(content[[1]]), fns)
      if(length(updated.parts) == 0){
          len <- length(files)
          files[[len+1]] <- list(name=names(content[[1]]), content=content[[1]][[1]][[1]])
	    } else {
	      files[[updated.parts]] <- list(name=names(content[[1]]), content=content[[1]][[1]][[1]]) 
	    }
    } else {
      updated.parts <- grep(names(fromJSON(content)[[1]]), fns)
      if (is.null(fromJSON(content)[[1]][[1]])) {
        files <- files[-updated.parts]
      }else {
        if(length(updated.parts) == 0){
          len <- length(files)
          files[[len+1]] <- list(name=names(fromJSON(content)[[1]]), content=fromJSON(content)[[1]][[1]][[1]])
	    } else {
	      files[[updated.parts]] <- list(name=names(fromJSON(content)[[1]]), content=fromJSON(content)[[1]][[1]][[1]]) 
	    }
      }
    }
    library(rredis)
    redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
    token <- redisGet("access_token")
    data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"files" : ',toJSON(files),' }')
    revision.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"files" : ',toJSON(files),' }')
    revision.response <- sni.post.request(revision.json, token)
    redisLPush(id, fromJSON(revision.response)[1])
    response <- sni.post.request(data.json, token, id)
    res <- .get.snippet.res(id)
    res <- .get.git.res(res)
    res
  } else {
    res <- list(ok=FALSE)
    res
  }  
}

delete.snippet <- function(id){
  token <- redisGet("access_token")
  sni.delete.request(id, token)
}

fork.snippet <- function(id, ctx = NULL) {
  token <- redisGet("access_token")
  snippet <- sni.get.request(id, token)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  data.json <- paste0('{ "name" : "',snippet.list$name,'-Copy","description":"',snippet.list$name,'-Copy" ,"files" : ',toJSON(files),' }')
  response <- sni.post.request(data.json, token)
  res <- .get.git.res(response)
  res$content$id <- fromJSON(response)$guid
  res$content$description <- fromJSON(response)$name
  res$content$user$id <- fromJSON(response)$userId
  res
}

get.snippet.comments <- function(id, ctx = NULL){
  library(rredis)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet("access_token")
  snippet <- sni.get.request(id, token)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name))
  comments <- grep("comment", fns)
  files <- files[comments]
  if(length(files) == 0){
    res <- redisGet("get_comment_res")
    res$content <- list()
    res
  } else {
    comments <- redisGet("comment_res")
    comments.list <- fromJSON(comments)
    for(i in 1:length(files)){
      comments.list[[i]] <- comments.list[[1]]
      comments.list[[i]]$body <- files[[i]]$content
    }
    res <- redisGet("get_comment_res")
    res$content <- comments.list
    return(res)
  }
}

get.snippet.without.comments <- function(id, version = NULL){
  token <- redisGet("access_token")
  snippet <- sni.get.request(id, token)
  if(length(grep("No such snippet", snippet)) == 0) {
    snippet.list <- fromJSON(snippet)
    files <- list()
    for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
    }
    fns <- as.vector(sapply(files, function(o) o$name))
    comments <- grep("comment", fns)
    files <- files[-comments]
    snippet.list$files <- files
    res <- .get.git.res(toJSON(snippet.list))
    res
  } else {
    res <- list(ok=FALSE)
    res
  }
}

get.snippet.user.comments <- function(id, user) {
  token <- redisGet("access_token")
  snippet <- sni.get.request(id, token)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name))
  comments <- grep(user, fns)
  files <- files[comments]
  return(toJSON(files))
}

.get.git.res <- function(snippet) {
  library(rredis)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  redisSet("snippet", snippet)
  snippet.files <- fromJSON(snippet)$files
  file_names <- as.vector(sapply(snippet.files, function(o) o$name))
  comments <- grep("comment", file_names)
  if(length(comments) !=0 ) {
    snippet.files <- snippet.files[-comments]
    file_names <- file_names[-comments]
  }
  notebook <- redisGet("notebook_res")
  notebook$content$id <- fromJSON(snippet)$guid
  notebook$content$description <- fromJSON(snippet)$name
  notebook$content$user$id <- fromJSON(snippet)$userId
  notebook$content$history[[1]]$user$id <- fromJSON(snippet)$userId
  for(i in 1:length(snippet.files)){
    notebook$content$files[i] <- notebook$content$files[1]
  }
  revisions <- redisLRange(fromJSON(snippet)$guid, 0 , -1)
  if(!is.null(revisions)) {
    for(i in 1:length(revisions)) {
      notebook$content$history[[i]] <- notebook$content$history[[1]]
      notebook$content$history[[i]]$version <- revisions[[i]]$guid
    }
  } else {
    for(i in 1:length(snippet.files)) {
      notebook$content$history[[i]] <- notebook$content$history[[1]]
    }
  }
  names(notebook$content$files) <- file_names
  for(i in 1:length(notebook$content$files)){
    notebook$content$files[[i]]$filename <- file_names[i]
    notebook$content$files[[i]]$content <- snippet.files[[i]]$content
  }
  #if(length(notebook$content$history) != length(notebook$content$history)) {

  return(notebook)
}

.get.snippet.res <- function(id) {
  library(rredis)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet("access_token")
  res <- sni.get.request(id, token)
  res
}

create.snippet.comment <- function(id, content, ctx = NULL) {
  snippet <- .get.snippet.res(id)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
    files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name)) 
  comment.part <- length(grep("comment", fns))
  len <- length(files)
  files[[len+1]] <- list(name=paste0("comment ",(comment.part+1),"-",fromJSON(snippet)$userId), content=fromJSON(content)$body)
  data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"files" : ',toJSON(files),' }')
  curl <- paste0('curl -X PUT -u nareshbabral:work4future -H "Content-Type:application/json" -d \'',data.json,'\' http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
  res <- system(curl, intern=T)
  comment_res <- redisGet("post_comment_res")
  comment_res$content$body <- fromJSON(content)$body
  comment_res
  return(comment_res)
}

sni.post.request <- function(data.json, token, id = NULL) {
  cKey <- "naresh"
  cSecret <- PKI.load.private.pem("/home/naresh/mykey.pem")
  if(is.null(id)){
    url <- "http://127.0.0.1:7990/rest/snippets/1.0/snippets"
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="POST", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- postForm(url, curl=curl, style="POST")
  } else {
    url <- paste0("http://127.0.0.1:7990/rest/snippets/1.0/snippets/",id)
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="PUT", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- httpPUT(url, curl=curl, style="PUT")
  }
  
}

sni.get.request <- function(id , token) {
  cKey <- "naresh"
  cSecret <- PKI.load.private.pem("/home/naresh/mykey.pem")
  url <- paste0("http://127.0.0.1:7990/rest/snippets/1.0/snippets/",id)
  params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="GET", signMethod='RSA', handshakeComplete=handshakeComplete)
  params <- lapply(params, encodeURI.sni)
  auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
  curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  getURL(url,curl=curl)
}

sni.delete.request<- function(id, token) {
  cKey <- "naresh"
  cSecret <- PKI.load.private.pem("/home/naresh/mykey.pem")
  url <- paste0("http://127.0.0.1:7990/rest/snippets/1.0/snippets/",id)
  params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="DELETE", signMethod='RSA', handshakeComplete=handshakeComplete)
  params <- lapply(params, encodeURI.sni)
  auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
  curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  httpDELETE(url,curl=curl)
}
