require(rredis, quietly= T)
require(rjson)
require(httr)
require(PKI)
redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)

create.snippet <- function(content, ctx = NULL){
  
  require(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  data <- fromJSON(content)  
  files <- list()
  for(i in 1:length(data$files)){
    files[[i]] <- list(name= names(data$files[i]), content = data$files[[i]]$content)
  }
  data.json <- paste0('{ "name" : "',data$description,'","description":"',data$description,'" ,"isVisible": true,"isPublic": true,"files" : ',toJSON(files),' }')
  token <- redisGet(paste0(ctx$user$login, "_access_token"))
  response <- sni.post.request(ctx, data.json, token)
  res <- .get.git.res(response)
  res  
}

get.snippet <- function(id, version = NULL, ctx = NULL){
  require(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet(paste0(ctx$user$login, "_access_token"))
  if(!is.null(version)) {
    res <- sni.get.request(version, token, ctx)
    res <- .get.git.res(res)
    res$content$id <- id
    res
  }else { 
    res <- sni.get.request(id, token, ctx)
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
  require(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet(paste0(ctx$user$login, "_access_token"))
  snippet <- .get.snippet.res(id, token, ctx)
  if(length(grep("No such snippet", snippet)) == 0) {
    snippet.list <- fromJSON(snippet)
    snippet.files <- list()
    for(i in 1:length(snippet.list$files)){
      snippet.files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
    }
    snippet.files.names <- as.vector(sapply(snippet.files, function(o) o$name))
    if(is.null(fromJSON(content)$description)){
      content.files <- fromJSON(content)$files
      for(i in 1:length(content.files)){
        if (length(grep(names(content.files)[i], snippet.files.names)) !=0 ){
          if(is.null(content.files[i][[1]]$content)){
            index <- grep(names(content.files)[i], snippet.files.names)
            snippet.files <- snippet.files[-index]
            snippet.files.names <- snippet.files.names[-index]
          } else {
            index <- grep(names(content.files)[i], snippet.files.names)
            snippet.files[[index]] <- list(name=names(content.files)[i], content=content.files[i][[1]]$content)
          }
        } else {
          len <- length(snippet.files)
          snippet.files[[len+1]] <- list(name=names(content.files)[i], content=content.files[i][[1]]$content)
        }
      } 
    } else {
      snippet.list$name <- fromJSON(content)$description
    }
    data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'","isVisible": true,"isPublic": true ,"files" : ',toJSON(snippet.files),' }')
    revision.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'","isVisible": true,"isPublic": true ,"files" : ',toJSON(snippet.files),' }')
    revision.response <- sni.post.request(ctx , revision.json, token)
    redisLPush(id, fromJSON(revision.response)[1])
    response <- sni.post.request(ctx, data.json, token, id) 
    res <- .get.snippet.res(id, token, ctx)
    res <- .get.git.res(res)
    res
  } else {
    res <- list(ok=FALSE)
    res
  }  
}

delete.snippet <- function(id, ctx = NULL){
  token <- redisGet(paste0(ctx$user$login, "access_token"))
  sni.delete.request(id, token, ctx)
}

fork.snippet <- function(id, ctx = NULL) {
  require(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet(paste0(ctx$user$login, "_access_token"))
  snippet <- sni.get.request(id, token, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"isVisible": true,"isPublic": true,"files" : ',toJSON(files),' }')
  response <- sni.post.request(ctx, data.json, token)
  res <- .get.git.res(response)
  res
}

get.snippet.comments <- function(id, ctx = NULL){
  library(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet(paste0(ctx$user$login, "_access_token"))
  snippet <- sni.get.request(id, token, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name))
  comments <- grep("comment", fns)
  files <- files[comments]
  #comments.name <- as.vector(sapply(files, function(o) o$name))
  #for (i in 1:length(comments.name)){
  #  comments.name[i] <- as.numeric(strsplit(comments.name[i], "-")[[1]][2])
  #}
  #redisSet("comments.name", comments.name)
  if(length(files) == 0){
    res <- redisGet("get_comment_res")
    res$content <- list()
    res
  } else {
    comments <- redisGet("comment_res")
    comments.list <- fromJSON(comments)
    users <- redisGet("users")
    index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(snippet)$userId))
    user <- fromJSON(users)$values[[index]]$name
    comments.list[[1]]$user$login <- user
    comments.list[[1]]$user$id <- fromJSON(snippet)$userId
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
  snippet <- sni.get.request(id, token, ctx)
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
  snippet <- sni.get.request(id, token, ctx)
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
  require(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  snippet.files <- fromJSON(snippet)$files
  file_names <- as.vector(sapply(snippet.files, function(o) o$name))
  comments <- grep("comment", file_names)
  if(length(comments) !=0 ) {
    snippet.files <- snippet.files[-comments]
    file_names <- file_names[-comments]
  }
  notebook <- redisGet("notebook_res")
  notebook$content$comments <- length(comments)
  cdate <- as.POSIXct(fromJSON(snippet)$createdAt/1000, origin="1970-01-01")
  udate <- as.POSIXct(fromJSON(snippet)$updatedAt/1000, origin="1970-01-01")
  notebook$content$created_at <-paste0(strsplit(as.character(cdate), " ")[[1]][1], 'T', strsplit(as.character(cdate), " ")[[1]][2], 'Z')
  notebook$content$updated_at <-paste0(strsplit(as.character(udate), " ")[[1]][1], 'T', strsplit(as.character(udate), " ")[[1]][2], 'Z')
  notebook$content$id <- fromJSON(snippet)$guid
  notebook$content$description <- fromJSON(snippet)$name
  notebook$content$user$id <- fromJSON(snippet)$userId
  notebook$content$history[[1]]$user$id <- fromJSON(snippet)$userId
  users <- redisGet("users")
  index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(snippet)$userId))
  user <- fromJSON(users)$values[[index]]$name
  notebook$content$user$login <- user
  notebook$content$history[[1]]$user$login <- user
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

.get.snippet.res <- function(id, token, ctx) {
  require(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  res <- sni.get.request(id, token, ctx)
  res
}

create.snippet.comment <- function(id, content, ctx = NULL) {
  require(rredis, quietly= T)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  token <- redisGet(paste0(ctx$user$login, "_access_token"))
  snippet <- .get.snippet.res(id, token, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
    files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name)) 
  comment.part <- length(grep("comment", fns))
  len <- length(files)
  files[[len+1]] <- list(name=paste0("comment ",(comment.part+1),"-",fromJSON(snippet)$userId), content=fromJSON(content)$body)
  data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'","isVisible": true,"isPublic": true ,"files" : ',toJSON(files),' }')
  #token <- redisGet("access_token")
  res <- sni.post.request(ctx, data.json, token, id)
  comment_res <- redisGet("post_comment_res")
  users <- redisGet("users")
  index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(snippet)$userId))
  user <- fromJSON(users)$values[[index]]$name
  comment_res$content$body <- fromJSON(content)$body
  comment_res$content$user$login <- user
  comment_res$content$user$id <- fromJSON(snippet)$userId
  comment_res
  return(comment_res)
}

sni.post.request <- function(ctx, data.json, token, id = NULL) {
  cKey <- ctx$ckey
  cSecret <- PKI.load.private.pem(ctx$rsakey)
  if(is.null(id)){
    url <- ctx$stash.api.url
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="POST", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- postForm(url, curl=curl, style="POST")
  } else {
    url <- paste0(ctx$stash.api.url ,"/",id)
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="PUT", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- httpPUT(url, curl=curl, style="PUT")
  }
  
}

sni.get.request <- function(id , token, ctx) {
  cKey <- ctx$ckey
  cSecret <- PKI.load.private.pem(ctx$rsakey)
  url <- paste0(ctx$stash.api.url,"/",id)
  params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="GET", signMethod='RSA', handshakeComplete=handshakeComplete)
  params <- lapply(params, encodeURI.sni)
  auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
  curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  getURL(url,curl=curl)
}

sni.delete.request<- function(id, token, ctx) {
  cKey <- ctx$ckey
  cSecret <- PKI.load.private.pem(ctx$rsakey)
  url <- paste0(ctx$stash.api.url,"/",id)
  params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="DELETE", signMethod='RSA', handshakeComplete=handshakeComplete)
  params <- lapply(params, encodeURI.sni)
  auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
  curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  httpDELETE(url,curl=curl)
}
