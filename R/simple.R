require(rjson)
require(httr)
require(PKI)

create.snippet <- function(content, ctx = NULL) {
  data <- fromJSON(content)  
  files <- list()
  ##Reading all files to be created in snippet
  for(i in 1:length(data$files)) {
    files[[i]] <- list(name= names(data$files[i]), content = data$files[[i]]$content)
  }
  data.json <- paste0('{ "name" : "',data$description,'","description":"',data$description,'" ,"isVisible": true,"isPublic": true,"files" : ',toJSON(files),' }')
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  response <- sni.post.request(ctx, data.json, token)
  ##To get response similar to github
  res <- .get.git.res(response)
  res  
}

get.snippet <- function(id, version = NULL, ctx = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  ##Whether version is given or not
  if(!is.null(version)) {
    res <- sni.get.request(version, token, ctx)
    res <- .get.git.res(res)
    res$content$id <- id
    res
  } else { 
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

modify.snippet <- function(id, content, ctx = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  ##first get all the files of an existing snippet
  snippet <- .get.snippet.res(id, token, ctx)
  if(length(grep("No such snippet", snippet)) == 0) {
    snippet.list <- fromJSON(snippet)
    snippet.files <- list()
    for(i in 1:length(snippet.list$files)) {
      snippet.files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
    }
    snippet.files.names <- as.vector(sapply(snippet.files, function(o) o$name))
    if(is.null(fromJSON(content)$description)) {
      content.files <- fromJSON(content)$files
      for(i in 1:length(content.files)) {
        if (length(grep(names(content.files)[i], snippet.files.names)) !=0 ) {
          if(is.null(content.files[i][[1]]$content)) {
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
    ##creating a snippet which serves the purpose of history version
    revision.response <- sni.post.request(ctx , revision.json, token)
    ##kepping a mapping of snippet id with its history versions
    redis.list( .session$rc , id, fromJSON(revision.response)[1])
    response <- sni.post.request(ctx, data.json, token, id) 
    res <- .get.snippet.res(id, token, ctx)
    res <- .get.git.res(res)
    res
  } else {
    res <- list(ok=FALSE)
    res
  }  
}

delete.snippet <- function(id, ctx = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  sni.delete.request(id, token, ctx)
}

fork.snippet <- function(id, ctx = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  snippet <- sni.get.request(id, token, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  ##taking out all the files of existing snippet 
  for(i in 1:length(snippet.list$files)) {
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"isVisible": true,"isPublic": true,"files" : ',toJSON(files),' }')
  ## creating a snippet copy with all the files 
  response <- sni.post.request(ctx, data.json, token)
  res <- .get.git.res(response)
  res
}

get.snippet.comments <- function(id, ctx = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  snippet <- sni.get.request(id, token, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)) {
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name))
  ## searching for comments inside files-- comments are also stored as snippet files
  comments <- grep("comment", fns)
  files <- files[comments]
  if(length(files) == 0) {
    res <- redis.get( .session$rc, "get_comment_res")
    res$content <- list()
    res
  } else {
    comments <- redis.get( .session$rc, "comment_res")
    comments.list <- fromJSON(comments)
    users <- redis.get( .session$rc, "users")
    index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(snippet)$userId))
    user <- fromJSON(users)$values[[index]]$name
    comments.list[[1]]$user$login <- user
    comments.list[[1]]$user$id <- fromJSON(snippet)$userId
    for(i in 1:length(files)){
      comments.list[[i]] <- comments.list[[1]]
      comments.list[[i]]$body <- files[[i]]$content
    }
    res <- redis.get( .session$rc, "get_comment_res")
    res$content <- comments.list
    return(res)
  }
}

get.snippet.without.comments <- function(id, version = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  snippet <- sni.get.request(id, token, ctx)
  if(length(grep("No such snippet", snippet)) == 0) {
    snippet.list <- fromJSON(snippet)
    files <- list()
    for(i in 1:length(snippet.list$files)) {
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
    }
    fns <- as.vector(sapply(files, function(o) o$name))
    comments <- grep("comment", fns)
    ## removing files which are basically created for comments workaround
    files <- files[-comments]
    snippet.list$files <- files
    res <- .get.git.res(toJSON(snippet.list))
    res
  } else {
    res <- list(ok=FALSE)
    res
  }
}

get.snippet.user.comments <- function(id, user, ctx = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  snippet <- sni.get.request(id, token, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)) {
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name))
  ## keeping files corresponding to particular user
  comments <- grep(user, fns)
  files <- files[comments]
  return(toJSON(files))
}

.get.git.res <- function(snippet) {
  ## HACK: this is basically to get similar api response as that of github
  snippet.files <- fromJSON(snippet)$files
  file_names <- as.vector(sapply(snippet.files, function(o) o$name))
  comments <- grep("comment", file_names)
  ## remove all the comment files
  if(length(comments) !=0 ) {
    snippet.files <- snippet.files[-comments]
    file_names <- file_names[-comments]
  }
  ## taking a dummy response as given by github
  notebook <- redis.get( .session$rc, "notebook_res")
  ## setting different metadata fields in the github response
  notebook$content$comments <- length(comments)
  cdate <- as.POSIXct(fromJSON(snippet)$createdAt/1000, origin="1970-01-01")
  udate <- as.POSIXct(fromJSON(snippet)$updatedAt/1000, origin="1970-01-01")
  notebook$content$created_at <-paste0(strsplit(as.character(cdate), " ")[[1]][1], 'T', strsplit(as.character(cdate), " ")[[1]][2], 'Z')
  notebook$content$updated_at <-paste0(strsplit(as.character(udate), " ")[[1]][1], 'T', strsplit(as.character(udate), " ")[[1]][2], 'Z')
  notebook$content$id <- fromJSON(snippet)$guid
  notebook$content$description <- fromJSON(snippet)$name
  notebook$content$user$id <- fromJSON(snippet)$userId
  notebook$content$history[[1]]$user$id <- fromJSON(snippet)$userId
  users <- redis.get( .session$rc, "users")
  index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(snippet)$userId))
  user <- fromJSON(users)$values[[index]]$name
  notebook$content$user$login <- user
  notebook$content$history[[1]]$user$login <- user
  for(i in 1:length(snippet.files)) {
    notebook$content$files[i] <- notebook$content$files[1]
  }
  ## to get all the history versions mapped inside redis
  revisions <- redis.list.range(.session$rc, fromJSON(snippet)$guid, 0 , -1)
  if(length(revisions) !=0 ) {
    for(i in 1:length(revisions)) {
      notebook$content$history[[i]] <- notebook$content$history[[1]]
      notebook$content$history[[i]]$version <- revisions[[i]]
    }
  } else {
    for(i in 1:length(snippet.files)) {
      notebook$content$history[[i]] <- notebook$content$history[[1]]
    }
  }
  names(notebook$content$files) <- file_names
  for(i in 1:length(notebook$content$files)) {
    notebook$content$files[[i]]$filename <- file_names[i]
    notebook$content$files[[i]]$content <- snippet.files[[i]]$content
  }
  return(notebook)
}

.get.snippet.res <- function(id, token, ctx) {
  res <- sni.get.request(id, token, ctx)
  res
}

create.snippet.comment <- function(id, content, ctx = NULL) {
  token <- redis.get( .session$rc, paste0(ctx$user$login, "_access_token"))
  snippet <- .get.snippet.res(id, token, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)) {
    files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  fns <- as.vector(sapply(files, function(o) o$name)) 
  comment.part <- length(grep("comment", fns))
  len <- length(files)
  ## HACK: storing comments as new file in snippets
  files[[len+1]] <- list(name=paste0("comment ",(comment.part+1),"-",fromJSON(snippet)$userId), content=fromJSON(content)$body)
  data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'","isVisible": true,"isPublic": true ,"files" : ',toJSON(files),' }')
  res <- sni.post.request(ctx, data.json, token, id)
  comment_res <- redis.get( .session$rc, "post_comment_res")
  users <- redis.get( .session$rc, "users")
  index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(snippet)$userId))
  user <- fromJSON(users)$values[[index]]$name
  comment_res$content$body <- fromJSON(content)$body
  comment_res$content$user$login <- user
  comment_res$content$user$id <- fromJSON(snippet)$userId
  return(comment_res)
}

## for POST and PUT request in snippet API
sni.post.request <- function(ctx, data.json, token, id = NULL) {
  cKey <- ctx$client_id
  cSecret <- PKI.load.private.pem(ctx$client_secret)
  if(is.null(id)) {
    url <- ctx$api_url
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="POST", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- postForm(url, curl=curl, style="POST")
  } else {
    url <- paste0(ctx$api_url ,"/",id)
    params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="PUT", signMethod='RSA', handshakeComplete=handshakeComplete)
    params <- lapply(params, encodeURI.sni)
    auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "") 
    curl <- curlSetOpt(.opts=list(postfields=data.json, httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())  
    sni <- httpPUT(url, curl=curl, style="PUT")
  }
}

## for GET request in snippet API
sni.get.request <- function(id , token, ctx) {
  cKey <- ctx$client_id
  cSecret <- PKI.load.private.pem(ctx$client_secret)
  url <- paste0(ctx$api_url,"/",id)
  params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="GET", signMethod='RSA', handshakeComplete=handshakeComplete)
  params <- lapply(params, encodeURI.sni)
  auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
  curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  getURL(url,curl=curl)
}

## for DELETE request in snippet API
sni.delete.request<- function(id, token, ctx) {
  cKey <- ctx$client_id
  cSecret <- PKI.load.private.pem(ctx$client_secret)
  url <- paste0(ctx$api_url,"/",id)
  params <- signRequest.sni(url, params=character(), consumerKey = cKey, consumerSecret = cSecret, oauthKey= strsplit(strsplit(token, "&")[[1]][1], "=")[[1]][2] , oauthSecret= strsplit(strsplit(token, "&")[[1]][2], "=")[[1]][2], httpMethod="DELETE", signMethod='RSA', handshakeComplete=handshakeComplete)
  params <- lapply(params, encodeURI.sni)
  auth = paste0("OAuth ", paste0(names(params),"=", '"',params, '"', collapse=",") , "")
  curl <- curlSetOpt(.opts=list(httpheader=c('Authorization'= auth , 'Content-Type' = 'application/json')), verbose=FALSE, curl=getCurlHandle())
  httpDELETE(url,curl=curl)
}

create.snippet.context <- function(api_url , client_id , client_secret , access_token = NULL, personal_token = NULL, max_etags = 10000, verbose = FALSE) {
  ctx <- list(api_url = api_url, client_id= client_id, client_secret= client_secret, token= access_token)
  url <- paste0("http://",strsplit(api_url, "/")[[1]][3], "/rest/api/1.0/users")
  vals <- redis.get( .session$rc, "Key")
  cSecret <- PKI.load.private.pem(client_secret)
  users <- oauthGET.sni(url, consumerKey= client_id, consumerSecret = cSecret,
   oauthKey = strsplit(access_token, "//")[[1]][1] , oauthSecret= strsplit(access_token, "//")[[1]][2], signMethod='RSA',
   curl=getCurlHandle())
  redis.set(.session$rc, "users", users)
  data.json <- paste0('{ "name" : "sample","description":"sample", "isVisible": true,"isPublic": true ,"files" : [{ "name" : "scratch.R", "content" : "#keep snippets here while working with your notebook cells" }] }')
  token <- paste0("oauth_token=",strsplit(access_token, "//")[[1]][1],"&oauth_token_secret=",strsplit(access_token, "//")[[1]][2])
  response <- sni.post.request(ctx , data.json, token)
  index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(response)$userId))
  user <- fromJSON(users)$values[[index]]$name
  redis.set( .session$rc, paste0(user,"_access_token"), token)
  ctx$user$login <- user
  ctx
}
