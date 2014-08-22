require(rjson)
require(rediscc)

create.gist <- function(content, ctx = .session$ctx) {
  data <- fromJSON(content)  
  files <- list()
  ##Reading all files to be created in snippet
  for(i in 1:length(data$files)) {
    files[[i]] <- list(name= names(data$files[i]), content = data$files[[i]]$content)
  }
  if(length(grep(".versions", files)) == 0) {
    files[[length(files)+1]] <- list(name= ".versions", content=toJSON(list()))
  } else {
    files[[grep(".versions", files)]]$content <- toJSON(list())
  }
  index.versions <- grep(".versions", files)
  revision.json <- paste0('{ "name" : "',data$description,'","description":"',data$description,'" ,"isVisible": true,"isPublic": true,"files" : ',toJSON(files),' }')
  response <- sni.post.request(ctx, revision.json)
  history.version <- list()
  history.version[[1]] <- fromJSON(response)$guid
  files[[index.versions]]$content <- toJSON(history.version)
  data.json <- paste0('{ "name" : "',data$description,'","description":"',data$description,'" ,"isVisible": true,"isPublic": true,"files" : ',toJSON(files),' }')
  res <- sni.post.request(ctx, data.json)
  ##To get response similar to github
  res <- .get.git.res(res)
  res  
}

get.gist <- function(id, version = NULL, ctx = .session$ctx) {
  ##Whether version is given or not
  if(!is.null(version)) {
    res <- sni.get.request(version, ctx)
    res <- .get.git.res(res)
    res$content$id <- id
    res
  } else { 
    res <- sni.get.request(id, ctx)
    if(length(grep("No such snippet", res)) == 0) {
      res <- .get.git.res(res)
      res
    } else {
      content <- '{"description":"Notebook1"}'
      res <- create.gist(content, ctx = NULL)
      res   
    }
  }
}

modify.gist <- function(id, content, ctx = .session$ctx) {
  ##first get all the files of an existing snippet
  snippet <- .get.snippet.res(id, ctx)
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
    revision.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'","isVisible": true,"isPublic": true ,"files" : ',toJSON(snippet.files),' }')
    ##creating a snippet which serves the purpose of history version
    revision.response <- sni.post.request(ctx , revision.json)
    index.scratch <- grep(".versions", snippet.files)
    history.revision <- fromJSON(snippet.files[[index.scratch]]$content)
    history.revision[[length(history.revision) + 1 ]] <- fromJSON(revision.response)$guid
    snippet.files[[index.scratch]]$content <- toJSON(history.revision)
    data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'","isVisible": true,"isPublic": true ,"files" : ',toJSON(snippet.files),' }')
    ##kepping a mapping of snippet id with its history versions
    response <- sni.post.request(ctx, data.json, id) 
    res <- .get.snippet.res(id, ctx)
    res <- .get.git.res(res)
    res
  } else {
    res <- list(ok=FALSE)
    res
  }  
}

delete.gist <- function(id, ctx = .session$ctx) {
  sni.delete.request(id, ctx)
}

fork.gist <- function(id, ctx = .session$ctx) {
  snippet <- sni.get.request(id, ctx)
  snippet.list <- fromJSON(snippet)
  files <- list()
  ##taking out all the files of existing snippet 
  for(i in 1:length(snippet.list$files)) {
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"isVisible": true,"isPublic": true,"files" : ',toJSON(files),' }')
  ## creating a snippet copy with all the files 
  response <- sni.post.request(ctx, data.json)
  res <- .get.git.res(response)
  res
}

get.gist.comments <- function(id, ctx = .session$ctx) {
  comments <- rev(redis.list.range(.session$rc, id, 0, -1))
  if(length(comments) == 0) {
    res <- redis.get( .session$rc, "get_comment_res")
    res$content <- list()
    res
  } else {
    comments.res <- redis.get( .session$rc, "comment_res")
    comments.list <- fromJSON(comments.res)
    users <- redis.get( .session$rc, "users")
    for(i in 1:length(comments)){
      comments.list[[i]] <- comments.list[[1]]
      index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$name == fromJSON(comments[[i]])$user))
      comments.list[[i]]$user$login <- fromJSON(users)$values[[index]]$name
      comments.list[[i]]$user$id <- fromJSON(users)$values[[index]]$id
      comments.list[[i]]$body <- fromJSON(comments[[i]])$comment
    }
    res <- redis.get( .session$rc, "get_comment_res")
    res$content <- comments.list
    return(res)
  }
}

get.gist.user.comments <- function(id, user, ctx = .session$ctx) {
  comments <- rev(redis.list.range(.session$rc, id, 0, -1))
  index <- grep(TRUE, lapply(comments, function(o) fromJSON(o)$user==user))
  ## keeping files corresponding to particular user
  return(comments[[index]])
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
  index.scratch <- grep(".versions", snippet.files)
  history.revision <- fromJSON(snippet.files[[index.scratch]]$content)
  if(length(history.revision) !=0 ) {
    for(i in 1:length(history.revision)) {
      notebook$content$history[[i]] <- notebook$content$history[[1]]
      notebook$content$history[[i]]$version <- history.revision[[i]]
    }
  } else {
    for(i in 1:length(snippet.files)) {
      notebook$content$history[[i]] <- notebook$content$history[[1]]
    }
  }
  names(notebook$content$files) <- file_names
  for(i in 1:length(notebook$content$files)) {
    notebook$content$files[[i]]$filename <- file_names[i]
    extension <- tail(strsplit(file_names[i],'\\.')[[1]], n=1)
    switch(extension,
			R = {notebook$content$files[[i]]$language <- "R"},
			md = {notebook$content$files[[i]]$language <- "Markdown"},
			py = {notebook$content$files[[i]]$language <- "Python"})
    notebook$content$files[[i]]$content <- snippet.files[[i]]$content
  }
  return(notebook)
}

.get.snippet.res <- function(id, ctx = .session$ctx) {
  res <- sni.get.request(id, ctx)
  res
}

create.gist.comment <- function(id, content, ctx = .session$ctx) {
  comment_res <- redis.get( .session$rc, "post_comment_res")
  users <- redis.get(.session$rc, "users")
  index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$name == ctx$user$login))
  comment_res$content$body <- fromJSON(content)$body
  comment_res$content$user$login <- ctx$user$login
  comment_res$content$user$id <- fromJSON(users)$values[[index]]$id
  redis.list(.session$rc, id, toJSON(list(user=ctx$user$login, comment=fromJSON(content)$body)))
  return(comment_res)
}

create.github.context <- function(api_url , client_id , client_secret , access_token = NULL, personal_token = NULL, max_etags = 10000, verbose = FALSE) {
  ctx <- list(api_url = api_url, client_id= client_id, client_secret= client_secret, token= access_token, authenticated = !is.null(access_token))
  if(!is.null(access_token)) {
    url <- paste0("http://",strsplit(api_url, "/")[[1]][3], "/rest/api/1.0/users")
    cSecret <- PKI::PKI.load.private.pem(client_secret)
    users <- oauthGET.sni(url, consumerKey= client_id, consumerSecret = cSecret,
     oauthKey = strsplit(access_token, "//")[[1]][1] , oauthSecret= strsplit(access_token, "//")[[1]][2], signMethod='RSA',
     curl=getCurlHandle())
    redis.set(.session$rc, "users", users)
    data.json <- paste0('{ "name" : "sample","description":"sample", "isVisible": true,"isPublic": true ,"files" : [{ "name" : "scratch.R", "content" : "#keep snippets here while working with your notebook cells" }] }')
    response <- sni.post.request(ctx , data.json)
    sni.delete.request(fromJSON(response)$guid, ctx)
    index <- grep(TRUE, lapply(fromJSON(users)$values, function(o) o$id == fromJSON(response)$userId))
    user <- fromJSON(users)$values[[index]]$name
    ctx$user$login <- user
  }
  class(ctx) <- "snippetcontext"
  .session$ctx <- ctx
  ctx
}
