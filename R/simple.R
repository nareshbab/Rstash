require(rredis)
require(rjson)
require(httr)
redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)

create.snippet <- function(content, ctx = NULL){
  data <- fromJSON(content)
  data.json <- paste0('{ "name" : "',data$description,'","description":"',data$description,'" ,"files" : [{ "name" : "Scratch.R", "content" : "#keep snippets here while working with your notebook cells" }] }')
  curl <- paste0('curl -X POST -u nareshbabral:work4future -H "Content-Type:application/json" -d \'',data.json,'\' http://127.0.0.1:7990/rest/snippets/1.0/snippets')
  response <- system(curl, intern=T)
  require(rredis)
  redisConnect(host = "localhost", port = 6379, password = NULL,returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  res <- redisGet("notebook_res")
  res$content$id <- fromJSON(response)$guid
  res$content$description <- fromJSON(response)$name
  res$content$user$id <- fromJSON(response)$userId
  res  
}

get.snippet <- function(id, version = NULL, ctx = NULL){
  if(!is.null(version)) {
    curl <- paste0('curl -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',version,'')
    res <- system(curl, intern=T)
    res <- .get.git.res(res)
    res$content$id <- id
    res
  }else { 
    curl <- paste0('curl -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
    res <- system(curl, intern=T)
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
    data.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"files" : ',toJSON(files),' }')
    revision.json <- paste0('{ "name" : "',snippet.list$name,'","description":"',snippet.list$name,'" ,"files" : ',toJSON(files),' }')
    curl.revision <- paste0('curl -X POST -u nareshbabral:work4future -H "Content-Type:application/json" -d \'',revision.json,'\' http://127.0.0.1:7990/rest/snippets/1.0/snippets')
    revision.response <- system(curl.revision, intern=T)
    library(rredis)
    redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
    redisLPush(id, fromJSON(revision.response)[1])
    curl <- paste0('curl -X PUT -u nareshbabral:work4future -H "Content-Type:application/json" -d \'',data.json,'\' http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
    response <- system(curl, intern=T)
    res <- .get.snippet.res(id)
    res <- .get.git.res(res)
    res
  } else {
    res <- list(ok=FALSE)
    res
  }  
}

delete.snippet <- function(id){
  curl <- paste0('curl -X DELETE -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
  res <- system(curl, intern=T)
  return(res)
}

fork.snippet <- function(id, ctx = NULL) {
  curl <- paste0('curl -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
  snippet <- system(curl, intern=T)
  snippet.list <- fromJSON(snippet)
  files <- list()
  for(i in 1:length(snippet.list$files)){
      files[[i]] <-  list(name=snippet.list$files[[i]]$name, content=snippet.list$files[[i]]$content)
  }
  data.json <- paste0('{ "name" : "',snippet.list$name,'-Copy","description":"',snippet.list$name,'-Copy" ,"files" : ',toJSON(files),' }')
  curl <- paste0('curl -X POST -u nareshbabral:work4future -H "Content-Type:application/json" -d \'',data.json,'\' http://127.0.0.1:7990/rest/snippets/1.0/snippets')
  response <- system(curl, intern=T)
  res <- redisGet("notebook_res")
  res$content$id <- fromJSON(response)$guid
  res$content$description <- fromJSON(response)$name
  res$content$user$id <- fromJSON(response)$userId
  res
}

get.snippet.comments <- function(id, ctx = NULL){
  library(rredis)
  redisConnect(host = "localhost", port = 6379, password = NULL, returnRef = FALSE, nodelay=FALSE, timeout=2678399L)
  curl <- paste0('curl -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
  snippet <- system(curl, intern=T)
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
  curl <- paste0('curl -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
  snippet <- system(curl, intern=T)
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
  curl <- paste0('curl -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
  snippet <- system(curl, intern=T)
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
  snippet.files <- fromJSON(snippet)$files
  file_names <- as.vector(sapply(snippet.files, function(o) o$name))
  notebook <- redisGet("notebook_res")
  notebook$content$id <- fromJSON(snippet)$guid
  notebook$content$description <- fromJSON(snippet)$name
  notebook$content$user$id <- fromJSON(snippet)$userId
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
  return(notebook)
}

.get.snippet.res <- function(id) {
  curl <- paste0('curl -u nareshbabral:work4future -H "Content-Type:application/json" http://127.0.0.1:7990/rest/snippets/1.0/snippets/',id,'')
  res <- system(curl, intern=T)
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
