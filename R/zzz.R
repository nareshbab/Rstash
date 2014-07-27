.session <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
   .session$rc <- rediscc::redis.connect()
}
