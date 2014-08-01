\name{get.gist}
\alias{get.gist}
\title{
  get.gist: Get a snippet in stash
}
\description{
  \code{get.gist} Get a snippet when searched by id,from the stash API

}
\usage{
get.gist(id, version, ctx = myctx)
}
\arguments{
  \item{id}{character: Snippet id}
  \item{version}{character: History version of the snippet id}
  \item{ctx}{list: List of snippet context containing stas API Url,RSA key path,consumer secret key and user name}
}
\details{
  This function is basicaly used to query the snippet API by snippet id.
  The context passed by the user is used to make a GET request to stash API by lower level functions. 
  
}
\value{
  \code{get.gist} Metadata response from Stash API (list)

}
%\references{
%}
\author{
  Mu Sigma 
}
%\note{
%}
\seealso{

}
\examples{
res <- get.gist(id, version, ctx = .session$snippet.context)
}

