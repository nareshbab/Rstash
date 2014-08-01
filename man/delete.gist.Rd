\name{delete.gist}
\alias{delete.gist}
\title{
  delete.gist: Delete a snippet in stash
}
\description{
  \code{delete.gist} delete a snippet when searched by id,from the stash API

}
\usage{
delete.gist(id,ctx = myctx)
}
\arguments{
  \item{id}{character: Snippet id}
  \item{ctx}{list: List of snippet context containing stas API Url,RSA key path,consumer secret key and user name}
}
\details{
  This function is used to delete a snippet based on snippet id
  The context passed by the user is used to make a DELETE request to stash API by lower level functions. 
  
}
\value{
  \code{delete.gist} No response

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
res <- delete.gist(id,ctx = .session$snippet.context)
}

