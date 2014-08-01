\name{get.gist.comments}
\alias{get.gist.comments}
\title{
  get.gist.comments: Get the list of comments on a snippet
}
\description{
  \code{get.gist.comments} Get the list of comments in  a snippet

}
\usage{
get.gist.comments(id,ctx = myctx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{character: Snippet id}
  \item{ctx}{list: List of snippet context containing stas API Url,RSA key path,consumer secret key and user name}
}
\details{
  This function gets the list of comments in the snippet
  The context passed by the user is used to make a post request to stash API by lower level functions 
  
}
\value{
  \code{get.gist.comments} Metadata response from Stash API (list)

}
%\references{
%}
\author{
  Mu Sigma 
}
%\note{
%}
\examples{
res <- get.gist.comments(id, ctx = .session$snippet.context)
}

