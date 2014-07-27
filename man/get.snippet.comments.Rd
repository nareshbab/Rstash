\name{get.snippet.comments}
\alias{get.snippet.comments}
\title{
  get.snippet.comments: Get the list of comments on a snippet
}
\description{
  \code{get.snippet.comments} Get the list of comments in  a snippet

}
\usage{
get.snippet.comments(id,ctx = myctx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{Character: Snippet id}
  \item{ctx}{List: List of snippet context containing stas API Url,RSA key path,consumer secret key and user name}
}
\details{
  This function gets the list of comments in the snippet
  The context passed by the user is used to make a post request to stash API by lower level functions 
  
}
\value{
  \code{get.snippet.comments} Metadata response from Stash API (list)

}
%\references{
%}
\author{
  Mu Sigma 
}
%\note{
%}
\examples{
res <- get.snippet.comments(id,ctx = .session$snippet.context)
}

