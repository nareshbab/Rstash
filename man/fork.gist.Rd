\name{fork.gist}
\alias{fork.gist}
\title{
  fork.gist: Creates a copy of a snippet in statsh
}
\description{
  \code{fork.gist} Forks a snippet in stash depending on the id passed by the user

}
\usage{
fork.gist(id, ctx = myctx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{character: Snippet id}
  \item{ctx}{list: List of snippet context containing stas API Url,RSA key path,consumer secret key and user name}
}
\details{
  This function is used to create a copy of snippet.
  The context passed by the user is used to make a POST request to stash API by lower level functions 
  
}
\value{
  \code{fork.gist} Metadata response from Stash API (list)

}
%\references{
%}
\author{
  Mu Sigma 
}
%\note{
%}
\examples{
res <- fork.gist(id, ctx = .session$snippet.context)
}

