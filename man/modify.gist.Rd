\name{modify.gist}
\alias{modify.gist}
\title{
  modify.gist: Modify a snippet
}
\description{
  \code{modify.gist} Modify files in a snippet based on user content

}
\usage{
modify.gist(id, content, ctx = myctx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{character: Id of a snippet}
  \item{content}{json:Content of the file to be modified}
  \item{ctx}{list: List of snippet context containing stas API Url,RSA key path,consumer secret key and user name}
  
}
\details{
  This function is used to add, remove or modify the existing files in a snippet.
  The context passed by the user is used to make a PUT request to the snippet API

}
\value{
  \code{modify.gist} Metadata response from stash API (list)

}
%\references{
%}
\author{
  Mu Sigma
}
%\note{
%}

\examples{
res <- modify.gist(id, content, ctx = .session$snippet.context)
}
