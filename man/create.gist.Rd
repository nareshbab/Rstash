\name{create.gist}
\alias{create.gist}
\title{
  create.gist: create a snippet in stash
}
\description{
  \code{create.gist} creates snippets in stash depending on the content passed by the user

}
\usage{
create.gist(content, ctx = myctx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{content}{json: Includes files to be created in snippet}
  \item{ctx}{list: List of snippet context containing stash API Url, RSA key path, consumer secret key and user name}
}
\details{
  This function creates a snippet in stash containing all the files that are passed in content JSON.
  The context passed by the user is used to make a POST request to snippet API by lower level functions 
  
}
\value{
  \code{create.gist} Metadata response from Stash API (list)

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
res <- create.gist(content, ctx = .session$snippet.context)
}

