\name{create.snippet.comment}
\alias{create.snippet.comment}
\title{
  create.snippet.comment: Create a comment on a snippet
}
\description{
  \code{create.snippet.comment} creates a comment in snippets in stash depending on the content passed by the user

}
\usage{
create.snippet.comment(id,content,ctx = myctx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id} {character: Snippet id}
  \item{content} {character: Content of the comment to be added}
  \item{ctx} {list: List of snippet context containing stas API Url,RSA key path,consumer secret key and user name}
}
\details{
  This function is used to create comments on a snippet.
  The context passed by the user is used to make a post request to stash API by lower level functions 
  
}
\value{
  \code{create.snippet.comment} Metadata response from Stash API (list)

}
%\references{
%}
\author{
  Mu Sigma 
}
%\note{
%}
\examples{
res <- create.snippet.comment(id, content, ctx = .session$snippet.context)
}

