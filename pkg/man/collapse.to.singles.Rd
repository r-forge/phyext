\name{collapse.to.singles}
\alias{collapse.to.singles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Collapse zero-length branches
}
\description{
This function is not being used at this point, but works. Sister function of expand.singles -> converts internal nodes with zero-length branches into singletons (in phylo4 format).  Will only work if the zero-len branch is connected to a tip.
}
\usage{
collapse.to.singles(tree, by.name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
Phylo4 object 
}
  \item{by.name}{
If this is set then tip labels are searched for this pattern and those nodes are collapsed. (see \code{tipLabels} function in phylobase).
}
}
\details{
Not currently being used.
}
\value{
A new phylogenetic tree which contains singleton nodes (possibly)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
J. Conrad Stack
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }

