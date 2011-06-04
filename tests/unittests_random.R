##### Simulate random trees, write them, read them, and compare and log the results
####
###
require(phyext)
source("diagnostics.R")

dolog = TRUE
treelog = "origtrees.log"
simlog = "simmaptrees.log"
testslog = "tests.log"

ntrees = 100

tsizes = sample(10:250,ntrees,replace=TRUE)
transprobs = runif(ntrees)


if(dolog)
{
	treelog = file(treelog,"w"); cat("Starting...\n",file=treelog)
	simlog = file(simlog,"w"); cat("Starting...\n",file=simlog)
	testslog = file(testslog,"w"); cat("Starting...\n",file=testslog)
}

for( i in seq(ntrees) )
{

	ape_tree = rcoal(tsizes[i],br=runif(9,5,15))
	phy_tree = as(ape_tree,"phylo4")
	#
	# Discrete trait data:
	# (Loosely simulate the evolution of a binary trait down a tree, 
	#  choosing a random value for the tip and then changing 
	#  the trait based on the the change matrix.)
	#
	phy_tree = reorder(phy_tree,order="preorder")
	sample_binarydata = data.frame(rep(0,(tsizes[i]*2 -1) )) # junk data
	names(sample_binarydata) <- "hasTrait"
	sample_binarydata[rootNode(phy_tree),1] = sample(c(0,1),1)  # choose root value
	changemat = matrix(c((1-transprobs[i]),transprobs[i],transprobs[i],transprobs[i]),byrow=T,nrow=2) # prob. of changing on branch
	sample_edges = integer(0)  # track edges where change occurs
	for(j in seq(nrow(edges(phy_tree))))
	{
		anc = edges(phy_tree)[j,1]
		desc = edges(phy_tree)[j,2]
		if(anc == 0)
			next

		newvalue = sample(c(0,1),1,prob=changemat[(sample_binarydata[anc,1]+1),])
		sample_binarydata[desc,1] = newvalue
		if( newvalue != sample_binarydata[anc,1] )
			sample_edges = append(sample_edges,j)
	}

	#
	#
	# Subnodes
	# (put subnodes on the branches where the trait changed showing
	#  where on the branch the change happened.  Note that the 
	#  subnode data.frame must have the same structure as the 
	#  parent data.frame)
	#
	tmpdata = as.integer(!sample_binarydata[edges(phy_tree)[sample_edges,2],1])
	sample_subnodedata = data.frame(tmpdata)
	names(sample_subnodedata) <- "hasTrait" 
	#
	# Position the subnodes along their respective branches
	# using the fraction of the overall branch length.  In 
	# this case, I'll put them near the middle of the branch.
	sample_positions = runif(n=length(tmpdata),min=0.05,max=0.95) 
	#
	# Create the phylo4d_ext object, which first creates 
	# a phylo4d object and then adds the subnodes to it.
	#
	# NOTE: any "extra" arguments (all.data) are passed 
	# on to the phylo4d constructor via the ellipsis argument (...).
	phyext_tree = phyext(phy_tree, 
				snode.data=sample_subnodedata, 
				snode.branch=edges(phy_tree)[sample_edges,], 
				snode.pos=sample_positions,
				all.data=sample_binarydata)

	# nodes, subnodes, and branches without the "trait" (state 0) are colored black
	# nodes, subnodes, and branches with the "trait" (state 1) are colored red
	#plot(phyext_tree,states=c(0,1),states.col=c("black","red"))

	text_tree = capture.output(write.simmap(phyext_tree,file=stdout()))

	if(dolog){
		write.tree(ape_tree,file=treelog,append=TRUE)
		cat(text_tree,file=simlog,append=TRUE,sep="\n")
	}

	reread_tree = phyext(read.simmap(text = text_tree))
	
	cmpres = cmptrees(reread_tree, phyext_tree)
	if(cmpres){	
		cat(i, "SUCCESS\n",file=testslog,append=TRUE)
	} else {
		cat(i, "FAILURE\n",file=testslog,append=TRUE)
	}
}

if(dolog)
{
	close(simlog)
	close(treelog)
	close(testslog)
}



