# -------------------------------------------------------------------------
# When using the PCLRC algorithm please cite the references:
# DOI: 10.1021/acs.jproteome.5b00344
# DOI: 10.1021/acs.jproteome.8b00781

# Find more info and a download for PCLRC on the following page:
# https://www.systemsbiology.nl/download-page/
# -------------------------------------------------------------------------

# This file holds the main functions that are used to create example networks

# library(GeneNet)
# library(minet)
# library(infotheo)

getHighestLinks <- function(matrix, rank.thr=0.3, verbose=FALSE)    {


    if(max(matrix, na.rm=TRUE)==0 ){
        th=0.01
        if(verbose) cat("null clr matrix \n")
    } else{                                      #get the threshold
        if(is.null(rank.thr)) {
            th <- min(matrix[matrix>0], na.rm=TRUE)
        } else{
            th <- quantile((matrix[upper.tri(matrix)]), 1-rank.thr,na.rm=TRUE)
            if(th==0){
                if(verbose) cat("threshold too low,  min of the (non-null) matrix chosen instead\n")
                th <- min(matrix[matrix>0])
            }
        }
    }

    net <- matrix
    net[which(net < th)] =0
    net[which(net >= th)] =1
    return(net)
}


PCLRC.gmm <- function(datamatrix, prob.threshold = 0.95, Niter=1000, frac=0.75, rank.thr=0.3){

    # Check if input parameters are in correct range

    if(prob.threshold > 1 || prob.threshold < 0)
        stop('prob.threshold must be between 0 and 1')

    if(frac > 1 || frac < 0)
        stop('frac must be between 0 and 1. Default 0.75')

    if(rank.thr > 1 || rank.thr  < 0)
        stop('frac must be between 0 and 1. Default 0.35')


    # Get a fraction of the total number of rows
    nsamp=round(dim(datamatrix)[1]*frac)
    Nvaliter=0

    # Create a square empty matrix with has a number of rows and columns equal
    #     to the number of columns in the input data frame
    table <- mat.or.vec(dim(datamatrix)[2], dim(datamatrix)[2])

    # Run 1000 times (Niter times)
    for( it in (1:Niter)){

        # Randomly reorder the fraction of selected samples
        samples=sample(dim(datamatrix)[1], nsamp)

        similarityMatrixSubset <- GeneNet::ggm.estimate.pcor(as.matrix(datamatrix[samples,]),
                                                             method = "static", verbose = F)
        similarityMatrixSubset = as.matrix(unclass(similarityMatrixSubset))
        similarityMatrixSubset <- similarityMatrixSubset^2


        adjSubset=minet::clr(similarityMatrixSubset)

        if(!is.na(sum(adjSubset))){

            out=getHighestLinks(adjSubset, rank.thr)

            table <- table+out
            Nvaliter=Nvaliter+1
        }
    }
    ProbMat  <- table/Nvaliter

    CorrMat = GeneNet::ggm.estimate.pcor(as.matrix(datamatrix), method = "static", verbose = F)
    CorrMat = as.matrix(unclass(CorrMat))

    CorrMatFiltered  <- CorrMat
    CorrMatFiltered[which(ProbMat<prob.threshold)] = 0
    diag(CorrMatFiltered) = 1

    networks= list(CorrMatFiltered=CorrMatFiltered,CorrMat=CorrMat,ProbMat=ProbMat,prob.threshold = prob.threshold)

    return(networks)
}


PCLRC.mi <- function(datamatrix, mi.estimator = 'mi.empirical', discretization = 'equalwidth', prob.threshold = 0.95, Niter=1000, frac=0.75, rank.thr=0.3){

    if(prob.threshold > 1 || prob.threshold < 0)
        stop('prob.threshold must be between 0 and 1')

    if(frac > 1 || frac < 0)
        stop('frac must be between 0 and 1. Default 0.75')

    if(rank.thr  > 1 || rank.thr  < 0)
        stop('frac must be between 0 and 1. Default 0.35')

    nsamp=round(dim(datamatrix)[1]*frac) #number of samples per iteration
    Nvaliter=0  #number of valid iterations (no NAs generated)

    #this table will store the number of times an interaction was selected
    table <- mat.or.vec(dim(datamatrix)[2], dim(datamatrix)[2])


    for( it in (1:Niter)){

        #randomly select the samples
        samples=sample(dim(datamatrix)[1], nsamp)

        #build similarity matrix based on mutual information

        #CONNECTIVITY based on MI
        similarityMatrixSubset = minet::build.mim(as.data.frame(datamatrix[samples,]),
                                                  estimator = mi.estimator,
                                                  disc = discretization)
        similarityMatrixSubset <- similarityMatrixSubset^2

        #infeer network for this iteration using CLR

        adjSubset <- minet::clr(similarityMatrixSubset)

        if(!is.na(sum(adjSubset))){ #valid iteration
            #extract highest links
            out=getHighestLinks(adjSubset, rank.thr)
            #collect output
            table <- table+out
            Nvaliter=Nvaliter+1
        }
    }

    ProbMat  <- table/Nvaliter
    MI.mat = minet::build.mim(as.data.frame(datamatrix),
                              estimator = mi.estimator,
                              disc = discretization)
    MI.mat.pclrc  <- MI.mat
    MI.mat.pclrc[which(ProbMat<prob.threshold)] = 0

    networks= list(MiMatFiltered=MI.mat.pclrc,
                   MiMat=MI.mat,
                   ProbMat=ProbMat,
                   estimator = mi.estimator,
                   disc = discretization,
                   prob.threshold = prob.threshold)

    return(networks)
}
