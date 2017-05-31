## A simple (and inefficient!) way of repeating the calculation in
## Figure 1 of Eddy, Nature Biotechnology, 2004, 22: 1315-1316.
## R.D-U, December-2011

## transitions between states
tp.1 <- 1 # Start
tp.end <- 0.1 # End, or Intron to end

te.e <- 0.9 # E to E
te.s <- 0.1 # E to splice
ts.i <- 1.0 # Splice to I
ti.i <- 0.9 # I to I


## base emisions
A.e <- C.e <- G.e <- T.e <- 0.25
A.i <- 0.4
C.i <- 0.1
G.i <- 0.1
T.i <- 0.4
A.s <- 0.05
G.s <- 0.95



transitions <- c(tp.1,
                 rep(te.e, 17),
                 te.s,
                 ts.i,
                 rep(ti.i, 6),
                 tp.end)

## Given exon, all equiprobable
## Given state, succesive draws are indep,
## thus we can reorder the counts of emisions
## at the intron.

emisions <- c(rep(A.e, 18),
              G.s,
              rep(T.i, 2),
              rep(A.i, 3),
              rep(C.i, 1),
              rep(G.i, 1))

## checks
length(transitions)
length(emisions)


sum(log(c(transitions, emisions))) ## same as in paper



##########################################################


#### A more general solution, and the fourteen state paths 

sequenceEddy <- c("C", "T", "T", "C", "A", "T", "G", "T", "G",
              "A", "A", "A", "G", "C", "A", "G", "A", "C",
              "G", "T", "A", "A", "G", "T", "C", "A")
length(sequenceEddy)


positions.5 <- c(5, 7, 9, 10, 11, 12, 13, 15, 16, 17, 19,
                 21, 22, 23)

length(positions.5)


prob.state.path <- function(pos.5, sequence = sequenceEddy) {
    ## Compute the probability of a state path for a given observed sequence
    ## All state paths are E...splice.I....
    ## so all we pass is pos.5, the position of the 5' splice site 
    
    all.transitions <- c(tp.1, 
                         rep(te.e, pos.5 - 1 - 1),
                         te.s,
                         ts.i,
                         rep(ti.i, (26 - pos.5 - 1)),
                         tp.end)

    emisions.E <- rep(0.25, pos.5 - 1)


    emisions.5 <- if (sequence[pos.5] == "G") {
                      0.95
                  } else if (sequence[pos.5] == "A") {
                      0.05
                  } else stop("splice position must be wrong") 
    
    emisions.I <- sapply(seq(pos.5 + 1, 26), function(x)
        switch(sequence[x],
               "A" = A.i,
               "C" = C.i,
               "G" = G.i,
               "T" = T.i))

    return(list(transitions = all.transitions,
                emisions = c(emisions.E, emisions.5, emisions.I),
                logP = sum(log(c(all.transitions, emisions.E, emisions.5, emisions.I)))))

}


allPs <- sapply(positions.5, function(x) prob.state.path(x)$logP)

## Posterior decoding. Same values (except for rounding error) as in paper

## positions 11, 14, and 9 are the 11th, 14th, and 9th
## position in the index of the 14 posible sites for the 5'

## (but this is numerically a bad idea)
exp(allPs[11])/sum(exp(allPs))

exp(allPs[14])/sum(exp(allPs))

exp(allPs[9])/sum(exp(allPs))



