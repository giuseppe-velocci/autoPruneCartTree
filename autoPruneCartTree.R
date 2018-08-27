## uncomment these lines if you need to install rpart + rpart.plot packages
# install.packages("rpart")
# install.packages("rpart.plot")

## require libraries
library(rpart)
library(rpart.plot)


## @from        = formula
## @dat         = data
## @maxAttempts = integer, number of max attempts 
## @attempts    = integer, a counter that overrides recursive solution to prevent infinite loops
## @return      = a pruned tree (recursive approach)
prunedTree <- function (form, dat, maxAttempts = 4, attempts = 0) {
  ## new tree
  cartTree <- rpart(formula = form, data = dat, method = "class")
  splits   <- dim(cartTree$frame)[1]
  print(splits)
  print(paste0("attempt:: ", attempts))
  
  ## recursion limit
  Attempts <- attempts +1
  ifelse (Attempts >= maxAttempts,
          return(cartTree),
          0)
  
  ## prune if too many splits
  n.max <- 5
  n.min <- 2
  if (splits > n.max) {
    min.err     <- cartTree$cptable[, 'rel error'] + cartTree$cptable[, 'xstd']
    min.err.sum <- which.min(min.err + cartTree$cptable[, 'xerror'])
    min.err.id  <- which(min.err < cartTree$cptable[, 'xerror'] )
    cp.min <- 0
    
    print(paste0("attempt: ", Attempts))  
    print(min.err.id)
    printcp(cartTree)
    
    ifelse (min.err.sum != min.err.id,
            min.err.id <- round((min.err.sum + min.err.id -0.5) /2),
            0)
    
    
    ifelse(! identical(min.err.id, integer(0)), ## if at least one value was found with the < above
           cp.min <- min.err.id[1], ## get the smaller id
           cp.min <- 1)
    
    
    cp.minsplits <- cartTree$cptable[cp.min, "nsplit"]
    
    print(paste0("min:  ", cp.min))
    print(cartTree$cptable[cp.min, "CP"])
    print(paste0("min nsplits: ", cp.minsplits)) 
    
    
    ## if still too big or too small:: repeat!
    ifelse (cp.minsplits < n.min || cp.minsplits > n.max,
            prunedTree (form, dat, maxAttempts, Attempts), ##recursive solution
            0)
    
    ## get the minor of the two
    cartTree <- prune(cartTree, cp = cartTree$cptable[cp.min, "CP"])
  } else if(splits < n.min) { ## else if no splits
    ifelse(attempts < maxAttempts,
           prunedTree (form, dat, maxAttempts, Attempts), ##recursive solution
           return(cartTree)
    ) 
  }
  
  
  ## if only ONE split
  ifelse (dim(cartTree$cptable) [1] < n.min,
          prunedTree (form, dat, maxAttempts, Attempts), ##recursive solution
          0
  )
  
  cartTree
}