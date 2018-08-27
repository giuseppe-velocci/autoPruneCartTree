## autoPruneCartTree
A function for R that can generate and prune a cart Tree automatically. Can be useful if many tests must be performed in a loop.
This function outputs messages to the console using print(). Remove those lines if no outuput is required.

```
@from        = formula
@dat         = data
@maxAttempts = integer, number of max attempts 
@attempts    = integer, a counter that overrides recursive solution to prevent infinite loops
@return      = a pruned tree (recursive approach)
```
