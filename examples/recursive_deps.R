## Recursive deps for two concurrents
pkg1 <- recursive_deps("reshape2")
pkg2 <- recursive_deps("tidyr")

## Compare number of dependencies
length(pkg1)
length(pkg2)

## Show the dependencies
pkg1
pkg2
