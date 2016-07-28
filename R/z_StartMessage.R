# TODO:   Message displayed on start (modified from vegdata)
# 
# Author: Miguel Alvarez
################################################################################

.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is Lexiguel ",
            utils::packageDescription("Lexiguel", field="Version"),
            appendLF=TRUE)
}
