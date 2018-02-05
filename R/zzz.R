# This file contains the .onLoad, .onAttach etc.

# LEAVE THIS IN FOR NOW, BUT REMOVE IN 6 MONTHS WHEN THE NEW
# PIM PACKAGE GETS TRACTION.

.onAttach <- function(libname, pkgname){
   themessage <- paste0(
     "Loading pim version ", utils::packageVersion(pkgname),".\n",
     "  If you want to try out the code from the original publications\n",
     "  on probabilistic index models, please install the package 'pimold'\n",
     "  from R-Forge. You can use following command:\n",
     "  install.packages('pimold', repos = 'http://R-Forge.R-project.org')\n\n"
   )
   if(interactive())
    packageStartupMessage(themessage)
}
