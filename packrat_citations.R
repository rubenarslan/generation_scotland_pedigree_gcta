library(stringr)
# use internal function to read lockfile (uses readDcf)
packages = packrat:::readLockFilePackages("packrat/packrat.lock")
package_names = names(packages) # get pkg names


getbib = function(...) { # small helper to extract citations
	# allow calling it with no argument to get R citation
	name = list(...)
	if (length(name) > 0) {
		name = name[[1]]
	} else {
		name = "R"
	}
	paste0(
		str_replace(
			as.character(toBibtex(citation(...))),
			# by default the bibtex entries dont have ids, I'm using the pkg name
			"\\{\\,", paste0( "{", name, ",")) ,
		collapse = "\n")
}

bibliography = paste0(c(
	getbib(), sapply(package_names, FUN = getbib)), # get R citation and all packages
	collapse = "\n\n")

# write bibliography to file
cat(bibliography, file = "packrat_bibliography.bibtex")

# generate YAML reference with nocite
cat(paste0("
bibliography: packrat_bibliography.bibtex
nocite: |
", paste0("@", c("R", package_names), collapse = " ")))
