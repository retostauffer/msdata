

# ---------------------------------------------------
# Devtools based cmds
# ---------------------------------------------------
document:
	Rscript -e "devtools::document()"

.PHONY: docs
docs:
	Rscript -e "pkgdown::build_site()"

.PHONY: vignettes
vignettes:
	Rscript -e "devtools::build_vignettes()"

install: document
	Rscript -e "devtools::install()"

devcheck: document
	Rscript -e "devtools::check()"

test: install
	Rscript -e "tinytest::test_package('msdata')"
testwarn: install
	Rscript -e "tinytest::test_package('msdata'); warnings()"

coverage: install
	Rscript -e 'covr::report(covr::package_coverage(), file = "../coverage.html")'

# ---------------------------------------------------
# R CMD build and check (--as-cran)
# ---------------------------------------------------
packageversion:=$(shell cat DESCRIPTION | egrep Version | sed 's/Version://g')

build: document
	cd ../ && \
	R CMD build msdata
check: build
	cd ../ && \
	R CMD check --as-cran msdata_$(shell printf "%s"${packageversion}).tar.gz
