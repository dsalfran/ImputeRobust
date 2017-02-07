.PHONY: all
all: README.md

R_OPTS=--no-save --no-restore --no-init-file --no-site-file

README.md: README.Rmd
	R ${R_OPTS} -e "rmarkdown::render('README.Rmd')"
