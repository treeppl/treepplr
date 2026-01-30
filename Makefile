build: 
	Rscript -e "library(\"pak\");pak::local_install(dependencies=TRUE, ask=FALSE)"

test: 
	Rscript -e "library(\"devtools\");devtools::check()"

clean:
	Rscript -e "remove.packages(\"treepplr\")"