options(stringsAsFactors = FALSE)
options(digits = 20)

libs = c('tidyr','hash','SDMTools','dplyr')
len = length
uni = unique
sorted = sort

luni = function(x){
	return (len(uni(x)))
}

.range = function(x){
	return(range(x,na.rm=TRUE))
}

.mean = function(x){
	return(mean(x,na.rm=TRUE))
}

.sd = function(x){
	return(sd(x,na.rm=TRUE))
}

.median = function(x){
	return(median(x,na.rm=TRUE))
}

.max = function(x){
	return(max(x,na.rm=TRUE))
}

.min = function(x){
	return(min(x,na.rm=TRUE))
}

install = function(x){
	install.packages(as.character(x),dependencies=TRUE, repos='http://cran.rstudio.com/',quiet=TRUE)
	library(x,character.only=TRUE)
	cat(sprintf("Installed and loaded %s library", x))
}

findsep <- function(f){
	if (endsWith(f,"/")){
		sep = ""
	} else {
		sep = "/"
	}
	return(sep)
}

fs <- function(dir){
	files = list.files(dir)

	sep = findsep(dir)
	
	lst = c()
	for (f in files){
		lst = c(lst,paste(dir,f,sep=sep))
	}

	return(lst)
}

fname <- function(dir,f){
	sep = findsep(dir)

	return(paste(dir,f,sep=sep))
}

for (lib in libs){
	if(lib %in% rownames(installed.packages()) == FALSE){
		install(lib)
	}
	else {
		library(lib,character.only=TRUE)
		cat(sprintf("Loaded %s library\n",lib))
	}
}