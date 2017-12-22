# drag and drop this R script into R to create a bilbiography from file S1 
# (file S1 and imports.R must be in same folder as create_bib_from_file_S1.R)

dname <- dirname(sys.frame(1)$ofile)
source(sprintf('%s/imports.R',dname))
#q <- read.csv("~/tgg/quant.csv")
q <- read.csv(sprintf('%s/supplemental_file_S1.txt',dname),sep='\t')
q$issue = ""
whiches <- c()
for (title in unique(q[,'title'])){
	whiches <- c(which(q[,'title']==title)[1],whiches)
}


L <- list()
for (w in whiches){
	name <- q[w,'author']
	year <- q[w,'year']
	journal <- q[w,'source']
	title <- q[w,'title']
	if (endsWith(as.character(title),".")){
		title <- substr(as.character(title),1,nchar(as.character(title))-1)
	}
	if (endsWith(as.character(title)," ")){
		title <- substr(as.character(title),1,nchar(as.character(title))-1)
	}
	if (endsWith(as.character(journal),".")){
		journal <- substr(as.character(journal),1,nchar(as.character(journal))-1)
	}
	if (q[w,'sec_auth']!=""){
		if (q[w,'sec_auth'] == "et al"){
			sec_author <- "et al."
		}
		else {sec_author <- sprintf("& %s",q[w,'sec_auth'])}
	}
	else {
		sec_author = ""
	}
	if (is.na(q[w,'issue']) | nchar(as.character(q[w,'issue'])) == 0)
	{	
		if (is.na(q[w,'vol_chapter']) | nchar(as.character(q[w,'vol_chapter'])) == 0)
		{
			L[length(L)+1] <- paste(name,sec_author, sprintf("(%s)",q[w,'year']),sprintf("%s.",title),sprintf("%s.",journal),sprintf("%s.",q[w,'page_range']))
		}
		else 
		{
			L[length(L)+1] <- paste(name,sec_author, sprintf("(%s)",q[w,'year']),sprintf("%s.",title),sprintf("%s.",journal),sprintf("%s:",q[w,'vol_chapter']),sprintf("%s.",q[w,'page_range']))
		}
	}
	else {
		if (is.na(q[w,'vol_chapter']) | nchar(as.character(q[w,'vol_chapter']))==0)
		{
			L[length(L)+1] <- paste(name,sec_author, sprintf("(%s)",q[w,'year']),sprintf("%s.",title),sprintf("%s.",journal),sprintf("(%s):",q[w,'issue']),sprintf("%s.",q[w,'page_range']))
		}
		else
		{
			L[length(L)+1] <- paste(name,sec_author, sprintf("(%s)",q[w,'year']),sprintf("%s.",title),sprintf("%s.",journal),sprintf("%s",q[w,'vol_chapter']),sprintf("(%s):",q[w,'issue']),sprintf("%s.",q[w,'page_range']))
		}
		
	}
	
	if (!(name %in% names(L))){
		names(L)[length(L)] <- as.character(name)
	}
	else{
		num = 1
		while (paste(as.character(name),num) %in% names(L)){
			num = num + 1
		}
		names(L)[length(L)] <- as.character(paste(name,num))
	}
	
}
sorted <- sort(names(L))
L <- L[sorted]

refs <- c()
for (i in 1:length(L)){
	refs <- c(refs,sprintf("%s",L[[i]]))
}

write.table(refs,sprintf('%s/file_S1_refs.txt',dname),sep="\t",row.names=F,col.names=F)