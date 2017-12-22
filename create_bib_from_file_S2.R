dname <- dirname(sys.frame(1)$ofile)
source(sprintf('%s/imports.R',dname))

data <- read.csv(sprintf("%s/supplemental_file_S2_edited_for_responses.txt",dname))

data = data[order(data[,'Year'],decreasing=FALSE),]

papers = c()
for (paper in unique(data[,'Cite'])) {
	if (nchar(as.character(paper)) > 1)
		{papers = c(paper,papers)}
}

bib = list()
for (paper in papers){
	i = which(papers == paper)
	bib [[i]] = as.character(paper)
	df = data[which(data[,'Cite']==paper),]
	author = df[1,'Author']
	year = df[1,'Year']
	bib[[i]][1] = as.character(year)
	bib[[i]][2] = as.character(paper)
	names(bib[[i]]) = c('year','paper')
	names(bib)[i] = as.character(author)
}

text = ""
for (i in 1:len(bib)) {
	newtext = sprintf("%s %s %s\n\n", names(bib)[i], bib[[i]]['year'][[1]], bib[[i]]['paper'][[1]] )
	text = sprintf("%s%s",text,newtext)
}

write(text,file=sprintf('%s/text.txt',dname))
