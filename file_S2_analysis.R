source('imports.R')
q <- read.csv('~/Desktop/supplemental_file_S2.txt',sep='\t')

range(q[,'r2'])

#png('~/tgg/r2_hist.png')
png('~/Desktop/r2_hist.png')
pdf('~/Desktop/r2_hist.pdf',width=5,height=5)
hist(log10(q[,'r2']),breaks = c(-4.0,-3.75,-3.5,-3.25,-3.0,-2.75,-2.5,-2.25,-2.0,-1.75,-1.5,-1.25,-1.0,-0.75,-0.5,-0.25,0.0),
	xaxt="n",col="grey",main ="",xlab="",ylab="")
axis(1,at=c(-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0),labels=rep("",9))
text(x = c(-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0), y = rep(-78,9), labels = c('0.0001','0.0003','0.0010','0.0032','0.0100','0.0316',
	'0.1000','0.3162','1.0000'),xpd=TRUE, srt=30)
title(xlab=expression(italic('r')^"2"))
title(ylab="Count",line=2.25,cex.lab=1.2)
box()
dev.off()

# r2 <- hist(log10(q[,'r2']),breaks = c(-4.0,-3.75,-3.5,-3.25,-3.0,-2.75,-2.5,-2.25,-2.0,-1.75,-1.5,-1.25,-1.0,-0.75,-0.5,-0.25,0.0),xaxt="n",
# 	col=c("blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","white","white","white","white","white","white"))
# axis(1,at=c(0.00010,0.00018,0.00032,0.00056,0.00100,0.00178,0.00316,0.00562,0.01000,0.01778,0.03162,0.05623,0.10000,0.17783,0.31623,0.56234,1.00000))

l <- log10(q[,'r2'])

len.lt.neg1pt5 <- l[which(l< -1.5)]
len(len.lt.neg1pt5)

png('~/tgg/r2_hist.png')
r2 <- hist(log10(q[,'r2']),breaks = c(-4.0,-3.75,-3.5,-3.25,-3.0,-2.75,-2.5,-2.25,-2.0,-1.75,-1.5,-1.25,-1.0,-0.75,-0.5,-0.25,0.0)
	,xaxt="n",xlab="",main="")
axis(1,at=c(-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0))
title(xlab=expression('Log'[10]~"("~italic('r')^"2"~")"))
box()
dev.off()

tconv <- hash()
#from weights_and_h2qst_figs.R
tconv["plant_secondary_metabolites"]     = 'PSM'     
# tconv["seed_and_seedling_properties"]    = 'SaSP'     # don't need this category
tconv["herbivore_and_insect_resistance"] = 'HaIR'    
# tconv["survival"]                        = 'Survival' # don't need this category
tconv["drought_hardiness"]               = 'DroughtH'
tconv["disease_resistance"]              = 'DisRes'
tconv["reproduction"]                    = 'Reprod'
tconv["leaf_and_needle_properties"]      = 'LaNP'
tconv["cold_hardiness"]                  = 'ColdH'
tconv["resource_allocation"]             = 'ResAllo'
tconv["form"]                            = 'Form'
tconv["wood_properties"]                 = 'WoodProp'
tconv["phenology"]                       = 'Phen'
tconv["growth"]							 = 'Growth'
# added:
tconv["plant_secondary_metabolites"]     = 'PSM'     
tconv["herbivore_and_insect_resistance"] = 'HaIR'
tconv["drought_hardiness"]               = 'DroughtH'
tconv["disease_resistance"]              = 'DisRes'
tconv["reproduction"]                    = 'Reprod'
tconv["leaf_and_needle_properties"]      = 'LaNP'
tconv["cold_hardiness"]                  = 'ColdH'
tconv["resource_allocation"]             = 'ResAllo'
tconv["form"]                            = 'Form'
tconv["wood_properties"]                 = 'WoodProp'
tconv["phenology"]                       = 'Phen'
tconv["growth"]							 = 'Growth'

x <- q[which(q[,'trait_group'] != ""),]
x <- x[which(is.na(x[,'r2'])==FALSE),]

n <-hash()
for (trait in keys(tconv)){
	cat(sprintf (c('\n',trait,'\n')))
	df <- x[which(x[,'trait_group']==trait),]
	df <- df %>% unite(combo,Genus,Spp)
	spps <- uni(df[,'combo'])
	print (sort(spps))
	n[trait] = sprintf("%s (%s)",nrow(df),len(spps))
}
tnames <- sort(uni(x[,'trait_group']))
xy <- c(0.8, 1.9, 2.7, 
		3.8, 4.8, 5.9, 
		6.9, 7.9, 8.9, 
		9.8,  10.8,  11.7)
png('~/Desktop/r2_by_trait_group.png',width=650)
pdf('~/Desktop/r2_by_trait_group.pdf',width=9.2,height=6)
xx <- boxplot(x[,'r2']~x[,'trait_group'],outline=FALSE,title="",xaxt="n",ylim=c(-0.01,0.25),col='grey')
lab <- as.vector(rep(-0.015))
axis(side=1,at=1:12,labels=rep("",len(xy)))
title(ylab=expression(paste(italic('r')^"2")), line=2,cex.lab=1.2)
text(x = xy,y =rep(-0.039,length(xx)), labels=values(tconv,key=names(tnames)), xpd=TRUE,srt=30,cex=0.8)
grp = hash()
for (group in 1:len(xx$group)){
	grp[group] = xx$group[group]
}
text(x = 1:len(tnames),y=lab,labels=values(n,keys=tnames))
points(jitter(values(grp,keys=1:len(xx$group)),factor=1), xx$out)
dev.off()


####### table 3
gymang <- hash()
gymang['pinus']			= 'gymnosperm'
gymang['psuedotsuga']	= 'gymnosperm'
gymang['quercus']		= 'angiosperm'
gymang['eucalyptus']	= 'angiosperm'
gymang['salix']			= 'angiosperm'
gymang['betula']		= 'angiosperm'
gymang['populus']		= 'angiosperm'
gymang['picea']			= 'gymnosperm'
gymang['ulmus']			= 'angiosperm'
gymang['pseudostuga']	= 'gymnosperm'
gymang['melaleuca']		= 'angiosperm'
gymang['vitellaria']	= 'angiosperm'
gymang['pseudotsuga']	= 'gymnosperm'
gymang['abies']			= 'gymnosperm'
gymang['araucaria']		= 'gymnosperm'
gymang['fraxinus']		= 'angiosperm'
gymang['austrocedrus']	= 'gymnosperm'
gymang['cryptomeria']	= 'gymnosperm'
gymang['pseudotsuga']	= 'gymnosperm'
gymang['castanea']		= 'angiosperm'
gymang['prosopis']		= 'angiosperm'
gymang['swietenia']		= 'angiosperm'
gymang['cupressus']		= 'gymnosperm'
gymang['corymbia']		= 'angiosperm'
gymang['fagus']			= 'angiosperm'


for (trait in sort(uni(x[,'trait_group'])))
{
	ang = 0
	gym = 0
	pin = 0
	euc = 0
	pop = 0
	df <- x[which(x[,'trait_group']==trait),]
	df2 <- df %>% unite(combo,Genus,Spp)
	cat(sprintf("\n\n%s: tot meas = %s, uni spp = %s\n",trait,nrow(df),luni(df2[,'combo'])))
	for (row in rownames(df))
	{
		genus <- tolower(df[row,'Genus'])
		if (!(genus == ""))
		{
			if (values(gymang[genus])[[1]] == 'angiosperm')
			{
				ang = ang + 1
			}
			else 
			{
				gym = gym + 1
			}
		}
		if (genus == 'pinus')
		{
			pin = pin +1
		}
		if (genus == 'eucalyptus')
		{
			euc = euc + 1
		}
		if (genus == 'populus')
		{
			pop = pop + 1
		}
	}
	cat(sprintf("ang = %s gym = %s euc = %s pin = %s pop = %s\n",ang,gym,euc,pin,pop))
}

# determine avg number of snps associated to multiple phenotypes per study
mult <- c()
for (cite in uni(q$Cite))
{
	df <- q[which(q$Cite == cite),]
	perdf = 0 # the number of loci in this study that were associated to multiple phenotypes
	for (locus in uni(df$Gene))
	{
		if (len(strsplit(tolower(locus),'unk')[[1]]) == 1)
		{
			if (is.na(locus) == FALSE)
				{
				if (locus != 'unk'){
				df2 <- df[which(df$Gene == locus),]
				if (luni(df2$Trait) > 1)
					{
					print('yup')
					perdf = perdf + 1
					}
				}

			}
		}
	}
	mult <- c(mult,perdf)
}

len(mult) == luni(q$Cite)
mean(mult)

# correct for multiple years determine avg number of snps associated to multiple phenotypes per study
getans <- function(d.f2){
	if (nrow(d.f2) == 1){
		return(1)
	} 
	print(sort(uni(d.f2$Trait)))
	cat(sprintf("\n"))
	resp <- menu(c("Yes","No"),title="Do we need a closer look?")
	if (resp == 1)
	{
		if (luni(d.f2$Trait) == 2)
		{
			ans <- 1
		} else {
			ans <- readline("how many effective phenotypes?")
		}
	} else {
		ans <- luni(d.f2$Trait)
	}
	return(ans)
}

mult2 <- list() # a list of vector-elements for each study, where the vector is of len(loci) 
				# and each of the vector's elements are the number of effective phenotypes associated to that locus
citecount = 0
for (cite in uni(q$Cite))
{
	cat(sprintf ("%s\n",cite))
	citecount = citecount + 1
	df <- q[which(q$Cite==cite),]
	eff <- c() # a vector where the elements are the effective number of traits associated to the locus under consideration
	for (locus in uni(df$Gene))
	{
		ans <- 'NA'
		if (len(strsplit(tolower(locus),'unk')[[1]]) == 1)
		{
			if (is.na(locus) == FALSE)
			{
				if (locus != 'unk')
				{
					df2 <- df[which(df$Gene==locus),]
					if (luni(df2$Trait) > 1) # if a given locus is associated to multiple traits
					{
						ans <- getans(df2)
						#x = 'hey'
					} else if (luni(df2$Spp) > 1){
							ans <- c()
							for (spp in uni(df2$Spp)){
								spdf <- df2[which(df2$Spp ==spp),]
								ans <- c(ans,getans(spdf))
							}
					} else if (luni(df2$Trait) == 1){
						ans <- 1
					} else {
						stop('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
					}
				}
			}
		}
		eff <- c(eff,ans)
	}
	# if (is.null(eff)){
	# 	eff <- c('NA')
	# }
	mult2[[citecount]] <- eff
	names(mult2)[citecount] <- cite
}
saveRDS(mult2,'~/tgg/mult2.RDS')

# iterate through mult2 to determine avg number of snps associated with mult effective phenotypes per study
efps <- c() # number of loci from each study associated with multiple effective phenotypes
for (cite in names(mult2)){
	lst <- mult2[[cite]]
	if (lst != 'NA'){
		effs <- len(which(lst > 1))
		efps <- c(efps,effs)
	} else { # for studies that had one sig hit that did not associate to multiple traits, or that was a locus that was 'unknown' 
		efps <- c(efps,0)
	}
}
mean(efps)
len(efps) == luni(q[,'Cite'])





