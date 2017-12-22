### Determine weights for h2 and qst data, make figs
source("~/imports.R")
#source("~/tgg/traits.R") # not necessary for supplemental files that already have this information
q <- read.csv('~/Desktop/supplemental_file_S1.txt',sep='\t')

error.bar <- function(x, y, upper, lower=upper, length=0.1,...)
{
	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
	{
		stop("vectors must be same length")
	}
	arrows(x,y+upper, x, ifelse(y-lower<0,0,y-lower), angle=90, code=3, length=length, ...)
}
#error.bar(xx,as.vector(values(narh,keys=names(t))),as.vector(values(narhsd,keys=names(t))))


getfams <- function(Q,bool)
{
	h <- hash()
	for (grp in unique(Q[,'trait_group']))
	{
		if (grp != "")
		{
			df <- Q[which(Q[,'trait_group'] ==grp),]
			if (bool) # if broad
			{
				df <- df[which(df[,'type_h']=='broad'),]	
			}
			else # if narrow
			{
				df <- df[which(df[,'type_h'] != 'broad'),]
			}
			df <- df[which(is.na(as.numeric(as.character(df[,'heritability'])))==FALSE),]
			sums = 0
			for (i in 1:nrow(df)){
				s <- strsplit(as.character(df[i,'tot_fam']),"-")
				if (len(s[[1]]>1)){
					sums = sums + round(mean(as.numeric(s[[1]])))
				}
				else {
					sums = sums + s[[1]][1]
				}
				# print (sums)
			}
			h[grp] = sums
		}
	}
	return(h)
}



#get tot_fam for each of the categories
h <- hash()
h['narrow'] <- getfams(q,FALSE)
h['broad'] <- getfams(q,TRUE)

q[,'brd_H2_wt']  <- as.numeric()
q[,'nar_h2_wt']  <- as.numeric()
q[,'wtd_nar_h2'] <- as.numeric()
q[,'wtd_brd_H2'] <- as.numeric()
for (row in rownames(q))
{
	if (is.na(q[row,'heritability'])==FALSE)
	{
		h.2 <- as.numeric(as.character(q[row,'heritability']))
		grp <- q[row,'trait_group']
		if (grp != "")
		{
			s <- strsplit(as.character(q[row,'tot_fam']),"-")
			if (len(s[[1]]>1)){
				fams = round(mean(as.numeric(s[[1]])))
			}
			else {
				fams = as.numeric(s[[1]][1])
			}
			# fams <- as.numeric(q[row,'tot_fam'])
			if (is.na(fams)==FALSE)
			{
				if (q[row,'type_h'] == 'broad') #broad
				{
					totfams <- as.numeric(values(h[['broad']][grp])[[1]])
					q[row,'brd_H2_wt'] <- (fams)/totfams
					q[row,'wtd_brd_H2'] <- (h.2*fams)/totfams
				}
				else #narrow
				{
					totfams <- as.numeric(values(h[['narrow']][grp])[[1]])
					q[row,'nar_h2_wt'] <- (fams)/totfams
					q[row,'wtd_nar_h2'] <- (h.2*fams)/totfams
				}
			}
		}
	}
}


#double check all weights sum to one
for (trait in uni(q[,'trait_group']))
{
	h2. <- q[which(q[,'type_h'] != 'broad'),]
	h2. <- h2.[which(h2.[,'trait_group']==trait),]
	H2. <- q[which(q[,'type_h'] == 'broad'),]
	H2. <- H2.[which(H2.[,'trait_group']==trait),]
	print(trait)
	print (sum(h2.[,'nar_h2_wt']))
	print(sum(H2.[,'brd_H2_wt']))
	print(sum(h2.[,'wtd_nar_h2']))
}



broad <- q[which(q[,'type_h'] == 'broad'),]
broad <- broad[which(broad[,'trait_group'] %in% c("","seed_and_seedling_properties") == FALSE),]
nar   <- q[which(q[,'type_h']!="broad"),]

traitgroups <- unique(q[,'trait_group'])
traitgroups <- traitgroups[which(traitgroups != "")]

broadh   <- hash()
broadhsd <- hash()
for (trait in traitgroups)
{
	df <- broad[which(broad[,'trait_group']==trait),]
	if (nrow(df) >0)
	{
		sum <- sum(as.numeric(as.character(df[,'wtd_brd_H2'])),na.rm=TRUE)
		broadh[trait] <- sum
		dfh2 <- as.numeric(as.character(df[,'heritability']))
		dfh2.wts <- as.numeric(as.character(df[,'wtd_brd_H2']))
		# broadhsd[trait] <- wt.sd(df[,'heritability'],df[,'brd_H2_wt'])
		broadhsd[trait] <- wt.sd(dfh2,dfh2.wts)
	}
}

t      <- sort(table(nar[which(nar[,'trait_group'] != ""),'trait_group']))
tnames <- sort(names(t))

narh   <- hash()
narhsd <- hash()
# for (trait in names(t))
# {
# 	df <- nar[which(nar[,'trait_group'] == trait),]
# 	if (nrow(df) > 0)
# 	{
# 		sum <- sum(as.numeric(as.character(df[,'nar_h2_wt'])),na.rm=TRUE)
# 		narh[trait] <- sum
# 		# narhsd[trait] <- wt.sd(df[,'heritability'],df[,'nar_h2_wt'])
# 		narhsd[trait] <- wt.sd(df[,'heritability'],as.numeric(df[,'nar_h2_wt']))
# 	}
# }
for (trait in names(t))
{
	df <- nar[which(nar[,'trait_group'] == trait),]
	if (nrow(df) > 0)
	{
		sum <- sum(as.numeric(as.character(df[,'wtd_nar_h2'])),na.rm=TRUE)
		narh[trait] <- sum
		# narhsd[trait] <- wt.sd(df[,'heritability'],df[,'nar_h2_wt'])
		narhsd[trait] <- wt.sd(df[,'heritability'],as.numeric(df[,'wtd_nar_h2']))
	}
}
saveRDS(narh,'~/tgg/narh.RDS')
saveRDS(narhsd,'~/tgg/narhsd.RDS')

meds = hash()
for (trait in names(t))
{
	meds[trait] <- median(nar[which(nar[,'trait_group']==trait),'heritability'],na.rm=T)
}
meds

tconv <- hash()	
tconv["plant_secondary_metabolites"]     = 'PSM'     
tconv["seed_and_seedling_properties"]    = 'SaSP'
tconv["herbivore_and_insect_resistance"] = 'HaIR'
tconv["survival"]                        = 'Survival'
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

colfunc <- colorRampPalette(c("pink","red","orange","yellow","green","blue","turquoise","lightblue"))
cols <- colfunc(len(unique(nar[,'trait_group']))-1)


#get n estimates and n spp per trait group
nn <- hash()
for (trait in keys(tconv))
{
	df <- nar[which(nar[,'trait_group']==trait),]
	df <- df[which(is.na(df[,'heritability'])==F),]
	
	df <- df %>% unite(combo,genus,spp)
	spps <- uni(df[,'combo'])
	spps <- spps[which(spps != "_")]
	cat(sprintf("%s\n",trait))
	print(sort(spps))

	nn[trait] = sprintf("%s (%s)",nrow(df),len(spps))
	#cat(sprintf("%s %s\n",trait,values(nn[trait])[[1]]))
}

sums = 0
for (trait in keys(nn))
{
	sums = sums + as.numeric(strsplit(values(nn[trait])[[1]]," ")[[1]][1])
}
sums


#pdf('~/tgg/h2.pdf',width=720)
#png('~/tgg/h2_3.png',width=720)
#png('~/Desktop/h2_4.png',width=720) # i moved to ~/tgg
pdf('~/Desktop/h2_4.pdf',width=10,height=6)
labh <- as.vector(values(narh,keys=names(tnames)))+as.vector(values(narhsd,keys=names(tnames)))+0.02
#xy <- c(0.5,1.5,2.7,4  ,5.1,6.4,7.6,8.8,9.9,11.1,12.3,13.6,14.8,15.9)
xy  <- c(0.5,1.5,2.7,4  ,5.1,6.4,7.6,8.8,9.9,11.1,12.3,13.6,14.8,15.9)
xx <- barplot(as.vector(values(narh,keys=names(tnames))),xaxt="n",ylim=c(0,1))
title(ylab=expression(paste(italic('h')^"2")~' (weighted average)'), line=2,cex.lab=1.2)
#xx <- barplot(as.vector(values(narh,keys=names(tnames))),xaxt="n",ylim=c(0,1),col=rep("white",len(t)))
text(x = xy,rep(-.05,length(xx)), labels=values(tconv,key=names(tnames)), xpd=TRUE,srt=30,cex=0.8)
error.bar(xx,as.vector(values(narh,keys=names(tnames))),as.vector(values(narhsd,keys=names(tnames))))
text(x = xx,labh, labels = values(nn,key=names(tnames)))
text(xx,values(meds,keys=names(tnames)),labels = rep('*',14),cex=2)
box(col='black')
dev.off()

#how many spp per trait
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
gymang['Vitellaria']	= 'angiosperm'
gymang['pseudotsuga']	= 'gymnosperm'
gymang['abies']			= 'gymnosperm'
gymang['araucaria']		= 'gymnosperm'
gymang['fraxinus']		= 'angiosperm'
gymang['austrocedrus']	= 'gymnosperm'
gymang['cryptomeria']	= 'gymnosperm'
gymang['Pseudotsuga']	= 'gymnosperm'
gymang['castanea']		= 'angiosperm'
gymang['prosopis']		= 'angiosperm'
gymang['swietenia']		= 'angiosperm'
gymang['Cupressus']		= 'gymnosperm'

hdf <- nar[which(is.na(nar[,'heritability'])==F),]
for (trait in sort(names(t)))
{
	ang = 0
	gym = 0
	pin = 0
	euc = 0
	pop = 0
	df <- hdf[which(hdf[,'trait_group']==trait),]
	df2 <- df %>% unite(combo,genus,spp)
	cat(sprintf("\n\n%s: tot meas = %s, uni spp = %s\n",trait,nrow(df),luni(df2[,'combo'])))
	for (row in rownames(df)){
		genus <- df[row,'genus']
		if (!(genus == "")){
			if (values(gymang[genus])[[1]] == 'angiosperm'){
				ang = ang + 1
			}
			else {
				gym = gym + 1
			}
		}
		if (genus == 'pinus'){
			pin = pin +1
		}
		if (genus == 'eucalyptus'){
			euc = euc + 1
		}
		if (genus == 'populus'){
			pop = pop + 1
		}
	}
	cat(sprintf("ang = %s gym = %s euc = %s pin = %s pop = %s\n",ang,gym,euc,pin,pop))
}


##############################################################################
############## QST STUFF #####################################################
##############################################################################
qst <- q[which(is.na(as.numeric(q[,'qst']))==FALSE),]
getQSTweights <- function(cue,H)
{
	for (row in rownames(cue))
	{
		if (is.na(cue[row,'qst'])==FALSE)
		{
			QST <- as.numeric(as.character(cue[row,'qst']))
			grp <- cue[row,'trait_group']
			if (grp != "")
			{
				fams <- as.numeric(as.character(cue[row,'tot_fam']))
				if (is.na(fams)==FALSE)
				{
					totfams <- as.numeric(values(H[grp])[[1]])
					cue[row,'weight'] <- (QST*fams)/totfams
				}
			}
		}
	}
	return(cue[,'weight'])
}

# getQSTfams <- function(q)
# {
# 	h <- hash()
# 	for (grp in unique(q[,'trait_group']))
# 	{
# 		if (grp != "")
# 		{
# 			df <- q[which(q[,'trait_group'] ==grp),]

# 			h[grp] = sum(as.numeric(df[,'tot_fam']),na.rm=T)
# 		}
# 	}
# 	return(h)
# }

#main shindig

# Q <- getQSTfams(qst)
Q <- hash() #use this instead
for (trait in uni(qst[,'trait_group'])){
	df <- qst[which(qst[,'trait_group']==trait),]
	sums = 0
	for (i in 1:nrow(df)){
		s = strsplit(as.character(df[i,'tot_fam']),"-")
		# print(s)
		if (len(s[[1]]) > 1){
			fams = round(mean(as.numeric(as.character(s[[1]]))))
		} else {
			fams = as.numeric(as.character(s[[1]][1]))
		}
		# print(fams)
		sums = sums + fams
	}
	Q[trait] = sums
}

# qst[,'qst_wt'] <- getQSTweights(qst,Q)
q[,'qst_wt']<-as.numeric()
q[,'wtd_qst'] <- as.numeric()
for (row in rownames(q))
{
	if (is.na(q[row,'qst'])==FALSE)
	{
		QST <- as.numeric(as.character(q[row,'qst']))
		grp <- q[row,'trait_group']
		if (grp != "")
		{
			s <- strsplit(as.character(q[row,'tot_fam']),"-")
			if (len(s[[1]]>1)){
				fams = round(mean(as.numeric(s[[1]])))
			}
			else {
				fams = as.numeric(s[[1]][1])
			}

			if (is.na(fams)==FALSE){
				totfams <- as.numeric(values(Q[grp])[[1]])
				q[row,'qst_wt'] <- (fams)/totfams
				q[row,'wtd_qst'] <- (QST*fams)/totfams
			}
		}
	}
}

#double check all weights sum to one
for (trait in uni(q[,'trait_group'])){
	df <- q[which(q[,'trait_group']==trait),]
	df <- df[which(is.na(df[,'qst'])==FALSE),]
	print(trait)
	print(sum(df[,'qst_wt']))
}


qst <- q[which(is.na(q[,'qst'])==FALSE),]
t2  <- sort(table(qst[which(qst[,'trait_group'] != ""),'trait_group']))

qsts   <- hash()
qstssd <- hash()
for (trait in names(t2))
{
	df <- qst[which(qst[,'trait_group'] == trait),]
	if (nrow(df) > 0)
	{
		sum <- sum(df[,'wtd_qst'],na.rm=TRUE)
		qsts[trait] <- sum
		qstssd[trait] <- wt.sd(df[,'qst'],df[,'wtd_qst'])
	}
}

L <- len(names(t2))

qq <- barplot(as.vector(values(qsts,keys=names(t2))),xaxt="n",ylim=c(-0.1,1),col=cols)
#text(x = xx,rep(0,length(xx)), labels=names(t2), xpd=TRUE,srt=30,cex=0.8)
error.bar(xx,as.vector(values(qsts,keys=names(t2))),as.vector(values(qstssd,keys=names(t2))))

qstspp <- c()
for (row in row.names(qst))
{
	spp <- paste(qst[row,'genus'],qst[row,'spp'],sep="_")
	qstspp <- c(qstspp,spp)
}
len(unique(qstspp))


nq <- hash()
for (trait in keys(tconv)){
	df   <- qst[which(qst[,'trait_group']==trait),]
	df   <- df[which(is.na(df[,'qst'])==F),]
	
	df   <- df %>% unite(combo,genus,spp)
	spps <- uni(df[,'combo'])

	nq[trait] = sprintf("%s (%s)",nrow(df),len(spps))
}

qstmeds = hash()
for (trait in names(t)){
	qstmeds[trait] <- median(qst[which(qst[,'trait_group']==trait),'qst'],na.rm=T)
}

labh <- as.vector(values(qsts,keys=names(tnames)))+as.vector(values(qstssd,keys=names(tnames)))+0.02
#xy <- c(0.5,1.5,2.7,3.8,5. ,6.3,7.5,8.8,9.9,11  ,12.3,13.6,14.8,15.9)
xy  <- c(0.5,1.5,2.7,4  ,5.1,6.4,7.6,8.8,9.9,11.1,12.3,13.6,14.8,15.9)
# png('~/Desktop/qst_2.png',width=720)
#png('~/Desktop/qst_3.png',width=720)
pdf('~/Desktop/qst_3.pdf',width=10,height=6)
xx <- barplot(as.vector(values(qsts,keys=names(tnames))),xaxt="n",ylim=c(0,1))
title(ylab=expression(paste(italic('Q')["ST"])~'(weighted average)'), line=2,cex.lab=1.2)
#xx <- barplot(as.vector(values(narh,keys=names(tnames))),xaxt="n",ylim=c(0,1),col=rep("white",len(t)))
text(x = xy,rep(-.05,length(xx)), labels=values(tconv,key=names(tnames)), xpd=TRUE,srt=30,cex=0.8)
error.bar(xx,as.vector(values(qsts,keys=names(tnames))),as.vector(values(qstssd,keys=names(tnames))))
text(x = xx,labh, labels = values(nq,key=names(tnames)))
text(xx,values(qstmeds,keys=names(tnames)),labels = rep('*',14),cex=2)
box()
dev.off()


hdf <- qst[which(is.na(qst[,'qst'])==F),]
for (trait in sort(names(t))){
	ang = 0
	gym = 0
	pin = 0
	euc = 0
	pop = 0
	df  <- hdf[which(hdf[,'trait_group']==trait),]
	df2 <- df %>% unite(combo,genus,spp)
	cat(sprintf("\n\n%s: tot meas = %s, uni spp = %s\n",trait,nrow(df),luni(df2[,'combo'])))
	for (row in rownames(df)){
		genus <- df[row,'genus']
		if (!(genus == "")){
			if (values(gymang[genus])[[1]] == 'angiosperm'){
				ang = ang + 1
			}
			else {
				gym = gym + 1
			}
		}
		if (genus == 'pinus'){
			pin = pin +1
		}
		if (genus == 'eucalyptus'){
			euc = euc + 1
		}
		if (genus == 'populus'){
			pop = pop + 1
		}
	}
	cat(sprintf("ang = %s gym = %s euc = %s pin = %s pop = %s\n",ang,gym,euc,pin,pop))
}

hdf <- r[which(is.na(r[,'r2'])==FALSE),]
gymang['corymbia']	= 'angiosperm'
gymang['fagus'] 	= 'angiosperm'
for (trait in sort(uni(r[,'trait_group']))){
	ang = 0
	gym = 0
	pin = 0
	euc = 0
	pop = 0
	df  <- hdf[which(hdf[,'trait_group']==trait),]
	df2 <- df %>% unite(combo,Genus,Spp)
	cat(sprintf("\n\n%s: tot meas = %s, uni spp = %s\n",trait,nrow(df),luni(df2[,'combo'])))
	for (row in rownames(df)){
		genus <- tolower(df[row,'Genus'])
		if (!(genus == "")){
			if (values(gymang[genus])[[1]] == 'angiosperm'){
				ang = ang + 1
			}
			else {
				gym = gym + 1
			}
		}
		if (genus == 'pinus'){
			pin = pin +1
		}
		if (genus == 'eucalyptus'){
			euc = euc + 1
		}
		if (genus == 'populus'){
			pop = pop + 1
		}
	}
	cat(sprintf("ang = %s gym = %s euc = %s pin = %s pop = %s\n",ang,gym,euc,pin,pop))
}


#### create age figs
ageh = hash()
ageh['#1_age_<1'] = '00-05'
ageh['#2_age_<1'] = '00-05'
ageh['8_month'] = '00-05'
ageh['age_<1'] = '00-05'
ageh['age_>=11'] = '10-15'
ageh['age_>1'] = '00-05'
ageh['age_0-4'] = '00-05'
ageh['age_00-05'] = '00-05'
ageh['age_1'] = '00-05'
ageh['age_1-1.5'] = '00-05'
ageh['age_1-16months'] = '00-05'
ageh['age_1-18months'] = '00-05'
ageh['age_1-2'] = '00-05'
ageh['age_1-3'] = '00-05'
ageh['age_10'] = '10-15'
ageh['age_11'] = '10-15'
ageh['age_12'] = '10-15'
ageh['age_12-15'] = '10-15'
ageh['age_13'] = '10-15'
ageh['age_13-16'] = '10-15'
ageh['age_13-17'] = '10-15'
ageh['age_13-18'] = '10-15'
ageh['age_13-19'] = '10-15'
ageh['age_13-20'] = '10-15'
ageh['age_13-21'] = '10-15'
ageh['age_13-22'] = '10-15'
ageh['age_13-23'] = '10-15'
ageh['age_14'] = '10-15'
ageh['age_14weeks'] = '00-05'
ageh['age_15'] = '10-15'
ageh['age_16'] = '15-20'
ageh['age_16-27'] = '15-20'
ageh['age_16months'] = '00-05'
ageh['age_17'] = '15-20'
ageh['age_18'] = '15-20'
ageh['age_18months'] = '00-05'
ageh['age_19'] = '15-20'
ageh['age_2'] = '00-05'
ageh['age_2_fall'] = '00-05'
ageh['age_2_spring'] = '00-05'
ageh['age_2-5'] = '00-05'
ageh['age_20'] = '15-20'
ageh['age_21'] = '>21'
ageh['age_22'] = '>21'
ageh['age_23'] = '>21'
ageh['age_24'] = '>21'
ageh['age_25'] = '>21'
ageh['age_26'] = '>21'
ageh['age_28-33'] = '>21'
ageh['age_29'] = '>21'
ageh['age_3'] = '00-05'
ageh['age_3-10'] = '05-10'
ageh['age_30'] = '>21'
ageh['age_33'] = '>21'
ageh['age_4'] = '00-05'
ageh['age_4-5'] = '00-05'
ageh['age_40'] = '>21'
ageh['age_44'] = '>21'
ageh['age_5'] = '00-05'
ageh['age_5-6'] = '00-05'
ageh['age_6'] = '05-10'
ageh['age_6.5'] = '05-10'
ageh['age_7'] = '05-10'
ageh['age_7_fall'] = '05-10'
ageh['age_7_spring'] = '05-10'
ageh['age_8'] = '05-10'
ageh['age_8-11'] = '05-10'
ageh['age_8.5'] = '05-10'
ageh['age_9'] = '05-10'
ageh['age_9-10'] = '05-10'
ageh['age_unk'] = 'unk'
ageh['age_varied'] = 'unk'
ageh['age_varied_2-6'] = '00-05'
ageh['age_varies'] = 'unk'
ageh['btwn_ages_13-18'] = '10-15'
ageh['btwn_ages_14-18'] = '10-15'
ageh['btwn_ages_14-24'] = '15-20'
ageh['btwn_ages_18-21'] = '15-20'
ageh['btwn_ages_21-24'] = '>21'
ageh['germination'] = '00-05'
ageh['gunns_ltd'] = 'unk'
ageh['jan_age_8_needle'] = '05-10'
ageh['jan_age_8_stem'] = '05-10'
ageh['ND_age_3'] = '00-05'
ageh['nov_age_7_bud'] = '05-10'
ageh['nov_age_7_needle'] = '05-10'
ageh['nov_age_7_stem'] = '05-10'
ageh['oct_age_7_bud'] = '05-10'
ageh['oct_age_7_needle'] = '05-10'
ageh['oct_age_7_stem'] = '05-10'
ageh['S_age_3'] = '00-05'
ageh['sept_age_7_bud'] = '05-10'
ageh['sept_age_7_needle'] = '05-10'
ageh['sept_age_7_stem'] = '05-10'
ageh['bud_flush_spring'] = 'unk'
ageh['age_31'] = '>21'
ageh['age_1_june'] = '00-05'
ageh['age_1_aug'] = '00-05'
ageh['age_2_june'] = '00-05'
ageh['age_2_aug'] = '00-05'
ageh['age_<1_day_83'] = '00-05'
ageh['age_at_harvest'] = 'unk'
ageh['age_<1_day_27'] = '00-05'
ageh['age_<1_day_41'] = '00-05'
ageh['age_<1_day_62'] = '00-05' 
ageh['June'] = 'unk'
ageh['Aug'] = 'unk'
ageh['age_2_july'] = '00-05'
ageh['age_2_after_aug'] = '00-05'
ageh['age_2_sept'] = '00-05'
ageh['age_34-35'] = '>21'
ageh['age_8'] = '00-05'
ageh['varied'] = 'unk'
ageh['age_2-3'] = '00-05'
ageh[' age_8'] = '05-10'
ageh['age_0-5'] = '00-05'


q[,'age_group'] = ''
for (row in rownames(q)){
	dets <- q[row,'age_and_other_nec_details']
	if (!(dets == "" | dets == "NA" | is.na(dets))){
		q[row,'age_group'] = values(ageh[q[row,'age_and_other_nec_details']])
	}
	else {
		q[row,'age_group'] = NA
	}
}

sort(uni(q[,'age_group']))

nar <- q[which(q[,'type_h']!='broad'),]
nar <- nar[which(is.na(nar[,'heritability'])==F),]
nar <- nar[which(nar[,'heritability']<1.01),]
herit <- nar[,'heritability']
groups <- nar[,'age_group']
png('~/tgg/h2_by_age_group.png')
boxplot(herit~groups,data = nar)
title(ylab=expression('Unweighted'~paste(italic('h')^'2')),line=2,cex.lab=1.2)
dev.off()

png('~/tgg/age_trends_by_trait_group.png',height=1200,width=1200)
par(mfrow=c(4,4))
for (trait in sort(names(t))){
	df <- nar[which(nar[,'trait_group']==trait),]
	boxplot(df[,'heritability']~df[,'age_group'],data=df)
	title(ylab=sprintf("%s",trait),size=10)
}
dev.off()


qst <- q[which(is.na(q[,'qst'])==FALSE),]


## qst v fst comparisons
q <- read.csv('~/tgg/supplemental_file_S1.txt',sep='\t')
qst <- q[which(is.na(q[,'qst'])==FALSE),]
luni(qst[,'title']) == 37

uni(qst[,'QST_and_FST.GST_compared']) == c(TRUE,FALSE) #make sure only T or F as options
comp <- qst[which(qst[,'QST_and_FST.GST_compared']==TRUE),]

uni(comp[,'Qst_significance_test'])
compsig <- comp[which(comp[,'Qst_significance_test']==TRUE),]


compt   = 0 #qst > fst
comptot = 0 #total qst-fst comparisons
for (title in uni(comp[,'title'])){
	df = comp[which(comp[,'title'] == title),]
	val = as.character(df[1,'qst_._fst_ratio'])
	lst <- strsplit(val,":|/")[[1]]
	if (len(lst) != 2){
		stop(sprintf("len(lst) != 2 for %s",title))
	}

	compt   = compt + as.numeric(lst[1])
	comptot = comptot + as.numeric(lst[2])

}
comptsig   = 0 #qst sig > fst
comptotsig = 0 #total statistical qst-fst comparisons
for (title in uni(compsig[,'title'])){
	df = compsig[which(compsig[,'title'] == title),]
	val = as.character(df[1,'qst_._fst_ratio'])
	lst <- strsplit(val,":|/")[[1]]
	if (len(lst) != 2){
		stop(sprintf("len(lst) != 2 for %s",title))
	}

	comptsig   = comptsig + as.numeric(lst[1])
	comptotsig = comptotsig + as.numeric(lst[2])

}
