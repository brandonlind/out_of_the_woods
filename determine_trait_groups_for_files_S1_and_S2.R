# get trait groupings for the phenotypes in files S1 and S2

source("~/imports.R")
### Quant gen (h2, qst)
q <- read.csv("~/tgg/supplemental_file_S1.txt",sep='\t')
trait_col <- 'trait'
groups_fname <- '~/tgg/quant_groups.RDS'
fname <- '~/tgg/quant_+trait_group.csv'

### GWAS
# q <- read.csv("~/tgg/supplemental_file_S2.txt",sep='\t')
# trait_col <- 'Trait'
# groups_fname <- '~/tgg/gwas_groups.RDS'
# fname <- '~/tgg/gwas_+trait_group.csv'


#### main shindig
q[,"trait_group"] = ""


groups <- list()
for (i in 1:nrow(q))
{
	trait <- tolower(as.character(q[i,trait_col]))
	if (trait != '')
	{
		lst <- strsplit(trait,"_|-| |\\*|:")[[1]]

		if (exists('trait_group'))
		{
			remove(trait_group)
		}

		if (any(c('flushes','flushing','yellowing','phenology','termination','break','flush','set','budset',
			'burst','foliation','season','duration','anthesis','emergence') %in% lst) | 
			all(c('flowering','time') %in% lst) | 
			all(c('leaf','senescence') %in% lst) | 
			all(c('leaf','drop') %in% lst) | 
			all(c('leaf','fall') %in% lst) |
			any(c('cessation','initiation') %in% lst) |
			all(c('growth','period') %in% lst))
		{
			trait_group <- 'phenology'
		}
		else if (all(c('mature','vol') %in% lst)){
			trait_group <- 'reproduction'
		}
		else if (any(c('browse','sideroxylonal','dignpeg','peg','dmdpeg','dignmpeg') %in% lst))
		{
			trait_group <- 'herbivore_and_insect_resistance'
		}
		else if (any(c('h','nmass') %in% lst))
		{
			trait_group <- 'resource_allocation'
		}

		else if (any(c('quambalaria','canker','spot','disease','resistance','rust','infection','lesions','incidence','health','latent','uredia',
			'spotting','symptoms','reaction','fungicidal','audpc') %in% lst))
		{
			trait_group <- 'disease_resistance'
		}

		else if (any(c('seed','germination','germinate','cotyledon','epicotyl','hypocotyl','cotyledons') %in% lst)) 
		{
			trait_group <- 'seed_and_seedling_properties'
		}

		else if (any(c('root','phosphorus','potassium','calcium','magnesium','mineral') %in% lst) && all(c('root','diameter','projected') %in% lst) ==FALSE &&
			any(c('dry_root_weight','first_order_lateral_root_number','green_root_weight','medium_root_lt','root_diameter','root_length','root_weight',
				'root_area','root_caliper','root_dry_weight','total_root_lt','root_projected_area','root_biomass','root_collar_diameter',
				'root_dry_wt') == trait) == FALSE)
			#&& !any(c('collar','order') %in% lst) && !all(c('leaf','width','ratio') %in% lst) && !'lt' %in% lst && 
			#!any(c('root_diameter','root_length','root_weight','coarse_root_number','root_biomass','root_collar_diameter','root_dry_wt',
				#'root_projected_area','total_root_lt') == trait) | !all(c('green','root') %in% lst)
		{
			if (any(c('stomata','axial') %in% lst)) {
				trait_group <- 'leaf_and_needle_properties'
			}
			else {
				trait_group <- 'resource_allocation'
			}
		}

		else if (any(c('serotiny','seronity','pollen','cones','cone','reproduction','reproductive','juv','strobili','fraction','index','flowering') %in% lst) 
			&& !all(c('cold','stem') %in% lst))
		{
			if ('fraction' %in% lst){
				trait_group <- 'resource_allocation'
			}
			else if (any(c('chlorophyll','leaf') %in% lst)) {
				trait_group <- 'leaf_and_needle_properties'
			}
			else if (any(c('cold','-20c','injury') %in% lst)) {
				trait_group <- 'cold_hardiness'
			}
			else if (any(c('branch','stem','harvest','shoot','volume') %in% lst)) {
				trait_group <- 'growth'
			}
			else {
				trait_group <- 'reproduction'
			}
		}

		else if (any(c('fork','straightness','leaning','circumference','form','axis','forks','forking','projected','foxtail','foxtails','texture',
			'NOD','nod','sinuosity','taper','crown','light') %in% lst) | 
			all(c('branch','angle') %in% lst) | 
			all(c('branch','whorl','ratio') %in% lst) | 
			all(c('n','branches') %in% lst) |
			all('crown' %in% lst, any(c('width','diameter') %in% lst)) |
			all(any(c('proleptic','sylleptic') %in% lst), !'leaf' %in% lst) |
			all(c('branch','area','crown') %in% lst) |
			all(c('branches','per','whorl') %in% lst) | 
			all('canopy' %in% lst , any(c('depth','radius') %in% lst)) )
		{
			trait_group <- 'form'
		}

		else if (any(c('avln','nitrogen','N15','n15','stem+branch','ht:dbh','n') %in% lst)) #&& !'2002' %in% lst
		{
			if (any(c('adaxial','axial','pinnae','leaflet','lateral') %in% lst)) {
				trait_group <- 'leaf_and_needle_properties'
			}
			if ('alive' %in% lst){
				trait_group <- 'survival'
			}
			if ('brances' %in% lst){
				trait_group <- 'form'
			}
			else if ('cracks' %in% lst){
				trait_group <- 'form'
			}
			else {
				trait_group <- 'resource_allocation'
			}
		}

		else if (all('ratio' %in% lst, 
			!any(c('cabon', 'leaflet', 'petiole', 'whorl', 'latewood') %in% lst) ) )
		{
			trait_group <- 'resource_allocation'
		}

		else if (any(c('den','rings','gravity','latewood','hypoderm','earlywood','density','tannin','tannins','microfibril','lignin','lignan','wood',
			'stiffness','lignification','branch','branches','elasticity','pulp','coarseness','curl','tensile','longitudinal','spiral',
			'pinosylvin','pimaric','abietic','resin','fatty','sitosterol','spiral-grain','cracks','extractive','cell','glands','moisture',
			'prolepsis','arabinose','galactose','glucose','xylose','syringyl','holocellulose','cellulose','hemicellulose','mannose') %in% lst) && 
			all(any(c('leaf','ad:ab','ad') %in% lst) == FALSE, 
				all(c('branch','biomass') %in% lst) ==FALSE,
				all(c('branch','dia') %in% lst) == FALSE, 
				all(c('branch','diameter') %in% lst) == FALSE,
				all(c('branch','number') %in% lst) == FALSE,
				any(c('weight','length','lengths','size','thickness','whorl','diam','lpi','photosynthetic') %in% lst) == FALSE))
		{
			trait_group <- 'wood_properties'
		}

		else if (any(c('lig','c6','ewsg','lwsg','lw','s','modulus','microfibril','fiber','fibre','cell','vein','wall',
			'cad','ccr1','pal1','cas3','cesa2','cesa9','atub','btub','aip','eif4a','knat7','mads',
			'myb2','myb4','120pr','adekin','bkacps','bqr','bmp1','gmp1','gmp2','importin','lp6','plr','ugp','xet3','xgft7',
			'xxt1','nh','averwoodden','crystallinity','hc','fw','kpy','mfa','fl') %in% lst | all(c('percentage','ew') %in% lst)))
		{
			trait_group <- 'wood_properties'
		}
		else if (all(c('presence','absence','vegetative') %in% lst)) {
			trait_group <- 'reproduction'
		}
		else if (all(c('specific','surface','area') %in% lst))
		{
			trait_group <- 'wood_properties'
		}
		else if (any(c('paqueb','shrinkage','collapse','rupture') %in% lst))
		{
			trait_group <- 'wood_properties'
		}
		else if (any(c('amax/mass','chlsummer','pn','cond','fluorescence','quatum','effeciency') %in% lst))
		{
			trait_group <- 'leaf_and_needle_properties'
		}
		else if ('area' == lst[1])
		{
			trait_group <- 'leaf_and_needle_properties'
		}

		else if (all(c('ring','width') %in% lst) | 
			all(any(c('dry','DIA','bioimass','basal','mass','calliper','caliper','height','ht','length','dbh','volume','vol','biomass','growth',
				'increment','DBH','diameter','dia','weight','wt','shoots','buds','whoorls','bud','shoot','brances','whorls','hon3',
				'brs','rhgr','lengths','epicormics','lignotuber','nodes','collar','order','root') %in% lst), 
				any(c('resin','leaf','needle','petiole','pinnae','leaflet','photosynthetic','cold') %in% lst) == FALSE) | 
			all(c('whole','tree','mass') %in% lst) && !'photosynthetic' %in% lst |
			all(c('branch','size') %in% lst)  && !'photosynthetic' %in% lst |
			all(c('number','stem','units') %in% lst) && !'photosynthetic' %in% lst |
			all(c('stem','index') %in% lst) |
			all(c('stem','mass') %in% lst) | 
			all(c('stem','fraction') %in% lst))
		{
			if ('lpi' %in% lst)
			{
				trait_group <- 'leaf_and_needle_properties'
			}
			else if ('tracheid' %in% lst)
			{
				trait_group <- 'wood_properties'
			}
			else if ('brs' %in% lst | 'order' %in% lst)
			{
				trait_group <- 'form'
			}
			else if (any(c('initiation') %in% lst))
			{
				trait_group <- 'phenology'
			}
			else if ('lignotuber' %in% lst){
				trait_group <- 'wood_properties'
			}
			else 
			{
				trait_group <- "growth"
			}
		}	

		else if (all(any(c('thickest','thickness') %in% lst), all(c('wall','cell') %in% lst) == FALSE ))
		{
			trait_group <- 'growth'
		}

		else if (('nitrogen' %in% lst) == FALSE && 
			('concentration' %in% lst) == FALSE && 
			any(c('discrimination','drought','electron','p50','wue','c13','13c','carbon','δ13c','efficiency','c13-2000','c13-2002',
				'photosynthetic') %in% lst))
		{
			trait_group <- "drought_hardiness"
		}

		else if (all(c('carbon','concentration','sensitivity') %in% lst))
		{
			trait_group <- "drought_hardiness"
		}
		else if (any(c('sensitivity','lmaspring') %in% lst)) {
			trait_group <- 'leaf_and_needle_properties'
		}
		else if (any(c('vi67','vi45','girth') %in% lst)){
			trait_group <- 'growth'
		}
		else if ('ref' %in% lst){
			trait_group <- 'drought_hardiness'
		}
		else if (all(c('total','carbon','concentration') %in% lst)){
			trait_group <- 'growth'
		}

		else if (any(c('survival','alive','wilting','mortality','necrosis','death') %in% lst))
		{
			trait_group <- 'survival'
		}

		else if (any(c('frost','injury','hardiness','cold','electrolyte') %in% lst))
		{
			trait_group <- 'cold_hardiness'
		}

		else if (any(c('sawfly','defoliation','beetle','phenolic','phenolics','attack','catalase','proline','superoxide') %in% lst))
		{
			trait_group <- 'herbivore_and_insect_resistance'
		}

		else if (any(c('cnpl','la','leaf','needle','leaflet','ad:ab','stomata','foliage','abaxial','adaxial','ad','ab',
			'chlorophyll','leaves','leafiness','needles','petiole','lpi','photosynthetic','stomatal','pinnae','photosynthesis','lfs','lamina') %in% lst))
		{
			if (any(c('weight','biomass') %in% lst)){
				trait_group <- 'resource_allocation'
			}
			else {
				trait_group <- 'leaf_and_needle_properties'
			}
		}
		else if (any(c('sla','carotenoid') %in% lst)){
			trait_group <- 'leaf_and_needle_properties'
		}

		else if (any(c('p27min','p28min','p39min','p48min','alanine','cellobiotol','isoleucine','phenylalanine','phytol','raffinose',
			'arachidic','arabitol','macrocarpal','oils','oil','cineole','cymene','cineole','terpineol','nerol','terpinyl_acetate','neryl_acetate',
			'pincarveol','aromadendrene','bcyclogermacrene','globulol','virdifloral','eudesmyl_acetate','terpinyl','neryl',
			'eudesmyl','extractives','eudesmol','maca','cin','totfpc','geudesmo','sumchem1','sumsesq','mon','apin','summ',
			'malondialdehyde','peroxidase') %in% lst))
		{
			trait_group <- 'plant_secondary_metabolites'
		}

		#keep track of which trait goes in which group, and make a dictionary of the vectors
		if (exists('trait_group'))
		{
			q[i,'trait_group'] <- trait_group
			if (trait_group %in% names(groups) == FALSE)
			{
				groups[[len(groups)+1]] <- trait
				names(groups)[len(groups)] <- trait_group
			}
			else if (trait %in% groups[trait_group][[1]] == FALSE)
				{
					groups[trait_group][[1]] <- sort(c(groups[trait_group][[1]],trait))
				}
		}
	}
}

#write.table(left,"~/tgg/left.csv",sep=",",row.names=F)
saveRDS(groups,groups_fname)
write.table(q,fname,sep=',',row.names=F)








