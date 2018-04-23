
# Get LH category and resilience for marine fin/shellfish stocks identified to species-level
# By Chris Free, Rutgers University

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tools)
library(plyr)
library(dplyr)
library(freeR)
library(ggplot2)
library(rfishbase)
library(datalimited)
library(FishLife)

# Directories
datadir <- "data/fao_landings/data"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_all.Rdata", sep="/"))
lh_catg_key <- read.csv(paste(datadir, "family_to_mprm_lh_catg_key.csv", sep="/"), as.is=T)


# Inspect mPRM life history classification
################################################################################

# Inspect mPRM life history classifications
mprm_stocks <- datalimited::ram_prm_dat %>% 
  select(stockid, stocklong, scientificname, species_cat, spp_category, res) %>% 
  unique()


# Identify species of interest
################################################################################

# Identify species to look up
# Marine wild capture finfish/inverts resolved to species
spp_lh_key <- landings %>% 
  # Remove irrelevant taxa
  filter(area_type=="marine") %>% 
  filter(prod_type=="Capture production") %>% 
  filter(class %in% c("Pisces", "Mollusca", "Crustacea", "Invertebrata Aquatica")) %>% 
  filter(taxa_level=="species") %>% 
  # Reduce to unique species
  select(sci_name, comm_name) %>% 
  unique() %>%
  # Create correct sci name column
  rename(sci_name_orig=sci_name) %>% 
  mutate(sci_name_fb=trimws(sci_name_orig),
         sci_name_fb=revalue(sci_name_fb, c("Penaeus kerathurus"="Melicertus kerathurus",
                                            "Psetta maxima"="Scophthalmus maximus",
                                            "Balistes carolinensis"="Balistes capriscus",
                                            "Diplodus sargus"="Diplodus sargus sargus",
                                            "Makaira indica"="Istiompax indica",
                                            "Tetrapturus audax"="Kajikia audax",
                                            "Sardinops ocellatus"="Sardinops sagax",
                                            "Conger orbignyanus"="Conger orbignianus",
                                            "Cheilodactylus bergi"="Nemadactylus bergi",
                                            "Tetrapturus albidus"="Kajikia albida",
                                            "Diplodus argenteus"="Diplodus argenteus argenteus",
                                            "Notothenia gibberifrons"="Gobionotothen gibberifrons",
                                            "Notothenia squamifrons"="Lepidonotothen squamifrons",
                                            "Nototheniops nudifrons"="Lindbergichthys nudifrons",
                                            "Pagothenia hansoni"="Trematomus hansoni",
                                            "Patagonotothen brevicauda"="Patagonotothen brevicauda brevicauda",
                                            "Raja georgiana"="Amblyraja georgiana",
                                            "Raja cyclophora"="Atlantoraja cyclophora",
                                            "Rioraja agassizi"="Rioraja agassizii",
                                            "Raja castelnaui"="Atlantoraja castelnaui",
                                            "Anguilla australis"="Anguilla australis australis",
                                            "Notothenia acuta"="Gobionotothen acuta",
                                            "Nototheniops mizops"="Lindbergichthys mizops",
                                            "Pleuragramma antarcticum"="Pleuragramma antarctica",
                                            "Zenopsis nebulosus"="Zenopsis nebulosa",
                                            "Raja taaf"="Amblyraja taaf",
                                            "Ariomma indica"="Ariomma indicum",
                                            "Arius thalassinus"="Netuma thalassina",
                                            "Solea lascaris"="Pegusa lascaris",
                                            "Aspitrigla cuculus"="Chelidonichthys cuculus",
                                            "Raja radiata"="Amblyraja radiata",
                                            "Raja naevus"="Leucoraja naevus",
                                            "Raja circularis"="Leucoraja circularis",
                                            "Alectis alexandrinus"="Alectis alexandrina",
                                            "Mugil soiuy"="Liza haematocheila",
                                            "Alosa pontica"="Alosa immaculata",
                                            "Macrozoarces americanus"="Zoarces americanus",
                                            "Merluccius gayi"="Merluccius gayi gayi",
                                            "Mullus barbatus"="Mullus barbatus barbatus",
                                            "Clupea pallasii"="Clupea pallasii pallasii",
                                            "Strangomera bentincki"="Clupea bentincki",
                                            "Pseudopentaceros richardsoni"="Pentaceros richardsoni",
                                            "Larimichthys croceus"="Larimichthys crocea",
                                            "Sardinops melanostictus"="Sardinops sagax",
                                            "Liza haematocheilus"="Liza haematocheila",
                                            "Etrumeus teres"="Etrumeus sadina",
                                            "Raja batis"="Dipturus batis",
                                            "Gymnocephalus cernuus"="Gymnocephalus cernua",
                                            "Emmelichthys nitidus"="Emmelichthys nitidus nitidus",
                                            "Trachyscorpia echinata"="Trachyscorpia cristulata echinata",
                                            "Raja oxyrinchus"="Dipturus oxyrinchus",
                                            "Raja fullonica"="Leucoraja fullonica",
                                            "Chelidonichthys lastoviza"="Trigloporus lastoviza",
                                            "Raja alba"="Rostroraja alba",
                                            "Dasyatis violacea"="Pteroplatytrygon violacea",
                                            "Raja hyperborea"="Amblyraja hyperborea",
                                            "Notothenia kempi"="Lepidonotothen squamifrons",
                                            "Salvelinus alpinus"="Salvelinus alpinus alpinus",
                                            "Raja lintea"="Rajella lintea",
                                            "Lampanyctus achirus"="Nannobrachium achirus",
                                            "Valamugil seheli"="Moolgarda seheli",
                                            "Raja fyllae"="Rajella fyllae",
                                            "Cypselurus agoo"="Cheilopogon agoo",
                                            "Pseudopentaceros wheeleri"="Pentaceros wheeleri",
                                            "Oncorhynchus masou"="Oncorhynchus masou masou",
                                            "Aspius aspius"="Leuciscus aspius",
                                            "Sardinops caeruleus"="Sardinops sagax",
                                            "Centroscymnus owstoni"="Centroscymnus owstonii",
                                            "Parika scaber"="Meuschenia scaber",
                                            "Sardinops neopilchardus"="Sardinops sagax",
                                            "Epinephelus mystacinus"="Hyporthodus mystacinus",
                                            "Eleginus navaga"="Eleginus nawaga",
                                            "Holocentrus ascensionis"="Holocentrus adscensionis",
                                            "Epinephelus caeruleopunctatus"="Epinephelus coeruleopunctatus",
                                            "Acanthopagrus schlegeli"="Acanthopagrus schlegelii",
                                            "Percarina demidoffi"="Percarina demidoffii",
                                            "Halaelurus canescens"="Bythaelurus canescens",
                                            "Raja nidarosiensis"="Dipturus nidarosiensis",
                                            "Epinephelus nigritus"="Hyporthodus nigritus",
                                            "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                                            "Pleuronectes vetulus"="Parophrys vetulus",
                                            "Epinephelus niveatus"="Hyporthodus niveatus",
                                            "Paralichthys oblongus"="Hippoglossina oblonga",
                                            "Raja erinacea"="Leucoraja erinacea",
                                            "Caranx bartholomaei"="Carangoides bartholomaei",
                                            "Corallium japonicum"="Paracorallium japonicum",
                                            "Herklotsichthys quadrimaculat."="Herklotsichthys quadrimaculatus",
                                            "Jasus verreauxi"="Sagmariasus verreauxi",
                                            "Lithodes aequispina"="Lithodes aequispinus",
                                            "Loligo forbesi"="Loligo forbesii",
                                            "Ommastrephes bartrami"="Ommastrephes bartramii",
                                            "Penaeus aztecus"="Farfantepenaeus aztecus",
                                            "Penaeus brasiliensis"="Farfantepenaeus brasiliensis",
                                            "Penaeus brevirostris"="Farfantepenaeus brevirostris",
                                            "Penaeus californiensis"="Farfantepenaeus californiensis",
                                            "Penaeus chinensis"="Fenneropenaeus chinensis",
                                            "Penaeus duorarum"="Farfantepenaeus duorarum",
                                            "Penaeus indicus"="Fenneropenaeus indicus",
                                            "Penaeus japonicus"="Marsupenaeus japonicus",
                                            "Penaeus merguiensis"="Fenneropenaeus merguiensis",
                                            "Penaeus notialis"="Farfantepenaeus notialis",
                                            "Penaeus occidentalis"="Litopenaeus occidentalis",
                                            "Penaeus paulensis"="Farfantepenaeus paulensis",
                                            "Penaeus schmitti"="Litopenaeus schmitti",
                                            "Penaeus setiferus"="Litopenaeus setiferus",
                                            "Penaeus stylirostris"="Litopenaeus stylirostris",
                                            "Penaeus subtilis"="Farfantepenaeus subtilis",
                                            "Penaeus vannamei"="Litopenaeus vannamei",
                                            "Pleuronectes quadrituberculat."="Pleuronectes quadrituberculatus",
                                            "Pseudopleuronectes herzenst."="Pseudopleuronectes herzensteini",
                                            "Saxidomus giganteus"="Saxidomus gigantea",
                                            "Sebastes marinus"="Sebastes caurinus",
                                            "Zygochlamys delicatula"="Psychrochlamys delicatula subantarctica",
                                            "Anadara granosa"="Tegillarca granosa",
                                            "Cancer edwardsii"="Metacarcinus edwardsii",
                                            "Cancer magister"="Metacarcinus magister",
                                            "Loligo duvauceli"="Uroteuthis duvaucelii",
                                            "Loligo gahi"="Doryteuthis gahi",
                                            "Loligo opalescens"="Doryteuthis opalescens",
                                            "Loligo pealeii"="Doryteuthis pealeii",
                                            "Loligo reynaudii"="Loligo vulgaris",
                                            "Patinopecten yessoensis"="Mizuhopecten yessoensis",
                                            "Penaeus latisulcatus"="Melicertus latisulcatus",
                                            "Protothaca staminea"="Leukoma staminea",
                                            "Protothaca thaca"="Leukoma thaca",
                                            "Scapharca subcrenata"="Scapharca sativa",
                                            "Spisula polynyma"="Mactromeris polynyma",
                                            "Trachypenaeus curvirostris"="Trachysalambria curvirostris",
                                            "Xiphopenaeus riveti"="Xiphopenaeus kroyeri"))) %>% 
  left_join(select(taxa_key, -species), by=c("sci_name_fb"="sciname")) %>% 
  mutate(type=ifelse(class%in%c("Actinopterygii", "Elasmobranchii", 
                                "Holocephali", "Myxini", "Cephalaspidomorphi"), "finfish", "invertebrate")) %>% 
  select(sci_name_orig, sci_name_fb, comm_name, type, everything())
anyDuplicated(spp_lh_key$sci_name)

# Check missing names (using rfishbase)
wrong_scinames <- sort(spp_lh_key$sci_name_orig[is.na(spp_lh_key$class)])
wrong_scinames_check <- lapply(1:length(wrong_scinames), function(x) validate_names(wrong_scinames[x]))
names(wrong_scinames_check) <- wrong_scinames 
wrong_scinames_check

# Check missing names (using grepl)
wrong_scinames_check2 <- lapply(1:length(wrong_scinames), 
                                function(x) taxa_key$sciname[agrep(wrong_scinames[x], taxa_key$sciname)])
names(wrong_scinames_check2) <- wrong_scinames
wrong_scinames_check2


# Get info
################################################################################

# Get FishBase life history info
##########################################

# Load cached data or download new data
lh.cached <- T
if(lh.cached){
  # Load cached data
  load(file=paste(datadir, "FAO_marine_species_life_history_info.Rdata", sep="/"))
}else{
  # Get finfish life history info
  species_all <- spp_lh_key$sci_name_fb
  finfish <- spp_lh_key$sci_name_fb[spp_lh_key$type=="finfish"]
  finfish_gen <- spp_lh_key$genus[spp_lh_key$type=="finfish"]
  finfish_get <- subset(taxa_key, genus %in% finfish_gen)$sciname
  species_fin <- species(finfish_get) # Summary info - one row per species
  popgrowth_fin <- popgrowth(finfish_get) # Growth parameters, natural mortality, length at first maturity - multiple rows per species
  stocks_fin <- stocks(finfish_get) # Vulnerability, PriceCateg, Resilience - multiple rows per species
  # popchar_fin <- popchar(finfish_get) # Maximum length, weight, age - multiple rows per species
  # ecosystem_fin <- ecosystem(finfish_get) # Habitat info - multiple rows per species
  
  # Get invertebrate life history info
  options(FISHBASE_API = "https://fishbase.ropensci.org/sealifebase")
  inverts <- spp_lh_key$sci_name_fb[spp_lh_key$type=="invertebrate"]
  inverts_gen <- spp_lh_key$genus[spp_lh_key$type=="invertebrate"]
  inverts_get <- subset(taxa_key, genus %in% inverts_gen)$sciname
  species_inv <- species(inverts_get)
  popgrowth_inv <- popgrowth(inverts_get)
  stocks_inv <- stocks(inverts_get)
  
  # Cache data (or load already cached data)
  save(species_all, finfish, inverts, finfish_get, inverts_get,
       species_fin, popgrowth_fin, stocks_fin,
       species_inv, popgrowth_inv, stocks_inv,
       file=paste(datadir, "FAO_marine_species_life_history_info.Rdata", sep="/"))
}


# Get FishLife life history info
##########################################

# Load cached data or download new data
lh.cached <- T
if(lh.cached){
  # Load cached data
  load(file=paste(datadir, "FAO_marine_finfish_life_history_fishlife.Rdata", sep="/"))
}else{

  # Setup container
  lhdata <- data.frame(species=finfish, linf=NA, k=NA, winf=NA, tmax=NA, tm=NA, 
                       m=NA, lm=NA, temp=NA, stringsAsFactors=F)
  
  # Loop through species
  for(i in 1:nrow(lhdata)){
    
    # Get spp info
    sciname <- lhdata$species[i]
    genus <- word(sciname, 1)
    nwords_in_spp <- length(strsplit(sciname, " ")[[1]])
    species <- word(sciname, start=2, end=nwords_in_spp)
    print(paste(i, sciname))
    
    # Get and plot life history info
    spp_info <- Plot_taxa(Search_species(Genus=genus, Species=species)$match_taxonomy)
    spp_lh_vals <- exp(spp_info[[1]]$Mean_pred)
    lhdata[i,2:ncol(lhdata)] <- spp_lh_vals
    
  }
  
  # Format life history data
  lhdata1 <- lhdata %>% 
    mutate(temp=log(temp),
           resilience=cut(k, breaks=c(0, 0.1, 0.2, 0.4, 8), 
                          labels=c("Very low", "Low", "Medium", "High"))) %>% 
    rename(linf_cm=linf, winf_g=winf, tmax_yr=tmax, tmat_yr=tm, lmat_cm=lm, temp_c=temp)

  # Completeness
  complete(lhdata1)
  
  # Export data
  save(lhdata1, file=paste(datadir, "FAO_marine_finfish_life_history_fishlife.Rdata", sep="/"))
  
}
  

# Merge info
################################################################################



# Format life history info
##########################################

# Mode of string
# x <- c(NA, NA, NA, NA, rep("a", 3), rep("b", 2), rep("c", 1)); mode.string(x)
mode.string <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Format resilience data
# Reduce to 1 observation per species
resilience <- stocks_fin %>%
  rbind.fill(stocks_inv) %>% 
  group_by(sciname) %>% 
  summarize(nresil=length(na.omit(unique(Resilience))),
            resil_all=paste(na.omit(unique(Resilience)), collapse=", "),
            resil_mode=mode.string(Resilience)) %>% 
  filter(nresil>0)
table(resilience$resil_mode)

species_inv$Vulnerability[!is.na(species_inv$Vulnerability)]

# Format Von B K data
# Reduce to 1 observation per species
vonb <- popgrowth_fin %>% 
  rbind.fill(popgrowth_inv) %>% 
  group_by(sciname) %>% 
  summarize(vonb_k=median(K),
            linf_cm=median(Loo),
            m=median(M))

# Format basic data
basic <- species_fin %>% 
  rbind.fill(species_inv)


# Resilience by genus and family keys
##########################################

# It turns out that assigning the genus mode doesn't fill that many gaps
# Resiliences are either known/unknown across genera

# Resilience by genus key
resil_gen <- resilience %>% 
  left_join(select(taxa_key, sciname, family, genus), by="sciname") %>% 
  group_by(family, genus) %>% 
  summarize(resilience=mode.string(resil_mode)) %>% 
  ungroup()

# Resilience by family key
resil_fam <- resilience %>% 
  left_join(select(taxa_key, sciname, family), by="sciname") %>% 
  group_by(family) %>% 
  summarize(resilience=mode.string(resil_mode)) %>% 
  ungroup()

# Resilience by order key
resil_ord <- resilience %>% 
  left_join(select(taxa_key, sciname, order), by="sciname") %>% 
  group_by(order) %>% 
  summarize(resilience=mode.string(resil_mode)) %>% 
  ungroup()


# Merge life history info
##########################################

# Merge life history data
spp_lh_data <- spp_lh_key %>% 
  # Add/format habitat/length/vulnerability info
  left_join(select(basic, sciname, DemersPelag, Vulnerability, Length, LongevityWild), by=c("sci_name_fb"="sciname")) %>% 
  rename(habitat=DemersPelag, vulnerability=Vulnerability, lmax_cm=Length, tmax_yr=LongevityWild) %>%
  # Add/format resilience info
  left_join(select(resilience, sciname, resil_mode), by=c("sci_name_fb"="sciname")) %>% 
  rename(resilience_fb=resil_mode) %>%
  left_join(select(resil_gen, genus, resilience), by="genus") %>% 
  rename(resilience_gen=resilience) %>% 
  left_join(resil_fam, by="family") %>% 
  rename(resilience_fam=resilience) %>%
  left_join(resil_ord, by="order") %>% 
  rename(resilience_ord=resilience) %>%
  # Add/format Von B K and natural mortality info
  left_join(vonb, by=c("sci_name_fb"="sciname")) %>% 
  # Add life history categories consistent with mPRM
  left_join(select(lh_catg_key, sci_name_fb, lh_catg), by="sci_name_fb") %>% 
  mutate(lh_catg1=revalue(habitat, c("bathydemersal"="demersal",
                                     "bathypelagic"="demersal",
                                     "benthic"="demersal",
                                     "benthopelagic"="demersal",
                                     "demersal"="demersal",
                                     "pelagic"="pelagic",
                                     "pelagic-neritic"="pelagic",
                                     "pelagic-oceanic"="pelagic",
                                     "reef-associated"="demersal",
                                     "sessile"="demersal")),
         lh_catg1=ifelse(sci_name_fb%in%c("Libinia emarginata", "Corallium sp. nov.", "Pharus legumen"), "demersal", lh_catg1),
         lh_catg2=lh_catg1,
         lh_catg2=ifelse(lh_catg1=="pelagic",
                         ifelse((linf_cm>=40 & !is.na(linf_cm)) | (lmax_cm>=50 & !is.na(lmax_cm)), "large pelagic", "small pelagic"), lh_catg2)) %>% 
  # Fill resilience gaps
  mutate(resilience=ifelse(!is.na(resilience_fb), resilience_fb,
                           ifelse(vulnerability<30, "High",
                                  ifelse(vulnerability>=30 & vulnerability<55, "Medium",
                                         ifelse(vulnerability>=55 & vulnerability<70, "Low", 
                                                ifelse(vulnerability>=70, "Very low", NA))))),
         resilience=ifelse(is.na(vonb_k), resilience,
                           ifelse(vonb_k>=0.4, "High",
                                  ifelse(vonb_k>=0.2 & vonb_k<0.4, "Medium",
                                         ifelse(vonb_k>=0.1 & vonb_k<0.2, "Low",
                                                ifelse(vonb_k<0.1, "Very low", resilience))))),
         resilience=ifelse(!is.na(resilience), resilience, resil_gen), 
         resilience=factor(resilience, levels=c("Very low", "Low", "Medium", "High")),
         resilience_fb=factor(resilience_fb, levels=c("Very low", "Low", "Medium", "High"))) %>% 
  # Rearrange column order
  select(sci_name_orig:genus, habitat, lh_catg, lh_catg1, lh_catg2, linf_cm, lmax_cm,
         resilience_gen, resilience_fb, resilience, 
         vulnerability, tmax_yr, m, everything())
  
# Inspect species missing life history categories
table(spp_lh_data$habitat)
table(spp_lh_data$lh_catg)
missing_lh <- subset(spp_lh_data, is.na(lh_catg)) # once 3, now 0 (a.k.a, problem fixed)

# Make sure pelagics w/out length stats are actually small pelagics
# (this is an assumption of the imperfect logical test above)
# Close, Onykia robusta (Robust clubhook squid) is 2 m long; Calanus finmarchicus is a tiny copepod
missing_tl <- subset(spp_lh_data, lh_catg1=="pelagic" & is.na(linf_cm) & is.na(lmax_cm))
spp_lh_data$lh_catg2[spp_lh_data$sci_name_fb=="Onykia robusta"] <- "large pelagic"

# Inspect data
table(spp_lh_data$lh_catg2)
apply(spp_lh_data, 2, function(x) sum(is.na(x)))


# Small vs. large pelagic plots
##########################################

# Histogram of pelagic Linf
ggplot(subset(spp_lh_data, lh_catg1=="pelagic"), aes(x=linf_cm)) +
  geom_histogram(binwidth=10) + theme_bw() + geom_vline(xintercept=40)

# How does a 40 cm Linf relate to maximum length? 50 cm max length
lmfit <- lm(lmax_cm ~ linf_cm, spp_lh_data, subset=lh_catg1=="pelagic")
predict(lmfit, newdata=data.frame(linf_cm=40))
ggplot(subset(spp_lh_data, lh_catg1=="pelagic"), aes(x=linf_cm, y=lmax_cm)) +
  geom_point() + theme_bw() + geom_vline(xintercept=40) + geom_hline(yintercept=50)

# Histogram of pelagic maximum length
ggplot(subset(spp_lh_data, lh_catg1=="pelagic"), aes(x=lmax_cm)) +
  geom_histogram(binwidth=10) + theme_bw()


# Resilience plots
##########################################

# For species without resilience, do they have more K or vulnerability data?
sum(!is.na(spp_lh_data$vonb_k[is.na(spp_lh_data$resilience_fb)]))
sum(!is.na(spp_lh_data$vulnerability[is.na(spp_lh_data$resilience_fb)]))

# How is resilience organized by K and lmax?
ggplot(spp_lh_data, aes(x=lmax_cm, y=vonb_k, color=resilience)) +
  geom_point() + theme_bw() + 
  labs(x="Lmax (cm)", y="Von B K") +
  scale_color_discrete(name="Resilience")

# Von B K is based on life history and is similar to vulnerability
ggplot(spp_lh_data, aes(y=vonb_k, x=resilience_fb, color=resilience_fb)) +
  geom_boxplot() + theme_bw() + scale_y_continuous(limits = c(0, 2)) +
  geom_hline(yintercept=c(0.40, 0.20, 0.1))

# Vulnerability is similar to Von B K but isn't based on a single trait
ggplot(spp_lh_data, aes(y=vulnerability, x=resilience_fb, color=resilience_fb)) +
  geom_boxplot() + theme_bw() + geom_hline(yintercept=c(30, 55, 70))

# Lmax isn't as good as Von B K or vulnerability
ggplot(spp_lh_data, aes(y=lmax_cm, x=resilience_fb, color=resilience_fb)) +
  geom_boxplot() + theme_bw() + scale_y_continuous(limits = c(0, 300))


# Export data
################################################################################

# Export for LH classification
# I used this to create the key which I now read in at the top
# key <- select(spp_lh_data, type:genus, sci_name_orig:comm_name, habitat)
# write.csv(key, paste(datadir, "family_to_mprm_lh_catg_key.csv", sep="/"), row.names=F)

# Export data
save(spp_lh_data,
     file=paste(datadir, "FAO_marine_species_resilience_plus.Rdata", sep="/"))

