
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
    mutate(temp=log(temp)) %>% 
    rename(linf_cm=linf, winf_g=winf, tmax_yr=tmax, tmat_yr=tm, lmat_cm=lm, temp_c=temp)

  # Completeness
  complete(lhdata1)
  
  # Export data
  save(lhdata1, file=paste(datadir, "FAO_marine_finfish_life_history_fishlife.Rdata", sep="/"))
  
}
  

# Merge FishBase info
################################################################################

# Mode of string
# x <- c(NA, NA, NA, NA, rep("a", 3), rep("b", 2), rep("c", 1)); mode.string(x)
mode.string <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Merge invert/finish resilience data
# Reduce to 1 observation per species
resilience <- stocks_fin %>%
  rbind.fill(stocks_inv) %>% 
  group_by(sciname) %>% 
  summarize(nresil=length(na.omit(unique(Resilience))),
            resil_all=paste(na.omit(unique(Resilience)), collapse=", "),
            resil_mode=mode.string(Resilience)) %>% 
  filter(nresil>0)

# Merge invert/finish growth data
# Reduce to 1 observation per species
vonb <- popgrowth_fin %>% 
  rbind.fill(popgrowth_inv) %>% 
  group_by(sciname) %>% 
  summarize(vonb_k=median(K),
            linf_cm=median(Loo),
            m=median(M))

# Merge invert/finish species information
basic <- species_fin %>% 
  rbind.fill(species_inv)

# Merge FishBase Info
lhdata <- spp_lh_key %>% 
  # Add FishBase base info
  left_join(select(basic, sciname, DemersPelag, Vulnerability, Length, LongevityWild), by=c("sci_name_fb"="sciname")) %>% 
  rename(habitat=DemersPelag, vulnerability_fb=Vulnerability, lmax_cm_fb=Length, tmax_yr_fb=LongevityWild) %>%
  # Add FishBase resilience info
  left_join(select(resilience, sciname, resil_mode), by=c("sci_name_fb"="sciname")) %>% 
  rename(resilience_fb=resil_mode) %>%
  # Add FishBase life history info
  left_join(vonb, by=c("sci_name_fb"="sciname")) %>% 
  rename(vonb_k_fb=vonb_k, linf_cm_fb=linf_cm, m_fb=m) %>% 
  # Add additional resilience categories
  mutate(resilience_k_fb=as.character(cut(vonb_k_fb, breaks=c(0, 0.05, 0.15, 0.30, 8), labels=c("Very low", "Low", "Medium", "High"))),
         resilience_vuln_fb=as.character(cut(vulnerability_fb, breaks=c(0, 30, 55, 65, 100), labels=c("High", "Medium", "Low", "Very low"))),
         resilience_tmax_fb=as.character(cut(tmax_yr_fb, breaks=c(0, 3.5, 10.5, 30, 400), labels=c("High", "Medium", "Low", "Very low")))) %>% 
  # Add FishLife life history info
  left_join(lhdata1, by=c("sci_name_fb"="species")) %>% 
  mutate(resilience_fl=as.character(cut(k, breaks=c(0, 0.05, 0.15, 0.30, 8), labels=c("Very low", "Low", "Medium", "High")))) %>% 
  rename(tmax_yr_fl=tmax_yr, vonb_k_fl=k, linf_cm_fl=linf_cm, m_fl=m, 
         winf_g_fl=winf_g, temp_c_fl=temp_c, lmat_cm_fl=lmat_cm) %>% 
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
                         ifelse((linf_cm_fl>=40 & !is.na(linf_cm_fl)) | (lmax_cm_fb>=50 & !is.na(lmax_cm_fb)), "large pelagic", "small pelagic"), lh_catg2)) %>% 
  # Add final resilience
  mutate(resilience=ifelse(!is.na(resilience_fb), resilience_fb, resilience_fl),
         resilience=ifelse(is.na(resilience), resilience_k_fb, resilience),
         resilience=ifelse(is.na(resilience), resilience_vuln_fb, resilience),
         resilience=ifelse(is.na(resilience), resilience_tmax_fb, resilience)) %>% 
  # Add resilience by genera
  group_by(genus) %>% 
    mutate(resilience_genus=mode.string(resilience)) %>% 
  ungroup() %>% 
  # Add resilience by family
  group_by(family) %>% 
  mutate(resilience_family=mode.string(resilience)) %>% 
  ungroup() %>% 
  # Add higher-level taxa resiliences
  mutate(resilience=ifelse(is.na(resilience), resilience_genus, resilience),
         resilience=ifelse(is.na(resilience), resilience_family, resilience)) %>% 
  # Add final M
  mutate(m_tmax_fb=4.899*tmax_yr_fb^-0.916,
         m_vonb_fb=4.118*vonb_k_fb^0.73*linf_cm_fb^-0.33,
         m=ifelse(!is.na(m_fl), m_fl, m_fb),
         m=ifelse(is.na(m), m_tmax_fb, m),
         m=ifelse(is.na(m), m_vonb_fb, m)) %>% 
  # Add M by genera
  group_by(genus) %>% 
  mutate(m_genus=median(m, na.rm=T)) %>% 
  ungroup() %>% 
  # Add resilience by family
  group_by(family) %>% 
  mutate(m_family=median(m, na.rm=T)) %>% 
  ungroup() %>% 
  # Add higher-level taxa resiliences
  mutate(m=ifelse(is.na(m), m_genus, m),
         m=ifelse(is.na(m), m_family, m)) %>% 
  # Rearrange columns
  select(sci_name_orig:genus, habitat, 
         lh_catg, lh_catg1, lh_catg2, 
         resilience, resilience_fl, resilience_fb, 
         resilience_k_fb, resilience_vuln_fb, resilience_tmax_fb, resilience_genus, resilience_family,
         m, m_fl, m_fb, m_tmax_fb, m_vonb_fb, m_genus, m_family,
         everything()) %>% 
  # Unique
  unique()
  
# Inspect completeness
freeR::complete(lhdata)


# Export data
################################################################################

# Export data
save(lhdata,
     file=paste(datadir, "FAO_marine_species_resilience_plus.Rdata", sep="/"))

