dwt <- "dietSS"
Aleut.DAT <- readLines(con=paste("/dedoop/mlmak/Oct_09/ch2_runs/",dwt,".DAT",sep=""))
g.path.name <- paste("/dedoop/mlmak/Oct_09/ch2_runs/noQ_sep19/",dwt,"/",sep="")
e.path.name <- paste("/dedoop/mlmak/Dec_09/dec19_obs/",sep="")

g.model.nms <- c("0","I","II","III","IV","V","VI","VII")
e.model.nms <- c("0","I","II","III","IV","V","VI","VII")
sp.names <- c("pollock","mackerel", "cod")
fsh.names <-c("pollock","mackerel","cod trawl","cod pot","cod longline")
diet_w_names <- c("pollpoll","pollmack","pollcod","pollother",
     "mackpoll","mackmack","mackcod","mackother",
     "codpoll","codmack","codcod","codother")
omega_sigma <- rep(0.0005,3)
sd_ration <- rep(0.05,3)
rMort<- 0.37            # cod pre-specified mortality
M_e <- c(0.3,0.3,rMort) 
est.ON <- TRUE          # T if estimating parameters
disc.phases <- FALSE    # T if estimating, F if generating 
srv.all.yrs <- FALSE     # T = all 25, F = observed yrs only (survey indices)
obs.srv.se.mean <- FALSE # T if using mean survey SEs for all surveys, srv.all.yrs must be True
diet.all.yrs <- FALSE   # T = all 25, F = observed yrs only
comps.all.yrs <- FALSE   # T = 25 years of fishery and survey compositions
rndm.fsh_srv <- TRUE   # T if randomized fishery and survey data
rndm.diets <- TRUE     # T if randomized prey lengths and weights
rndm.srv.Index <- TRUE # T if srv index (obs_srv) is randomized
mack.sel.invariant <- FALSE # time-invariant mackerel selectivity, normally false
maxDietN <- FALSE       # F for full diet wieghtings, F for max 20
force.survey <- FALSE    # T if survey SE = 0.01
nyrs_srv <- c(5, 6, 8)

f.diet.all.yrs <- function(){
     rksp <- 0
     stoms_w_N_e <- list()
     stoms_l_N_e <- list()
     for(rsp in 1:3){
       stoms_w_N_e[[rsp]]<-rep(1000,l_bins[rsp]*nyrs)
       dim(stoms_w_N_e[[rsp]]) <- c(l_bins[rsp],nyrs)
       for(ksp in 1:3){
         rksp <- rksp +1
         stoms_l_N_e[[rksp]]<-rep(1000,l_bins[rsp]*nyrs)
         dim(stoms_l_N_e[[rksp]]) <- c(l_bins[rsp],nyrs)
         }
       }
     return(list(stoms_w_N_e,stoms_l_N_e))
     }

for (g.model in c(0:7))
for (e.model in c(0:7)){ # DIAGONALS ONLY when commented-out
#e.model <- g.model # comment-out if off-diagonals are to be calculated
dir.create(paste(e.path.name,g.model,e.model,sep=""))
##################
g.model.path <- paste(g.path.name,"t",
            g.model,"/mlmak.rep",sep="") # path to read from to get estimated values
e.model.path <- paste(e.path.name,g.model,e.model,"/",
             sep="") # path to write generating files to
  mc_1.DAT <- Aleut.DAT[1:33] # header, phase switches
  mc_1.DAT[4] <- paste("# ",date()) # current date and time
  mc_1.DAT[6] <- paste("# ","generating model Type ",g.model.nms[g.model+1])
  mc_1.DAT[7] <- paste("# ","estimating model Type ",e.model.nms[e.model+1])
  mc_1.DAT[8] <- paste("# ","generating model path: ",g.model.path)
  mc_1.DAT[9:17] <- "# "
  if(!est.ON) {
    mc_1.DAT[22] <- 1                         # read from PIN
    mc_1.DAT[24] <- 0                         # ResetPhasesToZero
    }
  if(disc.phases){
    mc_1.DAT[26] <- 1                         # (Disc_first_phase)
    mc_1.DAT[28] <- 0                         # (Disc_any_phases) "1, 0" for est_0
    }                                         #                   "1, 1" for predation
  mc_1.DAT <-mc_1.DAT[c(1:9,19:33)]           # cut out empty lines
  mc_2_ptype.DAT <- Aleut.DAT[34:36]          # predation on-off, pred response
    if(e.model > 0) c(mc_2_ptype.DAT[1]<- 1, mc_2_ptype.DAT[3]<- (e.model))
  mc_3.DAT <- Aleut.DAT[37:58]                # age classes, length bins, fleets
  mc_4_catch.DAT <- Aleut.DAT[59:68]
  mc_5.DAT <- Aleut.DAT[69:199]               # wt_fsh
  mc_5oc.DAT <-  Aleut.DAT[200:208]           # yrs_fsh_comp
  mc_6_nsmpl_fsh.DAT <- Aleut.DAT[209:214]    # N per year for fishery age-comps
  mc_7_oc_fsh.DAT <- Aleut.DAT[215:287]       # fishery age-comps
  mc_8.DAT <- Aleut.DAT[288:300]              # survey index years
  mc_9_obs_srv.DAT <- Aleut.DAT[301:304]      # survey indices
  mc_10_obs_se_srv.DAT <- Aleut.DAT[305:308]  # survey index SEs
  mc_11.DAT <- Aleut.DAT[309:317]             # nyrs_srv_age, yrs_srv_age
  mc_12_nsmple_srv.DAT <- Aleut.DAT[318:320]  # N per year for survey age-comps
  mc_13_oc_srv.DAT <- Aleut.DAT[321:337]      # survey age-comps
  mc_14.DAT <- Aleut.DAT[338:492]             # wts at age, maturity, al_keys, pred_l_bin
  mc_15_ration.DAT <- Aleut.DAT[493:500]      # omega_vB
  mc_16.DAT <- Aleut.DAT[501:529]             # nyrs_stomwts, nyrs_stomlns, yrs_stomlns
  mc_17_stoms_w_N.DAT <- Aleut.DAT[530:589]   # N prey weights (3 x ages x yrsData)
  mc_18_stoms_l_N.DAT <- Aleut.DAT [590:768]  # N prey lengths (9 x l_bins x yrsData)
  mc_19_minmaxSS.DAT <- Aleut.DAT [769:780]   # max N prey wts, lns
  mc_20.DAT <- Aleut.DAT[781:787]             # i_wt_yrs_all (index for nyrs_stomwts)
  mc_21_diet_w_dat.DAT <- Aleut.DAT[788:1015] # data prey weights (3 x ages x yrsData)
  mc_22_diet_1_dat.DAT <- Aleut.DAT[1021:1196]# data prey lengths (9 x l_bins x yrsData)
  mc_23.DAT <- Aleut.DAT[1197:1220]           # steepness, sigmaR, styr_rec_est
  mc_24_M.DAT <- Aleut.DAT[1221]              # M
  mc_25.DAT <- Aleut.DAT[1222:1235]           # M CV, Q
  mc_26_cv_catchbiomass.DAT <- Aleut.DAT[1236:1237]
  mc_27_sd_ration.DAT <- Aleut.DAT[1238:1239]
  mc_28.DAT <- Aleut.DAT[1240:1295]            # survey options, penalties
###############
# assemble the e_data file from the operating model
source(g.model.path)
nyrs.pre <- nyrs_pred-nyrs ## 1960-25=44
nyrs_srv_e <- list()
yrs_srv <- list()             ##################
ifelse(srv.all.yrs == TRUE,   # SURVEY ALL YEARS
  c(  
    yrs_srv[[1]] <-yrs_srv[[2]] <-yrs_srv[[3]] <- c(1979:2003),
    nyrs_srv_e[[1]] <- nyrs_srv_e[[2]] <- nyrs_srv_e[[3]] <- 25
   ),                         #######################
  c(                          # SURVEY OBSERVED YEARS        
    yrs_srv[[1]] <- c(1991, 1994, 1997, 2000, 2002),
    yrs_srv[[2]] <- c(1986, 1991, 1994, 1997, 2000, 2002),
    yrs_srv[[3]] <- c(1980, 1983, 1986, 1991, 1994, 1997, 2000, 2002),
    nyrs_srv_e <- nyrs_srv
    )
  )
stoms_l_N_e <- stoms_l_N
ifelse(diet.all.yrs == TRUE && comps.all.yrs == FALSE,
                              ################
  c(nyrs_pred <- 25,          # DIET ALL YEARS, comps some years
     nyrs_stomwts <- c(25,25,25),
     nyrs_stomlns <- rep(25,9),
     yrs_stomwts <- list(),
     yrs_stomwts[[1]]<-yrs_stomwts[[2]]<-yrs_stomwts[[3]]<-c(1979:2003),
     yrs_stomlns <- list(),
     yrs_stomlns[[1]]<-yrs_stomlns[[2]]<-yrs_stomlns[[3]]<-yrs_stomlns[[4]]<-yrs_stomlns[[5]]<-
            yrs_stomlns[[6]]<-yrs_stomlns[[7]]<-yrs_stomlns[[8]]<-yrs_stomlns[[9]]<-
            c(1979:2003),
     stom_N_lists <- f.diet.all.yrs(),
     stoms_w_N_e <- stom_N_lists[[1]],
     stoms_l_N_e <- stom_N_lists[[2]]
    ),                        ####################
   c(yrs_stomwts <- list(),   # DIET OBSERVED YEARS
      yrs_stomwts[[1]]<-c(1982, 1983, 1986, 1987, 1988, 1990, 1991, 1992, 1993, 
                  1994, 1995, 1996, 1997, 2000, 2001, 2002, 2003), # poll
      yrs_stomwts[[2]]<-c(1986, 1987, 1988, 1990, 1991, 1994, 1997, 1999, 2000,
                  2001, 2002), # mack
      yrs_stomwts[[3]]<-c(1983, 1984, 1985, 1986, 1987, 1988, 1990, 1991, 1992,
                  1993, 1994, 1995, 1996, 1997, 1998, 1999,
                 2000, 2001, 2002, 2003), # cod
      yrs_stomlns <- list(), ## note: yrs_stomlns are not used
      yrs_stomlns[[1]]<-c(1986, 1987, 1988, 1990, 1991, 1994, 2001, 2003),
      yrs_stomlns[[2]]<-c(1994, 2000, 2002),
      yrs_stomlns[[3]]<-c(1991),
      yrs_stomlns[[4]]<-c(1986, 1987, 1991),
      yrs_stomlns[[5]]<-c(2001),
      yrs_stomlns[[6]]<-c(1994),
      yrs_stomlns[[7]]<-c(1985, 1986, 1987, 1988, 1990, 1991, 1993, 1994,
                1997, 1998, 2000, 2002, 2003),
      yrs_stomlns[[8]]<-c(1985, 1986, 1987, 1990, 1991, 1993, 1994, 1995,
                 1996, 1997, 1998, 2000, 2001, 2002, 2003),
      yrs_stomlns[[9]]<-c(1985, 1987, 1991, 1994)
    )
   )
if(diet.all.yrs == TRUE && comps.all.yrs == TRUE)
  c(nyrs_pred <- 25,          # DIET ALL YEARS, comps some years
     nyrs_stomwts <- c(25,25,25),
     nyrs_stomlns <- rep(25,9),
     yrs_stomwts <- list(),
     yrs_stomwts[[1]]<-yrs_stomwts[[2]]<-yrs_stomwts[[3]]<-c(1979:2003),
     yrs_stomlns <- list(),
     yrs_stomlns[[1]]<-yrs_stomlns[[2]]<-yrs_stomlns[[3]]<-yrs_stomlns[[4]]<-yrs_stomlns[[5]]<-
            yrs_stomlns[[6]]<-yrs_stomlns[[7]]<-yrs_stomlns[[8]]<-yrs_stomlns[[9]]<-
            c(1979:2003),
     stom_N_lists <- f.diet.all.yrs(),
     stoms_w_N_e <- stom_N_lists[[1]],
     stoms_l_N_e <- stom_N_lists[[2]]
   ) 
catch_e <- pred_catch ###########################
for (ifsh in 1:nfsh){ # assign 0s to tiny numbers
                      # in predicted catches
  catch_e[[ifsh]][catch_e[[ifsh]] < 1e-8] <- 0
  }                   ################
obs_srv_e <- list()   # survey indices
for(isrv in 1:nsrv){
   ifelse(srv.all.yrs == TRUE,
     obs_srv_e[[isrv]] <- pred_srv[[isrv]],
     obs_srv_e[[isrv]] <- pred_srv[[isrv]][yrs_srv[[isrv]]-1978]
     )
  if(rndm.srv.Index == TRUE){
      obs_srv_e[[isrv]] <- rnorm(nyrs_srv_e[[isrv]],mean=obs_srv_e[[isrv]],
                                     sd=obs_se_srv[[isrv]])
      while(length(which(obs_srv_e[[isrv]]<1e-8))!=0) obs_srv_e[[isrv]] <-
                                      rnorm(nyrs_srv_e[[isrv]],mean=pred_srv[[isrv]][yrs_srv[[isrv]]-1978],
                                      sd=obs_se_srv[[isrv]])    
      }
    }
###########################################################
#### COMPOSITIONS FROM FISHERIES AND SURVEYS
if(comps.all.yrs == FALSE){ 
  oc_fsh_e <- ec_fsh
  oc_srv_e <- ec_srv
  }
if(comps.all.yrs == TRUE && rndm.fsh_srv == FALSE){
  oc_fsh_e <- list()
  oc_srv_e <- list()
  for(ifsh in 1:nfsh){
    ifelse (ifsh < 3, isrv <- ifsh, isrv <- 3)
    isp <- isrv
    if(isp < 3){
      oc_fsh_e[[ifsh]] <- (F[[ifsh]]/Z[[isp]])*(1-exp(-Z[[isp]]))*natage[[isp]]/
                     apply((F[[ifsh]]/Z[[isp]])*(1-exp(-Z[[isp]]))*natage[[isp]],1,sum)
      oc_srv_e[[isrv]] <- exp(log_sel_srv[[isrv]])*natage[[isp]]/
                     apply(exp(log_sel_srv[[isrv]])*natage[[isp]],1,sum)
      }
    if(isp == 3){
      oc_fsh_e[[ifsh]] <- ((F[[ifsh]]/Z[[isp]])*(1-exp(-Z[[isp]]))*natage[[isp]]/
                     apply((F[[ifsh]]/Z[[isp]])*(1-exp(-Z[[isp]]))*natage[[isp]],1,sum)) %*%
                     al_key[[isp]]
      oc_srv_e[[isrv]] <- (exp(log_sel_srv[[isrv]])*natage[[isp]]/
                     apply(exp(log_sel_srv[[isrv]])*natage[[isp]],1,sum)) %*% 
                     al_key[[isp]]
      }
    for(iyr in 1:nyrs){
      if(catch_e[[ifsh]][iyr]==0) oc_fsh_e[[ifsh]][iyr,]<-0
      }
   }
 }
nsmpl_fsh_e <- nsmpl_fsh
nsmpl_srv_e <- nsmpl_srv
if(comps.all.yrs == TRUE){
  for (ifsh in 1:nfsh)
    nsmpl_fsh_e[[ifsh]] <- rep(10000,length(catch_e[[ifsh]][catch_e[[ifsh]] != 0]))
  nsmpl_srv_e <- nsmpl_srv
  for (isrv in 1:nsrv)
    nsmpl_srv_e[[isrv]] <- rep(10000,nyrs)
  oc_fsh_e <- list()
  for(ifsh in 1:nfsh){
    ifelse (ifsh < 3, isp <- ifsh, isp <- 3)
    oc_fsh_e[[ifsh]] <- array(dim = c(nyrs,nages[isp]))
    catage <- (F[[ifsh]]/Z[[isp]])*((1-exp(-Z[[isp]]))*natage[[isp]])
    for(iyr in 1:nyrs)
      for(iage in 1:nages[isp])
        oc_fsh_e[[ifsh]][iyr,iage] <- catage[iyr,iage]/sum(catage[iyr,])
        if(ifsh > 2)
          oc_fsh_e[[ifsh]] <- oc_fsh_e[[ifsh]] %*% al_key[[3]]
    yrs_fsh_comp[[ifsh]] <- c(styr:endyr)
    }
   ec_fsh <- oc_fsh_e
  oc_srv_e <- list()
  for(isrv in 1:nsrv){
    ifelse (isrv < 3, isp <- isrv, isp <- 3)
    oc_srv_e[[isrv]] <- array(dim = c(nyrs,nages[isp]))
    srv_age <- exp(log_sel_srv[[isrv]]) * natage[[isp]]
    for(iyr in 1:nyrs)
      for(iage in 1:nages[isp])
        oc_srv_e[[isrv]][iyr,iage] <- srv_age[iyr,iage]/sum(srv_age[iyr,])
        if(isrv > 2)
          oc_srv_e[[isrv]] <- oc_srv_e[[isrv]] %*% al_key[[3]]
    yrs_srv_comp[[isrv]] <- c(styr:endyr)
    }
   ec_srv <- oc_srv_e
   } # end comps.all.yrs TRUE
if(rndm.fsh_srv == TRUE){ #################################
                          # randomize fsh, srv compositions
  for(ifsh in 1:nfsh){
    if(comps.all.yrs == TRUE) nyrs_nsmpl_fsh <- length(catch_e[[ifsh]][catch_e[[ifsh]] != 0])
    if(comps.all.yrs == FALSE) nyrs_nsmpl_fsh <- nrow(oc_fsh_e[[ifsh]])
     for(iyr in 1:nyrs_nsmpl_fsh){
       oc_fsh_e[[ifsh]][iyr,] <- rmultinom(1,
             size=nsmpl_fsh_e[[ifsh]][iyr],
             prob=ec_fsh[[ifsh]][iyr,])
       oc_fsh_e[[ifsh]][iyr,] <- oc_fsh_e[[ifsh]][iyr,]/
             sum(oc_fsh_e[[ifsh]][iyr,])  
       row.names(oc_fsh_e[[ifsh]]) <- yrs_fsh_comp[[ifsh]]
       }
    }
 for(isrv in 1:nsrv)
     for(iyr in 1:nrow(oc_srv_e[[isrv]])){
       oc_srv_e[[isrv]][iyr,] <- rmultinom(1,
             size=nsmpl_srv_e[[isrv]][iyr],
             prob=ec_srv[[isrv]][iyr,])
       oc_srv_e[[isrv]][iyr,] <- oc_srv_e[[isrv]][iyr,]/
             sum(oc_srv_e[[isrv]][iyr,])
       row.names(oc_srv_e[[isrv]]) <- yrs_srv_comp[[isrv]]
       }
  }
#ration_e  <- omega_hat_ave
diet_l_e <- T_hat
diet_w_e <- list()
iisp <- 0
for (rsp in 1:nspp)
for (ksp in 1:(nspp+1)){
  iisp <- iisp + 1
  if(diet.all.yrs == TRUE) diet_w_e[[iisp]] <- Q_hat[[iisp]][,20:44]
  if(diet.all.yrs == FALSE){ 
   diet_w_e[[iisp]] <- Q_hat[[iisp]][,(nyrs.pre+1):nyrs_pred]
    # now reduce 25 years to actual data years
    diet_w_e[[iisp]] <- diet_w_e[[iisp]][,yrs_stomwts[[rsp]]-1978]
    }
  }
cv_catchbiomass_e <- c(0.01, 0.01, 0.01)
ifelse(srv.all.yrs == TRUE, {
  mc_8.DAT[6] <- "25 25 25"
  mc_8.DAT[8] <- "1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003" 
  mc_8.DAT[9] <- "1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003"   
  mc_8.DAT[10] <- "1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003"   
  obs_se_srv_e <- list()
  if(obs.srv.se.mean == FALSE)
  obs_se_srv_e[[1]] <- rep(0.01, 25)
  obs_se_srv_e[[2]] <- rep(0.01, 25)
  obs_se_srv_e[[3]] <- rep(0.01, 25)
  },
  {
  nsmpl_srv_e <- nsmpl_srv
  for (isrv in 1:nsrv)
    nsmpl_srv_e[[isrv]][] <- 10000 # note: not used
  obs_se_srv_e <- list()
  obs_se_srv_e[[1]] <- rep(0.01, 5)
  obs_se_srv_e[[2]] <- rep(0.01, 6)
  obs_se_srv_e[[3]] <- rep(0.01, 8)
  }
 )
if (obs.srv.se.mean == TRUE){
  obs_se_srv_e <- list()
  obs_se_srv_e[[1]] <- rep(106.2, 25)
  obs_se_srv_e[[2]] <- rep(414.5, 25)
  obs_se_srv_e[[3]] <- rep(141.5, 25)
  }
mc_stoms_l <- diet_l_dat
##################################
# RANDOMIZE DIETS ################
if (rndm.diets == TRUE){
  if (diet.all.yrs == FALSE)   nyrs_stomwts <- c(17, 11, 20)
  # RANDOMIZE PREY WEIGHTS
  if (comps.all.yrs == FALSE){  
  diet_w_array <- diet_w_e <- list()
  pred.no <- 1
  for (isp in 1:nspp){ # turn 4 lists into 1 array/sp
                       # dim(l_bins[isp],nyrs_stomwts[isp],1:4)
       diet_w_array[[isp]] <-
                 array(c(diet_w_dat[[pred.no]],diet_w_dat[[pred.no+1]],
                         diet_w_dat[[pred.no+2]],diet_w_dat[[pred.no+3]]))
       dim(diet_w_array[[isp]]) <- c(l_bins[isp],nyrs_stomwts[isp],4)
       diet_w_e[[isp]] <- diet_w_array[[isp]]
       pred.no <- pred.no + 4
       # randomize along the last dimension (prey-type)
       for (iln in 1:l_bins[isp])
       for (iyr in 1:nyrs_stomwts[isp]){
        if (sum(diet_w_array[[isp]][iln,iyr,]) > 0){
           diet_w_e[[isp]][iln,iyr,] <- rmultinom(1,
                                        size=sum(stoms_w_N[[rsp]][iln,]),
                                        prob=diet_w_array[[isp]][iln,iyr,])
           diet_w_e[[isp]][iln,iyr,] <- diet_w_e[[isp]][iln,iyr,]/
                                        sum(diet_w_e[[isp]][iln,iyr,])
           }
          }
        }
       }
  if (comps.all.yrs == TRUE && diet.all.yrs == TRUE){
    nyrs_stomwts <- c(25, 25, 25)
    diet_w_array <- diet_w_e <- list()
    pred.no <- 1
    for (isp in 1:nspp){ # turn 4 lists into 1 array/sp
                       # dim(l_bins[isp],nyrs_stomwts[isp],1:4)
       diet_w_array[[isp]] <-
                 array(c(Q_hat[[pred.no]][,20:44],Q_hat[[pred.no+1]][,20:44],
                         Q_hat[[pred.no+2]][,20:44],Q_hat[[pred.no+3]][,20:44]))
       dim(diet_w_array[[isp]]) <- c(l_bins[isp],nyrs_stomwts[isp],4)
       diet_w_e[[isp]] <- diet_w_array[[isp]]
       pred.no <- pred.no + 4
       # randomize along the last dimension (prey-type)
       for (iln in 1:l_bins[isp])
       for (iyr in 1:nyrs_stomwts[isp]){
        if (sum(diet_w_array[[isp]][iln,iyr,]) > 0){
           diet_w_e[[isp]][iln,iyr,] <- rmultinom(1,
                                        size=sum(stoms_w_N[[rsp]][iln,]),
                                        prob=diet_w_array[[isp]][iln,iyr,])
           diet_w_e[[isp]][iln,iyr,] <- diet_w_e[[isp]][iln,iyr,]/
                                        sum(diet_w_e[[isp]][iln,iyr,])
           }
          }
         # correct diet_w_array for Type 0 with nothing eaten
         diet_w_e[[isp]][,,1:3][diet_w_e[[isp]][,,1:3]=="NaN"] <- 0
         diet_w_e[[isp]][,,4][diet_w_e[[isp]][,,4]=="NaN"] <- 1
        }
    }
  for (isp in 1:nspp)
    diet_w_e[[isp]][diet_w_e[[isp]] < 1e-9] <- 0
  # RANDOMIZE PREY LENGTHS  
  rksp <- 0
  for(rsp in 1:nspp)
  for(ksp in 1:nspp)
    {
     rksp <- rksp+1
     for(rln in 1:nrow(T_hat[[rksp]]))
      if(sum(T_hat[[rksp]][rln,])>1e-10){
        # produce randomized counts
        mc_stoms_l[[rksp]][rln,] <- rmultinom(1,
               #size = 10000000,
               size=sum(stoms_l_N[[rksp]][rln,]),
               prob=T_hat[[rksp]][rln,])
        # change counts to proportions
        mc_stoms_l[[rksp]][rln,] <- mc_stoms_l[[rksp]][rln,] /
                                sum(mc_stoms_l[[rksp]][rln,])
        }
    } 
  diet_l_e <- mc_stoms_l
  } # end rndm.diets

  # TIME-INVARIANT MACKEREL SELECTIVITY  
  if(mack.sel.invariant == TRUE) mc_28.DAT[30] <- mc_28.DAT[29]

################################
# write the new data file
#e.model <- 1 # enter the estimating type here
e.DAT <- paste(e.model.path,"e.DAT",sep="")
write(mc_1.DAT, file = e.DAT)                                     # discounts, phasing
write(mc_2_ptype.DAT, file = e.DAT, append = TRUE) # functional response
write(mc_3.DAT, file = e.DAT,append = TRUE)             # no. spp, ages, bins, fleets
for(ifsh in 1:length(catch_e))
write((catch_e[[ifsh]]), ncolumns= length(catch_e[[ifsh]]),
  file= e.DAT,sep=" ",append = TRUE)                          # catches
write(mc_5.DAT, file = e.DAT,append = TRUE)             # wt_fsh
if(comps.all.yrs ==FALSE)                                             # no. yrs and yrs with age comps
  write(mc_5oc.DAT, file = e.DAT,append = TRUE)
yrs_fsh_comp_e <- yrs_fsh_comp
if(comps.all.yrs == TRUE){
  write("# nyrs_fsh_comp, the number of years with fishery compositions",
    file = e.DAT,append = TRUE)
  for(ifsh in 1:nfsh){
    write(length(catch_e[[ifsh]][catch_e[[ifsh]] != 0]), file = e.DAT,append = TRUE) 
    }
  write("# yrs_fsh_comp", file = e.DAT,append = TRUE)
  yrs_fsh_comp_e <- list()
  for(ifsh in 1:nfsh){
    yrs_fsh_comp_e[[ifsh]] <-NA
    for(testyr in 1979:2003){
     if(catch_e[[ifsh]][testyr-1978] != 0) 
        yrs_fsh_comp_e[[ifsh]] <- c(yrs_fsh_comp_e[[ifsh]],testyr)
      }
    yrs_fsh_comp_e[[ifsh]] <- yrs_fsh_comp_e[[ifsh]][2:length(yrs_fsh_comp_e[[ifsh]])]
    write(yrs_fsh_comp_e[[ifsh]],ncolumns= length(catch_e[[ifsh]][catch_e[[ifsh]] != 0]), 
           file = e.DAT,append = TRUE) 
    }
 }
 write("# nsmpl_fsh", file = e.DAT,append = TRUE)
  for(ifsh in 1:length(nsmpl_fsh_e))                             # nsmpl_fsh for age comps
   write((nsmpl_fsh_e[[ifsh]]), ncolumns= length(nsmpl_fsh_e[[ifsh]]),
      file= e.DAT,sep=" ",append = TRUE)
  write("# fishery compositions", file = e.DAT,append = TRUE)
  if(comps.all.yrs == TRUE)
  for(ifsh in 1:length(nsmpl_fsh_e)){                            # oc_fsh for age comps
    write(paste("# ",fsh.names[ifsh]," fsh comps (oc_fsh)", sep=""),
              file= e.DAT,append = TRUE)
    write.table(round(oc_fsh_e[[ifsh]][yrs_fsh_comp_e[[ifsh]]-1978,],digits=10), row.names = FALSE,
      col.names = FALSE,file= e.DAT,sep=" ",append = TRUE)
    }
  if(comps.all.yrs == FALSE)
  for(ifsh in 1:length(nsmpl_fsh_e)){
    write(paste("# ",fsh.names[ifsh]," fsh comps (oc_fsh)", sep=""),
              file= e.DAT,append = TRUE)
    write.table(round(oc_fsh_e[[ifsh]],digits=10), row.names = FALSE,
      col.names = FALSE,file= e.DAT,sep=" ",append = TRUE)
    }

write(mc_8.DAT, file = e.DAT,append = TRUE)
for(isrv in 1:length(obs_srv_e))
write((obs_srv_e[[isrv]]), ncolumns= length(obs_srv_e[[isrv]]),
  file= e.DAT,sep=" ",append = TRUE)
write("# survey SEs  (obs_se_srv)", file = e.DAT,append = TRUE)
for(isrv in 1:length(obs_se_srv_e)){
  if(srv.all.yrs == TRUE || force.survey == TRUE) 
    write((obs_se_srv_e[[isrv]]), ncolumns= length(obs_se_srv_e[[isrv]]),
    file= e.DAT,sep=" ",append = TRUE)
  if(srv.all.yrs == FALSE  && force.survey == FALSE) 
    write((obs_se_srv[[isrv]]), ncolumns= length(obs_se_srv[[isrv]]),
    file= e.DAT,sep=" ",append = TRUE)
  }
if(comps.all.yrs ==FALSE){  
  write(mc_11.DAT, file = e.DAT,append = TRUE)
  write("# number of composition samples from surveys",file = e.DAT,append = TRUE)
  for(isrv in 1:length(nsmpl_srv_e))
   write((nsmpl_srv[[isrv]]), ncolumns= length(nsmpl_srv[[isrv]]),
    file= e.DAT,sep=" ",append = TRUE)
  }
if(comps.all.yrs ==TRUE){ 
  write("# number of composition samples from surveys",file = e.DAT,append = TRUE)
  write(rep(nyrs,nspp), file = e.DAT,append = TRUE)
  write("# yrs_srv_comp",file = e.DAT,append = TRUE)
  write(rep(1979:2003,nspp) , ncolumns= 25,file = e.DAT,append = TRUE)
  write("# nsmpl_srv",file = e.DAT,append = TRUE)
  write(rep(10000,75),ncolumns = 25,file = e.DAT,append = TRUE)
  }
  write("# age/length compositions from surveys",file = e.DAT,append = TRUE)

for(isrv in 1:length(nsmpl_srv_e)){
  write(paste("# ",sp.names[isrv]," srv comps (oc_srv)", sep=""),
              file= e.DAT,append = TRUE)
  write.table(round(oc_srv_e[[isrv]],digits=10), row.names = FALSE, col.names = FALSE,
  file= e.DAT,sep=" ",append = TRUE)
  }
write(mc_14.DAT, file = e.DAT,append = TRUE)
write(mc_15_ration.DAT, file = e.DAT,append = TRUE)
if(diet.all.yrs == TRUE)
  {
    write("# omega_sigma (not used)", file = e.DAT,append = TRUE)
    write(c("0.0005","0.0005","0.0005"), file = e.DAT,append = TRUE)
    write("#nyrs_stomwts", file = e.DAT,append = TRUE)
    write(nyrs_stomwts, file = e.DAT,append = TRUE)
    write("#nyrs_stomlns", file = e.DAT,append = TRUE)
    write(nyrs_stomlns, file = e.DAT,ncol=9,append = TRUE)
    write("# yrs_stomwts_e", file = e.DAT,append = TRUE)
    for(i in 1:3) write(yrs_stomwts[[i]], file = e.DAT,ncolumns=25,append = TRUE)
    write("# yrs_stomlns_e", file = e.DAT,append = TRUE)
    for(i in 1:9) write(yrs_stomlns[[i]], file = e.DAT,ncolumns=25,append = TRUE)
    write("# stoms_w_N_e", file = e.DAT,append = TRUE)
    for(i in 1:3) write(stoms_w_N_e[[i]], file = e.DAT,ncol=ncol(stoms_w_N_e[[i]]),append = TRUE)
  }
if(diet.all.yrs == FALSE){
  write(mc_16.DAT, file = e.DAT,append = TRUE)
  write(mc_17_stoms_w_N.DAT, file = e.DAT,append = TRUE)
  }
write("# diet length sample sizes (stoms_l_N_e)",file = e.DAT,append = TRUE)
for(rksp in 1:length(stoms_l_N_e))
  write(t(stoms_l_N_e[[rksp]]), ncolumns= ncol(stoms_l_N_e[[rksp]]),
             file= e.DAT,sep=" ",append = TRUE)
if(maxDietN == TRUE){
  mc_19_minmaxSS.DAT[5] <- "1 1 1"
  mc_19_minmaxSS.DAT[11] <- "1 1 1 1 1 1 1 1 1"
  }
write(mc_19_minmaxSS.DAT, file = e.DAT,append = TRUE)

if(diet.all.yrs == TRUE){
  write("# i_wt_yrs_all",file = e.DAT,append = TRUE)
  write(rep(25,12), file = e.DAT,ncol=12,append = TRUE)
  }
if(diet.all.yrs == FALSE)
  write(mc_20.DAT, file = e.DAT,append = TRUE)
for(rksp in 1:length(diet_w_e)){
  write(paste("# diet wts (diet_w_e) ",diet_w_names[rksp]), file= e.DAT,append = TRUE)
    write(diet_w_e[[rksp]], ncolumns= ncol(diet_w_e[[rksp]]),
            file= e.DAT,sep=" ",append = TRUE)
    }
write("# diet length compositions (diet_l_dat)",file = e.DAT,append = TRUE)
diet_l_names <-
  (c("pollpoll","pollmack","pollcod","mackpoll","mackmack","mackcod","codpoll",
   "codmack","codcod"))
for(rksp in 1:length(diet_l_e)){
  write(paste("# diet lns ",diet_l_names[rksp]), file= e.DAT,append = TRUE)
  write(t(diet_l_e[[rksp]]), 
    ncolumns= ncol(diet_l_e[[rksp]]),
    file= e.DAT,sep=" ",append = TRUE)
  }
write(mc_23.DAT, file = e.DAT,append = TRUE)
write(M_e, file=e.DAT,append=TRUE)
write(mc_25.DAT, file = e.DAT,append = TRUE)
write("#cv_catchbiomass", file = e.DAT, append=TRUE)
write(cv_catchbiomass_e, file = e.DAT,append = TRUE)
write("#sd_ration", file = e.DAT,append = TRUE)
write(sd_ration, file=e.DAT, append = TRUE)
write(mc_28.DAT, file = e.DAT,append = TRUE)

} # end of e.model, g.model loops
###########################


