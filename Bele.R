library(aLBI)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(TropFishR)
library(LBSPR)
library(fishmethods)

# data processing
# For May
library(readxl)
library(openxlsx)
library(Matrix)
library(TropFishR)
setwd
set.seed(1)
data<-read.xlsx("BeleT.xlsx", sheet = 1)
dates<- as.Date(paste0("14-",01:08,"-2022"),format="%d-%m-%Y")


lfq<- list(dates = dates,midLengths = data$Length,
           catch = as.matrix(data[,2:ncol(data)]))
class(lfq) <- "lfq"
lfq_bin2 <- lfqModify(lfq)
ma <- 5
lfq_bin2_res <- lfqRestructure(lfq_bin2, MA = 5, addl.sqrt = FALSE)
opar <- par(mfrow = c(2,1), mar= c(2,5,2,3), oma = c(2,0,0,0))
plot(lfq_bin2_res,Fname = "catch", date.axis = "modern")
plot(lfq_bin2_res, Fname = "rcounts", date.axis = "modern")
par(opar)


Lmax = max(data[[1]])
linf_guess <- Lmax / 0.95

low_par <- list(Linf=0.8*linf_guess, k=0.01, t_anchor=0, c=0, ts=0)

up_par <- list(Linf=1.2*linf_guess, k=1, t_anchor=1, c=1, ts=1)

opar <- par(mfrow = c(1,1), mar= c(2,5,2,3), oma = c(2,0,0,0))
res_GA <- ELEFAN_GA(lfq_bin2, MA=ma, seasonalised=F,
                    maxiter=1000,
                    addl.sqrt = F,
                    low_par=low_par,
                    up_par=up_par,
                    monitor = T)
res_GA
res_GA$par
res_GA$Rn_max
opar <- par(mfrow = c(1,1), mar= c(2,5,2,3), oma = c(2,0,0,0))
plot(lfq_bin2_res, Fname= "rcounts",date.axis = "modern", ylim=c(1,16))
lt <- lfqFitCurves(lfq_bin2,par= res_GA$par,
                   draw=TRUE, col= "forestgreen",lty=1, lwd=2)

lfq_bin2 <- lfqModify(lfq_bin2, par=res_GA$par)

Ms <- M_empirical(Linf = lfq_bin2$par$Linf, K_l = lfq_bin2$par$K, method = "Then_growth")
Ms



lfq_bin2$par$M <- as.numeric(Ms)
plus_group <- lfq_bin2$midLengths[max(which(lfq_bin2$midLengths < lfq_bin2$par$Linf))]
lfq_catch_vec <- lfqModify(lfq_bin2, vectorise_catch = TRUE, plus_group = plus_group)

# Quick check of total catch
lfq_catch_vec$catch


plot(catchCurve(lfq_catch_vec))

res_cc <- catchCurve(lfq_catch_vec, reg_int = c(7,14), calc_ogive = T)
res_cc
#Calling my custom function for LW
lwdata <- read_excel("Bele.xlsx", sheet = 3) %>% dplyr::select(2,3)

aLBI::LWR(data = lwdata, main = NULL, log_transform = T, line_col = "pink")

lfq_catch_vec$par$Z <- res_cc$Z
lfq_catch_vec$par$FM <- as.numeric(lfq_catch_vec$par$Z- lfq_catch_vec$par$M)
lfq_catch_vec$par$E<- lfq_catch_vec$par$FM/lfq_catch_vec$par$Z
lfq_catch_vec$par$a <-  0.0155 
lfq_catch_vec$par$b <- 2.68

selectivity_list <- list(selecType="trawl_ogive", L50= res_cc$L50, L75= res_cc$L75)


TB1 <- predict_mod(lfq_catch_vec,type= "ThompBell",
                   FM_change = seq(0,2,0.05),
                   stock_size_1 = 1,
                   curr.E = lfq_catch_vec$par$E,
                   s_list= selectivity_list,
                   plot= FALSE, hide.progressbar = TRUE)


TB2 <- predict_mod(lfq_catch_vec, type="ThompBell",
                   FM_change = seq(0,2,0.1),
                   Lc_change = seq(1,8,0.1),
                   stock_size_1 = 1,
                   curr.E = lfq_catch_vec$par$E,
                   curr.Lc = res_cc$L50,
                   s_list=selectivity_list,
                   plot= FALSE, hide.progressbar= TRUE)
par(mfrow=c(1,1),mar=c(4,5,2,4.5),oma=c(1,0,0,0))
plot(TB1,mark=T)

#mtext("(a)",side = 3,at=0.1,line = 0.6)
plot(TB2,type="Isopleth",xaxis1 = "FM",mark = T,contour = 10)
#mtext("(b)",side = 3,at=-0.1,line = 0.8)
TB1$df_Es
TB1$currents

# LBSPR

library(LBSPR)

MyLengths <- new("LB_lengths")
datdir <- DataDir()
datdir
MyPrars <- new("LB_pars")
MyPrars@Species <- "Bele"
MyPrars@Linf <- 18.80
MyPrars@L50 <- 11.85 # Lmat
MyPrars@L95 <- 13.04
MyPrars@MK <- 1.82
MyPrars@L_units <- "cm"
Len2 <- new("LB_lengths", LB_pars=MyPrars,
            file=paste0(datdir, "/blbspr.csv"),
            dataType="freq", header=T)
plotSize(Len2)
myFit2 <- LBSPRfit(MyPrars, Len2)
myFit <- LBSPRfit(MyPrars, Len2)
myFit2@Ests
data.frame(rawSL50=myFit@SL50, rawLS95=myFit@SL95, rawFM=myFit@FM,
           rawSPR=myFit@SPR)
plotSize(myFit2)
plotMat(myFit2)
plotEsts(myFit)

Mypars <- new("LB_pars", verbose=F)
Mypars@Linf <- 18.80
Mypars@L50 <- 11.85
Mypars@L95 <- 13.04
Mypars@MK <- 1.82
Mypars@SPR <- 0.4
Mypars@BinWidth <- 1


LenDat <- new("LB_lengths", LB_pars=Mypars, file=paste0(datdir, "/blbspr.csv"),
              dataType="freq", header = T, verbose=F)
Mod <- LBSPRfit(Mypars, LenDat, verbose = F)
yr <- 1
Mypars@SL50 <- Mod@SL50[yr]
Mypars@SL95 <- Mod@SL95[yr]
plotTarg(Mypars, LenDat, yr=yr)


#aLBI for LWR of Bele
library(readxl)
library(aLBI)
library(dplyr)
library(ggplot2)
library(tidyverse)

lwr <- read_excel("Bele.xlsx", sheet = 3)


lw <- lwr %>% dplyr::select(2,3)


LWR(data = lw)

lw %>% summary

lw %>% 
  ggplot2::ggplot(aes(Weight))+
  geom_density()



adata <- read_excel("albi.xlsx", sheet = 1)

FishPar(data = adata, resample = 1000, progress = F, Linf = 20)
FishSS(data = CPdata,
       LM_ratio = 0.96,
       Pmat = 16.72,
       Pobj = 37.74,
       Popt = 7.96)



















albi <- read_excel("albi.xlsx")

ld <- mydata %>% dplyr::select(2)



aLBI::FrequencyTable(data = ld)

bele <- mydata %>% dplyr::select(1,2) 

zdata <- read_excel("zdata.xlsx", sheet = 1)


zfreq <- zdata %>% dplyr::select(1,3)

max(zfreq$Length)

FrequencyTable(data = bele)


aug <- bele %>% filter( Month == "Aug")

FrequencyTable(data = zfreq,  output_file = "test_aug.xlsx")

FrequencyTable(data = zfreq, bin_width = 2, output_file = "test_aug.xlsx")

FrequencyTable(data = zfreq, bin_width = 3, output_file = "test_aug.xlsx")

FrequencyTable(data = zfreq, bin_width = 4, output_file = "test_aug.xlsx")


head(bele, 100)

FrequencyTable(data = bele, date_config = list(day = 14, year = 2022), output_file = "BeleT.xlsx")

FrequencyTable(data = bele, bin_width = 2, date_config = list(day = 11, year = 2024), output_file = "test_aug.xlsx")

FrequencyTable(data = bele, bin_width = 3, output_file = "test_aug.xlsx")

FrequencyTable(data = bele, bin_width = 4, output_file = "test_aug.xlsx")

