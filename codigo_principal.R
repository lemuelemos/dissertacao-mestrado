setwd("~/Documentos/dissertacao")
library(tidyverse)
library(readxl)
library(BETS)
library(timeSeries)
library(ecoseries)
library(lubridate)
library(rmgarch)
library(zoo)
library(xts)
library(strucchange)
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

#### Carregar os dados e formatação
tamanho <- c(-2.095548,2.583353)

CDS <- read_excel("~/Documentos/dissertacao/dados/drive-download-20171018T001327Z-001/CDS.xlsx", 
                  col_types = c("date", "numeric", "blank", 
                                "date", "numeric"))
CDS <- CDS[-1,]
nomes <- colnames(CDS)
nomes <- nomes[c(1,3)]
cdsdatas5 <- as.timeDate(CDS$`BRAZIL CDS USD SR 5Y D14 Corp`)
cdsdatas10 <- as.timeDate(CDS$`BRAZIL CDS USD SR 10Y D14 Corp`)
CDS <- CDS[,c(-1,-3)]
CDS5 <- cbind(cdsdatas5,CDS[,1])
CDS10 <- cbind(cdsdatas10,CDS[,2])
colnames(CDS5)[2] <- nomes[1]
colnames(CDS10)[2] <- nomes[2]
names(CDS5)[1] <- "Data"
names(CDS10)[1] <- "Data"

DOLDI <- read_excel("~/Documentos/dissertacao/dados/drive-download-20171018T001327Z-001/DOLDI.xlsx", 
                    col_types = c("date", "numeric"))
nomes <- colnames(DOLDI)
nomes <- nomes[1]
DOLDI <- DOLDI[-1,]
colnames(DOLDI)[2] <- nomes
names(DOLDI)[1] <- "Data"

EMBI <- read_excel("~/Documentos/dissertacao/dados/drive-download-20171018T001327Z-001/EMBI.xlsx", 
                   col_types = c("date", "numeric"))
nomes <- colnames(EMBI)
nomes <- nomes[1]
EMBI <- EMBI[-1,]
colnames(EMBI)[2] <- nomes
names(EMBI)[1] <- "Data"

EMBI_BR <- series_ipeadata(40940, periodicity = c("D"))
EMBI_BR <- as.data.frame(EMBI_BR)
names(EMBI_BR)[1] <- "Data"
EMBI_BR <- EMBI_BR %>% arrange(`Data`)
EMBI_BR$Data <- parse_date_time(EMBI_BR$Data, "%Y.%0m.d")

cambiocontrat <- as.list(NULL)
codes <- c(13967,13970)
for(i in 1:2){
  cambiocontrat[[i]] <- BETS.get(codes[i])
}
cambiocontrat <- cbind(cambiocontrat[[1]],cambiocontrat[[2]])

DOLARSPOT <- read_excel("~/Documentos/dissertacao/dados/drive-download-20171018T001327Z-001/DOLARSPOT.xlsx", 
                        col_types = c("date", "numeric"))
nomes <- colnames(DOLARSPOT)
nomes <- nomes[1]
DOLARSPOT <- DOLARSPOT[-1,]
colnames(DOLARSPOT)[2] <- nomes
names(DOLARSPOT)[1] <- "Data"


result <- read_excel("~/Documentos/dissertacao/dados/drive-download-20171018T001327Z-001/RESULT.xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "text", "numeric", "numeric"))

Tipo_do_Participante <- read_excel("~/Documentos/dissertacao/Tipo do Participante.xlsx", 
                                   col_types = c("numeric", "text"))
#####################################
pessoas_juridicas_financeiras <-  result %>%
  select(`Data de referência`,
         Mercado,
         `Categoria do participante`, 
         `Quantidade de contratos comprados`,
         `Quantidade de contratos vendidos`) %>%
  dplyr::filter(`Categoria do participante` == 100 & Mercado == 2) %>%
  mutate(Saldo_Participantes = log(`Quantidade de contratos comprados`) - 
           log(`Quantidade de contratos vendidos`))

saldoPJF <- pessoas_juridicas_financeiras %>%
  select(`Data de referência`, `Saldo_Participantes`)
names(saldoPJF)[1] <- "Data"

###############################
investidor_institucional <- result %>%
  select(`Data de referência`,
         Mercado,
         `Categoria do participante`, 
         `Quantidade de contratos comprados`,
         `Quantidade de contratos vendidos`) %>%
  dplyr::filter(`Categoria do participante` == 200 & Mercado == 2) %>%
  mutate(Saldo_Participantes = log(`Quantidade de contratos comprados`) - 
           log(`Quantidade de contratos vendidos`))


saldoII <- investidor_institucional %>%
  select(`Data de referência`, `Saldo_Participantes`)
names(saldoII)[1] <- "Data"

##################################
investidor_naoresidente <- result %>%
  select(`Data de referência`,
         Mercado,
         `Categoria do participante`, 
         `Quantidade de contratos comprados`,
         `Quantidade de contratos vendidos`) %>%
  dplyr::filter(`Categoria do participante` == 352 & Mercado == 2) %>%
  mutate(Saldo_Participantes = log(`Quantidade de contratos comprados`) - 
           log(`Quantidade de contratos vendidos`))


saldoNR <- investidor_naoresidente %>%
  select(`Data de referência`, `Saldo_Participantes`)
names(saldoNR)[1] <- "Data"

#######################################
pessoa_juridica_nfinanceira <- result %>%
  select(`Data de referência`,
         Mercado,
         `Categoria do participante`, 
         `Quantidade de contratos comprados`,
         `Quantidade de contratos vendidos`) %>%
  dplyr::filter(`Categoria do participante` == 400 & Mercado == 2) %>%
  mutate(Saldo_Participantes = log(`Quantidade de contratos comprados`) - 
           log(`Quantidade de contratos vendidos`))


saldoPNJ <- pessoa_juridica_nfinanceira %>%
  select(`Data de referência`, `Saldo_Participantes`)
names(saldoPNJ)[1] <- "Data"

###############################################################################
############ Montando base de dados para as estimações #####
### EMBI
dolembi <- merge.data.frame(DOLARSPOT, EMBI, by="Data")
dolembi <- xts(dolembi[,-1], order.by = dolembi$Data)
dolembi <- log(dolembi)
ddolembi <- diff(dolembi)

### EMBI+br
dolembibr <- merge.data.frame(DOLARSPOT,EMBI_BR, by="Data")
dolembibr <- xts(dolembibr[,-1], order.by = dolembibr$Data)
dolembibr <- log(dolembibr)
ddolembibr <- diff(dolembibr)

### CDS 5 anos
dolcds5 <- merge.data.frame(DOLARSPOT,CDS5, by="Data")
dolcds5 <- xts(dolcds5[,-1], order.by = dolcds5$Data)
dolcds5 <- log(dolcds5)
ddolcds5 <- diff(dolcds5)

### CDS 10 anos
dolcds10 <- merge.data.frame(DOLARSPOT, CDS10, by="Data")
dolcds10 <- xts(dolcds10[,-1], order.by = dolcds10$Data)
dolcds10 <- log(dolcds10)
ddolcds10 <- diff(dolcds10)

################################################### EMBI
### Fluxo II
dolsaldoII <- merge.data.frame(DOLARSPOT,saldoII, by="Data")
dolsaldoII <- xts(dolsaldoII[,-1], order.by = dolsaldoII$Data)
dolsaldoII$`USDBRL Curncy` <- log(dolsaldoII$`USDBRL Curncy`)
ddolsaldoII <- diff(dolsaldoII)

### Fluxo PNJ
dolsaldoPNJ <- merge.data.frame(DOLARSPOT,saldoPNJ, by="Data")
dolsaldoPNJ <- xts(dolsaldoPNJ[,-1], order.by = dolsaldoPNJ$Data)
dolsaldoPNJ$`USDBRL Curncy` <- log(dolsaldoPNJ$`USDBRL Curncy`)
ddolsaldoPNJ <- diff(dolsaldoPNJ)

### Fluxo NR
dolsaldoNR <- merge.data.frame(DOLARSPOT,saldoNR, by="Data")
dolsaldoNR <- xts(dolsaldoNR[,-1], order.by = dolsaldoNR$Data)
dolsaldoNR$`USDBRL Curncy` <- log(dolsaldoNR$`USDBRL Curncy`)
ddolsaldoNR <- diff(dolsaldoNR)

### Fluxo PJF
dolsaldoPJF <- merge.data.frame(DOLARSPOT,saldoPJF, by="Data")
dolsaldoPJF <- xts(dolsaldoPJF[,-1], order.by = dolsaldoPJF$Data)
dolsaldoPJF$`USDBRL Curncy` <- log(dolsaldoPJF$`USDBRL Curncy`)
ddolsaldoPJF <- diff(dolsaldoPJF)



################## Estimação
############ Especificação
uspec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(0, 0), include.mean = F, archm = FALSE, 
                                     archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
                   distribution.model = "norm")

spec = dccspec(uspec = multispec(replicate(2, uspec)), 
               dccOrder = c(1,1), distribution = "mvnorm",model = "DCC")
####################################################################
### EMBI
fitembi <- dccfit(spec, ddolembi[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                  fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                  cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitembi, type="R")
r1.z=xts(r1[1,2,],time(ddolembi[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = length(bp.nile$breakpoints))
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = length(bp.nile$breakpoints)))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))

qbEMBI <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[1]),
           y=round(head(fitted(fm0),1),2)-0.01,
           label =  round(head(fitted(fm0),1),2))


### EMBI Br

fitembibr <- dccfit(spec, ddolembibr[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                    fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                    cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitembibr, type="R")
r1.z=xts(r1[1,2,],time(ddolembibr[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = length(bp.nile$breakpoints))
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = length(bp.nile$breakpoints)))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))

qbEMBIbr <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[180]),
           y=round(head(fitted(fm0),1),2)-0.01,
           label =  round(head(fitted(fm0),1),2))


### CDS 5

fitcds5 <- dccfit(spec, ddolcds5[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                  fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                  cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitcds5, type="R")
r1.z=xts(r1[1,2,],time(ddolcds5[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = 3)
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = 3))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))

qbCDS5 <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_bw()+
  ylab("") + xlab("")+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[1]),
           y=round(head(fitted(fm0),1),2)-0.01,
           label = round(head(fitted(fm0),1),3))


## CDS 10

fitcds10 <- dccfit(spec, ddolcds10[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                   fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                   cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitcds10, type="R")
r1.z=xts(r1[1,2,],time(ddolcds10[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = 2)
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = 2))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))

qbCDS10 <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_bw()+
  ylab("") + xlab("")+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[90]),
           y=round(head(fitted(fm0),1),2)-0.01,
           label =  round(head(fitted(fm0),1),3))


### Saldo II

fitsaldoII <- dccfit(spec, ddolsaldoII[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                     fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                     cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitsaldoII, type="R")
r1.z=xts(r1[1,2,],time(ddolsaldoII[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = 3)
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = 3))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))

qbII <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[1]),
           y=round(head(fitted(fm0),1),2)-0.01,
           label =  round(head(fitted(fm0),1),3))

### Saldo PNJ

fitsaldoPNJ <- dccfit(spec, ddolsaldoPNJ[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                      fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                      cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitsaldoPNJ, type="R")
r1.z=xts(r1[1,2,],time(ddolsaldoPNJ[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = 2)
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = 2))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))

qbPNJ <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_bw()+
  ylab("") + xlab("")+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[2]),
           y=round(head(fitted(fm0),1),2)-0.005,
           label = round(head(fitted(fm0),1),3))

### Saldo NR

fitsaldoNR <- dccfit(spec, ddolsaldoNR[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                     fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                     cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitsaldoNR, type="R")
r1.z=xts(r1[1,2,],time(ddolsaldoNR[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = length(bp.nile$breakpoints))
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = length(bp.nile$breakpoints)))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))

qbNR <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[60]),
           y=round(head(fitted(fm0),1),2)-0.015,
           label =  round(head(fitted(fm0),1),3))


### Saldo PJF

fitsaldoPJF <- dccfit(spec, ddolsaldoPJF[-1,], out.sample = 0, solver = "solnp", solver.control = list(), 
                      fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                      cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)

r1=rcor(fitsaldoPJF, type="R")
r1.z=xts(r1[1,2,],time(ddolsaldoPJF[-1,]))
bp.nile <- breakpoints(r1.z ~ 1)
ci.nile <- confint(bp.nile, breaks = 3)
fm0 <- lm(ts(r1.z) ~ 1)
fm1 <- lm(ts(r1.z) ~ breakfactor(bp.nile, breaks = 3))

intervalos <- data.frame(min = as.Date(time(r1.z[ci.nile$confint[,1]])),
                         max = as.Date(time(r1.z[ci.nile$confint[,3]])))
qbPJF <- as.Date(time(r1.z[ci.nile$confint[,2]]))

ggplot(r1.z, aes(as.Date(time(r1.z)),r1.z)) + 
  geom_line() +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))+
  theme_classic()+
  ylab("") + xlab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept =  fitted(fm0),  linetype = "Média"), size = 0.5, color="green")+
  geom_line(aes(x=as.Date(time(r1.z)),y=fitted(fm1), linetype = "Quebras"), color="blue")+
  scale_linetype_manual(name = "", values = c(1,1),
                        guide = guide_legend(override.aes = list(color = c("green", "blue"))))+
  geom_rect(data=intervalos,
            aes(xmin=min, 
                xmax=max, 
                ymin=-Inf, ymax=+Inf), 
            fill='gray', alpha=0.5,inherit.aes=F)+
  annotate("text", x = as.Date(time(r1.z)[1]),
           y=round(head(fitted(fm0),1),3)-0.0025,
           label =  round(head(fitted(fm0),1),3))

#############################################################################################################################
### Datas das Quebras
ggplot(EMBI_BR2, aes(as.Date(time(EMBI_BR2)),EMBI_BR2)) + 
  geom_line() + theme_classic() + ylab("") + xlab("") +
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))


perccont <- read_excel("~/Documentos/dissertacao/Dados/RESULT.xlsx", 
                       col_types = c("date", "numeric", "numeric", 
                                     "text", "numeric", "numeric"))


perccont <- perccont %>% mutate(`Categoria do participante`=ifelse(`Categoria do participante`==100,"PJF",
                                                                   ifelse(`Categoria do participante`==200,"II",
                                                                          ifelse(`Categoria do participante`==350,"NR","PJNF"))))

perccont %>% select(`Categoria do participante`,
                    `Percentual de contratos comprados`,
                    `Percentual de contratos vendidos`,
                    `Data de referência`) %>%
  ggplot(aes(as.Date(`Data de referência`),`Percentual de contratos comprados`))+
  geom_area()+
  theme_classic()+
  facet_grid(`Categoria do participante`~.)+
  ylab("") + xlab("")+
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))



perccont %>% select(`Categoria do participante`,
                    `Percentual de contratos comprados`,
                    `Percentual de contratos vendidos`,
                    `Data de referência`) %>%
  ggplot(aes(as.Date(`Data de referência`),`Percentual de contratos vendidos`))+
  geom_area()+
  theme_classic()+
  facet_grid(`Categoria do participante`~.)+
  ylab("") + xlab("")+
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))



result %>% dplyr::filter(Mercado == 2 & Mercadoria == "DOL" & 
                           `Categoria do participante` %in% c(100,200,350,400)) %>%
  mutate(`Categoria do participante`=ifelse(`Categoria do participante`==100,"PJF",
                                            ifelse(`Categoria do participante`==200,"II",
                                                   ifelse(`Categoria do participante`==350,"NR","PJNF"))),
         Saldo = `Quantidade de contratos comprados`-`Quantidade de contratos vendidos`) %>%
  select(`Data de referência`, `Categoria do participante`,Saldo) %>%
  mutate(Saldo = Saldo/1000) %>%
  ggplot()+
  geom_area(aes(as.Date(`Data de referência`),`Saldo`, group=`Categoria do participante`))+
  theme_classic()+
  facet_grid(`Categoria do participante`~.)+
  ylab("") + xlab("")+
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))

dolar <- xts(DOLARSPOT$`USDBRL Curncy`,order.by = DOLARSPOT$Data)
dolar <- dolar["2010-01-01/2016-12-31"]

ggplot()+
  geom_line(aes(as.Date(time(dolar)),dolar))+
  theme_classic()+
  ylab("") + xlab("")+
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))


EMBI_BR2 <- xts(EMBI_BR$serie_40940.valor,order.by = EMBI_BR$Data)
EMBI_BR2 <- EMBI_BR2["2002-01-01/2016-12-31"]
ggplot()+
  geom_line(aes(as.Date(time(EMBI_BR2)),EMBI_BR2))+
  theme_classic()+
  ylab("") + xlab("")+
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))

CDS52 <- xts(CDS5$`BRAZIL CDS USD SR 5Y D14 Corp`,order.by = CDS5$Data)
CDS52 <- CDS52["2002-01-01/2016-12-31"]
ggplot()+
  geom_line(aes(as.Date(time(CDS52)),CDS52))+
  theme_classic()+
  ylab("") + xlab("")+
  scale_x_date(breaks=date_breaks("years"),
               labels=date_format("%b %Y"))