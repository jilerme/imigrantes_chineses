#carregar pacotes
library(pacman)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(readr)

#importar arquivos
RCE_2011 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2011.csv", 
                                     delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                                     trim_ws = TRUE)

RCE_2012 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2012.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2013 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2013.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2014 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2014.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2015 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2015.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2016 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2016.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2017 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2017.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2018 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2018.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2019 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2019.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

RCE_2020 <- read_delim("RAIS CTPS ESTOQUE 2020/RAIS_CTPS_ESTOQUE_2020.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE)

#retirar colunas que não serão unificadas
RCE_2011 <- select(RCE_2011, -cnae_2_0_subclasse)
RCE_2012 <- select(RCE_2012, -cnae_2_0_subclasse)
RCE_2013 <- select(RCE_2013, -cnae_2_0_subclasse)
RCE_2014 <- select(RCE_2014, -cnae_2_0_subclasse)
RCE_2015 <- select(RCE_2015, -cnae_2_0_subclasse)
RCE_2016 <- select(RCE_2016, -cnae_2_0_subclasse)
RCE_2017 <- select(RCE_2017, -cnae_2_0_subclasse)
RCE_2018 <- select(RCE_2018, -co_municipio_local_trabalho)
RCE_2019 <- select(RCE_2019, -co_municipio_local_trabalho)
RCE_2020 <- select(RCE_2020, -co_municipio_local_trabalho)

#adicionar coluna ano
RCE_2011$ano <- 2011
RCE_2012$ano <- 2012
RCE_2013$ano <- 2013
RCE_2014$ano <- 2014
RCE_2015$ano <- 2015
RCE_2016$ano <- 2016
RCE_2017$ano <- 2017
RCE_2018$ano <- 2018
RCE_2019$ano <- 2019
RCE_2020$ano <- 2020

#padronizar nome colunas
colnames(RCE_2018)[9] <- 'cnae_2_0_classe'
colnames(RCE_2019)[9] <- 'cnae_2_0_classe'
colnames(RCE_2020)[9] <- 'cnae_2_0_classe'

#transformar dados p/ juntar
RCE_2011$cbo_ocupacao_2002 <- as.numeric(RCE_2011$cbo_ocupacao_2002)
RCE_2012$cbo_ocupacao_2002 <- as.numeric(RCE_2012$cbo_ocupacao_2002)
RCE_2013$cbo_ocupacao_2002 <- as.numeric(RCE_2013$cbo_ocupacao_2002)
RCE_2015$cbo_ocupacao_2002 <- as.numeric(RCE_2015$cbo_ocupacao_2002)
RCE_2016$cbo_ocupacao_2002 <- as.numeric(RCE_2016$cbo_ocupacao_2002)
RCE_2018$cbo_ocupacao_2002 <- as.numeric(RCE_2018$cbo_ocupacao_2002)
RCE_2011$raca_cor <- as.numeric(RCE_2011$raca_cor)
RCE_2012$raca_cor <- as.numeric(RCE_2012$raca_cor)
RCE_2013$raca_cor <- as.numeric(RCE_2013$raca_cor)
RCE_2014$raca_cor <- as.numeric(RCE_2014$raca_cor)
RCE_2015$raca_cor <- as.numeric(RCE_2015$raca_cor)
RCE_2016$cnae_2_0_classe <- as.character(RCE_2016$cnae_2_0_classe)
RCE_2017$cnae_2_0_classe <- as.character(RCE_2017$cnae_2_0_classe)
RCE_2018$vl_remun_media_nom <- as.numeric(RCE_2018$vl_remun_media_nom)
RCE_2019$tipo_vinculo <- as.numeric(RCE_2019$tipo_vinculo)
RCE_2020$tempo_emprego <- as.character(RCE_2020$tempo_emprego)
RCE_2020$cnae_2_0_classe <- as.character(RCE_2020$cnae_2_0_classe)

#juntar df
RCE <- full_join(RCE_2011, RCE_2012)
RCE <- full_join(RCE, RCE_2013)
RCE <- full_join(RCE, RCE_2014)
RCE <- full_join(RCE, RCE_2015)
RCE <- full_join(RCE, RCE_2016)
RCE <- full_join(RCE, RCE_2017)
RCE <- full_join(RCE, RCE_2018)
RCE <- full_join(RCE, RCE_2019)
RCE <- full_join(RCE, RCE_2020)

#transformar dados final
RCE$raca_cor <- as.character(RCE$raca_cor)
RCE$cnae_2_0_classe <- as.numeric(RCE$cnae_2_0_classe)
RCE$tipo_vinculo <- as.character(RCE$tipo_vinculo)
RCE$ano <- as.character(RCE$ano)

#adicionar decimais
RCE$vl_remun_media_nom <- format(RCE$vl_remun_media_nom / 100,nsmall = 2)
RCE$vl_remun_dezembro_nom <- format(RCE$vl_remun_dezembro_nom / 100,nsmall = 2)
RCE$vl_remun_media_nom <- as.numeric(RCE$vl_remun_media_nom)
RCE$vl_remun_dezembro_nom <- as.numeric(RCE$vl_remun_dezembro_nom)

#transformar virgula em ponto
RCE$vl_remun_media_sm <- as.numeric(sub(",", ".", RCE$vl_remun_media_sm, fixed = TRUE))

#criar RCE_China
RCE_China <- RCE %>% filter(pais == 'CHINA')

#exportar tebelas
write_csv(RCE, path = "RCE.csv")
write_csv(RCE_China, path = "RCE_china.csv")

