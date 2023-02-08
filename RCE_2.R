#importar arquivos
RCE <- read_csv("RCE.csv", col_types = cols(vl_remun_dezembro_nom = col_number(), 
                                            vl_remun_media_nom = col_number(), ano = col_character()))

#transformar em character
RCE$ano <- as.character(RCE$ano)

#adicionar trabalhador
RCE$trabalhadores <- 1

#transformando sexo em fator 
RCE$sexo <- factor(RCE$sexo, label = c("Ignorado", 'Masculino', 'Feminino'), 
                   levels = c("-1", '01', '02'))

#transformando nivel_instrucao em fator
RCE$nivel_instrucao <- factor(RCE$nivel_instrucao, label = 
                                c('Sem instrução ou ef. incompleto',
                                  'Ensino fundamental completo',
                                  'Ensino médio incompleto',
                                  'Ensino médio completo', 
                                  'Ensino superior incompleto',
                                  'Ensino superior completo',
                                  'Pós-graduação', 'Não especificado'),
                              levels = c('01', '02', '03', '04', 
                                         '05', '06', '07', '-1'))

#transformando uf em fator
RCE$uf <- factor(RCE$uf, labels = c('RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO',
                                    'MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL',
                                    'SE', 'BA', 'MG', 'ES', 'RJ', 'SP', 'PR',
                                    'SC', 'RS', 'MS', 'MT', 'GO', 'DF'),
                 levels = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24,
                            25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42,
                            43, 50, 51, 52, 53))

#transformando faixa_etaria em fator
RCE$faixa_etaria <- factor(RCE$faixa_etaria, 
                           labels = c('Menos de 18 anos', '18 a 24 anos',
                                      '25 a 29 anos', '30 a 39 anos',
                                      '40 a 49 anos', '50 a 64 anos',
                                      '65 anos ou mais'),
                           levels = c('01', '02', '03', '04',
                                      '05', '06', '07'))

#transformando faixa_horas_contrat em fator
RCE$faixa_horas_contrat<- factor(RCE$faixa_horas_contrat, 
                           labels = c('Até 12 horas', '13 a 15 horas',
                                      '16 a 20 horas', '21 a 30 horas',
                                      '31 a 40 horas', '41 a 44 horas'),
                           levels = c('01', '02', '03', '04', '05', '06'))

#criar RCE_total
RCE_total <- RCE %>% filter(pais != 'NÃO ESPECIFICADO')
RCE_total <- RCE_total %>% filter(pais != 'NATURALIDADE BRASILEIRA')
RCE_total <- select(RCE_total, pais, continente, ano, trabalhadores)

RCE_total <- RCE_total %>% 
  group_by(pais, ano, continente) %>%
  summarise(total = sum(trabalhadores))

#criar RCE_uf
RCE_uf <- RCE %>% filter(pais != 'NÃO ESPECIFICADO')
RCE_uf <- RCE_uf %>% filter(pais != 'NATURALIDADE BRASILEIRA')
RCE_uf <- select(RCE_uf, pais, continente, ano, uf, trabalhadores)

RCE_uf <- RCE_uf %>% 
  group_by(pais, ano, continente, uf) %>%
  summarise(total = sum(trabalhadores))

#gráficos
RCE_total %>% filter(pais %in% c('CHINA',
                           'JAPÃO',
                           'BANGLADESH',
                           'CORÉIA DO SUL',
                           'ÍNDIA',
                           'SÍRIA')) %>%
  ggplot(aes(x = ano, y=total, color = pais, group = pais)) +
  geom_point() +
  geom_line()


#criar sexo
sexo <- RCE %>% filter(pais != 'NÃO ESPECIFICADO')
sexo <- sexo %>% filter(pais != 'NATURALIDADE BRASILEIRA')
sexo <- sexo(sexo, pais, continente, ano, sexo, trabalhadores)
sexo <- sexo %>% 
  group_by(pais, ano, continente, sexo) %>%
  summarise(total = sum(trabalhadores))

#criar idade
idade <- RCE %>% filter(pais != 'NÃO ESPECIFICADO')
idade <- idade %>% filter(pais != 'NATURALIDADE BRASILEIRA')
idade <- select(idade, pais, continente, ano, faixa_etaria, trabalhadores)
idade <- idade %>% 
  group_by(pais, ano, continente, faixa_etaria) %>%
  summarise(total = sum(trabalhadores))

#criar sexo_idade
sexo_idade <- RCE %>% filter(pais != 'NÃO ESPECIFICADO')
sexo_idade <- sexo_idade %>% filter(pais != 'NATURALIDADE BRASILEIRA')
sexo_idade <- select(sexo_idade, pais, continente, ano, faixa_etaria, sexo, trabalhadores)
sexo_idade <- sexo_idade %>% 
  group_by(pais, ano, continente, sexo, faixa_etaria) %>%
  summarise(total = sum(trabalhadores))


#contar e rankear filtros
RCE %>% filter(pais == 'CHINA') %>%
  filter(ano == '2014') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(pais,sort = TRUE)

##Rank de trabalhadores/estados
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(uf, sort = TRUE)

##Rank de trabalhadores/municipios
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  filter(uf == 'SC')
  filter(municipio != 8888888) %>%
  count(municipio, sort = TRUE)

print(n = 15, RCE%>% 
        filter(ano == '2019') %>%
        filter(pais == 'CHINA') %>%
        filter(pais != 'NATURALIDADE BRASILEIRA') %>%
        filter(pais != 'NÃO ESPECIFICADO') %>%
        filter(municipio != 8888888) %>%
        filter(municipio != 888888) %>%
        count(municipio, sort = TRUE))

##Rank de trabalhadores/nivel_instrucao
RCE%>% 
  filter(ano == '2019') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(nivel_instrucao, sort = TRUE)

##Rank de migrante qualificado - NORTE GLOBAL
RCE%>% 
  filter(ano == '2019') %>%
  filter(nivel_instrucao %in% c('Ensino superior completo',
                           'Pós-graduação')) %>%
  filter(continente %in% c('EUROPA',
                           'AMÉRICA DO NORTE',
                           'ÁSIA')) %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(pais, sort = TRUE)

##Rank de trabalhadores/faixa_etaria
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(faixa_etaria, sort = TRUE)

##Rank de trabalhadores/status_migratorio
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(status_migratorio, sort = TRUE)

##Rank de trabalhadores/cbo
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(cbo_ocupacao_2002, sort = TRUE)

##Rank de trabalhadores/faixa_horas
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(faixa_horas_contrat, sort = TRUE)

##Rank de trabalhadores/tipo_vinculo
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(tipo_vinculo, sort = TRUE)

##Rank de trabalhadores/sexo
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(sexo, sort = TRUE)

##Contar CBO
RCE%>% 
  filter(ano == '2018') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(substr(cbo_ocupacao_2002, 1, 1) == '1')


##Rank de trabalhadores/estados
RCE%>% 
  filter(ano == '2020') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  count(uf, sort = TRUE)

##criar região
RCE <- full_join(teste, regiao, by = 'uf')

##Rank de trabalhadores/municipios
RCE%>% 
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  filter(uf == 'BA') %>%
filter(municipio != 8888888) %>%
  count(cbo_ocupacao_2002, sort = TRUE)

print(n = 10,
RCE%>% 
  filter(ano == '2011') %>%
  filter(pais == 'CHINA') %>%
  filter(pais != 'NATURALIDADE BRASILEIRA') %>%
  filter(pais != 'NÃO ESPECIFICADO') %>%
  filter(uf == 'PA') %>%
  filter(municipio != 8888888) %>%
  count(cbo_ocupacao_2002, sort = TRUE))


print(n = 33,
      RCE%>% 
        filter(ano == '2020') %>%
        filter(pais == 'CHINA') %>%
        filter(pais != 'NATURALIDADE BRASILEIRA') %>%
        filter(pais != 'NÃO ESPECIFICADO') %>%
        filter(uf == 'RJ') %>%
        filter(municipio != 8888888) %>%
        count(cbo_ocupacao_2002, sort = TRUE))

print(n = 10,
      RCE%>% 
        filter(ano == '2020') %>%
        filter(pais == 'CHINA') %>%
        filter(pais != 'NATURALIDADE BRASILEIRA') %>%
        filter(pais != 'NÃO ESPECIFICADO') %>%
        filter(uf == 'MG') %>%
        filter(municipio != 8888888) %>%
        count(cbo_ocupacao_2002, sort = TRUE))


        
print(n = 20,
      RCE%>% 
        filter(ano == '2017') %>%
        filter(pais == 'CHINA') %>%
        filter(pais != 'NATURALIDADE BRASILEIRA') %>%
        filter(pais != 'NÃO ESPECIFICADO') %>%
        filter(uf == 'PR') %>%
        count(municipio,sort = TRUE))

print(n = 20,
      RCE%>% 
        filter(pais == 'CHINA') %>%
        filter(pais != 'NATURALIDADE BRASILEIRA') %>%
        filter(pais != 'NÃO ESPECIFICADO') %>%
        filter(uf == 'RJ') %>%
        count(municipio,sort = TRUE))

print(n = 3,
      RCE%>% 
        filter(pais == 'CHINA') %>%
        filter(ano == '2020') %>%
        filter(pais != 'NATURALIDADE BRASILEIRA') %>%
        filter(pais != 'NÃO ESPECIFICADO') %>%
        count(uf , sort = TRUE))


      
print(n = 30,
      RCE%>% 
        filter(ano == '2011') %>%
        filter(pais == 'CHINA') %>%
        filter(pais != 'NATURALIDADE BRASILEIRA') %>%
        filter(pais != 'NÃO ESPECIFICADO') %>%
        count(cbo_ocupacao_2002, sort = TRUE))

