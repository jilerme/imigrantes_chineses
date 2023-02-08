#Gráficos para dissertação

##carregar pacotes
library(pacman)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(readr)
library(geobr)
library(sf)
library(udunits2)
library(hrbrthemes)
library(rio)
library(fmsb)
library(scales)
library(RColorBrewer)
library(stringi)


#Gráfico 1
## REGISTRO DE TRABALHADORES IMIGRANTES ASIÁTICOS NO BRASIL ENTRE 2011-2020

RCE_total %>% filter(pais %in% c('CHINA',
                                 'JAPÃO',
                                 'BANGLADESH',
                                 'CORÉIA DO SUL',
                                 'ÍNDIA',
                                 'SÍRIA')) %>%
  ggplot(aes(x = ano, y=total, color = pais, group = pais)) +
  geom_point(size = 3, shape = 19) +
  geom_line(size = 1.5, show.legend = F) +
  scale_color_manual(values = c('#67A9B8', '#CB5B5A', '#AD557A',
  '#40324E', '#8E4C7D', '#EABD5E')) +
  theme_minimal() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_text(size = '10', face = 'bold',
                                  margin = margin(r = 10)),
        axis.title.x = element_text(size = '10', face = 'bold',
                                          margin = margin(t = 10)),
        axis.text = element_text(size = '10'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(100,400))) +
  scale_x_discrete(expand = expansion(add = c(0.2,0.3)),
                  labels = c('', 2012, '', 2014, '', 2016, '', 2018, '', 2020))

ggsave('G1.png', height = 10, width = 16 , units = 'cm', dpi = 600)


#Gráfico 2
##REMUNERAÇÃO MÉDIA DO IMIGRANTE CHINÊS EM SALÁRIOS MINÍMOS ENTRE 2011-2020

salario %>%
  ggplot(aes(x = ano, y = media, label = media, group = 1)) +
  geom_line( color="#CB5B5A", size = 2) +
  geom_point(shape=19, color="#CB5B5A", fill="#CB5B5A", size=4) +
  geom_text(size = 4.5, vjust = 0, nudge_y = 0.35) +
  theme_minimal() + 
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = '10', face = 'bold',
                                    margin = margin(t = 10)),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(1,1))) +
  scale_x_discrete(expand = expansion(add = c(0.3,0.3)),
                   labels = c('', 2012, '', 2014, '', 2016, '', 2018, '', 2020))

ggsave('G2.png', height = 9, width = 16 , units = 'cm', dpi = 600)

#Gráfico 3
##REMUNERAÇÃO MEDIANA DO IMIGRANTE CHINÊS EM SALÁRIOS MINÍMOS ENTRE 2011-2020

salario %>%
  ggplot(aes(x = ano, y = mediana, label = mediana, group = 1)) +
  geom_line( color="#CB5B5A", size = 2) +
  geom_point(shape=19, color="#CB5B5A", fill="#CB5B5A", size=4) +
  geom_text(size = 4.5, vjust = 0, nudge_y = 0.35) +
  theme_minimal() + 
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = '10', face = 'bold',
                                    margin = margin(t = 10)),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(1,1))) +
  scale_x_discrete(expand = expansion(add = c(0.3,0.3)),
                   labels = c('', 2012, '', 2014, '', 2016, '', 2018, '', 2020))

ggsave('G3.png', height = 9, width = 16 , units = 'cm', dpi = 600)

#Gráfico 4
##DENSIDADE E FREQUÊNCIA DA REMUNERAÇÃO DE IMIGRANTES CHINESES NOS ANOS DE  2013 E 2019.

RCE%>% 
  filter(pais == 'CHINA') %>%
  filter(ano %in% c('2013','2019')) %>%
  filter(vl_remun_media_sm != 0) %>%
  ggplot(aes(x = vl_remun_media_sm, y = pais)) +
  geom_violin(aes(fill = ano)) +
  geom_point() +
  scale_fill_manual(values = c('#CB5B5A', '#EABD5E')) +
  facet_wrap(~ ano, ncol = 1) +
  labs(x = 'SALÁRIOS MÍNIMOS') +
  theme_minimal() +
  theme(legend.position="none",
        strip.text = element_text(size = 11, face = 'bold'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = '10', face = 'bold',
                                    margin = margin(t = 10)),
        axis.title.x = element_text(size = '10', face = 'bold',
                                    margin = margin(t = 10))) +
  scale_x_continuous(n.breaks = 15)

ggsave('G4.png', height = 13.8, width = 28.5 , units = 'cm', dpi = 600)


#Gráfico 5
## REMUNERAÇÃO POR SÁLARIO MINÍMO X GRANDES GRUPOS OCUPACIONAIS DOS TRABALHADORES CHINESES.

RCE_11%>% 
  ggplot(aes(y =vl_remun_media_sm, x= total, color = gg)) +
  geom_point(size = 1.2, shape = 16) + expand_limits(y=c(0, 790)) +
  scale_color_manual(values = c('#EABD5E', '#CB5B5A', '#AD557A',
                                '#8E4C7D', '#40324E', '#67A9B8', 
                                '#3961AB', '#688B58', '#95C11F')) +
  theme_bw() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "nonw",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(n.breaks = 10, expand = expansion(add = c(10,20))) +
  scale_x_discrete(expand = expansion(add = c(70,70)))

ggsave('G5-1.png', height = 15, width = 8.4 , units = 'cm', dpi = 600)

RCE_20%>% 
  ggplot(aes(y =vl_remun_media_sm, x= total, color = gg)) +
  geom_point(size = 1.2, shape = 16) + expand_limits(y=c(0, 790)) +
  scale_color_manual(values = c('#EABD5E', '#CB5B5A', '#AD557A',
                                '#8E4C7D', '#40324E', '#67A9B8', 
                                '#3961AB', '#688B58', '#95C11F')) +
  theme_bw() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "nonw",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(n.breaks = 10, expand = expansion(add = c(10,20))) +
  scale_x_discrete(expand = expansion(add = c(70,70)))

ggsave('G5-2.png', height = 15, width = 8.4 , units = 'cm', dpi = 600) 

#Gráfico 6
##DENSIDADE DOS GRANDES GRUPOS OCUPACIONAIS DOS IMIGRANTES CHINESES EM ANOS SELECIONADOS: 2011, 2018 E 2020.

coul <- c('#8E4C7D', '#CB5B5A', '#EABD5E')
colors_border <- coul
colors_in <- alpha(coul,0.3)

radarchart(cbo_111820, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.5,
           vlcex=1) 

#Gráfico 7
##COMPARAÇÃO ENTRE A DENSIDADE DOS GRANDES GRUPOS OCUPACIONAIS DOS IMIGRANTES NOS ANOS DE 2011 E 2020.

radarchart(CBO_m, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.5,
           vlcex=1)

radarchart(CBO_c, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.5,
           vlcex=1)

#Gráfico 8
##TRABALHADORES CHINÊSES POR GÊNERO.

sexo%>% 
  filter(pais == 'CHINA') %>%
  ggplot(aes(x= ano, y = total, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = total), size = 3,
            vjust = -0.5, position = position_dodge(.9)) +
  theme_minimal() +
labs(y = '', x = '', fill = '') + # adicionar nome x, y e legenda
  scale_fill_manual(values = c('#67A9B8', '#CB5B5A')) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(0,250))) +
  scale_x_discrete(expand = expansion(add = c(0.3,0.3)))

ggsave('G8.png', height = 9, width = 16 , units = 'cm', dpi = 600)

#Gráfico 9
##REMUNERAÇÃO POR SÁLARIO MINÍMO X GÊNERO DOS TRABALHADORES CHINESES.

RCE_11%>% 
  ggplot(aes(y =vl_remun_media_sm, x= total, color = sexo)) +
  geom_point(size = 1.2, shape = 16) + expand_limits(y=c(0, 790)) +
  scale_color_manual(values = c('#67A9B8', '#CB5B5A')) +
  theme_bw() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "none",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(n.breaks = 10, expand = expansion(add = c(10,20))) +
  scale_x_discrete(expand = expansion(add = c(70,70)))

ggsave('G9-1.png', height = 15, width = 8.4 , units = 'cm', dpi = 600)


RCE_20%>% 
  ggplot(aes(y =vl_remun_media_sm, x= total, color = sexo)) +
  geom_point(size = 1.2, shape = 16) + expand_limits(y=c(0, 790)) +
  scale_color_manual(values = c('#67A9B8', '#CB5B5A')) +
  theme_bw() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "none",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(n.breaks = 10, expand = expansion(add = c(10,20))) +
  scale_x_discrete(expand = expansion(add = c(70,70)))

ggsave('G9-2.png', height = 15, width = 8.4 , units = 'cm', dpi = 600)


#Gráfico 10
##FAIXA ETÁRIA DOS TRABALHADORES CHINESES POR GÊNERO.

sexo_idade%>% 
  filter(pais == 'CHINA') %>%
  filter(ano %in% c('2011','2020')) %>%
  ggplot(aes(x= sexo, y = total, fill = faixa_etaria)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = total), size = 3,
            vjust = -0.5, position = position_dodge(.9)) +
  facet_wrap(~ ano, ncol = 2) +
  scale_fill_manual(values = c('#EABD5E', '#CB5B5A', '#AD557A',
                               '#8E4C7D', '#40324E', '#67A9B8', '#688B58')) +
  theme_minimal() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', fill = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(0,100))) +
  scale_x_discrete(expand = expansion(add = c(0.2,0.3)))

ggsave('G10.png', height = 10, width = 16 , units = 'cm', dpi = 600)

#Gráfico 11
##REMUNERAÇÃO POR SÁLARIO MINÍMO X NÍVEL DE INSTRUÇÃO DOS TRABALHADORES CHINESES.

RCE_11%>% 
  ggplot(aes(y =vl_remun_media_sm, x= total, color = nivel_instrucao)) +
  geom_point(size = 1.2, shape = 16) + expand_limits(y=c(0, 790)) +
  scale_color_manual(values = c('#EABD5E', '#CB5B5A', '#AD557A',
                                '#8E4C7D', '#40324E', '#67A9B8', '#688B58')) +
  theme_bw() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "none",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(n.breaks = 10, expand = expansion(add = c(10,20))) +
  scale_x_discrete(expand = expansion(add = c(70,70)))

ggsave('G11-1.png', height = 15, width = 8.4 , units = 'cm', dpi = 600)

RCE_20%>% 
  ggplot(aes(y =vl_remun_media_sm, x= total, color = nivel_instrucao)) +
  geom_point(size = 1.2, shape = 16) + expand_limits(y=c(0, 790)) +
  scale_color_manual(values = c('#EABD5E', '#CB5B5A', '#AD557A',
                                '#8E4C7D', '#40324E', '#67A9B8', '#688B58')) +
  theme_bw() + 
  guides(col = guide_legend(nrow = 2)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "none",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(n.breaks = 10, expand = expansion(add = c(10,20))) +
  scale_x_discrete(expand = expansion(add = c(70,70)))

ggsave('G11-2.png', height = 15, width = 8.4 , units = 'cm', dpi = 600)

#Gráfico 12
##NÍVEL DE INSTRUÇÃO DOS TRABALHADORES CHINESES.

nivel_i%>% 
  filter(pais == 'CHINA') %>%
  filter(ano %in% c('2011', '2014', '2017','2020')) %>%
  ggplot(aes(x= ano, y = total, fill = nivel_instrucao)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = total), size = 3,
            vjust = -0.5, position = position_dodge(.9)) +
  scale_fill_manual(values = c('#EABD5E', '#CB5B5A', '#AD557A',
                               '#8E4C7D', '#40324E', '#67A9B8', '#688B58')) +
  theme_minimal() +
  labs(y = '', x = '', fill = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(0,100))) +
  scale_x_discrete(expand = expansion(add = c(0.2,0.3))) +
  guides(fill=guide_legend(ncol=2))

ggsave('G12.png', height = 10, width = 16 , units = 'cm', dpi = 600)

#Gráfico 13
##FLUXO DE INVESTIMENTOS CHINESES NO BRASIL NA DÉCADA DE 2010 (US$ BILHÕES).

investimentos %>%
  ggplot(aes(x = ano, y = investimento, fill = tipo, label = investimento)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(size = 3, vjust = -0.5, position = position_dodge(.9)) +
  scale_fill_manual(values = c('#CB5B5A', '#67A9B8')) +
  theme_minimal() +
  labs(y = '', x = '', fill = '') + # adicionar nome x, y e legenda
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(0,3))) +
  scale_x_continuous(n.breaks = 10, expand = expansion(add = c(0.2,0.3))) +
  guides(fill=guide_legend(ncol=2))

ggsave('G13.png', height = 10, width = 16 , units = 'cm', dpi = 600)

#Gráfico 14
##PROJETOS CHINESES NO BRASIL NA DÉCADA DE 2010.

projetos %>%
  ggplot(aes(x = ano, y = projeto, color = tipo, label = projeto)) +
  geom_point(size = 3, shape = 19) +
  geom_line(size = 1.5, show.legend = F) +
  geom_label(show.legend = F) +
  scale_color_manual(values = c('#CB5B5A', '#67A9B8')) +
  theme_minimal() + 
  guides(col = guide_legend(nrow = 1)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(5,3))) +
  scale_x_continuous(n.breaks = 10)

ggsave('G14.png', height = 11.5, width = 16 , units = 'cm', dpi = 600)


#Gráfico 15
##REGISTRO DE TRABALHADORES IMIGRANTES CHINESES NOS ESTADOS DO RIO DE JANEIRO E SÃO PAULO NA DÉCADA DE 2010.

sprj %>%
  ggplot(aes(x = ano, y = trabalhadores, 
             label = trabalhadores, color = estado)) +
geom_point(size = 3, shape = 19) +
  geom_line(size = 1.5, show.legend = F) +
  scale_color_manual(values = c('#EABD5E', '#CB5B5A')) +
  geom_text(size = 4.5, vjust = 0, nudge_y = 70, color = 'black') +
  theme_minimal() + 
  guides(col = guide_legend(nrow = 1)) +
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = '10', face = 'bold',
                                    margin = margin(t = 10)),
        axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10', face = 'bold'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(expand = expansion(add = c(90,275))) +
  scale_color_discrete(breaks = c('RJ', 'SP'),
                       labels = c('Rio de Janeiro', 'São Paulo'))

ggsave('G15.png', height = 9, width = 16 , units = 'cm', dpi = 600)

#Gráfico 16
##REGISTRO DE TRABALHADORES IMIGRANTES CHINESES NO ESTADO DO RIO GRANDE DO SUL NA DÉCADA DE 2010.

rs %>%
  ggplot(aes(x = ano, y = trabalhadores, label = trabalhadores, group = 1)) +
  geom_line( color="#CB5B5A", size = 2) +
  geom_label() +
  theme_minimal() + 
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = '10', face = 'bold',
                                    margin = margin(t = 10)),
        axis.text = element_text(size = '10', face = 'bold')) +
  scale_x_continuous(n.breaks = 10)


ggsave('G16.png', height = 9, width = 16 , units = 'cm', dpi = 600)


#Gráfico 17
##REGISTRO DE TRABALHADORES CHINESES X INVESTIMENTOS CONFIRMADOS NO BRASIL NA DÉCADA DE 2010.

investimentos %>% filter(tipo %in% c('Confirmado')) %>%
  ggplot(aes(x = ano, y = investimento)) +
  geom_col(fill = '#67A9B8') +
  theme_minimal() +
  labs(y = '', x = '', fill = '') + # adicionar nome x, y e legenda
  theme(axis.text = element_text(size = '10', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_text(size = '10'),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6)) +
  scale_y_continuous(expand = expansion(add = c(0,0.8))) +
  scale_x_continuous(n.breaks = 10, expand = expansion(add = c(0.2,0.3))) 

  ggsave('G17.png', height = 6, width = 16 , units = 'cm', dpi = 600)

#Figura 3
##distribuição de imigrantes pelo país - mapa de calor

brasil <- read_country(year = 2020)
china_uf <- brasil %>% left_join(china_uf, by = c('abbrev_state'= 'abbrev_state'))
  
china_uf %>%
    ggplot() +
    geom_sf(aes(fill=total), color = 'grey25') + 
    scale_fill_gradient(na.value = 'white', low = '#F9F3F0', high = '#453F78', limits = c(0, 2200)) +
    facet_wrap(~ ano) +
    theme_minimal() + 
  labs(y = '', x = '', color = '') + # adicionar nome x, y e legenda
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(size = '12', face = 'bold'),
        plot.caption = element_text(size = '10'),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = 'grey95', color = 'white'),
        legend.margin = margin(4,6,4,6))
  
ggsave('F3.png', height = 16, width = 16 , units = 'cm', dpi = 600)
  

#########
#paleta
#EABD5E
#CB5B5A
#AD557A
#8E4C7D
#40324E
#67A9B8
