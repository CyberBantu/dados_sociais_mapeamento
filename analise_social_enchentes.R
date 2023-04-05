library(dplyr)
library(ggplot2)
library(geobr)
library(readr)
library(ggspatial)
library(sf)
library(ggrepel)

# Preciso produzir um mapa por setor censitário ou bairros para o queimados e Jacarezinho
# Raça, Coleta de Lxo, Esgotamentos Sanitario

rm(list = ls())
#------------------------
# Importando os dados Geograficos de Queimados
rj = read_census_tract(code_tract = 33)

# olhando para como os nomes estavam escritos
unique(rj$name_muni)

# filtrando para queimados
queimados = rj %>% filter(name_muni == "Queimados")

# Plotando mapa das cidades visinhas
cidades_rj = read_municipality(code_muni = 33)
unique(cidades_rj$name_muni)

cidades_rj = cidades_rj %>% filter(name_muni %in% c('Japeri', 'Nova Iguaçu','Seropédica'))

# Plotando um mapa base Teste

ggplot()+
  geom_sf(data = queimados, aes(geometry = geom))

#carregando os dados do censo
base = read_csv2("Pessoa03_RJ.csv") %>% 
  select(1:9) %>% 
  mutate(porcent_negros = round((V003 + V005) / V001 *100,2),
         porcent_brancos = round(V002 / V001 * 100,2),
         porcent_indigenas = round( V006 / V001 *100,2 ),
         porcent_amarelos = round(V004 / V001 *100,2),
         code_tract = Cod_setor
         )

# Mudando tipo de dados
typeof(queimados$code_tract)

queimados$code_tract = as.double(queimados$code_tract)

# Juntando os dados para o mapa
queimados_mapa = left_join(queimados, base, by="code_tract" )

intervalos <- c(0, 20, 40, 60,80, 101)
cores <- c("#f0f7da", "#c9df8a", "#77ab59", "#36802d", "#234d20")

# Dividir a coluna "porcentagem_valas_rios" em categorias usando a função "cut"
queimados_mapa$porcent_negros_int <- cut(queimados_mapa$porcent_negros, intervalos, include.lowest = TRUE,labels = c("0 a 25%", "25 a 50%", "50 a 75%", "75 a 100%"))


# Plotando mapa
ggplot() +
  geom_sf(data = queimados_mapa, aes(geometry = geom, fill = porcent_negros_int)) +
  labs(title = 'População Negra por Setor Censitário no Municipio de Queimados',
       subtitle = 'Fonte - Censo 2010',
       caption = 'Elaborado por Christian Basilio') +
  scale_fill_manual(values = cores, name = "% de Domicídios",
                    guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "cm"), pad_y = unit(1, "cm")) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        text = element_text(family = "Arial"))


# Criando um mapa de Quantidade no municipio
queimados_cor = queimados_mapa %>% summarize(total_brancos = sum(V002),
                                             total_negros = sum(V003) + sum(V005),
                                             total_amarelos = sum(V004),
                                             total_indigenas = sum(V006)
                                             )


# ----------------------------
##### olhando para esgotamento sanitário
esgotamento = read_csv2('Domicilio01_RJ.csv') %>% select(1,3,19,22,23,24,37) %>% 
  mutate(porcentagem_esgotamento_publica = round(V017 / V001 * 100,2),
         porcentagem_valas_rios = round( (V020 + V021 + V022) / V001 * 100,2),
         porcentagem_coleta_lixo = round ( V035 / V001 *100, 2  ),
         code_tract = Cod_setor
         )

# Juntando dados
base_esgotamento = left_join(queimados, esgotamento, by = 'code_tract')


# Plotando mapa de esgotamento Publico

# selecionando novamente as cores
intervalos <- c(0, 25, 50, 75, 101)
cores <- c("#f0f7da", "#c9df8a", "#77ab59", "#234d20")

base_esgotamento$pub_red_int <- cut(base_esgotamento$porcentagem_esgotamento_publica, intervalos, include.lowest = TRUE,labels = c("0 a 25%", "25 a 50%", "50 a 75%", "75 a 100%"))



ggplot() +
  geom_sf(data = base_esgotamento, aes(geometry = geom, fill = pub_red_int)) +
  labs(title = 'Domicílios com esgotamento sanitário ligados a rede geral de esgoto por Setor Censitário no Municipio de Queimados',
       subtitle = 'Fonte - Censo 2010',
       caption = 'Elaborado por Christian Basilio') +
  scale_fill_manual(values = cores, name = "% de Domicídios",
                    guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "cm"), pad_y = unit(1, "cm")) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        text = element_text(family = "Arial"))

# Plot mapa de Esgotamento precario

# fatores de esgotamento precario
base_esgotamento$esg_precario <- cut(base_esgotamento$porcentagem_valas_rios, intervalos, include.lowest = TRUE,labels = c("0 a 25%", "25 a 50%", "50 a 75%", "75 a 100%"))


ggplot() +
  geom_sf(data = base_esgotamento, aes(geometry = geom, fill = esg_precario)) +
  labs(title = 'Domicílios com Esgotamento precário por Setor Censitário em Queimados',
       subtitle = 'Fonte - Censo 2010',
       caption = 'Elaborado por Christian Basilio') +
  scale_fill_manual(values = cores, name = "% de Domicídios",
                    guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "cm"), pad_y = unit(1, "cm")) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        text = element_text(family = "Arial"))



# regioes de queimados
media_porcent_pop_valas_rios = base_esgotamento %>% group_by(name_subdistrict) %>% summarise(porcent = mean(porcentagem_valas_rios), total = sum(V020 + V021 + V022))

ggplot(data = media_porcent_pop_valas_rios)+
  geom_sf( aes(geometry = geom, fill = total))+
  labs(title = 'Domicílios com Esgotamento precário por Região em Queimados',
       subtitle = 'Fonte - Censo 2010',
       caption = 'Elaborado por Christian Basilio') +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "cm"), pad_y = unit(1, "cm")) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        text = element_text(family = "Arial"))

#-----------------------------
