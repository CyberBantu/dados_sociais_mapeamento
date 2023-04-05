library(dplyr)
library(ggplot2)
library(geobr)
library(readr)
library(ggspatial)
library(sf)

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

area_interesse <- st_bbox(queimados_mapa) + c(-0.2, 0.2, -0.2, 0.2)

# Plotando mapa
ggplot() +
  geom_sf(data = queimados_mapa, aes(geometry = geom, fill = porcent_negros)) +
  labs(title = 'População Negra em Queimados por Setor Censitário',
       subtitle = 'Fonte - Censo 2010',
       caption = 'Elaborado por Christian Basilio') +
  scale_fill_gradient(low = "white", high = "#ff003d", name = "% de Domicídios",
                      guide = guide_colorbar(barwidth = 0.5,
                                             barheight = 7.5,
                                             title.position = "top",
                                             title.hjust = 0.15)) +
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
