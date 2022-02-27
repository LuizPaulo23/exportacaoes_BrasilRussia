rm(list = ls())

setwd("~/Área de Trabalho/Ciência de Dados: Machine Learning & Deep Learning/COMEX")

# Bibliotecas 

library(data.table) # importação 
library(readr) # importação 
library(tidyverse) # manipulação do dataset 
library(DataExplorer) # análise exploratória 
library(cowplot) # junção de plotes 
library(TSstudio) # plotes serie temporal 
library(echarts4r) # plotes interativo 
library(plotly) # plotes interativos 
library(lubridate) # dates and times
library(dygraphs)  ## plotes interativos
library(forecast) # series temporais 
library(downloadthis) # Download csv
library(geobr) # maps 
library(ggspatial) # maps 
library(waffle) # plote
library(urca) # Teste raiz unitaria 

# Importação do dataset, dos codigos dos países e dos NCMs 

data.base <- fread("EXP_COMPLETA.csv")
cod.nation <- fread("PAIS.csv", sep = ";", encoding = "Latin-1")
cod.ncm <- fread("NCM.csv", sep = ";", encoding = "Latin-1")

# Limpando e organizando a base de dados 

data.base = merge(data.base, cod.nation, by= c("CO_PAIS")) # Unindo os datasets 
rm(cod.nation) # removendo dataset para limpar memoria 
data.base = merge(data.base, cod.ncm, by = c("CO_NCM"))
rm(cod.ncm) # removendo dataset para limpar memoria 

# Filtrando apenas a Russia (1997-2021)

data.base = data.base %>% 
  filter(NO_PAIS %in% c("Rússia") & 
           CO_ANO <= 2021) 

# Nomeando os meses 

data.base$CO_MES <- factor(data.base$CO_MES, 
                           levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                           labels = c("Janeiro", "Fevereiro", "Março", 
                                      "Abril", "Maio", "Junho", 
                                      "Julho", "Agosto", "Setembro", 
                                      "Outubro", "Novembro", "Dezembro"))

# Sumarizando 

data.time <- data.base %>% 
  select(CO_ANO, VL_FOB, NO_NCM_POR, CO_MES) %>% 
  rename("Valor Exportado - FOB" = "VL_FOB", 
         "Time" = "CO_ANO", 
         "Produto" = "NO_NCM_POR", 
         "Meses" = "CO_MES")

sumario <- data.time %>% group_by(Time, Meses) %>% 
  summarise(FOB = sum(`Valor Exportado - FOB`)/10^6)

# Salvando as bases de dados organizadas 

rm(data.base)
data.time <- write_csv(data.time, file = "data.time.csv")
sumario <- write_csv(sumario, file = "sumario.csv")
data.map <- write_csv(data.map, file = "data.map.csv")

# Uma pção para reduzir consumo de memória na importação 
# importar as bases limpas e organizadas 

data.time <- read.csv("data.time.csv")
sumario <- read_csv("sumario.csv")
data.map <- read_csv("data.map.csv")

# Datasets limpos para download 

data.time %>%
  download_this(
    output_name = "data.time",
    output_extension = ".csv",
    button_label = "Download exportações mensais com descrição do produto",
    button_type = "default",
    has_icon = T,
    icon = "fa fa-save",
    class = "button_large")

sumario %>%
  download_this(
    output_name = "sumario",
    output_extension = ".csv",
    button_label = "Download sumario das exportações mensais",
    button_type = "default",
    has_icon = T,
    icon = "fa fa-save",
    class = "button_large")

sumario %>%
  download_this(
    output_name = "data.map",
    output_extension = ".csv",
    button_label = "Download exportações localizadas por Estado",
    button_type = "default",
    has_icon = T,
    icon = "fa fa-save",
    class = "button_large")


# Passando para série temporal 

time.series = ts(data = sumario, 
                 start = c(1997, 1),
                 end = c(2021, 12), frequency = 12)

# Plotando em plote interativo 

dygraph(time.series[, 3],
        main = "Exportações para a Rússia (1997-2021)", xlab = "Ano", 
        ylab = "Em milhões - FOB (US$)") %>% 
  dyRangeSelector(dateWindow = c("1997-01-01", "2021-12-31"), 
                  fillColor = "red", strokeColor = "blue")


# Boxplot - por mês 

box.month <- ggplot(sumario) +
  aes(x = Meses, y = FOB) +
  geom_boxplot(shape = "circle", fill = "blue", alpha = 0.5) +
  labs(x = "", y = "Em milhões - FOB (US$)", 
       title = "Boxplot por Mês - Exportações para a Rússia (1997-2021)", 
       caption = "Elaboraçao de Luiz Paulo Tavares Gonçalves")+
  theme_bw()

box.month+xlim("Janeiro", "Fevereiro", "Março", 
               "Abril", "Maio", "Junho", 
               "Julho", "Agosto", "Setembro", 
               "Outubro", "Novembro", "Dezembro")

ggplotly(box.month)

# Boxplot por Ano 

box.year <- ggplot(sumario) +
  aes(x =Time, y = FOB) +
  geom_boxplot(shape = "circle", fill = "blue", alpha = 0.5) +
  labs(x = "", y = "Em milhões - FOB (US$)", 
       title = "Boxplot por Ano - Exportações para a Rússia (1997-2021)", 
       caption = "Elaboraçao de Luiz Paulo Tavares Gonçalves")+
  theme_bw()

ggplotly(box.year)

shapiro.test(sumario$FOB)

ggplot(aes(x= FOB),
       data = sumario)+
  geom_histogram(aes(y=..density..), bins = 30L, fill = "blue", alpha = 0.5)+
  geom_density(color="red")+
  xlab("")+
  ylab("Densidade")+
  ggtitle("Histograma - Dist. Exportações para Rússia (1997-2021)",
          subtitle = "P-valor do teste de Shapiro-Wilk: 6.306e-10")+
  theme_bw()


# Visualizao por Estado 

data.map <- data.map %>% 
  select(SG_UF_NCM, CO_ANO, CO_MES, 
         VL_FOB) %>% 
  mutate()

uf <- data.map %>% 
  group_by(SG_UF_NCM, CO_ANO) %>% 
  summarise(fob = sum(VL_FOB)/10^6)

graf.uf <- uf %>%
  filter(CO_ANO >= 2020L & CO_ANO <= 2021L) %>%
  ggplot() +
  aes(x =reorder(SG_UF_NCM, fob), weight = fob) +
  geom_bar(fill = "blue", alpha = 0.5) +
  labs(y = "Em milhões - FOB (US$)", x = "Estados",
       title = "Exportações para a Russia por Estado de origem (2020-2021)",
       caption = "Elaboração de Luiz Paulo T. Gonçalves")+
  theme_bw() +
  facet_wrap(vars(CO_ANO), nrow = 2L)
ggplotly(graf.uf)

# Plotando mapas 

geo <- read_state(code_state = "all", year = 2016)
geo <- geo %>% 
  rename("SG_UF_NCM" = "abbrev_state")

maps = merge(uf, geo, by = c("SG_UF_NCM"))
maps <- maps %>% 
  filter(CO_ANO == 2021)

plote.map <- ggplot(maps$geom)+geom_sf(aes(fill= maps$fob))+
  scale_fill_viridis_c(direction = -1, limits = c(0.0535, 403.1440))+
  labs(fill = "FOB (US$)",
       title = "Exportações para a Russia por Estado de origem (2021)",
       subtitle = "") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale()+
  theme_bw()+
  theme(legend.position = "bottom")

ggplotly(plote.map)

# Porcentagem - Participação 

por.uf <- uf %>% filter(CO_ANO == 2021)%>% 
  mutate(porcentagem = (fob/1587.205)*100) %>% 
  rename("UFs" = "SG_UF_NCM") %>% 
  filter(porcentagem > 10.00)


ggplot(data = por.uf, 
       aes(fill = UFs, values = porcentagem)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "#ffffff",flip = F) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() +
  labs(title = "Parcela das exportação para Rússia por Estado em 2021 (%)",
       subtitle = "Aproximadamente 25.40% das exportações para a Rússia são de São Paulo
16.02% de MT | 14.13% de PR | 11.59% de MG",
       caption = "Luiz Paulo T. Gonçalves")

# Produtos exportados 

base.export <- data.time %>% 
  filter(Time == 2021) %>% 
  rename("FOB" = "Valor.Exportado...FOB") 

# Sumarizar em ordem decrescente 

best.export = base.export %>% 
  group_by(Produto) %>% 
  summarise(FOB = sum(FOB), 
            FOB = FOB/10^6) %>% 
  arrange(desc(FOB))

# Passando para uma tabela interativa 

best.export %>% head(1068) %>% 
  DT::datatable()

top<- best.export %>%
  filter(FOB >= 30L & FOB <= 345L) %>%
  ggplot() +
  aes(x = reorder(Produto,FOB), weight = FOB) +
  geom_bar(fill = "blue", alpha = 0.5) +
  labs(x = "", y = "Em milhões FOB US$", 
       title = "Os 10 produtos mais exportados para Rússia (2021)")+
  coord_flip()+
  theme_bw()

ggplotly(top)

# Decompondo a série temporal 

ts_decompose(time.series[, 3], 
             type = "additive")

# Analisando a presença de sazonalidade 

month.time <- ggseasonplot(time.series[, 3], year.labels = TRUE) +
  geom_point() + labs(
    x = "", y = "Em milhões - FOB (US$)", 
    title = "Exportações para a Rússia (1997-2021)", 
    caption = "Elaboração de Luiz Paulo Tavares Gonçalves")+
  theme_bw()

ggplotly(month.time) # Passando para o modo interativo 

ggseasonplot(time.series[, 3], polar = T)+
  labs(x = "Meses", y = "Em milhões - FOB (US$)",
       title = "Verificando possíveis pontos de sazonalidade", 
       caption = "Elaboração de Luiz Paulo Tavares Gonçalves")+
  theme_bw()

# Heatmap

ts_heatmap(time.series[, 3], 
           title = "Heatmap - Exportações para a Rússia (1997-2021) ")

# Sazonalidade 


monthplot(time.series[, 3], col.base = 2, lty.base = 2)
legend("topleft", legend = c("FOB", "Média"), lty = c(1,2), 
       col = c(1,2), bty = "n")

# Teste pp (Philips-Perron)

# Ho = estacionário: p > 0.05
# Ha = não estacionário: p <= 0.05

pp <- ur.pp(time.series[, 3])
summary(pp)

# Teste df (Dickey Fuller)

# Ho = não estacionário: teste estatístico > valor crítico
# Ha =  estacionário:  teste estatístico < valor crítico

df2 <- ur.df(time.series[, 3])
summary(df2)
