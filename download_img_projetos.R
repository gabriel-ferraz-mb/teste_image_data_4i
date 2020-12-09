## 1. Pacotes a serem utilizados ----

x <- c('rgdal','tmap','dplyr','raster','rgeos','gstat',
       'openair','sf','sp', 'hydroGOF','Metrics','geobr', 
       'googledrive','geojsonio','mapview')

# Função para carregar todos os pacotes, ou instalar e carregar caso não não tenha algum pacote
lapply(x, function(i){
  if(i %in% rownames(installed.packages())){
    library(i,character.only = T)
  }else{
    install.packages(i)
    library(i,character.only = T)
  }
}
)

# Instala e carrega terminal python Google Earth Engine

remotes::install_github("r-spatial/rgee", force = TRUE)
library(rgee)

# Muito importante para que o código rode setar o work directory para o memso onde está o códico

setwd(getwd())


# Antes de começarmos a trabalhar com o pacote 'rgee', precisamos acessar o site do Google Earth Engine 
# e fazer o registro de sua conta Google (GMail) dentro do site:

#https://earthengine.google.com/

# Este procedimento, servirá para o Google envie uma chave pessoal para o usuário poder operar de 
# forma segura dentro de um ambiente programável.

# Em seguida instalamos as dependências e os diretórios no computador 
# onde o pacote irá trabalhar.Isso só será necessário na primeira vez.
# Entre as dependências que serão instaladas, está a linguagem Python para se 
# comunicar com Google Earth Engine (GEE), os arquivos da API do GEE, além de 
# alguns pacotes extras que 'rgee' necessita.

ee_install()


# Após instalado, sempre que for necessário trabalhar com o 'rgee', será necessário inicializa-lo

ee_Initialize(
  email = "gabriel.ferraz@agrotools.com.br",
  drive = TRUE,
  gcs = F
) 

ee_check() ## checa o status do terminal RGEE, se estiver ok podemos continuar

## Agora , entra-se com o polígono a ser trabalhado.

area <- readOGR("inputs\\shp\\sorriso-mt\\sorriso-mt.shp") # Limitando área

centroide <- as.data.frame(coordinates(gCentroid(area, byid = T))) # definindo centróide

x = mean(centroide$x) # converte X do centóide para objeto reconhecível para o RGEE
y = mean(centroide$y) # converte Y do centóide para objeto reconhecível para o RGEE

point <- ee$Geometry$Point( x, y) # Define a coordenada em objeto reconhecível para o RGEE

area_f<-fortify(area) # Quebrando o polígono em vários pontos, com suas respectivas LAT e LONG.

lpi <- list() # Criando uma lista vazia

# Colocando todas as combinações de LAT e LONG dentro dos espaços desta lista
lpi <- lapply(1:nrow(area_f), function(i){
  a <- as.numeric(area_f$long[i])
  b <- as.numeric(area_f$lat[i])
  c <- append(a,b)
  return(c)
}
)

roi <- ee$Geometry$Polygon(lpi) # torna o polígono em objeto reconhecível por RGEE

extensao_f <- fortify(as(extent(area), 'SpatialPolygons')) #Quebrando a extensão em vários pontos, com suas respectivas LAT e LONG.
lpi_extensao <- list() # cria lista de coordenadas da extensão e adiciona as coordenadas no loop abaixo
lpi_extensao <- lapply(1:nrow(extensao_f), function(i){
  a <- as.numeric(extensao_f$long[i])
  b <- as.numeric(extensao_f$lat[i])
  c <- append(a,b)
  return(c)
}
)

extensao <- ee$Geometry$Polygon(lpi_extensao) # torna a extensão objeto reconhecível por RGEE


## criar diretórios para os Rasters
dir.create(file.path(getwd(), '/ndvi'), showWarnings = FALSE)
dir.create(file.path(getwd(), '/multiband'), showWarnings = FALSE)


# Vamos agora intervalo de tempo no qual a API do GEE vai buscar por imagens
# Cria sequência de datas dentro da janela requisitada, pulando semenalmente
datas <-  seq(from = as.Date('2020-01-01'), to = as.Date('2020-07-01'), by = 'week')

# Define quantas datas serão e subtrai 1, para ser input do Loop abaixo
range_datas <- seq_along(datas)[-length(seq_along(datas))]

# Agora para escolhermos qual satélite usaremos e quais imagens coletar, precisamos primeiro vasculhar
# o catálogo do GEE.

# https://developers.google.com/earth-engine/datasets/catalog


for (dia in range_datas) {
 
    start = ee$Date(as.character(datas[dia])) #torna data de início objeto reconhecível RGEE
   
    finish = ee$Date(as.character(datas[dia+1])) #torna data final objeto reconhecível RGEE

# Uma vez definido onde queremos procurar, o algorítmo monta uma coleção de imagens

    imagecol <- ee$ImageCollection("COPERNICUS/S2_SR")$  #Acesso ao banco de dados de imagens Sentinel 2A
    filterBounds(point)$ #Filtrando as imagens do nosso ponto de interesse
    filterDate(start, finish)$ # Filtrando pelo intervalo de tempo desejado
    sort("CLOUD_COVER", F)# Preferencialmente sem a presença de nuvens
    
    image <- imagecol$first() # seleciona a primeira imagem da lista, a com menos nuvens da coleção
    
    
    # Definir função que cria NDVI
    getNDVI <- function(image) {
      image$normalizedDifference(c('B8', 'B4'))
    }
    
    # Aplica função
    ndvi1 <- getNDVI(image)
    
    # Recorta o NDVI segundo polígono AOI
    clipped_ndvi <- ndvi1$clip(extensao)
    
    # Get data da imagem e converte em string para nomear o arquivo 
    data_da_imagem <- ee_get_date_img(image, time_end = FALSE)
    data_da_imagem_str <- gsub("-","",substr(as.character(data_da_imagem$time_start),1,10))
    
    # Converte o objeto RGEE em raster reconhecível pelo R
    ndvi_raster <- ee_as_raster(clipped_ndvi,region = ee$Geometry$Polygon(lpi_extensao),
                                via = 'drive')
    # Salva Raster no disco
    writeRaster(ndvi_raster, paste0("ndvi/ndvi_",data_da_imagem_str,".tif"))
    
}


# A cima, fez-se o download das imagens , converteu-se em NDVI e salvou-se apenas o NDVI no disco. Para responder a questão
## de assinatura espectral precisaremos de mias bandas além de V e IVP, de forma que abaixo repete-se o código, porém no disco
### é salvo todas as bandas exceto Aerosol, Water Vapor e Cirrus, e apenas uma data


start_multiband = ee$Date(as.character(datas[length(range_datas)-1]))

finish_multiband = ee$Date(as.character(datas[length(range_datas)]))

imagecol_multiband <- ee$ImageCollection("COPERNICUS/S2_SR")$ 
  filterBounds(point)$
  filterDate(start, finish)$
  sort("CLOUD_COVER", F)

image_multiband <- imagecol$first()

selecao_de_bandas <- ee$Image(image_multiband)$
  select(c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")) ### Selecionar todas as bandas

data_da_imagem_mb <- ee_get_date_img(image_multiband, time_end = FALSE)
data_da_imagem_str_mb <- gsub("-","",substr(as.character(data_da_imagem$time_start),1,10))


multiband_raster <- ee_as_raster(selecao_de_bandas,region = ee$Geometry$Polygon(lpi_extensao),
                            via = 'drive')

writeRaster(multiband_raster, filename=paste0("multiband/multiband_",data_da_imagem_str,".tif"), options="INTERLEAVE=BAND", overwrite=TRUE)


