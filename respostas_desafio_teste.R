

## 1. Pacotes a serem utilizados ----

x <- c('rgdal','tmap','dplyr','raster','rgeos','ggplot2','gstat',
       'openair','sf','sp', 'hydroGOF','Metrics','geobr',
       'geojsonio','mapview', "reshape",'ggplot2')


# Funcao para carregar todos os pacotes, ou instalar e carregar caso nao tenha algum pacote
lapply(x, function(i){
  if(i %in% rownames(installed.packages())){
    library(i,character.only = T)
  }else{
    install.packages(i)
    library(i,character.only = T)
  }
}
)

setwd(getwd())

###############################################################################

# (a) Indique-nos uma amostra da assinatura espectral de um pixel onde a vegetacao esta saudavel,
# e de um pixel com solo exposto, comparando os valores e como ele se comporta no
# espectro.

# Aqui carregamos o Raster Multibandas do dia 25-06-2020, de onde extrairemos 
# as assinaturas

raster_multiband <- stack("multiband/multiband_20200625.tif") 

# Designou-se pontos no programa QGIS, onde visualmente se via as duas classes
# meniconadas na questao

pontos <- readOGR("shp/pontos_utm_extract.shp")

#renomeia as bandas que compoe o raster stack

names(raster_multiband) <- c('Blue', 'Green','Red','Red1','Red2','Red3','NIR','Red4',
                             'SWIR1','SWIR2')

# extrai a assinatura em uma matriz
signature <- extract(raster_multiband, pontos, fun=NULL, na.rm=FALSE, weights=FALSE, 
        normalizeWeights=TRUE, cellnumbers=FALSE, small=TRUE, df=FALSE,
        factors=FALSE, sp=FALSE)

# renomeia classes
rownames(signature) <- c("Vegetacao", "Solo")

# aqui criamos um mapa para que seja possivel visualizar a localizacao dos pontos
# escolhidos em vermelho sobre composicao RGB

new_raster <- raster_multiband

for (i in  1:10) {
  new_raster[[i]] <- stretch(raster_multiband[[i]], maxv = 255, minv = 0, minq = 0.1, maxq = 0.9)  
} 

mapa_rgb <- tm_shape(new_raster)+tm_rgb(r=1, g=2, b=3, max.value = max(maxValue(new_raster)))+
  tm_shape(pontos) + 
  tm_dots(size=0.3, col='red')+
  tm_layout(legend.outside = T)

mapa_rgb

# funcao melt torna a matriz da assinatura em dataframe
melt_sig <- melt(signature)

#renomeia as colunas
colnames(melt_sig) <- c("Tipo","Bandas", "Reflectancia")

#ordena as bandas do Sentinel 2 por comprimento de onda
band_order <- c('Blue', 'Green','Red','Red1','Red2','Red3','NIR','Red4',
                'SWIR1','SWIR2')

# Plotagem grafico de linhas
assinaturas_espectrais <- 
  ggplot(melt_sig, aes(x= factor(Bandas, level= band_order), y=Reflectancia, group = Tipo))+
         geom_line(aes(color=Tipo))

assinaturas_espectrais+xlab("Bandas")

###############################################################################

# (b) Mostre-nos uma matriz com valores numericos contendo as entradas dos pixels de uma
# imagem NDVI.


# Primeiro carregamos os Raster NDVI gerados no script de aquisicao

ndvi_path =list.files(path = "ndvi/", pattern = '.tif', full.names = TRUE)

ndvi_stack <- stack(ndvi_path)

# a matriz de primeiro Raster, o do dia 02-01-2020, esta disposta abaixo
matriz <- as.matrix(ndvi_stack[[1]])
matriz

###############################################################################

# (c) Diante dessa analise temporal, conte-nos o momento em que voce supoe que houve o plantio,
# o crescimento da vegetacao e a colheita. Detalhando e justifique as metricas analisas.


# Inicialmente gostaria de apresentar a primera imagem disponivel com pouca 
# cobertura de nuvem, no dia 12-01-2020

plot(ndvi_stack[[2]])

# Nela, percebe-se que as areas dos pivos centrais estao com NDVI proximo de 0,
# Bem distinto da area de vegetacao nativa a sudoeste da imagem e das demais areas
# de lavoura, estando parecidas com as areas de solo exposto fora do pivo.
# Portanto, ainda nao havia sido semeada a cultura.

# A segunda imagem bastante ilustrativa e a do dia 16-02-2020

plot(ndvi_stack[[7]])

# Nesta ja se ve um comeco de distincao entro solo exposto e a area irrigada, 
# lado a lado na regiao oeste da imagem, com as linhas de plantio basteante definidas
# com NDVI um pouco mais alto entre 0.4 e 0.6. Isso indica que a cultura esta emergindo, 
# levando a conclusao que a o plantio se deu entre a segunda metade de
# Janeiro e o comeco de Fevereiro
 

# a terceira imagem e do dia 01-04-2020

plot(ndvi_stack[[14]])

# Nesta imagem e possivel ver que todas as areas agricolas estao com NDVI alto,
# proximo de 1. Assim, conlcuimos que a cultura atingiu seu estagio de maximo
# potencial vegetativo no inico de Abril

# A quarta imegem e do dia 26-04-2020

plot(ndvi_stack[[17]])

# Nesta ja vemos que alguumas areas de pivo ja devem ter sido colhidas, porem
# nos pivos a Oeste da imagem ve-se ja um valor de NDVI ntre 0.5 e 0, o que indica
# que, como e de habito de culturas anuais, ela ja perdeu sua capacidade vegetativa
# e esta em fase de maturacao de graos

# Esta imagem e de 16-05-2020 

plot(ndvi_stack[[20]])

# Nela, ja percebe-se um valor de NDVI entre 0.2 e 0 dentro dos pivos. Isso indica
# que na primeira quinzena de Maio a cultura foi colhida, provavelmente uma variedade
# de ciclo precoce devido aos 100 dias aproximadamente entre planio e colheita

# Por se tratar de uma area e rigada em uma regiao propicia (MT),
# e possivel fazer um terceiro ciclo, conforme ve-se na imagem de 31-05-2020

plot(ndvi_stack[[22]])

# Nela ja se ve novamente as linhas de plantio com NDVI mais elevado, proximo de 0.3,
# do que as areas fora dos pivos, mais proximos de 0, indicando um novo plantio
# logo apos a colheita, na terceira semana de Maio. A vegetacao nativa continua
# com  NDVI proximo de 1, contrastando com o solo exposto

# A ultima imagem da serie e de 25-6-2020

plot(ndvi_stack[[26]])

# Nela se ve a area irrigada com NDVI proximo de 1 bastante demarcada, contrastando
# com a area de sequeiro onde nao foi possivel fazer um novo ciclo com NDVI
# proximo de 0

###############################################################################

# (d) Se fosse necessario efetuar um download de 50 imagens do Sentinel 2A e 50 imagens do
# Landsat 8 em modo lote (batch), como voce faria isso? Conte para nos qual seria sua estrategia
# de ETL, e nos escreva um pseudocodigo minimalista desde o momento da aquisicao,
# ate o armazenamento e processamento desse volume de dados

# Para responder essa perunta, vou usar o memso metodo que usei para fazer a
# aquisicao das imagens para responder as questoes anteriores. Usei a API do 
# Google Earth Engine, por meio do pagote rgee. O script detalhado esta nessa
# pasta e chama-se download_img_projetos

remotes::install_github("r-spatial/rgee", force = TRUE)
library(rgee)

ee_install()

ee_Initialize(
  email = "gabriel.ferraz@agrotools.com.br",
  drive = TRUE,
  gcs = F
) 

ee_check()

# A determinacao do Tier escolhido e feito automaticamente por meio de um shapefile
# ponto que represente a area de interesse.

# Limitando area
area <- readOGR("inputs\\shp\\sorriso-mt\\sorriso-mt.shp") 

# definindo centroide
centroide <- as.data.frame(coordinates(gCentroid(area, byid = T)))

# converte X do centoide para objeto reconhecivel para o RGEE
x = mean(centroide$x) 
# converte Y do centoide para objeto reconhecivel para o RGEE
y = mean(centroide$y)

# Define a coordenada em objeto reconhecivel para o RGEE
point <- ee$Geometry$Point( x, y) 

# Quebrando o poligono em varios pontos, com suas respectivas LAT e LONG.
area_f<-fortify(area) 

# Criando uma lista vazia
lpi <- list() 

# Colocando todas as combinacoes de LAT e LONG dentro dos espacos desta lista
lpi <- lapply(1:nrow(area_f), function(i){
  a <- as.numeric(area_f$long[i])
  b <- as.numeric(area_f$lat[i])
  c <- append(a,b)
  return(c)
}
)

# torna o poligono em objeto reconhecivel por RGEE
roi <- ee$Geometry$Polygon(lpi) 

#Quebrando a extensao em varios pontos, com suas respectivas LAT e LONG.
extensao_f <- fortify(as(extent(area), 'SpatialPolygons')) 

# cria lista de coordenadas da extensao e adiciona as coordenadas no loop abaixo
lpi_extensao <- list() 
lpi_extensao <- lapply(1:nrow(extensao_f), function(i){
  a <- as.numeric(extensao_f$long[i])
  b <- as.numeric(extensao_f$lat[i])
  c <- append(a,b)
  return(c)
}
)

extensao <- ee$Geometry$Polygon(lpi_extensao) # torna a extensao objeto reconhecivel por RGEE

# A aquisicao de imagens em lote e bastante facil por ser possivel a estrutura
# de um for loop no R, variando de acordo com uma sequencia de datas. Basta escolher

# Cria sequencia de datas dentro da janela requisitada.
# No caso, length.out indica que sera0 50 imagens. e importante se certificar que
# a difetenca entre cada data seja igual ou maior ao periodo de revisita do satelite
# escolhido

datas <-  seq(from = as.Date('2019-07-01'), to = as.Date('2020-07-01'), length.out = 50)

# Define quantas datas serao e subtrai 1, para ser input do Loop abaixo
range_datas <- seq_along(datas)[-length(seq_along(datas))]

for (dia in range_datas) {
  #torna data de inicio objeto reconhecivel RGEE
  start = ee$Date(as.character(datas[dia])) 
  #torna data final objeto reconhecivel RGEE
  finish = ee$Date(as.character(datas[dia+1])) 
  
  # Aqui entra a escolha de satelite. No caso abaixo escolheu-se sentinel 2 1C, que
  # no portal do GEE chama-se "COPERNICUS/S2_SR". Na URL
  # https://developers.google.com/earth-engine/datasets/catalog encntra-se outros
  # como LandSat 8, que seria "LANDSAT/LC08/C01/T2_TOA", ja com correcao TOA
  
  # Uma vez definido onde queremos procurar, o algoritmo monta uma colecao de imagens
  
  #Acesso ao banco de dados de imagens Sentinel 2A
  imagecol <- ee$ImageCollection("COPERNICUS/S2_SR")$
    #Filtrando as imagens do nosso ponto de interesse
    filterBounds(point)$
    # Filtrando pelo intervalo de tempo desejado
    filterDate(start, finish)$ 
    # Preferencialmente sem a presenca de nuvens
    sort("CLOUD_COVER", F)
  
  # seleciona a primeira imagem da lista, a com menos nuvens da colecao
  image <- imagecol$first() 
  
  ### Selecionar todas as bandas desejadas
  selecao_de_bandas <- ee$Image(image)$
    select(c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12"))
  
  # Get data da imagem e converte em string para nomear o arquivo 
  data_da_imagem <- ee_get_date_img(image, time_end = FALSE)
  data_da_imagem_str <- gsub("-","",substr(as.character(data_da_imagem$time_start),1,10))
  
  
  # Ate aqui, nao ha imagem Raster consumindo memoria do PC, o que pode ser bem pesado.
  # Para que possamos armazenar localmente o Raster, converte-se a imagem ja
  # recortada para a area ou poligono de interesse, o que e mais economico quanto
  # a memoria
  
  multiband_raster <- ee_as_raster(selecao_de_bandas,region = ee$Geometry$Polygon(lpi_extensao),
                                   via = 'drive')
  # Salva Raster no disco
  writeRaster(multiband_raster, filename=paste0("multiband/multiband_",data_da_imagem_str,".tif"),
              options="INTERLEAVE=BAND", overwrite=TRUE)
 
}

# Esse e um codigo bastante simples, uma vez que o GEE te possibilita fazer uma
# serie de correcoes de ruido e melhoria nos dados,
# como cloud mask para ficar em um bem importante. Alem disso, ja e possivel
# fazer calculos com os dados antes de ter o Rster localmente, como e feito no 
# script download_img_projetos dessa pasta, onde e salvo apenas o Raster de NDVI
# localmente. Se for necessario, e possivel fazer todo o processamento da imagem,
# sem salvar nada no disco, apenas os eventuais outputs como mapas em .png



