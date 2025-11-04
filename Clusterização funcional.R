# Remove todos os objetos do ambiente de trabalho do R
rm(list = ls())

# -------------------------------------------------------------------
# temperatura_oceanica
# -------------------------------------------------------------------


library(fda)       # Pacote para análise de dados funcionais
library(fda.usc)   # Extensão do pacote fda, com funções adicionais

# -------------------------------------------------------------------
# Importando arquivo com dados de temperatura do mar
# -------------------------------------------------------------------

# Lê o arquivo CSV com as temperaturas oceânicas
dados <- read.csv("C:/Users/jacim/Downloads/temperatura_oceanica.CSV")

# Visualiza as primeiras linhas do arquivo importado
head(dados)

# -------------------------------------------------------------------
# Convertendo os dados em uma matriz
# -------------------------------------------------------------------

dados <- as.matrix(dados)  # Converte o data frame em matriz numérica

# -------------------------------------------------------------------
# Criando um objeto funcional (fdata) a partir da matriz de dados
# -------------------------------------------------------------------

matriz <- fdata(dados)     # Cria o objeto funcional
str(matriz)                # Mostra a estrutura do objeto criado

# Define os nomes e rótulos dos eixos do objeto funcional
matriz$names <- list(main = "Agrupamento", xlab = "t (Meses)", ylab = "X(t)")

# -------------------------------------------------------------------
# Agrupando as curvas funcionais com k-means funcional
# -------------------------------------------------------------------
set.seed(101)  # semente para reprodutibilidade dos resultados

# Executa o algoritmo k-means funcional com 3 grupos
P_clima1 <- kmeans.fd(matriz, ncl = 3, cluster.size = 3, max.iter = 100)

# -------------------------------------------------------------------
# Identificando as curvas que pertencem a cada grupo (cluster)
# -------------------------------------------------------------------

grupo1 <- which(P_clima1$cluster == 2)  # Grupo 1 → El Niño
grupo2 <- which(P_clima1$cluster == 3)  # Grupo 2 → Neutro
grupo3 <- which(P_clima1$cluster == 1)  # Grupo 3 → La Niña

# Mostra o tamanho (quantidade de curvas) de cada grupo
length(grupo1)  # Quantos anos são El Niño
length(grupo2)  # Quantos anos são Neutros
length(grupo3)  # Quantos anos são La Niña

# -------------------------------------------------------------------
# Função para classificar cada observação com base no cluster
# -------------------------------------------------------------------

fenomeno <- c()  # Cria vetor vazio que receberá os nomes dos fenômenos

yy <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == 2) {
      fenomeno = append(fenomeno, "El Nino")
    } else if (x[i] == 3) {
      fenomeno = append(fenomeno, "Neutro")
    } else {
      fenomeno = append(fenomeno, "La Nina")
    }
  }
  return(fenomeno)
}

# Aplica a função yy() para traduzir os números dos clusters em nomes
x = as.numeric(P_clima1$cluster)
resultado = yy(x)
print(resultado)  # Exibe o vetor com os nomes dos fenômenos

# -------------------------------------------------------------------
# Criação de um data frame com os nomes dos clusters
# -------------------------------------------------------------------

nomes_clusters <- c("LA NINA", "EL NINO", "NEUTRO")

# Cria um data frame com os resultados
linha_df <- data.frame(Fenomenos = resultado)
linha_df$Cluster <- x  # Adiciona coluna numérica do cluster

# Repete as linhas para ajustar estrutura 
linha_df <- linha_df[rep(1:nrow(linha_df), times = 16), ]
rownames(linha_df) <- 1:nrow(linha_df)  # Reatribui os nomes das linhas

# -------------------------------------------------------------------
# Salva os objetos de clusterização em um arquivo .RData
# -------------------------------------------------------------------

save(x, P_clima1, file = "C:/Users/jacim/OneDrive/Documentos/clusterizacao_RG.RData")

# -------------------------------------------------------------------
# Contabiliza a quantidade de cada fenômeno em períodos distintos
# -------------------------------------------------------------------

# 1961–2019
table(factor(P_clima1$cluster, levels = 1:3, labels = nomes_clusters))

# 1990–2019
table(factor(P_clima1$cluster[30:58], levels = 1:3, labels = nomes_clusters))

# -------------------------------------------------------------------
# Criação de data frame final com datas e fenômenos classificados
# -------------------------------------------------------------------

d <- data.frame(
  as.vector(P_clima1$cluster),
  datas = c("1961-1962","1962-1963","1963-1964","1964-1965",
            "1965-1966","1966-1967","1967-1968","1968-1969",
            "1969-1970","1970-1971","1971-1972","1972-1973",
            "1973-1974","1974-1975","1975-1976","1976-1977",
            "1977-1978","1978-1979","1979-1980","1980-1981",
            "1981-1982","1982-1983","1983-1984","1984-1985",
            "1985-1986","1986-1987","1987-1988","1988-1989",
            "1989-1990","1990-1991","1991-1992","1992-1993",
            "1993-1994","1994-1995","1995-1996","1996-1997",
            "1997-1998","1998-1999","1999-2000","2000-2001",
            "2001-2002","2002-2003","2003-2004","2004-2005",
            "2005-2006","2006-2007","2007-2008","2008-2009",
            "2009-2010","2010-2011","2011-2012","2012-2013",
            "2013-2014","2014-2015","2015-2016","2016-2017",
            "2017-2018","2018-2019"),
  fenomeno = resultado
)

# -------------------------------------------------------------------
# Visualiza a tabela de resultados
# -------------------------------------------------------------------

View(d)

#####################################################################
