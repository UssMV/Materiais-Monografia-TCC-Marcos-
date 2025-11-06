classificar_testes <- function(MAX_S, linha_df, t = 1:nrow(linha_df)) {
  
  # grupos para comparação
  EL_N <- as.vector(which(linha_df$Cluster[t] %in% c(2,3))) # El Nino e Neutro
  EL_LA <- as.vector(which(linha_df$Cluster[t] %in% c(2,1))) # El Nino e La Nina  
  LA_N <- as.vector(which(linha_df$Cluster[t] %in% c(1,3))) # La Nina e Neutro
  
  # Função FANOVA modificada para retornar apenas o p-valor do teste FP
  fanova_para_classificacao <- function(matriz, clusters) {
    set.seed(100)
    resultado_fanova <- fanova.tests(matriz, clusters,
                                     test = "FP",
                                     params = list(paramFP = list(int = c(0.025, 0.975),
                                                                  B.FP = 1000, basis = "b-spline",
                                                                  criterion = "eBIC",
                                                                  commonK = "mean",
                                                                  minK = 5, maxK = 30,
                                                                  norder = 4, gamma.eBIC = 0.7),
                                                   paramFmaxb = 1000))
    
    # Retorna apenas o p-valor do teste FP
    return(resultado_fanova$FP$pvalueFP)
  }
  
  # Executar os testes para cada comparação (apenas teste FP)
  resultado_EL_N <- fanova_para_classificacao(MAX_S[, EL_N], linha_df$Cluster[EL_N])
  resultado_EL_LA <- fanova_para_classificacao(MAX_S[, EL_LA], linha_df$Cluster[EL_LA])
  resultado_LA_N <- fanova_para_classificacao(MAX_S[, LA_N], linha_df$Cluster[LA_N])
  
  # Combinar todos os p-valores sem correção (apenas FP)
  pvalores_sem_correcao <- c(
    EL_N = resultado_EL_N,
    EL_LA = resultado_EL_LA,
    LA_N = resultado_LA_N
  )
  
  # Aplicar correção de Holm-Bonferroni para os p-valores do teste FP
  pvalores_com_correcao <- p.adjust(pvalores_sem_correcao, method = "holm")
  
  # Criar data frame com os resultados
  df_resultados <- data.frame(
    Comparacao = c("EL_N", "EL_LA", "LA_N"),
    Teste = rep("FP", 3),
    P_valor_sem_correcao = pvalores_sem_correcao,
    P_valor_com_correcao = pvalores_com_correcao
  )
  
  return(df_resultados)
}

# Exemplo de uso:
resultados <- classificar_testes(matriz_1990_2019_MAX_s , linha_df, t = 465:928)
print(resultados)
