# ==============================================================================
# Exercício de replicação de análise estatística
# 4) Gerar as análises que produzem os resultados da Tabela 12 com base nos arquivos 
# do processo. As colunas de operadores indicam o número de vezes que os operadores 
# foram utilizados nas linhas que o evento é “Found better in current neighborhood”.
# ==============================================================================

# ==============================================================================
# O Tidyverse é um conjunto de pacotes para a linguagem R projetado para ajudar 
# o desenvolvedor a importar, organizar, manipular, visualizar e comunicar 
# informações tabulares.
# ==============================================================================
install.packages("tidyverse")

# ==============================================================================
# carrega o pacote Tidyverse para a memória do R, tornando suas funções  
# disponíveis para utilização no ambiente
# ==============================================================================
library(tidyverse)

# ==============================================================================
# local onde estão as 50 pastas das rodadas
# ==============================================================================
diretorio_base <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias"

# ==============================================================================
# local onde está o arquivo instances.csv
# ==============================================================================
caminho_instances <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias/instances.csv"

# ==============================================================================
# Leitura do arquivo base e mapeamento dos nomes das instâncias.
# 
# A função read_csv2() lê arquivos de texto delimitados .csv, reconhecendo o 
# ponto e vírgula (;) como o caractere delimitador que separa cada coluna do arquivo.
# 
# O read_csv2() entrega o tibble resultante para o operador pipe (%>%), que o injeta 
# como entrada no comando seguinte "select(Instance, Order)".
# ==============================================================================
dados_instancias <- read_csv2(caminho_instances) %>%
  
  # ==============================================================================
  # O operador pipe %>% inclui o tibble resultante do "read_csv2" como o primeiro 
  # argumento implícito no "select()", bastando informar os nomes das colunas sem 
  # precisar repetir o nome da tabela ou usar cifrão ($).
  # 
  # A função "select(instance,order)" realiza uma seleção de colunas, mantendo 
  # no tibble resultante apenas as colunas Instance e Order e descartando todas 
  # as demais. 
  # 
  # O arquivo instances.csv possui originalmente 9 colunas. Como a replicação da
  # Tabela 12 precisa apenas do nome oficial de cada instância (instance) e da sua 
  # posição de exibição (order), o select() reduz o tibble de 15x9 para 15x2, deixando 
  # apenas o necessário para o cruzamento de dados nas etapas seguintes.
  # ==============================================================================
  select(instance,order)

# ==============================================================================
# Cria um vetor numérico de 50 elementos contendo a sequência de números 
# inteiros de 0 até 49 (incrementados de 1 em 1).
# Esse vetor representa os nomes dos 50 diretórios do experimento.
# ==============================================================================
rodadas <- 0:49

# ==============================================================================
# Esse bloco constrói um tibble contendo o endereço completo de todos os arquivos 
# do experimento (50 rodadas x 15 instâncias). Na prática, ele substitui os laços 
# "for" aninhados do R tradicional por uma estrutura vetorial pronta para ser percorrida 
# de uma só vez na etapa de leitura.
# 
# A função expand_grid() gera um tibble contendo todas as combinações possíveis, 
# o produto cartesiano, entre os vetores ou tabelas informados como argumentos.
# Como rodadas possui 50 elementos (0 a 49) e dados_instancias$instance possui 15 
# nomes de instâncias, ela gera automaticamente todas as 50 x 15 = 750 combinações 
# possíveis em um tibble de duas colunas (rodada e instance).
# 
# O operador pipe %>% encaminha o tibble do expand_grid() como primeiro argumento 
# implícito para a função mutate().
# ==============================================================================
grade_arquivos <- expand_grid( rodada = rodadas, instance = dados_instancias$instance ) %>%
  
  # ==============================================================================
  # A função mutate() adiciona uma terceira coluna chamada caminho ao tibble de 750 
  # linhas vindo do expand_grid(), montando de forma vetorizada o endereço completo 
  # para cada arquivo CSV do experimento. Como o comando está encadeado após o pipe 
  # %>% , recebe automaticamente a tabela com rodada e instance como entrada e anexa 
  # caminho à direita delas.
  # 
  # A função file.path() une pedaços de texto para formar um endereço de arquivo. Ela 
  # insere automaticamente o separador de pastas adequado (/ no macOS e Linux, 
  # ou \ no Windows), evitando erros de barra dupla (//) ou barra faltando ao concatenar caminhos.
  # 
  # O argumento as.character(rodada) converte os números inteiros da coluna rodada (0 a 49) 
  # para o tipo texto ("0" a "49"), para que o nome da subpasta seja manipulado como string.
  # 
  # A função "paste0(instance, ".csv")" cola textos, para cada linha, ela pega o texto 
  # da coluna instance e anexa a extensão ".csv".
  # ==============================================================================
  mutate(caminho = file.path(diretorio_base, as.character(rodada), paste0(instance, ".csv")))

# ==============================================================================
# Declaração da função "ler_melhorias_processo", responsável por ler um arquivo 
# de processo e filtrar apenas os eventos em que houve melhoria na vizinhança atual 
# ("Found better in current neighborhood").
#
# A função recebe 3 parâmetros obrigatórios: o caminho físico do CSV, o nome da 
# instância e o número da rodada, retornando um tibble filtrado com as ocorrências 
# válidas daquele arquivo.
# ==============================================================================
ler_melhorias_processo <- function(caminho_arquivo, nome_instancia, num_rodada) {

  # ==============================================================================
  # Lê apenas as colunas necessárias para o Objetivo 4.
  # 
  # A função read_csv2() lê arquivos de texto delimitados .csv, reconhecendo o ponto 
  # e vírgula (;) como o caractere delimitador que separa cada coluna do arquivo.
  # 
  # A função c() cria um vetor de texto com os nomes de duas colunas utilizadas 
  # do arquivo de processo ("Event Name" e "Operator Name"), passado ao parâmetro 
  # col_select para ignorar as demais colunas durante a importação.
  # ==============================================================================
  df <- read_csv2(caminho_arquivo, col_select = c("Event Name", "Operator Name"))

    # ==============================================================================
    # Filtra as linhas de melhoria e anexa os identificadores da instância e rodada.
    # 
    # O primeiro operador pipe %>% encaminha o tibble df como o primeiro argumento 
    # implícito para a função filter(), dispensando a repetição do nome da tabela 
    # ou do operador $.
    # 
    # A função "filter(...)" está selecionando apenas as linhas em que a coluna 
    # "Event Name" seja "Found better in current neighborhood".
    # 
    # O segundo pipe %>% pega o subconjunto reduzido de linhas devolvido pelo filter() 
    # e o encaminha diretamente como entrada para a função mutate().
    # 
    # A função mutate(...) cria novas colunas ao final do tibble. O parâmetro 
    # instance = nome_instancia cria uma nova coluna chamada instance e a preenche 
    # com o valor recebido no parâmetro nome_instancia. O parâmetro rodada = num_rodada
    # cria uma nova coluna chamada rodada e replica o número inteiro recebido em num_rodada.
    # ==============================================================================
    df %>%
      filter(`Event Name` == "Found better in current neighborhood") %>%
      mutate( instance = nome_instancia, rodada = num_rodada )
  }
  





  cat("Lendo e filtrando os 750 arquivos de processo...\n")
  
  # ==============================================================================
  # Leitura em lote dos 750 arquivos e empilhamento em um único tibble
  # ==============================================================================
  eventos_melhoria <- pmap_dfr(
    list(
      caminho_arquivo = grade_arquivos$caminho,
      nome_instancia  = grade_arquivos$instance,
      num_rodada      = grade_arquivos$rodada
    ),
    ler_melhorias_processo
  )
  
  cat("Leitura concluída! Total de melhorias registradas:", nrow(eventos_melhoria), "\n\n")
  
  # ==============================================================================
  # Padronização das siglas dos operadores e contagem acumulada nas 50 rodadas
  # ==============================================================================
  eventos_padronizados <- eventos_melhoria %>%
    mutate(
      op_limpo = str_remove_all(str_to_lower(`Operator Name`), "[- ]"),
      Operador = case_when(
        op_limpo == "extraction"     ~ "EXT",
        op_limpo == "swap"           ~ "SW",
        op_limpo == "range"          ~ "RNG",
        op_limpo == "dot"            ~ "DOT",
        op_limpo == "starquantifier" ~ "STAR",
        op_limpo == "concatenation"  ~ "CC",
        op_limpo == "negation"       ~ "NEG",
        op_limpo == "backreference"  ~ "BR",
        # Operadores menos frequentes omitidos das colunas da Tabela 12,
        # mas contabilizados no denominador do "Total (%)"
        op_limpo == "plusquantifier" ~ "PLUS",
        op_limpo == "startanchor"    ~ "START",
        op_limpo == "optional"       ~ "OPT",
        op_limpo == "endanchor"      ~ "END",
        TRUE                         ~ `Operator Name`
      )
    )
  
  contagem_por_instancia <- eventos_padronizados %>%
    count(instance, Operador, name = "qtd_aplicacoes")
  
  total_geral_todos_operadores <- sum(contagem_por_instancia$qtd_aplicacoes)

  # ==============================================================================
  # Construção da Tabela 12 (formato largo, ordenação por 'order', formatação
  # dos nomes das instâncias conforme o artigo e cálculo dos totais)
  # ==============================================================================
  top8_operadores <- c("EXT", "SW", "RNG", "DOT", "STAR", "CC", "NEG", "BR")
  
  tabela_12_instancias <- contagem_por_instancia %>%
    filter(Operador %in% top8_operadores) %>%
    pivot_wider(
      names_from  = Operador,
      values_from = qtd_aplicacoes,
      values_fill = 0
    ) %>%
    right_join(dados_instancias, by = "instance") %>%
    arrange(order) %>%
    # Converte os identificadores técnicos dos arquivos para os nomes formatados
    # publicados na coluna 'Instance' da Tabela 12 do artigo
    mutate(
      Instance = case_when(
        instance == "warmup"       ~ "Plain strings",
        instance == "anchors"      ~ "Anchors",
        instance == "ranges"       ~ "Ranges",
        instance == "backrefs"     ~ "Backrefs",
        instance == "abba"         ~ "Abba",
        instance == "aman-aplan"   ~ "A man, a plan",
        instance == "prime"        ~ "Prime",
        instance == "four"         ~ "Four",
        instance == "order"        ~ "Order",
        instance == "triples"      ~ "Triples",
        instance == "glob"         ~ "Glob",
        instance == "balance"      ~ "Balance",
        instance == "powers"       ~ "Powers",
        instance == "long-count"   ~ "Long count",
        instance == "alphabetical" ~ "Alphabetical",
        TRUE                       ~ instance
      )
    ) %>%
    select(Instance, all_of(top8_operadores))
  
  linha_total <- tabela_12_instancias %>%
    summarise(
      Instance = "Total",
      across(all_of(top8_operadores), sum)
    )
  
  linha_percentual <- linha_total %>%
    mutate(
      Instance = "Total (%)",
      across(all_of(top8_operadores), ~ round((.x / total_geral_todos_operadores) * 100, 2))
    )
  
  tabela_12_final <- bind_rows(
    tabela_12_instancias,
    linha_total,
    linha_percentual
  )

  # ==============================================================================
  # Exibição dos resultados no console e exportação para CSV
  # ==============================================================================
  cat("======================================================================\n")
  cat("TABELA 12 - APLICAÇÕES EFETIVAS DOS OPERADORES DE VIZINHANÇA (50 RODADAS)\n")
  cat("======================================================================\n")
  print(as.data.frame(tabela_12_final), row.names = FALSE)

  cat("\n----------------------------------------------------------------------\n")
  cat("COMPLEMENTO: Os 4 operadores menos frequentes omitidos da Tabela 12:\n")
  cat("----------------------------------------------------------------------\n")
  complemento_operadores <- contagem_por_instancia %>%
    filter(!Operador %in% top8_operadores) %>%
    group_by(Operador) %>%
    summarise(
      Total = sum(qtd_aplicacoes),
      `Total (%)` = round((Total / total_geral_todos_operadores) * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Total))

  print(as.data.frame(complemento_operadores), row.names = FALSE)
  
  arquivo_saida <- file.path(diretorio_base, "resultado_item4_tabela12.csv")
  write_csv2(tabela_12_final, arquivo_saida)
  cat("\nArquivo salvo em:", arquivo_saida, "\n")
