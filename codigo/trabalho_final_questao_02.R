# ==============================================================================
# Exercício de replicação de análise estatística
# 2) Gerar as análises que produzem os resultados da Figura 4 com base nos 
# arquivos do processo.
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
# LEITURA DAS 50 RODADAS
# a. Pega o maior valor de fitness para cada instância em cada rodada;
# ==============================================================================

# ==============================================================================
# local onde estão as 50 pastas das rodadas
# ==============================================================================
diretorio_base <- "/Users/tamds/Google Drive/My Drive/03-TIC/03.02-doutorado/03.02.02-métodos quantitativos de pesquisa/trabalho final/Regex Golf - Instancias"

# ==============================================================================
# Cria um vetor numérico de 50 elementos contendo a sequência de números 
# inteiros de 0 até 49 (incrementados de 1 em 1).
# Esse vetor representa os nomes dos 50 diretórios do experimento.
# ==============================================================================
rodadas <- 0:49

# ==============================================================================
# Criação de uma função para o processamento dos arquivos de uma rodada.
#
# "function()" É a palavra reservada utilizada para declarar uma nova função.
#
# "r" e "base_dir" São os dois parâmetros obrigatórios da função. Como não têm 
# um valor padrão, o R exigirá que ambos sejam informados na chamada da função.
#
# "r" Receberá o identificador numérico do diretório da rodada (de 0 a 49).
#
# "base_dir" Receberá o caminho do diretório raiz (diretorio_base).
# ==============================================================================
processar_rodada <- function(r, base_dir) {

  # ==============================================================================
  # Na variável "pasta_rodada" é armazenado o caminho completo até o diretório 
  # correspondente à rodada.
  #
  # "file.path()" É a função para unir componentes de diretórios de forma segura e 
  # independente do sistema operacional. Ela insere automaticamente o separador de 
  # pastas adequado ao ambiente — no caso do macOS, a barra / — e evita erros de 
  # concatenação manual, como barras duplicadas ("//").
  #
  # "as.character(r)" Converte o número da rodada recebido no parâmetro r para o 
  # tipo textual (string).
  # ==============================================================================
  pasta_rodada <- file.path(base_dir, as.character(r))

  # ==============================================================================
  # O comando "list.files()" é uma função que inspeciona um diretório e retorna um 
  # vetor de strings contendo os arquivos encontrados no diretório.
  # 
  # O argumento "pasta_rodada" indica o diretório que deve ser inspecionado para
  # encontrar os arquivos da rodada.
  # 
  # "full.names = TRUE" Faz a função retornar o caminho completo relativo junto ao 
  # nome do arquivo (ex.: "./0/abba.csv" em vez de apenas "abba.csv").
  # 
  # O operador pipe (%>%) pega o vetor de caminhos retornado por "list.files()" e 
  # o injeta como o primeiro argumento da função seguinte (keep).
  # 
  # A função "keep()" percorre cada elemento do vetor recebido e mantém apenas 
  # aqueles em que a condição testada resultar em TRUE. Pode-se entender como sendo
  # uma espécie de loop.
  # 
  # A função anônima " ~ " (notação de fórmula), é necessária porque a função keep() 
  # espera receber uma função como argumento, e não um resultado lógico já calculado.
  # No R, funções são tratadas como objetos que podem ser passados como parâmetros para outras funções.
  # 
  # A função "basename(.x)" remove todo o caminho das pastas e extrai apenas o nome 
  # do arquivo com sua extensão ("Abba.csv" ou "Abba.shrinker.csv").
  # 
  # ".x" representa o caminho completo do item atual enviado pela função "keep()", 
  # como "./0/Abba.csv" ou "./0/Abba.shrinker.csv".
  # 
  # "str_detect(..., "\\.shrinker\\.csv$")" é uma função que testa se o nome do arquivo 
  # termina com ".shrinker.csv" . A expressão regular \\. trata o ponto como caractere 
  # literal. Retorna TRUE se encontrar o padrão e FALSE caso contrário.
  # 
  # O ! inverte o resultado lógico anterior — transformando TRUE em FALSE e FALSE em TRUE.
  # ==============================================================================
  arquivos_processo <- list.files(pasta_rodada, full.names = TRUE) %>%
    keep(~ !str_detect(basename(.x), "\\.shrinker\\.csv"))
  
  # ==============================================================================
  # map_dfr(arquivos_processo, ...) é uma espécie de loop que percorre cada caminho 
  # de arquivo presente no vetor arquivos_processo.
  # 
  # O sufixo _dfr significa que à medida que a função lê cada arquivo .csv e devolve 
  # um tibble, o map_dfr() empilha automaticamente os tibbles um embaixo do outro, 
  # consolidando todas as instâncias da rodada em um único tibble. 
  # 
  # "function(arq) {" é a declaração de uma função anônima executada para cada 
  # arquivo percorrido por "map_dfr".
  # 
  # A cada iteração o map_dfr() pega um elemento (caminho do arquivo) do vetor 
  # arquivos_processo e o injeta automaticamente no parâmetro arq da função anônima.
  # ==============================================================================
  map_dfr(arquivos_processo, function(arq) {

    # ==============================================================================
    # O comando str_remove(basename(arq), "\\.csv") isola o nome do arquivo a partir 
    # do seu caminho completo e remove a extensão .csv, devolvendo apenas o nome limpo 
    # da instância (por exemplo, transformando "./0/Abba.csv" em "Abba").
    # 
    # A função basename(arq) descarta toda a estrutura de diretórios na variável arq 
    # (como "./0/") e extrai somente o nome do arquivo com a extensão ("Abba.csv").
    # 
    # Remover a extensão .csv transforma o nome físico do arquivo em um identificador 
    # limpo da instância, permitindo rastrear a origem de cada linha após a consolidação 
    # e cruzar esses dados com outras tabelas do experimento.
    # ==============================================================================
    nome_instancia <- str_remove(basename(arq), "\\.csv")
  
    # ==============================================================================
    # A função read_delim() lê arquivos de texto delimitados (como .csv, .tsv ou .txt) 
    # e carrega os dados diretamente para a memória no formato de um tibble.
    # 
    # O parâmetro delim = ";" instrui a função read_delim() a reconhecer o 
    # ponto e vírgula (;) como o caractere delimitador que separa cada coluna do arquivo.
    # ==============================================================================
    dados <- read_delim(arq,delim = ";")

    # ==============================================================================
    # É criado um tibble de 1 linha 3 3 colunas que resume o desempenho máximo alcançado 
    # em um único arquivo de instância dentro de uma rodada.
    # ==============================================================================
    tibble(
      Instancia      = nome_instancia,
      Rodada         = r,
      
      # ==============================================================================
      # O operador $ em dados$Fitness acessa e extrai toda a coluna Fitness do tibble 
      # dados, devolvendo-a como um vetor.
      # 
      # "as.numeric(...)" impõe explicitamente que os elementos do vetor sejam 
      # tratados como valores numéricos.
      # 
      # "max(...)" percorre o vetor numérico e retorna o maior valor encontrado.
      # 
      # O parâmetro "na.rm = TRUE" instrui descartar valores ausentes ou inválidos (NA). 
      # Sem esse argumento, a presença de um NA faria a função inteira devolver NA como resposta.
      # ==============================================================================
      Melhor_Fitness = max(as.numeric(dados$Fitness), na.rm = TRUE)
    )
  })
}

# ==============================================================================
# Executa-se o processamento das 50 rodads do experimento, consolida os resultados 
# em um único tibble e salva o resultado final na variável df_figura4.
# 
# Aqui, o map_dfr() percorre cada número do vetor rodadas (0 a 49) e executa a 
# função processar_rodada para cada rodada.
# 
# O argumento "base_dir = diretorio_base" é repassado como um parâmetro em todas 
# as chamadas internas de processar_rodada.
# 
# A passagem do parâmetro "r" da função "processar_rodada" acontece de forma implícita 
# e posicional pela própria mecânica do map_dfr(). Ele percorre cada elemento do 
# vetor "rodadas" e o injeta automaticamente como o primeiro argumento livre (r) 
# da função "processar_rodada" a cada iteração.
# 
# Estão sendo usadas 2 regras de passagem de parâmetros da linguagem R:
#  - Casamento pelo nome (base_dir = diretorio_base): Ao informar o nome do parâmetro 
#    explicitamente na chamada, o R vincula o valor de diretorio_base diretamente 
#    ao parâmetro base_dir de processar_rodada. Esse valor permanece fixo em todas 
#    as 50 execuções.
#  - Casamento por posição (r): Como base_dir já foi preenchido pelo nome, sobra 
#    apenas o parâmetro posicional r na assinatura function(r, base_dir). A cada 
#    passo sobre o vetor rodadas (0 a 49), o map_dfr() invoca processar_rodada enviando 
#    o elemento atual do vetor para ocupar essa primeira posição livre. 
# 
# Ao concluir as 50 iterações, o sufixo _dfr empilha linha por linha os tibbles 
# individuais de cada rodada em um único tibble consolidado.
# ==============================================================================
df_figura4 <- map_dfr(rodadas, processar_rodada, base_dir = diretorio_base)

# ------------------------------------------------------------------------------
# 2. CONSTRUÇÃO DO GRÁFICO DE BOXPLOTS - FIGURA 4
# b. Desenha os boxplots por instância.
# ------------------------------------------------------------------------------
grafico_fig4 <- ggplot(df_figura4, aes(x = Instancia, y = Melhor_Fitness)) +
  # Desenha os boxplots com visual limpo
  geom_boxplot(fill="white",color="black")+
  # Define as mesmas marcas do eixo Y da Figura 4 do artigo (-200 a 600)
  scale_y_continuous(
    breaks       = seq(-200, 600, by = 200),
    minor_breaks = seq(-200, 600, by = 100)
  ) +
  # Rótulos dos eixos (idênticos ao artigo)
  labs(
    x = NULL,
    y = "Fitness"
  ) +
  # Tema clássico com borda completa ao redor do painel e grade horizontal discreta
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major   = element_line(color = "grey90", linetype = "solid"),
    panel.grid.minor   = element_line(color = "grey90", linetype = "solid"),
    axis.text.x        = element_text(angle = 45, vjust = 1, hjust = 1, color = "black"),
    axis.text.y        = element_text(color = "black")
  )

# ==============================================================================
# Exibe o gráfico no RStudio
# ==============================================================================
print(grafico_fig4)
