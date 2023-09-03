#carregando pacote
library(yaml)
library(jsonlite)
library(purrr)
library(tidyverse)
library(ggplot2)

#lendo o arquivo yaml
leitura_arquivos = read_yaml("configuracoes.yaml")

#lista para armazenar os registros
registros_totais = list()

#loop pelos nomes e caminhos dos arquivos
for (arquivo_json in leitura_arquivos$arquivos) {
  #caminho completo ate o arquivo json
  caminho_arquivo = file.path("dados", arquivo_json)
  
  #lendo o arquivo e armazenando
  registro = read_json(caminho_arquivo)
  registros_totais = c(registros_totais, list(registro))
}

#combinando as listas de registros
registros_completos = do.call(c, registros_totais)

#criando data frame
dados = registros_completos %>% 
  map(as.data.frame) %>%
  list_rbind() %>%
  as.tibble() %>%
  mutate(horario = as_datetime(horario),
         evento = as.factor(evento),
         codigo = as.factor(codigo))


quantidade = dados %>% 
  select(horario, evento) %>%
  filter(evento == "recalibragem") %>%
  mutate(contagem = 1:n())

quantidade %>%
  mutate(anterior = lag(horario)) %>%
  mutate(diferenca = horario - anterior) %>%
  filter(diferenca <= 28800)

# Criar um gráfico de dispersão dos horários de recalibragem
ggplot(dados, aes(x = horario, y = intensidade)) +
  geom_point() +
  labs(x = "Horário", y = "Intensidade") 


# Função para ajustar um modelo de regressão linear com base em um registro
ajustar_modelo <- function(registro) {
  # Verificar se todas as variáveis estão presentes no registro
  if (all(c("v1", "v2", "v3", "v4") %in% names(dados))) {
    modelo <- lm(intensidade ~ v1 + v2 + v3 + v4, data = dados)
    return(modelo)
  } else {
    return(NULL)  # Retorna NULL se alguma variável estiver ausente
  }
}

# Lista para armazenar os modelos ajustados
modelos_ajustados <- lapply(dados, ajustar_modelo)

# Remover modelos NULL da lista
modelos_ajustados <- modelos_ajustados[!sapply(modelos_ajustados, is.null)]
modelos_ajustados
