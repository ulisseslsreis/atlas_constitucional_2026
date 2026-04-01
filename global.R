# =============================================================
# Executado uma vez na inicialização do app.
# Carrega pacotes, lê dados e define constantes globais.
# =============================================================

# --- Pacotes -------------------------------------------------
library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(dplyr)
library(countrycode)
library(ggplot2)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fontawesome)
library(glue)

# --- Dados ---------------------------------------------------
ccp_raw <- read_csv(
  "data/ccpcnc_v5.csv",
  show_col_types = FALSE
)

ccp <- ccp_raw |>
  select(
    # Identificação e cronologia
    cowcode, country, year, systyear, evnttype, region,

    # Sistema de governo
    execindp, hogname, hoshog, housenum,
    fedunit, federal_1, federal_2, federal_3,
    judcrts_2, judcrts_4, judcrts_6, judcrts_7, judcrts_8,

    # Direitos fundamentais
    express, press, assem, freerel,
    socecon, matequal, indcit,

    # Eleições e democracia
    hoselect, hogelect, votemin, voteun,

    # Durabilidade constitucional
    amndamaj, unamend
  ) |>
  mutate(
    # Idade da constituição vigente no momento de cada observação
    constage = pmax(year - systyear, 0, na.rm = TRUE),

    # Estrutura federal (fedunit)
    fedunit_label = case_when(
      fedunit == 1  ~ "Federação",
      fedunit == 2  ~ "Confederação",
      fedunit == 3  ~ "Unitário",
      fedunit == 96 ~ "Outro",
      fedunit == 97 ~ "Indeterminado",
      fedunit == 98 ~ "Não especificado",
      TRUE          ~ NA_character_
    ),

    # Independência do Executivo (execindp)
    execindp_label = case_when(
      execindp == 1  ~ "Sim",
      execindp == 2  ~ "Não",
      execindp == 96 ~ "Outro",
      execindp == 97 ~ "Indeterminado",
      TRUE           ~ NA_character_
    ),

    # Número de casas legislativas (housenum)
    housenum_label = case_when(
      housenum == 1  ~ "Nenhuma casa legislativa",
      housenum == 2  ~ "Uma casa legislativa",
      housenum == 3  ~ "Duas casas legislativas",
      housenum == 96 ~ "Outra composição",
      housenum == 97 ~ "Impossível determinar",
      housenum == 98 ~ "Não especificado",
      housenum == 99 ~ "Não aplicável",
      TRUE           ~ NA_character_
    ),

    # Reconhecimento de governos locais/municipais (federal_1)
    federal_1_label = case_when(
      federal_1 == 1 ~ "Sim",
      federal_1 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Reconhecimento de governos estaduais/regionais/provinciais (federal_2)
    federal_2_label = case_when(
      federal_2 == 1 ~ "Sim",
      federal_2 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Reconhecimento de autonomia para grupos indígenas (federal_3)
    federal_3_label = case_when(
      federal_3 == 1 ~ "Sim",
      federal_3 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Corte Constitucional (judcrts_2)
    judcrts_2_label = case_when(
      judcrts_2 == 1 ~ "Sim",
      judcrts_2 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Cortes militares (judcrts_4)
    judcrts_4_label = case_when(
      judcrts_4 == 1 ~ "Sim",
      judcrts_4 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Cortes tributárias/fiscais (judcrts_6)
    judcrts_6_label = case_when(
      judcrts_6 == 1 ~ "Sim",
      judcrts_6 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Cortes trabalhistas (judcrts_7)
    judcrts_7_label = case_when(
      judcrts_7 == 1 ~ "Sim",
      judcrts_7 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Cortes religiosas (judcrts_8)
    judcrts_8_label = case_when(
      judcrts_8 == 1 ~ "Sim",
      judcrts_8 == 0 ~ "Não",
      TRUE           ~ NA_character_
    ),

    # Liberdade de expressão (express)
    express_label = case_when(
      express == 1  ~ "Sim",
      express == 2  ~ "Não",
      express == 96 ~ "Outro",
      express == 97 ~ "Indeterminado",
      TRUE          ~ NA_character_
    ),

    # Liberdade de imprensa (press)
    press_label = case_when(
      press == 1  ~ "Sim",
      press == 2  ~ "Não",
      press == 90 ~ "Remetido à legislação ordinária",
      press == 96 ~ "Outro",
      press == 97 ~ "Indeterminado",
      TRUE        ~ NA_character_
    ),

    # Liberdade de reunião (assem)
    assem_label = case_when(
      assem == 1  ~ "Sim",
      assem == 2  ~ "Não",
      assem == 96 ~ "Outro",
      assem == 97 ~ "Indeterminado",
      TRUE        ~ NA_character_
    ),

    # Liberdade religiosa (freerel)
    freerel_label = case_when(
      freerel == 1  ~ "Sim",
      freerel == 2  ~ "Não",
      freerel == 96 ~ "Outro",
      freerel == 97 ~ "Indeterminado",
      TRUE          ~ NA_character_
    ),

    # Direitos socioeconômicos (socecon)
    socecon_label = case_when(
      socecon == 1  ~ "Sim",
      socecon == 2  ~ "Não",
      socecon == 96 ~ "Outro",
      socecon == 97 ~ "Indeterminado",
      TRUE          ~ NA_character_
    ),

    # Igualdade matrimonial (matequal)
    matequal_label = case_when(
      matequal == 1  ~ "Sim",
      matequal == 2  ~ "Não",
      matequal == 96 ~ "Outro",
      matequal == 97 ~ "Indeterminado",
      TRUE           ~ NA_character_
    ),

    # Cidadania plena para grupos indígenas (indcit)
    indcit_label = case_when(
      indcit == 1  ~ "Sim",
      indcit == 96 ~ "Outro",
      indcit == 97 ~ "Indeterminado",
      indcit == 98 ~ "Não especificado",
      indcit == 99 ~ "Não aplicável",
      TRUE         ~ NA_character_
    ),

    # Seleção do Chefe de Estado (hoselect)
    hoselect_label = case_when(
      hoselect == 1  ~ "Hereditariedade/Seleção real",
      hoselect == 2  ~ "Eleito pelos cidadãos",
      hoselect == 3  ~ "Eleito por grupo seleto",
      hoselect == 90 ~ "Remetido à legislação ordinária",
      hoselect == 96 ~ "Outro",
      hoselect == 97 ~ "Indeterminado",
      hoselect == 98 ~ "Não especificado",
      hoselect == 99 ~ "Não aplicável",
      TRUE           ~ NA_character_
    ),

    # Seleção do Chefe de Governo (hogelect)
    hogelect_label = case_when(
      hogelect == 1  ~ "Hereditariedade/Seleção real",
      hogelect == 2  ~ "Eleito pelos cidadãos",
      hogelect == 3  ~ "Eleito por grupo seleto",
      hogelect == 4  ~ "Nomeado",
      hogelect == 90 ~ "Remetido à legislação ordinária",
      hogelect == 96 ~ "Outro",
      hogelect == 97 ~ "Indeterminado",
      hogelect == 98 ~ "Não especificado",
      hogelect == 99 ~ "Não aplicável",
      TRUE           ~ NA_character_
    ),

    # Idade mínima para votar (votemin)
    votemin_label = case_when(
      votemin == 1  ~ "18 anos",
      votemin == 2  ~ "20 anos",
      votemin == 3  ~ "21 anos",
      votemin == 4  ~ "Maioridade civil",
      votemin == 90 ~ "Remetido à legislação ordinária",
      votemin == 96 ~ "Outro",
      votemin == 97 ~ "Indeterminado",
      votemin == 98 ~ "Não especificado",
      votemin == 99 ~ "Não aplicável",
      TRUE          ~ NA_character_
    ),

    # Sufrágio universal (voteun)
    voteun_label = case_when(
      voteun == 1  ~ "Sim",
      voteun == 2  ~ "Não",
      voteun == 96 ~ "Outro",
      voteun == 97 ~ "Indeterminado",
      TRUE         ~ NA_character_
    ),

    # Cláusulas pétreas (unamend)
    unamend_label = case_when(
      unamend == 1  ~ "Sim",
      unamend == 2  ~ "Não",
      unamend == 97 ~ "Indeterminado",
      unamend == 99 ~ "Não aplicável",
      TRUE          ~ NA_character_
    ),

    # Maioria qualificada para emendas (amndamaj)
    amndamaj_label = case_when(
      amndamaj == 1  ~ "Sim",
      amndamaj == 2  ~ "Não",
      amndamaj == 96 ~ "Outro",
      amndamaj == 97 ~ "Indeterminado",
      amndamaj == 99 ~ "Não aplicável",
      TRUE           ~ NA_character_
    )
  )

# --- Metadados das variáveis do Atlas ------------------------
atlas_vars <- list(

  fedunit_label = list(
    titulo     = "Estrutura federal",
    categorias = c(
      "Federação", "Unitário", "Confederação",
      "Outro", "Indeterminado", "Não especificado"
    ),
    cores = c(
      "#2e6da4",  # Federação — azul
      "#e07b2a",  # Unitário — laranja
      "#8e44ad",  # Confederação — roxo
      "#f5a623",  # Outro — âmbar
      "#7f8c8d",  # Indeterminado — cinza médio
      "#16a085"   # Não especificado — ciano
    )
  ),

  execindp_label = list(
    titulo     = "A Constituição prevê um Executivo independente?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  housenum_label = list(
    titulo     = "Quantas Casas o Poder Legislativo contém?",
    categorias = c(
      "Uma casa legislativa", "Duas casas legislativas",
      "Nenhuma casa legislativa", "Outra composição",
      "Impossível determinar", "Não especificado", "Não aplicável"
    ),
    cores = c(
      "#2e6da4",  # Uma casa — azul
      "#1a3a5c",  # Duas casas — azul escuro
      "#e07b2a",  # Nenhuma casa — laranja
      "#f5a623",  # Outra composição — âmbar
      "#8e44ad",  # Impossível determinar — roxo
      "#16a085",  # Não especificado — ciano
      "#7f8c8d"   # Não aplicável — cinza médio
    )
  ),

  federal_1_label = list(
    titulo     = "A Constituição reconhece governos locais/municipais?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  federal_2_label = list(
    titulo     = "A Constituição reconhece governos estaduais/regionais/provinciais?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  federal_3_label = list(
    titulo     = "A Constituição reconhece autonomia para grupos indígenas?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  judcrts_2_label = list(
    titulo     = "A Constituição prevê uma Corte Constitucional?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  judcrts_4_label = list(
    titulo     = "A Constituição prevê Cortes militares?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  judcrts_6_label = list(
    titulo     = "A Constituição prevê Cortes tributárias/fiscais?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  judcrts_7_label = list(
    titulo     = "A Constituição prevê Cortes trabalhistas?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  judcrts_8_label = list(
    titulo     = "A Constituição prevê Cortes religiosas?",
    categorias = c("Sim", "Não"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a"   # Não — laranja
    )
  ),

  express_label = list(
    titulo     = "A Constituição garante liberdade de expressão?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  press_label = list(
    titulo     = "A Constituição garante liberdade de imprensa?",
    categorias = c(
      "Sim", "Não", "Remetido à legislação ordinária",
      "Outro", "Indeterminado"
    ),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#16a085",  # Remetido à legislação ordinária — ciano
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  assem_label = list(
    titulo     = "A Constituição garante liberdade de reunião?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  freerel_label = list(
    titulo     = "A Constituição garante liberdade religiosa?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  socecon_label = list(
    titulo     = "A Constituição prevê direitos socioeconômicos?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  matequal_label = list(
    titulo     = "A Constituição prevê igualdade matrimonial?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  indcit_label = list(
    titulo     = "A Constituição garante cidadania plena a grupos indígenas?",
    categorias = c(
      "Sim", "Outro", "Indeterminado",
      "Não especificado", "Não aplicável"
    ),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#f5a623",  # Outro — âmbar
      "#8e44ad",  # Indeterminado — roxo
      "#16a085",  # Não especificado — ciano
      "#7f8c8d"   # Não aplicável — cinza médio
    )
  ),

  hoselect_label = list(
    titulo     = "Como é selecionado o Chefe de Estado?",
    categorias = c(
      "Eleito pelos cidadãos", "Eleito por grupo seleto",
      "Hereditariedade/Seleção real", "Remetido à legislação ordinária",
      "Outro", "Indeterminado", "Não especificado", "Não aplicável"
    ),
    cores = c(
      "#2e6da4",  # Eleito pelos cidadãos — azul
      "#1a3a5c",  # Eleito por grupo seleto — azul escuro
      "#8e44ad",  # Hereditariedade/Seleção real — roxo
      "#16a085",  # Remetido à legislação ordinária — ciano
      "#f5a623",  # Outro — âmbar
      "#e07b2a",  # Indeterminado — laranja
      "#7f8c8d",  # Não especificado — cinza médio
      "#cccccc"   # Não aplicável — cinza claro
    )
  ),

  hogelect_label = list(
    titulo     = "Como é selecionado o Chefe de Governo?",
    categorias = c(
      "Eleito pelos cidadãos", "Eleito por grupo seleto",
      "Hereditariedade/Seleção real", "Nomeado",
      "Remetido à legislação ordinária",
      "Outro", "Indeterminado", "Não especificado", "Não aplicável"
    ),
    cores = c(
      "#2e6da4",  # Eleito pelos cidadãos — azul
      "#1a3a5c",  # Eleito por grupo seleto — azul escuro
      "#8e44ad",  # Hereditariedade/Seleção real — roxo
      "#f5a623",  # Nomeado — âmbar
      "#16a085",  # Remetido à legislação ordinária — ciano
      "#e07b2a",  # Outro — laranja
      "#7f8c8d",  # Indeterminado — cinza médio
      "#cccccc",  # Não especificado — cinza claro
      "#b0bec5"   # Não aplicável — cinza azulado
    )
  ),

  votemin_label = list(
    titulo     = "Qual é a idade mínima para votar?",
    categorias = c(
      "18 anos", "20 anos", "21 anos", "Maioridade civil",
      "Remetido à legislação ordinária", "Outro",
      "Indeterminado", "Não especificado", "Não aplicável"
    ),
    cores = c(
      "#2e6da4",  # 18 anos — azul
      "#1a3a5c",  # 20 anos — azul escuro
      "#8e44ad",  # 21 anos — roxo
      "#16a085",  # Maioridade civil — ciano
      "#f5a623",  # Remetido à legislação ordinária — âmbar
      "#e07b2a",  # Outro — laranja
      "#7f8c8d",  # Indeterminado — cinza médio
      "#cccccc",  # Não especificado — cinza claro
      "#b0bec5"   # Não aplicável — cinza azulado
    )
  ),

  voteun_label = list(
    titulo     = "A Constituição prevê sufrágio universal?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad"   # Indeterminado — roxo
    )
  ),

  unamend_label = list(
    titulo     = "A Constituição possui cláusulas pétreas?",
    categorias = c("Sim", "Não", "Indeterminado", "Não aplicável"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#8e44ad",  # Indeterminado — roxo
      "#7f8c8d"   # Não aplicável — cinza médio
    )
  ),

  amndamaj_label = list(
    titulo     = "Emendas constitucionais exigem mais que maioria simples?",
    categorias = c("Sim", "Não", "Outro", "Indeterminado", "Não aplicável"),
    cores = c(
      "#2e6da4",  # Sim — azul
      "#e07b2a",  # Não — laranja
      "#f5a623",  # Outro — âmbar
      "#8e44ad",  # Indeterminado — roxo
      "#7f8c8d"   # Não aplicável — cinza médio
    )
  ),

  # Variável numérica contínua — usa colorNumeric no servidor
  constage = list(
    titulo     = "Idade da constituição vigente (anos)",
    categorias = NULL,
    cores      = NULL
  )

)

# --- Choices compartilhadas dos selectInput de variáveis -----
# Usadas em Atlas, Comparador, Tendências e Brasil.
# Brasil exclui "constage" do seletor de posicionamento
# (ver choices_variaveis_sem_constage abaixo).
choices_variaveis <- list(
  "Sistema de governo" = c(
    "Estrutura federal"                       = "fedunit_label",
    "Executivo independente"                  = "execindp_label",
    "Casas do Poder Legislativo"              = "housenum_label",
    "Reconhece governos locais/municipais"    = "federal_1_label",
    "Reconhece governos estaduais/regionais"  = "federal_2_label",
    "Reconhece autonomia de grupos indígenas" = "federal_3_label"
  ),
  "Poder Judiciário" = c(
    "Corte Constitucional"                    = "judcrts_2_label",
    "Cortes militares"                        = "judcrts_4_label",
    "Cortes tributárias/fiscais"              = "judcrts_6_label",
    "Cortes trabalhistas"                     = "judcrts_7_label",
    "Cortes religiosas"                       = "judcrts_8_label"
  ),
  "Direitos Fundamentais" = c(
    "Liberdade de expressão"                  = "express_label",
    "Liberdade de imprensa"                   = "press_label",
    "Liberdade de reunião"                    = "assem_label",
    "Liberdade religiosa"                     = "freerel_label",
    "Direitos socioeconômicos"                = "socecon_label",
    "Igualdade matrimonial"                   = "matequal_label",
    "Cidadania plena a grupos indígenas"      = "indcit_label"
  ),
  "Eleições e Democracia" = c(
    "Seleção do Chefe de Estado"              = "hoselect_label",
    "Seleção do Chefe de Governo"             = "hogelect_label",
    "Idade mínima para votar"                 = "votemin_label",
    "Sufrágio universal"                      = "voteun_label"
  ),
  "Durabilidade Constitucional" = c(
    "Cláusulas pétreas"                       = "unamend_label",
    "Maioria qualificada para emendas"        = "amndamaj_label",
    "Idade da constituição vigente"           = "constage"
  )
)

# Variante sem constage — usada no seletor de posicionamento do módulo Brasil
choices_variaveis_sem_constage <- choices_variaveis
choices_variaveis_sem_constage[["Durabilidade Constitucional"]] <- c(
  "Cláusulas pétreas"                = "unamend_label",
  "Maioria qualificada para emendas" = "amndamaj_label"
)

# --- Paleta de cores -----------------------------------------
cores <- list(
  azul_escuro  = "#1a3a5c",
  azul_medio   = "#2e6da4",
  laranja      = "#e07b2a",
  ambar        = "#f5a623"
)

# --- Geometria dos países ------------------------------------
world <- ne_countries(
  scale       = "medium",
  returnclass = "sf"
) |>
  mutate(
    cowcode = countrycode(
      sourcevar   = iso_a3,
      origin      = "iso3c",
      destination = "cown"
    ),
    # Correções manuais: países com iso_a3 == "-99" no rnaturalearth
    cowcode = case_when(
      name == "France"  ~ 220,
      name == "Norway"  ~ 385,
      name == "Germany" ~ 260,
      TRUE              ~ cowcode
    )
  )

# --- Módulos -------------------------------------------------
source("R/mod_atlas.R")
source("R/mod_comparador.R")
source("R/mod_tendencias.R")
source("R/mod_brasil.R")

# Libera memória: ccp_raw não é mais necessário
rm(ccp_raw)
gc()