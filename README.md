# Atlas Constitucional

O **Atlas Constitucional** é um aplicativo interativo que permite
explorar e comparar características das constituições de todo o mundo,
com base nos dados do **Comparative Constitutions Project (CCP v5)**,
compilados por Elkins, Ginsburg e Melton (Universidade do Texas).

O app foi desenvolvido para uso didático na disciplina de
**Direito Constitucional** da graduação em Direito da
**UFERSA**, mas está disponível publicamente para estudantes e
pesquisadores de qualquer instituição.

## Acesso ao aplicativo

> 🔗 [Acesse o Atlas Constitucional aqui](https://n8wo69-ulisses0reis.shinyapps.io/atlas-constitucional/)

## O que você pode explorar

O app é organizado em quatro módulos:

### 🗺️ Atlas Mundial
Mapa coroplético interativo que exibe, país a país, como as
constituições do mundo respondem a uma variável selecionada.
Cobre o período de **1789 a 2023**.

Exemplos de perguntas que você pode explorar:
- Quais países adotam estrutura federal, unitária ou confederativa?
- Em que países a constituição prevê uma Corte Constitucional?
- Como se distribuiu o sufrágio universal ao longo do século XX?
- Quais constituições vigentes têm mais de 100 anos?

### ⚖️ Comparador de Constituições
Selecione um conjunto de países, uma variável e um ano para
comparar como cada constituição se posiciona naquela dimensão.
Útil para análises lado a lado. Por exemplo, comparar os países
do BRICS ou da América Latina em relação à proteção de direitos
socioeconômicos em 2023.

### 📈 Tendências Históricas
Gráfico de linhas que mostra como cada categoria de uma variável
evoluiu ao longo do tempo, em termos do número de países.
Permite visualizar, por exemplo, a difusão histórica da liberdade
de imprensa ou do bicameralismo entre as constituições do mundo.

### 🇧🇷 Brasil no Mundo
Módulo dedicado ao posicionamento comparado da **CF/1988**,
cobrindo o período de **1988 a 2023**. Exibe onde o Brasil se
situa em relação ao conjunto de países com dados disponíveis para
cada variável e apresenta um perfil constitucional completo com
contexto mundial para cada dimensão analisada.

## Variáveis disponíveis

O app cobre atualmente 29 variáveis organizadas em cinco blocos temáticos:

| Bloco | Exemplos de variáveis |
|---|---|
| Sistema de governo | Estrutura federal, executivo independente, casas legislativas |
| Poder Judiciário | Corte constitucional, cortes militares, trabalhistas, religiosas |
| Direitos Fundamentais | Liberdade de expressão, de imprensa, de reunião, direitos socioeconômicos |
| Eleições e Democracia | Seleção do chefe de Estado e de governo, sufrágio universal, idade mínima para votar |
| Durabilidade Constitucional | Cláusulas pétreas, maioria qualificada para emendas, idade da constituição |

## Fonte dos dados

Os dados utilizados são do **Comparative Constitutions Project (CCP), versão 5** (Elkins, Ginsburg e Melton, Universidade do Texas).

- Site oficial: https://comparativeconstitutionsproject.org/data/
- O arquivo `data/ccpcnc_v5.csv` **não está versionado** neste
  repositório. Faça o download diretamente na fonte e coloque-o
  na pasta `data/` antes de rodar o app localmente.

## Reprodução local

**Requisitos:** R (≥ 4.2) e os seguintes pacotes:
```r
install.packages(c(
  "shiny", "bslib", "leaflet", "plotly", "dplyr",
  "ggplot2", "readr", "countrycode", "rnaturalearth",
  "rnaturalearthdata", "fontawesome", "glue"
))
```

**Para rodar:**
```r
shiny::runApp()
```

Execute a partir do diretório raiz do projeto, com o arquivo
`ccpcnc_v5.csv` já na pasta `data/`.

## Estrutura do projeto
```
atlas_constitucional_2026/
├── global.R          # Pacotes, dados e constantes globais
├── ui.R              # Interface principal
├── server.R          # Servidor principal
├── R/
│   ├── mod_atlas.R       # Módulo 1: Atlas Mundial
│   ├── mod_comparador.R  # Módulo 2: Comparador de Constituições
│   ├── mod_tendencias.R  # Módulo 3: Tendências Históricas
│   └── mod_brasil.R      # Módulo 4: Brasil no Mundo
├── data/
│   └── ccpcnc_v5.csv     # ⚠️ Não versionado — baixar na fonte
└── www/              # Ativos estáticos (CSS, imagens)
```

## Como citar

Se você utilizar o Atlas Constitucional em pesquisas, trabalhos
acadêmicos ou materiais didáticos, por favor cite:

**O aplicativo:**
> REIS, Ulisses Levy Silvério dos. *Atlas Constitucional*. Aplicativo
> Shiny interativo. UFERSA, 2026. Disponível em:
> https://n8wo69-ulisses0reis.shinyapps.io/atlas-constitucional/. Acesso em: [data].

**Os dados (CCP v5):**
> Elkins, Zachary and Tom Ginsburg. 2025 *Characteristics of National Constitutions*, Version 5.0.” Comparative Constitutions Project. Last modified: November 10, 2025. Available at https://comparativeconstitutionsproject.org.

## Agradecimentos

Os dados que alimentam este aplicativo foram produzidos pelo
**Comparative Constitutions Project**, cujo esforço sistemático
de codificação das constituições do mundo torna possível pesquisas
como esta. Agradecemos aos seus autores pela disponibilização
pública dos dados.

## Licença

Este projeto está licenciado sob a [MIT License](LICENSE).
Você pode usar, modificar e redistribuir o código livremente,
desde que mantenha a atribuição ao autor original.

## Autor

**Ulisses Levy Silvério dos Reis**

Professor do Curso de Bacharelado e do Programa de Pós-Graduação
em Direito da UFERSA

[ORCID](https://orcid.org/0000-0003-1476-416X) |
[Lattes](https://lattes.cnpq.br/5041818002534490) |
[ResearchGate](https://www.researchgate.net/profile/Ulisses-Reis-2) |
[GitHub](https://github.com/ulisseslsreis) |
[LinkedIn](https://www.linkedin.com/in/ulisses-reis-797919247/) |
[Bluesky](https://bsky.app/profile/ulissesreis.bsky.social) |
[Instagram](https://www.instagram.com/ulisseslsreis/)