# =============================================================
# Módulo 4: Brasil no Mundo
# Posicionamento comparado da CF/1988 em perspectiva mundial
# =============================================================

# --- Blocos temáticos para o perfil constitucional -----------
# Definidos fora do servidor: são constantes, não dependem de inputs.
blocos_perfil <- list(
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
    "Idade da constituição vigente (anos)"    = "constage"
  )
)

# --- UI do módulo --------------------------------------------
brasil_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # --- Sidebar com controles -------------------------------
    sidebar = sidebar(
      width = 280,

      sliderInput(
        inputId = ns("ano"),
        label   = "Ano",
        min     = 1988,
        max     = 2023,
        value   = 2023,
        step    = 1,
        sep     = "",
        ticks   = FALSE
      ),

      hr(),

      # Controle da Proposta A — variável para o gráfico
      selectInput(
        inputId  = ns("variavel"),
        label    = "Variável (painel de posicionamento)",
        choices  = choices_variaveis_sem_constage
      )
    ),

    # --- Painel principal ------------------------------------
    div(

      # --- Proposta A: painel de posicionamento -------------
      card(
        card_header(
          strong("Posicionamento do Brasil"),
          span(
            style = "float: right; color: #7f8c8d; font-size: 0.9em;",
            textOutput(ns("subtitulo_a"), inline = TRUE)
          )
        ),
        card_body(
          plotlyOutput(ns("grafico_posicionamento"), height = "320px"),
          div(
            style = "margin-top: 12px; font-size: 0.95em; color: #1a3a5c;",
            textOutput(ns("frase_automatica"))
          )
        ),
        card_footer(
          downloadButton(
            outputId = ns("dl_grafico_png"),
            label    = "Baixar PNG",
            class    = "btn-sm btn-outline-secondary"
          )
        )
      ),

      # --- Proposta B: perfil constitucional ----------------
      card(
        style = "margin-top: 20px;",
        card_header(
          strong("Perfil constitucional completo"),
          span(
            style = "float: right; color: #7f8c8d; font-size: 0.9em;",
            textOutput(ns("subtitulo_b"), inline = TRUE)
          )
        ),
        card_body(
          uiOutput(ns("tabela_perfil"))
        ),
        card_footer(
          downloadButton(
            outputId = ns("dl_tabela_csv"),
            label    = "Baixar CSV",
            class    = "btn-sm btn-outline-primary"
          )
        )
      )
    )
  )
}

# --- Servidor do módulo --------------------------------------
brasil_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Legendas de ano nos cabeçalhos
    output$subtitulo_a <- renderText({
      paste("Ano:", input$ano)
    })

    output$subtitulo_b <- renderText({
      paste("CF/1988 —", input$ano)
    })

    # --- Dado do Brasil para o ano selecionado ---------------
    # cowcode do Brasil no COW: 140
    dado_brasil <- reactive({
      ccp |>
        filter(cowcode == 140, year == input$ano) |>
        select(valor = all_of(input$variavel)) |>
        pull(valor)
    })

    # --- Distribuição mundial para o ano e variável ----------
    distribuicao_mundial <- reactive({
      meta <- atlas_vars[[input$variavel]]

      ccp |>
        filter(year == input$ano, !is.na(.data[[input$variavel]])) |>
        group_by(categoria = .data[[input$variavel]]) |>
        summarise(n = n(), .groups = "drop") |>
        mutate(
          pct      = round(n / sum(n) * 100, 1),
          e_brasil = categoria == dado_brasil(),
          cor      = if_else(e_brasil, "#f5a623", "#2e6da4"),
          categoria = factor(categoria, levels = meta$categorias)
        ) |>
        # Ordena da maior para a menor contagem
        arrange(desc(n))
    })

    # --- Proposta A: gráfico de posicionamento ---------------
    output$grafico_posicionamento <- renderPlotly({

      dist         <- distribuicao_mundial()
      valor_brasil <- dado_brasil()

      if (length(valor_brasil) == 0 || is.na(valor_brasil)) {
        return(
          plotly_empty() |>
            layout(
              title = list(
                text    = "Sem dado disponível para o Brasil neste ano.",
                font    = list(color = "#7f8c8d"),
                x       = 0.5,
                xanchor = "center"
              ),
              paper_bgcolor = "white",
              plot_bgcolor  = "white"
            )
        )
      }

      # Para barras horizontais com ordem decrescente de cima para baixo,
      # o eixo Y do plotly precisa dos níveis em ordem inversa da tabela
      # (plotly empilha de baixo para cima por padrão)
      ordem_y <- rev(as.character(dist$categoria))

      plot_ly(
        data          = dist,
        y             = ~categoria,
        x             = ~n,
        type          = "bar",
        orientation   = "h",
        marker        = list(color = ~cor),
        text          = ~paste0(n, " países (", pct, "%)"),
        textposition  = "outside",
        hovertemplate = ~paste0(
          categoria, "<br>",
          n, " países (", pct, "%)",
          "<extra></extra>"
        )
      ) |>
        layout(
          xaxis = list(
            title = "Nº de países",
            range = list(0, max(dist$n, na.rm = TRUE) * 1.25)
          ),
          yaxis = list(
            title         = "",
            categoryorder = "array",
            categoryarray = ordem_y
          ),
          showlegend    = FALSE,
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          margin        = list(l = 10, r = 80, t = 10, b = 40)
        )
    })

    # --- Frase automática ------------------------------------
    output$frase_automatica <- renderText({

      valor_brasil <- dado_brasil()

      if (length(valor_brasil) == 0 || is.na(valor_brasil)) {
        return("Sem dado disponível para o Brasil neste ano.")
      }

      dist         <- distribuicao_mundial()
      linha_brasil <- dist |> filter(e_brasil)

      if (nrow(linha_brasil) == 0) {
        return("Não foi possível localizar o Brasil na distribuição mundial.")
      }

      meta  <- atlas_vars[[input$variavel]]
      n_br  <- linha_brasil$n
      pct   <- format(linha_brasil$pct, decimal.mark = ",")
      cat   <- as.character(linha_brasil$categoria)

      glue::glue(
        "Em {input$ano}, o Brasil adota a categoria \"{cat}\" ",
        "para a variável \"{meta$titulo}\", ",
        "posição compartilhada por {n_br} países ({pct}% dos países com dados disponíveis)."
      )
    })

    # --- Dados do perfil (reativos, reutilizados em UI e CSV) -
    dados_perfil <- reactive({

      ano_sel    <- input$ano
      brasil_ano <- ccp |> filter(cowcode == 140, year == ano_sel)

      if (nrow(brasil_ano) == 0) return(NULL)

      linhas <- lapply(names(blocos_perfil), function(nome_bloco) {
        vars_bloco <- blocos_perfil[[nome_bloco]]

        lapply(seq_along(vars_bloco), function(i) {
          var_nome <- names(vars_bloco)[i]
          var_col  <- vars_bloco[[i]]
          valor_br <- brasil_ano[[var_col]]
          if (length(valor_br) == 0) valor_br <- NA

          if (var_col == "constage") {

            todos_vals <- ccp |>
              filter(year == ano_sel, !is.na(constage)) |>
              pull(constage)

            percentil <- if (!is.na(valor_br) && length(todos_vals) > 0) {
              round(mean(todos_vals <= valor_br) * 100, 0)
            } else {
              NA
            }

            valor_exibir    <- if (!is.na(valor_br)) paste0(valor_br, " anos") else "—"
            contexto_exibir <- if (!is.na(percentil)) {
              paste0("Percentil ", percentil, " no mundo")
            } else {
              "—"
            }

          } else {

            valor_exibir <- if (!is.na(valor_br)) as.character(valor_br) else "—"

            if (!is.na(valor_br)) {
              n_mesma_cat <- ccp |>
                filter(
                  year == ano_sel,
                  !is.na(.data[[var_col]]),
                  .data[[var_col]] == valor_br
                ) |>
                nrow()

              n_com_dado <- ccp |>
                filter(year == ano_sel, !is.na(.data[[var_col]])) |>
                nrow()

              pct_val <- round(n_mesma_cat / n_com_dado * 100, 1)
              # Vírgula como separador decimal
              contexto_exibir <- paste0(
                format(pct_val, decimal.mark = ","), "% dos países"
              )
            } else {
              contexto_exibir <- "—"
            }
          }

          list(
            bloco    = nome_bloco,
            variavel = var_nome,
            cf1988   = valor_exibir,
            contexto = contexto_exibir
          )
        })
      })

      # Achata a lista de listas em data.frame
      do.call(rbind, lapply(unlist(linhas, recursive = FALSE), as.data.frame))
    })

    # --- Proposta B: tabela HTML -----------------------------
    output$tabela_perfil <- renderUI({

      df <- dados_perfil()

      if (is.null(df)) {
        return(p(
          style = "color: #7f8c8d;",
          "Sem dados disponíveis para o Brasil neste ano."
        ))
      }

      secoes <- lapply(names(blocos_perfil), function(nome_bloco) {
        df_bloco <- df[df$bloco == nome_bloco, ]

        linhas_html <- lapply(seq_len(nrow(df_bloco)), function(j) {
          tags$tr(
            tags$td(
              style = "font-size: 0.88em; color: #444; width: 35%;",
              df_bloco$variavel[j]
            ),
            tags$td(
              style = "font-weight: 600; color: #1a3a5c; width: 35%;",
              df_bloco$cf1988[j]
            ),
            tags$td(
              style = "font-size: 0.85em; color: #7f8c8d; width: 30%;",
              df_bloco$contexto[j]
            )
          )
        })

        tagList(
          tags$tr(
            tags$td(
              colspan = 3,
              style   = "
                background-color: #1a3a5c;
                color: #ffffff;
                font-weight: 700;
                font-size: 0.82em;
                text-transform: uppercase;
                letter-spacing: 0.06em;
                padding: 6px 10px;
              ",
              nome_bloco
            )
          ),
          linhas_html
        )
      })

      div(
        style = "overflow-x: auto;",
        tags$table(
          class = "table table-sm table-hover",
          style = "margin-bottom: 0;",
          tags$thead(
            tags$tr(
              tags$th(style = "width: 35%;", "Variável"),
              tags$th(style = "width: 35%;", "CF/1988"),
              tags$th(style = "width: 30%;", "Contexto mundial")
            )
          ),
          tags$tbody(secoes)
        )
      )
    })

    # --- Download PNG do gráfico -----------------------------
    output$dl_grafico_png <- downloadHandler(
      filename = function() {
        paste0("brasil_posicionamento_", input$variavel, "_", input$ano, ".png")
      },
      content = function(file) {
        dist         <- distribuicao_mundial()
        valor_brasil <- dado_brasil()
        meta         <- atlas_vars[[input$variavel]]

        # Ordena para o ggplot: maior para menor, de cima para baixo
        dist <- dist |>
          arrange(n) |>
          mutate(categoria = factor(categoria, levels = as.character(categoria)))

        cores_barras <- if_else(
          as.character(dist$categoria) == as.character(valor_brasil),
          "#f5a623",
          "#2e6da4"
        )

        p <- ggplot2::ggplot(
          dist,
          ggplot2::aes(x = categoria, y = n)
        ) +
          ggplot2::geom_col(fill = cores_barras) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(n, " (", pct, "%)")),
            hjust = -0.1,
            size  = 3.2
          ) +
          ggplot2::coord_flip() +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.2))
          ) +
          ggplot2::labs(
            title    = meta$titulo,
            subtitle = paste("Ano:", input$ano),
            x        = NULL,
            y        = "Nº de países",
            caption  = "Reprodução de Atlas Constitucional. Dados: CCP v5."
          ) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            legend.position    = "none",
            panel.grid.major.y = ggplot2::element_blank()
          )

        ggplot2::ggsave(file, plot = p, width = 8, height = 5, dpi = 150)
      }
    )

    # --- Download CSV da tabela ------------------------------
    output$dl_tabela_csv <- downloadHandler(
      filename = function() {
        paste0("brasil_perfil_", input$ano, ".csv")
      },
      content = function(file) {
        df <- dados_perfil()
        if (!is.null(df)) {
          # Renomeia colunas para o arquivo exportado
          names(df) <- c("Bloco", "Variável", "CF/1988", "Contexto mundial")
          readr::write_csv(df, file)
        }
      }
    )

  })
}