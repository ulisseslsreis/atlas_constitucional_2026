# =============================================================
# Módulo 2: Comparador de Constituições
# UI e servidor da comparação entre países
# =============================================================

# --- UI do módulo --------------------------------------------
comparador_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # --- Painel de controles no topo -------------------------
    card(
      full_screen = FALSE,
      card_body(
        style = "padding: 24px 16px; min-height: 240px; display: flex; align-items: center;",
        layout_columns(
          col_widths = c(3, 3, 3, 2, 1),

          selectInput(
            inputId  = ns("paises"),
            label    = "Países (mínimo 2)",
            choices  = NULL,
            multiple = TRUE
          ),

          selectInput(
            inputId = ns("variavel"),
            label   = "Variável constitucional",
            choices = choices_variaveis
          ),

          numericInput(
            inputId = ns("ano"),
            label   = "Ano (digite entre 1789 e 2023)",
            value   = 2023,
            min     = 1789,
            max     = 2023,
            step    = 1
          ),

          actionButton(
            inputId = ns("adicionar"),
            label   = "Comparar",
            class   = "btn-primary",
            style   = "margin-top: 24px; width: 100%;"
          ),

          actionButton(
            inputId = ns("limpar"),
            label   = "Limpar",
            class   = "btn-outline-secondary",
            style   = "margin-top: 24px; width: 100%;"
          )
        )
      )
    ),

    # --- Área do painel de resultado -------------------------
    uiOutput(ns("painel"))
  )
}

# --- Servidor do módulo --------------------------------------
comparador_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Comparação atual (NULL = nenhuma)
    comparacao_atual <- reactiveVal(NULL)

    # Preenche selectInput de países
    observe({
      paises_disponiveis <- ccp |>
        distinct(country) |>
        arrange(country) |>
        pull(country)

      updateSelectInput(session, "paises", choices = paises_disponiveis)
    })

    # --- Botão Comparar --------------------------------------
    observeEvent(input$adicionar, {

      # Validação: ano
      if (is.na(input$ano) || input$ano < 1789 || input$ano > 2023) {
        showNotification(
          "Digite um ano válido entre 1789 e 2023.",
          type     = "warning",
          duration = 4
        )
        return()
      }

      # Validação: países
      n_paises <- length(input$paises)
      if (n_paises < 2) {
        showNotification(
          "Selecione ao menos 2 países.",
          type     = "warning",
          duration = 4
        )
        return()
      }

      # --- Bifurcação: contínua vs. categórica ---------------
      if (input$variavel == "constage") {

        # Variável contínua: uma linha por país
        dados_continuos <- ccp |>
          filter(
            year    == input$ano,
            country %in% input$paises
          ) |>
          select(country, systyear, valor = constage) |>
          arrange(desc(valor))

        comparacao_atual(list(
          tipo     = "continua",
          variavel = "constage",
          titulo   = "Idade da constituição vigente (anos)",
          ano      = input$ano,
          n_paises = nrow(dados_continuos),
          dados    = dados_continuos
        ))

      } else {

        # Variável categórica: agrupamento por categoria
        meta <- atlas_vars[[input$variavel]]

        dados_brutos <- ccp |>
          filter(
            year    == input$ano,
            country %in% input$paises
          ) |>
          select(country, valor = all_of(input$variavel))

        n_total <- nrow(dados_brutos)

        tabela_agregada <- dados_brutos |>
          group_by(valor) |>
          summarise(
            paises_lista = paste(sort(country), collapse = "; "),
            n            = n(),
            .groups      = "drop"
          ) |>
          mutate(
            pct = round(n / n_total * 100, 1)
          ) |>
          arrange(desc(n)) |>
          rename(categoria = valor)

        comparacao_atual(list(
          tipo            = "categorica",
          variavel        = input$variavel,
          titulo          = meta$titulo,
          ano             = input$ano,
          n_paises        = n_total,
          tabela_agregada = tabela_agregada,
          cores           = meta$cores,
          cats            = meta$categorias
        ))
      }
    })

    # --- Botão Limpar ----------------------------------------
    observeEvent(input$limpar, {
      comparacao_atual(NULL)
    })

    # --- Painel de resultado ---------------------------------
    output$painel <- renderUI({

      if (is.null(comparacao_atual())) {
        return(
          p(
            style = "color: #7f8c8d; margin-top: 32px; text-align: center;",
            "Selecione países, variável e ano, depois clique em Comparar."
          )
        )
      }

      comp <- comparacao_atual()

      # Cabeçalho comum
      cabecalho <- card_header(
        strong(comp$titulo),
        span(
          style = "float: right; color: #7f8c8d; font-size: 0.9em;",
          paste("Ano:", comp$ano, "\u2014", comp$n_paises, "países")
        )
      )

      # Rodapé comum
      rodape <- card_footer(
        downloadButton(
          outputId = session$ns("dl_csv"),
          label    = "Baixar CSV",
          class    = "btn-sm btn-outline-primary"
        ),
        downloadButton(
          outputId = session$ns("dl_png"),
          label    = "Baixar PNG",
          class    = "btn-sm btn-outline-secondary",
          style    = "margin-left: 8px;"
        )
      )

      if (comp$tipo == "continua") {

        # --- Painel contínuo: tabela por país ----------------
        card(
          style = "margin-top: 20px;",
          cabecalho,
          card_body(
            layout_columns(
              col_widths = c(5, 7),

              # Tabela com país, ano de adoção e idade
              div(
                style = "overflow-x: auto;",
                tags$table(
                  class = "table table-sm table-striped",
                  tags$thead(
                    tags$tr(
                      tags$th("País"),
                      tags$th("Ano de adoção"),
                      tags$th("Idade (anos)")
                    )
                  ),
                  tags$tbody(
                    lapply(seq_len(nrow(comp$dados)), function(j) {
                      linha <- comp$dados[j, ]
                      tags$tr(
                        tags$td(linha$country),
                        tags$td(linha$systyear),
                        tags$td(linha$valor)
                      )
                    })
                  )
                )
              ),

              plotlyOutput(
                outputId = session$ns("grafico"),
                height   = "280px"
              )
            )
          ),
          rodape
        )

      } else {

        # --- Painel categórico: tabela agregada --------------
        card(
          style = "margin-top: 20px;",
          cabecalho,
          card_body(
            layout_columns(
              col_widths = c(5, 7),

              div(
                style = "overflow-x: auto;",
                tags$table(
                  class = "table table-sm table-striped",
                  tags$thead(
                    tags$tr(
                      tags$th(comp$titulo),
                      tags$th("Países"),
                      tags$th("N"),
                      tags$th("%")
                    )
                  ),
                  tags$tbody(
                    lapply(seq_len(nrow(comp$tabela_agregada)), function(j) {
                      linha <- comp$tabela_agregada[j, ]
                      tags$tr(
                        tags$td(linha$categoria),
                        tags$td(
                          style = "font-size: 0.85em; color: #555;",
                          linha$paises_lista
                        ),
                        tags$td(linha$n),
                        tags$td(paste0(linha$pct, "%"))
                      )
                    })
                  )
                )
              ),

              plotlyOutput(
                outputId = session$ns("grafico"),
                height   = "280px"
              )
            )
          ),
          rodape
        )
      }
    })

    # --- Gráfico ---------------------------------------------
    output$grafico <- renderPlotly({
      req(comparacao_atual())
      comp <- comparacao_atual()

      if (comp$tipo == "continua") {

        # Barras por país, ordenadas da maior para a menor
        dados <- comp$dados |>
          mutate(country = factor(country, levels = rev(country)))

        plot_ly(
          data          = dados,
          x             = ~country,
          y             = ~valor,
          type          = "bar",
          marker        = list(color = "#2e6da4"),
          text          = ~paste0(valor, " anos"),
          textposition  = "outside",
          hovertemplate = ~paste0(
            country, "<br>",
            "Constituição de ", systyear, "<br>",
            valor, " anos",
            "<extra></extra>"
          )
        ) |>
          layout(
            xaxis = list(
              title         = "",
              categoryorder = "array",
              categoryarray = levels(dados$country)
            ),
            yaxis = list(
              title = "Anos",
              range = list(0, max(dados$valor, na.rm = TRUE) * 1.15)
            ),
            showlegend    = FALSE,
            plot_bgcolor  = "white",
            paper_bgcolor = "white"
          )

      } else {

        # Barras por categoria
        tab <- comp$tabela_agregada
        mapa_cores   <- setNames(comp$cores, comp$cats)
        cores_barras <- mapa_cores[tab$categoria]
        cores_barras[is.na(cores_barras)] <- "#cccccc"

        tab$categoria <- factor(tab$categoria, levels = tab$categoria)

        plot_ly(
          data          = tab,
          x             = ~categoria,
          y             = ~n,
          type          = "bar",
          marker        = list(color = cores_barras),
          text          = ~paste0(n, " (", pct, "%)"),
          textposition  = "outside",
          hovertemplate = ~paste0(
            categoria, "<br>",
            n, " países (", pct, "%)<br>",
            paises_lista,
            "<extra></extra>"
          )
        ) |>
          layout(
            xaxis = list(
              title         = "",
              categoryorder = "array",
              categoryarray = levels(tab$categoria)
            ),
            yaxis = list(
              title = "Nº de países",
              dtick = 1,
              range = list(0, max(tab$n) * 1.15)
            ),
            showlegend    = FALSE,
            plot_bgcolor  = "white",
            paper_bgcolor = "white",
            margin        = list(t = 40)
          )
      }
    })

    # --- Download CSV ----------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function() {
        comp <- comparacao_atual()
        paste0("comparacao_", comp$ano, ".csv")
      },
      content = function(file) {
        comp <- comparacao_atual()
        if (comp$tipo == "continua") {
          readr::write_csv(comp$dados, file)
        } else {
          readr::write_csv(comp$tabela_agregada, file)
        }
      }
    )

    # --- Download PNG ----------------------------------------
    output$dl_png <- downloadHandler(
      filename = function() {
        comp <- comparacao_atual()
        paste0("comparacao_", comp$ano, ".png")
      },
      content = function(file) {
        comp <- comparacao_atual()

        if (comp$tipo == "continua") {

          dados <- comp$dados |>
            dplyr::mutate(country = factor(country, levels = rev(country)))

          p <- ggplot2::ggplot(
            dados,
            ggplot2::aes(x = country, y = valor)
          ) +
            ggplot2::geom_col(fill = "#2e6da4") +
            ggplot2::geom_text(
              ggplot2::aes(label = paste0(valor, " anos")),
              vjust = -0.4,
              size  = 3.5
            ) +
            ggplot2::labs(
              title    = comp$titulo,
              subtitle = paste("Ano:", comp$ano),
              x        = NULL,
              y        = "Anos",
              caption  = "Reprodução de Atlas Constitucional. Dados: CCP, v5."
            ) +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(
              legend.position    = "none",
              panel.grid.major.x = ggplot2::element_blank()
            )

        } else {

          tab <- comp$tabela_agregada
          mapa_cores   <- setNames(comp$cores, comp$cats)
          cores_barras <- mapa_cores[tab$categoria]
          cores_barras[is.na(cores_barras)] <- "#cccccc"

          tab$categoria <- factor(tab$categoria, levels = tab$categoria)

          p <- ggplot2::ggplot(
            tab,
            ggplot2::aes(x = categoria, y = n, fill = categoria)
          ) +
            ggplot2::geom_col() +
            ggplot2::geom_text(
              ggplot2::aes(label = paste0(n, " (", pct, "%)")),
              vjust = -0.4,
              size  = 3.5
            ) +
            ggplot2::scale_fill_manual(
              values   = cores_barras,
              na.value = "#cccccc"
            ) +
            ggplot2::labs(
              title    = comp$titulo,
              subtitle = paste("Ano:", comp$ano, "\u2014", comp$n_paises, "países"),
              x        = NULL,
              y        = "Nº de países",
              caption  = "Reprodução de Atlas Constitucional. Dados: CCP, v5."
            ) +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(
              legend.position    = "none",
              panel.grid.major.x = ggplot2::element_blank()
            )
        }

        ggplot2::ggsave(file, plot = p, width = 8, height = 5, dpi = 150)
      }
    )

  })
}