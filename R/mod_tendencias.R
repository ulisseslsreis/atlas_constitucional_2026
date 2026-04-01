# =============================================================
# Módulo 3: Tendências Históricas
# UI e servidor do gráfico de linhas por variável ao longo do tempo
# =============================================================

# --- UI do módulo --------------------------------------------
tendencias_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # --- Sidebar com controles -------------------------------
    sidebar = sidebar(
      width = 280,

      selectInput(
        inputId = ns("variavel"),
        label   = "Variável constitucional",
        choices = choices_variaveis_sem_constage
      ),

      numericInput(
        inputId = ns("ano_ini"),
        label   = "Ano inicial (1789–2023)",
        value   = 1789,
        min     = 1789,
        max     = 2023,
        step    = 1
      ),

      numericInput(
        inputId = ns("ano_fim"),
        label   = "Ano final (1789–2023)",
        value   = 2023,
        min     = 1789,
        max     = 2023,
        step    = 1
      ),

      checkboxInput(
        inputId = ns("suavizar"),
        label   = "Suavizar linhas",
        value   = FALSE
      ),

      actionButton(
        inputId = ns("gerar"),
        label   = "Gerar gráfico",
        class   = "btn-primary",
        width   = "100%"
      ),

      br(),

      downloadButton(
        outputId = ns("dl_png"),
        label    = "Baixar PNG",
        class    = "btn-outline-secondary",
        style    = "width: 100%;"
      )
    ),

    # --- Painel principal ------------------------------------
    div(
      plotlyOutput(
        outputId = ns("grafico"),
        height   = "65vh"
      ),

      # Nota metodológica
      div(
        style = "
          margin-top: 12px;
          font-size: 0.8em;
          color: #7f8c8d;
          padding: 0 4px;
        ",
        tags$em(
          "Atenção: a cobertura do CCP v5 antes de 1900 é muito limitada.
           O número reduzido de países observados nos anos mais recuados
           pode distorcer as tendências exibidas.
           Interprete séries anteriores a 1900 com cautela."
        )
      )
    )
  )
}

# --- Servidor do módulo --------------------------------------
tendencias_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Resultado reativo — gerado ao clicar em "Gerar gráfico"
    resultado <- eventReactive(input$gerar, {

      # Validações
      if (is.na(input$ano_ini) || is.na(input$ano_fim)) {
        showNotification(
          "Digite anos válidos entre 1789 e 2023.",
          type = "warning", duration = 4
        )
        return(NULL)
      }

      if (input$ano_ini >= input$ano_fim) {
        showNotification(
          "O ano inicial deve ser menor que o ano final.",
          type = "warning", duration = 4
        )
        return(NULL)
      }

      meta <- atlas_vars[[input$variavel]]

      # Conta N de países por ano e categoria — todos os países
      dados <- ccp |>
        filter(
          year >= input$ano_ini,
          year <= input$ano_fim,
          !is.na(.data[[input$variavel]])
        ) |>
        group_by(year, categoria = .data[[input$variavel]]) |>
        summarise(n = n(), .groups = "drop")

      list(
        dados    = dados,
        titulo   = meta$titulo,
        cores    = setNames(meta$cores, meta$categorias),
        variavel = input$variavel,
        ano_ini  = input$ano_ini,
        ano_fim  = input$ano_fim,
        suavizar = input$suavizar
      )
    })

    # --- Gráfico de linhas -----------------------------------
    output$grafico <- renderPlotly({
      req(resultado())
      res        <- resultado()
      categorias <- unique(res$dados$categoria)

      p <- plot_ly()

      for (cat in categorias) {
        dados_cat <- res$dados |> filter(categoria == cat)
        cor        <- res$cores[[cat]]
        if (is.null(cor) || is.na(cor)) cor <- "#cccccc"

        # Linha bruta
        p <- add_trace(
          p,
          data          = dados_cat,
          x             = ~year,
          y             = ~n,
          type          = "scatter",
          mode          = "lines",
          name          = cat,
          line          = list(
            color = cor,
            width = 1.5,
            dash  = if (res$suavizar) "dot" else "solid"
          ),
          hovertemplate = paste0(
            cat, "<br>Ano: %{x}<br>Países: %{y}<extra></extra>"
          ),
          showlegend = TRUE
        )

        # Linha suavizada (opcional) — mínimo 10 pontos para loess
        if (res$suavizar && nrow(dados_cat) >= 10) {
          suave             <- stats::loess(n ~ year, data = dados_cat, span = 0.3)
          dados_cat$n_suave <- pmax(predict(suave), 0)

          p <- add_trace(
            p,
            data          = dados_cat,
            x             = ~year,
            y             = ~n_suave,
            type          = "scatter",
            mode          = "lines",
            name          = cat,
            line          = list(color = cor, width = 2.5),
            hovertemplate = paste0(
              cat, " (suavizado)<br>Ano: %{x}<br>Países: %{y:.1f}<extra></extra>"
            ),
            showlegend = FALSE
          )
        }
      }

      p |>
        layout(
          xaxis = list(
            title = "Ano",
            range = list(res$ano_ini, res$ano_fim)
          ),
          yaxis = list(
            title     = "Nº de países",
            rangemode = "tozero"
          ),
          legend = list(
            title       = list(text = res$titulo),
            orientation = "v"
          ),
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          margin        = list(t = 20)
        )
    })

    # --- Download PNG ----------------------------------------
    output$dl_png <- downloadHandler(
      filename = function() {
        paste0("tendencias_", input$ano_ini, "_", input$ano_fim, ".png")
      },
      content = function(file) {
        req(resultado())
        res   <- resultado()
        dados <- res$dados

        p <- ggplot2::ggplot(
          dados,
          ggplot2::aes(
            x     = year,
            y     = n,
            color = categoria,
            group = categoria
          )
        ) +
          ggplot2::geom_line(
            linewidth = 0.9,
            alpha     = if (res$suavizar) 0.3 else 1
          ) +
          { if (res$suavizar)
              ggplot2::geom_smooth(
                method    = "loess",
                formula   = y ~ x,
                se        = FALSE,
                span      = 0.3,
                linewidth = 1.2
              )
          } +
          ggplot2::scale_color_manual(
            values   = res$cores,
            na.value = "#cccccc"
          ) +
          ggplot2::labs(
            title    = res$titulo,
            subtitle = paste("Período:", res$ano_ini, "–", res$ano_fim),
            x        = "Ano",
            y        = "Nº de países",
            color    = NULL,
            caption  = paste(
              "Reprodução de Atlas Constitucional. Dados: CCP, v5."
            )
          ) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            legend.position  = "right",
            panel.grid.minor = ggplot2::element_blank()
          )

        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 150)
      }
    )

  })
}