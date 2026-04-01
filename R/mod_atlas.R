# =============================================================
# Módulo 1: Atlas Mundial
# UI e servidor do mapa coroplético interativo
# =============================================================

# --- UI do módulo --------------------------------------------
atlas_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # --- Sidebar com controles -------------------------------
    sidebar = sidebar(
      width = 280,

      selectInput(
        inputId  = ns("variavel"),
        label    = "Variável constitucional",
        choices  = choices_variaveis
      ),

      sliderInput(
        inputId = ns("ano"),
        label   = "Ano",
        min     = 1789,
        max     = 2023,
        value   = 2023,
        step    = 1,
        sep     = "",
        ticks   = FALSE
      ),

      hr(),

      downloadButton(
        outputId = ns("dl_png"),
        label    = "Baixar PNG",
        class    = "btn-outline-secondary",
        style    = "width: 100%; margin-top: 4px;"
      )
    ),

    # --- Painel principal com o mapa -------------------------
    leafletOutput(
      outputId = ns("mapa"),
      height   = "75vh"
    )
  )
}

# --- Servidor do módulo --------------------------------------
atlas_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Metadados da variável selecionada
    meta <- reactive({
      atlas_vars[[input$variavel]]
    })

    # Dados filtrados pelo ano e variável selecionados
    dados_ano <- reactive({
      ccp |>
        filter(year == input$ano) |>
        select(cowcode, valor = all_of(input$variavel))
    })

    # Geometria unida aos dados do CCP
    mapa_data <- reactive({
      world |>
        left_join(dados_ano(), by = "cowcode")
    })

    # Paleta: contínua para constage, categórica para demais
    pal <- reactive({
      if (input$variavel == "constage") {
        colorNumeric(
          palette  = c("#d6e8f7", "#1a3a5c"),
          domain   = c(0, 150),
          na.color = "#cccccc"
        )
      } else {
        colorFactor(
          palette  = meta()$cores,
          levels   = meta()$categorias,
          na.color = "#cccccc"
        )
      }
    })

    # --- Mapa base: criado uma única vez ---------------------
    # Sem dados, sem polígonos — apenas o container Leaflet.
    # O zoom inicial cobre o mundo inteiro.
    output$mapa <- renderLeaflet({
      leaflet() |>
        setView(lng = 0, lat = 20, zoom = 2)
    })

    # --- Atualização reativa via leafletProxy ----------------
    # Roda sempre que mapa_data() ou pal() mudam,
    # mas sem recriar o mapa nem redefinir o zoom.
    observe({
      req(mapa_data())

      dados  <- mapa_data()
      paleta <- pal()

      valores_mapa <- if (input$variavel == "constage") {
        pmin(dados$valor, 150)
      } else {
        dados$valor
      }

      leafletProxy("mapa", session) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          data        = dados,
          fillColor   = paleta(valores_mapa),
          fillOpacity = 0.85,
          color       = "#000000",
          weight      = 0.7,
          label       = ~paste0(
            name, ": ",
            ifelse(is.na(dados$valor), "Sem dado", dados$valor)
          )
        ) |>
        addLegend(
          position  = "bottomright",
          pal       = paleta,
          values    = if (input$variavel == "constage") c(0, 150) else dados$valor,
          title     = meta()$titulo,
          opacity   = 0.85,
          labFormat = if (input$variavel == "constage") {
            labelFormat(suffix = " anos")
          } else {
            labelFormat()
          }
        )
    })

    # --- Download PNG (sem alteração) ------------------------
    output$dl_png <- downloadHandler(
      filename = function() {
        paste0("atlas_", input$variavel, "_", input$ano, ".png")
      },
      content = function(file) {
        meta  <- atlas_vars[[input$variavel]]
        dados <- world |>
          left_join(
            ccp |>
              filter(year == input$ano) |>
              select(cowcode, valor = all_of(input$variavel)),
            by = "cowcode"
          )

        if (input$variavel == "constage") {

          dados <- dados |>
            dplyr::mutate(valor_plot = pmin(valor, 200))

          p <- ggplot2::ggplot(dados) +
            ggplot2::geom_sf(
              ggplot2::aes(fill = valor_plot),
              color     = "#000000",
              linewidth = 0.15
            ) +
            ggplot2::scale_fill_gradient(
              low      = "#d6e8f7",
              high     = "#1a3a5c",
              limits   = c(0, 200),
              na.value = "#cccccc",
              name     = meta$titulo,
              breaks   = c(0, 50, 100, 150, 200),
              labels   = c("0 anos", "50 anos", "100 anos", "150 anos", "200 anos")
            )

        } else {

          dados <- dados |>
            dplyr::mutate(
              valor = factor(valor, levels = meta$categorias)
            )

          p <- ggplot2::ggplot(dados) +
            ggplot2::geom_sf(
              ggplot2::aes(fill = valor),
              color     = "#000000",
              linewidth = 0.15
            ) +
            ggplot2::scale_fill_manual(
              values   = setNames(meta$cores, meta$categorias),
              na.value = "#cccccc",
              name     = meta$titulo,
              drop     = FALSE
            )
        }

        p <- p +
          ggplot2::theme_void(base_size = 11) +
          ggplot2::theme(
            legend.position = if (input$variavel == "constage") "right" else "bottom",
            legend.title    = ggplot2::element_text(face = "bold", color = "black"),
            plot.title      = ggplot2::element_text(
              face   = "bold",
              size   = 13,
              hjust  = 0.5,
              color  = "black",
              margin = ggplot2::margin(b = 6)
            ),
            plot.subtitle   = ggplot2::element_text(
              hjust  = 0.5,
              color  = "black",
              margin = ggplot2::margin(b = 10)
            ),
            plot.caption    = ggplot2::element_text(
              size  = 8,
              color = "black",
              hjust = 1
            ),
            plot.margin     = ggplot2::margin(10, 10, 10, 10)
          ) +
          ggplot2::labs(
            title    = meta$titulo,
            subtitle = paste("Ano:", input$ano),
            caption  = "Reprodução de Atlas Constitucional. Dados: CCP v5."
          )

        ggplot2::ggsave(file, plot = p, width = 12, height = 7, dpi = 150)
      }
    )

  })
}