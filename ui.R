# =============================================================
# Interface do usuário — estrutura principal do app
# =============================================================

ui <- shiny::tagList(

  # --- CSS global ---------------------------------------------
  shiny::tags$style(shiny::HTML("
    .navbar-nav {
      width: 100%;
      justify-content: center;
    }
    .navbar-nav .nav-link.active {
      color: #f5a623 !important;
      border-bottom-color: #f5a623 !important;
    }
    .leaflet-container {
      background: #f8f8f8;
    }
    html, body {
      height: auto !important;
      overflow-y: auto !important;
    }
    .bslib-page-navbar {
      height: auto !important;
      overflow: visible !important;
    }

    .footer-links a {
      color: #555;
      text-decoration: none;
      font-size: 1.15rem;
      transition: color 0.2s ease, transform 0.2s ease;
      display: inline-flex;
      align-items: center;
      justify-content: center;
    }

    .footer-links a:hover {
      color: #2e6da4;
      transform: translateY(-1px);
    }

    .footer-links .fa-icon {
      vertical-align: middle;
    }
  ")),

  # --- Cabeçalho (acima da navbar) ----------------------------
  shiny::div(
    style = "
      background-color: #1a3a5c;
      padding: 20px 32px;
      width: 100%;
      text-align: center;
    ",
    shiny::div(
      style = "font-size: 1.8em; font-weight: 700; color: #ffffff; letter-spacing: 0.02em;",
      "Atlas Constitucional"
    ),
    shiny::div(
      style = "font-size: 1em; color: #f5a623; margin-top: 6px;",
      "Explore e conheça as Constituições do mundo inteiro"
    )
  ),

  # --- Navbar com as abas -------------------------------------
  bslib::page_navbar(

    theme = bslib::bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#2e6da4",
      secondary  = "#e07b2a",
      base_font  = bslib::font_google("Inter")
    ),

    title = NULL,

    # --- Módulo 1: Atlas Mundial ------------------------------
    bslib::nav_panel(
      title = "Atlas Mundial",
      atlas_ui("atlas")
    ),

    # --- Módulo 2: Comparador de Constituições ---------------
    bslib::nav_panel(
      title = "Comparador de Constituições",
      comparador_ui("comparador")
    ),

    # --- Módulo 3: Tendências Históricas ---------------------
    bslib::nav_panel(
      title = "Tendências Históricas",
      tendencias_ui("tendencias")
    ),

    # --- Módulo 4: Brasil no Mundo ---------------------------
    bslib::nav_panel(
      title = "Brasil no Mundo",
      brasil_ui("brasil")
    ),

    # --- Rodapé -----------------------------------------------
    footer = shiny::div(
      style = "
        text-align: center;
        padding: 16px 24px;
        font-size: 0.85em;
        color: #555;
        border-top: 1px solid #e0e0e0;
        background-color: #fafafa;
      ",

      shiny::div(
        style = "margin-bottom: 10px; line-height: 1.6;",
        "Desenvolvido por ",
        shiny::tags$strong("Ulisses Reis"),
        " · UFERSA · Código aberto · Dados: ",
        shiny::tags$a(
          "CCP v5",
          href   = "https://comparativeconstitutionsproject.org/data/",
          target = "_blank",
          style  = "color: #2e6da4; text-decoration: none;"
        )
      ),

      shiny::div(
        class = "footer-links",
        style = "display: flex; justify-content: center; gap: 18px; flex-wrap: wrap;",

        shiny::tags$a(
          href = "mailto:ulisseslreis@gmail.com",
          target = "_blank",
          title = "E-mail",
          fontawesome::fa("envelope")
        ),

        shiny::tags$a(
          href = "https://github.com/ulisseslsreis",
          target = "_blank",
          title = "GitHub",
          fontawesome::fa("github")
        ),

        shiny::tags$a(
          href = "https://www.linkedin.com/in/ulisses-reis-797919247/",
          target = "_blank",
          title = "LinkedIn",
          fontawesome::fa("linkedin")
        ),

        shiny::tags$a(
          href = "https://www.researchgate.net/profile/Ulisses-Reis-2",
          target = "_blank",
          title = "ResearchGate",
          fontawesome::fa("graduation-cap")
        ),

        shiny::tags$a(
          href = "http://lattes.cnpq.br/5041818002534490",
          target = "_blank",
          title = "Lattes",
          fontawesome::fa("file-lines")
        ),

        shiny::tags$a(
          href = "https://orcid.org/0000-0003-1476-416X",
          target = "_blank",
          title = "ORCID",
          fontawesome::fa("id-badge")
        ),

        shiny::tags$a(
          href = "https://bsky.app/profile/ulissesreis.bsky.social",
          target = "_blank",
          title = "Bluesky",
          fontawesome::fa("cloud")
        ),

        shiny::tags$a(
          href = "https://www.instagram.com/ulisseslsreis/",
          target = "_blank",
          title = "Instagram",
          fontawesome::fa("instagram")
        )
      )
    )
  )
)