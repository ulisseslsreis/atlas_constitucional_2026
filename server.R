# =============================================================
# Servidor principal do app
# =============================================================

server <- function(input, output, session) {

  # --- Módulo 1: Atlas Mundial -------------------------------
  atlas_server("atlas")

  # --- Módulo 2: Comparador de Constituições ----------------
  comparador_server("comparador")

  # --- Módulo 3: Tendências Históricas ----------------------
  tendencias_server("tendencias")

  # --- Módulo 4: Brasil no Mundo ----------------------
  brasil_server("brasil")

}