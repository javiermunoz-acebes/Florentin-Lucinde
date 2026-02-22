# =============================================================================
# ANÁLISIS POR CAPÍTULO — Evolución de conceptos clave a lo largo de la obra
#
# Argumento del artículo (§4): Florentin traza una progresión espacial
# Wald (cap. I) → Garten/Artificio (caps. XVI-XVII) que materializa
# la tesis del «invernadero social».
#
# También permite ver la concentración de Müßiggang en el cap. VI de Lucinde
# («Idylle über den Müßiggang») frente a su casi total ausencia en Florentin.
# =============================================================================

library(tidyverse)
library(scales)

# ── Se asume que libros y texto_obras ya están en memoria
# ── Si ejecutas este script de forma independiente, añade la sección
# ── de carga de datos del script figuras_articulo.R

# ── 1. PREPARACIÓN: texto por capítulo con número de orden ───────────────────

capitulos <- libros |>
  filter(obra != "Otro") |>
  mutate(
    # Número de capítulo como entero para ordenar correctamente
    num_cap = as.integer(str_extract(doc_id, "[0-9]+")),
    # Etiqueta corta para el eje
    cap_label = paste0(
      ifelse(obra == "Florentin (Dorothea)", "F", "L"),
      str_pad(num_cap, 2, pad = "0")
    ),
    texto_lower = str_to_lower(text),
    # Total de palabras por capítulo (para normalizar)
    total_palabras = str_count(texto_lower, "[[:alpha:]]+")
  ) |>
  filter(total_palabras > 50)  # excluimos capítulos excesivamente cortos


# ── 2. CONCEPTOS A RASTREAR ───────────────────────────────────────────────────
# Seleccionados por su relevancia directa en los argumentos del artículo

conceptos_cap <- tribble(
  ~concepto,              ~patron,                                          ~seccion,
  "M\u00fc\u00dfiggang",  "m\u00fc\u00dfiggang|m\u00fc\u00dfig|mu\u00dfe",  "\u00a75 Ocio",
  "T\u00e4tigkeit",       "t\u00e4tigkeit|besch\u00e4ftigung|arbeit",       "\u00a75 Ocio",
  "Wald / Natur",         "wald|wild|wildnis|urwald|pflanze|knospe",        "\u00a74 Natur",
  "Garten / Artificio",   "garten|k\u00fcnstlich|werk|maschine|topf",       "\u00a74 Natur",
  "Bildung / Harmonie",   "bildung|harmonie|einheit|vollendung",            "\u00a73 Masculinidad",
  "Fremd / Zufall",       "fremd|zufall|irren|wander|flucht",               "\u00a73 Masculinidad"
)


# ── 3. CONTEO POR CAPÍTULO ────────────────────────────────────────────────────

frecuencias_cap <- capitulos |>
  crossing(conceptos_cap) |>
  mutate(
    frecuencia  = map2_dbl(texto_lower, patron, ~ str_count(.x, .y)),
    freq_rel    = round(frecuencia / total_palabras * 1000, 2)
  ) |>
  select(obra, cap_label, num_cap, concepto, seccion, frecuencia, freq_rel)


# ── 4. GRÁFICO A: PROGRESIÓN DE Natur vs. Garten EN FLORENTIN ─────────────────
# Este gráfico es el soporte visual del argumento Wald→Garten de §4

datos_natur_fl <- frecuencias_cap |>
  filter(
    obra    == "Florentin (Dorothea)",
    seccion == "\u00a74 Natur"
  )

g_natur_cap <- ggplot(
  datos_natur_fl,
  aes(x = num_cap, y = freq_rel, color = concepto, group = concepto)
) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 2.5) +
  # Anotamos los capítulos clave mencionados en el artículo
  annotate("rect", xmin = 0.5, xmax = 1.5,
           ymin = -Inf, ymax = Inf,
           fill = "#27AE60", alpha = 0.08) +
  annotate("text", x = 1, y = Inf, vjust = 1.5, size = 2.8,
           color = "#27AE60", label = "Cap. I\nWald", fontface = "italic") +
  annotate("rect", xmin = 15.5, xmax = 18.5,
           ymin = -Inf, ymax = Inf,
           fill = "#E67E22", alpha = 0.08) +
  annotate("text", x = 17, y = Inf, vjust = 1.5, size = 2.8,
           color = "#E67E22", label = "Caps. XVI-XVIII\nGarten", fontface = "italic") +
  scale_color_manual(values = c(
    "Wald / Natur"       = "#27AE60",
    "Garten / Artificio" = "#E67E22"
  )) +
  scale_x_continuous(breaks = 1:18) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title    = "Del bosque al jard\u00edn: evoluci\u00f3n narrativa en Florentin",
    subtitle = "Frecuencia por cap\u00edtulo (\u2030) de Natur silvestre vs. Natur cultivada",
    x        = "Cap\u00edtulo",
    y        = "Frecuencia (\u2030)",
    color    = "Campo sem\u00e1ntico",
    caption  = "Fig. D. Progres\u00f3n narrativa de la met\u00e1fora vegetal en Florentin (\u00a74)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.caption    = element_text(size = 8, color = "grey40",
                                   hjust = 0, margin = margin(t = 8))
  )

print(g_natur_cap)
ggsave("Fig_D_Progresion_Natur_Florentin.png", plot = g_natur_cap,
       width = 11, height = 6, dpi = 300)
message("\u2713 Fig_D_Progresion_Natur_Florentin.png guardada")


# ── 5. GRÁFICO B: MÜSSIGGANG capítulo a capítulo en ambas obras ───────────────
# Hace visible la concentración en el cap. VI de Lucinde («Idylle»)
# y la casi total ausencia en Florentin — el argumento central de §5

datos_musig_cap <- frecuencias_cap |>
  filter(concepto == "M\u00fc\u00dfiggang")

g_musig_cap <- ggplot(
  datos_musig_cap,
  aes(x = num_cap, y = freq_rel, fill = obra)
) +
  geom_col(width = 0.7) +
  facet_wrap(~obra, ncol = 1, scales = "free_x") +
  # Marcamos el cap. VI de Lucinde explícitamente
  geom_text(
    data = filter(datos_musig_cap,
                  obra == "Lucinde (Friedrich)", freq_rel > 0),
    aes(label = cap_label),
    vjust = -0.4, size = 2.8, color = "#2980B9"
  ) +
  geom_text(
    data = filter(datos_musig_cap,
                  obra == "Florentin (Dorothea)", freq_rel > 0),
    aes(label = cap_label),
    vjust = -0.4, size = 2.8, color = "#E74C3C"
  ) +
  scale_fill_manual(values = c(
    "Florentin (Dorothea)" = "#E74C3C",
    "Lucinde (Friedrich)"  = "#2980B9"
  )) +
  scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "M\u00fc\u00dfiggang / Mu\u00dfe por cap\u00edtulo",
    subtitle = "Lucinde: concentrado en el cap. VI (\u00abIdylle\u00bb) \u00b7 Florentin: ausencia casi total",
    x        = "Cap\u00edtulo",
    y        = "Frecuencia (\u2030)",
    caption  = "Fig. E. Distribuci\u00f3n cap. a cap. de m\u00fc\u00dfig/Mu\u00dfe (\u00a75)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, color = "grey40",
                                    hjust = 0, margin = margin(t = 8))
  )

print(g_musig_cap)
ggsave("Fig_E_Muessiggang_por_Capitulo.png", plot = g_musig_cap,
       width = 11, height = 7, dpi = 300)
message("\u2713 Fig_E_Muessiggang_por_Capitulo.png guardada")


# ── 6. GRÁFICO C: TELEOLOGÍA VS. ERRANCIA capítulo a capítulo ─────────────────
# Confirma que el contraste no es solo global sino estructural:
# en Lucinde la Teleología domina de forma sostenida;
# en Florentin la Errancia surge y persiste.

datos_masc_cap <- frecuencias_cap |>
  filter(seccion == "\u00a73 Masculinidad")

g_masc_cap <- ggplot(
  datos_masc_cap,
  aes(x = num_cap, y = freq_rel,
      color = concepto, linetype = concepto, group = concepto)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~obra, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c(
    "Bildung / Harmonie" = "#2980B9",
    "Fremd / Zufall"     = "#E74C3C"
  )) +
  scale_linetype_manual(values = c(
    "Bildung / Harmonie" = "solid",
    "Fremd / Zufall"     = "dashed"
  )) +
  scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title    = "Teleolog\u00eda vs. Errancia: evoluci\u00f3n por cap\u00edtulo",
    subtitle = "Frecuencia (\u2030) de los dos campos de la masculinidad rom\u00e1ntica",
    x        = "Cap\u00edtulo",
    y        = "Frecuencia (\u2030)",
    color    = "Campo sem\u00e1ntico",
    linetype = "Campo sem\u00e1ntico",
    caption  = "Fig. F. Evoluci\u00f3n cap. a cap. de los conceptos de masculinidad (\u00a73)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, color = "grey40",
                                    hjust = 0, margin = margin(t = 8))
  )

print(g_masc_cap)
ggsave("Fig_F_Masculinidad_por_Capitulo.png", plot = g_masc_cap,
       width = 11, height = 8, dpi = 300)
message("\u2713 Fig_F_Masculinidad_por_Capitulo.png guardada")


# ── RESUMEN ───────────────────────────────────────────────────────────────────
message("\n\u2714 analisis_por_capitulo.R completado:")
message("  \u00b7 Fig_D_Progresion_Natur_Florentin.png")
message("  \u00b7 Fig_E_Muessiggang_por_Capitulo.png")
message("  \u00b7 Fig_F_Masculinidad_por_Capitulo.png")

