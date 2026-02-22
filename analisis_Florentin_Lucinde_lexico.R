# =============================================================================
# ANÁLISIS LÉXICO COMPARATIVO: Florentin y Lucinde
# Figuras:
#   «El léxico romántico a prueba: Desafíos hermenéuticos en la traducción
#    de Florentin a la luz de Lucinde»
#
# Genera:
#   · Fig_1_Masculinidades.png  → Sección 3 del artículo
#   · Fig_2_Ocio_Desplazamientos.png → Sección 5 del artículo
#
# Repositorio del corpus:
#   https://github.com/javiermunoz-acebes/Florentin-Lucinde
#
# Requisitos: R >= 4.1
#   install.packages(c("tidyverse", "stopwords", "scales"))
#
# Reproducibilidad: el script descarga el corpus directamente desde GitHub.
#   No se requieren archivos locales.
# =============================================================================

# ── PAQUETES ──────────────────────────────────────────────────────────────────
paquetes <- c("tidyverse", "tidytext", "stopwords", "scales")
nuevos   <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(nuevos) > 0) install.packages(nuevos)

library(tidyverse)
library(tidytext)   # necesario para TF-IDF y dispersión
library(stopwords)
library(scales)


# ── CARGA DEL CORPUS (GitHub) ─────────────────────────────────────────────────

base_url <- "https://raw.githubusercontent.com/javiermunoz-acebes/Florentin-Lucinde/main/corpus/"

archivos <- c(
  paste0("Florentin_", 1:18, ".txt"),
  "Lucinde_01_Prolog.txt",
  "Lucinde_02_Bekenntnisse eines Ungeschickten.txt",
  "Lucinde_3_Dithyrambische Fantasie.txt",
  "Lucinde_4_Charakteristik der kleinen Wilhelmine .txt",
  "Lucinde_5_Allegorie von der Frechheit .txt",
  "Lucinde_6_Idylle über den Müßiggang .txt",
  "Lucinde_7_Treue und Scherz .txt",
  "Lucinde_8_Lehrjahre der Männlichkeit  .txt",
  "Lucinde_9_ Metamorphosen.txt",
  "Lucinde_10_Zwei Briefe .txt",
  "Lucinde_11_Zweiter Brief .txt",
  "Lucinde_12_Eine Reflexion .txt",
  "Lucinde_13_Julius an Antonio .txt",
  "Lucinde_14_Sehnsucht und Ruhe .txt",
  "Lucinde_15_Tändeleien der Fantasie .txt"
)

message("Descargando corpus desde GitHub...")

raw_data <- map_dfr(archivos, function(archivo) {
  url   <- paste0(base_url, URLencode(archivo, reserved = TRUE))
  texto <- tryCatch(
    paste(readLines(url, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
    error = function(e) { message("  ✗ Error: ", archivo); NA_character_ }
  )
  tibble(doc_id = archivo, text = texto)
}) |>
  filter(!is.na(text))

message("✓ Descargados: ", nrow(raw_data), " de ", length(archivos), " archivos\n")

# Etiquetamos por obra
libros <- raw_data |>
  mutate(obra = case_when(
    str_detect(doc_id, "Florentin") ~ "Florentin (Dorothea)",
    str_detect(doc_id, "Lucinde")   ~ "Lucinde (Friedrich)",
    TRUE ~ "Otro"
  ))

# Texto completo por obra en minúsculas
texto_obras <- libros |>
  group_by(obra) |>
  summarise(texto = str_to_lower(paste(text, collapse = "\n\n")), .groups = "drop")

# Total de palabras por obra (para normalizar)
total_palabras_obra <- texto_obras |>
  mutate(total_tokens = str_count(texto, "[[:alpha:]]+")) |>
  select(obra, total_tokens)

# Stopwords alemanas (necesarias para Fig. B y Fig. C)
stop_de <- tibble(word = stopwords::stopwords("de"))

# =============================================================================
# FIGURA 1 — MASCULINIDADES: TELEOLOGÍA VS. ERRANCIA  (Sección 3)
# =============================================================================
# Argumento: el vocabulario de Julius (Bildung, Harmonie) estructura Lucinde
# en torno a una masculinidad teleológica. Florentin invierte esta lógica:
# el léxico de la errancia y el azar (Fremd, Zufall, Irren) domina.

patrones_masc <- tribble(
  ~concepto,                             ~patron,
  "Teleología (Bildung / Harmonie)",     "bildung|harmonie|einheit|bestimmung|zweck|vollendung",
  "Errancia (Zufall / Fremdheit)",       "fremd|zufall|irren|verfehlen|wander|flucht|heimatlos"
)

datos_masc <- texto_obras |>
  crossing(patrones_masc) |>
  mutate(frecuencia = map2_dbl(texto, patron, ~ str_count(.x, .y))) |>
  left_join(total_palabras_obra, by = "obra") |>
  mutate(freq_rel = round(frecuencia / total_tokens * 1000, 2)) |>
  select(obra, concepto, frecuencia, freq_rel)

message("── Figura 1: Masculinidades ──")
print(as.data.frame(datos_masc))

fig1 <- ggplot(datos_masc, aes(x = freq_rel, y = concepto, fill = obra)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(
    aes(label = paste0(frecuencia, "  (", round(freq_rel, 1), "\u2030)")),
    position = position_dodge(0.6),
    hjust = -0.08, size = 3.3
  ) +
  scale_fill_manual(values = c(
    "Florentin (Dorothea)" = "#E74C3C",
    "Lucinde (Friedrich)"  = "#2980B9"
  )) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.40))) +
  labs(
    title    = "Estructura narrativa: De la Teleolog\u00eda a la Errancia",
    subtitle = "Frecuencia l\u00e9xica en la construcci\u00f3n del sujeto masculino",
    x        = "Frecuencia (\u2030)",
    y        = NULL,
    fill     = "Obra",
    caption  = paste0(
      "Figura 1. An\u00e1lisis computacional del corpus (Distant Reading).\n",
      "Corpus completo disponible en: ",
      "https://github.com/javiermunoz-acebes/Florentin-Lucinde"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    axis.text.y      = element_text(size = 11, face = "bold"),
    legend.position  = "bottom",
    plot.margin      = margin(t = 10, r = 30, b = 10, l = 10),
    plot.caption     = element_text(size = 8, color = "grey40",
                                    hjust = 0, margin = margin(t = 8))
  )

print(fig1)
ggsave("Fig_1_Masculinidades.png", plot = fig1,
       width = 10, height = 5, dpi = 300)
message("✓ Fig_1_Masculinidades.png guardada")


# =============================================================================
# FIGURA 2 — EL LÉXICO DEL OCIO: TRES DESPLAZAMIENTOS SEMÁNTICOS  (Sección 5)
# =============================================================================
# Tres fenómenos documentados:
#   1. Desaparición del ocio sagrado: Müßiggang/Muße casi ausente en Florentin
#   2. Explosión de la actividad vacía: Vergnügen y Tätigkeit se disparan
#   3. Predominio del artificio: Kunst/Werk supera a Pflanze en Florentin

figura2_datos <- tribble(
  ~concepto,                         ~patron,                                     ~fenomeno,
  # Panel 1
  "M\u00fc\u00dfiggang / Mu\u00dfe",
  "m\u00fc\u00dfiggang|m\u00fc\u00dfig|mu\u00dfe",
  "1. Desaparici\u00f3n del ocio sagrado",
  "Heilige Stille / Passivit\u00e4t",
  "heilige stille|passivit\u00e4t|passiv",
  "1. Desaparici\u00f3n del ocio sagrado",
  # Panel 2
  "Vergn\u00fcgen / Erg\u00f6tzlichkeit",
  "vergn\u00fcgen|erg\u00f6tz|belustigung",
  "2. Explosi\u00f3n de la actividad vac\u00eda",
  "T\u00e4tigkeit / Besch\u00e4ftigung",
  "t\u00e4tigkeit|besch\u00e4ftigung|arbeit",
  "2. Explosi\u00f3n de la actividad vac\u00eda",
  # Panel 3
  "Pflanze / organisches Wachstum",
  "pflanze|knospe|wachstum|keimen|entfalten",
  "3. Predominio del artificio",
  "Kunst / Werk / Artificio",
  "kunst|kunstwerk|werk|k\u00fcnstlich|gem\u00e4lde",
  "3. Predominio del artificio"
) |>
  mutate(fenomeno = factor(fenomeno, levels = c(
    "1. Desaparici\u00f3n del ocio sagrado",
    "2. Explosi\u00f3n de la actividad vac\u00eda",
    "3. Predominio del artificio"
  )))

figura2_resultados <- texto_obras |>
  crossing(figura2_datos) |>
  mutate(frecuencia = map2_dbl(texto, patron, ~ str_count(.x, .y))) |>
  left_join(total_palabras_obra, by = "obra") |>
  mutate(freq_relativa = round(frecuencia / total_tokens * 1000, 2)) |>
  select(obra, concepto, fenomeno, frecuencia, freq_relativa)

message("\n── Figura 2: Tres desplazamientos del ocio ──")
figura2_resultados |>
  select(obra, concepto, freq_relativa) |>
  pivot_wider(names_from = obra, values_from = freq_relativa) |>
  print()

fig2 <- ggplot(
  figura2_resultados,
  aes(x = reorder_within(concepto, freq_relativa, fenomeno),
      y = freq_relativa,
      fill = obra)
) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(
    aes(label = frecuencia),
    position = position_dodge(0.65),
    hjust = -0.2, size = 2.8
  ) +
  coord_flip() +
  facet_wrap(~fenomeno, scales = "free_y", ncol = 1) +
  tidytext::scale_x_reordered() +
  scale_fill_manual(values = c(
    "Florentin (Dorothea)" = "#E74C3C",
    "Lucinde (Friedrich)"  = "#2980B9"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30))) +
  labs(
    title    = "El l\u00e9xico del ocio: tres desplazamientos sem\u00e1nticos",
    subtitle = "Frecuencia relativa (\u2030) \u00b7 La etiqueta indica la frecuencia absoluta",
    x        = NULL,
    y        = "Frecuencia (\u2030)",
    fill     = "Obra",
    caption  = paste0(
      "Figura 2. An\u00e1lisis computacional del corpus (Distant Reading).\n",
      "Corpus completo y an\u00e1lisis detallados disponibles en: ",
      "https://github.com/javiermunoz-acebes/Florentin-Lucinde"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold", size = 10),
    plot.margin     = margin(t = 10, r = 25, b = 10, l = 10),
    plot.caption    = element_text(size = 8, color = "grey40",
                                   hjust = 0, margin = margin(t = 8))
  )

print(fig2)
ggsave("Fig_2_Ocio_Desplazamientos.png", plot = fig2,
       width = 11, height = 9, dpi = 300)
message("✓ Fig_2_Ocio_Desplazamientos.png guardada")


# ── RESUMEN ───────────────────────────────────────────────────────────────────
message("\n✓ Proceso completado. Archivos generados:")
message("  · Fig_1_Masculinidades.png")
message("  · Fig_2_Ocio_Desplazamientos.png")

# =============================================================================
# ANÁLISIS ADICIONALES PARA EL REPOSITORIO
# No publicados en el artículo — disponibles en GitHub para verificación
#
# · Fig_A_Koketterie.png          — Campo semántico §2 (Wilhelmine/Juliane)
# · Fig_B_Dispersion_Muessig.png  — Dispersión narrativa de müßig §5
# · Fig_C_TFIDF.png               — Vocabulario más característico (TF-IDF)
# =============================================================================


# ── FIGURA A — KOKETTERIE: INTRADUCIBILIDAD DE LA INOCENCIA  (§2) ─────────────
# Argumento: el mismo vocabulario gestual (Scherz, Ironie, Koketterie) tiene
# carga utópica en Lucinde y táctica/social en Florentin.

campos_coq <- tribble(
  ~campo,                          ~patron,                                      ~polo,
  "Unschuld / Scherz",             "unschuld|scherz|bouffonerie|harmlos|naiv",   "Inocencia ut\u00f3pica",
  "Ironie / Schlauheit",           "ironie|schlauheit|schlau|witz|listig",        "Inocencia ut\u00f3pica",
  "Koketterie / Eitelkeit",        "koketterie|eitelkeit|kokett",                 "Coquetr\u00eda socializada",
  "Kindisch / K\u00fcnstlich",     "kindisch|k\u00fcnstlich|affektiert|zier",     "Coquetr\u00eda socializada",
  "Falsche Scham / Beherrschung",  "scham|beherrschen|zwang|verstellung|maske",  "Coquetr\u00eda socializada"
) |>
  mutate(polo = factor(polo, levels = c(
    "Inocencia ut\u00f3pica",
    "Coquetr\u00eda socializada"
  )))

resultados_coq <- texto_obras |>
  crossing(campos_coq) |>
  mutate(frecuencia = map2_dbl(texto, patron, ~ str_count(.x, .y))) |>
  left_join(total_palabras_obra, by = "obra") |>
  mutate(freq_relativa = round(frecuencia / total_tokens * 1000, 2)) |>
  select(obra, campo, polo, frecuencia, freq_relativa)

message("\n── Figura A: Koketterie ──")
resultados_coq |>
  select(obra, campo, freq_relativa) |>
  pivot_wider(names_from = obra, values_from = freq_relativa) |>
  print()

fig_a <- ggplot(
  resultados_coq,
  aes(x = reorder(campo, freq_relativa), y = freq_relativa, fill = obra)
) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(aes(label = round(freq_relativa, 1)),
            position = position_dodge(0.65), hjust = -0.2, size = 2.8) +
  coord_flip() +
  facet_wrap(~polo, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30))) +
  labs(
    title    = "Intraducibilidad de la inocencia: Wilhelmine vs. Juliane",
    subtitle = "El mismo vocabulario gestual, funci\u00f3n pragm\u00e1tica opuesta",
    x        = NULL,
    y        = "Frecuencia (\u2030)",
    fill     = "Obra",
    caption  = "Fig. A. Campo sem\u00e1ntico de la coquetr\u00eda (an\u00e1lisis complementario \u00a72)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold", size = 10),
    plot.margin     = margin(t = 10, r = 25, b = 10, l = 10),
    plot.caption    = element_text(size = 8, color = "grey40",
                                   hjust = 0, margin = margin(t = 8))
  )

print(fig_a)
ggsave("Fig_A_Koketterie.png", plot = fig_a, width = 10, height = 8, dpi = 300)
message("\u2713 Fig_A_Koketterie.png guardada")


# ── FIGURA B — DISPERSIÓN NARRATIVA DE müßig/Muße  (§5) ──────────────────────
# Argumento: en Lucinde el término es teológico y recurrente (cap. VI, Idylle).
# En Florentin aparece escasas veces y siempre en sentido peyorativo.
# La dispersión hace visible la «censura» del concepto que describe §5.

dispersion_base <- libros |>
  unnest_tokens(word, text) |>       # convierte a minúsculas automáticamente
  filter(!str_detect(word, "[0-9]")) |>
  group_by(obra) |>
  mutate(pos_rel = row_number() / n()) |>
  ungroup()

dispersion_musig <- dispersion_base |>
  filter(str_detect(word, "^m\u00fc\u00dfig|^mu\u00dfe")) |>
  mutate(
    valencia = case_when(
      obra == "Lucinde (Friedrich)"  ~ "Positiva / Teol\u00f3gica",
      obra == "Florentin (Dorothea)" ~ "Peyorativa / Negativa"
    )
  )

message("\n── Figura B: Dispersión de müßig/Muße ──")
message("Ocurrencias detectadas:")
dispersion_musig |>
  count(obra, word, valencia) |>
  arrange(obra) |>
  print()

fig_b <- ggplot(
  dispersion_musig,
  aes(x = pos_rel, y = obra, color = valencia, shape = valencia)
) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_manual(values = c(
    "Positiva / Teol\u00f3gica" = "#2980B9",
    "Peyorativa / Negativa"     = "#E74C3C"
  )) +
  scale_shape_manual(values = c(
    "Positiva / Teol\u00f3gica" = 16,   # círculo sólido
    "Peyorativa / Negativa"     = 4     # ×
  )) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  # Anotaciones manuales de los capítulos clave
  annotate("text", x = 0.42, y = 2.35,
           label  = "Cap. VI \u00abIdylle\u00bb",
           size   = 3, color = "#2980B9", fontface = "italic") +
  annotate("text", x = 0.73, y = 1.35,
           label  = "Cap. XII\n(peyorativo)",
           size   = 3, color = "#E74C3C", fontface = "italic") +
  labs(
    title    = "Dispersi\u00f3n narrativa de 'm\u00fc\u00dfig / Mu\u00dfe': presencia y ausencia",
    subtitle = paste0("Lucinde: concepto teol\u00f3gico recurrente \u00b7 ",
                      "Florentin: ocurrencias escasas y peyorativas"),
    x        = "Posici\u00f3n relativa en la obra (0% = inicio, 100% = fin)",
    y        = NULL,
    color    = "Valencia",
    shape    = "Valencia",
    caption  = "Fig. B. Dispersi\u00f3n de m\u00fc\u00dfig/Mu\u00dfe (an\u00e1lisis complementario \u00a75)."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(face = "bold", size = 11),
    plot.margin      = margin(t = 10, r = 20, b = 10, l = 10),
    plot.caption     = element_text(size = 8, color = "grey40",
                                    hjust = 0, margin = margin(t = 8))
  )

print(fig_b)
ggsave("Fig_B_Dispersion_Muessig.png", plot = fig_b,
       width = 11, height = 5, dpi = 300)
message("\u2713 Fig_B_Dispersion_Muessig.png guardada")


# ── FIGURA C — TF-IDF: VOCABULARIO MÁS CARACTERÍSTICO POR OBRA ───────────────
# TF-IDF pondera cada palabra por su frecuencia en la obra y su rareza
# en el corpus total: identifica lo que distingue cada novela, no lo más
# frecuente. Se trabaja sobre formas de superficie (sin lematización).
# Para análisis con lemas udpipe, véase analisis_completo.R

tokens_tfidf <- libros |>
  unnest_tokens(word, text) |>
  filter(
    !str_detect(word, "[0-9]"),
    !word %in% stop_de$word,
    nchar(word) > 2
  )

top_tfidf <- tokens_tfidf |>
  count(obra, word, sort = TRUE) |>
  bind_tf_idf(word, obra, n) |>
  group_by(obra) |>
  slice_max(tf_idf, n = 15) |>
  ungroup()

message("\n── Figura C: TF-IDF ──")
top_tfidf |>
  select(obra, word, n, tf_idf) |>
  arrange(obra, desc(tf_idf)) |>
  print(n = 30)

fig_c <- ggplot(
  top_tfidf,
  aes(x = reorder_within(word, tf_idf, obra), y = tf_idf, fill = obra)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~obra, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  labs(
    title    = "Vocabulario m\u00e1s caracter\u00edstico por obra (TF-IDF)",
    subtitle = "Formas de superficie tras eliminaci\u00f3n de stopwords",
    x        = NULL,
    y        = "Puntuaci\u00f3n TF-IDF",
    caption  = paste0(
      "Fig. C. An\u00e1lisis TF-IDF (an\u00e1lisis complementario).\n",
      "Sin lematizaci\u00f3n; para an\u00e1lisis con lemas v\u00e9ase analisis_completo.R"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin  = margin(t = 10, r = 20, b = 10, l = 10),
    plot.caption = element_text(size = 8, color = "grey40",
                                hjust = 0, margin = margin(t = 8))
  )

print(fig_c)
ggsave("Fig_C_TFIDF.png", plot = fig_c, width = 11, height = 7, dpi = 300)
message("\u2713 Fig_C_TFIDF.png guardada")


# ── RESUMEN FINAL ──────────────────────────────────────────────────────────────
message("\n\u2714 Proceso completado. Todos los archivos generados:")
message("  PUBLICADOS EN EL ART\u00cdCULO:")
message("    \u00b7 Fig_1_Masculinidades.png")
message("    \u00b7 Fig_2_Ocio_Desplazamientos.png")
message("  REPOSITORIO GITHUB (an\u00e1lisis complementarios):")
message("    \u00b7 Fig_A_Koketterie.png")
message("    \u00b7 Fig_B_Dispersion_Muessig.png")
message("    \u00b7 Fig_C_TFIDF.png")

