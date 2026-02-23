# =============================================================================
# ANÁLISIS LÉXICO COMPARATIVO
# Lucinde (Friedrich Schlegel) vs. Florentin (Dorothea Schlegel)
# =============================================================================
# Análisis incluidos:
#  01 · Riqueza léxica          (TTR, MATTR, Hápax)
#  02 · TF-IDF Sustantivos      (vocabulario nominal característico)
#  03 · TF-IDF Adjetivos        (vocabulario adjetival característico)
#  04 · Campos semánticos       (8 campos ampliados, frecuencia relativa)
#  05 · La aporía del ocio      (Müßiggang, Tätigkeit, Natur, Kunst...)
#  06 · Palabras distintivas    (Log₂-Ratio sobre lemas)
#  07 · Masculinidades          (Teleología vs. Errancia)
#  08 · Colocaciones            (quanteda, lambda λ)
#  09 · Dispersión narrativa    (posición relativa 0–100%)
#  10 · Nubes de palabras       (TF-IDF sobre sustantivos)
# =============================================================================
# NOTA: Ajusta el directorio de trabajo según tu sistema:
# setwd("/Users/Javi/Desktop/florentin+lucinde")
# =============================================================================


# ── 1. INSTALACIÓN Y CARGA DE PAQUETES ───────────────────────────────────────

paquetes_necesarios <- c(
  "tidyverse", "tidytext", "stopwords", "scales",
  "ggwordcloud", "udpipe", "quanteda",
  "quanteda.textstats", "ggrepel", "patchwork"
)
nuevos <- paquetes_necesarios[!paquetes_necesarios %in% installed.packages()[, "Package"]]
if (length(nuevos) > 0) install.packages(nuevos)

library(tidyverse)
library(tidytext)
library(stopwords)
library(scales)
library(ggwordcloud)
library(udpipe)
library(quanteda)
library(quanteda.textstats)
library(ggrepel)
library(patchwork)


# ── 2. CARGA DE DATOS (GitHub) ───────────────────────────────────────────────

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

cat("Descargando corpus desde GitHub...\n")

raw_data <- map_dfr(archivos, function(archivo) {
  url <- paste0(base_url, URLencode(archivo, reserved = TRUE))
  texto <- tryCatch({
    paste(readLines(url, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  }, error = function(e) {
    cat("  ✗ Error:", archivo, "\n"); NA_character_
  })
  tibble(doc_id = archivo, text = texto)
}) |> filter(!is.na(text))

cat("✓ Descargados:", nrow(raw_data), "de", length(archivos), "archivos\n\n")

# Etiquetamos por obra (conservamos doc_id para quanteda)
libros <- raw_data |>
  mutate(obra = case_when(
    str_detect(doc_id, "Florentin") ~ "Florentin (Dorothea)",
    str_detect(doc_id, "Lucinde")   ~ "Lucinde (Friedrich)",
    TRUE ~ "Otro"
  ))

# Texto completo por obra en minúsculas (para análisis con regex directa)
texto_obras <- libros |>
  group_by(obra) |>
  summarise(texto = str_to_lower(paste(text, collapse = "\n\n")), .groups = "drop")

# Total de palabras por obra (para normalizar frecuencias absolutas)
total_palabras_obra <- texto_obras |>
  mutate(total_tokens = str_count(texto, "[[:alpha:]]+")) |>
  select(obra, total_tokens)

# Stopwords alemanas
stop_de <- tibble(word = stopwords::stopwords("de"))


# ── 3. LEMATIZACIÓN Y POS-TAGGING CON UDPIPE ─────────────────────────────────
# udpipe reduce "Bildungen", "Bildes", "gebildet" → lema "Bildung" / "bilden"
# y etiqueta cada token como NOUN, VERB, ADJ, etc.

# Descarga el modelo alemán solo la primera vez (~15 MB)
modelo_archivo <- "german-gsd-ud-2.5-191206.udpipe"
if (!file.exists(modelo_archivo)) {
  cat("Descargando modelo udpipe alemán (primera vez, ~15 MB)...\n")
  udpipe_download_model(language = "german")
}
ud_model <- udpipe_load_model(modelo_archivo)
cat("✓ Modelo udpipe alemán cargado\n\n")

# Sistema de caché: guarda las anotaciones para no repetirlas en cada sesión
# La lematización tarda 3-8 minutos la primera vez
cache_file <- "cache_anotaciones_udpipe.rds"

if (file.exists(cache_file)) {
  cat("✓ Cargando anotaciones desde caché...\n")
  anotaciones <- readRDS(cache_file)
} else {
  cat("Procesando lematización (primera vez, 3-8 min)...\n")
  anotaciones <- map2_dfr(
    texto_obras$texto,
    texto_obras$obra,
    function(texto, obra_nombre) {
      cat("  Anotando:", obra_nombre, "...\n")
      udpipe_annotate(
        ud_model,
        x      = texto,
        doc_id = obra_nombre,
        tagger = "default",
        parser = "none",    # omitimos el parsing sintáctico (más rápido)
        trace  = FALSE
      ) |>
        as.data.frame() |>
        select(doc_id, sentence_id, token_id, token, lemma, upos)
    }
  )
  saveRDS(anotaciones, cache_file)
  cat("✓ Anotaciones guardadas en caché\n")
}

cat("✓ Lematización completa:", nrow(anotaciones), "tokens anotados\n\n")

# Limpieza: eliminamos puntuación, números, stopwords y lemas muy cortos
anotaciones_limpias <- anotaciones |>
  rename(obra = doc_id) |>
  filter(
    !upos %in% c("PUNCT", "NUM", "SYM"),
    !is.na(lemma),
    nchar(lemma) > 2,
    !str_to_lower(lemma) %in% stop_de$word,
    !str_detect(lemma, "[0-9]")
  ) |>
  mutate(lemma_min = str_to_lower(lemma))

# Subconjuntos por categoría gramatical
sustantivos <- filter(anotaciones_limpias, upos == "NOUN")
verbos      <- filter(anotaciones_limpias, upos == "VERB")
adjetivos   <- filter(anotaciones_limpias, upos == "ADJ")

cat("Tokens limpios por obra y categoría gramatical:\n")
anotaciones_limpias |>
  count(obra, upos) |>
  pivot_wider(names_from = upos, values_from = n, values_fill = 0) |>
  print()
cat("\n")


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 1 · RIQUEZA LÉXICA
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══════════════════════════════════════════════\n")
cat("ANÁLISIS 1: RIQUEZA LÉXICA\n")
cat("═══════════════════════════════════════════════\n\n")

# MATTR: Moving Average Type-Token Ratio
# Menos sensible al tamaño del texto que el TTR simple
# step=10 es un buen equilibrio entre precisión y velocidad
mattr_fun <- function(tokens_vec, window = 500, step = 10) {
  n <- length(tokens_vec)
  if (n < window) return(round(n_distinct(tokens_vec) / n, 4))
  starts <- seq(1, n - window + 1, by = step)
  ttrs   <- map_dbl(starts, \(i) n_distinct(tokens_vec[i:(i + window - 1)]) / window)
  round(mean(ttrs), 4)
}

riqueza <- anotaciones_limpias |>
  group_by(obra) |>
  summarise(
    N_tokens      = n(),
    N_types       = n_distinct(lemma_min),
    TTR           = round(N_types / N_tokens, 4),
    MATTR_500     = mattr_fun(lemma_min, window = 500),
    Hapax         = sum(table(lemma_min) == 1),
    Pct_hapax     = round(Hapax / N_tokens * 100, 2),
    N_sustantivos = sum(upos == "NOUN"),
    N_verbos      = sum(upos == "VERB"),
    N_adjetivos   = sum(upos == "ADJ"),
    .groups = "drop"
  )

cat("Métricas de riqueza léxica:\n")
print(as.data.frame(riqueza))
write.csv(riqueza, "Riqueza_Lexica.csv", row.names = FALSE)

# Gráfico
riqueza_long <- riqueza |>
  select(obra,
         `TTR`                 = TTR,
         `MATTR\n(ventana 500)` = MATTR_500,
         `Hápax (%)`           = Pct_hapax) |>
  pivot_longer(-obra, names_to = "Métrica", values_to = "Valor")

g_riqueza <- ggplot(riqueza_long, aes(x = Métrica, y = Valor, fill = obra)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = round(Valor, 3)),
            position = position_dodge(0.6), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Métricas de riqueza léxica",
       subtitle = "TTR, MATTR (ventana 500) y porcentaje de hápax legómena",
       x = NULL, y = NULL, fill = "Obra") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(g_riqueza)
ggsave("01_Riqueza_Lexica.png", plot = g_riqueza, width = 8, height = 5, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 2–3 · TF-IDF (SUSTANTIVOS Y ADJETIVOS)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════\n")
cat("ANÁLISIS 2–3: TF-IDF (SUSTANTIVOS Y ADJETIVOS)\n")
cat("═══════════════════════════════════════════════\n\n")

# ── TF-IDF Sustantivos ──────────────────────────────────────────────────────
top_tfidf_sust <- sustantivos |>
  count(obra, lemma_min) |>
  bind_tf_idf(lemma_min, obra, n) |>
  group_by(obra) |>
  slice_max(tf_idf, n = 15) |>
  ungroup()

g_tfidf_sust <- ggplot(
  top_tfidf_sust,
  aes(x = reorder_within(lemma_min, tf_idf, obra), y = tf_idf, fill = obra)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~obra, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  labs(title    = "Sustantivos más característicos por obra (TF-IDF)",
       subtitle = "Calculado sobre lemas nominales (udpipe)",
       x = NULL, y = "Puntuación TF-IDF") +
  theme_minimal(base_size = 12)

print(g_tfidf_sust)
ggsave("02_TF-IDF_Sustantivos.png", plot = g_tfidf_sust, width = 11, height = 7, dpi = 300)

# ── TF-IDF Adjetivos ────────────────────────────────────────────────────────
top_tfidf_adj <- adjetivos |>
  count(obra, lemma_min) |>
  bind_tf_idf(lemma_min, obra, n) |>
  group_by(obra) |>
  slice_max(tf_idf, n = 15) |>
  ungroup()

g_tfidf_adj <- ggplot(
  top_tfidf_adj,
  aes(x = reorder_within(lemma_min, tf_idf, obra), y = tf_idf, fill = obra)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~obra, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  labs(title    = "Adjetivos más característicos por obra (TF-IDF)",
       subtitle = "Calculado sobre lemas adjetivales (udpipe)",
       x = NULL, y = "Puntuación TF-IDF") +
  theme_minimal(base_size = 12)

print(g_tfidf_adj)
ggsave("03_TF-IDF_Adjetivos.png", plot = g_tfidf_adj, width = 11, height = 7, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 4 · CAMPOS SEMÁNTICOS AMPLIADOS
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════\n")
cat("ANÁLISIS 4: CAMPOS SEMÁNTICOS AMPLIADOS\n")
cat("═══════════════════════════════════════════════\n\n")

campos <- tribble(
  ~campo,                   ~patron,
  "Müßiggang / Muße",       "müßig|muße|faulheit|ruhe|genuss",
  "Tätigkeit / Streben",    "tätigkeit|arbeit|streben|fleiß|eifer|beschäftigung",
  "Langeweile / Leere",     "langeweile|leere|leer|öde|gleichgültigkeit",
  "Natur / Organismus",     "natur|pflanze|blume|gewächs|wald|organismus|wachstum",
  "Kunst / Poesie",         "kunst|poesie|roman|dichten|bild|gemälde|schönheit",
  "Bildung / Harmonie",     "bildung|harmonie|entwicklung|vollendung|einheit|bestimmung",
  "Errancia / Zufall",      "zufall|irren|fremd|wandern|flucht|heimatlos",
  "Liebe / Geschlecht",     "liebe|geschlecht|weiblich|männlich|ehe|hingabe"
)

resultados_campos <- texto_obras |>
  crossing(campos) |>
  mutate(frecuencia = map2_dbl(texto, patron, ~ str_count(.x, .y))) |>
  left_join(total_palabras_obra, by = "obra") |>
  mutate(freq_relativa = round(frecuencia / total_tokens * 1000, 2)) |>
  select(obra, campo, frecuencia, freq_relativa)

cat("Frecuencias por campo semántico (por 1.000 palabras):\n")
resultados_campos |>
  select(obra, campo, freq_relativa) |>
  pivot_wider(names_from = obra, values_from = freq_relativa) |>
  print()
cat("\n")

write.csv(resultados_campos, "Campos_Semanticos.csv", row.names = FALSE)

g_campos <- ggplot(
  resultados_campos,
  aes(x = reorder(campo, freq_relativa), y = freq_relativa, fill = obra)
) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(freq_relativa, 1)),
            position = position_dodge(0.7), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(title    = "Campos semánticos: frecuencia relativa",
       subtitle = "Ocurrencias por cada 1.000 palabras del corpus",
       x = NULL, y = "Frecuencia (‰)", fill = "Obra") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(g_campos)
ggsave("04_Campos_Semanticos.png", plot = g_campos, width = 10, height = 7, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 5 · LA APORÍA DEL OCIO
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══════════════════════════════════════════════\n")
cat("ANÁLISIS 5: LA APORÍA DEL OCIO\n")
cat("═══════════════════════════════════════════════\n\n")

terminos_ocio <- anotaciones_limpias |>
  mutate(categoria = case_when(
    str_detect(lemma_min, "^müßig|^muße|^faulheit")                  ~ "Müßiggang / Muße",
    str_detect(lemma_min, "^langeweile|^leere|^öde")                  ~ "Langeweile / Leere",
    str_detect(lemma_min, "^tätigkeit|^beschäftig|^arbeit|^streben")  ~ "Tätigkeit (Actividad)",
    str_detect(lemma_min, "^natur|^wachstum|^organismus")             ~ "Natur (Naturaleza)",
    str_detect(lemma_min, "^kunst|^künstlich|^poesie|^schönheit")     ~ "Kunst / Poesie",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(categoria))

freq_ocio <- terminos_ocio |>
  count(obra, categoria) |>
  left_join(
    anotaciones_limpias |> count(obra, name = "total"),
    by = "obra"
  ) |>
  mutate(freq_rel = round(n / total * 1000, 2))

g_ocio <- ggplot(
  freq_ocio,
  aes(x = reorder(categoria, freq_rel), y = freq_rel, fill = obra)
) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(n, "\n(", round(freq_rel, 1), "‰)")),
            position = position_dodge(0.7), hjust = -0.1, size = 2.6) +
  coord_flip() +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title    = "La aporía del ocio: comparativa léxica",
       subtitle = "Frecuencia relativa con valores absolutos anotados",
       x = NULL, y = "Frecuencia (‰)", fill = "Obra",
       caption  = "Figura 1. Análisis computacional del corpus.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(g_ocio)
ggsave("05_Aporia_Ocio.png", plot = g_ocio, width = 10, height = 6, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 6 · PALABRAS DISTINTIVAS (LOG₂-RATIO)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════\n")
cat("ANÁLISIS 6: PALABRAS DISTINTIVAS (LOG₂-RATIO)\n")
cat("═══════════════════════════════════════════════\n\n")

palabras_ratio <- anotaciones_limpias |>
  count(lemma_min, obra) |>
  group_by(lemma_min) |>
  filter(sum(n) >= 5) |>
  ungroup() |>
  pivot_wider(names_from = obra, values_from = n, values_fill = 0) |>
  mutate(
    log_ratio = log2((`Florentin (Dorothea)` + 1) / (`Lucinde (Friedrich)` + 1))
  )

top_log <- palabras_ratio |>
  mutate(direccion = ifelse(log_ratio > 0, "Más en Florentin", "Más en Lucinde")) |>
  group_by(direccion) |>
  slice_max(abs(log_ratio), n = 15) |>
  ungroup()

g_logratio <- ggplot(
  top_log,
  aes(x = reorder(lemma_min, log_ratio), y = log_ratio, fill = direccion)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Más en Florentin" = "#E74C3C",
                               "Más en Lucinde"   = "#2980B9")) +
  labs(title    = "Palabras más distintivas de cada novela (Log₂-Ratio)",
       subtitle = "Calculado sobre lemas limpios — mínimo 5 ocurrencias en el corpus total",
       y = "Log₂ Ratio (+ Florentin / − Lucinde)", x = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(g_logratio)
ggsave("06_Log_Ratio_Distintivo.png", plot = g_logratio, width = 9, height = 8, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 7 · MASCULINIDADES: TELEOLOGÍA VS. ERRANCIA
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════\n")
cat("ANÁLISIS 7: MASCULINIDADES (TELEOLOGÍA VS. ERRANCIA)\n")
cat("═══════════════════════════════════════════════\n\n")

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

cat("Masculinidades — frecuencias absolutas y relativas:\n")
print(as.data.frame(datos_masc))

g_masc <- ggplot(datos_masc, aes(x = freq_rel, y = concepto, fill = obra)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(frecuencia, "  (", round(freq_rel, 1), "‰)")),
            position = position_dodge(0.6), hjust = -0.1, size = 3.3) +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(title    = "Estructura narrativa: De la Teleología a la Errancia",
       subtitle = "Comparativa de frecuencia léxica en la construcción del sujeto masculino",
       x = "Frecuencia (‰)", y = NULL, fill = "Obra",
       caption  = "Figura 2. Análisis computacional del corpus.") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold", size = 13),
        axis.text.y      = element_text(size = 11, face = "bold"),
        legend.position  = "bottom")

print(g_masc)
ggsave("07_Masculinidades_Teleologia.png", plot = g_masc, width = 10, height = 5, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 8 · KWIC Y COLOCACIONES (quanteda)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════\n")
cat("ANÁLISIS 8: KWIC Y COLOCACIONES (quanteda)\n")
cat("═══════════════════════════════════════════════\n\n")

# Corpus quanteda con doc_id por capítulo y docvar 'obra'
corpus_q <- corpus(libros, text_field = "text", docid_field = "doc_id")
docvars(corpus_q, "obra") <- libros$obra

# Tokenización
tokens_q <- tokens(corpus_q,
                   remove_punct   = TRUE,
                   remove_numbers = TRUE,
                   remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_remove(pattern = stopwords("de"))

# ── KWIC: contexto ±6 palabras de los términos de la tesis ─────────────────
kwic_terminos <- c("müßig*", "bildung*", "natur*", "kunst*",
                   "fremd*", "liebe*", "harmoni*", "zufall*", "wander*")

cat("Exportando contextos KWIC:\n")
walk(kwic_terminos, function(t) {
  resultado <- kwic(tokens_q, pattern = t, window = 6)
  nombre    <- str_remove(t, "\\*")
  if (nrow(resultado) > 0) {
    write.csv(as.data.frame(resultado), paste0("KWIC_", nombre, ".csv"), row.names = FALSE)
    cat("  ✓ KWIC_", nombre, ".csv  →  ", nrow(resultado), " ocurrencias\n", sep = "")
  } else {
    cat("  ✗ KWIC_", nombre, ": sin ocurrencias\n", sep = "")
  }
})

# ── Colocaciones estadísticas por obra (bigramas, λ = fuerza de asociación) ──
# tokens_subset usa docvars directamente (no-standard evaluation)
tok_fl <- tokens_subset(tokens_q, obra == "Florentin (Dorothea)")
tok_lu <- tokens_subset(tokens_q, obra == "Lucinde (Friedrich)")

coloc_fl <- textstat_collocations(tok_fl, min_count = 3, size = 2) |>
  arrange(desc(lambda)) |> head(20)
coloc_lu <- textstat_collocations(tok_lu, min_count = 3, size = 2) |>
  arrange(desc(lambda)) |> head(20)

cat("\n── Top colocaciones Florentin (λ) ──\n"); print(as.data.frame(coloc_fl))
cat("\n── Top colocaciones Lucinde (λ) ──\n");   print(as.data.frame(coloc_lu))

write.csv(coloc_fl, "Colocaciones_Florentin.csv", row.names = FALSE)
write.csv(coloc_lu, "Colocaciones_Lucinde.csv",   row.names = FALSE)

# Gráfico de colocaciones
coloc_plot <- bind_rows(
  mutate(head(coloc_fl, 12), obra = "Florentin (Dorothea)"),
  mutate(head(coloc_lu, 12), obra = "Lucinde (Friedrich)")
)

g_coloc <- ggplot(
  coloc_plot,
  aes(x = reorder_within(collocation, lambda, obra), y = lambda, fill = obra)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~obra, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                               "Lucinde (Friedrich)"  = "#2980B9")) +
  labs(title    = "Colocaciones más frecuentes por obra",
       subtitle = "Ordenadas por fuerza de asociación (lambda λ)",
       x = NULL, y = "λ") +
  theme_minimal(base_size = 12)

print(g_coloc)
ggsave("08_Colocaciones.png", plot = g_coloc, width = 11, height = 7, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 9 · DISPERSIÓN NARRATIVA
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════\n")
cat("ANÁLISIS 9: DISPERSIÓN NARRATIVA\n")
cat("═══════════════════════════════════════════════\n\n")

# Tokenizamos con posición relativa dentro de cada obra
# unnest_tokens convierte a minúsculas por defecto
dispersion_base <- libros |>
  unnest_tokens(word, text) |>
  filter(!str_detect(word, "[0-9]"),
         !word %in% stop_de$word) |>
  group_by(obra) |>
  mutate(pos_rel = row_number() / n()) |>
  ungroup()

# Etiquetamos por campo semántico usando case_when
dispersion_datos <- dispersion_base |>
  mutate(concepto = case_when(
    str_detect(word, "^müßig|^muße")      ~ "Müßiggang",
    str_detect(word, "^bildung")           ~ "Bildung",
    str_detect(word, "^natur")             ~ "Natur",
    str_detect(word, "^kunst|^poesie")     ~ "Kunst / Poesie",
    str_detect(word, "^fremd")             ~ "Fremdheit",
    str_detect(word, "^liebe")             ~ "Liebe",
    str_detect(word, "^wander|^irr")       ~ "Errancia",
    str_detect(word, "^harmoni")           ~ "Harmonie",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(concepto))

cat("Ocurrencias por concepto y obra:\n")
dispersion_datos |> count(obra, concepto) |> print()
cat("\n")

g_dispersion <- ggplot(
  dispersion_datos,
  aes(x = pos_rel, y = concepto, color = obra)
) +
  geom_point(size = 0.8, alpha = 0.6) +
  facet_wrap(~obra, ncol = 1) +
  scale_color_manual(values = c("Florentin (Dorothea)" = "#E74C3C",
                                "Lucinde (Friedrich)"  = "#2980B9")) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.1)) +
  labs(title    = "Dispersión narrativa de conceptos clave",
       subtitle = "Distribución a lo largo del texto (0% = inicio, 100% = final)",
       x = "Posición en la obra", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "none",
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold", size = 11))

print(g_dispersion)
ggsave("09_Dispersion_Narrativa.png", plot = g_dispersion,
       width = 12, height = 7, dpi = 300)


# ═══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS 10 · NUBES DE PALABRAS (TF-IDF SOBRE SUSTANTIVOS)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════\n")
cat("ANÁLISIS 10: NUBES DE PALABRAS (TF-IDF)\n")
cat("═══════════════════════════════════════════════\n\n")

# El tamaño de cada palabra = su especificidad en esa obra (TF-IDF)
# así las palabras más grandes son las más CARACTERÍSTICAS, no las más frecuentes
top_nube <- sustantivos |>
  count(obra, lemma_min) |>
  bind_tf_idf(lemma_min, obra, n) |>
  group_by(obra) |>
  slice_max(tf_idf, n = 60) |>
  ungroup()

g_nube_fl <- ggplot(
  top_nube |> filter(obra == "Florentin (Dorothea)"),
  aes(label = lemma_min, size = tf_idf)
) +
  geom_text_wordcloud(color = "#E74C3C", rm_outside = TRUE, seed = 42) +
  scale_size_area(max_size = 14) +
  theme_minimal() +
  labs(title = "Florentin (Dorothea Schlegel)")

g_nube_lu <- ggplot(
  top_nube |> filter(obra == "Lucinde (Friedrich)"),
  aes(label = lemma_min, size = tf_idf)
) +
  geom_text_wordcloud(color = "#2980B9", rm_outside = TRUE, seed = 42) +
  scale_size_area(max_size = 14) +
  theme_minimal() +
  labs(title = "Lucinde (Friedrich Schlegel)")

g_nubes <- g_nube_fl + g_nube_lu +
  plot_annotation(
    title    = "Vocabulario más característico de cada novela (TF-IDF)",
    subtitle = "El tamaño refleja la especificidad del lema frente a la otra obra",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  )

print(g_nubes)
ggsave("10_Nubes_Palabras.png", plot = g_nubes, width = 14, height = 7, dpi = 300)


# ── RESUMEN FINAL ──────────────────────────────────────────────────────────────

cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║  ✓ ANÁLISIS COMPLETADO                           ║\n")
cat("╠══════════════════════════════════════════════════╣\n")
cat("║  GRÁFICOS (PNG, 300 dpi):                        ║\n")
cat("║  01_Riqueza_Lexica.png                           ║\n")
cat("║  02_TF-IDF_Sustantivos.png                       ║\n")
cat("║  03_TF-IDF_Adjetivos.png                         ║\n")
cat("║  04_Campos_Semanticos.png                        ║\n")
cat("║  05_Aporia_Ocio.png                              ║\n")
cat("║  06_Log_Ratio_Distintivo.png                     ║\n")
cat("║  07_Masculinidades_Teleologia.png                ║\n")
cat("║  08_Colocaciones.png                             ║\n")
cat("║  09_Dispersion_Narrativa.png                     ║\n")
cat("║  10_Nubes_Palabras.png                           ║\n")
cat("╠══════════════════════════════════════════════════╣\n")
cat("║  TABLAS (CSV):                                   ║\n")
cat("║  Riqueza_Lexica.csv                              ║\n")
cat("║  Campos_Semanticos.csv                           ║\n")
cat("║  Colocaciones_Florentin.csv                      ║\n")
cat("║  Colocaciones_Lucinde.csv                        ║\n")
cat("║  KWIC_[término].csv  (×9)                        ║\n")
cat("╠══════════════════════════════════════════════════╣\n")
cat("║  CACHÉ:                                          ║\n")
cat("║  cache_anotaciones_udpipe.rds                    ║\n")
cat("╚══════════════════════════════════════════════════╝\n")
