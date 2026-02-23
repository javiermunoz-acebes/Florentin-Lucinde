# Florentin & Lucinde — Análisis léxico computacional

Corpus y scripts de análisis de frecuencia léxica comparada de
*Lucinde* (Friedrich Schlegel, 1799) y *Florentin* (Dorothea Schlegel, 1801).

Desarrollado como material complementario al artículo:
> Muñoz Acebes, J. (2026). «El léxico romántico a prueba: Desafíos
> hermenéuticos en la traducción de *Florentin* a la luz de *Lucinde*».

## Figuras publicadas en el artículo
- `Fig_1_Masculinidades.png` — Sección 3
- `Fig_2_Ocio_Desplazamientos.png` — Sección 5

## Scripts
- analisis_Florentin_Lucinde_lexico.R — Genera las figuras publicadas así como el análisis completo de los textos
- analisis_capitulos.R - Evolución de conceptos clave a lo largo de la obra
- analisis_lexico_lemma.R - Análisis experimental a través del paquete "udpipe"

## Requisitos
R >= 4.1 | tidyverse, stopwords, scales, tidytext
El corpus se descarga automáticamente desde este repositorio.
