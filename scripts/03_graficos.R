# =======================
#   LIBRERÍAS Y RUTAS
# =======================

library(plotly)
library(dplyr)
library(lubridate)
library(readr)
library(purrr)
library(openxlsx)
library(stringr)
library(ggthemes)
library(scales)

# Seteo de rutas
base_dir <- getwd()         
project_dir <- dirname(base_dir) 

instub <- file.path(project_dir, "input")
outstub <- file.path(project_dir, "output")

# Leer datos
delitos <- read.csv(file.path(instub, "delitos.csv"))

options(scipen = 999)

# Tipografía
windowsFonts(SegoeUI = windowsFont("Segoe UI"))

# =======================
#   LIMPIEZA DE DATOS
# =======================

# Ordenar y convertir
delitos <- delitos %>%
  mutate(
    mes = factor(
      mes,
      levels = c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
                 "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"),
      ordered = TRUE
    ),
    dia = factor(
      dia,
      levels = c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"),
      ordered = TRUE
    ),
    fecha = as.Date(fecha),
    mes_num = match(
      mes,
      c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
        "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
    ),
    mes_fecha = make_date(year = anio, month = mes_num, day = 1)
  )

glimpse(delitos)

# =======================
#   HORA - DONUT PLOT
# =======================

delitos_por_rango <- delitos %>%
  mutate(
    rango = case_when(
      franja >= 0  & franja < 6  ~ "Madrugada\n00:00 a 06:00 hs",
      franja >= 6  & franja < 12 ~ "Mañana\n06:00 a 12:00 hs",
      franja >= 12 & franja < 18 ~ "Tarde\n12:00 a 18:00 hs",
      TRUE                       ~ "Noche\n18:00 a 00:00 hs"
    )
  ) %>%
  group_by(rango) %>%
  summarise(total_delitos = sum(cantidad, na.rm = TRUE)) %>%
  mutate(
    rango = factor(
      rango,
      levels = c(
        "Madrugada\n00:00 a 06:00 hs",
        "Mañana\n06:00 a 12:00 hs",
        "Tarde\n12:00 a 18:00 hs",
        "Noche\n18:00 a 00:00 hs"
      )
    ),
    proporcion = round(total_delitos / sum(total_delitos) * 100, 1)
  ) %>%
  arrange(rango) %>%
  mutate(
    ymax = cumsum(proporcion),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2,
    label = paste0(proporcion, "%")
  )

donut_plot <- ggplot(delitos_por_rango, aes(ymax = ymax, ymin = ymin, xmax = 2.5, xmin = 1.5, fill = rango)) +
  geom_rect(color = "white") +
  geom_text(
    aes(x = 2, y = label_pos, label = label),
    family = "Segoe UI",
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = c(
      "Madrugada\n00:00 a 06:00 hs" = "#bbd1c4",
      "Mañana\n06:00 a 12:00 hs"    = "#6C9286",
      "Tarde\n12:00 a 18:00 hs"     = "#7E8B97",
      "Noche\n18:00 a 00:00 hs"     = "#365B6D"
    )
  ) +
  xlim(0.5, 2.5) +
  labs(
    title = "Porcentaje de delitos por franja horaria",
    subtitle = "Ciudad de Buenos Aires (2019–2023)",
    caption = "Fuente: BA Data",
    fill = "Rango horario"
  ) +
  theme_void(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#365B6D"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4A4A4A"),
    plot.caption = element_text(size = 8, face = "italic", color = "#666666"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 11)
  )

#guardar
ggsave(file.path(outstub, "donut_plot_con_horarios.png"), plot = donut_plot, width = 8, height = 5, dpi = 300)

# =======================
#   DÍA - BARRAS
# =======================

delitos_por_dia_total <- delitos %>%
  group_by(dia) %>%
  summarise(total_delitos = sum(cantidad, na.rm = TRUE)) %>%
  arrange(desc(total_delitos)) %>%
  mutate(
    rank = row_number(),
    color = colorRampPalette(c("#365b6d", "#8fab9a"))(7)[rank]
  )

delitos_por_dia_total$dia <- factor(delitos_por_dia_total$dia, levels = delitos_por_dia_total$dia)

delitos_dia <- ggplot(delitos_por_dia_total, aes(x = dia, y = total_delitos, fill = color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = comma(total_delitos, big.mark = ".", decimal.mark = ",")),
    hjust = -0.05,
    family = "Segoe UI",
    fontface = "bold",
    size = 3.5,
    color = "#2E2E2E"
  ) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Total de delitos por día de la semana",
    subtitle = "Ciudad de Buenos Aires (2019–2023)",
    x = "Día de la semana",
    y = "Cantidad de delitos",
    caption = "Fuente: BA Data"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(color = "#365B6D", face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(color = "#4A4A4A", size = 14, hjust = 0.5),
    axis.title = element_text(color = "#4A4A4A", size = 12),
    axis.text = element_text(color = "#333333", size = 13),
    plot.caption = element_text(size = 8, face = "italic", color = "#666666"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.y = element_line(color = "gray70"),
    axis.line.x = element_line(color = "gray70"),
    panel.background = element_rect(fill = "#F2F1EC", color = NA),
    plot.background = element_rect(fill = "#F2F1EC", color = NA)
  )

#guardar
ggsave(file.path(outstub, "delitos_dia.png"), plot = delitos_dia, width = 7, height = 7, dpi = 300)

# =======================
#   MES - BARRAS
# =======================

delitos_por_mes <- delitos %>%
  group_by(mes) %>%
  summarise(total_delitos = sum(cantidad, na.rm = TRUE)) %>%
  arrange(mes)

paleta_mes <- colorRampPalette(c("#6C9286", "#7E8B97"))(12)

delitos_por_mes <- delitos_por_mes %>%
  arrange(desc(total_delitos)) %>%
  mutate(
    rank = row_number(),
    color = paleta_mes[rank],
    mes = factor(mes, levels = mes)  # reordena factor segun total_delitos
  )

delitos_mes <- ggplot(delitos_por_mes, aes(x = mes, y = total_delitos, fill = color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = comma(total_delitos, big.mark = ".", decimal.mark = ",")),
    vjust = -0.5,
    family = "Segoe UI",
    fontface = "bold",
    size = 3.5,
    color = "#2E2E2E"
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Total de delitos por mes",
    subtitle = "Ciudad de Buenos Aires",
    x = "Mes",
    y = "Cantidad de delitos",
    caption = "Fuente: BA Data"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(color = "#365B6D", face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(color = "#4A4A4A", size = 14, hjust = 0.5),
    axis.title = element_text(color = "#4A4A4A", size = 12),
    axis.text = element_text(color = "#333333", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 8, face = "italic", color = "#666666"),
    panel.background = element_rect(fill = "#F2F1EC", color = NA),
    plot.background = element_rect(fill = "#F2F1EC", color = NA)
  )
print(delitos_mes)
ggsave(file.path(outstub, "delitos_mes.png"), plot = delitos_mes, width = 7, height = 7, dpi = 300)

# =======================
#   AÑO - BARRAS
# =======================

delitos_por_anio <- delitos %>%
  group_by(anio) %>%
  summarise(total_delitos = sum(cantidad, na.rm = TRUE)) %>%
  arrange(anio) %>%
  mutate(
    color = colorRampPalette(c("#6C9286", "#7E8B97"))(n())
  )

delitos_anio <- ggplot(delitos_por_anio, aes(x = factor(anio), y = total_delitos, fill = color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = comma(total_delitos, big.mark = ".", decimal.mark = ",")),
    vjust = -0.5,
    family = "Segoe UI",
    fontface = "bold",
    size = 3.5,
    color = "#2E2E2E"
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Total de delitos por año",
    subtitle = "Ciudad de Buenos Aires",
    x = "Año",
    y = "Cantidad de delitos",
    caption = "Fuente: BA Data"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(color = "#365B6D", face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(color = "#4A4A4A", size = 14, hjust = 0.5),
    axis.title = element_text(color = "#4A4A4A", size = 12),
    axis.text = element_text(color = "#333333", size = 8),
    plot.caption = element_text(size = 8, face = "italic", color = "#666666"),
    panel.background = element_rect(fill = "#F2F1EC", color = NA),
    plot.background = element_rect(fill = "#F2F1EC", color = NA)
  )

ggsave(file.path(outstub, "delitos_anio.png"), plot = delitos_anio, width = 7, height = 7, dpi = 300)

# =======================
#   SERIE DE TIEMPO
# =======================

# Agrupar y preparar
delitos_por_mes <- delitos %>%
  group_by(mes_fecha) %>%
  summarise(total_delitos = sum(cantidad, na.rm = TRUE)) %>%
  arrange(mes_fecha)

# Gráfico
serie_de_tiempo <- ggplot(delitos_por_mes, aes(x = mes_fecha, y = total_delitos)) +
  geom_line(color = "#365B6D", size = 1) +
  geom_point(color = "#365B6D", size = 2) +
  labs(
    title = "Evolución temporal",
    subtitle = "Ciudad de Buenos Aires (2019–2023)",
    x = "Mes",
    y = "Cantidad de delitos",
    caption = "Fuente: BA Data"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 months",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(color = "#365B6D", face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(color = "#4A4A4A", size = 14, hjust = 0.5),
    axis.title = element_text(color = "#4A4A4A", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, color = "#333333"),
    axis.text.y = element_text(size = 8, color = "#333333"),
    plot.caption = element_text(size = 8, face = "italic", color = "#666666"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    axis.line.x = element_line(color = "gray70"),
    axis.line.y = element_line(color = "gray70"),
    panel.background = element_rect(fill = "#F2F1EC", color = NA),
    plot.background = element_rect(fill = "#F2F1EC", color = NA)
  )

#guardar

ggsave(
  filename = file.path(outstub, "serie_tiempo.png"),
  plot = serie_de_tiempo,
  width = 12,
  height = 7,
  dpi = 300
)

# =======================
#   DELITOS POR TIPO
# =======================
delitos_por_tipo <- delitos %>%
  group_by(tipo) %>%
  summarise(total = sum(cantidad, na.rm = TRUE)) %>%
  arrange(desc(total))

# === Paleta de colores ===
paleta <- colorRampPalette(c("#6C9286", "#7E8B97"))(nrow(delitos_por_tipo))

# Agregar color según ranking
delitos_por_tipo <- delitos_por_tipo %>%
  mutate(
    tipo = reorder(tipo, total),
    color = paleta
  )

# === Plot ===
barras_tipo <- ggplot(delitos_por_tipo, aes(x = tipo, y = total, fill = color)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(
    aes(label = comma(total, big.mark = ".", decimal.mark = ",")),
    hjust = -0.1,
    size = 3.5,
    family = "Arial",
    fontface = "bold",
    color = "#2E2E2E"
  ) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    breaks = seq(0, 250000, 50000),
    labels = comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Total de delitos por tipo",
    subtitle = "Ciudad de Buenos Aires (2019-2023)",
    x = "",
    y = "Cantidad de delitos",
    caption = "Fuente: BA Data"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(color = "#365B6D", face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(color = "#4A4A4A", size = 14, hjust = 0.5),
    axis.title = element_text(color = "#4A4A4A", size = 12),
    axis.text.x = element_text(color = "#333333", size = 10),
    axis.text.y = element_text(color = "#333333", size = 10, face = "bold"),
    plot.caption = element_text(size = 8, face = "italic", color = "#666666"),
    panel.background = element_rect(fill = "#F2F1EC", color = NA),
    plot.background = element_rect(fill = "#F2F1EC", color = NA)
  )

#guardar
ggsave(
  filename = file.path(outstub, "barras_tipo.png"),
  plot = barras_tipo,
  width = 7,
  height = 7,
  dpi = 300
)