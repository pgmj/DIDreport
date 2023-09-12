#DIDcolors <- c("lightgrey","#009E73", "#F0E442", "#D55E00")
DIDcolorsGGYR <- c("lightgrey","#509B8E", "#E2C578", "#DB7358")
DIDcolorsRYGG <- c("#DB7358", "#E2C578", "#509B8E", "lightgrey")
#DIDtestColors <- c("#509B8E","#E2C578","#DB7358")

### text sizes
ax.size <- 10
title.size <- 12
legend.size <- 10
stript.size <- 10

theme_rise <- function(fontfamily = "Lato", axissize = 13, titlesize = 15,
                       margins = 12, axisface = "plain", stripsize = 12,
                       panelDist = 0.6, legendSize = 11, legendTsize = 12) {
  theme_minimal() +
  theme(
    text = element_text(family = fontfamily),
    axis.title.x = element_text(
      margin = margin(t = margins),
      size = axissize
    ),
    axis.title.y = element_text(
      margin = margin(r = margins),
      size = axissize
    ),
    plot.title = element_text(
      face = "bold",
      size = titlesize
    ),
    axis.title = element_text(
      face = axisface
    ),
    plot.caption = element_text(
      face = "italic"
    ),
    legend.text = element_text(family = fontfamily, size = legendSize),
    legend.title = element_text(family = fontfamily, size = legendTsize),
    legend.background = element_rect(color = "lightgrey"),
    strip.text = element_text(size = stripsize),
    strip.background = element_rect(color = "lightgrey"),
    panel.spacing = unit(panelDist, "cm", data = NULL)
  ) +
    # these rows are for geom_text() and geom_text_repel() to match font family
    update_geom_defaults("text", list(family = fontfamily)) +
    update_geom_defaults("text_repel", list(family = fontfamily)) +
    update_geom_defaults("textpath", list(family = fontfamily)) +
    update_geom_defaults("texthline", list(family = fontfamily))
}

gender_colors <- c("Pojke" = "#F5A127", "Flicka" = "#009CA6")
scale_color_gender <- partial(scale_color_manual, values = gender_colors)
scale_fill_gender <- partial(scale_fill_manual, values = gender_colors)

# Överblick ---------------------------------------------------------------


DIDsnirkel <- function(årtal) {
  df.risk %>%
    filter(Kommun == fokusKommun) %>%
    filter(År == {{ årtal }} ) %>%
    filter(!Index %in% c("Välbefinnande", "Positiv skolanknytning")) %>%
    mutate(riskLevel = car::recode(riskLevel,"NA='För få svar';
                                   '<NA>'='För få svar'")) %>%
    mutate(Risknivå = factor(riskLevel,
                             levels = c("För få svar", "Låg risk", "Något förhöjd risk", "Förhöjd risk"))) %>%
    ggplot(aes(x = Index, y = Andel, fill = Risknivå)) +
    geom_col(alpha = 0.9) +
    geom_textpath(aes(label = Index, group = Index),
                  text_only = T,
                  position = "stack",
                  hjust = 0,
                  size = 4
    ) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = DIDcolorsGGYR) +
    scale_x_discrete(
      expand = expansion(add = c(3, 0)),
      limits = rev,
      labels = NULL
    ) +
    scale_y_continuous(
      breaks = seq(0, 90, 10),
      labels = paste0(seq(0, 90, 10), "%")
    ) +
    labs(title = paste0(fokusKommun, " - ", årtal),
         caption = "Datakälla: Stockholmsenkäten.") +
    geom_texthline(
      yintercept = 10, color = "black",
      linetype = 2, size = 2.5, alpha = 0.6,
      label = "Förhöjd risk",
      hjust = 0.05
    ) +
    geom_texthline(
      yintercept = 25, color = RISEprimRed,
      linetype = 2, size = 2.5, alpha = 0.6,
      label = "Något förhöjd risk",
      hjust = 0.15
    ) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme_rise()
}

DIDstapel <- function(data,årtal, tpathsize = 4) {
  year <- årtal
  data %>%
    filter(Kommun == fokusKommun) %>%
    filter(År == year) %>%
    filter(!Index %in% c("Välbefinnande", "Positiv skolanknytning")) %>%
    mutate(riskLevel = car::recode(riskLevel,"NA='För få svar'")) %>%
    mutate(Risknivå = factor(riskLevel, levels = c("För få svar", "Låg risk", "Något förhöjd risk", "Förhöjd risk"))) %>%
    ggplot(aes(x = Index, y = Andel, fill = Risknivå)) +
    geom_col(alpha = 0.9) +
    geom_textpath(aes(label = Index, group = Index),
                  text_only = T,
                  position = "stack",
                  hjust = 0,
                  size = tpathsize
    ) +
    scale_fill_manual(values = DIDcolorsGGYR) +
    scale_x_discrete(
      limits = rev,
      labels = NULL
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20),
      minor_breaks = seq(0, 100, 10),
      labels = paste0(seq(0, 100, 20), "%"),
    ) +
    labs(title = paste0(fokusKommun, " - ", year),
         caption = "Datakälla: Stockholmsenkäten.") +
    geom_texthline(
      yintercept = 10, color = "black",
      linetype = 2, size = 2.5, alpha = 0.6,
      label = "Förhöjd risk",
      hjust = 0.05
    ) +
    geom_texthline(
      yintercept = 25, color = RISEprimRed,
      linetype = 2, size = 2.5, alpha = 0.6,
      label = "Något förhöjd risk",
      hjust = 0.15
    ) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme_rise()
}

DIDareaPlot <- function(faktor) {
  plotFaktor <- faktor
  df.plot <- df %>%
    filter(Kommun == fokusKommun) %>%
    mutate(
      Risknivå = case_when(
        .data[[plotFaktor]] < rslimits |>
          select(all_of(plotFaktor)) |>
          slice(1) |>
          pull() ~ "Låg risk",
        .data[[plotFaktor]] >= rslimits |>
          select(all_of(plotFaktor)) |>
          slice(1) |>
          pull() &
          .data[[plotFaktor]] < rslimits |>
          select(all_of(plotFaktor)) |>
          slice(2) |>
          pull() ~ "Något förhöjd risk",
        .data[[plotFaktor]] >= rslimits |>
          select(all_of(plotFaktor)) |>
          slice(2) |>
          pull() ~ "Förhöjd risk",
        TRUE ~ "Otillräckliga svar"
      )
    )

  df.plot %>%
    filter(Kön %in% c("Pojke", "Flicka")) %>%
    mutate(Risknivå = factor(Risknivå, levels = c("Förhöjd risk", "Något förhöjd risk", "Låg risk", "Otillräckliga svar"))) %>%
    group_by(ar, Kön) %>%
    count(Risknivå, .drop = FALSE) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    ggplot(aes(x = ar, y = Andel)) +
    geom_area(aes(fill = Risknivå),
              position = "stack",
              alpha = 0.9
    ) +
    scale_fill_manual(values = DIDcolorsRYGG) +
    #scale_color_manual(values = c("#D55E00", "#F0E442", "#009E73", "lightgrey")) +
    geom_hline(yintercept = 90, color = "black", linetype = 2, linewidth = 0.66) +
    geom_hline(yintercept = 75, color = RISEprimRed, linetype = 2, linewidth = 0.66) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
    scale_x_continuous(breaks = årtal, guide = guide_axis(n.dodge = 2)) +
    xlab("Årtal") +
    ylab("Andel i %") +
    labs(
      title = paste0(plotFaktor, " - ", fokusKommun),
      subtitle = "Uppdelat på kön",
      caption = "Svart streckad linje = referensvärde för 10% med högst indexvärde.\n
      Röd linje = referensvärde för 25% med högst indexvärde.\n
                         Datakälla: Stockholmsenkäten."
    ) +
    facet_wrap(~Kön) +
    theme_minimal() +
    theme_rise() +
    theme(plot.caption = element_text(lineheight = 0.45))
}

DIDareaPlot2 <- function(faktor) {
  plotFaktor <- faktor
  df.plot <- df %>%
    filter(Kommun == fokusKommun) %>%
    mutate(
      Risknivå = case_when(
        .data[[plotFaktor]] < rslimits |>
          select(all_of(plotFaktor)) |>
          slice(1) |>
          pull() ~ "Låg risk",
        .data[[plotFaktor]] >= rslimits |>
          select(all_of(plotFaktor)) |>
          slice(1) |>
          pull() &
          .data[[plotFaktor]] < rslimits |>
          select(all_of(plotFaktor)) |>
          slice(2) |>
          pull() ~ "Något förhöjd risk",
        .data[[plotFaktor]] >= rslimits |>
          select(all_of(plotFaktor)) |>
          slice(2) |>
          pull() ~ "Förhöjd risk",
        TRUE ~ "Otillräckliga svar"
      )
    )

  df.plot %>%
    filter(Kön %in% c("Pojke", "Flicka")) %>%
    filter(!is.na(ARSKURS)) %>%
    mutate(Risknivå = factor(Risknivå, levels = c("Förhöjd risk", "Något förhöjd risk", "Låg risk", "Otillräckliga svar"))) %>%
    group_by(ar, Kön, ARSKURS) %>%
    count(Risknivå, .drop = FALSE) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    ggplot(aes(x = ar, y = Andel)) +
    geom_area(aes(fill = Risknivå),
              position = "stack",
              alpha = 0.9
    ) +
    scale_fill_manual(values = DIDcolorsRYGG) +
    #scale_color_manual(values = c("#D55E00", "#F0E442", "#009E73", "lightgrey")) +
    geom_hline(yintercept = 90, color = "black", linetype = 2, linewidth = 0.66) +
    geom_hline(yintercept = 75, color = RISEprimRed, linetype = 2, linewidth = 0.66) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
    scale_x_continuous(breaks = årtal, guide = guide_axis(n.dodge = 2)) +
    xlab("Årtal") +
    ylab("Andel i %") +
    labs(
      title = paste0(plotFaktor, " - ", fokusKommun),
      subtitle = "Uppdelat på kön och årskurs",
      caption = "Svart streckad linje = referensvärde för 10% med högst indexvärde.\n
      Röd linje = referensvärde för 25% med högst indexvärde.\n
                         Datakälla: Stockholmsenkäten."
    ) +
    facet_grid(ARSKURS~Kön) +
    theme_minimal() +
    theme_rise() +
    theme(plot.caption = element_text(lineheight = 0.45))
}


DIDradarPlot <- function(årtal) {
  year <- årtal
  df.plot <- sums.index %>%
    filter(Kommun %in% jmfKommun) %>%
    filter(Faktor %in% rfactors) %>%
    filter(Kön %in% c("Flicka", "Pojke")) %>%
    filter(År == year)

  ggplot(df.plot, aes(x = Faktor, y = Medel, group = Kommun, color = Kommun, fill = Kommun)) + # make plot, with area color
    geom_line(
      linewidth = 0.9,
      alpha = 0.6,
      linetype = 3
    ) +
    geom_point(size = 2.5, alpha = 0.6) +
    geom_line(data = filter(df.plot, Kommun == fokusKommun), alpha = 0.9) +
    geom_point(data = filter(df.plot, Kommun == fokusKommun), alpha = 0.9) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    see::coord_radar(theta = "x", start = 3, clip = "off") +
    scale_y_continuous(limits = c(-1.5, NA), expand = c(0, 0, 0, 0)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(
      title = paste0("Riskfaktorer ", year),
      subtitle = "Högre värde = större risk",
      y = "", x = "",
      caption = "Datakälla: Stockholmsenkäten"
    ) +
    facet_wrap(~Kön, nrow = 2) +
    theme_minimal() +
    theme_rise() +
    theme(legend.position = "right",
          axis.text.y = element_blank())
}

DIDkoladaPlot <- function(data) {
  data %>%
  ggplot(aes(x = År, y = Andel, group = Kommun, color = Kommun)) +
    geom_smooth(method = "loess",
                span = 8,
                aes(group = 1),
                alpha = 0.17,
                color = "darkblue",
                linewidth = 0,
                linetype = 2) +
    geom_line(data = filter({{data}}, Kommun %in% jmfKommun),
              alpha = 0.5, linewidth = 0.8, linetype = 3) +
    geom_point(data = filter({{data}}, Kommun %in% fokusKommun),
               alpha = 0.5, size = 2) +
    geom_line(data = filter({{data}}, Kommun == fokusKommun),
              alpha = 1) +
    geom_point(data = filter({{data}}, Kommun == fokusKommun),
               alpha = 1) +
    scale_x_continuous(guide = guide_axis(n.dodge = 2),
                       breaks = årtal) +
    #scale_y_continuous(limits = c(0, 100)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    ylab("Andel i %") +
    xlab("") +
    facet_wrap(~KPI,
               ncol = 2,
               scales = "free",
               labeller = labeller(KPI = label_wrap_gen(22))) +
    theme_rise() +
    theme(legend.position = "top") +
    labs(caption = "Ljusgrått fält visar 95% konfidensintervall för en oviktad trendlinje baserad på länets kommuner.\nDatakälla: Kolada")
}

DIDkoladaPlotG <- function(data) {
  data %>%
    mutate(Kön = fct_rev(Kön)) %>%
    filter(Kommun %in% jmfKommun,
           Kön %in% c("Flicka","Pojke")) %>%
    mutate(Kommun = factor(Kommun, levels = jmfKommun)) %>%
    ggplot(aes(x = År, y = Andel, group = Kön, color = Kön)) +
    geom_line(alpha = 0.9, linewidth = 0.8) +
    geom_point(alpha = 0.9, size = 1.6) +
    scale_x_continuous(guide = guide_axis(n.dodge = 2),
                       breaks = seq(2010, 2022, 2)) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_color_manual(values = RISEpalette1[c(1,5)])  +
    ylab("Andel i %") +
    xlab("") +
    facet_grid(Kommun~KPI,
               #scales = "free",
               labeller = labeller(KPI = label_wrap_gen(22))) +
    theme_rise(stripsize = 10) +
    theme(legend.position = "top") +
    labs(caption = "Datakälla: Kolada")
}

# Mean ~ Time -------------------------------------------------------------


DIDmedelSD <- function(faktor, xlim = c(-3,3)) {
  plotFaktor <- faktor
  df.plot <- sums.index %>%
    filter(Faktor == plotFaktor) %>%
    filter(Kommun %in% jmfKommun) %>%
    filter(Kön == "Flickor och pojkar") %>%
    filter(!År < 2006) %>%
    mutate(År = as.factor(År))

  ggplot(df.plot, aes(x = År, y = Medel, group = Kommun, color = Kommun, fill = Kommun)) + # make plot, with area color
    geom_line(linewidth = 0.9,
              alpha = 0.7) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_line(data = filter(df.plot, Kommun == fokusKommun), alpha = 1) +
    geom_point(data = filter(df.plot, Kommun == fokusKommun), alpha = 1) +
    geom_ribbon(aes(ymin = sd.lo, ymax = sd.hi),
                alpha = 0.1, linetype = 0
    ) +
    scale_y_continuous(limits = xlim) +
    scale_x_discrete(guide = guide_axis(n.dodge = 1)) +

    labs(title = "Medelvärde över tid",
         subtitle = "Skuggat fält indikerar en standardavvikelse (~ 68%)",
         caption = "Datakälla: Stockholmsenkäten") +
    xlab("Årtal") +
    ylab(paste0(plotFaktor)) +
    theme_minimal() +
    theme_rise() +
    theme(text = element_text(family = "Lato"),
          axis.text.x = element_text(size = ax.size),
          axis.text.y = element_text(size = ax.size),
          title = element_text(size = title.size),
          legend.text = element_text(size = legend.size),
          strip.text.x = element_text(size = 13),
          panel.spacing = unit(pandist, "cm", data = NULL)
    )
}

DIDmedelSDg <- function(data,faktor, xlim = c(-2,2.5)) {
  plotFaktor <- faktor
  df.plot <- data %>%
    filter(Faktor == plotFaktor) %>%
    filter(Kommun %in% jmfKommun) %>%
    filter(Kön %in% c("Flicka", "Pojke")) %>%
    filter(!År < 2006) %>%
    mutate(År = as.factor(År))

  ggplot(df.plot, aes(x = År, y = Medel, group = Kommun, color = Kommun, fill = Kommun)) + # make plot, with area color
    geom_line(linewidth = 1,
              alpha = 0.6,
              linetype = 3) +
    geom_point(size = 2.5, alpha = 0.6) +
    geom_line(data = filter(df.plot, Kommun == fokusKommun),
              alpha = 0.9, linewidth = 1.2,) +
    geom_point(data = filter(df.plot, Kommun == fokusKommun),
               alpha = 0.9, size = 3) +
    geom_ribbon(aes(ymin = sd.lo, ymax = sd.hi),
                alpha = 0.05, linetype = 0
    ) +
    scale_y_continuous(limits = xlim) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_brewer(type = "qual", palette = "Dark2",
                       aesthetics = c("color","fill")) +
    labs(title = "Medelvärde över tid",
         subtitle = "Skuggat fält indikerar en standardavvikelse (~ 68%)") +
    xlab("Årtal") +
    ylab(paste0(plotFaktor)) +
    theme_minimal() +
    theme_rise()
}

# font settings for interactive plots with library(ggiraph)
set_girafe_defaults(
  fonts = list(sans = "Lato",
               serif = "Lato",
               mono = "Lato"))
init_girafe_defaults()

# DIDline90old <- function(faktor){
#   plotFaktor <- faktor
#   df.plot <- sums.index %>%
#     filter(Faktor == plotFaktor) %>%
#     filter(Kommun %in% jmfKommun) %>%
#     filter(Kön %in% c("Flicka", "Pojke")) %>%
#     filter(!År < 2006) %>%
#     mutate(År = as.factor(År))
# #   didline90 <-
#   ggplot(df.plot, aes(x = År, y = n.90, group = Kön, color = Kön, tooltip = n)) + # make plot, with area color
#     geom_line(linewidth = 1) +
#     geom_point(size = 3) +
#     #geom_point_interactive(size = 3) +
#     scale_y_continuous(limits = c(0, 30)) +
#     scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#     scale_color_manual(values = RISEpalette1[c(1,5)]) +
#     geom_hline(yintercept = 10, color = "darkgrey", linetype = 2, linewidth = 0.4, alpha = 0.7) +
#     ggtitle("Andel i grupp \"förhöjd risk\"") +
#     xlab("") +
#     ylab(paste0(plotFaktor)) +
#     labs(caption = "Datakälla: Stockholmsenkäten.") +
#     facet_wrap(~Kommun, labeller = labeller(Kommun = label_wrap_gen(12))) +
#     theme_minimal() +
#     theme_rise()
#
#   #girafe(ggobj = didline90)
# }

DIDline90 <- function(faktor){

  plotFaktor <- faktor

  df.plot <- df.risk.gender %>%
    filter(Index == plotFaktor) %>%
    filter(Kommun %in% jmfKommun) %>%
    filter(Kön %in% c("Flicka", "Pojke")) %>%
    filter(riskLevel == "Förhöjd risk")

  ggplot(df.plot, aes(x = År, y = Andel, group = Kön, color = Kön)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    #geom_point_interactive(aes(tooltip = Andel), size = 3) +
    scale_y_continuous(limits = c(0, 30)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_manual(values = RISEpalette1[c(1,5)]) +
    geom_hline(yintercept = 10, color = "darkgrey", linetype = 2, linewidth = 0.4, alpha = 0.7) +
    ggtitle("Andel i grupp \"förhöjd risk\"") +
    xlab("") +
    ylab(paste0(plotFaktor)) +
    labs(subtitle = "Åk 9 och Gy 2 tillsammans",
         caption = "Datakälla: Stockholmsenkäten.") +
    facet_wrap(~Kommun, labeller = labeller(Kommun = label_wrap_gen(12))) +
    theme_minimal() +
    theme_rise()

  #girafe(ggobj = didline90)
}

DIDline90åk <- function(faktor){

  plotFaktor <- faktor

  df.plot <- df.risk.gender.arskurs %>%
    filter(Index == plotFaktor) %>%
    filter(Kommun %in% jmfKommun) %>%
    filter(Kön %in% c("Flicka", "Pojke")) %>%
    filter(riskLevel == "Förhöjd risk") %>%
    filter(!is.na(Årskurs))

  ggplot(df.plot, aes(x = År, y = Andel, group = Kön, color = Kön)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    #geom_point_interactive(aes(tooltip = Andel), size = 3) +
    scale_y_continuous(limits = c(0, 30)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_manual(values = RISEpalette1[c(1,5)]) +
    geom_hline(yintercept = 10, color = "darkgrey", linetype = 2, linewidth = 0.4, alpha = 0.7) +
    ggtitle("Andel i grupp \"förhöjd risk\"") +
    xlab("") +
    ylab(paste0(plotFaktor)) +
    labs(caption = "Datakälla: Stockholmsenkäten.") +
    facet_grid(Årskurs~Kommun, labeller = labeller(Kommun = label_wrap_gen(12))) +
    theme_minimal() +
    theme_rise()

  #girafe(ggobj = didline90)
}

# Demografi ---------------------------------------------------------------

stapelDemografi <- function(demografi) {
  demogr <- demografi
  df %>%
    select(demogr,ar,Kommun) %>%
    filter(Kommun %in% fokusKommun) %>%
    group_by(ar,Kommun) %>%
    pivot_longer(demogr) %>%
    count(name, value) %>%
    mutate(percent = (100 * n / sum(n)) %>% round(digits = 3)) %>%
    mutate(proportion = (n/sum(n) %>% round(digits = 3))) %>%
    mutate(sem = sqrt(proportion*(1-proportion)/sum(n))) %>%
    mutate(lower.95ci = proportion - sem*1.96,
           upper.95ci = proportion + sem*1.96
    ) %>%
    rename(Svarsalternativ = value,
           'Antal svar' = n,
           Procent = percent) %>%
    mutate(Svarsalternativ = car::recode(Svarsalternativ,"NA='Svar saknas'")) %>%
    mutate(Procent = round(Procent,1)) %>%
    mutate(procentText = sprintf("%1.1f%%", Procent)) %>%
    mutate(År = factor(ar)) %>%
    ggplot(aes(x = Svarsalternativ, y = Procent, fill = År, group = År)) +
    geom_bar(position=position_dodge(), stat = 'identity') +
    geom_text(aes(label = .data$'Antal svar'),
              position = position_dodge(width = 0.9),
              hjust = -0.22, vjust = 0.5, angle = 90, size = 2.7,
              color = "darkgrey") +
    scale_fill_viridis_d(begin = 0.3, end = 0.9, option = 7) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20)) +
    theme_minimal() +
    theme_rise() +
    labs(title = str_wrap(paste0(demogr)),
         subtitle = fokusKommun,
         caption = "Siffrorna ovanför staplarna anger antalet respondenter i varje svarskategori.\n
         Datakälla: Stockholmsenkäten.")
}


# ANDTS specific ----------------------------------------------------------

# for FNY12020r and F14r (e-cig and smoking)

# ta fram ett indexvärde för bruk under senaste 4v
senaste4v <- c("F14r","F18r","F36new","F49new","FNY12020r")
df <- df %>%
  mutate(Senaste4v = rowSums(df %>% select(all_of(senaste4v)), na.rm = T))

itemlabelsPlotFaktor <- cbind(senaste4v,
                              c("Rökning","Snus","Alkohol","Narkotika (inkl. cannabis)","E-cigaretter")) %>%
  as.data.frame() %>%
  rename(itemnr = senaste4v,
         item = V2)


andtsUseShare <- function(andts) {
  plotFaktor <- andts
  plotFaktorName <- itemlabelsPlotFaktor %>%
    filter(itemnr == plotFaktor) %>%
    pull(item)

  df %>%
    filter(Kön %in% c("Pojke", "Flicka"),
           !is.na(ARSKURS),
           Kommun == fokusKommun) %>%
    select(all_of(senaste4v), ar, Kön, ARSKURS) %>%
    mutate(
      bruk4v = case_when(
        .data[[plotFaktor]] == 0 ~ "Ej använt",
        .data[[plotFaktor]] == 1 ~ "Ibland men inte varje dag",
        .data[[plotFaktor]] == 2 ~ "Dagligen",
        TRUE ~ "Svar saknas"
      )
    ) %>%
    mutate(bruk4v = factor(bruk4v, levels = c("Dagligen","Ibland men inte varje dag","Ej använt","Svar saknas"))) %>%
    group_by(ar, Kön, ARSKURS) %>%
    count(bruk4v, .drop = FALSE) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    ggplot(aes(x = ar, y = Andel)) +
    geom_area(aes(fill = bruk4v),
              position = "stack",
              alpha = 0.9
    ) +
    scale_fill_viridis_d('Bruk senaste 4 veckorna') +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
    scale_x_continuous(breaks = årtal, guide = guide_axis(n.dodge = 2)) +
    xlab("Årtal") +
    ylab("Andel i %") +
    theme_minimal() +
    theme_rise() +
    labs(
      title = paste0(plotFaktorName),
      subtitle = fokusKommun,
      caption = "Datakälla: Stockholmsenkäten."
    ) +
    facet_grid(ARSKURS ~ Kön) +
    theme(legend.background = element_rect(color = "lightgrey"))
}

andtsUseShare2 <- function(andts) {
  plotFaktor <- andts
  plotFaktorName <- itemlabelsPlotFaktor %>%
    filter(itemnr == plotFaktor) %>%
    pull(item)

  df %>%
    filter(Kön %in% c("Pojke", "Flicka"),
           !is.na(ARSKURS),
           Kommun == fokusKommun) %>%
    select(all_of(senaste4v), ar, Kön, ARSKURS) %>%
    mutate(
      bruk4v = case_when(
        .data[[plotFaktor]] == 0 ~ "Ej använt",
        .data[[plotFaktor]] == 1 ~ "En gång",
        .data[[plotFaktor]] == 2 ~ "Två gånger",
        .data[[plotFaktor]] == 3 ~ "Tre gånger eller fler",
        TRUE ~ "Svar saknas"
      )
    ) %>%
    mutate(bruk4v = factor(bruk4v, levels = c("Tre gånger eller fler","Två gånger","En gång","Ej använt","Svar saknas"))) %>%
    group_by(ar, Kön, ARSKURS) %>%
    count(bruk4v, .drop = FALSE) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    ggplot(aes(x = ar, y = Andel)) +
    geom_area(aes(fill = bruk4v),
              position = "stack",
              alpha = 0.85
    ) +
    scale_fill_viridis_d('Bruk senaste 4 veckorna') +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
    scale_x_continuous(breaks = årtal, guide = guide_axis(n.dodge = 2)) +
    xlab("Årtal") +
    ylab("Andel i %") +
    theme_minimal() +
    theme_rise() +
    labs(
      title = paste0(plotFaktorName, ", användning senaste 4 veckorna"),
      subtitle = fokusKommun,
      caption = "Datakälla: Stockholmsenkäten."
    ) +
    facet_grid(ARSKURS ~ Kön) +
    theme(legend.background = element_rect(color = "lightgrey"))
}


# Mobbning ----------------------------------------------------------------

DIDmobbadAlla <- function(year) {

  mobbadAlla <- df %>%
    filter(Kommun == fokusKommun,
           ar == year) %>%
    count(mobbad, .drop = F) %>%
    mutate(Andel = 100*n/sum(n)) %>%
    mutate(mobbad = fct_reorder(mobbad, desc(n)))

  nejMedel <- mobbadAlla %>%
    filter(mobbad == 'Nej') %>%
    pull(Andel) %>%
    round(1)

  df %>%
    filter(Kommun == fokusKommun,
           ar == year,
           Kön %in% c("Pojke","Flicka")) %>%
    drop_na(mobbad) %>%
    group_by(Kön) %>%
    count(mobbad, .drop = F) %>%
    mutate(Andel = 100*n/sum(n)) %>%
    mutate(mobbad = fct_reorder(mobbad, desc(n))) %>%
    ggplot(data = .,
           aes(x = mobbad, y = Andel, fill = Kön)) +
    geom_bar(position=position_dodge(),
             stat = 'identity') +
    scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
    scale_x_discrete(labels = ~ stringr::str_wrap(.x, width = 8)) +
    scale_color_manual(values = RISEpalette1[c(1,5)],
                       aesthetics = c("fill","color")) +
    theme_minimal() +
    theme_rise() +
    labs(title = "Har du känt dig mobbad eller trakasserad i skolan det här läsåret?",
         subtitle = glue("{year}, både åk 9 och gy 2."),
         caption = "Medelvärde inkluderar alla svar, oavsett angivet kön. Datakälla: Stockholmsenkäten") +
    ylab("Andel i %") +
    xlab("") +
    geom_text(aes(label = round(Andel,1)),
              position = position_dodge(width = 0.9),
              hjust = -0.22, vjust = 0.5, angle = 90, size = 2.7,
              color = "darkgrey") +
    annotate("text", y = 95, x = 3,
             label = paste0("Medelvärde ",nejMedel,"%"),
             color = "black") +
    geom_curve(x = 3, y = 91,
               xend = 1, yend = nejMedel,
               color = "black",
               curvature = -0.4,
               arrow = arrow())
}

DIDmobbadÅK <- function(year, årskurs) {

  mobbadAlla <- df %>%
    filter(Kommun == fokusKommun,
           ar == year) %>%
    count(mobbad, .drop = F) %>%
    mutate(Andel = 100*n/sum(n)) %>%
    mutate(mobbad = fct_reorder(mobbad, desc(n)))

  nejMedel <- df %>%
    filter(Kommun == fokusKommun,
           ar == year,
           ARSKURS == årskurs) %>%
    count(mobbad, .drop = F) %>%
    mutate(Andel = 100*n/sum(n)) %>%
    filter(mobbad == 'Nej') %>%
    pull(Andel) %>%
    round(1)

  df %>%
    filter(Kommun == fokusKommun,
           ar == year,
           ARSKURS == årskurs,
           Kön %in% c("Pojke","Flicka")
           ) %>%
    drop_na(mobbad) %>%
    group_by(Kön) %>%
    count(mobbad, .drop = F) %>%
    mutate(Andel = 100*n/sum(n)) %>%
    mutate(mobbad = fct_reorder(mobbad, desc(n))) %>%
    ggplot(data = .,
           aes(x = mobbad, y = Andel, fill = Kön)) +
    geom_bar(position=position_dodge(),
             stat = 'identity') +
    scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
    scale_x_discrete(labels = ~ stringr::str_wrap(.x, width = 8)) +
    scale_color_manual(values = RISEpalette1[c(1,5)],
                       aesthetics = c("fill","color")) +
    theme_minimal() +
    theme_rise() +
    labs(title = "Har du känt dig mobbad eller trakasserad i skolan det här läsåret?",
         subtitle = glue("{year}, endast svar från {årskurs}."),
         caption = "Medelvärde inkluderar alla svar, oavsett angivet kön. Datakälla: Stockholmsenkäten") +
    ylab("Andel i %") +
    xlab("") +
    geom_text(aes(label = round(Andel,1)),
              position = position_dodge(width = 0.9),
              hjust = -0.22, vjust = 0.5, angle = 90, size = 2.7,
              color = "darkgrey") +
    annotate("text", y = 95, x = 3,
             label = paste0("Medelvärde ",nejMedel,"%"),
             color = "black") +
    geom_curve(x = 3, y = 91,
               xend = 1, yend = nejMedel,
               color = "black",
               curvature = -0.4,
               arrow = arrow())
}



# RSrapportfigurer --------------------------------------------------------

RSfigur <- function(kontext, rs = "Riskfaktor") {
  lst.kontext <- subset(lst.data, Kontext == kontext & RSfaktor == rs)
  # koden nedan är lånad från nedanstående källor, och modifierad:
  # https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram
  # https://medium.com/@emtiazahmed.cs/sankey-diagram-step-by-step-using-r-b3e7bea53224
  # https://christophergandrud.github.io/networkD3/

  # extrahera vektorer med unika Spetsar & RS-faktorer
  spetsar <- lst.kontext %>%
    distinct(Spets) %>%
    dplyr::rename(label = Spets)
  faktorer <- lst.kontext %>%
    distinct(Faktor) %>%
    dplyr::rename(label = Faktor)

  # sammanfoga dem
  rsfaktorer <- full_join(faktorer, spetsar, by = "label")
  rsfaktorer <- rsfaktorer %>% rowid_to_column("id")

  # skapa table som visar hur många gånger varje rsfaktor kopplas till en spets
  per_route <- lst.kontext %>%
    group_by(Faktor, Spets) %>%
    dplyr::summarise(count = n()) %>%
    ungroup()

  # ta fram variabler för nätverksmodeller och liknande visualisering
  edges <- per_route %>%
    left_join(rsfaktorer, by = c("Faktor" = "label")) %>%
    dplyr::rename(from = id)
  edges <- edges %>%
    left_join(rsfaktorer, by = c("Spets" = "label")) %>%
    dplyr::rename(to = id)

  edges <- select(edges, from, to, count)
  edges <- mutate(edges, width = count + 1)
  nodes_d3 <- mutate(rsfaktorer, id = id - 1)
  edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
  edges_d3$group <- per_route$Spets
  nodes_d3$nodecolor <- c("allsamecolor")

  # färgsättning av flöden (edges) utifrån spetsarna och rätblocken intill faktorer & spetsar i diagrammet
  # kulörer lånade bl.a. från http://opencolor.tools/palettes/wesanderson/
  my_color <- 'd3.scaleOrdinal() .domain(["Psyk. ohälsa","Utanförskap","Våld","Kriminalitet","Missbruk/ANDTS", "allsamecolor"])
              .range(["lightblue", "#F5CDB6", "#F7B0AA", "#FDDDA4", "#76A08A", "#FCD16B"])'
  # färgkod #FCD16B för skyddsfaktorer (förvalt i koden ovanför) och #D8A49B för riskfaktorer
  # ändra "#FCD16B" på rad 91 till "#D8A49B" för att byta färg på rätblocken

  # skapa ett interaktivt Sankey-diagram där spetsarna i preventionsstjärnan finns till höger.
  sankeyNetwork(
    Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
    NodeID = "label", Value = "count", fontSize = 20, unit = "Antal",
    fontFamily = "sans-serif", LinkGroup = "group", colourScale = my_color,
    nodeWidth = 13, NodeGroup = "nodecolor", nodePadding = 18
  )

}


# Skolinspektionen single item --------------------------------------------
# other possible values of svarskategorier are: "viss", "inte", "vetinte"

DIDskolinsp <- function(item, årskurs, svarskategorier = c("helt", "stor")) {
  df.si.long %>%
    filter(
      item == {{item}},
      Årskurs == {{årskurs}}
    ) %>%
    filter(svarskategori %in% {{svarskategorier}}) %>%
    group_by(Kommun, Årskurs, item, Svarsfrekvens) %>%
    summarise(Andel = sum(Andel, na.rm = T)) %>%
    ungroup() %>%
    mutate(Kommun = fct_reorder(Kommun, Andel)) %>%

    ggplot(aes(x = Kommun, y = Andel)) +
    geom_col(aes(fill = Kommun)) +
    geom_col(data = . %>%
               filter(Kommun == fokusKommun),
             color = "black",
             fill = "darkgrey") +
    geom_col(data = . %>%
               filter(Kommun %in% jmfKommun),
             color = "darkgrey",
             fill = "lightgrey") +
    geom_hline(aes(yintercept = mean(Andel, na.rm = T)),
               linetype = 3,
               color = "#D55E00",
               linewidth = 0.9,
               alpha = 0.7
    ) +
    geom_text(aes(label = paste0(round(mean(Andel, na.rm = T),1),"%"),
                  y = mean(Andel, na.rm = T)+3),
              x = 1,
              color = "#D55E00",
              alpha = 0.7) +
    geom_text(aes(label = paste0(round(Andel,1),"%")),
              position = position_dodge(width = 0.9),
              hjust = 0, vjust = -0.35, angle = 45, size = 2.9,
              color = "black") +
    geom_text(aes(label = paste0(Svarsfrekvens,"%"),
                  y = 1,
                  angle = 0),
              position = position_dodge(width = 1),
              hjust = 0.5,
              vjust = 0,
              size = 2.4,
              color = "white"
    ) +
    scale_fill_viridis_d(aesthetics = c("color","fill"),
                         guide = "none") +
    theme_rise() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
    labs(title = paste0("Skolinspektionen - ",årskurs," - 2022"),
         subtitle = glue("Andel respondenter som svarat positivt på frågan om '{item}'"),
         caption = "Siffror längst ner i kolumnen indikerar svarsfrekvensen.\nKälla: Skolinspektionens skolenkät") +
    coord_cartesian(clip = "off")
}

# DIDskolinsp("trygghet","Åk 8", c("viss","inte")) + labs(subtitle = glue("Andel respondenter som svarat negativt på frågan om trygghet"))


DIDskolinspG <- function(KPI,kön) {
  df.si.kolada %>%
    filter(
      KPI == {{KPI}},
      Kön == kön
    ) %>%
    drop_na(Andel) %>%
    mutate(Kommun = fct_reorder(Kommun, Andel)) %>%

    ggplot(aes(x = Kommun, y = Andel)) +
    geom_col(aes(fill = Kommun)) +
    geom_col(data = . %>%
               filter(Kommun == fokusKommun),
             color = "black",
             fill = "darkgrey") +
    geom_col(data = . %>%
               filter(Kommun %in% jmfKommun),
             color = "darkgrey",
             fill = "lightgrey") +
    geom_hline(aes(yintercept = mean(Andel, na.rm = T)),
               linetype = 3,
               color = "#D55E00",
               linewidth = 0.9,
               alpha = 0.7
    ) +
    geom_text(aes(label = paste0(round(mean(Andel, na.rm = T),1),"%"),
                  y = mean(Andel, na.rm = T)+3),
              x = 1,
              color = "#D55E00",
              alpha = 0.7) +
    geom_text(aes(label = paste0(round(Andel,1),"%")),
              position = position_dodge(width = 0.9),
              hjust = 0, vjust = -0.35, angle = 45, size = 2.9,
              color = "black") +
    scale_fill_viridis_d(aesthetics = c("color","fill"),
                         guide = "none") +
    theme_rise() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
    labs(title = str_wrap(KPI,60),
         subtitle = paste0(kön,", År 2022"),
         caption = "Källa: Skolinspektionens skolenkät") +
    coord_cartesian(clip = "off")
} # Kön recoded to either "Flickor" or "Pojkar".

