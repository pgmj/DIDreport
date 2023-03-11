DIDsnirkel <- function(årtal) {
  year <- årtal
  df.risk %>%
    #filter(!riskLevel == "NA") %>%
    filter(Kommun == fokusKommun) %>%
    filter(År == year) %>%
    filter(!Index %in% c("Välbefinnande", "Positiv skolanknytning")) %>%
    mutate(riskLevel = car::recode(riskLevel,"NA='För få svar';
                                   'Medelhög risk'='Något förhöjd risk';
                                   'Hög risk'='Förhöjd risk'")) %>%
    mutate(Risknivå = factor(riskLevel, levels = c("För få svar", "Låg risk", "Något förhöjd risk", "Förhöjd risk"))) %>%
    ggplot(aes(x = Index, y = Andel, fill = Risknivå)) +
    geom_col() +
    geom_textpath(aes(label = Index, group = Index),
                  text_only = T,
                  position = "stack",
                  hjust = 0,
                  size = 4
    ) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = lighten(c("lightgrey","#009E73", "#F0E442", "#D55E00"),
                                       amount = 0.1, space = "HLS")) +
    theme_bw() +
    scale_x_discrete(
      expand = expansion(add = c(3, 0)),
      limits = rev,
      labels = NULL
    ) +
    scale_y_continuous(
      breaks = seq(0, 90, 10),
      labels = paste0(seq(0, 90, 10), "%"),
      limits = c(0, 100)
    ) +
    labs(title = paste0(fokusKommun, " - ", year)) +
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
    theme(
      axis.text.x = element_text(size = ax.size),
      axis.text.y = element_text(size = ax.size),
      title = element_text(size = title.size),
      legend.text = element_text(size = legend.size),
      strip.text.x = element_text(size = stript.size),
      panel.spacing = unit(pandist, "cm", data = NULL)
    ) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme_rise() +
    theme(text = element_text(family = "Lato"))
}

DIDstapel <- function(årtal) {
  year <- årtal
  df.risk %>%
    #filter(!riskLevel == "NA") %>%
    filter(Kommun == fokusKommun) %>%
    filter(År == year) %>%
    filter(!Index %in% c("Välbefinnande", "Positiv skolanknytning")) %>%
    mutate(riskLevel = car::recode(riskLevel,"NA='För få svar';
                                   'Medelhög risk'='Något förhöjd risk';
                                   'Hög risk'='Förhöjd risk'")) %>%
    mutate(Risknivå = factor(riskLevel, levels = c("För få svar", "Låg risk", "Något förhöjd risk", "Förhöjd risk"))) %>%
    ggplot(aes(x = Index, y = Andel, fill = Risknivå)) +
    geom_col() +
    geom_textpath(aes(label = Index, group = Index),
                  text_only = T,
                  position = "stack",
                  hjust = 0,
                  size = 4
    ) +
    #coord_polar(theta = "y") +
    scale_fill_manual(values = lighten(c("lightgrey","#009E73", "#F0E442", "#D55E00"),
                                       amount = 0.1, space = "HLS")) +
    theme_bw() +
    scale_x_discrete(
      #expand = expansion(add = c(3, 0)),
      limits = rev,
      labels = NULL
    ) +
    scale_y_continuous(
      breaks = seq(0, 90, 10),
      labels = paste0(seq(0, 90, 10), "%"),
      limits = c(0, 100)
    ) +
    labs(title = paste0(fokusKommun, " - ", year)) +
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
    theme(
      axis.text.x = element_text(size = ax.size),
      axis.text.y = element_text(size = ax.size),
      title = element_text(size = title.size),
      legend.text = element_text(size = legend.size),
      strip.text.x = element_text(size = stript.size),
      panel.spacing = unit(pandist, "cm", data = NULL)
    ) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme_rise() +
    theme(text = element_text(family = "Lato"))
}

DIDareaPlot <- function(faktor) {
  plotFaktor <- faktor
  df.plot <- df %>%
    filter(Kommun == fokusKommun) %>%
    mutate(
      Risknivå = case_when(
        .data[[plotFaktor]] < rslimits |>
          select(plotFaktor) |>
          slice(1) |>
          pull() ~ "Låg risk",
        .data[[plotFaktor]] >= rslimits |>
          select(plotFaktor) |>
          slice(1) |>
          pull() &
          .data[[plotFaktor]] < rslimits |>
          select(plotFaktor) |>
          slice(2) |>
          pull() ~ "Något förhöjd risk",
        .data[[plotFaktor]] >= rslimits |>
          select(plotFaktor) |>
          slice(2) |>
          pull() ~ "Förhöjd risk",
        TRUE ~ "Otillräckliga svar"
      )
    )

  df.plot %>%
    filter(Kön %in% c("Pojke", "Flicka")) %>%
    # filter(ar %in% input$years0) %>% # allow selection of span of years?
    mutate(Risknivå = factor(Risknivå, levels = c("Förhöjd risk", "Något förhöjd risk", "Låg risk", "Otillräckliga svar"))) %>%
    group_by(ar, Kön) %>%
    count(Risknivå, .drop = FALSE) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    ggplot(aes(x = ar, y = Andel)) +
    geom_area(aes(fill = Risknivå),
              position = "stack",
              alpha = 0.85
    ) +
    scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "lightgrey")) +
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
      caption = str_wrap("Svart streckad linje = referensvärde för 10% med högst risk.
      Röd linje = referensvärde för 25% med högst risk",
                         width = 60
      )
    ) +
    facet_wrap(~Kön) +
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

DIDareaPlot2 <- function(faktor) {
  plotFaktor <- faktor
  df.plot <- df %>%
    filter(Kommun == fokusKommun) %>%
    mutate(
      Risknivå = case_when(
        .data[[plotFaktor]] < rslimits |>
          select(plotFaktor) |>
          slice(1) |>
          pull() ~ "Låg risk",
        .data[[plotFaktor]] >= rslimits |>
          select(plotFaktor) |>
          slice(1) |>
          pull() &
          .data[[plotFaktor]] < rslimits |>
          select(plotFaktor) |>
          slice(2) |>
          pull() ~ "Något förhöjd risk",
        .data[[plotFaktor]] >= rslimits |>
          select(plotFaktor) |>
          slice(2) |>
          pull() ~ "Förhöjd risk",
        TRUE ~ "Otillräckliga svar"
      )
    )

  df.plot %>%
    filter(Kön %in% c("Pojke", "Flicka")) %>%
    # filter(ar %in% input$years0) %>% # allow selection of span of years?
    mutate(Risknivå = factor(Risknivå, levels = c("Förhöjd risk", "Något förhöjd risk", "Låg risk", "Otillräckliga svar"))) %>%
    group_by(ar, Kön, ARSKURS) %>%
    count(Risknivå, .drop = FALSE) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    ggplot(aes(x = ar, y = Andel)) +
    geom_area(aes(fill = Risknivå),
              position = "stack",
              alpha = 0.85
    ) +
    scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "lightgrey")) +
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
      caption = str_wrap("Svart streckad linje = referensvärde för 10% med högst risk.
      Röd linje = referensvärde för 25% med högst risk",
                         width = 60
      )
    ) +
    facet_grid(ARSKURS~Kön) +
    theme_minimal() +
    theme_rise() +
    theme(text = element_text(family = "Lato"),
          axis.text.x = element_text(size = ax.size),
          axis.text.y = element_text(size = ax.size),
          title = element_text(size = title.size),
          legend.text = element_text(size = legend.size),
          strip.text = element_text(size = 11),
          panel.spacing = unit(pandist, "cm", data = NULL)
    )
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
      alpha = 0.7
    ) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_line(data = filter(df.plot, Kommun == fokusKommun), alpha = 1) +
    geom_point(data = filter(df.plot, Kommun == fokusKommun), alpha = 1) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    see::coord_radar(theta = "x", start = 3, clip = "off") +
    scale_y_continuous(limits = c(-3, NA), expand = c(0, 0, 0, 0)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(
      title = paste0("Riskfaktorer ", year),
      subtitle = "Högre värde = större risk",
      y = "", x = ""
    ) +
    facet_wrap(~Kön) +
    theme_rise() +
    theme(text = element_text(family = "Lato"),
          axis.text.x = element_text(size = ax.size),
          axis.text.y = element_blank(),
          title = element_text(size = title.size),
          legend.text = element_text(size = legend.size),
          strip.text.x = element_text(size = 14),
          strip.background = element_rect(fill = RISEprimYellowLight),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey", linetype = "dotted"),
          panel.grid.minor = element_line(color = "grey", linewidth = 2),
          panel.spacing = unit(pandist, "cm", data = NULL)
    )
}

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

    labs(title = "Medelvärde över tid", subtitle = "Skuggat fält indikerar en standardavvikelse (~ 68%)") +
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

DIDmedelSDg <- function(faktor, xlim = c(-3,3)) {
  plotFaktor <- faktor
  df.plot <- sums.index %>%
    filter(Faktor == plotFaktor) %>%
    filter(Kommun %in% jmfKommun) %>%
    filter(Kön %in% c("Flicka", "Pojke")) %>%
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
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(title = "Medelvärde över tid", subtitle = "Skuggat fält indikerar en standardavvikelse (~ 68%)") +
    xlab("Årtal") +
    ylab(paste0(plotFaktor)) +
    theme_minimal() +
    theme_rise() +
    facet_wrap(~Kön) +
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

DIDline90 <- function(faktor){
  plotFaktor <- faktor
  df.plot <- sums.index %>%
    filter(Faktor == plotFaktor) %>%
    filter(Kommun %in% jmfKommun) %>%
    filter(Kön %in% c("Flicka", "Pojke")) %>%
    filter(!År < 2006) %>%
    mutate(År = as.factor(År))

  ggplot(df.plot, aes(x = År, y = n.90, group = Kön, color = Kön, tooltip = n, data_id = n)) + # make plot, with area color
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 30)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    #geom_hline(yintercept = 10, color = RISEprimRed, linetype = 2, linewidth = 0.4) +
    ggtitle("Procent över percentil 90, per år") +
    xlab("") +
    ylab(paste0(plotFaktor)) +
    facet_wrap(~Kommun, labeller = labeller(Kommun = label_wrap_gen(12))) +
    theme_minimal(base_family = "Lato") +
    theme_rise() +
    theme(
      axis.text.x = element_text(size = ax.size),
      axis.text.y = element_text(size = ax.size),
      title = element_text(size = title.size),
      legend.text = element_text(size = legend.size),
      strip.text.x = element_text(size = stript.size+1),
      panel.spacing = unit(pandist, "cm", data = NULL)
    )
}
