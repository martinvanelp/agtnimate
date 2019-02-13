library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(cbsodataR)

library(randomcoloR)
library(ggthemes)

bbp_meta <- cbs_get_meta("84105NED")
bbp      <- cbs_get_data("84105NED")

bbp_meta_key <- 
    bbp_meta$DataProperties %>%
    select(Key, Title, ParentID) %>%
    full_join(bbp_meta$DataProperties %>%
                  select(ParentKey = Key, ParentTitle = Title, ID),
              by = c("ParentID" = "ID")) %>%
    mutate(Title = ifelse(Title == "Totaal", ParentTitle, Title)) %>%
    select(Key, Title) %>%
    na.omit()

bbp_bt <- bbp %>%
    select(SoortGegevens,
           Perioden, 
           Totaal_18:BrutoBinnenlandsProduct_63)

bbp_bt_jj <- bbp_bt %>%
    filter(grepl("JJ", Perioden)) %>%
    mutate(Jaar = as.numeric(substr(Perioden, 0, 4)))

bbp_bt_jj_wp <- bbp_bt_jj %>%
    filter(SoortGegevens == "A045297")

bbp_bt_jj_wp_tidy <- bbp_bt_jj_wp %>%
    select(-SoortGegevens, -Perioden) %>%
    gather(Key, Waarde,
           Totaal_18:BrutoBinnenlandsProduct_63) %>%
    left_join(bbp_meta_key,
              by = "Key")

taart_bt_selectie <-
    c("ALandbouwBosbouwEnVisserij_20",
      "BDelfstoffenwinning_22",
      "Totaal_23",
      "DEnergievoorziening_35",
      "EWaterbedrijvenEnAfvalbeheer_36",
      "FBouwnijverheid_37",
      "GHandel_40",
      "HVervoerEnOpslag_41",
      "IHoreca_42",
      "JInformatieEnCommunicatie_43",
      "KFinancieleDienstverlening_44",
      "LVerhuurEnHandelVanOnroerendGoed_45",
      "Totaal_46",
      "Totaal_55",
      "QGezondheidsEnWelzijnszorg_58",
      "RUCultuurRecreatieOverigeDiensten_59")

taart_bt <- bbp_bt_jj_wp_tidy %>%
    #filter(Jaar %in% 1995:2000) %>%
    filter(Key %in% taart_bt_selectie) %>%
    rename(Bedrijfstak = Title)

taart_bt_pc <- taart_bt %>%
    group_by(Jaar) %>%
    mutate(Percentage = Waarde / sum(Waarde))


# https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
set.seed(1)
palet_bt <- distinctColorPalette(16)

# https://www.displayr.com/how-to-make-a-pie-chart-in-r/
plot_taart_bt <- taart_bt_pc %>% 
    
    # plot
    ggplot(aes(x = 1,
               y = Percentage,
               fill = Bedrijfstak)) +
    geom_bar(stat="identity") + 
    coord_polar(theta = "y", start = 0, direction = -1) +
    geom_text(aes(
        label = paste0(round(Percentage * 100), "%")), 
        position = position_stack(vjust = 0.5),
        size = 10) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_fill_manual(values = palet_bt) +
    guides(col = guide_legend(keyheight = 200)) + 
    theme_tufte() + 
    theme(text = element_text(size = 30),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#FFFFFF"),
          plot.background = element_rect(fill = "#000000"),
          legend.key.height = unit(32, "points"),
          legend.text = element_text(color = "white")) +

    # animatie
    labs(title = 'Aandeel bedrijfstakken, < {frame_time} >, werkelijke prijzen') +
    transition_time(Jaar)

n_taart_bt_jaren <- length(unique(taart_bt$Jaar))

animate(plot_taart_bt, 
        nframes = n_taart_bt_jaren, 
        fps = n_taart_bt_jaren / 30,
        width = 1498, 
        height = 1080)
 