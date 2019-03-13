library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
# library(ggalluvial)
library(animation)
library(networkD3)
library(htmltools)

library(treemap)
library(cbsodataR)

library(randomcoloR)
library(ggthemes)

# BBP & Groei ----
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

groei_meta <- cbs_get_meta("84106NED")
groei      <- cbs_get_data("84106NED")

groei_meta_key <- 
    groei_meta$DataProperties %>%
    select(Key, Title, ParentID) %>%
    full_join(groei_meta$DataProperties %>%
                  select(ParentKey = Key, ParentTitle = Title, ID),
              by = c("ParentID" = "ID")) %>%
    mutate(Title = ifelse(Title == "Totaal", ParentTitle, Title)) %>%
    select(Key, Title) %>%
    na.omit()


# Taart BBP Bedrijfstakken ----
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
    rename(Bedrijfstak = Title) %>%
    
    filter(Jaar <= 2017)

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
          plot.title = element_text(size=60, 
                                    hjust = 0.5, 
                                    color = "#FFFFFF"),
          plot.background = element_rect(fill = "#000000"),
          legend.key.height = unit(32, "points"),
          legend.text = element_text(color = "white")) +

    # animatie
    labs(title = '< {frame_time} >') +
    transition_time(Jaar)

n_taart_bt_jaren <- length(unique(taart_bt$Jaar))

animate(plot_taart_bt, 
        nframes = n_taart_bt_jaren, 
        fps = n_taart_bt_jaren / 20,
        width = 1480, 
        height = 1080)

anim_save("movie-1/plot_taart_bt.gif")

# Staaf BBP Bestedingen ----
bbp_bs <- bbp %>%
    select(SoortGegevens,
           Perioden, 
           Totaal_1:Diensten_17)

bbp_bs_jj <- bbp_bs %>%
    filter(grepl("JJ", Perioden)) %>%
    mutate(Jaar = as.numeric(substr(Perioden, 0, 4)))

bbp_bs_jj_cp <- bbp_bs_jj %>%
    filter(SoortGegevens == "A045295")  # Prijsniveau 2015

bbp_bs_jj_cp_tidy <- bbp_bs_jj_cp %>%
    select(-SoortGegevens, -Perioden) %>%
    gather(Key, Waarde,
           Totaal_1:Diensten_17)

staaf_bs_selectie <-
    data.frame(
        Key   = c("Goederen_4",
                  "Diensten_5",
                  "Huishoudens_9",
                  "Overheid_10",
                  "BedrijvenEnHuishoudens_12",
                  "Overheid_13",
                  "VeranderingInVoorraden_14",
                  "Goederen_16",
                  "Diensten_17"),
        Title = c("Invoer van goederen",
                  "Invoer van diensten",
                  "Consumptie huishoudens (hh)",
                  "Consumptie overheid",
                  "Investeringen bedrijven en hh",
                  "Investeringen overheid",
                  "Verandering in voorraden",
                  "Uitvoer van goederen",
                  "Uitvoer van diensten"),
        stringsAsFactors = FALSE
    )

staaf_bs <- bbp_bs_jj_cp_tidy %>%
    #filter(Jaar %in% 1995:2000) %>%
    right_join(staaf_bs_selectie, by = "Key") %>%
    rename(Besteding = Title) %>%
    mutate(Waarde = ifelse(grepl("Invoer", Besteding), 
                           -Waarde, Waarde)) %>%
    
    filter(Jaar <= 2017)

staaf_bs_pc <- staaf_bs %>%
    group_by(Jaar) %>%
    mutate(Percentage = Waarde / sum(Waarde))


# https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
set.seed(2)
palet_bs <- distinctColorPalette(9)

# https://www.displayr.com/how-to-make-a-pie-chart-in-r/
plot_staaf_bs <- staaf_bs_pc %>% 
    
    # plot
    ggplot(aes(x = 1,
               y = Waarde,
               fill = Besteding)) +
    geom_bar(stat="identity") + 
    geom_text(aes(
        label = paste0(round(Percentage * 100), "%")), 
        position = position_stack(vjust = 0.5),
        size = 10) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_fill_manual(values = palet_bs) +
    guides(col = guide_legend(keyheight = 200)) + 
    theme_tufte() + 
    theme(text = element_text(size = 30),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 60, 
                                    hjust = 0.5, 
                                    color = "#FFFFFF"),
          plot.background = element_rect(fill = "#000000"),
          legend.key.height = unit(32, "points"),
          legend.text = element_text(color = "white")) +
    
    # animatie
    labs(title = '< {frame_time} >') +
    transition_time(Jaar)

n_staaf_bs_jaren <- length(unique(staaf_bs$Jaar))

animate(plot_staaf_bs, 
        nframes = n_staaf_bs_jaren, 
        fps = n_staaf_bs_jaren / 20,
        width = 1480, 
        height = 1080)

anim_save("movie-1/plot_staaf_bs.gif")
 
# Treemap BBP Bedrijfstakken ----
groei_bt_jj_vi_tidy <- groei %>%
    select(SoortMutaties,
           Perioden, 
           Totaal_19:BbpGecorrigeerdVoorWerkdageneffecten_74) %>%
    filter(grepl("JJ", Perioden)) %>%
    mutate(Jaar = as.numeric(substr(Perioden, 0, 4))) %>%
    filter(SoortMutaties == "A045299") %>%
    select(-SoortMutaties, -Perioden) %>%
    gather(Key, Waarde,
           Totaal_19:BbpGecorrigeerdVoorWerkdageneffecten_74) %>%
    left_join(groei_meta_key,
              by = "Key")

groei_bt_selectie <-
    c("ALandbouwBosbouwEnVisserij_21",
      "BDelfstoffenwinning_23",
      "Totaal_24",
      "DEnergievoorziening_36",
      "EWaterbedrijvenEnAfvalbeheer_37",
      "FBouwnijverheid_38",
      "GHandel_41",
      "HVervoerEnOpslag_42",
      "IHoreca_43",
      "JInformatieEnCommunicatie_44",
      "KFinancieleDienstverlening_45",
      "LVerhuurEnHandelVanOnroerendGoed_46",
      "Totaal_47",
      "Totaal_56",
      "QGezondheidsEnWelzijnszorg_59",
      "RUCultuurRecreatieOverigeDiensten_60")

treemap_groei_bt <- groei_bt_jj_vi_tidy %>%
    #filter(Jaar %in% 1995:2000) %>%
    filter(Key %in% groei_bt_selectie) %>%
    rename(Bedrijfstak = Title,
           Groei = Waarde) %>%
    
    filter(Jaar <= 2017)

treemap_bt <- taart_bt %>%
    full_join(treemap_groei_bt, by = c("Jaar", "Bedrijfstak")) %>%
    mutate(Sortering = as.numeric(as.factor(Bedrijfstak)))

d <- getwd()
setwd(paste0(d, "/movie-1"))
saveGIF({
    par(bg = "white")
    for (j in 1996:2017) 
    {
        treemap(treemap_bt %>% filter(Jaar == j),
                title = paste("<", j, ">"),
                type = "value",
                range = c(-15, 15),
                index = "Bedrijfstak", 
                vSize = "Waarde",
                vColor = "Groei",
                sortID = "Sortering",
                fontfamily.title = "serif",
                fontsize.title = 80,
                fontfamily.legend = "serif",
                fontsize.legend = 50,
                fontsize.labels = 30)
    }
}, 
    movie.name = "treemap.gif",
    ani.width = 1600, 
    ani.height = 1200,
    interval = 60 / length(1996:2017)
)
setwd(d)

# Productie en IV ----
pv_meta <- cbs_get_meta("84088NED")
pv      <- cbs_get_data("84088NED")

pv_meta_key <- 
    pv_meta$DataProperties %>%
    select(Key, Title, ParentID) %>%
    full_join(pv_meta$DataProperties %>%
                  select(ParentKey = Key, ParentTitle = Title, ID),
              by = c("ParentID" = "ID")) %>%
    mutate(Title = ifelse(Title == "Totaal", ParentTitle, Title)) %>%
    select(Key, Title) %>%
    na.omit()

pv_bewerkt <- pv %>%
    left_join(pv_meta$BedrijfstakkenBranchesSBI2008 %>%
                  select(Key, Bedrijfstak = Title),
              by = c("BedrijfstakkenBranchesSBI2008" = "Key")) %>%
    rename(Productie   = OutputBasisprijzen_1,
           Verbruik    = IntermediairVerbruik_2,
           TW          = BrutoToegevoegdeWaardeBasisprijzen_3) %>%
    mutate(Verbruik    = -Verbruik) %>%
    filter(grepl("JJ", Perioden)) %>%
    mutate(Jaar = as.numeric(substr(Perioden, 0, 4))) %>%
    select(Jaar, BedrijfstakkenBranchesSBI2008, Bedrijfstak, 
           Productie, Verbruik, TW)

staaf_pv_filter <- 
    c("301000",
      "300002",
      "350000",
      "300006",
      "391600",
      "396300",
      "402000",
      "300010",
      "300012",
      "300014")

staaf_pv <- pv_bewerkt %>%
    filter(trimws(BedrijfstakkenBranchesSBI2008) %in% staaf_pv_filter) %>%
    select(-Productie) %>%
    gather(Wat, Waarde, Verbruik:TW)

plot_staaf_pv <- staaf_pv %>%
    
    mutate(Wat = ifelse(Wat == "TW", "Toegevoegde waarde", Wat)) %>%

    # plot
    ggplot(aes(x = Bedrijfstak,
               y = Waarde,
               fill = Wat)) +
    geom_bar(stat="identity", color = "blue", size = 2) + 
    geom_text(aes(
        label = Waarde), 
        position = position_stack(vjust = 0.5),
        size = 5) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_fill_manual(values = c("green", "orange")) +
    guides(fill= guide_legend(title="Productie"),
           col = guide_legend(keyheight = 200)) + 
    theme_tufte() + 
    theme(text = element_text(size = 30),
          
          axis.line = element_blank(),
          axis.text.x = element_text(size = 20,
                                     hjust = 1,
                                   angle = 75,
                                   color = "#FFFFFF"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          
          plot.title = element_text(size = 60, 
                                    hjust = 0.5, 
                                    color = "#FFFFFF"),
          plot.background = element_rect(fill = "#000000"),
          
          legend.title = element_text(color = "white"),
          legend.key.height = unit(32, "points"),
          legend.text = element_text(color = "white")) +
    
    # animatie
    labs(title = '< {frame_time} >') +
    transition_time(Jaar)

n_staaf_pv_jaren <- length(unique(staaf_pv$Jaar))

animate(plot_staaf_pv, 
        nframes = n_staaf_pv_jaren, 
        fps = n_staaf_pv_jaren / 20,
        width = 1480, 
        height = 1080)

anim_save("movie-1/plot_staaf_pv.gif")

# Gapminder ----
arbeid_meta <- cbs_get_meta("84164NED")
arbeid      <- cbs_get_data("84164NED")

arbeid_bt <- arbeid %>%
    select(Geslacht,
           TypeWerkenden,
           Perioden, 
           BedrijfstakkenBranchesSBI2008,
           Arbeidsjaren_3) %>%
    filter(grepl("JJ", Perioden),
           Geslacht == 1100,
           TypeWerkenden == "T001413") %>%
    mutate(Jaar = as.numeric(substr(Perioden, 0, 4)),
           BedrijfstakkenBranchesSBI2008 = 
               as.character(BedrijfstakkenBranchesSBI2008)) %>%
    select(-Geslacht, - TypeWerkenden, -Perioden) %>%
    left_join(arbeid_meta$BedrijfstakkenBranchesSBI2008 %>%
                  select(Key, Title),
              by = c("BedrijfstakkenBranchesSBI2008" = "Key")) %>%
    select(Jaar,
           Title,
           Arbeidsjaren = Arbeidsjaren_3,
           -BedrijfstakkenBranchesSBI2008)

arbeid_bt_bewerkt <-
    bind_rows(arbeid_bt,
              arbeid_bt %>%
                  filter(Title %in% 
                             c("O Openbaar bestuur en overheidsdiensten",
                               "P Onderwijs")) %>%
                  mutate(Title = "O-P Overheid en onderwijs") %>%
                  group_by(Jaar, Title) %>%
                  summarise_all(sum) %>% ungroup()
    )

gapminder_data <- bbp_bt_jj_wp_tidy %>%
    
    filter(Key %in% taart_bt_selectie) %>%
    select(-Key) %>%
    
    left_join(pv_bewerkt,
              by = c("Jaar", "Title" = "Bedrijfstak")) %>%
    select(Jaar, Title, everything()) %>%
    
    left_join(arbeid_bt_bewerkt,
              by = c("Jaar", "Title")) %>%
    
    # filter(Jaar %in% 1995:2000) %>%
    rename(Bedrijfstak = Title,
           BBP = Waarde) %>%
    
    mutate(PV_Verhouding = Productie / -Verbruik,
           BBP_Arbeidsjaar = BBP / Arbeidsjaren) %>%
    
    select(Jaar, Bedrijfstak, BBP, PV_Verhouding, BBP_Arbeidsjaar) %>%
    
    filter(Jaar > 1995 & Jaar <= 2017)

plot_gapminder <- gapminder_data %>%
    
    # plot
    ggplot(aes(BBP_Arbeidsjaar, 
               PV_Verhouding, 
               size = BBP, 
               colour = Bedrijfstak)) +
    geom_point(alpha = 0.7, 
               show.legend = FALSE) +
    geom_text(aes(label = Bedrijfstak),
              size = 8,
              show.legend = FALSE) +
    scale_colour_manual(values = palet_bt) +
    scale_size(range = c(2, 80)) +
    scale_x_log10() +
    scale_y_sqrt() +
    theme_tufte() + 
    theme(text = element_text(size = 30,
                              colour = "#FFFFFF"),
          
          axis.line = element_blank(),
          axis.text = element_text(size = 20),
          axis.ticks = element_blank(),
          
          plot.title = element_text(size = 60, 
                                    hjust = 0.5, 
                                    color = "#FFFFFF"),
          plot.background = element_rect(fill = "#000000")) +
    
    # animatie
    labs(title = '< {frame_time} >', 
         x = 'BBP per Arbeidsjaar', 
         y = 'Productie / Verbruik') +
    transition_time(Jaar)

n_gapminder_jaren <- length(unique(gapminder_data$Jaar))

animate(plot_gapminder, 
        nframes = n_gapminder_jaren, 
        fps = n_gapminder_jaren / 20,
        width = 1480, 
        height = 1080)

anim_save("movie-1/plot_gapminder.gif")

# Correlatiematrix ----
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
groei_bt_kw_vi_tidy <- groei %>%
    select(SoortMutaties,
           Perioden, 
           Totaal_19:BbpGecorrigeerdVoorWerkdageneffecten_74) %>%
    filter(grepl("KW", Perioden)) %>%    
    mutate(Jaar = as.numeric(substr(Perioden, 0, 4)),
           Kwartaal = as.numeric(substr(Perioden, 8, 8))) %>%
    filter(SoortMutaties == "A045299") %>%
    select(-SoortMutaties, -Perioden) %>%
    gather(Key, Waarde,
           Totaal_19:BbpGecorrigeerdVoorWerkdageneffecten_74) %>%
    left_join(groei_meta_key,
              by = "Key")

correlatie_bt <- function(input, kwartaal)
{
    input %>%
        filter(Key %in% groei_bt_selectie,
               Kwartaal == kwartaal) %>%
        select(Jaar, Kwartaal, Title, Waarde) %>%
        mutate(Bedrijfstak = substr(Title, 0, 15),
               Groei = Waarde) %>%
        filter(Jaar > 1995 & Jaar <= 2017) %>%
        select(-Title, -Waarde) %>%
        spread(Bedrijfstak, Groei) %>%
        select(-Jaar, -Kwartaal) %>%
        cor() %>%
        round(2)
}

correlatie_plot <- function(correlatiematrix, kwartaal)
{
    # Get lower triangle of the correlation matrix
    get_lower_tri<-function(cormat){
        cormat[upper.tri(cormat)] <- NA
        return(cormat)
    }
    # Get upper triangle of the correlation matrix
    get_upper_tri <- function(cormat){
        cormat[lower.tri(cormat)]<- NA
        return(cormat)
    }
    
    correlatiematrix_melted <- 
        get_lower_tri(correlatiematrix) %>%
        reshape2::melt(na.rm = TRUE)
    
    ggplot(data = correlatiematrix_melted,
           aes(x = Var1,
               y = Var2,
               fill = value)) +
        
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Pearson\nCorrelation") +
        
        theme_minimal() +
        
        coord_fixed() + 
        
        geom_text(aes(Var1, Var2, label = value), 
                  color = "black", size = 4) +
        
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks = element_blank(),
              legend.justification = c(1, 0),
              legend.position = c(0.5, 0.8),
              legend.direction = "horizontal",
              plot.title = element_text(size = 30, 
                                        hjust = 0.5, 
                                        color = "#000000")) +
        
        guides(fill = guide_colorbar(barwidth = 7, 
                                     barheight = 1,
                                     title.position = "top",
                                     title.hjust = 0.5)) +
        
        labs(title = paste0('Groei ', kwartaal, 'e kwartaal'))
}

for(k in 1:4) {
    correlatie_bt(groei_bt_kw_vi_tidy, kwartaal = k) %>% 
        correlatie_plot(k)
    
    ggsave(filename = paste0("movie-1/correlatie/plot_correlatie_kw0", 
                             k, ".png"),
           width = 9, 
           height = 9,
           dpi = 600,
           units = "in")
}

# Stroomdiagram ----
gg_meta <- cbs_get_meta("84092NED")
gg      <- cbs_get_data("84092NED")

gg_meta_key <- 
    gg_meta$DataProperties %>%
    select(Key, Title, ParentID) %>%
    full_join(gg_meta$DataProperties %>%
                  select(ParentKey = Key, ParentTitle = Title, ID),
              by = c("ParentID" = "ID")) %>%
    mutate(Title = ifelse(Title == "Totaal", ParentTitle, Title)) %>%
    select(Key, Title) %>%
    na.omit()

GG_SELECTIE <- filter(gg_meta$ProductgroepenEnOverigePosten,
                      CategoryGroupID == 4)

gg_jaar <- function(gg, jaar)
{
    gg %>%
        
        filter(grepl(paste0(jaar, "JJ"), Perioden)) %>%
        mutate(Jaar = as.numeric(substr(Perioden, 0, 4))) %>%
        
        # Landbouw
        filter(ProductgroepenEnOverigePosten %in% GG_SELECTIE$Key) %>%
        left_join(gg_meta$ProductgroepenEnOverigePosten %>%
                      select(Key, Title),
                  by = c("ProductgroepenEnOverigePosten" = "Key")) %>%
        select(Jaar,
               Goed = Title,
               AanbodUitBinnenlandseProductie_17,
               Totaal_3,
               IntermediairVerbruik_7,
               Totaal_8,
               BrutoInvesteringenInVasteActiva_11,
               VeranderingInVoorraden_12,
               Totaal_13)
}

gg_aanbod <- function(gg)
{
    gg %>%
        select(Jaar, Goed,
               AanbodUitBinnenlandseProductie_17:Totaal_3,
               VeranderingInVoorraden_12) %>%
        gather(Key, Waarde,
               AanbodUitBinnenlandseProductie_17:Totaal_3,
               VeranderingInVoorraden_12) %>%
        left_join(gg_meta_key,
                  by = "Key") %>%
        group_by(Jaar) %>%
        mutate(Title = ifelse(Title == "Verandering in voorraden",
                              "Afname voorraden", Title),
               Waarde = ifelse(Title == "Afname voorraden" & Waarde > 0,
                               0, Waarde),
               Waarde = ifelse(Title == "Afname voorraden" & Waarde < 0,
                               -Waarde, Waarde),
               Aandeel = Waarde / sum(Waarde)) %>%
        select(Jaar, Goed, Aanbod = Title, Waarde)
}

gg_gebruik <- function(gg)
{
    gg %>%
        select(Jaar, Goed,
               IntermediairVerbruik_7:Totaal_13) %>%
        gather(Key, Waarde,
               IntermediairVerbruik_7:Totaal_13) %>%
        left_join(gg_meta_key,
                  by = "Key") %>%
        mutate(Title = ifelse(Title == "Verandering in voorraden",
                              "Toename voorraden", Title),
               Waarde = ifelse(Title == "Toename voorraden" & Waarde < 0,
                               0, Waarde)) %>%
        select(Jaar, Goed, Gebruik = Title, Waarde)
}

gg_bs <- function(aanbod, gebruik)
{
    full_join(aanbod %>%
                  group_by(Jaar, Goed) %>%
                  summarise(A = sum(Waarde)),
              gebruik %>%
                  group_by(Jaar, Goed) %>%
                  summarise(G = sum(Waarde)),
              by = c("Jaar", "Goed")) %>%
        mutate(Aanbod = "Belastingen en subsidies",
               Waarde = G - A) %>%
        select(Jaar, Goed, Aanbod, Waarde)
}

# gg_industrie <- gg %>%
#     
#     filter(grepl("JJ", Perioden)) %>%
#     mutate(Jaar = as.numeric(substr(Perioden, 0, 4))) %>%
# 
#     # Landbouw
#     filter(ProductgroepenEnOverigePosten == 1771374) %>%
#     select(Jaar,
#            AanbodUitBinnenlandseProductie_17,
#            Totaal_3,
#            IntermediairVerbruik_7,
#            Totaal_8,
#            BrutoInvesteringenInVasteActiva_11,
#            VeranderingInVoorraden_12,
#            Totaal_13)
# 
# gg_industrie_aanbod <- gg_industrie %>%
#     select(Jaar, 
#            AanbodUitBinnenlandseProductie_17:Totaal_3, 
#            VeranderingInVoorraden_12) %>%
#     gather(Key, Waarde,
#            AanbodUitBinnenlandseProductie_17:Totaal_3,
#            VeranderingInVoorraden_12) %>%
#     left_join(gg_meta_key,
#               by = "Key") %>%
#     group_by(Jaar) %>%
#     mutate(Title = ifelse(Title == "Verandering in voorraden",
#                           "Afname voorraden", Title),
#            Waarde = ifelse(Title == "Afname voorraden" & Waarde > 0,
#                            0, Waarde),
#            Aandeel = Waarde / sum(Waarde)) %>%
#     select(Jaar, Aanbod = Title, Waarde)
# 
# gg_industrie_gebruik <- gg_industrie %>%
#     select(Jaar, IntermediairVerbruik_7:Totaal_13) %>%
#     gather(Key, Waarde,
#            IntermediairVerbruik_7:Totaal_13) %>%
#     filter(!(Key == "VeranderingInVoorraden_12" & Waarde < 0)) %>%
#     left_join(gg_meta_key,
#               by = "Key") %>%
#     mutate(Title = ifelse(Title == "Verandering in voorraden",
#                           "Toename voorraden", Title),
#            Waarde = ifelse(Title == "Toename voorraden" & Waarde < 0,
#                            0, Waarde)) %>%
#     select(Jaar, Gebruik = Title, Waarde)
# 
# gg_industrie_bs <- 
#     full_join(gg_industrie_aanbod %>% 
#                   group_by(Jaar) %>% 
#                   summarise(A = sum(Waarde)),
#               gg_industrie_gebruik %>% 
#                   group_by(Jaar) %>% 
#                   summarise(G = sum(Waarde)),
#               by = "Jaar") %>%
#     mutate(Aanbod = "Belastingen en subsidies",
#            Waarde = G - A) %>%
#     select(Jaar, Aanbod, Waarde)

## ggalluvial ----
# stroom_data <- as.data.frame(Titanic)
# 
# stroom_data <-
#     full_join(gg_landbouw_aanbod,
#               gg_landbouw_gebruik,
#               by = "Jaar") %>%
#     mutate(Waarde = Waarde * Aandeel) %>%
#     select(-Aandeel) %>%
#     mutate(Economie = "Nederland")
# 
# plot_stroom <- stroom_data %>%
#     
#     # plot
#     ggplot(aes(y = Waarde,
#                axis1 = Aanbod, axis2 = Economie, axis3 = Gebruik)) +
#     geom_alluvium(aes(fill = Aanbod),
#                   width = 0, knot.pos = 0, reverse = FALSE,
#                   alpha = 0.7, 
#                   show.legend = FALSE) +
#     # guides(fill = FALSE) +
#     geom_stratum(width = 1/8, reverse = FALSE) +
#     geom_text(stat = "stratum", 
#               size = 4,
#               label.strata = TRUE, 
#               reverse = FALSE) +
#     scale_x_continuous(breaks = 1:3, 
#                        labels = c("Aanbod", "Economie", "Gebruik")) +
#     # coord_flip() +
#     scale_colour_manual(values = palet_bt) +
#     
#     theme_tufte() + 
#     theme(text = element_text(size = 30,
#                               colour = "#FFFFFF"),
#           
#           axis.line = element_blank(),
#           axis.text = element_text(size = 20),
#           axis.ticks = element_blank(),
#           
#           plot.title = element_text(size = 60, 
#                                     hjust = 0.5, 
#                                     color = "#FFFFFF"),
#           plot.background = element_rect(fill = "#000000")) +
#     
#     # animatie
#     labs(title = '< {frame_time} >', 
#          # x = 'BBP per Arbeidsjaar', 
#          y = 'Euro') +
#     transition_time(Jaar)

## networkD3 ----
SANKEY_JAAR <- 2016

g <- gg_jaar(gg, SANKEY_JAAR)

stroom_links <- 
    gg_aanbod(g) %>%
    bind_rows(gg_bs(gg_aanbod(g), gg_gebruik(g))) %>%
    rename(Bron = Aanbod) %>%
    mutate(Groep = "Aanbod",
           Bestemming = "Economie") %>%
    bind_rows(gg_gebruik(g) %>%
                  rename(Bestemming = Gebruik) %>%
                  mutate(Groep = "Gebruik",
                         Bron = "Economie")) %>%
    mutate(Groep = as.factor(Groep))

stroom_nodes <- 
    bind_rows(stroom_links %>% ungroup() %>%
                  select(Naam = Bron, Groep),
              stroom_links %>% ungroup() %>%
                  select(Naam = Bestemming, Groep)) %>%
    mutate(Groep = as.factor(
        ifelse(Naam == "Economie", "Economie", Groep))) %>%
    unique()
    
### With networkD3, connection must be provided using id, 
### not using real name like in the links dataframe.. So we need to reformat it.
stroom_links$Bron_ID       = match(stroom_links$Bron, 
                                   stroom_nodes$Naam) - 1
stroom_links$Bestemming_ID = match(stroom_links$Bestemming, 
                                   stroom_nodes$Naam) - 1

### Give a color for each group:
sankey_kleuren <- 'd3.scaleOrdinal() .domain(["Aanbod", "Gebruik", "Economie"]) .range(["blue", "green", "yellow"])'

for (i in 1:nrow(g)) {
    network <-
        browsable(
            tagList(
                tags$head(
                    tags$style('
                               h1{text-align: center;}
                               ')
                ),
                tags$body(
                    tags$h1(paste0("< ", g$Goed[i], " >")),
                    sankeyNetwork(Links = stroom_links %>%
                                      filter(Goed == g$Goed[i],
                                             Waarde != 0), 
                                  Nodes = stroom_nodes,
                                  Source = "Bron_ID", 
                                  Target = "Bestemming_ID",
                                  Value = "Waarde", 
                                  NodeID = "Naam", 
                                  sinksRight = TRUE,
                                  
                                  colourScale = sankey_kleuren, 
                                  LinkGroup = "Groep", 
                                  NodeGroup = "Groep",
                                  
                                  fontSize = 32,
                                  
                                  width = 1900,
                                  height = 960
                    )
                )
            )
        )
    
    d <- getwd()
    setwd(paste0(d, "/movie-1/sankey"))
    save_html(network, file = paste0("sankey-", 
                                     strsplit(g$Goed[i], " ")[[1]][1], 
                                     ".html"))
    setwd(d)
}
