    source(here::here("load_pkgs.R"))

    biomass_plot <- read_csv(here::here("data/biomass_plot.csv"), col_types = cols()) %>% 
        dplyr::select(id_plot, code, loc, size, ws, wb27, wb2, wr, type, size_m, h_mean, h_min, h_max, dbh_mean) %>% 
      mutate(cluster = case_when(
        loc %in% c("MON", "GEN", "DIL", "DUR") ~ "NW",
        loc %in% c("CAN", "POQ", "TRE") ~ "S",
        loc == "CAM" ~ "N"
      ))

Kruskal Wallis
--------------

    b <- biomass_plot %>% 
      dplyr::select(id_plot, ws, wb27, wb2, wr, h_mean, h_min, h_max, dbh_mean, cluster) %>% 
      pivot_longer(!cluster, names_to = "variables", values_to = "values") 

    ## pivot_longer: reorganized (id_plot, ws, wb27, wb2, wr, …) into (variables, values) [was 113x10, now 1017x3]

    kw <- b %>% 
      filter(variables !="id_plot") %>% 
      group_by(variables) %>% 
      group_modify(
        ~broom::tidy(kruskal.test(values ~ cluster, data=.x))) %>% 
      mutate(parameter = as.character(parameter))

    ## filter: removed 113 rows (11%), 904 rows remaining

    ### Aproach 1. Using gtsummary 
    se <- function(x) sd(x, na.rm=TRUE)/sqrt(length(x))

    s <- biomass_plot %>% 
      dplyr::select(ws, wb27, wb2, wr, h_mean, h_min, h_max, dbh_mean, cluster) %>% 
      tbl_summary(by = cluster,
                  statistic = list(all_continuous() ~ "{mean} ({se})"), 
                  digits = list(everything() ~ 2)) %>% 
      modify_header(stat_by = "**{level}**")  %>% 
      modify_footnote(update = everything() ~ NA) %>% 
      as_tibble() %>% 
      as.data.frame()

    names(s) <- c("variables","N","NW","S")

#### GET letters ———————————————

    # Custom function
    letritas <- function(df, v, g){
      m <- pairwise.wilcox.test(df[,v], df[,g], exact=FALSE)
      l <- biostat::make_cld(m) %>% 
        dplyr::select(group, cld) %>% 
        mutate(variables = v) %>% 
        pivot_wider(names_from = group, values_from = cld)
      return(l)
    }


    df <- biomass_plot %>% 
      dplyr::select(cluster, ws, wb27, wb2, wr, h_mean, h_min, h_max, dbh_mean) %>% 
      as.data.frame()

    all_variables <- names(df[,-1])

    out <- data.frame()
    for (i in seq_along(all_variables)){
     m <- letritas(df = df, v=all_variables[i], "cluster") %>% as.data.frame()
     out <- rbind(out, m)
    }

    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]
    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]
    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]
    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]
    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]
    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]
    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]
    ## pivot_wider: reorganized (group, cld) into (N, NW, S) [was 3x3, now 1x4]

    out <- out %>% rename_with(tolower)

    m <- s %>% 
      inner_join(out) %>% 
      unite("N", c("N", "n"), sep=" ") %>% 
      unite("NW", c("NW", "nw"), sep=" ") %>% 
      unite("S", c("S", "s"), sep=" ")

    ## Joining, by = "variables"

    ## inner_join: added 3 columns (n, nw, s)

    ##             > rows only in x  (0)

    ##             > rows only in y  (0)

    ##             > matched rows     8

    ##             >                 ===

    ##             > rows total       8

    df <- df %>% 
      rename(clusterG = cluster) %>% 
      mutate(cluster = case_when(
        clusterG == "N" ~ as.numeric(0),
        clusterG == "NW"~ as.numeric(1),
        clusterG == "S" ~ as.numeric(2)
      ))

    ## rename: renamed one variable (clusterG)

    my_comparisons <- list(c("0", "1"), c("0", "2"), c("1", "2"))
    # my_comparisons <- list(c("N", "NW"), c("N", "S"), c("NW", "S"))

    xsd <- scale_x_discrete(labels = c("0" = "N", "1" = "NW","2" = "S")) 
    co <- scale_color_discrete(labels = c("0" = "N", "1" = "NW","2" = "S"))


    p_hmean <- ggboxplot(df, x="cluster", y="h_mean", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Avg. Tree height (m)") + 
      stat_compare_means(comparisons = my_comparisons) +
      stat_compare_means(label.y = 25) + xsd 

      
    p_hmin <- ggboxplot(df, x="cluster", y="h_min", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Tree height min (m)") + 
      stat_compare_means(comparisons = my_comparisons) +
      stat_compare_means(label.y = 25) + xsd 


    p_hmax <- ggboxplot(df, x="cluster", y="h_max", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Tree height max (m)") + 
      stat_compare_means(comparisons = my_comparisons) +
      stat_compare_means(label.y = 28) + xsd 

    p_dbh <- ggboxplot(df, x="cluster", y="dbh_mean", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Avg. DBH (cm)") + 
      stat_compare_means(comparisons = my_comparisons) +
      stat_compare_means(label.y = 80) + xsd 


    p_ws <- ggboxplot(df, x="cluster", y="ws", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Biomass ws (ton/ha)") + 
      stat_compare_means(comparisons = my_comparisons) + 
      stat_compare_means(label.y = 700) + xsd 

    p_wb27 <- ggboxplot(df, x="cluster", y="wb27", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Biomass wb27 (ton/ha)") + 
      stat_compare_means(comparisons = my_comparisons) + 
      stat_compare_means(label.y = 100) + xsd 

    p_wb2 <- ggboxplot(df, x="cluster", y="wb2", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Biomass wb2 (ton/ha)") + 
      stat_compare_means(comparisons = my_comparisons) + 
      stat_compare_means(label.y = 35) + xsd

    p_wr <- ggboxplot(df, x="cluster", y="wr", color = "cluster", palette = "jco",
                         xlab = "", ylab = "Biomass wr (ton/ha)") + 
      stat_compare_means(comparisons = my_comparisons) + 
      stat_compare_means(label.y = 250) + xsd



    ph <- ggarrange(p_hmean, p_hmax, p_hmin, p_dbh, legend = "none")

    ## Warning in wilcox.test.default(c(15.8166666666667, 17.0277777777778,
    ## 14.8533333333333, : cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(c(17.8, 20.6, 18.8, 16.5, 19), c(15.6, 14.2, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(c(14.5, 12.2, 9, 10.2, 5.7), c(4.6647232081191, :
    ## cannot compute exact p-value with ties

    ggsave(here::here("/fig/kw_height.pdf"), ph, width = 20, height = 20, units = "cm")
    pb <- ggarrange(p_ws, p_wb27, p_wb2, p_wr, legend = "none") 
    ggsave(here::here("/fig/kw_biomass.pdf"), pb, width = 20, height = 20, units = "cm")
