    source(here::here("load_pkgs.R"))

    biomass_tree <- read_csv(here::here("data/biomass_trees.csv"), col_types = cols())

Group by plot
=============

    biomass_plot <- biomass_tree %>% 
      group_by(code) %>% 
      summarise(
         ws_plot = sum(ws, na.rm=TRUE),
         wb27_plot = sum(wb27, na.rm=TRUE),
         wb2_plot = sum(wb2, na.rm=TRUE),
         wr_plot = sum(wr, na.rm=TRUE),
         h_mean = mean(h, na.rm=TRUE),
         h_min = min(h, na.rm=TRUE),
         h_max = max(h, na.rm=TRUE),
         dbh_mean = mean(dbh, na.rm=TRUE)
      )

    ## group_by: one grouping variable (code)

    ## summarise: now 113 rows and 9 columns, ungrouped

    # Scale-up to ha and to Ton (or Megagram. 1 ton = 1 Megagram = 1000 kg )
    dicc_plots <- read_csv(here::here("raw_data/dicc_plots.csv"), col_types = cols())   

    biomass_plot <- biomass_plot  %>% 
      inner_join((
        dicc_plots %>% dplyr::select(
          id_plot, original_code, loc, type, size_m, size)), 
        by = c("code"="original_code"))

    ## inner_join: added 5 columns (id_plot, loc, type, size_m, size)

    ##             > rows only in x  (  0)

    ##             > rows only in y  ( 12)

    ##             > matched rows     113

    ##             >                 =====

    ##             > rows total       113

    # ((biomasa * 10000 m2/1ha) / size) kg * 1Mg /1000 kg --> (biomasa * 10 )/size 

    biomass_plot <- biomass_plot %>% 
      mutate(
        ws = (ws_plot * 10)/size,
        wb27 = (wb27_plot * 10)/size, 
        wb2 = (wb2_plot * 10)/size, 
        wr = (wr_plot* 10)/size) 

    ## mutate: new variable 'ws' with 113 unique values and 0% NA

    ##         new variable 'wb27' with 113 unique values and 0% NA

    ##         new variable 'wb2' with 113 unique values and 0% NA

    ##         new variable 'wr' with 113 unique values and 0% NA

    # Ojo son datos en Mg biomass / ha 
    write_csv(biomass_plot, here::here("data/biomass_plot.csv"))

    plotsQP_selected <- st_read(here::here("/data/geoinfo/plotsQP_selected.shp"))

    ## Reading layer `plotsQP_selected' from data source `/Users/ajpelu/Google Drive/_phd/04_seq_carbon/qp_seq_carbon/data/geoinfo/plotsQP_selected.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 113 features and 13 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -3.482064 ymin: 36.94205 xmax: -3.257967 ymax: 37.18328
    ## CRS:            4326

    b <- plotsQP_selected %>% 
      inner_join(biomass_plot) %>% 
      dplyr::select(id_plot, code, project, loc, size, ws, wb27, wb2, wr, type, size_m, h_mean, h_min, h_max, dbh_mean) %>% 
      mutate(across(where(is.numeric), ~ round(., digits = 2))) %>% 
      mutate(id_plot = as.integer(id_plot)) 

    ## Joining, by = c("code", "id_plot", "loc", "type", "size_m", "size")

    ## inner_join: added 12 columns (ws_plot, wb27_plot, wb2_plot, wr_plot, h_mean, …)

    ##             > rows only in x  (  0)

    ##             > rows only in y  (  0)

    ##             > matched rows     113

    ##             >                 =====

    ##             > rows total       113

    ## mutate: changed 80 values (71%) of 'size' (0 new NA)

    ##         changed 113 values (100%) of 'ws' (0 new NA)

    ##         changed 113 values (100%) of 'wb27' (0 new NA)

    ##         changed 113 values (100%) of 'wb2' (0 new NA)

    ##         changed 113 values (100%) of 'wr' (0 new NA)

    ##         changed 101 values (89%) of 'h_mean' (0 new NA)

    ##         changed 15 values (13%) of 'h_min' (0 new NA)

    ##         changed 16 values (14%) of 'h_max' (0 new NA)

    ##         changed 102 values (90%) of 'dbh_mean' (0 new NA)

    ## mutate: converted 'id_plot' from double to integer (0 new NA)

    st_write(b, here::here("/data/geoinfo/plotsQP_biomass.shp"), append = FALSE)

    ## Deleting layer `plotsQP_biomass' using driver `ESRI Shapefile'
    ## Writing layer `plotsQP_biomass' to data source `/Users/ajpelu/Google Drive/_phd/04_seq_carbon/qp_seq_carbon//data/geoinfo/plotsQP_biomass.shp' using driver `ESRI Shapefile'
    ## Writing 113 features with 15 fields and geometry type Polygon.