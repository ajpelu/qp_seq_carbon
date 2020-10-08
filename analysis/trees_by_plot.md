    source(here::here("load_pkgs.R"))

    biomass_tree <- read_csv(here::here("data/biomass_trees.csv"), col_types = cols())
    dicc_plots <- read_csv(here::here("raw_data/dicc_plots.csv"), col_types = cols())   

    b <- biomass_tree %>% 
      inner_join((
        dicc_plots %>% dplyr::select(
          id_plot, original_code, loc, type, size_m, size)), 
        by = c("code"="original_code"))

    tabla <- b %>% group_by(project) %>% 
      summarise(
        n_trees = n(),
        dbh_m = mean(dbh, na.rm=TRUE), 
        dbh_min = min(dbh, na.rm=TRUE),
        dbh_max = max(dbh, na.rm=TRUE),
        h_m = mean(h, na.rm=TRUE), 
        h_min = round(min(h, na.rm=TRUE), 1),
        h_max = round(max(h, na.rm=TRUE), 1)
      ) %>% 
      unite("dbh_r", dbh_min:dbh_max, sep= " - ") %>% 
      unite("h_r", h_min:h_max, sep= " - ")

    ## group_by: one grouping variable (project)

    ## summarise: now 6 rows and 8 columns, ungrouped

    knitr::kable(tabla)

<table>
<thead>
<tr class="header">
<th style="text-align: left;">project</th>
<th style="text-align: right;">n_trees</th>
<th style="text-align: right;">dbh_m</th>
<th style="text-align: left;">dbh_r</th>
<th style="text-align: right;">h_m</th>
<th style="text-align: left;">h_r</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">dendro_life</td>
<td style="text-align: right;">191</td>
<td style="text-align: right;">7.734764</td>
<td style="text-align: left;">2.1 - 42.2</td>
<td style="text-align: right;">4.475655</td>
<td style="text-align: left;">1.1 - 13.6</td>
</tr>
<tr class="even">
<td style="text-align: left;">dendro_sn</td>
<td style="text-align: right;">457</td>
<td style="text-align: right;">26.299234</td>
<td style="text-align: left;">2.86 - 122.55</td>
<td style="text-align: right;">10.309519</td>
<td style="text-align: left;">3 - 20.4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">migrame</td>
<td style="text-align: right;">1422</td>
<td style="text-align: right;">10.765176</td>
<td style="text-align: left;">1 - 105</td>
<td style="text-align: right;">4.273387</td>
<td style="text-align: left;">0.1 - 18</td>
</tr>
<tr class="even">
<td style="text-align: left;">obsnev_sjer</td>
<td style="text-align: right;">122</td>
<td style="text-align: right;">19.032787</td>
<td style="text-align: left;">8 - 36</td>
<td style="text-align: right;">9.566277</td>
<td style="text-align: left;">4.7 - 16.4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">qp_dasometrico</td>
<td style="text-align: right;">382</td>
<td style="text-align: right;">24.315759</td>
<td style="text-align: left;">2.91 - 67</td>
<td style="text-align: right;">11.420550</td>
<td style="text-align: left;">1.7 - 20.6</td>
</tr>
<tr class="even">
<td style="text-align: left;">robledal_life</td>
<td style="text-align: right;">650</td>
<td style="text-align: right;">13.260769</td>
<td style="text-align: left;">7.6 - 50</td>
<td style="text-align: right;">6.959888</td>
<td style="text-align: left;">3.6 - 14</td>
</tr>
</tbody>
</table>

    b %>% group_by(project, loc) %>% 
      summarise(nplots = unique(code)) %>% 
      summarise(n = n()) %>% knitr::kable()

    ## group_by: 2 grouping variables (project, loc)

    ## summarise: now 113 rows and 3 columns, 2 group variables remaining (project, loc)

    ## summarise: now 12 rows and 3 columns, one group variable remaining (project)

<table>
<thead>
<tr class="header">
<th style="text-align: left;">project</th>
<th style="text-align: left;">loc</th>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">dendro_life</td>
<td style="text-align: left;">CAN</td>
<td style="text-align: right;">2</td>
</tr>
<tr class="even">
<td style="text-align: left;">dendro_sn</td>
<td style="text-align: left;">CAN</td>
<td style="text-align: right;">24</td>
</tr>
<tr class="odd">
<td style="text-align: left;">dendro_sn</td>
<td style="text-align: left;">GEN</td>
<td style="text-align: right;">15</td>
</tr>
<tr class="even">
<td style="text-align: left;">migrame</td>
<td style="text-align: left;">CAN</td>
<td style="text-align: right;">14</td>
</tr>
<tr class="odd">
<td style="text-align: left;">migrame</td>
<td style="text-align: left;">GEN</td>
<td style="text-align: right;">16</td>
</tr>
<tr class="even">
<td style="text-align: left;">obsnev_sjer</td>
<td style="text-align: left;">MON</td>
<td style="text-align: right;">3</td>
</tr>
<tr class="odd">
<td style="text-align: left;">qp_dasometrico</td>
<td style="text-align: left;">CAM</td>
<td style="text-align: right;">5</td>
</tr>
<tr class="even">
<td style="text-align: left;">qp_dasometrico</td>
<td style="text-align: left;">DIL</td>
<td style="text-align: right;">5</td>
</tr>
<tr class="odd">
<td style="text-align: left;">qp_dasometrico</td>
<td style="text-align: left;">DUR</td>
<td style="text-align: right;">5</td>
</tr>
<tr class="even">
<td style="text-align: left;">qp_dasometrico</td>
<td style="text-align: left;">POQ</td>
<td style="text-align: right;">4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">qp_dasometrico</td>
<td style="text-align: left;">TRE</td>
<td style="text-align: right;">4</td>
</tr>
<tr class="even">
<td style="text-align: left;">robledal_life</td>
<td style="text-align: left;">CAN</td>
<td style="text-align: right;">16</td>
</tr>
</tbody>
</table>
