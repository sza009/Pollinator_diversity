source("Descriptive.R")

#rare fraction


#function to name sites
name_tbl_list_from_colum <- function(tbl_list, col) {
  out <- map(tbl_list, \(tbl)unique(tbl[[col]]))
  set_names(tbl_list, out)
}

#rarefaction, richness per individual instead of per sample
bumble_frac <- bumblebees |>   
  group_by(site, date, time) |> 
  count(species)  |> 
  ungroup() |> 
  pivot_wider(names_from = species, 
              values_from = n, values_fill = 0) 
bumble_frac_by_site <- bumble_frac |> 
  group_split(site) |> 
  name_tbl_list_from_colum("site") |> 
  imap(\(subset, name) {
    select(subset, -c(site:time)) |> 
      specaccum(method = "rarefaction")
  })
bumble_frac_total <- bumble_frac |> 
  dplyr::select(-c(site:time)) |> 
  specaccum(method = "rarefaction")

bumble_frac_by_site |> 
  imap(function(df, name) {
    tibble(sampling_size = df$individuals, richness = df$richness, sd = df$sd)  
  }) |> 
  list_rbind(names_to = "site_id") |> 
  ggplot(aes(x = sampling_size, y = richness, color = site_id)) + 
  #geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), fill = "lightblue") +
  geom_smooth() +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(x = "Number of individuals", y = "Number of species") +
  labs(color = "Site") +
  theme_bw()

#grey shading is standard error



#for all sites combined
bumble_spec <- bumblebees |>   #make df with only species richness per sample
  group_by(date, time, site, trap_ID) |> 
  count(species)  |> 
  ungroup() |> 
  pivot_wider(names_from = species, 
              values_from = n, values_fill = 0) |> 
  dplyr::select(-c(date:trap_ID)) |> 
  specaccum()
tibble(sampling_size = bumble_spec$sites, richness = bumble_spec$richness, sd = bumble_spec$sd) |> 
  ggplot(aes(x = sampling_size, y = richness)) + 
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), fill = "lightblue") +
  geom_line() +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(x = "Sample size", y = "Richness") +
  theme_bw()

#Appendix

# for individual sites
specaccum_df <- bumblebees |> 
  group_split(site)
site_names <- specaccum_df |> 
  map(\(x)unique(x$site)) 
specaccum_df <- purrr::set_names(specaccum_df, site_names) |> 
  map(\(x) {
    out <- group_by(x, date, time, site, trap_ID) |> 
      count(species, name = "sampling_size")  |>
      ungroup() |> 
      pivot_wider(names_from = species, 
                  values_from = sampling_size, values_fill = 0) |> 
      dplyr::select(-c(date:trap_ID))  |> 
      specaccum()
    tibble(sampling_size = out$sites, richness = out$richness, sd = out$sd)
  }) |> 
  bind_rows(.id = "site")


ggplot(specaccum_df, aes(x = sampling_size, y = richness)) + 
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), fill = "lightblue") +
  geom_line() +
  labs(x = "Number of samples", y = "Number of species") +
  scale_y_continuous(breaks = seq(0,10,2)) +
  facet_wrap(vars(site)) +
  theme_bw()


