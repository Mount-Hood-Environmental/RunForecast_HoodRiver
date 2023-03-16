#First pass at the Hood River run forecasting models
#plan is to build the model code here, then drop into a markdown for report auto-generation
#Alternatively, can build a basic R script that loads/installs markdown then runs the markdown script. Probably preferable.
#Author: Mark Roes
#Client: Warm Springs
#Date: 2/7/2023

source('R/package_load.r')
#source('R/generate_markdown.r')
package_load(package_list = c("plyr","tidyverse", "janitor", "lubridate", "dataRetrieval", "brms", "readxl")) #will add rmarkdown, tinytex

prediction_year = 2023
#load data ---- 
#Abundance and age data

ad_count = read_excel('Data input/Abundance/AbundanceData.xlsx',
                       sheet = 'Adult') %>%
  janitor::clean_names()

smolt_count = read_excel('Data input/Abundance/AbundanceData.xlsx',
                          sheet = 'Smolt') %>%
   janitor::clean_names()

# age_summary = read_excel('Data input/Abundance/AgeData.xlsx',
#                          sheet = 'Return age summary') %>%
#   janitor::clean_names()



# Env data
npgo = read_csv('Data input/Environmental/NPGO.csv') %>%
  janitor::clean_names() %>%
  bind_rows(tibble(year = prediction_year)) %>%
  dplyr::mutate(dam = ifelse(year < 2011, 1, 0))
#Load in Tucker bridge gage flow

tucker_flow = readNWISdv("14120000", "00060", "1987-01-01", Sys.Date()) %>%
  dplyr::select(Date, X_00060_00003) %>%
  dplyr::rename(cfs_daily = X_00060_00003) %>%
  janitor::clean_names() %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year) %>%
  dplyr::summarise(min_flow_yr = min(cfs_daily)) %>%
  ungroup()

#load bonneville metrics
bon = read_csv("Data input/Environmental/Bonneville_data.csv") %>%
  janitor::clean_names() %>%
  mutate(month = month(date),
         year = year(date),
         jday = yday(date),
         wsthd_return = ifelse(month %in% c(12,1,2,3,4), "yes","no" ),
         wsthd_out = ifelse(month %in% c(5), 'yes','no' ),
         hsc_out = ifelse(month %in% c(4,5),'yes','no'))



#NOR summer steelhead----
#Transform data

sst_props = read_excel('Data input/Abundance/AgeData.xlsx',
                       sheet = 'HOR SSTHD') %>%
  janitor::clean_names() %>%
  pivot_longer(2:last_col(1)) %>%
  dplyr::mutate(salt = substr(name,2,2)) %>%
  group_by(salt) %>%
  dplyr::summarise(prop =  sum(value)/sum(total)) %>%
  ungroup() %>%
  mutate(type = 'salt') %>%
  rename(class = salt)
  


sst_dat = npgo %>%
  filter(month == 5 | is.na(month)) %>%
  left_join(tucker_flow) %>%
  left_join(ad_count %>% 
              dplyr::select(year, ssthd, ssthd_bon, ssthd_dalles)) %>%
  left_join(smolt_count %>%
              dplyr::select(year, nor_sthd)) %>%
  mutate(damcount = ssthd_bon - ssthd_dalles,
         damcount_adj = lag(damcount,1),
         smolt_adj = lag(nor_sthd, 1) * filter(sst_props, class == 1)$prop+
           lag(nor_sthd, 2) * filter(sst_props, class == 2)$prop+
           lag(nor_sthd, 3) * filter(sst_props, class == 3)$prop,
         npgo_adj = lag(npgo,1) * filter(sst_props, class == 1)$prop+
           lag(npgo, 2) * filter(sst_props, class == 2)$prop+
           lag(npgo, 3) * filter(sst_props, class == 3)$prop,
         min_flow_adj = lag(min_flow_yr,2) * filter(sst_props, class == 1)$prop+
           lag(min_flow_yr,3) * filter(sst_props, class == 2)$prop+
           lag(min_flow_yr,4) * filter(sst_props, class == 3)$prop)
  


#run model
mod_sst = brm(ssthd ~ 0 + damcount_adj + npgo_adj + min_flow_adj, data = sst_dat)

mod_sst_lm = lm(ssthd ~ 0 + damcount_adj + npgo_adj + min_flow_adj, data = sst_dat)
#Model diagnostics-

#summaries
summary(mod_sst)

#obs vs. pred
fit_pred = posterior_predict(mod_sst, filter(sst_dat, !is.na(ssthd) & !is.na(damcount_adj)))

obs_pred = tibble(year = filter(sst_dat, !is.na(ssthd) & !is.na(damcount_adj))$year,
            med = apply(fit_pred,2, median),
             q.05 = apply(fit_pred, 2,function(x) quantile(x,0.05)),
            q.95 =apply(fit_pred, 2,function(x) quantile(x, 0.95))
            ) %>%
  left_join(sst_dat %>% dplyr::select(year, ssthd)) %>%
  mutate(q.05 = ifelse(q.05 < 0, 0, q.05)) %>%
  pivot_longer(cols = c('med','ssthd')) %>%
  ggplot()+
  geom_point(aes(x = year, y = value, shape = name))+
  geom_errorbar(aes(x = year, ymin = q.05, ymax = q.95))+
  scale_shape_manual(values = c(16,17),labels = c('predicted','observed'))+
  coord_cartesian(ylim = c(0,770))+
  theme_bw()+
  theme(legend.position = c(.15,.85),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = 'black')
        )+
  labs(y = "Adult Returns")
obs_pred

#Model prediction
predict(mod_sst, sst_dat[nrow(sst_dat),], interval = "prediction", type = "response", level = .9)
#Figures and tables

#NOR winter steelhead----
#data
bon_wst_env = bon %>%
  filter(wsthd_out == 'yes') %>%
  group_by(year) %>%
  dplyr::summarise(mean_spill = mean(spill_kcfs, na.rm = T),
                   temp_out = mean(temperature_c, na.rm = T)) %>%
  ungroup() %>%
  left_join(bon %>%
              filter(wsthd_return == "yes") %>%
              group_by(year) %>%
              dplyr::summarise(temp_in = mean(temperature_c, na.rm = T)) %>%
              ungroup())


wst_env_dat = tucker_flow %>%
  left_join(bon_wst_env) %>%
  left_join(npgo %>%
              filter(month == 5))

wst_ages = read_excel('Data input/Abundance/AgeData.xlsx',
                      sheet = 'NOR WSTHD scales') %>%
  janitor::clean_names() %>%
  pivot_longer(2:last_col(1)) %>%
  dplyr::mutate(fresh = substr(name,2,2),
                salt = substr(name,4,4))

wst_props = bind_rows(wst_ages %>%
                        dplyr::mutate(age = as.numeric(fresh) + as.numeric(salt)) %>%
                        group_by(age, year) %>%
                        dplyr::summarise(prop_yr = age/total) %>%
                        ungroup() %>%
                        group_by(age) %>%
                        dplyr::summarise(prop = mean(prop_yr)) %>%
                        ungroup() %>%
                        dplyr::mutate(type = 'age',
                                      class = as.factor(age)) %>%
                        dplyr::select(type, class, prop),
                        # age_summary %>%
                        # #proportions by age
                        # filter(run == "WSTHD") %>%
                        # pivot_longer(cols = names(.)[-c(1,2,9)]) %>%
                        # dplyr::mutate(type = 'age',
                        #               class = substr(name,5,5),
                        #               prop_yr = value/total) %>%
                        # group_by(type, class) %>%
                        # dplyr::summarise(prop = mean(prop_yr)) %>%
                        # ungroup(),
                      #proportions by fresh
                      wst_ages %>%
                        group_by(year, total, fresh) %>%
                        dplyr::summarise(prop_fresh_yr = sum(value)/total) %>%
                        ungroup() %>%
                        distinct() %>%
                        group_by(fresh) %>%
                        dplyr::summarise(prop = mean(prop_fresh_yr)) %>%
                        ungroup() %>%
                        mutate(type = 'fresh') %>%
                        rename(class = fresh),
                      #proportions by salt
                      wst_ages %>%
                        group_by(year, total, salt) %>%
                        dplyr::summarise(prop_salt_yr = sum(value)/total) %>%
                        ungroup() %>%
                        distinct() %>%
                        group_by(salt) %>%
                        dplyr::summarise(prop = mean(prop_salt_yr)) %>%
                        ungroup() %>%
                        mutate(type = 'salt') %>%
                        rename(class = salt)
)

wst_dat = wst_env_dat %>%
  dplyr::select(-month) %>%
  left_join(smolt_count %>%
  dplyr::select(year, nor_sthd)) %>%
  left_join(ad_count %>%
  dplyr::select(year, nor_wsthd_returns, wsthd_total_escapement)) %>%
  #adjust covs to years
  mutate(esc_adj = ifelse(is.na(lag(wsthd_total_escapement,2)), lag(wsthd_total_escapement,3) * filter(wst_props, type == 'age' & class == 2)$prop,
                          lag(wsthd_total_escapement,2)*filter(wst_props, type == 'age' & class == 2)$prop) +
           lag(wsthd_total_escapement,3) * filter(wst_props, type == 'age' & class == 3)$prop+
           lag(wsthd_total_escapement,4) * filter(wst_props, type == 'age' & class == 4)$prop+
           lag(wsthd_total_escapement,5) * filter(wst_props, type == 'age' & class == 5)$prop+
           ifelse(is.na(lag(wsthd_total_escapement,6)), lag(wsthd_total_escapement,5) * filter(wst_props, type == 'age' & class == 6)$prop,
                  lag(wsthd_total_escapement, 6) * filter(wst_props, type == 'age' & class == 6)$prop),
         smolt_adj = ifelse(is.na(lag(nor_sthd,1)),
                            lag(nor_sthd,2) * filter(wst_props, type == 'salt' & class == 1)$prop,
                            lag(nor_sthd,1) * filter(wst_props, type == 'salt' & class == 1)$prop)+
           lag(nor_sthd,2) * filter(wst_props, type == 'salt' & class == 2)$prop+
           lag(nor_sthd,3) * filter(wst_props, type == 'salt' & class == 3)$prop,
         spill_adj = lag(mean_spill, 1) * filter(wst_props, type == 'salt' & class == 1)$prop+
           lag(mean_spill, 2) * filter(wst_props, type == 'salt' & class == 2)$prop+
           lag(mean_spill, 3) * filter(wst_props, type == 'salt' & class == 3)$prop+
           lag(mean_spill, 4) * filter(wst_props, type == 'salt' & class == 4)$prop,
         min_flow_adj = lag(min_flow_yr, 2) * filter(wst_props, type == 'age' & class == 2)$prop+
           lag(min_flow_yr, 3) * filter(wst_props, type == 'age' & class == 3)$prop+
           lag(min_flow_yr, 4) * filter(wst_props, type == 'age' & class == 4)$prop+
           lag(min_flow_yr, 5) * filter(wst_props, type == 'age' & class == 5)$prop+
           ifelse(is.na(lag(min_flow_yr,6)), lag(min_flow_yr,5) * filter(wst_props, type == 'age' & class == 6)$prop,
                  lag(min_flow_yr, 6) * filter(wst_props, type == 'age' & class == 6)$prop),
         temp_out_adj = lag(temp_out, 1) * filter(wst_props, type == 'salt' & class == 1)$prop+
           lag(temp_out, 2) * filter(wst_props, type == 'salt' & class == 2)$prop+
           lag(temp_out, 3) * filter(wst_props, type == 'salt' & class == 3)$prop+
           lag(temp_out, 4) * filter(wst_props, type == 'salt' & class == 4)$prop,
         npgo_adj = lag(npgo, 1) * filter(wst_props, type == 'salt' & class == 1)$prop+
           lag(npgo, 2) * filter(wst_props, type == 'salt' & class == 2)$prop+
           lag(npgo, 3) * filter(wst_props, type == 'salt' & class == 3)$prop+
           lag(npgo, 4) * filter(wst_props, type == 'salt' & class == 4)$prop) %>%
  mutate(esc_std = (esc_adj - mean(esc_adj, na.rm = T))/sd(esc_adj, na.rm = T),
         spill_std = (spill_adj - mean(spill_adj, na.rm = T))/sd(spill_adj, na.rm = T),
         min_flow_std = (min_flow_adj - mean(min_flow_adj, na.rm = T))/sd(min_flow_adj, na.rm = T),
         npgo_std = (npgo_adj - mean(npgo_adj, na.rm = T))/sd(npgo_adj, na.rm = T),
         temp_in_std = (temp_in - mean(temp_in, na.rm = T))/sd(temp_in, na.rm = T),
         temp_out_std = (temp_out_adj - mean(temp_out_adj, na.rm = T))/sd(temp_out_adj, na.rm = T),
         esc_smoltmod_adj = ifelse(is.na(lag(wsthd_total_escapement, 1)), lag(wsthd_total_escapement,2) * filter(wst_props, type == 'fresh' & class == 1)$prop,
                                   lag(wsthd_total_escapement, 1)*filter(wst_props, type == 'fresh' & class == 1)$prop)+
           lag(wsthd_total_escapement, 2) * filter(wst_props, type == 'fresh' & class == 2)$prop+
           lag(wsthd_total_escapement, 3) * filter(wst_props, type == 'fresh' & class == 3)$prop+
           lag(wsthd_total_escapement, 4) * filter(wst_props, type == 'fresh' & class == 4)$prop,
         min_flow_smoltmod_adj = lag(min_flow_yr,1) * filter(wst_props, type == 'fresh' & class == 1)$prop+
           lag(min_flow_yr,2) * filter(wst_props, type == 'fresh' & class == 2)$prop+
           lag(min_flow_yr,3) * filter(wst_props, type == 'fresh' & class == 3)$prop+
           lag(min_flow_yr,4) * filter(wst_props, type == 'fresh' & class == 4)$prop
         
  )


#Impute smolt abundance for 2021, 2022
wst_smolt_lm = brm(nor_sthd ~ 0+esc_smoltmod_adj + min_flow_smoltmod_adj, data = wst_dat)
summary(wst_smolt_lm)
smolt_pred = predict(wst_smolt_lm, filter(wst_dat, year %in% c(2021,2022)), type = 'response')
smolt_pred_brm = posterior_predict(wst_smolt_lm, filter(wst_dat, year %in% c(2021,2022))) %>%
  apply(2,median)
#run model
mod_wst_brm = brm(nor_wsthd_returns ~ 0 + smolt_adj + min_flow_adj  +  npgo_adj, data = wst_dat)

mod_wst_lm = lm(nor_wsthd_returns ~ 0 + smolt_adj +  min_flow_adj + npgo_adj , data = wst_dat)
#Model diagnostics

#summaries
mod_summary = summary(mod_wst)
mod_summary


#Model prediction

wst_pred_lm = predict.lm(mod_wst, wst_dat[nrow(wst_dat),], type = c("response"))

wst_pred_brm = brmsmargins::brmsmargins(mod_wst, at = wst_dat[nrow(wst_dat),], CI = .9)
wst_post_pred = posterior_predict(mod_wst, wst_dat[nrow(wst_dat),])
predictive_interval(wst_post_pred)


#HOR winter steelhead----
hwst_props = read_excel('Data input/Abundance/AgeData.xlsx',
                        sheet = 'HOR WSTHD') %>%
  janitor::clean_names() %>%
  pivot_longer(2:last_col(1)) %>%
  dplyr::mutate(fresh = substr(name,2,2),
                salt = substr(name,4,4),
                age = as.numeric(fresh)+as.numeric(salt)) %>%
  group_by(year, total, age) %>%
  dplyr::summarise(prop_age_yr = sum(value)/total) %>%
  ungroup() %>%
  distinct() %>%
  group_by(age) %>%
  dplyr::summarise(prop = mean(prop_age_yr)) %>%
  ungroup()

hwst_dat = npgo %>%
  filter(month == 5 | is.na(month)) %>%
  left_join(smolt_count %>%
  dplyr::select(year, hor_wsthd) %>%
  dplyr::rename(hor_smolt = hor_wsthd)) %>%
  left_join(ad_count %>%
              dplyr::select(year, hor_wsthd)) %>%
  mutate(smolt_adj = lag(hor_smolt, 1) * filter(hwst_props, age == '3')$prop +
           lag(hor_smolt, 2) * filter(hwst_props, age == '4')$prop+
           lag(hor_smolt, 3) * filter(hwst_props, age == '5')$prop+
           lag(hor_smolt, 4) * filter(hwst_props, age == '6')$prop,
         npgo_adj = lag(npgo, 1) * filter(hwst_props, age == '3')$prop +
           lag(npgo, 2) * filter(hwst_props, age == '4')$prop +
           lag(npgo, 3) * filter(hwst_props, age == '5')$prop
  )

#Model
hwst_mod_lm = lm(hor_wsthd ~ 0 + smolt_adj + npgo_adj, data = hwst_dat)
summary(hwst_mod_lm)

#predict
hwst_pred = predict(hwst_mod_lm, hwst_dat[nrow(hwst_dat),], interval = "prediction", type = "response", level = .9)

#NOR spring chinook----
#Data manip
nsc_dat = npgo %>%
  filter(month == 9 | is.na(month)) %>%
  left_join(tucker_flow) %>%
  left_join(ad_count %>%
  dplyr::select(year, nor_spch, nor_spch_jack, spch_bon, spch_dalles)) %>%
  mutate(spch_damcount = spch_bon - spch_dalles,
         spch_dam_adj = lag(spch_damcount, 4) * filter(hsc_props, salt == 2)$prop_tot+
           lag(spch_damcount, 5) * filter(hsc_props, salt == 3)$prop_tot,
         min_flow_adj = lag(min_flow_yr, 3) * filter(hsc_props, salt == 2)$prop_tot+
           lag(min_flow_yr, 4) * filter(hsc_props, salt == 3)$prop_tot,
         npgo_adj = lag(npgo, 2) * filter(hsc_props, salt == 2)$prop_tot +
           lag(npgo, 3) * filter(hsc_props, salt == 3)$prop_tot,
         spch_dam_jack_adj = lag(spch_damcount, 3),
         min_flow_jack_adj = lag(min_flow_yr, 2),
         npgo_jack_adj = lag(npgo, 1)
         )

#run model
mod_nsc_lm = lm(nor_spch ~ 0 + spch_dam_adj + min_flow_adj, data = nsc_dat)
summary(mod_nsc_lm)
mod_nsc_brm = brm(nor_spch ~ 0 + spch_dam_adj + min_flow_adj, data = nsc_dat)
summary(mod_nsc_brm)

mod_nsc_jack = lm(nor_spch_jack ~ 0 + spch_dam_jack_adj + npgo_jack_adj, data = nsc_dat)
summary(mod_nsc_jack)
#Model diagnostics
plot(mod_nsc_lm)


#obs vs. pred

#Model prediction
nsc_pred_lm = predict(mod_nsc_lm, nsc_dat[nrow(nsc_dat),], interval = "prediction", type = "response", level = .9)
nsc_pred_lm

nsc_post_pred = posterior_predict(mod_nsc_brm, nsc_dat[nrow(nsc_dat),])
median(nsc_post_pred)
predictive_interval(nsc_post_pred)


nsc_jack_pred = predict(mod_nsc_jack, nsc_dat[nrow(nsc_dat),], interval = "prediction", type = "response", level = .9)
nsc_jack_pred
#Figures and tables


#HOR spring chinook----

hsc_ages = read_excel('Data input/Abundance/AgeData.xlsx',
                      sheet = 'HOR SPCH') %>%
  janitor::clean_names() %>%
  pivot_longer(2:last_col(1)) %>%
  dplyr::mutate(salt = substr(name,2,2),
                salt_prop = value/total)

hsc_props = left_join(hsc_ages %>% group_by(salt) %>%
                        dplyr::summarise(prop_yr = mean(salt_prop)) %>%
                        ungroup(),
                      hsc_ages %>% group_by(salt) %>%
                        dplyr::summarise(prop_tot = sum(value)/sum(total)) %>%
                        ungroup()
                      
)

#Get data together
#abundance
hsc_count = ad_count %>%
  dplyr::select(year, hor_spch, hor_spch_jack) %>%
  left_join(smolt_count %>%
              rename(hor_spch_smolt = hor_spch) %>%
              dplyr::select(year, hor_spch_smolt))

#env
hsc_env =  npgo %>% 
  filter(month == 5 | is.na(month)) %>%
  dplyr::select(year,npgo) %>%
  left_join(bon %>%
  filter(hsc_out == 'yes') %>%
  group_by(year) %>%
  dplyr::summarise(mean_spill = mean(spill_kcfs, na.rm = T),
                   temp_out = mean(temperature_c, na.rm = T)) %>%
  ungroup())

#join together, offset by ages (for both adult and jack model)
hsc_dat = hsc_env %>%
  left_join(hsc_count) %>%
  mutate(smolt_jack_adj = lag(hor_spch_smolt,1),
         smolt_ad_adj_tot = lag(hor_spch_smolt,2)*filter(hsc_props, salt == 2)$prop_tot +
           lag(hor_spch_smolt,3)*filter(hsc_props,salt==3)$prop_tot,
         npgo_jack_adj = lag(npgo,1),
         npgo_ad_adj_tot = lag(npgo,2)*filter(hsc_props, salt == 2)$prop_tot +
           lag(npgo,3)*filter(hsc_props,salt == 3)$prop_tot,
         spill_jack_adj = lag(mean_spill, 1),
         spill_ad_adj_tot = lag(mean_spill,2)*filter(hsc_props, salt == 2)$prop_tot +
           lag(mean_spill,3)*filter(hsc_props,salt == 3)$prop_tot,
         prior_jack_adj_tot = lag(smolt_jack_adj,1)*filter(hsc_props, salt == 2)$prop_tot+
           lag(smolt_jack_adj,2)*filter(hsc_props, salt == 3)$prop_tot,
         prior_returns_adj_tot = lag(smolt_jack_adj,1)*filter(hsc_props, salt == 2)$prop_tot+
           lag(smolt_jack_adj,2)*filter(hsc_props, salt == 3)$prop_tot+
           lag(hor_spch,1)*filter(hsc_props,salt == 3)$prop_tot
  )

#run model
#jacks
mod_hsc_jack = lm(hor_spch_jack ~ 0 + smolt_jack_adj + npgo_jack_adj, data = hsc_dat)
summary(mod_hsc_jack)

#adults
mod_hsc_ad = lm(hor_spch ~ 0 + smolt_ad_adj_tot + npgo_ad_adj_tot, data = hsc_dat)
summary(mod_hsc_ad)

#Model diagnostics
#summaries


#traceplots

#obs vs. pred

#Model prediction
hsc_jack_pred = predict(mod_hsc_jack, hsc_dat[nrow(hsc_dat),], interval = "prediction", type = "response", level = .9)

hsc_ad_pred = predict(mod_hsc_ad, hsc_dat[nrow(hsc_dat),], interval = "prediction", type = "response", level = .9)
#Figures and tables


#Final table