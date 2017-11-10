checkpoint::checkpoint('2015-10-01')
devtools::document()
library(dplyr)
library(lubridate)
library(stringr)
library(reshape2)
library(myutilities)
library(digest)
library(RPostgreSQL)
library(tidyr)
library(Rcanreg5)

dir = 'C:/Users/diego_000/Dropbox/SALA DE SITUACION (compartida) (1)/Registro de tumores/'

datos_crudos = c5_import(dir = paste0(dir, 'basetumores201015.txt'), sep = '\t')

datos_crudos %>% glimpse

datos_crudos %>% transformar %>% recodificar -> datos_ok

datos_ok %>% its_export()

pm <- read.delim('C:/Users/diego_000/Dropbox/SALA DE SITUACION (compartida) (1)/Registro de tumores/chequeos/pm_20151020.tsv')

nocaso_dup <- pm %>% filter(dup%in%c('*','**')) %>% .$nocaso

datos_ok %>% filter(estado == 1 | estado == 0,
                    !nocaso %in% nocaso_dup,
                    year(fechadiag)>=2008,
                    year(fechadiag)<=2012) %>% 
  mutate(aniodiag = year(fechadiag),
         region = str_sub(patientid,1,1)) %>%
  group_by(region_lab, dept_lab, aniodiag) %>% 
  summarise(casos = n()) %>% 
  spread(aniodiag, casos, 0) %>%
  copiar

pm %>%
  filter(!nocaso%in%c('record(s)','duplicate')) %>% 
  select(nocaso, dup) %>%
  mutate(nocaso = as.numeric(as.character(nocaso))) %>%
  right_join(
    datos_ok %>% filter(),
    by = c('nocaso' = 'nocaso2')) %>%
  filter(!dup == '*' | is.na(dup),
         year(fechadiag)>= 2002,
         year(fechadiag)<= 2014) %>%
  group_by(idpersona) %>% arrange(fechadiag) %>%
  mutate(misma_region = n_distinct(region_lab)==1,
         nfilas = row_number(),
         numero = str_pad(rank(fechadiag,ties.method= "first"), 2, pad = 0),
         pm = length(nfilas)>1,
         distinto_sexo = n_distinct(sexo)>1) %>% 
  ungroup() %>% 
  mutate(n_seq = ifelse(pm == FALSE ,'00',numero)) %>%
  arrange(idpersona) %>%
  transmute(idpaciente = str_pad(
    sapply(idpersona, digest,"crc32"), 8, pad = 0),
            n_seq,
            fecha_nacimiento = format(fechan, '%d%m%Y'),
            sexo,
            fecha_incidencia = format(fechadiag, '%d%m%Y'),
            edad,
            topo,
            morf,
            comport,
            base = ifelse(base%in%c('01','02'), 2, ifelse(base == '04', 0, 1)),
            estado_vital = estvit,
            fecha_ultimo_contacto = format(fuc, '%d%m%Y')) %>% glimpse
#filter(idpaciente == '3457f580') %>%
  write.txt(dir = paste0(dir,'ci52.txt'))

ci5<-read.delim(paste0(dir,'datos_entrerios_20012009_2.txt'))  


