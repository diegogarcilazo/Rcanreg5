##CREA UNA VISTA CON LAS TRANSFORMACIONES
transformar <- . %>%
  dplyr::mutate(
      fechadiag = lubridate::ymd(fechadiag),
      fechan = lubridate::ymd(fechan),
      fuc = lubridate::ymd(fuc),
      nodoc = as.integer(
        stringr::str_replace_all(nodoc, '^0-9|^0+', '')),
      idpersona = ifelse(is.na(nodoc),
                    paste0(sexo,
                    stringr::str_sub(apellido,1,2),
                    stringr::str_sub(nombres,1,2),
                    stringr::str_sub(format(fechan, '%Y%m%d'),2,6)
                                ), paste0(sexo,nodoc)),
      depto = stringr::str_sub(stringr::str_pad(dept, 5, 'left',pad = 0),1,3),
      depto = ifelse(depto%in%dept_r$codigo, depto, 999),
      base = stringr::str_pad(base,2,pad = 0),
      top = stringr::str_sub(stringr::str_pad(topo,3, pad = 0), 1, 2),
             icd = stringr::str_sub(icd10,1,3),
             base = ifelse(base%in%c('10','11','13','14'),'02',
                           ifelse(base == '12', '03', base))
             )

recodificar <- . %>%
  dplyr::left_join(lookup_tbls$estvit) %>%
  dplyr::left_join(lookup_tbls$base) %>%
  dplyr::left_join(lookup_tbls$sexo) %>%
  dplyr::left_join(lookup_tbls$top) %>%
  dplyr::left_join(lookup_tbls$clasiarc) %>%
  dplyr::left_join(lookup_tbls$comport) %>%
  dplyr::left_join(lookup_tbls$morf) %>%
  dplyr::left_join(lookup_tbls$fuente) %>%
  dplyr::left_join(lookup_tbls$dept_r)
