#' Export data for check with IarcTools.
#' @param df data.frame: data imported and transformed.
#' @param dir chr: Directory path. Default file.choose().
#'
its_export <- function(df, dir = file.choose(), type = 'pm'){

  if(type == 'check'){

    df <- dplyr::transmute(filter(df, estado == 1 | estado == 0),
                     region = stringr::str_sub(patientid, 1,1),
                     nocaso,
                     idpaciente = patientid,
                     sexo,
                     topo = stringr::str_pad(topo,3, pad = 0),
                     morf,
                     comport,
                     estadio,
                     base,
                     fechadediag = format(fechadiag, '%d/%m/%Y'),
                     fechan = format(fechan, '%d/%m/%Y'),
                     edad
    )

  }

  if(type == 'pm'){

    df <- dplyr::transmute(filter(df, estado == 1 | estado == 0),
                           region = stringr::str_sub(patientid, 1,1),
                           nocaso,
                           idpaciente = patientid,
                           sexo,
                           topo = stringr::str_pad(topo,3, pad = 0),
                           morf,
                           comport,
                           fechadediag = format(fechadiag, '%d/%m/%Y')
                           )

  }

    df %>%
    write.csv(
      dir,
      row.names = F, na = '')
}
