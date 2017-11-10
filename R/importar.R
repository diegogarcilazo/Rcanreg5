#' Import data from export CANREG5 rename columns and check for presence of esential variables.
#'
#' @param dir chr: Directory path. Default file.choose.

c5_import <- function(dir = file.choose(), sep = ',', encoding = 'cp1252', delim = ','){

#read

datos <- readr::read_delim(dir, delim = delim ,locale = locale(encoding = encoding), guess_max = 20000);

#rename
datos <- setNames(datos,
                  stringr::str_replace_all(
                    tolower(myutilities::acc_rm(names(datos))), '[^[:alnum:]]',''));

#35 esential variables
esential_vars <- colnames_list[c(1:3, 6:21, 28:42)];

#Check presence of esential
if(length(intersect(esential_vars, colnames(datos))) == 34){

  message('All columns are OK.\n In addition, serial id named nocaso was added');

  datos$nocaso <- c(1:nrow(datos));

  return(datos);

  } else {

    diff_cols <- setdiff(esential_vars, colnames(datos));

    stop(paste('This columns are NOT present', paste0(diff_cols, collapse = ',')))
}
}


#' Read data from .mul file from IarcTools, rename variables and clean data.
#' @param file chr: Directory file.
#'
read_mul <- function(file = file.choose()){
  #read
  df <- read.delim(file,
  sep = "", skip = 11, header=F);
  #rename
  colnames(df) <-c('region','nocaso','idpersona','sexo','topo','morfo', 'comport','fechadiag','dup');
  #clean
  df2 <- subset(df, !nocaso%in%c('record(s)','duplicate'));

  return(df2);
}
