create_mock_inmet_data <- function(cache_dir, year = 2020) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  csv_lines <- c(
    "REGIAO:;S;",
    "UF:;RS;",
    "ESTACAO:;PORTO ALEGRE;",
    "CODIGO (WMO):;A801;",
    "LATITUDE:;-30,05;",
    "LONGITUDE:;-51,16;",
    "ALTITUDE:;46,97;",
    "DATA DE FUNDACAO:;2000-09-22;",
    "Data;Hora UTC;PRECIPITA TOTAL (mm);PRESSAO ESTACAO (mB);RADIACAO GLOBAL (KJ/m\u00b2);TEMP BULBO SECO (\u00b0C);UMIDADE HORARIA (%);VENTO VELOCIDADE (m/s);",
    "2020/09/25;0000 UTC;0,0;1015,8;-9999;14,7;92;1,3;",
    "2020/09/25;0100 UTC;0,0;1015,2;-9999;14,0;93;0,8;",
    "2020/09/26;0000 UTC;2,5;1010,1;1500;18,5;80;3,5;"
  )

  csv_name <- "INMET_S_RS_A801_PORTO_ALEGRE_01-01-2020_A_31-12-2020.CSV"
  csv_path <- file.path(tempdir(), csv_name)

  con <- file(csv_path, open = "w", encoding = "latin1")
  writeLines(csv_lines, con)
  close(con)

  zip_path <- file.path(cache_dir, paste0(year, ".zip"))
  old_wd <- setwd(tempdir())
  utils::zip(zipfile = zip_path, files = csv_name)
  setwd(old_wd)

  file.remove(csv_path)
  return(zip_path)
}

# Writes station files with the given encoding and zips them as
# <cache_dir>/<year>.zip. `lines` is one character vector (one file) or a list
# of them, with matching `csv_name`. Non-ASCII text must use Unicode escapes.
create_mock_zip <- function(cache_dir, lines, year = 2020, encoding = "latin1",
                            csv_name = "INMET_S_RS_A801_PORTO ALEGRE_01-01-2020_A_31-12-2020.CSV") {
  if (!is.list(lines)) lines <- list(lines)
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  src <- tempfile("rmet-src-")
  dir.create(src)
  for (i in seq_along(lines)) {
    bytes <- iconv(paste0(paste(lines[[i]], collapse = "\r\n"), "\r\n"), from = "UTF-8",
                   to = encoding, toRaw = TRUE)[[1]]
    writeBin(bytes, file.path(src, csv_name[[i]]))
  }
  zip_path <- file.path(cache_dir, paste0(year, ".zip"))
  old_wd <- setwd(src)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zipfile = zip_path, files = csv_name, flags = "-q")
  zip_path
}

# Header and rows in the 2019+ layout (Data / Hora UTC), Latin-1 accents.
inmet_lines_2019 <- function(station_line = "ESTA\u00c7\u00c3O:;PORTO ALEGRE") {
  c(
    "REGI\u00c3O:;S",
    "UF:;RS",
    station_line,
    "CODIGO (WMO):;A801",
    "LATITUDE:;-30,053536",
    "LONGITUDE:;-51,174766",
    "ALTITUDE:;41,18",
    "DATA DE FUNDA\u00c7\u00c3O:;22/09/00",
    paste0(
      "Data;Hora UTC;PRECIPITA\u00c7\u00c3O TOTAL, HOR\u00c1RIO (mm);",
      "TEMPERATURA DO AR - BULBO SECO, HORARIA (\u00b0C);",
      "TEMPERATURA M\u00c1XIMA NA HORA ANT. (AUT) (\u00b0C);",
      "TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (\u00b0C);"
    ),
    "2019/01/01;0000 UTC;0;25,8;26,4;23,4;",
    "2019/01/01;0100 UTC;;;;;",
    "2019/01/01;0200 UTC;1,2;-9999;25,3;-9999,0;",
    "2019/01/01;0300 UTC;0;24,1;24,9"
  )
}

create_mock_inmet_bad_datetime <- function(cache_dir, year = 2020) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  csv_lines <- c(
    "REGIAO:;S;",
    "UF:;RS;",
    "ESTACAO:;PORTO ALEGRE;",
    "CODIGO (WMO):;A801;",
    "LATITUDE:;-30,05;",
    "LONGITUDE:;-51,16;",
    "ALTITUDE:;46,97;",
    "DATA DE FUNDACAO:;2000-09-22;",
    "Data;Hora UTC;PRECIPITA TOTAL (mm);TEMP BULBO SECO (\u00b0C);",
    "25-09-2020;0000 UTC;0,0;14,7;",
    "INVALID;XXXX;0,0;14,0;"
  )

  csv_name <- "bad_datetime.CSV"
  csv_path <- file.path(tempdir(), csv_name)

  writeLines(csv_lines, csv_path)

  zip_path <- file.path(cache_dir, paste0(year, ".zip"))
  old_wd <- setwd(tempdir())
  utils::zip(zipfile = zip_path, files = csv_name)
  setwd(old_wd)

  file.remove(csv_path)
  zip_path
}

create_mock_inmet_allna <- function(cache_dir, year = 2020) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  csv_lines <- c(
    "REGIAO:;S;",
    "UF:;RS;",
    "ESTACAO:;PORTO ALEGRE;",
    "CODIGO (WMO):;A801;",
    "LATITUDE:;-30,05;",
    "LONGITUDE:;-51,16;",
    "ALTITUDE:;46,97;",
    "DATA DE FUNDACAO:;2000-09-22;",
    "Data;Hora UTC;PRECIPITA TOTAL (mm);TEMP BULBO SECO (\u00b0C);",
    "2020/09/25;0000 UTC;;;",
    "2020/09/26;0000 UTC;;;"
  )
  csv_name <- "INMET_S_RS_A801_PORTO_ALEGRE_01-01-2020_A_31-12-2020.CSV"
  csv_path <- file.path(tempdir(), csv_name)
  con <- file(csv_path, open = "w", encoding = "latin1")
  writeLines(csv_lines, con)
  close(con)
  zip_path <- file.path(cache_dir, paste0(year, ".zip"))
  old_wd <- setwd(tempdir())
  utils::zip(zipfile = zip_path, files = csv_name)
  setwd(old_wd)
  file.remove(csv_path)
  zip_path
}
