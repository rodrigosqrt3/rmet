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
    "Data;Hora UTC;PRECIPITA TOTAL (mm);PRESSAO ESTACAO (mB);RADIACAO GLOBAL (KJ/m²);TEMP BULBO SECO (°C);UMIDADE HORARIA (%);VENTO VELOCIDADE (m/s);",
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
