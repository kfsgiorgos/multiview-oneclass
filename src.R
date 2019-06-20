# selected_configuration <- "macBook"

if(selected_configuration == "macBook"){
  print("1")
  config <- config::get(file = "~/R Language Default Dir/Github-projects/multiview-oneclass/config.yml")
  print("2")
  setwd(config::get("directory"))
  print("3")
  source(config::get("source_scripts"))
  print("4")
  use_condaenv(config::get("conda_env"))
  print("5")
  }



