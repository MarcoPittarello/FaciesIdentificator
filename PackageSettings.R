#installare git https://git-scm.com/downloads , se ancora non fatto
#in R studio, tools -> global option -> Git -> abilitare e inserire il percorso di git.exe

# 1 - creazione repository su github e copiare link  -----

#2 - R studio, creare nuovo progetto -> version control -> git -> incollare link repository github ----

#3 - nel progetto, eseguire i comandi----

library(usethis)
library(devtools)
library(roxygen2)
library(rmarkdown)
library(pkgdown)

use_git_config(user.name="MarcoPittarello",user.mail="marco.pittarello1987@yahoo.it")#per collegare git con github

## creates description and namespace files ----
usethis::use_description()
usethis::use_namespace()

## Create R directory ----
base::dir.create("R")

## creates Package-level documentation so you can run ?nameofpackage  ----
usethis::use_package_doc()
devtools::document()

## created README.Rmd for Github landing page ----
usethis::use_readme_rmd()

## creates license file ----
usethis::use_gpl_license(version = 3,include_future = T)

## creates news file ----
usethis::use_news_md()

## setup continuous integration via travis-ci ----
#usethis::use_github_actions()

## sets up testing infrastructure ----
#usethis::use_testthat()
#devtools::test()

'#to connect your local package to a remote GitHub repository (URL e BUGS nel description file)
usethis::use_github()'


#4 - riavviare il programma affinchè compaia il label 'build' nella finestra in alto a destra ----

#5- in R studio, tools -> project options -> git-> version control deve essere git  ----

#6- in R studio, tools -> project options -> build tools -> generate documentation with Roxygen ->anche install deve essere spuntato ----

'When you re editing your README.Rmd file, your README.md file is not automatically synchronized.
Since GitHub will display your README.md (and not your README.Rmd file), there is a check that 
you have build your README.md file before pushing it to GitHub. Not doing so would prevent any 
change that you made in the README.Rmd file to appear on your repository.

I would suggest to always use the following workflow :

Edit your README.Rmd file
Build your README.md file by running devtools::build_readme() in the R console
Commit both your README.Rmd and README.md
Doing this should not throw any warning and everything will work the way you probably want.'

devtools::build_readme()

#7 - vignette ----
# generare una vignetta e poi modificarla con rmarkdown
usethis::use_vignette(name="TUTORIAL_2_-_Customized_training_database")
# dopo eseguire il seguente comando
devtools::build_vignettes()

# - 8 - per creare cartelle per contenere dataset usati nel pacchetto, sia file di sistema che pubblici ----
usethis::use_data_raw() #la cartella 'data' contiene dati visibili agli utenti,
#la cartella 'data-raw' contiene database di sistema non visibili agli utenti

usethis::use_data(trainingPie.resnatseed, internal = T,overwrite = T)

# - 9 - Create Package Website ------

# install R package {pkgdown}

usethis::use_pkgdown()

pkgdown::build_site() #and push the new files
pkgdown::preview_site()
#then, option 1
#' on GitHub, go to the settings of your repo, and enable 
#' GitHub Pages using the master branch /docs/ folder. 
#' This will render everything that you have in this folder
#'  as a website (after 0-2 minutes

#option 2
usethis::use_pkgdown_github_pages()

#' done! can take a few minutes to be available online
#' more details at https://pkgdown.r-lib.org/


devtools::check()
devtools::check_man()
devtools::build_manual()

# 10 - tutti i cambiamenti devono essere aggiornati con "build" (finestra alto a destra) e inviati a git (da label Git, commit) ---- 

# 11 - per installare il pacchetto da altri pc ----

library(devtools)
install_github("nomeuser/nome_repository")

install_github("nomeuser/nome_repository")


# Shiny app - collegamento al server 
install.packages('rsconnect')
rsconnect::setAccountInfo(name='marco-pittarello', token='597E84CE5CC18F11E063F82C92A9A105', secret='j2uJO/bWQBYyKs+WLSqKYS45LtMdt/2MeU7TZd8W')


