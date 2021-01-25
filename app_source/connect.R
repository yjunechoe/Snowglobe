# connect to database
con <- dbConnect(
  drv = MariaDB(),
  host = "nortonlab.coeodvofteoq.us-east-2.rds.amazonaws.com",
  username = 'nortonadmin',
  password = 'PVk9BU2LUqk',
  port = 3306
)
dbSendQuery(con, "use snowglobe")

# microsoft academic API key
Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287")

# scopus api key // check remaining with `verbose = TRUE` argument
Sys.setenv(ELSEVIER_SCOPUS_KEY = "9c9423562dfa9cef97f2e80c236a5ff1") # dd017ab5c552d4af6089cc6182758186
scopusopts <- list(key = Sys.getenv("ELSEVIER_SCOPUS_KEY"))
