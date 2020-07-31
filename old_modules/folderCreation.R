suppressMessages({
  library(faosws)
  library(sendmailR)

})

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  SETTINGS = ReadSettings("sws.yml")
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
}

path <- file.path(Sys.getenv("R_SWS_SHARE_PATH"))

setwd(path)

dir.create("population")


dir.create("population/total")
dir.create("population/female")
dir.create("population/male")
dir.create("population/urban")
dir.create("population/rural")

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "Population folder"
body = "The folders have been successfully created"


sendmail(from = from, to = to, subject = subject, msg = body)



message("The folders have been successfully created.")

