setupSWBEnv24 <- function() {
  Sys.setenv(IMPROVER_STEP="repo-a.env24:ST-78792")
  Sys.setenv(IMPROVER_REPO_URL="https://repo-a.env24.scintecodev.com:8443/repository")
  Sys.setenv(IMPROVER_USER="admin")
  Sys.setenv(IMPROVER_PASSWORD="admin")
  Sys.setenv(IMPROVER_TOKEN="")

  Sys.setenv(TEST_FOLDER = "/SWB/Projects/BIOSTAT-2371")

  Sys.setenv(NONMEM_RUNSERVER="run-c.env26")
  Sys.setenv(NONMEM_TOOL_INSTANCE="Nonmem7.4")
  Sys.setenv(NONMEM_TOOL="Nonmem 7.4")

  Sys.setenv(R_RUNSERVER="run-a.env24")
  Sys.setenv(R_TOOL_INSTANCE="improverbatch")
  Sys.setenv(R_TOOL="R")

}

setupRepo10 <- function() {
  Sys.setenv(IMPROVER_STEP="repo10long:ST-1467816")
  Sys.setenv(IMPROVER_REPO_URL="https://repo10a.scinteco.com:8443/repository")
  Sys.setenv(IMPROVER_USER="admin")
  Sys.setenv(IMPROVER_PASSWORD="admin")
  Sys.setenv(IMPROVER_TOKEN="")
  Sys.setenv(TEST_FOLDER = "/Projects/Tests")
}


setupRepo34 <- function() {
  Sys.setenv(IMPROVER_STEP="repo-a.env34:ST-98332")
  Sys.setenv(IMPROVER_REPO_URL="https://repo-a.env34.scintecodev.com:8443/repository")
  Sys.setenv(IMPROVER_USER="admin")
  Sys.setenv(IMPROVER_PASSWORD="admin")
  Sys.setenv(IMPROVER_TOKEN="")
  Sys.setenv(TEST_FOLDER = "/Projects/Tests")
}

setupRepoDemo02 <- function() {
  Sys.setenv(IMPROVER_STEP="repo-a.demo02:FI-33051")
  Sys.setenv(IMPROVER_REPO_URL="https://repo-a.demo02.scintecodev.com:8443/repository")
  Sys.setenv(IMPROVER_USER="admin")
  Sys.setenv(IMPROVER_PASSWORD="admin")
  Sys.setenv(IMPROVER_TOKEN="")
  Sys.setenv(TEST_FOLDER = "/Projects/Tests")
  Sys.setenv(NONMEM_RUNSERVER="run-c.env26")
  Sys.setenv(NONMEM_TOOL_INSTANCE="Nonmem7.4")
  Sys.setenv(NONMEM_TOOL="Nonmem 7.4")

  Sys.setenv(R_RUNSERVER="demo02")
  Sys.setenv(R_TOOL_INSTANCE="improveRbatch")
  Sys.setenv(R_TOOL="R 4.2")
}

setupRobertEnv <- function() {
  Sys.setenv(IMPROVER_STEP="robert_oracle-1:ST-33703")
  Sys.setenv(IMPROVER_REPO_URL="http://envhost1.hc.scintecodev.internal:4210/repository")
  Sys.setenv(IMPROVER_USER="admin")
  Sys.setenv(IMPROVER_PASSWORD="admin")
  Sys.setenv(IMPROVER_TOKEN="")
  Sys.setenv(TEST_FOLDER = "/Projects/Tests")
  Sys.setenv("DB_DSN"="ORCLPDB1")
  Sys.setenv("DB_USER"="im_robert_oracle1")
  Sys.setenv("DB_PASSWORD"="sc1nt3c0")


  Sys.setenv(NONMEM_RUNSERVER="runserver1-docker-lsf")
  Sys.setenv(NONMEM_TOOL_INSTANCE="nonmem_7.4-lsf")
  Sys.setenv(NONMEM_TOOL="nonmem_7.4")

  Sys.setenv(R_RUNSERVER="runserver1-docker-lsf")
  Sys.setenv(R_TOOL_INSTANCE="rbatch")
  Sys.setenv(R_TOOL="R_4.2")
}



#improveRcore::improveConnect(secure=F)
#improveRcore::loadFile("repo_name_todo:FO-101",filePath = "./Library")
#improveRcore::loadFile("repo_name_todo:FI-205",filePath = "./Library")
#improveRcore::loadFile("repo_name_todo:FI-254",filePath = "./Library")
