Sys.setenv(IMPROVER_STEP="envhost1.hc.scintecodev.internal-5310:FO-19756")
Sys.setenv(IMPROVER_REPO_URL="http://envhost1.hc.scintecodev.internal:5310/repository")
Sys.setenv(TEST_FOLDER = "/Projects/Tests")


Sys.setenv(NONMEM_RUNSERVER="runserver")
Sys.setenv(NONMEM_TOOL_INSTANCE="nonmem_7.4")
Sys.setenv(NONMEM_TOOL="nonmem_7.5")

Sys.setenv(R_RUNSERVER="runserver")
Sys.setenv(R_TOOL_INSTANCE="rbatch")
Sys.setenv(R_TOOL="R_4.2")
Sys.setenv(TEST_SERVER="5310")
devtools::load_all()
clearConnectionData()
improveConnect()
setEditable(T)




#adapt all stephelpers to use main process as default but others possibly
#add gridarguments to process mapping
#loadParentStep
#load over tree boundaries (use DMG 123 testcase)
#create all in same treee, create trees by name, create in
#error message if vpn is missing

#setup mixed testworkflow: nonmem, r, outdated link

#integrate import export directly in library, added hashes to workflow version and latest version to workflow - add container around workflow use imrpoveCLI to actually import and export
#test circular workflow (actually use runs to build workflow) (handles from steps vs handles from runs)
# add sorting of the json to workflow
#parametrised workflows
#rerun check identity with string comparison
#strict rerun rules (rerun with multiple processes,...), review condition? injectable checks
#check those fields
#' @param toolBrowserUrl a URL improve uses to automatically open while the step is running
#' @param toolDeletePatterns files that wont get checked in
#' @param toolStreamablePatterns file that can be streamed to monitor the step
#check step finish use for main tool?
#check variables do exist but needed one not (position)


#dmgResult <- authenticatedREST("/resources/{resourceId}/dmg",
#                            list(resourceId=loadResource(testTree)$resourceId))
#dmg <- httr::content(dmgResult)
