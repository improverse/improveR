Sys.setenv(IMPROVER_STEP="envhost1.hc.scintecodev.internal-5310:FO-54947")
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
improveConnect()

testTree <- "envhost1.hc.scintecodev.internal-5310:AT-59469"
workflow <- retrieveWorkflow(handlesFromTree(testTree))

dmgResult <- authenticatedREST("/resources/{resourceId}/dmg",
                            list(resourceId=loadResource(testTree)$resourceId))
dmg <- httr::content(dmgResult)
