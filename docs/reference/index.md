# Package index

## Authentication and Connection

- [`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
  : improveConnect
- [`improveConnected()`](https://improverse.github.io/improveR/reference/improveConnected.md)
  : improveConnected
- [`improveDisconnect()`](https://improverse.github.io/improveR/reference/improveDisconnect.md)
  : improveDisconnect
- [`improveClose()`](https://improverse.github.io/improveR/reference/improveClose.md)
  : improveClose
- [`improveEditable()`](https://improverse.github.io/improveR/reference/improveEditable.md)
  : Verify Write Mode is Enabled (Guard Function)
- [`improveOAuth()`](https://improverse.github.io/improveR/reference/improveOAuth.md)
  : Improve OAuth
- [`improveInit()`](https://improverse.github.io/improveR/reference/improveInit.md)
  : Initialize R Modules from improve Repository
- [`setEditable()`](https://improverse.github.io/improveR/reference/setEditable.md)
  : Enable or Disable Repository Write Mode
- [`clearConnectionData()`](https://improverse.github.io/improveR/reference/clearConnectionData.md)
  : Clear Connection Data
- [`checkConnect()`](https://improverse.github.io/improveR/reference/checkConnect.md)
  : checkConnect
- [`getRepositoryVersion()`](https://improverse.github.io/improveR/reference/getRepositoryVersion.md)
  : Get Repository Version

## Resource Operations

- [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md)
  : Load Resource
- [`loadResourceFromServer()`](https://improverse.github.io/improveR/reference/loadResourceFromServer.md)
  : Load Resource From Server
- [`loadResourceVersionFromServer()`](https://improverse.github.io/improveR/reference/loadResourceVersionFromServer.md)
  : Load Resource Version From Server
- [`refreshResource()`](https://improverse.github.io/improveR/reference/refreshResource.md)
  [`updateResource()`](https://improverse.github.io/improveR/reference/refreshResource.md)
  : Refresh Resource
- [`unloadResource()`](https://improverse.github.io/improveR/reference/unloadResource.md)
  : Unload Resource
- [`loadChildResources()`](https://improverse.github.io/improveR/reference/loadChildResources.md)
  : Load Child Resources
- [`refreshChildResources()`](https://improverse.github.io/improveR/reference/refreshChildResources.md)
  [`updateChildResources()`](https://improverse.github.io/improveR/reference/refreshChildResources.md)
  : Refresh Child Resources from Server
- [`unloadChildResources()`](https://improverse.github.io/improveR/reference/unloadChildResources.md)
  : Unload Child Resources from Cache
- [`loadFullChildResources()`](https://improverse.github.io/improveR/reference/loadFullChildResources.md)
  : loads all child resources by the resourceId, entity ID or entity
  version ID the childResources REST call does not return all the
  fields, like run status for steps, just the fields all resources have
  in common, with this function, the full resources are loaded It Uses
  Caching the results are returned as a data frame or a list of data
  frames the dates are also converted to posix dates via
  convertImproveTimestampToPosix resourceId can be a list
- [`refreshFullChildResources()`](https://improverse.github.io/improveR/reference/refreshFullChildResources.md)
  [`updateFullChildResources()`](https://improverse.github.io/improveR/reference/refreshFullChildResources.md)
  : refreshFullChildResources
- [`unloadFullChildResources()`](https://improverse.github.io/improveR/reference/unloadFullChildResources.md)
  : unloadFullChildResources
- [`createFile()`](https://improverse.github.io/improveR/reference/createFile.md)
  : Create File in improve Repository
- [`createFolder()`](https://improverse.github.io/improveR/reference/createFolder.md)
  : Create Folder in improve Repository
- [`createLink()`](https://improverse.github.io/improveR/reference/createLink.md)
  : Create Link to Resource
- [`createExternalLink()`](https://improverse.github.io/improveR/reference/createExternalLink.md)
  : Create External Link in improve Repository
- [`updateFileContent()`](https://improverse.github.io/improveR/reference/updateFileContent.md)
  : Update File Content Prerequisite: must be a file. Multiple files can
  be updated at once.
- [`uploadFolder()`](https://improverse.github.io/improveR/reference/uploadFolder.md)
  : Upload a Complete Folder Prerequisite: must be a folder.
- [`copy()`](https://improverse.github.io/improveR/reference/copy.md) :
  Copy Resources to Target Location
- [`move()`](https://improverse.github.io/improveR/reference/move.md) :
  Move Resources to Target Location
- [`delete()`](https://improverse.github.io/improveR/reference/delete.md)
  : Delete Resources Permanently
- [`getCopy()`](https://improverse.github.io/improveR/reference/getCopy.md)
  : Get a Local Copy of a File
- [`getFile()`](https://improverse.github.io/improveR/reference/getFile.md)
  : Get a File Object
- [`getFilesFromFolder()`](https://improverse.github.io/improveR/reference/getFilesFromFolder.md)
  : Retrieve a List of Files from a Folder
- [`loadFile()`](https://improverse.github.io/improveR/reference/loadFile.md)
  : Load File
- [`refreshFile()`](https://improverse.github.io/improveR/reference/refreshFile.md)
  [`updateFile()`](https://improverse.github.io/improveR/reference/refreshFile.md)
  : refreshFile
- [`unloadFile()`](https://improverse.github.io/improveR/reference/unloadFile.md)
  : unloadFile
- [`isFileUp2Date()`](https://improverse.github.io/improveR/reference/isFileUp2Date.md)
  : isFileUp2Date
- [`isResourceUp2Date()`](https://improverse.github.io/improveR/reference/isResourceUp2Date.md)
  : Is Resource Up2 Date
- [`updateLinks()`](https://improverse.github.io/improveR/reference/updateLinks.md)
  : Update Link Resources to Latest Target Version
- [`getParent()`](https://improverse.github.io/improveR/reference/getParent.md)
  : Returns the Parent of Anything Resolved by GetCorrectId

## Step and Workflow Management

- [`attachStep()`](https://improverse.github.io/improveR/reference/attachStep.md)
  : Attach a Step to a Parent Step
- [`detachStep()`](https://improverse.github.io/improveR/reference/detachStep.md)
  : Detach Step from Parent
- [`changeStepDescription()`](https://improverse.github.io/improveR/reference/changeStepDescription.md)
  : Change Step Description
- [`changeStepRationale()`](https://improverse.github.io/improveR/reference/changeStepRationale.md)
  : Change Step Rationale
- [`getStep()`](https://improverse.github.io/improveR/reference/getStep.md)
  : Get Step Environment
- [`getStepTemplate()`](https://improverse.github.io/improveR/reference/getStepTemplate.md)
  : Create Step Template
- [`createStepEnv()`](https://improverse.github.io/improveR/reference/createStepEnv.md)
  : Create Step Environment
- [`createStepTemplateEnv()`](https://improverse.github.io/improveR/reference/createStepTemplateEnv.md)
  : Create a Step Template Environment
- [`createAnalysisTree()`](https://improverse.github.io/improveR/reference/createAnalysisTree.md)
  : Create Analysis Tree in improve Repository
- [`createWorkflow()`](https://improverse.github.io/improveR/reference/createWorkflow.md)
  : Create New Workflow Environment
- [`createWorkflowTemplateEnv()`](https://improverse.github.io/improveR/reference/createWorkflowTemplateEnv.md)
  : Create a workflow template environment from an existing workflow
- [`getWorkflow()`](https://improverse.github.io/improveR/reference/getWorkflow.md)
  : Load Workflow Environment
- [`workflowTemplateFromJSON()`](https://improverse.github.io/improveR/reference/workflowTemplateFromJSON.md)
  : Load workflow template from JSON file
- [`createWorkflowTemplateForImport()`](https://improverse.github.io/improveR/reference/createWorkflowTemplateForImport.md)
  : Create a workflow template environment specifically for import
  operations
- [`getMainProcess()`](https://improverse.github.io/improveR/reference/getMainProcess.md)
  : Get Main Process for a Step
- [`getProcessFileVariables()`](https://improverse.github.io/improveR/reference/getProcessFileVariables.md)
  : Get Process File Variables
- [`setCommandFile()`](https://improverse.github.io/improveR/reference/setCommandFile.md)
  : Set Command File on a Realised Step
- [`markStep()`](https://improverse.github.io/improveR/reference/markStep.md)
  : Mark Step Flags
- [`updateProcessProperty()`](https://improverse.github.io/improveR/reference/updateProcessProperty.md)
  : Update a Single Process Property on a Live Step
- [`runStepResource()`](https://improverse.github.io/improveR/reference/runStepResource.md)
  : Run a Step Resource
- [`finishRunResource()`](https://improverse.github.io/improveR/reference/finishRunResource.md)
  : Wait for Step Execution to Finish
- [`terminateStepResource()`](https://improverse.github.io/improveR/reference/terminateStepResource.md)
  : Terminate a Running Step Resource
- [`loadParentStep()`](https://improverse.github.io/improveR/reference/loadParentStep.md)
  : Loads the Parent Step of One Step, Not Applicable to Multiple Steps
- [`refreshParentStep()`](https://improverse.github.io/improveR/reference/refreshParentStep.md)
  [`updateParentStep()`](https://improverse.github.io/improveR/reference/refreshParentStep.md)
  : Refresh Parent Step from Server
- [`unloadParentStep()`](https://improverse.github.io/improveR/reference/unloadParentStep.md)
  : Unload Parent Step
- [`loadChildSteps()`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
  : Load Child Steps
- [`refreshChildSteps()`](https://improverse.github.io/improveR/reference/refreshChildSteps.md)
  [`updateChildSteps()`](https://improverse.github.io/improveR/reference/refreshChildSteps.md)
  : Refresh Child Steps from Server
- [`unloadChildSteps()`](https://improverse.github.io/improveR/reference/unloadChildSteps.md)
  : Unload Child Steps from Cache
- [`loadParentalDescendant()`](https://improverse.github.io/improveR/reference/loadParentalDescendant.md)
  : Load the parental/descendant relationships of a resource
- [`refreshParentalDescendant()`](https://improverse.github.io/improveR/reference/refreshParentalDescendant.md)
  [`updateParentalDescendant()`](https://improverse.github.io/improveR/reference/refreshParentalDescendant.md)
  : Refresh parental/descendant cache for a resource
- [`unloadParentalDescendant()`](https://improverse.github.io/improveR/reference/unloadParentalDescendant.md)
  : Unload parental/descendant cache for a resource

## Tool and Runserver Management

- [`loadToolCategories()`](https://improverse.github.io/improveR/reference/loadToolCategories.md)
  : Load Tool Categories
- [`loadToolsForCategory()`](https://improverse.github.io/improveR/reference/loadToolsForCategory.md)
  : Load Tools for a Category
- [`createToolCategory()`](https://improverse.github.io/improveR/reference/createToolCategory.md)
  : Create a Tool Category
- [`renameToolCategory()`](https://improverse.github.io/improveR/reference/renameToolCategory.md)
  : Rename a Tool Category
- [`deleteToolCategory()`](https://improverse.github.io/improveR/reference/deleteToolCategory.md)
  : Delete a Tool Category
- [`createTool()`](https://improverse.github.io/improveR/reference/createTool.md)
  : Create a Tool in a Category
- [`createToolInstance()`](https://improverse.github.io/improveR/reference/createToolInstance.md)
  : Create a Tool Instance on a Runserver
- [`updateToolInstance()`](https://improverse.github.io/improveR/reference/updateToolInstance.md)
  : Update a Tool Instance
- [`createToolParameter()`](https://improverse.github.io/improveR/reference/createToolParameter.md)
  : Create a Tool Parameter on an Instance
- [`updateToolParameter()`](https://improverse.github.io/improveR/reference/updateToolParameter.md)
  : Update a Tool Parameter Value
- [`getToolInstances()`](https://improverse.github.io/improveR/reference/getToolInstances.md)
  : Get Tool Instances
- [`resetToolInstances()`](https://improverse.github.io/improveR/reference/resetToolInstances.md)
  : Reset Tool Instance Cache
- [`loadRunserver()`](https://improverse.github.io/improveR/reference/loadRunserver.md)
  : Loads Runserver By Label

## Import and Export

- [`exportFolder()`](https://improverse.github.io/improveR/reference/exportFolder.md)
  : Export a Folder Hierarchy to a Portable Zip File
- [`importFolder()`](https://improverse.github.io/improveR/reference/importFolder.md)
  : Import a Folder Hierarchy from a Zip File into a Repository Folder
- [`exportWorkflow()`](https://improverse.github.io/improveR/reference/exportWorkflow.md)
  : Export a Workflow to a Portable Zip File
- [`importWorkflow()`](https://improverse.github.io/improveR/reference/importWorkflow.md)
  : Import a Workflow from a zip File into a Repository Folder

## Permissions and Access Control

- [`getResourcePermissions()`](https://improverse.github.io/improveR/reference/getResourcePermissions.md)
  : Get Resource ACL Entries
- [`effectiveUserPermissions()`](https://improverse.github.io/improveR/reference/effectiveUserPermissions.md)
  : Get Effective Permissions for All Users
- [`effectiveRights()`](https://improverse.github.io/improveR/reference/effectiveRights.md)
  : Get Effective User Rights for Resource
- [`setResourcePermission()`](https://improverse.github.io/improveR/reference/setResourcePermission.md)
  : Set a Permission on a Resource
- [`replaceResourcePermissions()`](https://improverse.github.io/improveR/reference/replaceResourcePermissions.md)
  : Replace All Permissions on a Resource (Bulk PUT)
- [`updateResourcePermission()`](https://improverse.github.io/improveR/reference/updateResourcePermission.md)
  : Update an Existing Permission on a Resource
- [`removeResourcePermission()`](https://improverse.github.io/improveR/reference/removeResourcePermission.md)
  : Remove a Permission from a Resource
- [`loadGroups()`](https://improverse.github.io/improveR/reference/loadGroups.md)
  : List All Groups
- [`loadGroup()`](https://improverse.github.io/improveR/reference/loadGroup.md)
  : Get Group Details
- [`loadGroupUsers()`](https://improverse.github.io/improveR/reference/loadGroupUsers.md)
  : Get Group Members
- [`createGroup()`](https://improverse.github.io/improveR/reference/createGroup.md)
  : Create a Group
- [`deleteGroup()`](https://improverse.github.io/improveR/reference/deleteGroup.md)
  : Delete a Group
- [`addGroupUser()`](https://improverse.github.io/improveR/reference/addGroupUser.md)
  : Add a User to a Group
- [`removeGroupUser()`](https://improverse.github.io/improveR/reference/removeGroupUser.md)
  : Remove a User from a Group
- [`addSubgroup()`](https://improverse.github.io/improveR/reference/addSubgroup.md)
  : Add a Subgroup to a Group
- [`users()`](https://improverse.github.io/improveR/reference/users.md)
  : List All Users in improve Repository
- [`whoami()`](https://improverse.github.io/improveR/reference/whoami.md)
  : Get Current Authenticated User

## Favorites

- [`loadFavorites()`](https://improverse.github.io/improveR/reference/loadFavorites.md)
  : Load Favorites
- [`loadFavoriteChildren()`](https://improverse.github.io/improveR/reference/loadFavoriteChildren.md)
  : Load Favorite Children
- [`addFavoriteLink()`](https://improverse.github.io/improveR/reference/addFavoriteLink.md)
  : Add Favorite Link
- [`createFavoriteFolder()`](https://improverse.github.io/improveR/reference/createFavoriteFolder.md)
  : Create Favorite Folder
- [`removeFavorite()`](https://improverse.github.io/improveR/reference/removeFavorite.md)
  : Remove Favorite

## Reviews

- [`createReview()`](https://improverse.github.io/improveR/reference/createReview.md)
  : Creates a New Review
- [`getReviewById()`](https://improverse.github.io/improveR/reference/getReviewById.md)
  : Get Review By ID
- [`acceptReviewInvitation()`](https://improverse.github.io/improveR/reference/acceptReviewInvitation.md)
  : Accept Review Invitation
- [`declineReviewInvitation()`](https://improverse.github.io/improveR/reference/declineReviewInvitation.md)
  : Decline Review Invitation
- [`changeReviewStatus()`](https://improverse.github.io/improveR/reference/changeReviewStatus.md)
  : Change Review Status
- [`loadReviews()`](https://improverse.github.io/improveR/reference/loadReviews.md)
  : Loads All Registered Reviews
- [`refreshReviews()`](https://improverse.github.io/improveR/reference/refreshReviews.md)
  [`updateReviews()`](https://improverse.github.io/improveR/reference/refreshReviews.md)
  : Reloads the Reviews
- [`unloadReviews()`](https://improverse.github.io/improveR/reference/unloadReviews.md)
  : Unloads All Reviews
- [`createReviewer()`](https://improverse.github.io/improveR/reference/createReviewer.md)
  : Adds a User as Reviewer to a Review
- [`deleteReviewer()`](https://improverse.github.io/improveR/reference/deleteReviewer.md)
  : Removes a Reviewer from a Review
- [`getReviewers()`](https://improverse.github.io/improveR/reference/getReviewers.md)
  : Get Reviewers
- [`loadReviewers()`](https://improverse.github.io/improveR/reference/loadReviewers.md)
  : Loads All The Reviewers For A Review
- [`refreshReviewers()`](https://improverse.github.io/improveR/reference/refreshReviewers.md)
  [`updateReviewers()`](https://improverse.github.io/improveR/reference/refreshReviewers.md)
  : Reloads the Reviewers for a Review
- [`unloadReviewers()`](https://improverse.github.io/improveR/reference/unloadReviewers.md)
  : Unloads the Reviewers for a Review
- [`approveReviewEntries()`](https://improverse.github.io/improveR/reference/approveReviewEntries.md)
  : Approve Review Entries
- [`rejectReviewEntries()`](https://improverse.github.io/improveR/reference/rejectReviewEntries.md)
  : Reject Review Entries
- [`resetReviewEntries()`](https://improverse.github.io/improveR/reference/resetReviewEntries.md)
  : Reset Review Entry Approval State
- [`createReviewEntry()`](https://improverse.github.io/improveR/reference/createReviewEntry.md)
  : Add Review Entries
- [`deleteReviewEntry()`](https://improverse.github.io/improveR/reference/deleteReviewEntry.md)
  : Delete Review Entries
- [`getReviewEntries()`](https://improverse.github.io/improveR/reference/getReviewEntries.md)
  : Get Review Entries
- [`loadReviewEntries()`](https://improverse.github.io/improveR/reference/loadReviewEntries.md)
  : Loads All The Review Entries For A Review
- [`refreshReviewEntries()`](https://improverse.github.io/improveR/reference/refreshReviewEntries.md)
  [`updateReviewEntries()`](https://improverse.github.io/improveR/reference/refreshReviewEntries.md)
  : Reloads the Review Entries for a Review
- [`unloadReviewEntries()`](https://improverse.github.io/improveR/reference/unloadReviewEntries.md)
  : Unloads the Review Entries for a Review
- [`createReviewComment()`](https://improverse.github.io/improveR/reference/createReviewComment.md)
  : Creates a Comment for a Review
- [`createReviewEntryComment()`](https://improverse.github.io/improveR/reference/createReviewEntryComment.md)
  : Creates a Comment for a Review Entry
- [`getReviewComments()`](https://improverse.github.io/improveR/reference/getReviewComments.md)
  : Get Review Comments
- [`getReviewEntryComments()`](https://improverse.github.io/improveR/reference/getReviewEntryComments.md)
  : Get Review Entry Comments
- [`loadReviewComments()`](https://improverse.github.io/improveR/reference/loadReviewComments.md)
  : Loads All The Review Comments For A Review
- [`refreshReviewComments()`](https://improverse.github.io/improveR/reference/refreshReviewComments.md)
  [`updateReviewComments()`](https://improverse.github.io/improveR/reference/refreshReviewComments.md)
  : Reloads the Review Comments for a Review
- [`unloadReviewComments()`](https://improverse.github.io/improveR/reference/unloadReviewComments.md)
  : Unloads the Review Comments for a Review

## Metadata and Resource Relations

- [`addMetaDate()`](https://improverse.github.io/improveR/reference/addMetaDate.md)
  : Adds Metadata Value To A Resource, Always Adds, Never Updates
- [`addBulkMetaDate()`](https://improverse.github.io/improveR/reference/addBulkMetaDate.md)
  : Adds Metadata Value To A Resource, Always Adds, Never Updates
- [`updateMetaDate()`](https://improverse.github.io/improveR/reference/updateMetaDate.md)
  : Updates Metadata Value To A Resource
- [`updateMetaDateById()`](https://improverse.github.io/improveR/reference/updateMetaDateById.md)
  : Updates Metadata Value To A Resource
- [`deleteMetaDate()`](https://improverse.github.io/improveR/reference/deleteMetaDate.md)
  : Deletes Metadata From A Resource
- [`getMetaData()`](https://improverse.github.io/improveR/reference/getMetaData.md)
  : Gets a Metadata Data Frame
- [`getMetaDataMap()`](https://improverse.github.io/improveR/reference/getMetaDataMap.md)
  : Loads the Metadata of a Given Resource and Writes It as String
  Values Into a List
- [`loadMetaData()`](https://improverse.github.io/improveR/reference/loadMetaData.md)
  : Loads the History by the ResourceId, Entity ID or Entity Version ID
  it uses caching the results are returned as a data frame or a list of
  data frames the dates are also converted to posix dates via
  convertImproveTimestampToPosix resourceId can be a list
- [`refreshMetaData()`](https://improverse.github.io/improveR/reference/refreshMetaData.md)
  [`updateMetaData()`](https://improverse.github.io/improveR/reference/refreshMetaData.md)
  : Refresh Meta Data
- [`unloadMetaData()`](https://improverse.github.io/improveR/reference/unloadMetaData.md)
  : Unload Meta Data
- [`loadMetaDataDefinition()`](https://improverse.github.io/improveR/reference/loadMetaDataDefinition.md)
  : Loads One Meta Data Definitions By Name For A Scope, The Default
  Scope Is "Improve Client"
- [`loadMetaDataDefinitionPickList()`](https://improverse.github.io/improveR/reference/loadMetaDataDefinitionPickList.md)
  : Loads One Meta Data Definition Picklist Values By Name For A Scope,
  The Default Scope Is "Improve Client"
- [`loadMetaDataDefinitions()`](https://improverse.github.io/improveR/reference/loadMetaDataDefinitions.md)
  : Loads All Meta Data Definitions For A Scope, The Default Scope Is
  "Improve Client"
- [`refreshMetaDataDefinitions()`](https://improverse.github.io/improveR/reference/refreshMetaDataDefinitions.md)
  [`updateMetaDataDefinitions()`](https://improverse.github.io/improveR/reference/refreshMetaDataDefinitions.md)
  : Refresh Meta Data Definitions
- [`unloadMetaDataDefinitions()`](https://improverse.github.io/improveR/reference/unloadMetaDataDefinitions.md)
  : Unload Meta Data Definitions
- [`createResourceRelation()`](https://improverse.github.io/improveR/reference/createResourceRelation.md)
  : Creates a New Resource Relation
- [`updateResourceRelation()`](https://improverse.github.io/improveR/reference/updateResourceRelation.md)
  : Updates a Resource Relation
- [`deleteResourceRelation()`](https://improverse.github.io/improveR/reference/deleteResourceRelation.md)
  : Deletes a Resource Relation
- [`loadResourceRelations()`](https://improverse.github.io/improveR/reference/loadResourceRelations.md)
  : Loads All Registered Resource Relations
- [`refreshResourceRelations()`](https://improverse.github.io/improveR/reference/refreshResourceRelations.md)
  [`updateResourceRelations()`](https://improverse.github.io/improveR/reference/refreshResourceRelations.md)
  : Reloads the Resource Relations
- [`unloadResourceRelations()`](https://improverse.github.io/improveR/reference/unloadResourceRelations.md)
  : Unloads All Resource Relations
- [`createRelationType()`](https://improverse.github.io/improveR/reference/createRelationType.md)
  : Create a New Relation Type
- [`updateRelationType()`](https://improverse.github.io/improveR/reference/updateRelationType.md)
  : Update an Existing Relation Type
- [`deleteRelationType()`](https://improverse.github.io/improveR/reference/deleteRelationType.md)
  : Delete a Relation Type
- [`loadRelationTypes()`](https://improverse.github.io/improveR/reference/loadRelationTypes.md)
  : Loads All Registered Relation Types
- [`refreshRelationTypes()`](https://improverse.github.io/improveR/reference/refreshRelationTypes.md)
  [`updateRelationTypes()`](https://improverse.github.io/improveR/reference/refreshRelationTypes.md)
  : Reloads the Relation Types
- [`unloadRelationTypes()`](https://improverse.github.io/improveR/reference/unloadRelationTypes.md)
  : Unloads All Relation Types

## Resource Lifecycle

- [`finishResource()`](https://improverse.github.io/improveR/reference/finishResource.md)
  : Finish Resource
- [`reopenResource()`](https://improverse.github.io/improveR/reference/reopenResource.md)
  : Reopen Resource
- [`lockResource()`](https://improverse.github.io/improveR/reference/lockResource.md)
  : Lock Resource for Exclusive Editing
- [`unlockResource()`](https://improverse.github.io/improveR/reference/unlockResource.md)
  : Unlock Resource to Allow Collaborative Access

## Transactions

- [`getLatestRevision()`](https://improverse.github.io/improveR/reference/getLatestRevision.md)
  : Get Latest Revision
- [`getLatestRun()`](https://improverse.github.io/improveR/reference/getLatestRun.md)
  : Get Latest Run
- [`getRun()`](https://improverse.github.io/improveR/reference/getRun.md)
  : Get Run
- [`getRunGridArguments()`](https://improverse.github.io/improveR/reference/getRunGridArguments.md)
  : Get Run Grid Arguments
- [`getRunParameters()`](https://improverse.github.io/improveR/reference/getRunParameters.md)
  : Get Run Parameters
- [`getRunPhases()`](https://improverse.github.io/improveR/reference/getRunPhases.md)
  : Get Run Phases
- [`createTransaction()`](https://improverse.github.io/improveR/reference/createTransaction.md)
  : Create Transaction

## History and Audit Trail

- [`loadHistory()`](https://improverse.github.io/improveR/reference/loadHistory.md)
  : Loads the History by the ResourceId, Entity ID or Entity Version ID
  it uses caching the results are returned as a data frame or a list of
  data frames the dates are also converted to posix dates via
  convertImproveTimestampToPosix resourceId can be a list
- [`refreshHistory()`](https://improverse.github.io/improveR/reference/refreshHistory.md)
  [`updateHistory()`](https://improverse.github.io/improveR/reference/refreshHistory.md)
  : refreshHistory
- [`unloadHistory()`](https://improverse.github.io/improveR/reference/unloadHistory.md)
  : unloadHistory
- [`loadAuditTrail()`](https://improverse.github.io/improveR/reference/loadAuditTrail.md)
  : Load Audit Trail
- [`refreshAuditTrail()`](https://improverse.github.io/improveR/reference/refreshAuditTrail.md)
  [`updateAuditTrail()`](https://improverse.github.io/improveR/reference/refreshAuditTrail.md)
  : Refresh Audit Trail from Server
- [`unloadAuditTrail()`](https://improverse.github.io/improveR/reference/unloadAuditTrail.md)
  : Unload Audit Trail from Cache
- [`getFullFolderAuditTrail()`](https://improverse.github.io/improveR/reference/getFullFolderAuditTrail.md)
  : Retrieve Recursive Audit Trail For A Specific Folder
- [`loadReferences()`](https://improverse.github.io/improveR/reference/loadReferences.md)
  : Loads the References by the ResourceId, Entity ID or Entity Version
  ID it uses caching the results are returned as a data frame or a list
  of data frames the dates are also converted to posix dates via
  convertImproveTimestampToPosix resourceId can be a list
- [`refreshReferences()`](https://improverse.github.io/improveR/reference/refreshReferences.md)
  [`updateReferences()`](https://improverse.github.io/improveR/reference/refreshReferences.md)
  : Refresh References
- [`unloadReferences()`](https://improverse.github.io/improveR/reference/unloadReferences.md)
  : Unload References

## CLI Tools

- [`cloneCli()`](https://improverse.github.io/improveR/reference/cloneCli.md)
  : Clone CLI Resource
- [`pullCli()`](https://improverse.github.io/improveR/reference/pullCli.md)
  : Pull CLI
- [`pushCli()`](https://improverse.github.io/improveR/reference/pushCli.md)
  : Push CLI
- [`pushRunCli()`](https://improverse.github.io/improveR/reference/pushRunCli.md)
  : Push Run CLI
- [`versionCli()`](https://improverse.github.io/improveR/reference/versionCli.md)
  : Version CLI
- [`createContentCache()`](https://improverse.github.io/improveR/reference/createContentCache.md)
  : Create Content Cache

## Token Management

- [`refreshToken()`](https://improverse.github.io/improveR/reference/refreshToken.md)
  : Refresh token using registered refresher
- [`applyTokenData()`](https://improverse.github.io/improveR/reference/applyTokenData.md)
  : Apply token data to session
- [`updateAccessToken()`](https://improverse.github.io/improveR/reference/updateAccessToken.md)
  : Update access token in current session
- [`getCurrentTokenData()`](https://improverse.github.io/improveR/reference/getCurrentTokenData.md)
  : Get current token data
- [`shouldRefreshToken()`](https://improverse.github.io/improveR/reference/shouldRefreshToken.md)
  : Check if token should be refreshed
- [`getActiveTokenRefresher()`](https://improverse.github.io/improveR/reference/getActiveTokenRefresher.md)
  : Get active token refresher
- [`getTokenRefresher()`](https://improverse.github.io/improveR/reference/getTokenRefresher.md)
  : Get registered token refresher
- [`listTokenRefreshers()`](https://improverse.github.io/improveR/reference/listTokenRefreshers.md)
  : List registered token refreshers
- [`registerTokenRefresher()`](https://improverse.github.io/improveR/reference/registerTokenRefresher.md)
  : Register a token refresher plugin

## Data Retrieval Helpers

- [`getData()`](https://improverse.github.io/improveR/reference/getData.md)
  : Get Data Table from improve Repository
- [`getGraphics()`](https://improverse.github.io/improveR/reference/getGraphics.md)
  : Get Graphics File from improve Repository
- [`getHTML()`](https://improverse.github.io/improveR/reference/getHTML.md)
  : Get HTML File from improve Repository
- [`getR()`](https://improverse.github.io/improveR/reference/getR.md) :
  Get R Script or RDS Object from improve Repository
- [`getTextString()`](https://improverse.github.io/improveR/reference/getTextString.md)
  : Get Text File as Character String
- [`includeGraphics()`](https://improverse.github.io/improveR/reference/includeGraphics.md)
  : Include Graphics in Knitr Chunk
- [`showGraphics()`](https://improverse.github.io/improveR/reference/showGraphics.md)
  : Generate Markdown Syntax for Graphics
- [`showHTML()`](https://improverse.github.io/improveR/reference/showHTML.md)
  : Includes HTML in rMarkdown, creates a string that has to be
  outputted
- [`sourceR()`](https://improverse.github.io/improveR/reference/sourceR.md)
  : Source R Scripts from improve Repository

## Query and Search

- [`query()`](https://improverse.github.io/improveR/reference/query.md)
  : Search Resources Using Elasticsearch Query
- [`queryFolder()`](https://improverse.github.io/improveR/reference/queryFolder.md)
  : Search Resources Within a Folder Using Elasticsearch Query

## SSH Tunnels

- [`improveSetupSSH()`](https://improverse.github.io/improveR/reference/improveSetupSSH.md)
  : Set Up SSH Keys for Tunnel Access
- [`improveOpenTunnel()`](https://improverse.github.io/improveR/reference/improveOpenTunnel.md)
  : Open SSH Tunnel to a Running Step
- [`improveCloseTunnel()`](https://improverse.github.io/improveR/reference/improveCloseTunnel.md)
  : Close an SSH Tunnel

## Utilities

- [`getCorrectId()`](https://improverse.github.io/improveR/reference/getCorrectId.md)
  : getCorrectId
- [`normalisePath()`](https://improverse.github.io/improveR/reference/normalisePath.md)
  : normalisePath
- [`repoPrefix()`](https://improverse.github.io/improveR/reference/repoPrefix.md)
  : Get the Repository Entity ID Prefix
- [`reexports`](https://improverse.github.io/improveR/reference/reexports.md)
  [`%>%`](https://improverse.github.io/improveR/reference/reexports.md)
  : Objects exported from other packages
- [`byNotEmpty()`](https://improverse.github.io/improveR/reference/byNotEmpty.md)
  : Apply Function Row-Wise With Null Protection
- [`byNotEmptyAsDf()`](https://improverse.github.io/improveR/reference/byNotEmptyAsDf.md)
  : Apply Function Row-Wise and Merge Results to Data Frame
- [`convertImproveTimestampToPosix()`](https://improverse.github.io/improveR/reference/convertImproveTimestampToPosix.md)
  : Convert Improve Timestamp to POSIX
- [`getImproveInternalDir()`](https://improverse.github.io/improveR/reference/getImproveInternalDir.md)
  : Get improve internal directory
- [`getImproveWorkspaceDir()`](https://improverse.github.io/improveR/reference/getImproveWorkspaceDir.md)
  : Get improve workspace directory
- [`getLogFile()`](https://improverse.github.io/improveR/reference/getLogFile.md)
  : Get Log File
- [`lastRestError()`](https://improverse.github.io/improveR/reference/lastRestError.md)
  : Last REST Error
- [`log_debug()`](https://improverse.github.io/improveR/reference/log_debug.md)
  : Log_debug
- [`log_error()`](https://improverse.github.io/improveR/reference/log_error.md)
  : Log_error
- [`log_info()`](https://improverse.github.io/improveR/reference/log_info.md)
  : Log_info
- [`log_warn()`](https://improverse.github.io/improveR/reference/log_warn.md)
  : Log_warn
- [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md) :
  Pwd
- [`resetCache()`](https://improverse.github.io/improveR/reference/resetCache.md)
  : Reset Cache
- [`strip()`](https://improverse.github.io/improveR/reference/strip.md)
  : Strip
- [`unauthenticatedREST()`](https://improverse.github.io/improveR/reference/unauthenticatedREST.md)
  : Unauthenticated REST

## Cache Refresh (deprecated update\* aliases)

Functions that clear and reload cached data. The `update*` variants are
deprecated aliases for their `refresh*` counterparts.

- [`refreshChildResources()`](https://improverse.github.io/improveR/reference/refreshChildResources.md)
  [`updateChildResources()`](https://improverse.github.io/improveR/reference/refreshChildResources.md)
  : Refresh Child Resources from Server
- [`refreshChildSteps()`](https://improverse.github.io/improveR/reference/refreshChildSteps.md)
  [`updateChildSteps()`](https://improverse.github.io/improveR/reference/refreshChildSteps.md)
  : Refresh Child Steps from Server
- [`refreshFullChildResources()`](https://improverse.github.io/improveR/reference/refreshFullChildResources.md)
  [`updateFullChildResources()`](https://improverse.github.io/improveR/reference/refreshFullChildResources.md)
  : refreshFullChildResources
- [`refreshParentStep()`](https://improverse.github.io/improveR/reference/refreshParentStep.md)
  [`updateParentStep()`](https://improverse.github.io/improveR/reference/refreshParentStep.md)
  : Refresh Parent Step from Server
- [`refreshParentalDescendant()`](https://improverse.github.io/improveR/reference/refreshParentalDescendant.md)
  [`updateParentalDescendant()`](https://improverse.github.io/improveR/reference/refreshParentalDescendant.md)
  : Refresh parental/descendant cache for a resource
- [`refreshReviewComments()`](https://improverse.github.io/improveR/reference/refreshReviewComments.md)
  [`updateReviewComments()`](https://improverse.github.io/improveR/reference/refreshReviewComments.md)
  : Reloads the Review Comments for a Review
- [`refreshReviewEntries()`](https://improverse.github.io/improveR/reference/refreshReviewEntries.md)
  [`updateReviewEntries()`](https://improverse.github.io/improveR/reference/refreshReviewEntries.md)
  : Reloads the Review Entries for a Review
- [`refreshReviewers()`](https://improverse.github.io/improveR/reference/refreshReviewers.md)
  [`updateReviewers()`](https://improverse.github.io/improveR/reference/refreshReviewers.md)
  : Reloads the Reviewers for a Review
- [`refreshReviews()`](https://improverse.github.io/improveR/reference/refreshReviews.md)
  [`updateReviews()`](https://improverse.github.io/improveR/reference/refreshReviews.md)
  : Reloads the Reviews
- [`refreshFile()`](https://improverse.github.io/improveR/reference/refreshFile.md)
  [`updateFile()`](https://improverse.github.io/improveR/reference/refreshFile.md)
  : refreshFile
- [`refreshResource()`](https://improverse.github.io/improveR/reference/refreshResource.md)
  [`updateResource()`](https://improverse.github.io/improveR/reference/refreshResource.md)
  : Refresh Resource
