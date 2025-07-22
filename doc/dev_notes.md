# Development Notes and TODOs

## Roadmap & Implementation Plan

### 1. Interactive Step Navigation
- Implement `usage`, `children`, and `parent` navigation environments, analogous to `lineage`. check
- Each navigation env should support `$load()` to populate related steps for interactive exploration. check 

### 2. Workflow Editing
- Add a method to remove a step from a workflow, enabling tailored workflows. check 

### 3. Full Graph Traversal
- Implement `fullLineage`, `fullUsage`, `fullParent`, and `fullChildren` methods.
- These methods should traverse the step graph recursively, with configurable depth limits. (treeDepth and stepDepth) check

### 4. Workflow Templates (Next Steps)
- Implement `createWorkflowTemplateEnv(workflow)` to generate templates from existing workflows.
- Implement `executeWorkflowTemplate(template, ...)` to instantiate and run workflow templates.
- Ensure new workflow template system supports:
  - Creating templates from trees/workflows
  - Making steps relative
  - Executing workflows (dispatcher support)
  - Managing handles, files, dependencies, and parameters
- Port/replace legacy logic (`handlesFromTree`, `makeStepsRelative`, `executeWorkflow`) as needed.
-getCommandFIleContent
-addContent instead of addLocalFile
unify execute from workflow and workflow template
return workflow after realise

### 5. Execution State Persistence
- Persist the execution state of workflows and steps for reproducibility and tracking.

### 6. Testing & Validation
- Develop comprehensive tests for navigation, workflow editing, template instantiation, and execution state persistence.

---

**Summary:**  
This plan will enable interactive navigation, flexible workflow editing, advanced graph traversal, template-based workflow creation, robust execution tracking, and thorough testing for the improveR system.


## getStep.R

- Step is a fully loaded step, workflow is a fully loaded workflow as df
- All edit functions for a step are in a steptemplate (code sourced into env)
- A steptemplate is always in a workflowtemplate
- A workflowtemplate has a list of all used files and their references
- A workflowtemplate can directly have parameters
- A workflowtemplate has merge / intersect, subset, ... functionalities
- Execute workflow is built in a way that dispatcher works also
- Check if all tool parameters can be set
- Check runs against processes
- Check grid arguments
- Check links
- Check external links
- Check folders
- Two envs: getStep and getStepTemplate. fileList and dependencies handled in workflow
- stepNames: treeName and stepName
- getLineage / get Usage in workflow
- getParentstep in workflow
- removeStep
- makeStepRelative in workflow, dependencies in workflow
- detach from resource and detach from tree in workflow

## createStepTemplateEnv.R

- Attach all other public methods as needed...
- Logic for checking if a step already exists in the target tree and reusing it if possible is not yet implemented.
- Design constraint: maximum folder depth allowed is 1 when moving resources.
- Commented out code in completeToolPresets for future aggregation of process data frames.
- Validation message: Only files or resources can be added to an inventory.
- Validation message: cannot complete a step without process.



