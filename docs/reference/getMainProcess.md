# Get Main Process for a Step

Retrieves the main process configuration for a workflow step. The main
process defines how the step executes, including the tool (R, NONMEM,
etc.), runserver, and execution parameters.

## Usage

``` r
getMainProcess(stepIdent)
```

## Arguments

- stepIdent:

  Step identifier. Can be the step's path, resource ID, entity ID, or
  short entity ID.

## Value

A single-row data frame with 28 columns containing process
configuration:

**Process Identification:**

- id:

  (character) Unique process identifier

- processType:

  (character) Type of process, always "main" for this function

- position:

  (integer) Position in the process list

- name:

  (character) Process name (typically "Main")

- stepId:

  (character) Step ID this process belongs to

**Runserver Configuration:**

- runserverId:

  (character) ID of the runserver

- runserverLabel:

  (character) Label of the runserver

- runserverUrl:

  (character) URL endpoint of the runserver

**Tool Configuration:**

- toolId:

  (character) Tool identifier

- toolLabel:

  (character) Tool label (e.g., "R4.2")

- toolInstance:

  (character) Tool instance name

- toolArgs:

  (character) Tool arguments including environment variables

- toolStreamablePatterns:

  (character) File patterns for streaming (e.g., "*.txt\r\n*.out")

- runserverToolId:

  (character) Runserver-specific tool ID

- gridTool:

  (logical) Whether this is a grid computing tool

- repoTool:

  (logical) Whether this is a repository tool

- main:

  (logical) Whether this is the main process

**Execution Status:**

- runStatus:

  (character) Current status (e.g., "FINISHED", "RUNNING")

- runResult:

  (character) Execution result (e.g., "COMPLETED", "FAILED")

- runId:

  (character) Current or last run identifier

- startedAt:

  (character) Start timestamp in milliseconds since epoch

- stoppedAt:

  (character) Stop timestamp in milliseconds since epoch

- selected:

  (logical) Whether this process is selected for execution

- deleted:

  (logical) Whether this process has been deleted

**Nested Data (list columns):**

- variables:

  List of process variables (e.g., command-file references)

- subProcesses:

  List of subprocess configurations

- gridArguments:

  List of grid computing arguments

- runs:

  List of historical run information

## References

ics1218

## See also

[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)
for retrieving full step environments,
[`runStepResource`](https://improverse.github.io/improveR/reference/runStepResource.md)
for executing steps,
[`loadRunserver`](https://improverse.github.io/improveR/reference/loadRunserver.md)
for runserver details,
[`getToolInstances`](https://improverse.github.io/improveR/reference/getToolInstances.md)
for available tools

## Examples

``` r
if (FALSE) { # \dontrun{
# Get main process for a step
mainProcess <- getMainProcess("/my-project/workflow/Step 1")

# Check execution status
mainProcess$runStatus
mainProcess$runResult

# Get tool information
mainProcess$toolLabel
mainProcess$runserverLabel
} # }
```
