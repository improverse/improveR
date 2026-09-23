# API v2 — Step Specification (Core Atom)

**Date:** 2026-02-18 **Status:** Draft for discussion with Repository
Team **Prerequisite for:** Workflow assembly, template creation,
execution **Source of truth:** Based on actual v1 REST payloads, process
structures, tool/runserver configuration, and grid argument handling
from the improve and improveR codebases.

------------------------------------------------------------------------

## 1. Design Principle

A step is the fundamental unit of work. In v2, a step is always
represented as a **single, complete JSON object** — no assembly from
sub-resources. Every read returns the full object. Every create accepts
the full object.

------------------------------------------------------------------------

## 2. The Step Object

### 2.1 Complete JSON Schema (as returned by GET)

``` json
{
  "resourceId": "uuid",
  "entityId": "ST-12345",
  "entityVersionId": "STV-67890",
  "revisionId": "rev-uuid",

  "name": "Step 3",
  "description": "Clean and transform raw data",
  "rationale": "Required for downstream modeling",
  "comment": "improveRW",

  "tree": {
    "resourceId": "uuid",
    "entityId": "AT-100",
    "name": "Modeling",
    "path": "/Project/Modeling"
  },

  "parentStep": {
    "resourceId": "uuid",
    "entityId": "ST-12300",
    "name": "Step 1",
    "inheritFromParent": true
  },

  "classification": {
    "keyStep": false,
    "baseModel": false,
    "fullModel": false,
    "finalModel": false,
    "referenceModel": false
  },

  "processes": [
    {
      "id": "proc-uuid",
      "name": "Main",
      "position": 1,
      "processType": "main",
      "main": true,
      "selected": true,
      "deleted": false,

      "runserverId": "rs-uuid",
      "runserverLabel": "Linux Server 1",
      "runserverUrl": "https://runserver1.example.com",

      "runserverToolId": "rst-uuid",
      "toolId": "tool-uuid",
      "toolLabel": "R 4.3",
      "toolInstance": "Rbatch",

      "toolArgs": "R_LIBS=/opt/R/libs\nR CMD BATCH --vanilla",
      "toolStreamablePatterns": "*.txt\r\n*.out\r\n*.Rout",
      "toolIgnorePatterns": "*.tmp",
      "toolDeletePatterns": "",
      "toolBrowserUrl": "",

      "gridTool": false,
      "repoTool": false,

      "runStatus": "FINISHED",
      "runResult": "COMPLETED",
      "runId": "run-uuid",
      "startedAt": "1708243200000",
      "stoppedAt": "1708244700000",

      "variables": [
        {
          "id": "var-uuid",
          "type": "processVariable",
          "name": "command-file",
          "position": 1,
          "variableType": "fileRef",
          "valueResourceId": "file-uuid",
          "processId": "proc-uuid"
        }
      ],

      "gridArguments": [],

      "subProcesses": [],

      "runs": [
        {
          "id": "run-uuid",
          "processId": "proc-uuid",
          "startedAt": "1708243200000",
          "stoppedAt": "1708244700000",
          "status": "FINISHED",
          "result": "COMPLETED"
        }
      ]
    },
    {
      "id": "proc2-uuid",
      "name": "GridProcess",
      "position": 2,
      "processType": "main",
      "main": false,
      "selected": true,
      "deleted": false,

      "runserverId": "rs-uuid",
      "runserverLabel": "Cluster 1",
      "runserverUrl": "https://cluster1.example.com",

      "runserverToolId": "rst2-uuid",
      "toolId": "tool2-uuid",
      "toolLabel": "NONMEM 7.5",
      "toolInstance": "nm75-cpu",

      "toolArgs": "",
      "toolStreamablePatterns": "*.ext\r\n*.lst",
      "toolIgnorePatterns": "",
      "toolDeletePatterns": "FCON FDATA",
      "toolBrowserUrl": "",

      "gridTool": true,
      "repoTool": false,

      "runStatus": "INITIAL",
      "runResult": null,
      "runId": null,
      "startedAt": null,
      "stoppedAt": null,

      "variables": [],

      "gridArguments": [
        {
          "id": "ga-uuid",
          "definitionId": "gadef-uuid",
          "name": "NODES",
          "gridArgumentType": "TEXT",
          "textValue": "8",
          "lovValueId": null,
          "categoryId": null,
          "dateValue": null
        },
        {
          "id": "ga2-uuid",
          "definitionId": "gadef2-uuid",
          "name": "NGDR",
          "gridArgumentType": "LOV",
          "lovValueId": "lov-uuid",
          "categoryId": "cat-uuid",
          "textValue": null,
          "dateValue": null
        }
      ],

      "subProcesses": [],
      "runs": []
    }
  ],

  "inventory": [
    {
      "resourceId": "file-uuid",
      "entityId": "FIV-500",
      "name": "output.csv",
      "nodeType": "FIV",
      "inventoryPath": "/results/output.csv",
      "fileHash": "md5:abc123def456",
      "lastModifiedOn": "1708244700000",
      "revisionId": "rev-uuid"
    },
    {
      "resourceId": "link-uuid",
      "entityId": "LIV-600",
      "name": "input_data.csv",
      "nodeType": "LIV",
      "inventoryPath": "/input/input_data.csv",
      "targetEntityId": "source-entity-id",
      "targetRevisionId": "target-rev-uuid",
      "outdatedLink": false,
      "fileHash": "md5:789xyz",
      "lastModifiedOn": "1708158000000"
    },
    {
      "resourceId": "folder-uuid",
      "entityId": "FOV-700",
      "name": "scripts",
      "nodeType": "FOV",
      "children": [
        {
          "resourceId": "script-uuid",
          "entityId": "FIV-701",
          "name": "main.R",
          "nodeType": "FIV",
          "inventoryPath": "/scripts/main.R",
          "fileHash": "md5:script123",
          "lastModifiedOn": "1708239600000"
        }
      ]
    },
    {
      "resourceId": "extlink-uuid",
      "name": "Documentation",
      "nodeType": "ExtLink",
      "url": "https://docs.example.com/analysis"
    }
  ],

  "lastModifiedOn": "1708244700000",
  "path": "/Project/Modeling/Step 3",
  "deleted": false,
  "ownedBy": "user@example.com"
}
```

------------------------------------------------------------------------

### 2.2 Field Reference

#### Step (top level)

| Field             | Type    | Read |    Create    | Update | Notes                                                                |
|-------------------|---------|:----:|:------------:|:------:|----------------------------------------------------------------------|
| `resourceId`      | string  | yes  |      —       |   —    | Server-generated UUID                                                |
| `entityId`        | string  | yes  |      —       |   —    | Server-generated (ST-nnnnn)                                          |
| `entityVersionId` | string  | yes  |      —       |   —    | Server-generated                                                     |
| `revisionId`      | string  | yes  |      —       |   —    | Server-generated, for version tracking                               |
| `name`            | string  | yes  |   optional   |  yes   | **Auto-generated** (Step 1, Step 2, …)                               |
| `description`     | string  | yes  |   optional   |  yes   | Default: “”                                                          |
| `rationale`       | string  | yes  |   optional   |  yes   | Default: “”                                                          |
| `comment`         | string  | yes  |   optional   |  yes   | Default: “”                                                          |
| `tree`            | object  | yes  | **required** |   —    | Identifies the analysis tree                                         |
| `parentStep`      | object  | yes  |   optional   |  yes   | null = no parent. Triggers inheritance on create. Can be cross-tree. |
| `classification`  | object  | yes  |   optional   |  yes   | All default false                                                    |
| `processes`       | array   | yes  | conditional  |  yes   | **Required** if no parentStep. Optional with parentStep (inherited). |
| `inventory`       | array   | yes  |   optional   |   —    | Links at creation. Files via S3 upload.                              |
| `path`            | string  | yes  |      —       |   —    | Server-managed full path                                             |
| `lastModifiedOn`  | string  | yes  |      —       |   —    | Epoch milliseconds                                                   |
| `deleted`         | boolean | yes  |      —       |   —    | Server-managed                                                       |
| `ownedBy`         | string  | yes  |      —       |   —    | Server-managed                                                       |

#### tree

| Field        | Type   |  Create  | Notes                                        |
|--------------|--------|:--------:|----------------------------------------------|
| `resourceId` | string | option A | One of resourceId / entityId / path required |
| `entityId`   | string | option B |                                              |
| `path`       | string | option C | e.g., “/Project/Modeling”                    |
| `name`       | string |    —     | Server-resolved (read only)                  |

#### parentStep

| Field               | Type    |  Create  | Notes                                    |
|---------------------|---------|:--------:|------------------------------------------|
| `resourceId`        | string  | option A | One of resourceId / entityId required    |
| `entityId`          | string  | option B |                                          |
| `name`              | string  |    —     | Server-resolved (read only)              |
| `inheritFromParent` | boolean |    —     | Read only. Set by server on child steps. |

**Inheritance behavior:** When a step is created with `parentStep`, the
server copies the parent’s process configuration (tool, runserver,
toolArgs, etc.) and links to input files. This is a **snapshot at
creation time** — later changes to the parent do not propagate. If the
client provides explicit `processes` or `inventory` in the create
request, they **override** the inherited values.

#### classification

| Field            | Type    | Default | Notes          |
|------------------|---------|:-------:|----------------|
| `keyStep`        | boolean |  false  | Milestone step |
| `baseModel`      | boolean |  false  |                |
| `fullModel`      | boolean |  false  |                |
| `finalModel`     | boolean |  false  |                |
| `referenceModel` | boolean |  false  |                |

------------------------------------------------------------------------

### 2.3 Process

A process defines how a step executes. Each step has at least one
process. The `main` process determines the step’s execution status.

| Field                            | Type    | Read |    Create    | Update | Notes                                                                                         |
|----------------------------------|---------|:----:|:------------:|:------:|-----------------------------------------------------------------------------------------------|
| `id`                             | string  | yes  |      —       |   —    | Server-generated                                                                              |
| `name`                           | string  | yes  | **required** |  yes   | “Main”, “Post”, custom name                                                                   |
| `position`                       | integer | yes  | **required** |  yes   | Execution order, ≥ 1                                                                          |
| `processType`                    | enum    | yes  | **required** |  yes   | `main`, `sub`, `post`                                                                         |
| `main`                           | boolean | yes  | **required** |  yes   | Exactly one per step must be true                                                             |
| `selected`                       | boolean | yes  |   optional   |  yes   | Default: true. If false, skipped during run.                                                  |
| `deleted`                        | boolean | yes  |      —       |   —    | Server-managed                                                                                |
| **Runserver**                    |         |      |              |        |                                                                                               |
| `runserverId`                    | string  | yes  | **required** |  yes   | UUID of the runserver                                                                         |
| `runserverLabel`                 | string  | yes  |      —       |   —    | Read only. Server-resolved from runserverId.                                                  |
| `runserverUrl`                   | string  | yes  |      —       |   —    | Read only. Server-resolved from runserverId.                                                  |
| **Tool**                         |         |      |              |        |                                                                                               |
| `runserverToolId`                | string  | yes  | **required** |  yes   | ID of the tool instance on the runserver. This is the primary tool identifier for a process.  |
| `toolId`                         | string  | yes  |      —       |   —    | Read only. Tool category ID, resolved from runserverToolId.                                   |
| `toolLabel`                      | string  | yes  |      —       |   —    | Read only. e.g., “R 4.3”, “NONMEM 7.5”. Resolved from toolId.                                 |
| `toolInstance`                   | string  | yes  |      —       |   —    | Read only. e.g., “Rbatch”, “nm75-cpu”. Resolved from runserverToolId.                         |
| **Tool Arguments**               |         |      |              |        |                                                                                               |
| `toolArgs`                       | string  | yes  |   optional   |  yes   | Command-line args, env vars. Multi-line string. Comes from tool parameters if not overridden. |
| `toolStreamablePatterns`         | string  | yes  |   optional   |  yes   | File patterns for live monitoring. `\r\n`-separated.                                          |
| `toolIgnorePatterns`             | string  | yes  |   optional   |  yes   | Files to ignore                                                                               |
| `toolDeletePatterns`             | string  | yes  |   optional   |  yes   | Files to delete after run (not checked in)                                                    |
| `toolBrowserUrl`                 | string  | yes  |   optional   |  yes   | URL to open during run                                                                        |
| **Grid**                         |         |      |              |        |                                                                                               |
| `gridTool`                       | boolean | yes  | **required** |  yes   | If true, tool uses grid computing (e.g., SGE, SLURM)                                          |
| `repoTool`                       | boolean | yes  |      —       |   —    | Read only.                                                                                    |
| **Execution Status** (read only) |         |      |              |        |                                                                                               |
| `runStatus`                      | string  | yes  |      —       |   —    | `INITIAL`, `RUNNING`, `FINISHED`, `FAILED`, `TERMINATED`                                      |
| `runResult`                      | string  | yes  |      —       |   —    | `COMPLETED`, `FAILED`, `TERMINATED`, null                                                     |
| `runId`                          | string  | yes  |      —       |   —    | Current/last run UUID                                                                         |
| `startedAt`                      | string  | yes  |      —       |   —    | Epoch milliseconds                                                                            |
| `stoppedAt`                      | string  | yes  |      —       |   —    | Epoch milliseconds                                                                            |
| **Sub-objects**                  |         |      |              |        |                                                                                               |
| `variables`                      | array   | yes  |   optional   |  yes   | Process file variables                                                                        |
| `gridArguments`                  | array   | yes  |   optional   |  yes   | Only relevant when gridTool=true                                                              |
| `subProcesses`                   | array   | yes  |      —       |   —    | Sub-process definitions (for processType=sub)                                                 |
| `runs`                           | array   | yes  |      —       |   —    | Historical run data                                                                           |
| `parentProcessId`                | string  |  —   | conditional  |   —    | **Required** when processType=“sub”                                                           |

**Key insight: how tool selection works.** The client selects a tool by
specifying `runserverId` + `runserverToolId`. These two fields together
uniquely identify a tool instance on a specific runserver. All the
label/name fields (`runserverLabel`, `toolLabel`, `toolInstance`, etc.)
are **server-resolved** — the client never needs to send them.

The relationship chain:

    runserverId → identifies the runserver (has: label, url, hostname)
    runserverToolId → identifies a tool instance on that runserver (has: name/toolInstance, gridProvider)
      ↳ linked to toolId → identifies the tool in a category (has: toolLabel, categoryName)
         ↳ linked to toolCategory (has: categoryIdentifier)

**Where toolArgs come from:** When a tool is configured on a runserver,
it has `parameters` (loaded from
`/configuration/runservers/{runserverId}/tools/{toolId}/parameters`).
These parameters include default tool arguments. The `toolArgs` field on
a process is either: - Copied from the tool’s default parameters (at
step creation), or - Explicitly set/overridden by the client

------------------------------------------------------------------------

### 2.4 Process Variable

Process variables bind files to a process. They define which files from
the inventory are inputs to the process.

| Field             | Type    |    Create    | Notes                                                                             |
|-------------------|---------|:------------:|-----------------------------------------------------------------------------------|
| `id`              | string  |      —       | Server-generated                                                                  |
| `type`            | const   | **required** | Always `"processVariable"`                                                        |
| `name`            | string  | **required** | Variable name. Unique within process.                                             |
| `position`        | integer | **required** | Order in variable list, ≥ 1                                                       |
| `variableType`    | enum    | **required** | `fileRef` or `filePath`                                                           |
| `valueResourceId` | string  | conditional  | **Required** for `fileRef`. Points to a file/link resource in the step inventory. |
| `processId`       | string  |      —       | Server-set. References parent process.                                            |

**How variables work:** - A `fileRef` variable links to an improve
resource (a file or link in the step’s inventory) via `valueResourceId`.
The runserver uses this to know which file is the “command-file” or
“data” for the process. - A `filePath` variable points to a literal path
on the runserver filesystem. - Variables are the bridge between
inventory items and processes — without a variable, a file exists in the
step but the process doesn’t “know about it.”

------------------------------------------------------------------------

### 2.5 Grid Arguments

Grid arguments configure parallel/cluster execution parameters. They
only apply to processes where `gridTool=true`.

| Field              | Type   |    Create    | Notes                                                               |
|--------------------|--------|:------------:|---------------------------------------------------------------------|
| `id`               | string |      —       | Server-generated                                                    |
| `definitionId`     | string | **required** | References a grid argument definition from the grid provider        |
| `name`             | string |      —       | Server-resolved from definitionId                                   |
| `gridArgumentType` | enum   |      —       | Server-resolved: `LOV`, `TEXT`, `DATE_TIME`                         |
| `categoryId`       | string | conditional  | **Required** for LOV type                                           |
| `lovValueId`       | string | conditional  | **Required** for LOV type. Must be a valid value from the category. |
| `textValue`        | string | conditional  | **Required** for TEXT type                                          |
| `dateValue`        | string | conditional  | **Required** for DATE_TIME type. Epoch milliseconds as string.      |

**How grid arguments work:** 1. A tool instance on a runserver has a
`gridProvider` (e.g., “SGE”, “SLURM”, “LSF”) 2. The grid provider
defines available argument definitions via
`GET /configuration/gridArguments/{gridProvider}/definitions` 3. Each
definition has a name (e.g., “NODES”, “NGDR”, “QUEUE”), a type
(LOV/TEXT/DATE_TIME), and for LOV types, a `category` with `values`
(list of `{id, text}` pairs) 4. The process stores grid argument
**values** (not definitions) — one value per definition used 5. For LOV:
client must look up the `lovValueId` matching the desired text value
from the category 6. For TEXT: client sends the value as `textValue` 7.
For DATE_TIME: client sends epoch milliseconds as `dateValue`

**Grid argument definition structure** (from
`/configuration/gridArguments/{gridProvider}/definitions`):

``` json
[
  {
    "id": "gadef-uuid",
    "name": "NODES",
    "gridArgumentType": "TEXT"
  },
  {
    "id": "gadef2-uuid",
    "name": "NGDR",
    "gridArgumentType": "LOV",
    "categoryId": "cat-uuid",
    "category": [
      {
        "values": [
          { "id": "lov1", "text": "1" },
          { "id": "lov2", "text": "5" },
          { "id": "lov3", "text": "10" }
        ]
      }
    ]
  },
  {
    "id": "gadef3-uuid",
    "name": "DEADLINE",
    "gridArgumentType": "DATE_TIME"
  }
]
```

------------------------------------------------------------------------

### 2.6 Inventory Items

The step inventory contains four node types:

#### FIV (File Inventory Version) — a file within the step

| Field            | Type   | Read |    Create    | Notes                                  |
|------------------|--------|:----:|:------------:|----------------------------------------|
| `resourceId`     | string | yes  |      —       | Server-generated                       |
| `entityId`       | string | yes  |      —       | Server-generated (FIV-nnnnn)           |
| `name`           | string | yes  | **required** | File name                              |
| `nodeType`       | const  | yes  |      —       | `"FIV"` (or `"File"` in some contexts) |
| `inventoryPath`  | string | yes  | **required** | Relative path in step                  |
| `fileHash`       | string | yes  |      —       | Server-computed MD5                    |
| `lastModifiedOn` | string | yes  |      —       | Epoch milliseconds                     |
| `revisionId`     | string | yes  |      —       | Server-managed                         |

File content is uploaded separately via pre-authenticated S3 links.

#### LIV (Link Inventory Version) — a link to a file in another step

| Field              | Type    | Read |    Create    | Notes                                               |
|--------------------|---------|:----:|:------------:|-----------------------------------------------------|
| `resourceId`       | string  | yes  |      —       | Server-generated                                    |
| `entityId`         | string  | yes  |      —       | Server-generated (LIV-nnnnn)                        |
| `name`             | string  | yes  |   optional   | Default: target file name                           |
| `nodeType`         | const   | yes  |      —       | `"LIV"` (or `"Link"`)                               |
| `inventoryPath`    | string  | yes  | **required** | Relative path in step                               |
| `targetEntityId`   | string  | yes  | **required** | Entity ID of the file being linked                  |
| `targetRevisionId` | string  | yes  |      —       | Specific revision (for version pinning)             |
| `outdatedLink`     | boolean | yes  |      —       | Server-computed. True if target has newer revision. |
| `fileHash`         | string  | yes  |      —       | Hash of linked file                                 |
| `lastModifiedOn`   | string  | yes  |      —       | Epoch milliseconds of linked file                   |

Links are the mechanism for step-to-step data flow. A link from Step B
to a file in Step A creates a dependency: B depends on A.

#### FOV (Folder Overview Version) — a subfolder in the step

| Field        | Type   | Read |    Create    | Notes                    |
|--------------|--------|:----:|:------------:|--------------------------|
| `resourceId` | string | yes  |      —       | Server-generated         |
| `entityId`   | string | yes  |      —       | Server-generated         |
| `name`       | string | yes  | **required** | Folder name              |
| `nodeType`   | const  | yes  |      —       | `"FOV"`                  |
| `children`   | array  | yes  |      —       | Nested FIV/LIV/FOV items |

Folders are auto-created from inventory paths on step creation.

#### ExtLink — an external URL reference

| Field        | Type   | Read |    Create    | Notes            |
|--------------|--------|:----:|:------------:|------------------|
| `resourceId` | string | yes  |      —       | Server-generated |
| `name`       | string | yes  | **required** | Display name     |
| `nodeType`   | const  | yes  |      —       | `"ExtLink"`      |
| `url`        | string | yes  | **required** | Target URL       |

------------------------------------------------------------------------

## 3. Endpoints

### 3.1 Read Step

    GET /api/v2/steps/{stepId}

**Path parameter:** `stepId` — UUID (resourceId), entity ID (ST-12345),
or full path

**Query parameters:** \| Param \| Type \| Notes \| \|——-\|——\|——-\| \|
`fields` \| string \| Optional. Comma-separated list of top-level fields
to return (e.g., `resourceId,name,processes`). Default: all fields. \|

**Response:** Full step object (Section 2.1), or partial if `fields`
specified

**Status codes:** - `200` — Success - `404` — Step not found - `403` —
No read permission

------------------------------------------------------------------------

### 3.2 List Steps in Tree

    GET /api/v2/steps?treeId={treeId}

**Query parameters:** \| Param \| Type \| Required \| Notes \|
\|——-\|——\|:——–:\|——-\| \| `treeId` \| string \| yes \| Tree UUID,
entity ID, or path \| \| `fields` \| string \| no \| Partial response \|

**Response:**

``` json
{
  "steps": [ /* full or partial step objects */ ],
  "total": 10
}
```

------------------------------------------------------------------------

### 3.3 Create Step

    POST /api/v2/steps

#### Minimal example (standalone step):

``` json
{
  "tree": { "resourceId": "tree-uuid" },
  "processes": [
    {
      "name": "Main",
      "position": 1,
      "processType": "main",
      "main": true,
      "gridTool": false,
      "runserverId": "rs-uuid",
      "runserverToolId": "rst-uuid"
    }
  ]
}
```

#### Minimal example (child step — inherits everything from parent):

``` json
{
  "tree": { "resourceId": "tree-uuid" },
  "parentStep": { "resourceId": "parent-step-uuid" }
}
```

#### Full example (with inventory, variables, grid arguments):

``` json
{
  "description": "Analyze cleaned data",
  "rationale": "Core analysis step",
  "tree": { "path": "/Project/Modeling" },
  "parentStep": { "entityId": "ST-12300" },
  "classification": {
    "keyStep": true
  },
  "processes": [
    {
      "name": "Main",
      "position": 1,
      "processType": "main",
      "main": true,
      "selected": true,
      "gridTool": false,
      "runserverId": "rs-uuid",
      "runserverToolId": "rst-uuid",
      "toolArgs": "R_LIBS=/opt/R/libs\nR CMD BATCH --vanilla",
      "variables": [
        {
          "type": "processVariable",
          "name": "command-file",
          "position": 1,
          "variableType": "fileRef"
        }
      ],
      "resources": [
        {
          "sourceResourceId": "upstream-file-uuid",
          "targetName": "input_data.csv",
          "variableName": "command-file",
          "operation": "COPY"
        }
      ]
    },
    {
      "name": "GridRun",
      "position": 2,
      "processType": "main",
      "main": false,
      "selected": true,
      "gridTool": true,
      "runserverId": "cluster-uuid",
      "runserverToolId": "nm75-uuid",
      "gridArguments": [
        {
          "definitionId": "gadef-uuid",
          "textValue": "8"
        },
        {
          "definitionId": "gadef2-uuid",
          "lovValueId": "lov-uuid",
          "categoryId": "cat-uuid"
        }
      ]
    }
  ],
  "inventory": [
    {
      "nodeType": "Link",
      "inventoryPath": "/input/data.csv",
      "targetEntityId": "source-file-entity-id"
    },
    {
      "nodeType": "ExtLink",
      "name": "Reference Docs",
      "url": "https://docs.example.com"
    }
  ],
  "run": false
}
```

**Note the `resources` array on the process:** This is how files get
mapped to a process at creation time. Each entry says: “take
`sourceResourceId`, put it in the step as `targetName`, optionally bind
it to a variable (`variableName`), and optionally COPY instead of LINK
(`operation`=COPY).” This is the v1 mechanism used by `prepareProcess()`
in createStepTemplateEnv.R.

**Behaviors:** - Atomic: all-or-nothing, rolls back on any failure -
`"run": true` → creates and immediately starts execution - `name`
auto-generated as “Step 1”, “Step 2”, etc. Override by providing
`name`. - When `parentStep` is set without explicit `processes`: server
copies parent’s process configuration and input links (snapshot at
creation time). Explicit fields override inherited values. - Folders in
inventory auto-created from paths - File content uploaded separately via
pre-authenticated S3 links - Response is the full step object — no need
to GET after POST

**Response:** `201 Created` with full step object

**Status codes:** - `201` — Created - `400` — Validation error (see
Section 4) - `404` — Tree/parent/tool/runserver not found - `403` — No
create permission

------------------------------------------------------------------------

### 3.4 Update Step

    PATCH /api/v2/steps/{stepId}

**Nested field-level partial update.** Only send changed fields.

**Example: update description and toolArgs on one process:**

``` json
{
  "description": "Updated description",
  "processes": [
    {
      "id": "existing-proc-uuid",
      "toolArgs": "R_LIBS=/opt/R/newlibs\nR CMD BATCH --vanilla --no-save"
    }
  ]
}
```

**Example: add a grid argument to an existing process:**

``` json
{
  "processes": [
    {
      "id": "proc-uuid",
      "gridArguments": [
        { "definitionId": "gadef-uuid", "textValue": "16" }
      ]
    }
  ]
}
```

**Example: update a single process variable’s target:**

``` json
{
  "processes": [
    {
      "id": "proc-uuid",
      "variables": [
        { "id": "var-uuid", "valueResourceId": "new-file-uuid" }
      ]
    }
  ]
}
```

**Update semantics for nested arrays:** - Item with `id` → update that
existing item (merge fields) - Item without `id` → create new item - To
remove: `{ "id": "item-to-remove", "_delete": true }`

**Response:** Full updated step object

**Status codes:** - `200` — Updated - `400` — Validation error - `404` —
Step not found - `403` — No write permission - `409` — Step is currently
RUNNING

------------------------------------------------------------------------

### 3.5 Delete Step

    DELETE /api/v2/steps/{stepId}

**Status codes:** - `204` — Deleted - `404` — Not found - `403` — No
delete permission - `409` — Step is RUNNING (must terminate first)

------------------------------------------------------------------------

### 3.6 Run Step

    POST /api/v2/steps/{stepId}/run

Non-blocking. Returns immediately.

**Response:**

``` json
{
  "runId": "uuid",
  "runStatus": "RUNNING",
  "startedAt": "1708243200000"
}
```

**Status codes:** - `202` — Accepted - `409` — Already RUNNING - `412` —
Cannot run (runStatus must be INITIAL or FINISHED)

------------------------------------------------------------------------

### 3.7 Terminate Step

    POST /api/v2/steps/{stepId}/terminate

**Status codes:** - `200` — Terminated - `409` — Not RUNNING - `502` —
Runserver not reachable

------------------------------------------------------------------------

## 4. Validation Rules

### 4.1 Step-Level

| Rule                                                                            | Error Code               |
|---------------------------------------------------------------------------------|--------------------------|
| `tree` must reference an existing Analysis Tree                                 | `TREE_NOT_FOUND`         |
| `parentStep` (if set) must reference an existing Step (can be cross-tree)       | `PARENT_NOT_FOUND`       |
| `processes` required if no `parentStep`; optional with `parentStep` (inherited) | `NO_PROCESSES`           |
| Exactly one process must have `main: true`                                      | `MAIN_PROCESS_REQUIRED`  |
| Process `position` values must be unique within step                            | `DUPLICATE_POSITION`     |
| Process `name` values must be unique within step                                | `DUPLICATE_PROCESS_NAME` |
| Cannot update/delete while `runStatus` = RUNNING                                | `STEP_RUNNING`           |
| Cannot run unless `runStatus` ∈ {INITIAL, FINISHED}                             | `INVALID_RUN_STATE`      |

### 4.2 Process-Level

| Rule                                                            | Error Code                |
|-----------------------------------------------------------------|---------------------------|
| `runserverId` must reference an existing runserver              | `RUNSERVER_NOT_FOUND`     |
| `runserverToolId` must reference a valid tool on that runserver | `TOOL_NOT_FOUND`          |
| `gridArguments` only valid when `gridTool: true`                | `GRID_ARGS_NOT_GRID_TOOL` |
| `processType: "sub"` requires `parentProcessId`                 | `SUB_REQUIRES_PARENT`     |
| `position` must be ≥ 1                                          | `INVALID_POSITION`        |

### 4.3 Variable-Level

| Rule                                                  | Error Code                    |
|-------------------------------------------------------|-------------------------------|
| `name` must be unique within process                  | `DUPLICATE_VARIABLE_NAME`     |
| `position` must be unique within process              | `DUPLICATE_VARIABLE_POSITION` |
| `variableType: "fileRef"` requires `valueResourceId`  | `MISSING_TARGET_RESOURCE`     |
| `valueResourceId` must reference an existing resource | `TARGET_NOT_FOUND`            |

### 4.4 Grid Argument-Level

| Rule                                                                            | Error Code            |
|---------------------------------------------------------------------------------|-----------------------|
| `definitionId` must reference a valid definition for the process’s gridProvider | `GRID_DEF_NOT_FOUND`  |
| LOV type requires `lovValueId` valid for the definition’s category              | `INVALID_LOV_VALUE`   |
| LOV type requires `categoryId`                                                  | `MISSING_CATEGORY_ID` |
| TEXT type requires `textValue`                                                  | `MISSING_TEXT_VALUE`  |
| DATE_TIME type requires `dateValue` (epoch ms as string)                        | `MISSING_DATE_VALUE`  |

### 4.5 Inventory-Level

| Rule                                                                                        | Error Code                 |
|---------------------------------------------------------------------------------------------|----------------------------|
| `inventoryPath` must be a valid relative path (no `..`, no absolute)                        | `INVALID_PATH`             |
| Link `targetEntityId` must reference an existing file resource (any file in the repository) | `LINK_TARGET_NOT_FOUND`    |
| ExtLink `url` must be a valid URL                                                           | `INVALID_URL`              |
| Inventory paths must be unique within step                                                  | `DUPLICATE_INVENTORY_PATH` |

### 4.6 Resource Mapping (on create)

| Rule                                                                    | Error Code           |
|-------------------------------------------------------------------------|----------------------|
| `sourceResourceId` in `resources[]` must reference an existing resource | `SOURCE_NOT_FOUND`   |
| `variableName` in `resources[]` must match a `name` in `variables[]`    | `VARIABLE_NOT_FOUND` |
| `operation` must be `"COPY"` or omitted (default: link)                 | `INVALID_OPERATION`  |

------------------------------------------------------------------------

## 5. Configuration Endpoints (Supporting)

These already exist and are NOT part of the step v2 redesign, but are
needed by clients to build valid step creation requests.

| Endpoint                                                       | Returns                                                                                                                                         |
|----------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|
| `GET /configuration/runservers`                                | All runservers: `{id, url, hostname, label, local, deleted, generic, reproducible, sshPort}`                                                    |
| `GET /configuration/runservers/{id}/tools`                     | Tools on a runserver: merged view with `{id (=runserverToolId), toolId, toolName, name (=toolInstance), gridProvider, runserverId, label, ...}` |
| `GET /configuration/toolCategories`                            | Tool categories: `{id, name, identifier}`                                                                                                       |
| `GET /configuration/toolCategories/{id}/tools`                 | Tools in category: `{id (=toolId), name (=toolLabel), categoryId}`                                                                              |
| `GET /configuration/gridArguments/{gridProvider}/definitions`  | Grid arg definitions: `{id, name, gridArgumentType, categoryId, category[{values[{id, text}]}]}`                                                |
| `GET /configuration/runservers/{id}/tools/{toolId}/parameters` | Tool parameters: `{lovType, name, description, value, parameterLovId}`                                                                          |

------------------------------------------------------------------------

## 6. Resolved Decisions

| \#  | Decision                                                                            |
|-----|-------------------------------------------------------------------------------------|
| 1   | Step names are **auto-generated** (Step 1, Step 2, …). Client can override.         |
| 2   | No inventory path depth limit. Runserver determines folder structure.               |
| 3   | File upload via **pre-authenticated S3 links** (already exists). No inline base64.  |
| 4   | **Dependency graph** determines execution parallelism. Runserver executes.          |
| 5   | **WebSocket** for notifications exists, defer to later phase.                       |
| 6   | **Multi-tree workflows** supported. Parent can be cross-tree.                       |
| 7   | **Field-level nested updates** via PATCH.                                           |
| 8   | **Keep** existing JWT/OAuth auth.                                                   |
| 9   | **Standard v2 error format** already planned by repo team.                          |
| 10  | **Return all** steps for listings. Pagination later if needed.                      |
| 11  | Execution state **persists forever**.                                               |
| 12  | Inheritance is a **snapshot at creation time**. Explicit fields override inherited. |
| 13  | Link targets can reference **any file in the repository**, not just files in steps. |

------------------------------------------------------------------------

## 7. Open Questions for Repository Team

1.  **Process `resources` array** — The current v1 step creation accepts
    a `resources[]` array on each process (with `sourceResourceId`,
    `targetName`, `variableName`, `operation`). Should v2 keep this
    mechanism, or should linking files to processes be done differently?

2.  **Process ordering gaps** — When `position` values have gaps (1, 3,
    5), does the server normalize them or preserve gaps?

3.  **`toolArgs` defaults** — When a process is created with just
    `runserverId` + `runserverToolId`, does the server auto-populate
    `toolArgs` from the tool’s default parameters? Or does the client
    always need to provide them explicitly?

4.  **Grid provider resolution** — The `gridProvider` is a property of
    the `runserverToolId`. When `gridTool=true`, does the server
    automatically resolve which grid argument definitions are valid, or
    does the client need to query the configuration endpoints first?

5.  **Concurrent modification / locking** — Steps have one owner. Should
    the API enforce this (reject updates from non-owner), or is this
    handled at a higher level?

6.  **Timestamps** — The v1 API uses epoch milliseconds as strings.
    Should v2 switch to ISO 8601 (e.g., `2026-02-18T10:00:00Z`), or keep
    epoch milliseconds for consistency with the existing data model?
