# Proposal: API v2 — Workflow & Step Redesign

**Date:** 2026-02-18
**Status:** Draft for discussion with Repository Team
**Context:** The v1 API requires excessive round-trips for step and workflow operations. API v2 is a clean-slate redesign — the v1 API is frozen for legacy clients, so v2 has **zero backward compatibility constraints**.

---

## 1. Problem Statement

### Current v1 Round-Trip Counts

| Operation | v1 API Calls | Notes |
|-----------|:-----------:|-------|
| **Read 1 step** | 5 + 2N | N = processes + inventory items |
| **Create 1 step** (realise) | 2 + P + V + F + (5+2N) + 1 | P=processes, V=variables, F=files |
| **Load 10-step workflow** | S × (5+2N) ≈ 200 | Sequential, one step at a time |
| **Wait for step completion** | unbounded polling | 5s intervals, indefinite |
| **Analyze workflow for re-execution** | ~250 | Load all steps + DMG + dependency graph |

### Root Cause

The v1 API is **resource-granular**: steps, processes, variables, grid arguments, inventory items, and links are all separate resources with separate endpoints. The client must assemble the full picture from many small pieces. Workflow is not a server concept at all — it's entirely client-side logic.

### Design Principle for v2

**Steps and workflows are the primary domain objects.** The API should speak in terms of steps and workflows, not individual sub-resources. A step is always returned with its full context. A workflow is a first-class server-side concept.

---

## 2. API v2 Design

### 2.1 Base Path

```
/api/v2/
```

All endpoints below are relative to this base.

---

### 2.2 Steps — Full Object by Default

A step in v2 is always a **complete, self-contained object**. No assembly required.

#### GET /steps/{stepId}

Returns the full step with all nested data.

**Response:**
```json
{
  "id": "step-uuid",
  "entityId": "ST-12345",
  "name": "Data Processing",
  "description": "...",
  "rationale": "...",
  "runStatus": "FINISHED",
  "lastModifiedOn": "2026-02-18T10:30:00Z",

  "parent": {
    "id": "parent-step-uuid",
    "entityId": "ST-12300",
    "name": "Parent Step"
  },

  "tree": {
    "id": "tree-uuid",
    "name": "Analysis Tree 1",
    "path": "/Project/Analysis Tree 1"
  },

  "processes": [
    {
      "id": "proc-uuid",
      "position": 1,
      "main": true,
      "tool": {
        "id": "R-4.3",
        "label": "R 4.3.1",
        "category": "Statistics"
      },
      "runserver": {
        "id": "rs-uuid",
        "name": "Linux Server 1"
      },
      "args": "--vanilla",
      "variables": [
        {
          "id": "var-uuid",
          "name": "input_data",
          "position": 1,
          "type": "fileRef",
          "targetResourceId": "file-uuid"
        }
      ],
      "gridArguments": [
        {
          "id": "ga-uuid",
          "name": "cores",
          "type": "TEXT",
          "value": "8"
        }
      ],
      "lastRun": {
        "runId": "run-uuid",
        "status": "FINISHED",
        "startedAt": "2026-02-18T10:00:00Z",
        "finishedAt": "2026-02-18T10:25:00Z"
      }
    }
  ],

  "inventory": [
    {
      "id": "file-uuid",
      "name": "output.csv",
      "nodeType": "File",
      "path": "/results/output.csv",
      "hash": "md5:abc123...",
      "size": 1048576,
      "lastModified": "2026-02-18T10:25:00Z"
    },
    {
      "id": "link-uuid",
      "name": "input_data.csv",
      "nodeType": "Link",
      "path": "/input/input_data.csv",
      "targetId": "source-file-uuid",
      "targetStepId": "upstream-step-uuid",
      "outdated": false
    }
  ]
}
```

**Key difference from v1:** This is not an "enriched" response — this IS the step. There is no stripped-down version. One call, complete object.

#### GET /steps?treeId={treeId}

Returns all steps in a tree, each as a full step object.

```json
{
  "steps": [ /* array of full step objects */ ],
  "total": 10
}
```

Replaces the current pattern of listing step IDs then loading each individually.

#### POST /steps

Creates a fully configured step in one atomic call.

**Request:**
```json
{
  "treeId": "tree-uuid",
  "name": "My New Step",
  "description": "...",
  "rationale": "...",
  "parentStepId": "parent-step-uuid",

  "processes": [
    {
      "toolId": "R-4.3",
      "runserverId": "rs-uuid",
      "main": true,
      "position": 1,
      "args": "--vanilla",
      "variables": [
        { "name": "input_data", "targetResourceId": "file-uuid" },
        { "name": "script", "filePath": "main.R" }
      ],
      "gridArguments": [
        { "name": "cores", "type": "TEXT", "value": "8" }
      ]
    }
  ],

  "files": [
    {
      "name": "main.R",
      "path": "/scripts/main.R",
      "content": "base64-encoded-content"
    }
  ],

  "links": [
    {
      "targetResourceId": "source-file-uuid",
      "path": "/input/data.csv"
    }
  ],

  "run": false
}
```

**Response:** The full step object (same format as GET).

**Behaviors:**
- Atomic: everything succeeds or everything rolls back
- `"run": true` creates and immediately starts execution
- Response includes the complete step, no need to read it back

#### PATCH /steps/{stepId}

Updates step properties. Supports **nested field-level updates** — only send what changed. Can update a single variable within a single process without sending the entire step.

```json
{
  "name": "Updated Name",
  "description": "New description",
  "processes": [
    {
      "id": "existing-proc-uuid",
      "args": "--no-save --vanilla",
      "variables": [
        {
          "id": "existing-var-uuid",
          "targetResourceId": "new-file-uuid"
        }
      ]
    }
  ]
}
```

**Response:** The full updated step object.

#### DELETE /steps/{stepId}

Deletes a step and all its inventory.

#### POST /steps/{stepId}/run

Starts step execution. Returns immediately (non-blocking).

**Response:**
```json
{
  "runId": "run-uuid",
  "status": "RUNNING",
  "startedAt": "2026-02-18T11:00:00Z"
}
```

#### POST /steps/{stepId}/terminate

Stops a running step.

#### POST /steps/{stepId}/duplicate

Creates a copy of a step with all its configuration.

```json
{
  "name": "Step Copy",
  "treeId": "target-tree-uuid",
  "parentStepId": "new-parent-uuid"
}
```

**Response:** The full new step object.

---

### 2.3 Workflows — First-Class Server Concept

Workflows become a real API concept. The server handles dependency resolution, execution ordering, and change detection — not the client.

#### GET /workflows/{treeId}

Returns the complete workflow: all steps, their dependency graph, change status, and execution plan.

**Response:**
```json
{
  "tree": {
    "id": "tree-uuid",
    "name": "Analysis Tree 1",
    "path": "/Project/Analysis Tree 1"
  },

  "steps": [
    { /* full step object */ },
    { /* full step object */ }
  ],

  "graph": {
    "edges": [
      {
        "from": "step-1-uuid",
        "to": "step-2-uuid",
        "files": [
          {
            "sourceFileId": "output-uuid",
            "linkId": "link-uuid",
            "outdated": false
          }
        ]
      }
    ],
    "roots": ["step-1-uuid"],
    "leaves": ["step-3-uuid"],
    "executionOrder": ["step-1-uuid", "step-2-uuid", "step-3-uuid"]
  },

  "status": {
    "outdatedSteps": ["step-2-uuid"],
    "changedFiles": [
      {
        "fileId": "file-uuid",
        "stepId": "step-1-uuid",
        "changedSince": "2026-02-17T08:00:00Z"
      }
    ],
    "runningSteps": [],
    "failedSteps": []
  }
}
```

**This single call replaces:**
- `getWorkflow()` (loading all steps)
- `changedAndOutdatedFiles()` (DMG analysis)
- `createReexecutionPlan()` (topological sort + change detection)
- All dependency/usage resolution

#### POST /workflows/{treeId}/execute

Tells the server to execute the workflow (or parts of it). The server manages ordering and link updates.

**Request:**
```json
{
  "mode": "outdated",
  "options": {
    "updateLinks": true,
    "stopOnFailure": true
  }
}
```

**Modes:**
- `"all"` — re-run every step in dependency order
- `"outdated"` — only re-run outdated steps and their downstream dependents
- `"steps"` — run specific steps (provide `"stepIds": [...]`)

**Response:**
```json
{
  "executionId": "exec-uuid",
  "plan": [
    { "stepId": "step-1-uuid", "order": 1, "reason": "changed" },
    { "stepId": "step-2-uuid", "order": 2, "reason": "downstream" }
  ],
  "status": "RUNNING"
}
```

The server orchestrates the execution. The client does NOT poll individual steps.

#### GET /workflows/{treeId}/executions/{executionId}

Check execution progress (non-blocking).

**Response:**
```json
{
  "executionId": "exec-uuid",
  "status": "RUNNING",
  "progress": {
    "total": 5,
    "completed": 3,
    "running": 1,
    "pending": 1,
    "failed": 0
  },
  "steps": [
    { "stepId": "step-1-uuid", "status": "FINISHED", "duration": 120 },
    { "stepId": "step-2-uuid", "status": "FINISHED", "duration": 45 },
    { "stepId": "step-3-uuid", "status": "FINISHED", "duration": 200 },
    { "stepId": "step-4-uuid", "status": "RUNNING", "startedAt": "..." },
    { "stepId": "step-5-uuid", "status": "PENDING" }
  ]
}
```

#### POST /workflows/{treeId}/executions/{executionId}/terminate

Stop a running workflow execution.

#### GET /workflows/{treeId}/dependencies/{stepId}

Get the dependency lineage for a specific step within the workflow.

**Response:**
```json
{
  "step": { "id": "step-2-uuid", "name": "Step 2" },
  "upstream": [
    { "id": "step-1-uuid", "name": "Step 1", "distance": 1 }
  ],
  "downstream": [
    { "id": "step-3-uuid", "name": "Step 3", "distance": 1 },
    { "id": "step-4-uuid", "name": "Step 4", "distance": 2 }
  ]
}
```

---

### 2.4 Batch Operations

For operations that span multiple unrelated resources.

#### POST /batch/steps

Retrieve multiple steps by ID in a single call.

**Request:**
```json
{
  "ids": ["step-1-uuid", "step-2-uuid", "step-3-uuid"]
}
```

**Response:**
```json
{
  "steps": [ /* full step objects */ ],
  "errors": [
    { "id": "step-4-uuid", "code": "NOT_FOUND", "message": "..." }
  ]
}
```

#### POST /batch/status

Check run status of multiple steps.

**Request:**
```json
{
  "ids": ["step-1-uuid", "step-2-uuid", "step-3-uuid"]
}
```

**Response:**
```json
{
  "statuses": [
    { "id": "step-1-uuid", "runStatus": "FINISHED" },
    { "id": "step-2-uuid", "runStatus": "RUNNING", "startedAt": "..." },
    { "id": "step-3-uuid", "runStatus": "WAITING" }
  ]
}
```

#### POST /batch/links/update

Update multiple outdated links at once.

**Request:**
```json
{
  "linkIds": ["link-1-uuid", "link-2-uuid"]
}
```

---

### 2.5 Step Hierarchy

Parent-child relationships are managed directly on the step.

#### PUT /steps/{stepId}/parent

```json
{ "parentStepId": "new-parent-uuid" }
```

#### DELETE /steps/{stepId}/parent

Detach from parent step.

---

### 2.6 Inventory Management

File and link operations within a step's inventory.

#### GET /steps/{stepId}/inventory

Returns the step's inventory (already included in the step object, but available standalone for partial refresh).

#### POST /steps/{stepId}/inventory/files

Create a file in the step inventory.

```json
{
  "name": "script.R",
  "path": "/scripts/script.R",
  "content": "base64-encoded"
}
```

#### POST /steps/{stepId}/inventory/links

Create a link to an external resource.

```json
{
  "targetResourceId": "file-uuid",
  "path": "/input/data.csv"
}
```

#### PUT /steps/{stepId}/inventory/{itemId}/content

Upload/update file content.

#### DELETE /steps/{stepId}/inventory/{itemId}

Remove a file or link from the step.

---

### 2.7 Process Runs (History)

#### GET /steps/{stepId}/processes/{processId}/runs

List execution history for a process. The latest run is already on the step object; this endpoint provides the full history.

```json
{
  "runs": [
    {
      "runId": "...",
      "status": "FINISHED",
      "startedAt": "...",
      "finishedAt": "...",
      "tool": { "id": "R-4.3", "label": "R 4.3.1" },
      "runserver": { "id": "...", "name": "..." }
    }
  ]
}
```

---

## 3. v1 → v2 Comparison

### API Call Counts

| Operation | v1 Calls | v2 Calls | Reduction |
|-----------|:--------:|:--------:|:---------:|
| Read 1 step | ~20 | **1** | 95% |
| Read 10-step workflow | ~200 | **1** | 99% |
| Create 1 step (3 processes, 5 files) | ~15 | **1** | 93% |
| Check status of 5 running steps | 5 | **1** | 80% |
| Load workflow + plan re-execution | ~250 | **1** | 99% |
| Execute workflow (10 steps) | ~10 + polling | **1** + check | 90%+ |

### Conceptual Shift

| Aspect | v1 | v2 |
|--------|-----|-----|
| Step model | Assembled from parts | Complete object |
| Workflow | Client-side concept | Server-side first-class |
| Execution ordering | Client computes | Server computes |
| Change detection (DMG) | Client requests + analyzes | Server includes in workflow |
| Step creation | Multi-step assembly | Single atomic call |
| Dependency resolution | Client-side traversal | Server-side graph |
| Polling | Per-step, 5s interval | Per-workflow execution check |
| Link updates | One-by-one | Batch or automatic in execution |

---

## 4. Implementation Plan

### Phase 1: Step as Complete Object
**Scope:** `GET /steps/{stepId}`, `GET /steps?treeId=`, `POST /steps`, `PUT /steps/{stepId}`, `DELETE /steps/{stepId}`
**Value:** Every step operation becomes a single call.

Tasks for repository team:
1. Design v2 step JSON schema (based on Section 2.2)
2. Implement `GET /steps/{stepId}` — aggregate resource + processes + variables + gridArgs + inventory + parent into single response
3. Implement `GET /steps?treeId={treeId}` — return all steps in tree with full objects
4. Implement `POST /steps` — atomic creation with processes, variables, files, links
5. Implement `PUT /steps/{stepId}` — partial update with full response
6. Implement `DELETE /steps/{stepId}`

Tasks for improveR team:
1. New `authenticatedREST_v2()` or adapt base path to `/api/v2/`
2. Rewrite `getStep()` — single call, map response to stepEnv
3. Rewrite `realise()` — single POST, map response to stepEnv
4. Remove: `loadProcessesForStep()`, `updateProcessGridArguments()`, `loadProcessFileVariables()`, individual inventory resource loading
5. Remove: version detection code (pre-4.4 fallbacks in getLineage.R etc.)

### Phase 2: Workflow as Server Concept
**Scope:** `GET /workflows/{treeId}`, `POST /workflows/{treeId}/execute`, `GET /workflows/{treeId}/executions/{id}`
**Value:** Workflow analysis and execution become server-managed.

Tasks for repository team:
1. Implement server-side dependency graph construction (topological sort)
2. Implement server-side DMG (change detection) aggregation
3. Implement `GET /workflows/{treeId}` — return steps + graph + status
4. Implement workflow execution engine — `POST /workflows/{treeId}/execute` orchestrates step runs in order, manages link updates
5. Implement `GET /workflows/{treeId}/executions/{id}` — execution progress

Tasks for improveR team:
1. Rewrite `getWorkflow()` — single call to `GET /workflows/{treeId}`
2. Rewrite `createReexecutionPlan()` — use `graph.executionOrder` from response
3. Rewrite `executePlan()` — single POST to `/workflows/{treeId}/execute`, then check progress
4. Remove: client-side topological sort, client-side DMG analysis, `finishRunResource()` polling loop
5. Simplify `createWorkflow.R` significantly — most logic moves server-side

### Phase 3: Batch & Execution Monitoring
**Scope:** `POST /batch/*`, execution status endpoints
**Value:** Remaining multi-call patterns eliminated.

Tasks for repository team:
1. Implement `POST /batch/steps` — multi-step retrieval
2. Implement `POST /batch/status` — multi-step status check
3. Implement `POST /batch/links/update` — batch link refresh
4. Consider event bus / webhook support for execution notifications

Tasks for improveR team:
1. Use batch endpoints where individual step loading remains (e.g., loading dependencies across trees)
2. Replace polling patterns with execution status checks
3. Optionally integrate webhook/event bus for verticle-based workflows

---

## 5. Cleanup Opportunities in improveR

With v2 being a clean break, these can be removed from the R client:

| Remove | Reason |
|--------|--------|
| `loadProcessesForStep()` | Processes embedded in step |
| `updateProcessGridArguments()` | Grid args embedded in process |
| `loadProcessFileVariables()` | Variables embedded in process |
| `loadChildResources()` for inventory assembly | Inventory embedded in step |
| `loadResource()` calls for inventory items | Resource details embedded |
| Version detection code (`repo_version >= "4.4"`) | v2 only, no version branches |
| `getDependencies_deprecated()` | No pre-4.4 support |
| `finishRunResource()` polling loop | Server manages execution |
| Client-side topological sort in `createWorkflow.R` | Server computes order |
| Client-side DMG analysis | Server includes in workflow |
| `createStepTemplateEnv.R` multi-step realise logic | Replaced by atomic POST |

---

## 6. Design Decisions (Resolved)

| # | Question | Decision |
|---|----------|----------|
| 1 | **Partial responses** | YES — support a `fields` parameter for partial step responses (e.g., `?fields=id,name,runStatus`) |
| 2 | **Parallel step execution** | Steps execute on the runserver. The dependency graph determines what can be parallelized — server computes this automatically |
| 3 | **Execution lifecycle** | Execution state persists **forever** (permanent audit trail) |
| 4 | **File upload** | NOT inline base64. Future: **pre-authenticated S3 links** for file upload/download. Leave out of first step — files handled separately for now |
| 5 | **Completion notifications** | A **WebSocket system** already exists. Leave out of first implementation step — use batch status polling initially |
| 6 | **Workflow scope** | **Multi-tree workflows** are important and must be supported |
| 7 | **Update granularity** | **Nested field-level updates** — can update a single variable within a single process without sending the entire step |
| 8 | **Authentication** | Keeps existing JWT/OAuth mechanism — no changes |
| 9 | **Error format** | Standard error format **already planned for v2** (handled by repo team) |
| 10 | **Pagination** | Prefer **return all** for step listings. Pagination to be discussed for edge cases |

### Implications for Design

- **`fields` parameter** → `GET /steps/{stepId}?fields=id,name,status.runStatus,processes` returns only requested fields. Useful for workflow overview without loading full inventory.
- **S3 file handling** → Step creation (`POST /steps`) does NOT include file content. Instead: create step → get pre-authenticated S3 upload URLs → upload files directly to S3. Inventory items reference S3 objects. This is a future phase.
- **Multi-tree workflows** → Workflows are NOT tied to a single tree. The client passes a list of step identifiers from any tree. The server computes the dependency graph across trees.
- **WebSocket (future)** → Existing WebSocket infrastructure will eventually replace polling for execution status. First step uses `POST /batch/status`.
