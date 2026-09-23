# Improve Platform — Capability Mapping to User Stories

## Data Provider Stories

### 1. Large File / ZIP Ingestion (\>5GB)

**Status: Covered**

File upload via client API and CLI tools supports large files. ZIP
handling is built into the client. SHA-256 hashing ensures integrity.
Full versioning with revisions. CLI and client uploads can deliver
complete packages in a single revision. Validation and post-upload
processing (extraction, completeness checks) handled by import scripts
on the runserver. No first-class “data package” concept in Improve, but
existing tools cover the need.

### 2. Secure Manual Upload

**Status: Covered**

The Improve RCP client provides folder upload and ZIP upload with
optional extraction. All uploads go through the same authenticated,
audited, encrypted pipeline. OAuth 2.0 authentication, full audit trail,
SSL/TLS encryption.

### 3. Transfer Completion/Failure Notifications

**Status: Partial gap**

Direct uploads via RCP client or R client return immediate
success/failure feedback to the caller. Audit trail captures all
operations with timestamps. However, there is no proactive push
notification system (email, webhook) to notify third parties. Notifying
others (e.g., PMx team that data has arrived) would require post-upload
scripting or a future platform-level notification feature.

### 4. Transfer Metadata Retention

**Status: Covered**

Automatic tracking of sender (`createdBy`), timestamp
(`lastModifiedOnDate`), dataset identifiers (`entityId`), and version
(`entityVersionId`, `revisionId`). Full version history and audit trail.
Additional structured metadata (study name, protocol, data type)
attachable via the metadata API with controlled vocabularies. SHA-256
hashing for file integrity.

### 5. API for File Upload to a Folder

**Status: Covered**

Core functionality.
[`createFile()`](https://improverse.github.io/improveR/reference/createFile.md)
uploads to a specific folder,
[`uploadFolder()`](https://improverse.github.io/improveR/reference/uploadFolder.md)
handles directory trees,
[`pushCli()`](https://improverse.github.io/improveR/reference/pushCli.md)
via CLI. ZIP handling in the client with optional extraction. OAuth
authentication on all endpoints.

### 6. API for Secure Folder Creation (Admin)

**Status: Covered**

[`createFolder()`](https://improverse.github.io/improveR/reference/createFolder.md)
and
[`createAnalysisTree()`](https://improverse.github.io/improveR/reference/createAnalysisTree.md)
for folder creation. Full permission management API to lock down folders
to specific users or groups. Group management API for team-based access.
All API-driven and auditable.

### 7. Integrated Feedback Loop (Accept/Reject Data)

**Status: Covered**

Uploaded files can be added directly as review entries. PMx reviewers
inspect individual files, comment, then accept or decline. Review status
lifecycle (PENDING, APPROVED, DECLINED, REOPENED) with threaded comments
per file. Data provider sees status and rejection details. Scripting or
templates can automate review creation and file attachment after upload.

------------------------------------------------------------------------

## Pharmacometrician Stories — Compute & Workspace

### 8. Scalable Compute Resources

**Status: Covered**

Multiple runservers with grid computing support (SGE, SLURM, LSF).
Server-side orchestration with automatic parallelization based on
dependency graphs. Steps execute on runserver infrastructure, not on the
user’s machine. Scaling managed at infrastructure level by
administrators.

### 9. Abstract Compute Configuration

**Status: Covered**

Tool instances come with server-configured defaults. Users create steps
and choose a tool without provisioning servers or allocating resources.
Default or policy-based configuration applied automatically. Power users
can override grid arguments if needed, but it’s not required.

### 10. Secure, Access-Controlled Workspaces

**Status: Covered**

Per-resource ACL with fine-grained permissions. Group management with
role-based access. Effective permissions computed with group
inheritance. Resource locking for concurrent access control. OAuth
authentication on all operations. Enforced consistently across all
access paths.

### 11. Standard PMx Tools Available

**Status: Covered**

NONMEM (multiple versions), R, PsN, Stan, Monolix, MATLAB, SAS all
configurable per runserver. Extensible tool framework for adding new
tools. Users discover available tools and select them when configuring
steps. No user-side installation needed.

------------------------------------------------------------------------

## Pharmacometrician Stories — Analysis Environment

### 12. Co-located Data, Code, and Outputs

**Status: Covered**

The step model co-locates data, code, and outputs in a single traceable
unit by design. Analysis trees group steps into a workspace where all
artifacts are accessible together. Fundamental platform architecture.

### 13. Traceability (Inputs, Code Versions, Outputs)

**Status: Covered**

Full upstream lineage and downstream usage tracking. Version-specific
links between resources. Outdated link detection flags stale
dependencies. DMG (change detection) identifies which steps need
re-execution. Complete audit trail. Every revision captures the exact
state of inputs, code, and outputs.

### 14. Separate Workspaces per Analysis/Study

**Status: Covered**

Independent analysis trees or workflows per study/analysis. Each fully
isolated with independent versioning, permissions, and traceability. No
shared state unless explicitly linked.

### 15. Initialize R Project in Workspace

**Status: Covered**

CICO (Check-In/Check-Out) creates a local R working environment with
defined directory structure and project metadata. Git-like workflow via
CLI for local editing and push-back. RStudio integration via
improveRstudio/improveRswb. Steps serve as the project definition with
managed directory structure, source files, and history.

### 16. Preconfigured Project Templates (PopPK, E-R, MBMA)

**Status: Coming soon**

The existing workflow template system serves a different purpose
(templating workflow step chains). What’s needed is a folder structure
template concept — reusable blueprints defining folder hierarchy,
default permission schemes, and optional starter files. Currently
achievable via scripting but a first-class feature is in development.

### 17. R and Python Libraries

**Status: Covered**

R and Python libraries available on runservers as configured by
administrators. Python availability is a deployment/configuration
matter. Users don’t install packages — validated environments are
pre-configured on the runserver.

### 18. Library Interdependency Capture

**Status: Covered**

Validated run environments are controlled at the infrastructure level —
admins provision them, users execute against them. Run details capture
which tool instance and runserver were used. No library drift to capture
since users don’t install libraries. Where needed, `renv` setups can
store environment snapshots on the server.

------------------------------------------------------------------------

## Summary

| \#  | Story                         | Status      |
|-----|-------------------------------|-------------|
| 1   | Large file / ZIP ingestion    | Covered     |
| 2   | Secure manual upload          | Covered     |
| 3   | Transfer notifications        | Partial gap |
| 4   | Transfer metadata retention   | Covered     |
| 5   | API file upload               | Covered     |
| 6   | API folder creation (admin)   | Covered     |
| 7   | Feedback loop (accept/reject) | Covered     |
| 8   | Scalable compute              | Covered     |
| 9   | Abstract compute config       | Covered     |
| 10  | Access-controlled workspaces  | Covered     |
| 11  | PMx tools available           | Covered     |
| 12  | Co-located artifacts          | Covered     |
| 13  | Traceability                  | Covered     |
| 14  | Separate workspaces           | Covered     |
| 15  | Initialize R project          | Covered     |
| 16  | Project templates             | Coming soon |
| 17  | R/Python libraries            | Covered     |
| 18  | Library dependencies          | Covered     |
