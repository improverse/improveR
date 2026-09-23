# Getting started with improveR

## Overview

**improveR** is the R interface to Scinteco’s improve platform - a
secure, versioned repository for pharmaceutical R&D workflows. It
provides seamless integration between R and the improve platform,
enabling 21 CFR Part 11 compliant workflow management for pharmaceutical
modeling, biostatistical analysis, and data science.

Key capabilities include:

- **Workflow Management**: Create and execute multi-step pharmaceutical
  modeling workflows
- **Version Control**: Full versioning of data, code, and analyses with
  complete audit trails
- **Resource Management**: Load and save analytical resources with
  metadata and lineage tracking
- **Collaborative Research**: Team-based review system for
  pharmaceutical research

## Installation

Install improveR from the Scinteco repository:

``` r
# Add the improve repository
options(repos = c(getOption("repos"),
  improve = "https://r.scinteco.com/4.4.0/"))

# Install the package
install.packages("improveR")

# Optional: Install CLI tools
install.packages("improveRcontributions")
```

For detailed installation instructions, see
[`vignette("install-improveR")`](https://improverse.github.io/improveR/articles/install-improveR.md).

## Setup and Connection

Configure your environment variables to connect to the improve
repository:

``` r
# Add to your .Renviron file:
# Sys.setenv(IMPROVER_REPO_URL = "https://<url>:<port>/repository")
# Sys.setenv(IMPROVER_STEP = "<valid-entityId>")

# Load the package
library(improveR)

# Connect to the repository
improveConnect()

# Enable editing
setEditable(TRUE)

# Verify connection
whoami()
```

## Core Concepts

### Resources

The improve platform organizes everything as **resources** - versioned
entities with unique identifiers:

- **Folders**: Organize resources hierarchically
- **Files**: Store data, scripts, and outputs with automatic versioning
- **Links**: Create stable references to specific versions of resources
- **Steps**: Executable units of analytical work within workflows

``` r
# Load a resource by path
resource <- loadResource("/Projects/my-analysis/data")

# Load by entity ID
resource <- loadResource("EN-12345")

# List folder contents
children <- loadChildResources(resource)

# Create a new file
myData <- data.frame(x = 1:10, y = rnorm(10))
write.csv(myData, "data.csv", row.names = FALSE)

dataFile <- createFile(
  targetIdent = resource$entityId,
  fileName = "data.csv",
  localPath = "data.csv",
  comment = "My analysis data"
)
```

### Workflows

Workflows chain together analytical **steps** organized in **analysis
trees**:

``` r
# Create an analysis tree
analysisTree <- createAnalysisTree(
  targetIdent = "/Projects/my-analysis",
  treeName = "EDA_Workflow",
  comment = "Exploratory data analysis"
)

# Create a step environment
stepEnv <- createStepTemplateEnv(treeIdent = analysisTree)

# Configure the step
stepEnv$setStepDescription("Load and prepare data")
stepEnv$setStepRationale("Initial data loading and validation")

# Add input files
stepEnv$addStepRemoteFile(ident = dataFile$entityId, variableName = "input-data")

# Set execution configuration
stepEnv$setStepRunserverLabel("runserver")
stepEnv$setStepToolLabel("R_4.2")
stepEnv$setStepToolInstance("rbatch")

# Create the step
resultStep <- stepEnv$realise()
step <- resultStep$getStepResource()
```

### Navigating Workflows

Use
[`getStep()`](https://improverse.github.io/improveR/reference/getStep.md)
to explore workflow relationships:

``` r
# Get a step with its full context
stepEnv <- getStep("/Projects/my-analysis/EDA_Workflow/Step 1")

# Load and explore child steps
stepEnv$children$load()
ls(stepEnv$children)

# Load and explore lineage (upstream dependencies)
stepEnv$lineage$load(treeDepth = -1)
workflow <- stepEnv$workflow$df()

# Load usage (downstream consumers)
stepEnv$usage$load(treeDepth = -1)

# Check step state
stepEnv$getStepState()

# Get step inventory
inventory <- stepEnv$getStepInventory()
```

## Next Steps

Now that you understand the basics, explore these topics in more detail:

- **[Basic Node
  Types](https://improverse.github.io/improveR/articles/basic-node-types.md)**:
  Detailed guide to folders, files, links, and versioning
- **[Workflows
  Basic](https://improverse.github.io/improveR/articles/workflows-basic.md)**:
  Create and execute your first workflow
- **[Working with
  Steps](https://improverse.github.io/improveR/articles/workingWithSteps.md)**:
  Advanced step management and navigation
- **[Working with
  Workflows](https://improverse.github.io/improveR/articles/workingWithWorkflows.md)**:
  Workflow orchestration and execution
- **[CLI
  Tools](https://improverse.github.io/improveR/articles/cli-tools.md)**:
  Git-like version control operations

## Getting Help

``` r
# Function documentation
?improveConnect
?createFile
?getStep

# List all improveR functions
ls("package:improveR")

# Disconnect when done
improveDisconnect()
```

For support, visit the [Scinteco website](https://www.scinteco.com/).
