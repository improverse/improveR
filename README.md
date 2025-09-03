
# improveR

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

**improveR** is the R client package for Scinteco's **improve** platform - a secure, versioned repository designed specifically for pharmaceutical R&D data science and modeling workflows. This package enables seamless integration between R environments and the improve platform, providing pharmaceutical researchers with tools to manage, track, and control reproducible analytical workflows in a 21 CFR Part 11 compliant environment.

The improve platform supports comprehensive workflow management for pharmaceutical modeling (NONMEM, MATLAB, Monolix), biostatistical analysis, and data science processes, with improveR serving as the primary R interface.

## Key Features

### Core Functionality
- **Resource Management**: Load, save, and manage analytical resources with full metadata and lineage tracking
- **Workflow Control**: Execute and manage multi-step pharmaceutical modeling workflows
- **Version Control**: Built-in CLI integration for cloning, pushing, and pulling workflow changes
- **Authentication**: Secure OAuth-based authentication with token management

### Pharmaceutical R&D Focus
- **Regulatory Compliance**: 21 CFR Part 11 compliant workflow tracking and documentation
- **Multi-Tool Integration**: Seamless integration with NONMEM, MATLAB, Monolix, SAS, and other pharmaceutical modeling tools
- **Data Lineage**: Comprehensive tracking of data transformations and analysis steps
- **Collaborative Research**: Review system for team-based pharmaceutical research

### Advanced Capabilities  
- **Automated Reporting**: Integration with R Markdown and LaTeX for streamlined pharmaceutical reporting
- **Metadata Management**: Rich metadata handling for regulatory documentation
- **Intelligent Caching**: Performance optimization for large pharmaceutical datasets

## Installation

You can install the development version of improveR:

``` r
# install.packages("pak")
pak::pak("scinteco/improveR")
```

## Getting Started

### Connect to improve Platform

``` r
library(improveR)

# Connect to your improve server
improveConnect(server_url = "https://your-improve-server.com")

# Verify connection
improveConnected()
```

### Loading Pharmaceutical Data

``` r
# Load clinical trial data
clinical_data <- loadResource("/studies/phase2/pk-data.csv")

# Load NONMEM model files
pk_model <- loadResource("/models/population-pk/run001")

# Load multiple related resources
study_resources <- loadResource(c(
  "/studies/phase2/demographics.csv",
  "/studies/phase2/dosing.csv",
  "/studies/phase2/concentrations.csv"
))
```

### Workflow Execution

``` r
# Load a modeling workflow step
pk_analysis <- loadStep("/workflows/pk-modeling/base-model")

# Execute population PK analysis
runStep(pk_analysis)

# Create new modeling workflow
createWorkflow(
  name = "Phase II PK Analysis", 
  description = "Population pharmacokinetic modeling workflow",
  path = "/studies/phase2/pk-workflow"
)
```

### Version Control for Regulatory Compliance

``` r
# Clone existing validated workflow
cliClone(workflow_path = "/validated/pk-models/base-model", 
         local_path = "./pk-analysis")

# Track changes with audit trail
cliPush(message = "Updated covariate model - final for submission")

# Pull latest validated methods
cliPull()
```

## Advanced Usage

### Regulatory Metadata

``` r
# Set regulatory metadata
setMetaData(resource_id = "pk-model-001",
            metadata = list(
              study_id = "PROTO-001",
              analysis_version = "v2.1",
              analyst = "John Doe",
              review_status = "approved",
              regulatory_compliance = "21 CFR Part 11"
            ))
```

### Audit Trail and Lineage

``` r
# Load complete audit trail
audit_trail <- loadAuditTrail("/models/final/pk-model")

# Track data lineage
lineage <- getLineage("/results/efficacy-analysis")

# Load review history
reviews <- loadReviews("/workflows/safety-analysis")
```

## Platform Integration

improveR integrates with the broader improve platform ecosystem:

- **Data Sources**: Automated fetching from clinical databases and EDC systems
- **Analysis Tools**: NONMEM, MATLAB, Monolix, SAS integration
- **Reporting**: R Markdown and LaTeX report generation  
- **Collaboration**: Team review and approval workflows
- **Compliance**: 21 CFR Part 11 validation and audit capabilities

## About Scinteco

[Scinteco](https://www.scinteco.com/) is a Vienna-based technology company specializing in IT solutions for the pharmaceutical industry. The improve platform represents their commitment to providing secure, compliant, and efficient data science workflows for pharmaceutical R&D.

## License

GPL-3

