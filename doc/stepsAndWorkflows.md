# Current Concepts in improveR

## 1. Step
- Represents a single, concrete process or task in your repository.
- Has a unique identity and metadata.
- Can be loaded, inspected, and executed.
- May have lineage (dependencies) and usage (downstream consumers).

## 2. Workflow
- Represents a collection of steps and their relationships (dependencies, execution order).
- Also a concrete entity in the repository.
- Can be loaded, inspected, and executed as a whole.
- Tracks which steps are outdated, manages execution plans, etc.

## 3. StepTemplate
- A blueprint for creating new steps.
- Not an actual step in the repository, but a reusable definition.
- Can be instantiated to create a new step entity in the repository.

## 4. WorkflowTemplate
- A blueprint for creating new workflows (and their steps).
- Not an actual workflow in the repository, but a reusable definition.
- When executed, creates new steps and workflows in the repository, runs them, and can create further steps as needed.

---

## Interactive Navigation

Each step environment contains navigation environments: `lineage`, `usage`, `parent`, and `children`.  
These environments allow interactive exploration of the step graph in the R console.  
Each navigation environment provides a `$load()` method. When called, this method loads the related steps into the environment as named objects.  
This enables tab-completion and chaining, e.g.:

```r
S1$lineage$load()
S1$lineage$S3$usage$load()
S1$lineage$S3$usage$S5$lineage$load()
```

As you navigate, the corresponding steps are loaded into the workflow and become available for further exploration.

---

## Summary Table

| Concept           | Is Blueprint? | Is Entity? | Can be Executed? | Can Create Entities? |
|-------------------|:-------------:|:----------:|:----------------:|:--------------------:|
| Step              |      No       |    Yes     |      Yes         |         No           |
| Workflow          |      No       |    Yes     |      Yes         |         No           |
| StepTemplate      |     Yes       |    No      |      Yes*        |        Yes           |
| WorkflowTemplate  |     Yes       |    No      |      Yes*        |        Yes           |

\*Execution means instantiation + execution of the resulting entities.