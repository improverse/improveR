# Unit tests for workflowExecutionOrder
# These are fast, local tests — no server connection needed.

test_that("Linear chain of 3 steps orders correctly", {
  plan <- data.frame(
    fullName = c("Step1", "Step2", "Step3"),
    dependencies = c(NA, "Step1", "Step2"),
    usage = c("Step2", "Step3", NA),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), 3)
  expect_equal(result$fullName, c("Step1", "Step2", "Step3"))
})

test_that("Branching: one source, two consumers", {
  plan <- data.frame(
    fullName = c("Source", "BranchA", "BranchB"),
    dependencies = c(NA, "Source", "Source"),
    usage = c("BranchA,BranchB", NA, NA),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), 3)
  expect_equal(result$fullName[1], "Source")
  expect_setequal(result$fullName[2:3], c("BranchA", "BranchB"))
})

test_that("Diamond pattern: source -> A,B -> merge", {
  plan <- data.frame(
    fullName = c("Source", "BranchA", "BranchB", "Merge"),
    dependencies = c(NA, "Source", "Source", "BranchA,BranchB"),
    usage = c("BranchA,BranchB", "Merge", "Merge", NA),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), 4)
  expect_equal(result$fullName[1], "Source")
  expect_equal(result$fullName[4], "Merge")
  # A and B can be in either order but must come before Merge
  expect_setequal(result$fullName[2:3], c("BranchA", "BranchB"))
})

test_that("All independent steps (no dependencies)", {
  plan <- data.frame(
    fullName = c("A", "B", "C", "D", "E"),
    dependencies = rep(NA, 5),
    usage = rep(NA, 5),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), 5)
  expect_setequal(result$fullName, c("A", "B", "C", "D", "E"))
})

test_that("Single step works", {
  plan <- data.frame(
    fullName = "OnlyStep",
    dependencies = NA,
    usage = NA,
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), 1)
  expect_equal(result$fullName, "OnlyStep")
})

test_that("Missing dependencies column is treated as no dependencies", {
  plan <- data.frame(
    fullName = c("A", "B"),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), 2)
})

test_that("Missing usage column is handled", {
  plan <- data.frame(
    fullName = c("A", "B"),
    dependencies = c(NA, NA),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), 2)
})

# --- Circular dependency detection ---

test_that("Pure cycle (all steps have deps) returns NULL", {
  plan <- data.frame(
    fullName = c("A", "B"),
    dependencies = c("B", "A"),
    usage = c("B", "A"),
    stringsAsFactors = FALSE
  )
  # log_warn is used (not base::warning), so no R warning is thrown
  result <- workflowExecutionOrder(plan)
  expect_null(result)
})

test_that("Three-step cycle returns NULL", {
  plan <- data.frame(
    fullName = c("A", "B", "C"),
    dependencies = c("C", "A", "B"),
    usage = c("B", "C", "A"),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_null(result)
})

test_that("Partial cycle: reachable root + stuck subgraph errors", {
  # Step1 has no deps. Step2 and Step3 form a cycle.
  plan <- data.frame(
    fullName = c("Step1", "Step2", "Step3"),
    dependencies = c(NA, "Step3", "Step2"),
    usage = c(NA, "Step3", "Step2"),
    stringsAsFactors = FALSE
  )
  expect_error(
    workflowExecutionOrder(plan),
    "Circular dependency detected"
  )
})

test_that("Partial cycle error message includes stuck step names", {
  plan <- data.frame(
    fullName = c("Root", "CycleA", "CycleB"),
    dependencies = c(NA, "CycleB", "CycleA"),
    usage = c(NA, "CycleB", "CycleA"),
    stringsAsFactors = FALSE
  )
  expect_error(
    workflowExecutionOrder(plan),
    "CycleA.*CycleB|CycleB.*CycleA"
  )
})

# --- Scale: large workflows ---

test_that("Large linear chain (600 steps) completes without false cycle warning", {
  n <- 600
  fullNames <- paste0("Step", seq_len(n))
  deps <- c(NA, fullNames[1:(n-1)])
  usages <- c(fullNames[2:n], NA)
  plan <- data.frame(
    fullName = fullNames,
    dependencies = deps,
    usage = usages,
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), n)
  expect_equal(result$fullName, fullNames)
})

test_that("Large wide workflow (1000 independent steps) works", {
  n <- 1000
  plan <- data.frame(
    fullName = paste0("Step", seq_len(n)),
    dependencies = rep(NA, n),
    usage = rep(NA, n),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), n)
})

test_that("Large branching workflow (source + 500 consumers) works", {
  n <- 500
  consumers <- paste0("Consumer", seq_len(n))
  plan <- data.frame(
    fullName = c("Source", consumers),
    dependencies = c(NA, rep("Source", n)),
    usage = c(paste(consumers, collapse = ","), rep(NA, n)),
    stringsAsFactors = FALSE
  )
  result <- workflowExecutionOrder(plan)
  expect_equal(nrow(result), n + 1)
  expect_equal(result$fullName[1], "Source")
})
