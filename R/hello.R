if (!is.null(get0(x = "BUILD",envir = globalenv()))) {
  ## do not forget to include in the DESCRIPTION
  improveRcore:::buildMetaPackage("improveR",c("improveRcore","improveRget"))
}









