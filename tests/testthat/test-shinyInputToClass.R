test_that("test shinyInputToClass - blackBearData with default inputs", {
  testData <-
    readRDS(testthat::test_path("testdata", "blackBearData_default.rds"))
  
  # new column added: values[["optimalPrior"]], default value == TRUE
  testData$optimalPrior <- TRUE
  
  resObject <- shinyInputToClass(testData,
                                 as.list(NULL),
                                 as.list(NULL))
  
  expect_identical(resObject$data[["obsvn"]][1:3], c(-20.6, -20.9, -22.4))
  expect_identical(resObject$data[["obsvnError"]][1:3], c(1, 1, 1))
  expect_identical(resObject$data[["weights"]][1:3], c(100, 0, 0))
  expect_identical(resObject$data[["weightsUncert"]][1:3], c(0, 0, 0))
  expect_identical(resObject$data[["concentration"]][1:3], c(47, 53, 4))
  expect_identical(resObject$data[["concentrationUncert"]][1:3], c(4.2, 2.6, 4.4))
  expect_identical(resObject$data[["source"]][1:3], c(-21.939, -16.943, -21.939))
  expect_identical(resObject$data[["sourceUncert"]][1:3], c(0.94608, 0.78144, 0.94608))
  expect_identical(resObject$data[["covariates"]],
                   structure(
                     "",
                     dim = c(1L, 1L),
                     dimnames = list("Individual_1",
                                     "Covariate_1")
                   ))
  expect_identical(resObject$data[["sourceDirichPrior"]], c(0.4, 0.4)) # if optimalPrior == TRUE

  expect_identical(resObject$priors, list())
  
  expect_identical(resObject$modelOptions[["modelType"]], "2")
  expect_identical(resObject$modelOptions[["modelWeights"]], FALSE)
  expect_identical(resObject$modelOptions[["categoricalVars"]], NULL)
  expect_identical(resObject$modelOptions[["numericVars"]], NULL)
  expect_identical(resObject$modelOptions[["modelWeightsContrained"]], TRUE)
  expect_identical(resObject$modelOptions[["modelConcentrations"]], TRUE)
  expect_identical(resObject$modelOptions[["modelConcentrationsContrained"]], TRUE)
  expect_identical(resObject$modelOptions[["minUnc"]], 0.005)
  expect_identical(resObject$modelOptions[["targetOffset"]], FALSE)
  expect_identical(resObject$modelOptions[["includeSourceOffset"]], FALSE)
  expect_identical(resObject$modelOptions[["burnin"]], 10000L)
  expect_identical(resObject$modelOptions[["iterations"]], 10000L)
  expect_identical(resObject$modelOptions[["thinning"]], 10L)
  expect_identical(resObject$modelOptions[["nchains"]], 1L)
  expect_identical(resObject$modelOptions[["hierarchical"]], FALSE)
  expect_identical(resObject$modelOptions[["weightsDist"]], "normal")
  expect_identical(resObject$modelOptions[["sourceDist"]], list(default = "normal"))
  expect_identical(resObject$modelOptions[["sourceDistCovRep"]], list(default = FALSE))
  expect_identical(resObject$modelOptions[["concentrationDist"]], "normal")
  expect_identical(resObject$modelOptions[["concentrationDistCovRep"]], FALSE)
  expect_identical(resObject$modelOptions[["obsvnDist"]], list(default = "normal"))
  expect_identical(resObject$modelOptions[["inflatedBeta"]], "0")
  expect_identical(resObject$modelOptions[["alphaHyper"]], 1L)
  expect_identical(resObject$modelOptions[["covariateType"]], "0")
  
  expect_identical(resObject$constants[["nTargets"]], 55L)
  expect_identical(resObject$constants[["nSources"]], 2L)
  expect_identical(resObject$constants[["nFractions"]], 2L)
  expect_identical(resObject$constants[["nProxies"]], 2L)
  expect_identical(resObject$constants[["nHierLevels"]], 0)
  
  expect_identical(resObject$userEstimates[[1]], list())
  expect_identical(resObject$userEstimates[[2]], list())
  
  expect_identical(resObject$valueNames[["targets"]], c("d13C", "d15N"))
  expect_identical(resObject$valueNames[["fractions"]], c("d13C", "d15N"))
  expect_identical(resObject$valueNames[["sources"]], c("Natural", "Human"))
  
  expect_identical(
    as.character(resObject$modelCode),
    c(
      "{",
      "for (h_ in 1:nTargets) {\n    for (k_ in 1:nProxies) {\n        for (j_ in 1:nFractions) {\n            component.contrib_[h_, j_, k_] <- W_[k_, j_] * sum(alpha_[h_, 1:nSources] * C_[1:nSources, j_] * (I_[1:nSources, j_, k_]))/sum(alpha_[h_, 1:nSources] * C_[1:nSources, j_])\n        }\n        mu[h_, k_] <- sum(component.contrib_[h_, 1:nFractions, k_])/sum(W_[k_, 1:nFractions])\n        obsvn[h_, k_] ~ dnorm(mu[h_, k_], pow(obsvnError[h_, k_], -2))\n    }\n}",
      "for (h_ in 1:nTargets) {\n    for (j_ in 1:nFractions) {\n        beta_[h_, j_] <- sum(alpha_[h_, 1:nSources] * C_[1:nSources, j_])/sum(alpha_[h_, 1:nSources] %*% C_[1:nSources, 1:nFractions])\n    }\n}",
      "for (h_ in 1:nTargets) {\n    for (i_ in 1:nSources) {\n        for (j_ in 1:nFractions) {\n            aux6_[h_, i_, j_] <- alpha_[h_, i_] * C_[i_, j_]/sum(alpha_[h_, 1:nSources] * C_[1:nSources, j_])\n        }\n    }\n    for (k_ in 1:nProxies) {\n        for (i_ in 1:nSources) {\n            theta_[h_, k_, i_] <- sum(aux6_[h_, i_, 1:nFractions] * W_[k_, 1:nFractions])/sum(aux6_[h_, 1:nSources, 1:nFractions] %*% W_[k_, 1:nFractions])\n        }\n    }\n}",
      "for (h_ in 1:nTargets) {\n    alpha_[h_, 1:nSources] ~ ddirch(sourceDirichPrior[1:nSources])\n}",
      "for (k_ in 1:nProxies) {\n    for (j_ in 1:nFractions) {\n        for (i_ in 1:nSources) {\n            I_[i_, j_, k_] <- I0_[i_, j_, k_]\n        }\n    }\n}",
      "for (k_ in 1:nProxies) {\n    for (j_ in 1:nFractions) {\n        for (i_ in 1:nSources) {\n            I0_[i_, j_, k_] ~ dnorm(source[i_, j_, k_], pow(sourceUncert[i_, j_, k_], -2))\n        }\n    }\n}",
      "for (j_ in 1:nFractions) {\n    for (i_ in 1:nSources) {\n        C_[i_, j_] ~ T(dnorm(concentration[i_, j_], pow(concentrationUncert[i_, j_], -2)), 0, 100)\n    }\n}",
      "for (k_ in 1:nProxies) {\n    for (j_ in 1:nFractions) {\n        W_[k_, j_] ~ T(dnorm(weights[k_, j_], pow(weightsUncert[k_, j_], -2)), -1e-04, 100)\n    }\n}"
    )
  )
  
})

test_that("test shinyInputToClass - brownBearData with default inputs", {
  testData <-
    readRDS(testthat::test_path("testdata", "brownBearData_default.rds"))
  
  # new column added: values[["optimalPrior"]], default value == TRUE
  testData$optimalPrior <- TRUE
  
  resObject <- shinyInputToClass(testData,
                                 as.list(NULL),
                                 as.list(NULL))
  
  expect_identical(resObject$data[["obsvn"]][1:3], c(-22.3, -23.1, -21.5))
  expect_identical(resObject$data[["obsvnError"]][1:3], c(1, 1, 1))
  expect_identical(resObject$data[["weights"]],
                   structure(
                     100,
                     dim = c(1L, 1L),
                     dimnames = list("d13C", "d13C")
                   ))
  expect_identical(resObject$data[["weightsUncert"]],
                   structure(
                     0,
                     dim = c(1L, 1L),
                     dimnames = list("d13C", "d13C")
                   ))
  expect_identical(resObject$data[["concentration"]], NULL)
  expect_identical(resObject$data[["concentrationUncert"]], NULL)
  expect_identical(resObject$data[["source"]],
                   structure(
                     c(-25.798, -10.714),
                     dim = c(2L, 1L, 1L),
                     dimnames = list(c("C3", "Corn"), "d13C", "d13C")
                   ))
  expect_identical(resObject$data[["sourceUncert"]],
                   structure(
                     c(2.2247, 1.0367),
                     dim = c(2L, 1L, 1L),
                     dimnames = list(c("C3", "Corn"), "d13C", "d13C")
                   ))
  expect_identical(resObject$data[["covariates"]][1:3], c("f", "f", "f"))
  expect_identical(resObject$data[["sourceDirichPrior"]], c(0.4, 0.4)) # if optimalPrior == TRUE

  expect_identical(resObject$priors, list())
  
  expect_identical(resObject$modelOptions[["modelType"]], "2")
  expect_identical(resObject$modelOptions[["modelWeights"]], FALSE)
  expect_identical(resObject$modelOptions[["categoricalVars"]], c("group", "year"))
  expect_identical(resObject$modelOptions[["numericVars"]], character(0))
  expect_identical(resObject$modelOptions[["modelWeightsContrained"]], TRUE)
  expect_identical(resObject$modelOptions[["modelConcentrations"]], FALSE)
  expect_identical(resObject$modelOptions[["modelConcentrationsContrained"]], TRUE)
  expect_identical(resObject$modelOptions[["minUnc"]], 0.005)
  expect_identical(resObject$modelOptions[["targetOffset"]], FALSE)
  expect_identical(resObject$modelOptions[["includeSourceOffset"]], FALSE)
  expect_identical(resObject$modelOptions[["burnin"]], 10000)
  expect_identical(resObject$modelOptions[["iterations"]], 10000)
  expect_identical(resObject$modelOptions[["thinning"]], 10)
  expect_identical(resObject$modelOptions[["nchains"]], 1)
  expect_identical(resObject$modelOptions[["hierarchical"]], TRUE)
  expect_identical(resObject$modelOptions[["weightsDist"]], list(default = "normal"))
  expect_identical(resObject$modelOptions[["sourceDist"]], list(default = "normal"))
  expect_identical(resObject$modelOptions[["sourceDistCovRep"]], list(default = FALSE))
  expect_identical(resObject$modelOptions[["concentrationDist"]], "normal")
  expect_identical(resObject$modelOptions[["concentrationDistCovRep"]], FALSE)
  expect_identical(resObject$modelOptions[["obsvnDist"]], list(default = "normal"))
  expect_identical(resObject$modelOptions[["inflatedBeta"]], "0")
  expect_identical(resObject$modelOptions[["alphaHyper"]], 1)
  expect_identical(resObject$modelOptions[["covariateType"]], "2")
  
  expect_identical(resObject$constants[["nTargets"]], 40L)
  expect_identical(resObject$constants[["nSources"]], 2L)
  expect_identical(resObject$constants[["nFractions"]], 1L)
  expect_identical(resObject$constants[["nProxies"]], 1L)
  expect_identical(resObject$constants[["nHierLevels"]], 4L)
  
  expect_identical(resObject$userEstimates[[1]], list())
  expect_identical(resObject$userEstimates[[2]], list())
  
  expect_identical(resObject$valueNames[["targets"]], c("d13C"))
  expect_identical(resObject$valueNames[["fractions"]], c("d13C"))
  expect_identical(resObject$valueNames[["sources"]], c("C3", "Corn"))
  
  expect_identical(
    as.character(resObject$modelCode),
    c(
      "{",
      "for (h_ in 1:nTargets) {\n    for (k_ in 1:nProxies) {\n        for (j_ in 1:nFractions) {\n            component.contrib_[h_, j_, k_] <- W_[k_, j_] * sum(alpha_[h_, 1:nSources] * (I_[1:nSources, j_, k_]))/sum(alpha_[h_, 1:nSources])\n        }\n        mu[h_, k_] <- sum(component.contrib_[h_, 1:nFractions, k_])/sum(W_[k_, 1:nFractions])\n        obsvn[h_, k_] ~ dnorm(mu[h_, k_], pow(obsvnError[h_, k_], -2))\n    }\n}",
      "for (h_ in 1:nTargets) {\n    for (j_ in 1:nFractions) {\n        beta_[h_, j_] <- sum(alpha_[h_, 1:nSources])/sum(alpha_[h_, 1:nSources])\n    }\n}",
      "for (h_ in 1:nTargets) {\n    for (i_ in 1:nSources) {\n        for (j_ in 1:nFractions) {\n            aux6_[h_, i_, j_] <- alpha_[h_, i_]/sum(alpha_[h_, 1:nSources])\n        }\n    }\n    for (k_ in 1:nProxies) {\n        for (i_ in 1:nSources) {\n            theta_[h_, k_, i_] <- sum(aux6_[h_, i_, 1:nFractions] * W_[k_, 1:nFractions])/sum(aux6_[h_, 1:nSources, 1:nFractions] %*% W_[k_, 1:nFractions])\n        }\n    }\n}",
      "for (h_ in 1:nTargets) {\n    dirParams[h_, 1:nSources] <- q[hierMatch[h_], 1:nSources] * sourceDirichPrior[1:nSources]\n    alpha_[h_, 1:nSources] ~ ddirch(dirParams[h_, 1:nSources])\n}",
      "for (i_ in 1:nSources) {\n    for (g_ in 1:nHierLevels) {\n        q[g_, i_] ~ dlnorm(muQ, sdQ)\n    }\n}",
      "muQ ~ dnorm(0, 5)",
      "sdQ ~ dunif(0, 5)",
      "for (k_ in 1:nProxies) {\n    for (j_ in 1:nFractions) {\n        for (i_ in 1:nSources) {\n            I_[i_, j_, k_] <- I0_[i_, j_, k_]\n        }\n    }\n}",
      "for (k_ in 1:nProxies) {\n    for (j_ in 1:nFractions) {\n        for (i_ in 1:nSources) {\n            I0_[i_, j_, k_] ~ dnorm(source[i_, j_, k_], pow(sourceUncert[i_, j_, k_], -2))\n        }\n    }\n}",
      "for (k_ in 1:nProxies) {\n    for (j_ in 1:nFractions) {\n        W_[k_, j_] ~ T(dnorm(weights[k_, j_], pow(weightsUncert[k_, j_], -2)), -1e-04, 100)\n    }\n}"
    )
  )
  
})
