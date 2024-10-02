exampleValues <- function() {
  targetNames <- c("Carbon", "Nitrogen")
  fractionNames <- c("Protein", "Energy")
  sourceNames <- c("Herbivores", "Carnivores", "Plants", "Fish1", "Fish2")
  obsvnNames <- paste("Individual", 1:5, sep = "_")

  list(
    modelType = "2",
    modelWeights = TRUE,
    modelWeightsContrained = TRUE,
    modelConcentrations = TRUE,
    modelConcentrationsContrained = TRUE,
    alphaHyper = c(source_1 = 1),
    optimalPrior = TRUE,
    covariateType = 2,
    targetOffset = TRUE,
    minUnc = 1E-4,
    obsvn = list(
      default = matrix(
        c(-19, -15, -17, -16, -21, 14, 15, 12, 17, 13),
        ncol = 2,
        dimnames = list(obsvnNames, targetNames)
      )
    ),
    targetValuesCovariates = matrix(
      c(
        "male", "male", "female", "female", "female",
        "germany", "england", "germany", "england", "germany"
      ),
      dimnames = list(obsvnNames, c("sex", "country")),
      ncol = 2
    ),
    obsvnError = list(
      default = matrix(
        rep(0.5, 10),
        ncol = length(targetNames),
        dimnames = list(obsvnNames, targetNames)
      )
    ),
    weights = matrix(
      c(74, 100, 24, 0),
      nrow = length(fractionNames),
      ncol = length(targetNames),
      dimnames = list(targetNames, fractionNames)
    ),
    weightsUncert = matrix(
      c(0.5, 0, 0.5, 0),
      nrow = length(fractionNames),
      ncol = length(targetNames),
      dimnames = list(targetNames, fractionNames)
    ),
    targetValuesShowCovariates = TRUE,
    weightOffset = matrix(
      c(4, 7),
      nrow = 2,
      dimnames = list(targetNames, "Offset")
    ),
    weightOffsetUncert = matrix(
      c(.5, 1),
      nrow = 2,
      dimnames = list(targetNames, "Offset")
    ),
    concentration = matrix(
      c(80, 52, 10, 90, 92, 20, 48, 90, 10, 8),
      nrow = length(sourceNames),
      ncol = length(fractionNames),
      dimnames = list(sourceNames, fractionNames)
    ),
    concentrationUncert = matrix(
      c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
      nrow = length(sourceNames),
      ncol = length(fractionNames),
      dimnames = list(sourceNames, fractionNames)
    ),
    source = list(
      default = list(
        list(
          Carbon = matrix(
            c(-19, -21, -25, -13, -19, -24, -26, -29, -16, -23),
            nrow = 5,
            dimnames = list(sourceNames, fractionNames)
          ),
          Nitrogen = matrix(
            c(5, 7, 1, 10, 11, 0, 0, 0, 0, 0),
            nrow = 5,
            dimnames = list(sourceNames, fractionNames)
          )
        )
      )
    ),
    sourceUncert = list(
      default = list(
        list(
          Carbon = matrix(
            rep(0.5, 10),
            nrow = 5,
            dimnames = list(sourceNames, fractionNames)
          ),
          Nitrogen = matrix(
            c(rep(0.5, 5), rep(1E-6, 5)),
            nrow = 5,
            dimnames = list(sourceNames, fractionNames)
          )
        )
      )
    ),
    includeSourceOffset = FALSE,
    sourceOffset = list(
      list(
        Carbon = matrix(
          NA,
          nrow = 5,
          ncol = 2,
          dimnames = list(sourceNames, fractionNames)
        ),
        Nitrogen = matrix(
          NA,
          nrow = 5,
          ncol = 2,
          dimnames = list(sourceNames, fractionNames)
        )
      )
    ),
    sourceOffsetUncert = list(
      list(
        Carbon = matrix(
          NA,
          nrow = 5,
          ncol = 2,
          dimnames = list(sourceNames, fractionNames)
        ),
        Nitrogen = matrix(
          NA,
          nrow = 5,
          ncol = 2,
          dimnames = list(sourceNames, fractionNames)
        )
      )
    ),
    sourceDistribution = list(default = "normal"),
    sourceDistCovRep = list(default = FALSE),
    weightDistribution = "normal",
    concentrationDistribution = "normal",
    concentrationDistCovRep = FALSE,
    obsvnDistribution = list(default = "normal"),
    sourceCovariance = list(),
    targetValuesCovariance = list(
      default = list(
        Individual_1 = matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(targetNames, targetNames)),
        Individual_2 = matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(targetNames, targetNames)),
        Individual_3 = matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(targetNames, targetNames)),
        Individual_4 = matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(targetNames, targetNames)),
        Individual_5 = matrix(c(1, 0, 0, 1), 2, 2, dimnames = list(targetNames, targetNames))
      )
    ),
    burnin = 10000,
    iterations = 10000,
    fileNotes = "This is an example model for testing purposes.",
    inflatedBeta = "0",
    targetValuesShowCoordinates = TRUE,
    exportCoordinates = matrix(
      c(45, 50, 55, 56, 43, 0, 5, 10, 6, 16, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 5, 4,
      dimnames = list(
        paste("Individual", 1:5, sep = "_"),
        c("Longitude", "Latitude", "LowerLimit/Mean/Point", "UpperLimit/SD")
      )
    ),
    userEstimateGroups = list(),
    priors = character(),
    userEstimate = character()
  )
}
