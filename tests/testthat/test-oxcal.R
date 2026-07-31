test_that("getCurveTitlesXlsx - terrestrial.xlsx", {
  file1 <- safe_read_xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/b7732618-7764-460a-b1fa-c614f4cdbe95/download/terrestrial.xlsx"
  )

  expect_equal(
    getCurveTitlesXlsx(file1),
    structure(1:3, .Names = c("IntCal20", "SHCal20", "Mix IntCal20 andSHCal20"))
  )
})

test_that("getCurveTitlesXlsx_2", {
  file2 <- safe_read_xlsx("https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/2037632f-f984-4834-8e25-4af5498df163/download/aquatic1.xlsx")

  expect_equal(
    getCurveTitlesXlsx(file2),
    structure(1:2, .Names = c("Marine20", "Same as terrestrial"))
  )
})

test_that("getCurveTitlesXlsx_3", {
  file3 <- safe_read_xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/120d810e-ff7d-49b7-80b8-e9791e2980b3/download/aquatic2.xlsx"
  )

  expect_equal(
    getCurveTitlesXlsx(file3),
    structure(1:2, .Names = c("Marine20", "Same as terrestrial"))
  )
})

test_that("getCodeTerrestrial", {
  file <- safe_read_xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/b7732618-7764-460a-b1fa-c614f4cdbe95/download/terrestrial.xlsx"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[1]), ],
      mixOption = NULL,
      mixParams = NULL
    ),
    "Curve(\"terrestrial\",\"IntCal20.14c\");"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      mixOption = NULL,
      mixParams = NULL
    ),
    "Curve(\"terrestrial\",\"SHCal20.14c\");"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[3]), ],
      mixOption = "Option point",
      mixParams = c(3, 0)
    ),
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 3);"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[3]), ],
      mixOption = "Option Mean SD",
      mixParams = c(2, 1)
    ),
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 2,1);"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[3]), ],
      mixOption = "Option uniform",
      mixParams = c(0, 2)
    ),
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", U(0,2));"
  )
})

test_that("getCodeAquatic", {
  file <- safe_read_xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/2037632f-f984-4834-8e25-4af5498df163/download/aquatic1.xlsx"
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[1]), ],
      binOption = "Option Mean SD",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      binOption = "Option PDF",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )
  
  file <- safe_read_xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/120d810e-ff7d-49b7-80b8-e9791e2980b3/download/aquatic2.xlsx"
  )
  
  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[1]), ],
      binOption = "Option PDF",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )
  
  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      binOption = "Option Mean SD",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )
})

test_that("Create OxCal Output", {
  data_path <- testthat::test_path("testdata_large", "test-oxcalModelData.rds")
  skip_if_not(file.exists(data_path), "Skipping large data test on CI or for devtools:check()")
  
  model <- readRDS(data_path)
  basicCode <-
    c(
      "Plot()",
      "{",
      "%%Terrestrial_curve_VAR1%%",
      "%%Aquatic_curve_1_VAR1%%",
      "%%Aquatic_curve_2_VAR1%%",
      "",
      "%%String_from_loop%%",
      "",
      "",
      "};"
    )
  
  terrestrialCurve <-
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 2,1);"
  
  aquaticCurve1Mean <-
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  
  aquaticCurve1PDF <-
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  
  aquaticCurve2PDF <-
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  
  aquaticCurve2Mean <-
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  
  oxCalA <- "Source contributions_Carnivores"
  oxCalB <- "Source contributions_Fish2"
  exportCoordinates <- structure(
    c(45, 50, 55, 56, 43, 0, 5, 10, 6, 16, 1001:1005, 1:5),
    .Dim = 5:4,
    .Dimnames = list(
      c(
        "Individual_1",
        "Individual_2",
        "Individual_3",
        "Individual_4",
        "Individual_5"
      ),
      c(
        "longitude",
        "latitude",
        "LowerLimit/Mean/Point",
        "UpperLimit/SD"
      )
    )
  )

  oxcalLines1 <- createOxCalText(
    model = model,
    basicCode = basicCode %>% paste(collapse = "\n"),
    terrestrialCurve = terrestrialCurve,
    aquaticCurve1 = aquaticCurve1Mean,
    aquaticCurve2 = aquaticCurve2Mean,
    OxCalA = oxCalA,
    OxCalB = oxCalB,
    bins = FALSE,
    coordinates = exportCoordinates
  ) %>%
    strsplit(split = "\n") %>%
    unlist()

  oxcalLines2 <- createOxCalText(
    model = model,
    basicCode = basicCode %>% paste(collapse = "\n"),
    terrestrialCurve = terrestrialCurve,
    aquaticCurve1 = aquaticCurve1PDF,
    aquaticCurve2 = aquaticCurve2PDF,
    OxCalA = oxCalA,
    OxCalB = oxCalB,
    bins = TRUE,
    coordinates = exportCoordinates
  ) %>%
    strsplit(split = "\n") %>%
    unlist()

  expect_equal(oxcalLines1[1], "Plot()")
  expect_equal(oxcalLines1[3], "Curve(\"IntCal20\",\"IntCal20.14c\");\r")
  expect_equal(
    oxcalLines1[11] %>% substr(start = 1, stop = 100),
    "Mix_Curve(\"Individual_1\",\"terrestrial\",\"Aquatic1\", 0.434,0.294);"
  )
  expect_equal(
    oxcalLines1[13] %>% substr(start = 1, stop = 100),
    "R_Date(“Individual_1“, 1001,1);"
  )
  expect_equal(
    oxcalLines1[42] %>% substr(start = 1, stop = 100),
    "Mix_Curve(\"england.male\",\"terrestrial\",\"Aquatic1\", 0.083,0.101);"
  )

  expect_equal(oxcalLines2[1], "Plot()")
  expect_equal(oxcalLines2[3], "Curve(\"IntCal20\",\"IntCal20.14c\");\r")
  expect_equal(
    oxcalLines2[11] %>% substr(start = 1, stop = 100),
    "Mix_Curve(\"Individual_1\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0, 0, 0.002, 0.003, 0.007, 0.01,"
  )
  expect_equal(
    oxcalLines2[15] %>% substr(start = 1, stop = 100),
    "Mix_Curve(\"Individual_2\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,"
  )
})
