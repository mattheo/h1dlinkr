
base_opt <- list(
  Head = "Run from R",
  # units
  LUnit = "cm", TUnit = "days", MUnit = "mmol", lDummy = FALSE,
  # modules
  lWat = TRUE, lChem = TRUE, lTemp = FALSE, lSink = TRUE, lRoot = FALSE,
  lShort = FALSE, lWDep = FALSE, lScreen = TRUE, lVariabBC = TRUE, lEquil = TRUE,
  lInverse = FALSE, lSnow = TRUE, lHP1 = FALSE, lMeteo = FALSE, lVapor = FALSE,
  lActiveU = FALSE, lFluxes = FALSE, lIrrig = FALSE, lIsotope = TRUE,
  # Describing soil profile and model domain
  NMat = 3L, NLay = 4L, CosAlpha = 1L, ObsPoints = c(10L, 30L, 50L),
  domain = c(0, 140), grid = 0.2, head = -270, temp = list(x = c(0, 140), y = c(-5, 5)),
  layers = list(y = c(1, 2, 3, 4), x = c(0, 20, 40, 60)), conc = 108,
  # Soil hydraulic model: type and paramaters
  TopInF = TRUE, WLayer = TRUE, KodTop = -1, InitCond = FALSE,
  BotInF = TRUE, qGWLF = FALSE, FreeD = FALSE, SeepF = FALSE, KodBot = 1L,
  qDrainF = FALSE, hSeep = 0L,
  hTab1 = 0L, hTabN = 0L,
  Model = 0L, Hysteresis = 0L,
  hydraulics = tibble::tibble(
    thr = 0.095,
    ths = 0.43,
    alpha = 0.019,
    n = 1.23,
    Ks = 2.4,
    l = 0.5
  ),
  # Numerical evaluation parameter, Time Information, output printing
  MaxIt = 20L, TolTh = 0.001, TolH = 1.0,
  dt = 0.001, dtMin = 1e-05, dtMax = 1.0, dMul = 1.3, dMul2 = 0.7, ItMin = 3L, ItMax = 7L,
  lPrintD = TRUE, nPrintSteps = 1L, tPrintInterval = 1L, lEnter = FALSE,
  tInit = 0, tMax = 100,
  MPL = 10L, TPrint = seq(10, 100, by = 10),
  # Heat Transport and snow module
  heat = tibble::tibble(
    Qn = 0.4827,
    Qo = 0.03,
    Disper = 5.0,
    B1 = 1.56728E+016,
    B2 = 2.53474E+016,
    B3 = 9.89388E+016,
    Cn = 1.43327E+014,
    Co = 1.8737E+014,
    Cw = 3.12035E+014
  ),
  tAmpl = 10.0, tPeriod = 1.0, Campbell = 0L, MeltConst = 0.43, lSnowInit = TRUE,
  SublimConst = 0., InitSnow = 0.,
  kTopT = 1., TTop = 20., kBotT = 1., TBot = 20.,
  # Solute Transport
  Epsi = 0.5, lUpW = FALSE, lArtD = FALSE, lTDep = FALSE, cTolA = 0, cTolR = 0,
  MaxItC = 0L, PeCr = 2L, NSolutes = 1L, lTort = TRUE, iBacter = 0L, lFiltr = FALSE, nChPar = 16L,
  iNonEqul = 0L, lWatDep = FALSE, lDualNEq = FALSE, lInitM = FALSE, lInitEq = FALSE,
  lTortA = FALSE,
  reaction = tibble::tibble(
    Ks = 0,
    Eta = 0,
    Beta = 1,
    Henry = 0,
    SnkL1 = 0,
    SnkS1 = 0,
    SnkG1 = 0,
    SnkL1. = 0,
    SnkS1. = 0,
    SnkG1. = 0,
    SnkL0 = 0,
    SnkS0 = 0,
    SnkG0 = 0,
    Alfa = 0
  ),
  transport = tibble::tibble(
    BulkD = 1.2,
    DisperL = 1.68,
    Frac = 1,
    MobileWc = 0
  ),
  diffusivity = data.frame(DifW = 2.13, DifG = 0),
  kTopSolute = -1, SolTop = 0, kBotSolute = 0, SolBot = 0, tPulse = 0,
  # Root model parameters
  iModSink = 0L, cRootMax = 10000, OmegaC = 1L,
  root = list(
    P0 = -10, P2H = -200, P2L = -800, P3 = -8000, r2H = 0.5, r2L = 0.1, POptm = -25
  ),
  lOmegaW = FALSE, root_densfun = "root_logistic",
  # Atmospheric parameters and boundary conditions
  DailyVar = TRUE, SinusVar = FALSE, lLai = TRUE, lBCCycles = FALSE, lInterc = FALSE,
  rExtinct = 0.463, hCritS = 100
  # varying parameters
)

jsonlite::write_json(base_opt, "data/SA_test/base_opt.json", pretty = T, auto_unbox = T)
# file.show("data/SA_test/base_opt.json")
# test <- jsonlite::fromJSON("data/SA_test/base_opt.json", flatten = F)

atmos_bc <- readRDS("../data/ga_l1_atmosBC.rds")
base_opt$bc <- atmos_bc[1:500, ]
# profile <- do.call(create_profile, args = base_opt)

write_selector(base_opt, stdout())
write_bc(base_opt, stdout())

# write_profile(base_opt, "data/SA_test/")
# template for parameters subject to the SA
varying_template <-list(
  hydraulics = tibble::tibble(
    thr = structure(c(rep(NA, 4), 0.095), lower = 0, upper = 0.1),
    ths = structure(c(rep(NA, 4), 0.43), lower = 0.4, upper = 0.7),
    alpha = structure(c(rep(NA, 4), 0.019), lower = 0, upper = 0.5, distfun = qbeta, shape1 = 0.8, shape2 = 5),
    n = structure(c(rep(NA, 4), 1.23), lower = 1.01, upper = 3, distfun = qbeta, shape1 = 0.8, shape2 = 5),
    Ks = structure(c(rep(NA, 4), 2.4), lower = 0, upper = 100, distfun = qbeta, shape1 = 0.5, shape2 = 15),
    l = structure(rep(NA, 5), lower = -5, upper = 5)
  ),
  transport = tibble::tibble(
    BulkD = 1.2,
    DisperL = structure(c(rep(NA, 4), 1.68), lower = 0, upper = 50),
    Frac = 1,
    MobileWc = 0
  ),
  mats = list(
    x = c(0, 20, 60),
    y = structure(NA, oneof = list(rep.int(1, 3), c(1, 2, 2), 1:3))
  ),
  impounding = structure(NA, oneof = c(TRUE, FALSE)),
  root_denspars = list(
    location = structure(NA, lower = 1e-15, upper = 50),
    scale = structure(NA, lower = 1e-15, upper = 50)
  )
)

nvar <- sum(rapply(varying_template, is.na))
samples <- matrix(runif(n = 1000 * nvar), ncol = nvar)
create_parset(samples[1, ], varying_template)

write_selector(
  replace(base_opt, names(varying_template), create_parset(samples[1, ], varying_template)),
  stdout()
)

# recursivley iterate over a list of lists (or data frames)
# each element is an atomic vector with possible attributes
# if a vector has attributes, on every NA in that vector the
# rescale function is called. The arguments for that function are
#   - a random number [0,1] that is fetched from a vector of random numbers
#     with length nvar which is the number of NAs in the list of lists
#     vector of random numbers:
#   - the attributes belonging to the atomic vector
varying_pars <- apply(samples, 1, create_parset, parameters = varying_template, .fun = rescale)

runs <- readRDS("data/SA_test/out/runs.rds")

xyz <- rapply(
  varying_template,
  function(el) {
    sapply(
      el,
      function(x) {
        if(is.na(x)) {
          attributes(el)
        }
      }
    )
  },
  how = "list"
)

