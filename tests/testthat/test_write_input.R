
base_model <- list(
  file_version = 4L,
  Head = "Run from R",
  # units
  LUnit = "cm", TUnit = "days", MUnit = "mmol",
  # model modules
  lWat = TRUE, lChem = TRUE, lTemp = TRUE, lSink = TRUE, lRoot = FALSE,
  lShort = FALSE, lWDep = FALSE, lScreen = TRUE, lVariabBC = TRUE, lEquil = TRUE,
  lInverse = FALSE, lSnow = TRUE, lHP1 = FALSE, lMeteo = FALSE, lVapor = FALSE,
  lActiveU = FALSE, lFluxes = FALSE, lIrrig = FALSE, lIsotope = TRUE,
  # Describing soil and model space
  NMat = 3, NLay = 4, CosAlpha = 1,
  # Parameters for the numerical evaluation
  MaxIt = 50, TolTh = 0.001, TolH = 1,
  # Soil hydraulic model: type and paramaters
  TopInF = TRUE, WLayer = TRUE, KodTop = -1, InitCond = FALSE,
  BotInF = TRUE, qGWLF = FALSE, FreeD = FALSE, SeepF = FALSE, KodBot = 1,
  qDrainF = FALSE, hSeep = 0,
  hTab1 = 1e-5, hTabN = 1e5,
  Model = 0, Hysteresis = 0,
  hydraulics = tibble::tibble(
    thr = rep(0.1, 3),
    ths = 0.498369,
    Alfa = 0.0193021,
    n = 1.20768,
    Ks = 10.8,
    l = 0.5
  ),
  # Time Information, output printing
  dt = 0.001, dtMin = 1e-05, dtMax = 1, dMul = 1.3, dMul2 = 0.7, ItMin = 3, ItMax = 7, MPL = 1000,
  tInit = 0, tMax = 2193, lPrintD = TRUE, nPrintSteps = 1, tPrintInterval = 1, lEnter = FALSE,
  TPrint = matrix(seq(from = 1097, length.out = 1000), nrow = 1),
  # Heat Transport and snow module
  heat = tibble::tibble(
    Qn = rep(0.4827, 3),
    Qo = rep(0.03, 3),
    Disper = rep(5, 3),
    B1 = rep(1.56728E+016, 3),
    B2 = rep(2.53474E+016, 3),
    B3 = rep(9.89388E+016, 3),
    Cn = rep(1.43327E+014, 3),
    Co = rep(1.8737E+014, 3),
    Cw = rep(3.12035E+014, 3)
  ),
  tAmpl = 10, tPeriod = 1, Campbell = 0, MeltConst = 0.43, lSnowInit = TRUE,
  SublimConst = 0, InitSnow = 0,
  kTopT = 1, TTop = 20, kBotT = 1, TBot = 20,
  # Solute Transport
  Epsi = 0.5, lUpW = FALSE, lArtD = FALSE, lTDep = FALSE, cTolA = 0, cTolR = 0,
  MaxItC = 0, PeCr = 2, No.Solutes = 1, lTort = TRUE, iBacter = 0, lFiltr = FALSE, nChPar = 16,
  iNonEqul = 0, lWatDep = FALSE, lDualNEq = FALSE, lInitM = FALSE, lInitEq = FALSE,
  lTortA = FALSE,
  solute = tibble::tibble(
    BulkD = 1.2,
    DisperL = c(5, 7, 8),
    Frac = 1,
    Mobile_WC = 0
  ),
  diffusivity = tibble::tibble(
    DifW = 2.13,
    DifG = 0
  ),
  reaction = tibble::tibble(
    Ks = rep(0, 3), Eta = 0, Beta = 1, Henry = 0, SnkL1 = 0, SnkS1 = 0, SnkG1 = 0,
    "SnkL1'" = 0, "SnkS1'" = 0, "SnkG1'" = 0, SnkL0 = 0, SnkS0 = 0, SnkG0 = 0, Alfa = 0),
  kTopSolute = -1, SolTop = 0, kBotSolute = 0, SolBot = 0,
  tPulse = 0,
  # Root water uptake
  iModSink = 0, cRootMax = 1000, OmegaC = 1,
  P0 = -10, P2H = -200, P2L = -800, P3 = -8000, r2H = 0.5, r2L = 0.1,
  POptm = matrix(-25, ncol = 3),
  lOmegaW = FALSE
)

write_selector(
  base_model,
  folder = "tests/"
)
