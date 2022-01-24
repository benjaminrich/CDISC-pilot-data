# Get it here:
#   https://github.com/cdisc-org/sdtm-adam-pilot-project/tree/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01

devtools::load_all()  # Load the package functions

# SDTM

options(path.sdtm="./cdiscpilot01/sdtm")

ae <- read_sdtm("ae")
cm <- read_sdtm("cm")
dm <- read_sdtm("dm")
ds <- read_sdtm("ds")
ex <- read_sdtm("ex")
lb <- read_sdtm("lb")
mh <- read_sdtm("mh")
qs <- read_sdtm("qs")
sc <- read_sdtm("sc")
se <- read_sdtm("se")
sv <- read_sdtm("sv")
ta <- read_sdtm("ta")
te <- read_sdtm("te")
ti <- read_sdtm("ti")
ts <- read_sdtm("ts")
tv <- read_sdtm("tv")
vs <- read_sdtm("vs")

relrec <- read_sdtm("relrec")

# ADaM

options(path.adam="./cdiscpilot01/adam")

adae     <- read_adam("adae"    )
adlbc    <- read_adam("adlbc"   )
adlbh    <- read_adam("adlbh"   )
adlbhy   <- read_adam("adlbhy"  )
adqsadas <- read_adam("adqsadas")
adqscibc <- read_adam("adqscibc")
adqsnpix <- read_adam("adqsnpix")
adsl     <- read_adam("adsl"    )
adtte    <- read_adam("adtte"   )
advs     <- read_adam("advs"    )

cdiscpilot01 <- list(

    sdtm <- list(
        ae = ae,
        cm = cm,
        dm = dm,
        ds = ds,
        ex = ex,
        lb = lb,
        mh = mh,
        qs = qs,
        sc = sc,
        se = se,
        sv = sv,
        ta = ta,
        te = te,
        ti = ti,
        ts = ts,
        tv = tv,
        vs = vs),

    adam <- list(
        adae     = adae,
        adlbc    = adlbc,
        adlbh    = adlbh,
        adlbhy   = adlbhy,
        adqsadas = adqsadas,
        adqscibc = adqscibc,
        adqsnpix = adqsnpix,
        adsl     = adsl,
        adtte    = adtte,
        advs     = advs))

save(cdiscpilot01, file="../data/cdiscpilot01.rda", compress="bzip2")

