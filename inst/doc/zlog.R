## ----zlog---------------------------------------------------------------------
library("zlog")

albumin <- c(42, 34, 38, 43, 50, 42, 27, 31, 24)
z(albumin, limits = c(35, 52))
zlog(albumin, limits = c(35, 52))

## ----izlog--------------------------------------------------------------------
izlog(zlog(albumin, limits = c(35, 52)), limits = c(35, 52))

## ----zcol, echo = FALSE, out.width = "95%", fig.width = 10, fig.height = 1, fig.align = "center"----
z <- -10:10
oldpar <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
image(matrix(z, ncol = 1), col = zcol(z), axes = FALSE)
text(seq(0, 1, length.out=length(z)), 0, label = z)
par(oldpar)

## ----zcoltable, echo = FALSE, result = "asis"---------------------------------
bilirubin <- c(11, 9, 2, 5, 22, 42, 37, 200, 20)
zloga <- zlog(albumin, limits = c(35, 52))
zlogb <- zlog(bilirubin, limits = c(2, 21))
d <- data.frame(
    Category = c(
        rep(c(
            "blood donor",
            "hepatitis without cirrhosis",
            "hepatitis with cirrhosis"
            ),
            each = 3
        )
    ),
    albumin = albumin,
    zloga = zloga,
    bilirubin = bilirubin,
    zlogb = zlogb
)
d$albumin <- kableExtra::cell_spec(
    d$albumin, background = zcol(zloga), align = "right"
)
d$bilirubin <- kableExtra::cell_spec(
    d$bilirubin, background = zcol(zlogb), align = "right"
)
kableExtra::kable_classic(
    kableExtra::kbl(
        d,
        col.names = c(
            "Category",
            "albumin", "zlog(albumin)", "bilirubin", "zlog(bilirubin)"
        ),
        digits = 2,
        escape = FALSE,
        caption = paste0(
            "Table reproduced from @hoffmann2017, Table 2, limits used: ",
            "albumin 35-52 g/l, bilirubin 2-21 µmol/l."
        )
    ),
    "basic"
)

## ----reference_limits---------------------------------------------------------
reference_limits(albumin)
reference_limits(albumin, probs = c(0.05, 0.95))

exp(reference_limits(log(albumin)))

## ----reference_table----------------------------------------------------------
# toy example
reference <- data.frame(
    param = c("albumin", rep("bilirubin", 4)),
    age = c(0, 1, 2, 3, 7),     # days
    sex = "both",
    units = c("g/l", rep("µmol/l", 4)), # ignored
    lower = c(35, rep(NA, 4)),  # no real reference values
    upper = c(52, 5, 8, 13, 18) # no real reference values
)
knitr::kable(reference)

# lookup albumin reference values for 18 year old woman
lookup_limits(
    age = 18 * 365.25,
    sex = "female",
    table = reference[reference$param == "albumin",]
)

# lookup albumin and bilirubin values for 18 year old woman
lookup_limits(
    age = 18 * 365.25,
    sex = "female",
    table = reference
)

# lookup bilirubin reference values for infants
lookup_limits(
    age = 0:8,
    sex = rep(c("female", "male"), 5:4),
    table = reference[reference$param == "bilirubin",]
)

## ----missing_reference--------------------------------------------------------
# use default fractions
set_missing_limits(reference)

# set fractions manually
set_missing_limits(reference, fraction = c(0.2, 5))

## ----impute_missing_values----------------------------------------------------
x <- data.frame(
    age = c(40, 50),
    sex = c("female", "male"),
    albumin = c(42, NA)
)
x
z_df(impute_df(x, reference, method = "mean"), reference)
zlog_df(impute_df(x, reference), reference)

## ----pbc_load-----------------------------------------------------------------
library("survival")
data("pbc")
labs <- c(
    "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig",
    "platelet", "protime"
)
pbc <- pbc[, c("age", "sex", labs)]
knitr::kable(head(pbc), digits = 1)

## ----pbc_reference_limits-----------------------------------------------------
## replicate copper and ast 2 times, use the others just once
param <- rep(labs, ifelse(labs %in% c("copper", "ast"), 2, 1))
sex <- rep_len("both", length(param))

## replace sex == both with female and male for copper and ast
sex[param %in% c("copper", "ast")] <- c("f", "m")

## create data.frame, we ignore age-specific values for now and set age to zero
## (means applicable for all ages)
reference <- data.frame(
    param = param, age = 0, sex = sex, lower = NA, upper = NA
)

## estimate reference limits from sample data
for (i in seq_len(nrow(reference))) {
    reference[i, c("lower", "upper")] <-
        if (reference$sex[i] == "both")
            reference_limits(pbc[reference$param[i]])
        else
            reference_limits(pbc[pbc$sex == reference$sex[i], reference$param[i]])
}
knitr::kable(reference)

## ----pbc_impute---------------------------------------------------------------
pbc[c(6, 14),]
pbc <- impute_df(pbc, reference)
pbc[c(6, 14),]

## ----pbc_zlog-----------------------------------------------------------------
pbc <- zlog_df(pbc, reference)

## ----pbc_table, echo = FALSE--------------------------------------------------
pbctbl <- head(pbc, n = 25)
pbctbl[labs] <- lapply(labs, function(l) {
    kableExtra::cell_spec(
        sprintf("%.1f", unlist(pbctbl[l])),
        background = zcol(unlist(pbctbl[l])),
        align = "right"
    )
})

kableExtra::kable_classic(
    kableExtra::kbl(pbctbl, digits = 1, escape = FALSE),
    "basic"
)

## ----si-----------------------------------------------------------------------
sessionInfo()

