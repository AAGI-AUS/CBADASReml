skip_on_cran()
library(asreml)
model <- asreml(
    fixed = yield ~ Variety,
    random = ~units,
    residual = ~ ar1v(Row):ar1(Column),
    data = shf,
    trace = FALSE
)

test_that("table matches output as if we used agricolae::orderPvalue", {
    agricolae_table <-
        data.frame(
            group = c(
                "a",
                "a",
                "ab",
                "abc",
                "abcd",
                "abcde",
                "bcde",
                "bcdef",
                "bcdef",
                "bcdef",
                "cdef",
                "defg",
                "defg",
                "defg",
                "efgh",
                "fghi",
                "fghi",
                "ghij",
                "hijk",
                "ijkl",
                "ijkl",
                "ijkl",
                "jkl",
                "kl",
                "l"
            ),
            treatment = c(
                "19",
                "20",
                "13",
                "22",
                "25",
                "24",
                "18",
                "6",
                "2",
                "21",
                "17",
                "5",
                "15",
                "8",
                "12",
                "4",
                "3",
                "7",
                "11",
                "23",
                "14",
                "16",
                "9",
                "1",
                "10"
            ),
            lsd = c(
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256,
                120.5256
            )
        )

    lt <- lsd_table(model, "Variety")
    lt$lsd <- round(lt$lsd, 4)

    expect_equal(lt$group, agricolae_table$group)
    expect_equal(lt$treatment, agricolae_table$treatment)
    expect_equal(lt$lsd, agricolae_table$lsd)
})
