test_that("returns correct types", {
  res <- lapply(sporeg::results, `[[`, 1) |>
    dplyr::bind_rows() |>
    dplyr::mutate(res_name = "100km")

  df_var <- zero_var(res)
  expect_s3_class(
    df_var,
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})

test_that("removes groups with no variance", {
  res <- lapply(sporeg::results, `[[`, 1) |>
    dplyr::bind_rows() |>
    dplyr::mutate(res_name = "100km") |>
    # Add 50 rows with no variance for demonstration purposes
    dplyr::bind_rows(
      data.frame(gid = rep(100, 50), dif = rep(100, 50), res_name = "100km")
    )

  df_var <- zero_var(res)

  expect_lt(nrow(df_var), nrow(res))
  expect_false(100 %in% df_var$gid)

  df_var |>
    dplyr::summarise(sd = sd(dif), .groups = "drop_last") |>
    (\(.) .$sd != 0)() |>
    all() |>
    expect_true()
})
