obj <- new_my_object(c(1L, 2L, 3L))

expect_true(inherits(obj, "my_object"))
expect_true(is.list(obj))
expect_equal(obj$data, c(1L, 2L, 3L))
expect_error(new_my_object(c(1, 2, 3)))


