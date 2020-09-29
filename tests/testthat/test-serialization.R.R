context("serialization.R")

test_that(
  "can deserialize primitives",
  {
    expect_identical(unpack("42", .mlc_integer), 42L)
    expect_identical(unpack("\"42\"", .mlc_character), "42")
    expect_identical(unpack("true", .mlc_logical), T)
    expect_identical(unpack("false", .mlc_logical), F)
    expect_identical(unpack("\"false\"", .mlc_character), "false")
    expect_identical(unpack("null", .mlc_null), NULL)
  }
)

test_that(
  "can serialize primitives",
  {
    expect_identical(pack(as.integer(42), .mlc_integer), "42")
    expect_identical(pack(T, .mlc_logical), "true")
    expect_identical(pack(F, .mlc_logical), "false")
    expect_identical(pack(NULL, .mlc_null), "null")
  }
)

test_that(
  "can deserialize lists",
  {
    expect_identical( unpack("[]", .mlc_list(.mlc_integer)),   integer(0))
    expect_identical( unpack("[]", .mlc_list(.mlc_numeric)),   numeric(0))
    expect_identical( unpack("[]", .mlc_list(.mlc_character)), character(0))
    expect_identical( unpack("[]", .mlc_list(.mlc_logical)),   logical(0))
    expect_identical(
      unpack("[1,2,3]", .mlc_list(.mlc_integer)),
      as.integer(c(1,2,3))
    )
    expect_identical(
      unpack("[\"1\",\"2\",\"3\"]", .mlc_list(.mlc_character)),
      c("1","2","3")
    )
    expect_identical(
      unpack("[[1,2],[3,4]]", .mlc_list(.mlc_tuple("integer", "integer"))),
      list(as.integer(c(1,2)), as.integer(c(3,4)))
    )
  }
)

test_that(
  "can serialize lists",
  {
    expect_identical( pack(integer(0),   .mlc_list(.mlc_integer)),   "[]")
    expect_identical( pack(numeric(0),   .mlc_list(.mlc_numeric)),   "[]")
    expect_identical( pack(character(0), .mlc_list(.mlc_character)), "[]")
    expect_identical( pack(logical(0),   .mlc_list(.mlc_logical)),   "[]")
    expect_identical(
      pack(as.integer(1:3), .mlc_list(.mlc_integer)),
      "[1,2,3]"
    )
    expect_identical(
      pack(as.character(1:3), .mlc_list(.mlc_character)),
      "[\"1\",\"2\",\"3\"]"
    )
    expect_identical(
      pack(list(as.integer(c(1,2)), as.integer(c(3,4))), .mlc_list(.mlc_tuple("integer", "integer"))),
      "[[1,2],[3,4]]"
    )
  }
)
