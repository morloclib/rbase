context("serialization.R")

test_that(
  "can deserialize primitives",
  {
    expect_identical(deserialize("42", .mlc_integer), 42L)
    expect_identical(deserialize("\"42\"", .mlc_character), "42")
    expect_identical(deserialize("true", .mlc_logical), T)
    expect_identical(deserialize("false", .mlc_logical), F)
    expect_identical(deserialize("\"false\"", .mlc_character), "false")
    expect_identical(deserialize("null", .mlc_null), NULL)
  }
)

test_that(
  "can serialize primitives",
  {
    expect_identical(serialize(as.integer(42), .mlc_integer), "42")
    expect_identical(serialize(T, .mlc_logical), "true")
    expect_identical(serialize(F, .mlc_logical), "false")
    expect_identical(serialize(NULL, .mlc_null), "null")
  }
)

test_that(
  "can deserialize lists",
  {
    expect_identical( deserialize("[]", .mlc_list(.mlc_integer)),   integer(0))
    expect_identical( deserialize("[]", .mlc_list(.mlc_numeric)),   numeric(0))
    expect_identical( deserialize("[]", .mlc_list(.mlc_character)), character(0))
    expect_identical( deserialize("[]", .mlc_list(.mlc_logical)),   logical(0))
    expect_identical(
      deserialize("[1,2,3]", .mlc_list(.mlc_integer)),
      as.integer(c(1,2,3))
    )
    expect_identical(
      deserialize("[\"1\",\"2\",\"3\"]", .mlc_list(.mlc_character)),
      c("1","2","3")
    )
    expect_identical(
      deserialize("[[1,2],[3,4]]", .mlc_list(.mlc_tuple("integer", "integer"))),
      list(as.integer(c(1,2)), as.integer(c(3,4)))
    )
  }
)

test_that(
  "can serialize lists",
  {
    expect_identical( serialize(integer(0),   .mlc_list(.mlc_integer)),   "[]")
    expect_identical( serialize(numeric(0),   .mlc_list(.mlc_numeric)),   "[]")
    expect_identical( serialize(character(0), .mlc_list(.mlc_character)), "[]")
    expect_identical( serialize(logical(0),   .mlc_list(.mlc_logical)),   "[]")
    expect_identical(
      serialize(as.integer(1:3), .mlc_list(.mlc_integer)),
      "[1,2,3]"
    )
    expect_identical(
      serialize(as.character(1:3), .mlc_list(.mlc_character)),
      "[\"1\",\"2\",\"3\"]"
    )
    expect_identical(
      serialize(list(as.integer(c(1,2)), as.integer(c(3,4))), .mlc_list(.mlc_tuple("integer", "integer"))),
      "[[1,2],[3,4]]"
    )
  }
)
