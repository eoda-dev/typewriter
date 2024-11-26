Person <- typed_struct(
  name = "character",
  age = "integer"
)

hanna <- Person(
  name = "Hanna",
  age = 10L
)

hanna

inherits(hanna, "Person")
inherits(hanna, "list")
