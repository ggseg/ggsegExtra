suppressMessages(
  j <- sapply(
    c("dplyr", "tidyr",
      "ggseg", "ggseg3d"),
    library, character.only=TRUE
  )
)
