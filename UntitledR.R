conditionalPanel(
  condition = "output.scatter",
  checkboxInput(
    inputId = "interaction",
    label = "Interaction Effects",
    value = FALSE
  )
