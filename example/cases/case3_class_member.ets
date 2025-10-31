@ExportKotlinClass
class Bar {
  @ExportKotlinField(alias = "json_value", type = "Int")
  value : number

  @ExportKotlinField
  strField : string

}