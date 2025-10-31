@ExportKotlinClass
@KotlinExportType(type = "Proxy", lang = "Kotlin")
class Bar {
  @ExportKotlinField(alias = "json_value", type = "Int")
  value : number
  
  @ExportKotlinFunction
  bar(x: number): number {
    return x + 1
  }
}