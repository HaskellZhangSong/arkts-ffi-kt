@KotlinExportClass
@KotlinExportType(type = "Proxy", lang = "Kotlin")
class Bar {
  @KotlinExportField(alias = "json_value", type = "Int")
  value : number
  
  @KotlinExportFunction
  bar(x: number): number {
    return x + 1
  }
}