@KotlinExportClass
@KotlinExportType(type = "Proxy", lang = "Kotlin")
class Bar {
  @KotlinExportField(type = "FooProxy")
  obj : Foo
  @KotlinExportField(alias = "json_value", type = "Int")
  value : number
  
  @KotlinExportFunction(type = "Int", type = "Double", type = "FooProxy")
  bar(x: number, y : number ,z : Foo): number {
    return x + 1
  }
}

@KotlinExportFunction(type = "abc")
function global_foo(x: number, y: string) {
  bala
  return 10
}