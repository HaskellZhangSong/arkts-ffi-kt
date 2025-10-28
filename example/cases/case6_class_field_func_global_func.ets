@KotlinExportClass
class Bar {
  @KotlinExportField(type = "FooProxy")
  obj : Foo
  @KotlinExportField(type = "Int")
  value : number
  
  @KotlinExportFunction(type = "Int", type = "Double", type = "FooProxy", type = "BarProxy")
  bar(x: number, y : number ,z : Foo): Bar {
    return x + 1
  }
}

@KotlinExportFunction(type = "abc")
function global_foo(x: number, y: string) {
  bala
  return 10
}