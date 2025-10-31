@ExportKotlinClass
class Bar {
  @ExportKotlinField(type = "FooProxy")
  obj : Foo
  @ExportKotlinField(type = "Double")
  value : number
  
  @ExportKotlinFunction(type = "Int", type = "Double", type = "FooProxy", type = "Double")
  bar(x: number, y : number ,z : Foo): number {
    return x + 1
  }
}
