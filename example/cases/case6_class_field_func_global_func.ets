@ExportKotlinClass
class Bar {
  @ExportKotlinField(type = "FooProxy")
  obj : Foo
  @ExportKotlinField(type = "Int")
  value : number
  
  @ExportKotlinFunction(type = "Int", type = "Double", type = "FooProxy", type = "BarProxy")
  bar(x: number, y : number ,z : Foo): Bar {
    return x + 1
  }
}

@ExportKotlinFunction(type = "abc")
function global_foo(x: number, y: string): number {
  bala
  return 10
}