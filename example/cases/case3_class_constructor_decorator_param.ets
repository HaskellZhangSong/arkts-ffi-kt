@ExportKotlinClass
@KotlinExportType(type = "Int", alias = "foo")
class Bar {
    constructor(x:number, b:string) {
        x + 1
        console.log("this is string")
    }
}