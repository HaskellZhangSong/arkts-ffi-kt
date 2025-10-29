# arkts-ffi-kt

This project will generate kotlin Proxy code from arkts(typescript). You should parse your arkts or typescript code with [typescript-parser](https://github.com/HaskellZhangSong/typescript-parser), then use it as an input of this project.

## build

1. 克隆这个项目：

```
git clone https://github.com/HaskellZhangSong/arkts-ffi-kt.git
```

2. 构建需要stack工具

这里下载[stack](https://docs.haskellstack.org/en/stable/)

注：可以使用清华的[stackage](https://mirrors.tuna.tsinghua.edu.cn/help/stackage/)镜像源

修改`~/.stack/config.yaml`（在 Windows 下是 `%APPDATA%\stack\config.yaml`）, 加上：
```
setup-info-locations: ["https://mirrors.tuna.tsinghua.edu.cn/stackage/stack-setup.yaml"]
urls:
  latest-snapshot: https://mirrors.tuna.tsinghua.edu.cn/stackage/snapshots.json

snapshot-location-base: https://mirrors.tuna.tsinghua.edu.cn/stackage/stackage-snapshots/
```

```
stack build

Installing library in /Users/songzh/project/haskell/arkts-ffi-kt/.stack-work/install/aarch64-osx/6684ad97e0586094be9dd3eb4bce9a3a70d6581292e19e97b2998cf7a7340e8d/9.10.1/lib/aarch64-osx-ghc-9.10.1-64dd/arkts-ffi-kt-0.1.0.0-DlbVOXv1Jlt17Cd5Ebh1q1
Installing executable arkts-ffi-kt-exe in /Users/songzh/project/haskell/arkts-ffi-kt/.stack-work/install/aarch64-osx/6684ad97e0586094be9dd3eb4bce9a3a70d6581292e19e97b2998cf7a7340e8d/9.10.1/bin
```

`arkts-ffi-kt-exe` 就是需要的可执行文件。

## Use

```
arkts-ffi-kt-exe example2.ets -o test.kt
```

For `example/cases/case5_class_field_func_global_func.ets`

```typescript
@KotlinExportClass
class Bar {
  @KotlinExportField(type = "FooProxy")
  obj : Foo
  @KotlinExportField(type = "Int")
  value : number
  
  @KotlinExportFunction(type = "Int", type = "Double", type = "FooProxy", type = "Double")
  bar(x: number, y : number ,z : Foo): number {
    return x + 1
  }
}
```

it will generate following kotlin code:

```kotlin
package arkts.ffi

class BarProxy(ref: ArkObjectSaafeReference) {
  var obj: FooProxy
    get() = FooProxy(getProperty("obj"))
    set(value) = setProperty("obj", value)
  var value: Int
    get() = getInt("value")
    set(value) = setInt("value", value)
  fun bar(x: Int, y: Double, z: FooProxy): Double {
    callMethod<Double>("bar", x, y, z.ref)
  }
}
```