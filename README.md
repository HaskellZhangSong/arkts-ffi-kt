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
arkts-ffi-kt-exe example2.ets -o test.kt --package-name="ffi.txt"  
```

For `example/cases/case5_class_field_func_global_func.ets`

```typescript
import {
    ExportKotlinClass
  , ExportKotlinField
  , ExportKotlinFunction} from './Decorators'
@ExportKotlinClass({"ark_module_path": "fds/src/main/ets/pages/ClassTest"})
class Bar {
  @ExportKotlinField({"type": "Double"})
  value : number = 0
  @ExportKotlinFunction({"x": "Double", "ark_module_path": "entfdsary/src/main/ets/pages/ClassTest"})
  bar(x:number): Bar {
    return x + 1
  }
}
```

it will generate following kotlin code:

```kotlin
@file:OptIn(ExperimentalForeignApi::class)

package ffi.txt

import com.bytedance.kmp.ohos_ffi.types.FFIProxy
import com.bytedance.kmp.ohos_ffi.types.ArkObjectSafeReference
import platform.ohos.napi.*
import kotlinx.cinterop.*
import com.bytedance.kmp.ohos_ffi.annotation.ArkTsExportClass
import com.bytedance.kmp.ohos_ffi.annotation.ArkTsExportCustomTransform
import com.bytedance.kmp.ohos_ffi.transform.ArkTsExportCustomTransformer
import com.bytedance.kmp.ohos_ffi.types.ArkModule
import com.bytedance.kmp.ohos_ffi.types.transformer.IntTypeTransformer
import com.bytedance.kmp.ohos_ffi.types.transformer.ListTypeTransformer

@ArkTsExportClass(customTransform = true)
class BarProxy {
  val ref: ArkObjectSafeReference

  constructor() {
    var module = ArkModule("fds/src/main/ets/pages/ClassTest")
    var clazz = module.getExportClass("Bar")
    var instance = clazz.newInstance()
    this.ref = ArkObjectSafeReference(instance.getNapiValue())
  }

  constructor(ref: ArkObjectSafeReference) {
    this.ref = ref
  }

  var value: Double
    get() = ref.getDouble("value")!!
    set(value)  {
      ref.setDouble("value", value)
    }

  fun bar(x: Double): BarProxy {
    return BarProxy(ref.callMethod<BarProxy>("bar", x))
  }
}

@ArkTsExportCustomTransform(BarProxy::class)
object BarProxyTransformer : ArkTsExportCustomTransformer<BarProxy> {
  override fun fromJsObject(obj: napi_value): BarProxy {
    return BarProxy(ArkObjectSafeReference(obj))
  }

  override fun toJsObject(obj: BarProxy): napi_value {
    return obj.ref.handle
  }
}
```

A default kotlin type will be used if variable type and function/method parameter type are not given in its annotation.