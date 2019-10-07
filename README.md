# r2-rs
R2 interpreter implemented with Rust

对照 [怎样写一个解释器](http://www.yinwang.org/blog-cn/2012/08/01/interpreter) 实现的 scheme 语言的子集 R2 语言的解释器。

有三种执行方式（以下用 `r2` 代表编译生成的可执行文件，可以用 `cargo run --` 代替）：
- `r2 src.r2` 将文件 `src.r2` 中全部内容作为一个表达式执行计算
- `r2 -c "(+ 1 2)"` 使用 `-c` 参数，并在其后直接跟随想要计算的表达式
- `r2 -i` 开启 REPL

可运行 `cargo test` 进行测试。

本实现相对于原始版本增加了一个 `is_zero` 内置函数，当参数为 0 时结果是 `(lambda (x) (lambda (y) x))`，参数不为 0 时的结果是 `(lambda (x) (lambda (y) y))`，分别是无类型 lambda 演算中 true 和 false 的 Church 定义。

根目录下提供了两个示例源代码文件 `yin.r2` 和 `yc.r2`。其中 `yin.r2` 的计算结果为 6，说明 R2 语言采用的是词法作用域；`yc.r2` 中则利用 Y Combinator 定义出递归的阶乘函数并计算了 `100!` 的值。

## TODO

- [x] 使用更好的错误类型<del>，通过 error_chain 或者 failure<del/>
- [x] REPL 错误时提示而非 panic
- [x] 使用 nom 的 VerboseError 提供更好的错误提示
- [ ] REPL 中增加大环境和 ~~let~~ define 语法，使得前面行中定义的变量在后面可用 
- [x] 使用 `Rc` 链表作为 `Env` 类型
- [ ] 使用某 `BigInt` 作为整型