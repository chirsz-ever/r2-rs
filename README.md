# r2-rs
R2 interpreter implemented with Rust

对照 [怎样写一个解释器](http://www.yinwang.org/blog-cn/2012/08/01/interpreter) 实现的 scheme 语言的<del>子集</del>方言 R2 语言的解释器。

有三种执行方式（以下用 `r2c` 代表编译生成的可执行文件，可以用 `cargo run --` 代替）：
- `r2c src.r2` 将文件 `src.r2` 中全部内容作为一个表达式执行计算
- `r2c -c "(+ 1 2)"` 使用 `-c` 参数，并在其后直接跟随想要计算的表达式
- `r2c -i` 或直接执行 `r2c` 开启 REPL

可运行 `cargo test` 进行测试。

本实现相对于原始版本增加了一个 `is_zero` 内置函数，当参数为 0 时结果是 `(lambda (x) (lambda (y) x))`，参数不为 0 时的结果是 `(lambda (x) (lambda (y) y))`，分别是无类型 lambda 演算中 `true` 和 `false` 的 Church 定义；增加了 `define` 和 `begin` 语法。

可以使用 `(λ (var) expression)` 的形式定义 lambda 表达式；在 REPL 中按下 <kbd>Ctrl</kbd> + <kbd>\\</kbd> 组合键会插入一个 `λ` 字符。

examples 目录下提供了一些示例源代码文件：
- `yin.r2` 的计算结果为 6，说明 R2 语言采用的是词法作用域；
- `yc.r2` 中利用 Y Combinator 定义出递归的阶乘函数并计算了 `100!` 的值。
- `yc_new.r2` 意义与 `yc.r2` 相同，但使用 `define` 语法。

## TODO

- [x] 使用更好的错误类型<del>，通过 error_chain 或者 failure</del>（现在使用 anyhow）
- [x] REPL 错误时提示而非 panic
- [x] <del>使用 nom 的 VerboseError 提供更好的错误提示</del>（现在使用 pest 进行语法分析）
- [x] REPL 中增加大环境和 define 语法，使得前面行中定义的变量在后面可用
- [x] 使用 `Rc` 链表作为 `Env` 类型
- [x] 使用某 `BigInt` 作为整型
- [x] 使 `+`、`-`、`*`、`/` 行为与 scheme 中一致
- [x] 整合 `RetValue` 到 AST 中
- [ ] 限制 deine 语法的出现范围
- [ ] 增加原生 `boolean` 类型与 `if` 语法
- [ ] 增加 `number?`，`boolean?`，`zero?` 等函数
- [ ] 增加输入输出
- [ ] 使用 structopt
- [ ] 优化 lambda 所捕获变量
