open CPar
open Interp

let fromFile = Parse.fromFile
let run = Interp.run
let argv = System.Environment.GetCommandLineArgs()

let _ =
    printf "Micro-C interpreter v 1.1.0 of 2021-5-19\n"

let _ =
    let args = Array.filter ((<>) "-g") argv

    if args.Length > 1 then
        let source = args.[1]

        let inputargs =
            Array.splitAt 2 args
            |> snd
            |> (Array.map int)
            |> Array.toList

        printf "interpreting %s ...inputargs:%A\n" source inputargs

        // ex 是 paser 返回的 抽象语法树
//导入 CPar 模块和 fromFile 函数：使用 open CPar 和 let fromFile = Parse.fromFile 行导入 CPar 模块，并将 fromFile 函数绑定到 fromFile 变量上。

// 从文件中解析 Micro-C 代码：使用 fromFile source 将源文件路径传递给 fromFile 函数来解析 Micro-C 代码。这将返回一个表示代码的抽象语法树。

// 对抽象语法树进行求值（可选）：使用 run ex inputargs 调用 run 函数对抽象语法树进行求值。其中，ex 是从文件中解析出的抽象语法树，inputargs 是传递给程序的输入参数。

// 请注意，代码中的 source 变量应该包含要解析的 Micro-C 代码文件的路径，并且可能需要提供适当的输入参数（如果需要的话）。解析后的抽象语法树可以在进一步处理、分析或执行操作中使用。

// 确保已安装所需的依赖项，并将代码与适当的输入一起运行，以生成 Micro-C 代码的抽象语法树。
        try
            ignore (
                let ex = fromFile source
                run ex inputargs
            ) 
        with Failure msg -> printf "ERROR: %s\n" msg
    else
        printf "Usage: interpc.exe <source file> <args>\n"
        printf "example: interpc.exe ex1.c 8\n"
