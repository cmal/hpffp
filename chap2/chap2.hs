sayHello :: String -> IO ()
let sayHello x = putStrLn ("Hello, " ++ x ++ "!")

let triple x = x * 3 -- in REPL should add `let` in the beginning
let triple 2

let half x = x / 2
let square x = x * x

(\f -> 3.14 * (f * f)) 5

let areaCircle x = 3.14 * (x * x)

let piArea x = pi * (x * x)


let area x = 3.14 * (x * x)
let double x = b * 2
x = 7
y = 10
f = x + y
