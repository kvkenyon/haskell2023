fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

testFib :: [Bool]
testFib =
  [ fib 0 == 0,
    fib 1 == 1,
    fib 2 == 1,
    fib 3 == 2,
    fib 9 == 34,
    fib 14 == 377
  ]