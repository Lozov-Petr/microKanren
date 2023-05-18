  $ ./numbers.exe
  Query: add x 1 3
  Var counter: 5
  Substitution: 
  _.0 <- Succ (_.1)
  _.1 <- Succ (_.3)
  _.2 <- Succ (Succ (Zero))
  _.3 <- Zero
  _.4 <- Succ (Zero)
  
  
  
  Query: add x y 3
  Var counter: 8
  Substitution: 
  _.0 <- Succ (_.2)
  _.1 <- Zero
  _.2 <- Succ (_.4)
  _.3 <- Succ (Succ (Zero))
  _.4 <- Succ (_.6)
  _.5 <- Succ (Zero)
  _.6 <- Zero
  _.7 <- Zero
  
  Var counter: 2
  Substitution: 
  _.0 <- Zero
  _.1 <- Succ (Succ (Succ (Zero)))
  
  Var counter: 4
  Substitution: 
  _.0 <- Succ (_.2)
  _.1 <- Succ (Succ (Zero))
  _.2 <- Zero
  _.3 <- Succ (Succ (Zero))
  
  Var counter: 6
  Substitution: 
  _.0 <- Succ (_.2)
  _.1 <- Succ (Zero)
  _.2 <- Succ (_.4)
  _.3 <- Succ (Succ (Zero))
  _.4 <- Zero
  _.5 <- Succ (Zero)
