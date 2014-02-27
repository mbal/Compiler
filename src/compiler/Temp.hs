
putLine l =
  do
    s <- get
    put $ s { code = l : (code s) }
compile (Sum e1 e2) = do
  compile e1
  putLine "movl %eax, -4(%rbp)"
  compile e2
  putLine "addl -4(%rbp), %eax"
  return 0

compile (Sub e1 e2) = do
  compile e1
  putLine "movl %eax, -4(%rbp)"
  compile e2
  putLine "subl -4(%rbp), %eax"
  putLine "negl %eax"
  return 0

compile (Mul e1 e2) = do
  putLine "add $-4, %rbp"
  compile e1
  putLine "movl %eax, -4(%rbp)"
  compile e2
  putLine "imull -4(%rbp), %eax"
  putLine "movl %eax, -4(%rbp)"
  putLine "add $4, %rbp"
  return 0

compile (Div e1 e2) = do
  putLine "add $-4, %rbp"
  compile e2
  putLine "movl %eax, -4(%rbp)"
  compile e1
  putLine "idiv -4(%rbp), %eax"
  putLine "movl %eax, -4(%rbp)"
  putLine "add $4, %rbp"
  return 0
  
compile (Num a) = do
  putLine $ "movl $" ++ (show a) ++ ", %eax"
  return 0
