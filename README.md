Simple tree defined as below should suffice for binary family tree.

```haskell
data Tree a = Nil | Node a (Tree a) (Tree a)
```

Now we need a way to store and read text and transform it into Abstract Syntax Tree(AST). This can be done by reading sequence of items and turning them into complete binary tree using indexes `2*n+1` for left and `2*(n+1)` for right, and vice-versa for storing the AST in linear fashion.


