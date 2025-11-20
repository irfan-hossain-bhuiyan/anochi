Ok my programming language is structured in a way where new code can be evaluated after the previous code is used,
Like 
```
let gen=fn {x=int} {
    let T1:type=Sometype;
    let T2:type=Sometype2;
    if x>0 T1 else T2
}
```
So in checking it is verified by expanding the expression that Sometype and Sometype2 are types.
So the expression are executed,than if x is int,does it allow greater than operation.
Also execution and type checking of the function,I don't need that to be one thing,It can be any type I want,So let's change the implementation on that.
Let's separate the Vm from ast,It don't need to contain ast,Ast is executed by it.
So I guess I will stick with the interp
When I am defining any function,they can only be typechecked on the high level,
can't be evaluated as the variable are unknown
but like,
but some variable are known,So they are gonna be 
