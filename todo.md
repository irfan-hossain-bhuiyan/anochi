## Done:
- [x] Make better error report
    - [x] Let's start with tokenizer.
    - [x] Now for parser
        - Loop based parser,like parsing
            - go for fixing expression
 

    - Add debug statement for rust
    - Added a type system,type container

- [x] Add struct based type in VmValue(also name it object value),where I can create {x=10,y=10},
    - [x] Changing Literal type to VmType 
    - [x] Adding Ast for struct type
    - [x] Adding parser for the ast that is a expression
    - [x] So when evaluating the ast,it will convert it to the vm

- [x] change assignment to other than like Identifier = Identifer convert to Expression=Expression,
- [x] Changing the Identifier to actual Identifier,
- [x] Now change the vmvalue implementation in anochi,It use Literal as well,which need to be changed for bigint,bigfloat etc,String implmentation will be handled later,also automatically generate type for the vmvalue,type will be the builtin value type.
- [x] Moving StacKScope to other other module,Variable,changing Variable Entry to make everything more modular.
- [x] Add loop,continue,break;
        - In loop,I am thinking about loop being a wrapper around goto,Let's just ignore that for now.
        now continue and break this statement can be called inside loop.
- [x] Change the representation Expression and Statement to be generic
- [x] fixed loop
    - A way to fix this issue,Iis to add return type in the execute statement if break or continue is called,the loop will evaluate the decision and check if it is okay.to break or continue.

## Doing:
- [ ] Having type checking and code running side by side.
    - [ ] make the implementation of function to todo
    - [ ] adding type for both runtime and compile time
    - [ ] Separate the type to comptime and their is also runtime situation
        - The issue in my programming language is 
            fn {x=i32,}
in checking phase,their will be type checking,and the container will have which will validate if all the code written is right,and the error will be given with VmError,than in runtime error,that think will be managed later,as a compiled binary,as binary can be used without having to track the orginal file.
- [ ] create function and call them,dynamically for now.
    - [x] Add function type in VmValue,Let's make it id for now.
    - [ ] Have two types of data type,one is runtime,one is comptime,the comptime will be evaluated  
- [ ] add struct destructuring,a operation that destrucure the struct and push it to scope
- [ ] add struct parsing so now {a,b=10,c} are allowed.
    - this will help for struct like {self,a,b} in function input.
- [ ] changing the tree_vm to just check type and verify the types are valid.
    - [ ] having both comptime types and runtime types as well
- [ ] add function and run it.
    - [ ] 
- [ ] adding function for my language
    - Like in the language,VmValue will have funcId,It is more like a pointer for now.
    - As the statement in the function will not be hashed.

- [ ] Currently I have assignment ast that use expression on the right hand side,for say x.a=10; etc,but that is only used 
    for when the object is already created,I am returning the expression from that,not when I am creating a new variable.
- [ ] checks if the test are right
    - [x] checks token test
    - [x] checks parser test
    - [ ] vm_test
    - [x] typing test
        - [x] make it simplfy
        - [ ] make it 
- [ ] Changing the tokenization code simplify.
## Todo:
- [ ] dereference struct,create a function for it.that will extract all the variable from struct and push to stack.
- [ ] A operation that will extract it,Add it in the ast,but not decide what will it be,make it "**" for now.
- [ ] Add const. in variable state.
- [ ] support for function and for function verification.
- [ ] support for a.x operation,and I can do something like a.x=10;
- [ ] Change the assignment to have comptime,let,None
    - [ ] It will check for keyword first before the assignment.
- [ ] Change the ast to encompass it,to know what kind of assignment is this 

## QnA:
> Why use third bracket for statement,not first bracket?
Add struct based type in Literal,where I can create {x=10,y=10},
        Now I am in a confused do I use first bracket or seacnd bracket,If I am using
        first bracket it can be confused with grouping
        If I am using third bracket It won't be hard to parse,As there is no expression that use third bracket,
            A question than arise,like what about if I wanna add x=if(x) y else z,Let's not think about that,It also need to if to
            take expression as input,Which I not doing,Let's think about it later.
        So that's why I am using third bracket

> Why the duplication like TypeDefinition,OptimizedTypeDefinition,UnifiedTypeDefinition?

## Ideas:
    How I code is compiled,My code and type checking are all dynamic,
## Design Problem:
    Problem with type as expression and it's optimization,Now the 

- change the recursive tree like data structure to not,Let's not go with it,See how far I can go?
## Things I disabled

//#[cfg(test)]
//mod scope_stack_tests;

## Question:
In set theory,A set can't contain itself.
But in type theory we always have recursive types,

A other representaton of set is you can have a subset of welldefined set,or union of some set,not manually construct new set from a universal prespective
as type is the universal of all types you can't have {a=type,b=type}

## Bug fixes:
let x:type={x=i64,y=i64} is not working,fix it
