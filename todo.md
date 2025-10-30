## Done:
- [x] Make better error report
    - [x] Let's start with tokenizer.
    - [x] Now for parser
        - Loop based parser,like parsing
            - go for fixing expression
 

    - Add debug statement for rust
    - Added a type system,type container
## Doing:

- [x] Add struct based type in VmValue(also name it object value),where I can create {x=10,y=10},
    - [x] Changing Literal type to VmType 
    - [x] Adding Ast for struct type
    - [ ] Adding parser for the ast that is a expression
    - [ ] So when evaluating the ast,it will convert it to the vm

## Todo:
    - change assignment to other than like Identifier = Identifer convert to Expression=Expression,and I can do something like a.x=10;
    - Add the orginal typedefination,other than having ref,maybe not(This decision is pending)
    - Change the assignment to have comptime,let,None
        - It will check for keyword first before the assignment.
    - Change the ast to encompass it,to know what kind of assignment is this 
    - Add Option<TypeDefination>,int eh assignment ast,To do static type checking,
        - I need to transfer the ast as mut,in typechecker function,which will update the necessary types in there

## QnA:
> Why use third bracket for statement,not first bracket?
Add struct based type in Literal,where I can create {x=10,y=10},
        Now I am in a confused do I use first bracket or seacnd bracket,If I am using
        first bracket it can be confused with grouping
        If I am using third bracket It won't be hard to parse,As there is no expression that use third bracket,
            A question than arise,like what about if I wanna add x=if(x) y else z,Let's not think about that,It also need to if to
            take expression as input,Which I not doing,Let's think about it later.
        So that's why I am using third bracket

## Ideas:
    Types are values as well,they are object in vmvalue
    So I can add them as expresssion,and generate them from struct,
    In expression I need to add set as expression as well,So they can be 
    so all the variable that takes I can give it 
    Every objects are global,localization is in the name,the name is connected to a scope,or a path.
    All the type that get created,It will work like python,the typeId will be pushed to the hashmap,Now there are 2 problem,
        - How do I define type that hints to type that are not known in that time,
        - One example is to create a new type for that

