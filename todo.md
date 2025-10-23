## Done:
- [x] Make better error report
    - [x] Let's start with tokenizer.
    - [x] Now for parser
        - Loop based parser,like parsing
            - go for fixing expression
 

    - Add debug statement for rust
    - Added a type system,type container
## Doing:
    - Add struct based type in Literal,where I can create (x=10,y=10),
       
## Todo:
    - change assignment to other than like Identifier = Identifer convert to Expression=Expression,and I can do something like a.x=10;
    - Add the orginal typedefination,other than having ref,maybe not(This decision is pending)
    - Change the assignment to have comptime,let,None
        - It will check for keyword first before the assignment.
    - Change the ast to encompass it,to know what kind of assignment is this 
    - Add Option<TypeDefination>,int eh assignment ast,To do static type checking,
        - I need to transfer the ast as mut,in typechecker function,which will update the necessary types in there
## Ideas:
    Every objects are global,localization is in the name,the name is connected to a scope,or a path.
    All the type that get created,It will work like python,the typeId will be pushed to the hashmap,Now there are 2 problem,
        - How do I define type that hints to type that are not known in that time,
        - One example is to create a new type for that

