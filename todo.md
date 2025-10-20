## Done:
- [x] Make better error report
    - [x] Let's start with tokenizer.
    - [x] Now for parser
        - Loop based parser,like parsing
            - go for fixing expression
 

    - Add debug statement for rust
## Doing:

       
## Todo:
    - Add generalized types,
        - Literal is used for datatype for now,Have a Value variable for now,TreeWalk will return it
        - A typedefination is a set of typeId (represented by set) for typed union or hashmap of Identifier(here element of struct) maps to typeId
        - It is what a type contains
        - typeContainer is what maps typeid,with all the information of types,like typedefination,also to create new type,you pass the typedefination as input and get typeid for it,also there is other struct that maps names(Identifier) with typeId,A typeid can have multiple name because of alias,everything in my language can have alias,also not map names(Identifier) with typeId,the name would be varId(applying unique identity to every var in the program),but as varId is not implemented,I will make a type alias varId=Identifier for now
        ,this is runtime initialized,
        - typeid is a usize,given the type id 
        - There will be a type container,where we create now type,Now the 
        typecontainer will be a combination of vector and hashmap,The vector contains everything about the type,
        and hasmap links name with the type,Now the name will be a path,which contains 


## Ideas:
    Every objects are global,localization is in the name,the name is connected to a scope,or a path.
    

