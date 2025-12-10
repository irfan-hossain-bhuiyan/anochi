1. Make a error data type,for now expression error,that contains the Position and String for now,which contains the error report.
2. All the ast.rs,token.rs,vm.rs,parser.rs,make each of them it's own folder.

Ok,you don't need TypeContainer,what I want is because type are expression as well,the convert to UnifiedTypeDefnition is TryInto,as it will fail,It will only convert if the expression resemble type,so ValuePrimitive will fail,ProductType will success if it can be converted to type so {x=i64,y=164} can be converted to type,also 

Ok thanks for the suggestion,I have got an approach,I can add destructure in the syntax,so let {*}=self,which will say I am taking everything in self,and spliting in variable,Now it's great,But I have still facing issue,I have to pass self.as a function paramter,even though I am just calling a function from there no sideeffect,Now a idea I can think of is 
