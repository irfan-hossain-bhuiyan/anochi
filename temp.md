1. Make a error data type,for now expression error,that contains the Position and String for now,which contains the error report.
2. All the ast.rs,token.rs,vm.rs,parser.rs,make each of them it's own folder.

Ok,you don't need TypeContainer,what I want is because type are expression as well,the convert to UnifiedTypeDefnition is TryInto,as it will fail,It will only convert if the expression resemble type,so ValuePrimitive will fail,ProductType will success if it can be converted to type so {x=i64,y=164} can be converted to type,also 
