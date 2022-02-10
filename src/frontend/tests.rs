#![allow(non_snake_case)]
#![cfg(test)]

use super::*;
#[test]
fn scopeRedefine() {
    let input = include_str!("parser/valid/scope/scopeRedefine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn scopeBasic() {
    let input = include_str!("parser/valid/scope/scopeBasic.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn ifNested1() {
    let input = include_str!("parser/valid/scope/ifNested1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn scopeSimpleRedefine() {
    let input = include_str!("parser/valid/scope/scopeSimpleRedefine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn scopeWhileNested() {
    let input = include_str!("parser/valid/scope/scopeWhileNested.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn scopeIfRedefine() {
    let input = include_str!("parser/valid/scope/scopeIfRedefine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn ifNested2() {
    let input = include_str!("parser/valid/scope/ifNested2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn scopeWhileRedefine() {
    let input = include_str!("parser/valid/scope/scopeWhileRedefine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intsAndKeywords() {
    let input = include_str!("parser/valid/scope/intsAndKeywords.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printAllTypes() {
    let input = include_str!("parser/valid/scope/printAllTypes.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn scopeVars() {
    let input = include_str!("parser/valid/scope/scopeVars.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn indentationNotImportant() {
    let input = include_str!("parser/valid/scope/indentationNotImportant.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn scope() {
    let input = include_str!("parser/valid/scope/scope.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow2() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intWayOverflow() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intWayOverflow.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intmultOverflow() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intmultOverflow.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intJustOverflow() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intJustOverflow.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intnegateOverflow.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow4() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow3() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intUnderflow() {
    let input = include_str!("parser/valid/runtimeErr/integerOverflow/intUnderflow.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn divZero() {
    let input = include_str!("parser/valid/runtimeErr/divideByZero/divZero.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn divideByZero() {
    let input = include_str!("parser/valid/runtimeErr/divideByZero/divideByZero.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn modByZero() {
    let input = include_str!("parser/valid/runtimeErr/divideByZero/modByZero.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn readNull2() {
    let input = include_str!("parser/valid/runtimeErr/nullDereference/readNull2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn useNull1() {
    let input = include_str!("parser/valid/runtimeErr/nullDereference/useNull1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn readNull1() {
    let input = include_str!("parser/valid/runtimeErr/nullDereference/readNull1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn useNull2() {
    let input = include_str!("parser/valid/runtimeErr/nullDereference/useNull2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn setNull2() {
    let input = include_str!("parser/valid/runtimeErr/nullDereference/setNull2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn freeNull() {
    let input = include_str!("parser/valid/runtimeErr/nullDereference/freeNull.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn setNull1() {
    let input = include_str!("parser/valid/runtimeErr/nullDereference/setNull1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayOutOfBounds() {
    let input = include_str!("parser/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayOutOfBoundsWrite() {
    let input = include_str!("parser/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayNegBounds() {
    let input = include_str!("parser/valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn incFunction() {
    let input = include_str!("parser/valid/function/simple_functions/incFunction.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionUpdateParameter() {
    let input = include_str!("parser/valid/function/simple_functions/functionUpdateParameter.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionSimpleLoop() {
    let input = include_str!("parser/valid/function/simple_functions/functionSimpleLoop.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionDoubleReturn() {
    let input = include_str!("parser/valid/function/simple_functions/functionDoubleReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn sameArgName2() {
    let input = include_str!("parser/valid/function/simple_functions/sameArgName2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionMultiReturns() {
    let input = include_str!("parser/valid/function/simple_functions/functionMultiReturns.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn sameNameAsVar() {
    let input = include_str!("parser/valid/function/simple_functions/sameNameAsVar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionIfReturns() {
    let input = include_str!("parser/valid/function/simple_functions/functionIfReturns.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn sameArgName() {
    let input = include_str!("parser/valid/function/simple_functions/sameArgName.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionDeclaration() {
    let input = include_str!("parser/valid/function/simple_functions/functionDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionManyArguments() {
    let input = include_str!("parser/valid/function/simple_functions/functionManyArguments.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negFunction() {
    let input = include_str!("parser/valid/function/simple_functions/negFunction.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn asciiTable() {
    let input = include_str!("parser/valid/function/simple_functions/asciiTable.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionReturnPair() {
    let input = include_str!("parser/valid/function/simple_functions/functionReturnPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionSimple() {
    let input = include_str!("parser/valid/function/simple_functions/functionSimple.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn fibonacciFullRec() {
    let input = include_str!("parser/valid/function/nested_functions/fibonacciFullRec.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printInputTriangle() {
    let input = include_str!("parser/valid/function/nested_functions/printInputTriangle.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn functionConditionalReturn() {
    let input = include_str!("parser/valid/function/nested_functions/functionConditionalReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printTriangle() {
    let input = include_str!("parser/valid/function/nested_functions/printTriangle.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn mutualRecursion() {
    let input = include_str!("parser/valid/function/nested_functions/mutualRecursion.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn fixedPointRealArithmetic() {
    let input = include_str!("parser/valid/function/nested_functions/fixedPointRealArithmetic.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn simpleRecursion() {
    let input = include_str!("parser/valid/function/nested_functions/simpleRecursion.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn fibonacciRecursive() {
    let input = include_str!("parser/valid/function/nested_functions/fibonacciRecursive.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn _VarNames() {
    let input = include_str!("parser/valid/variables/_VarNames.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn puncCharDeclaration() {
    let input = include_str!("parser/valid/variables/puncCharDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn longVarNames() {
    let input = include_str!("parser/valid/variables/longVarNames.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn boolDeclaration() {
    let input = include_str!("parser/valid/variables/boolDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn charDeclaration2() {
    let input = include_str!("parser/valid/variables/charDeclaration2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn zeroIntDeclaration() {
    let input = include_str!("parser/valid/variables/zeroIntDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn emptyStringDeclaration() {
    let input = include_str!("parser/valid/variables/emptyStringDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negIntDeclaration() {
    let input = include_str!("parser/valid/variables/negIntDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intDeclaration() {
    let input = include_str!("parser/valid/variables/intDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn manyVariables() {
    let input = include_str!("parser/valid/variables/manyVariables.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn capCharDeclaration() {
    let input = include_str!("parser/valid/variables/capCharDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn charDeclaration() {
    let input = include_str!("parser/valid/variables/charDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn boolDeclaration2() {
    let input = include_str!("parser/valid/variables/boolDeclaration2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn stringDeclaration() {
    let input = include_str!("parser/valid/variables/stringDeclaration.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayLookup() {
    let input = include_str!("parser/valid/array/arrayLookup.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayBasic() {
    let input = include_str!("parser/valid/array/arrayBasic.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayEmpty() {
    let input = include_str!("parser/valid/array/arrayEmpty.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayLength() {
    let input = include_str!("parser/valid/array/arrayLength.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayNested() {
    let input = include_str!("parser/valid/array/arrayNested.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn modifyString() {
    let input = include_str!("parser/valid/array/modifyString.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arrayPrint() {
    let input = include_str!("parser/valid/array/arrayPrint.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn arraySimple() {
    let input = include_str!("parser/valid/array/arraySimple.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printRef() {
    let input = include_str!("parser/valid/array/printRef.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn array() {
    let input = include_str!("parser/valid/array/array.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn IOLoop() {
    let input = include_str!("parser/valid/IO/IOLoop.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn IOSequence() {
    let input = include_str!("parser/valid/IO/IOSequence.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn echoNegInt() {
    let input = include_str!("parser/valid/IO/read/echoNegInt.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn echoBigInt() {
    let input = include_str!("parser/valid/IO/read/echoBigInt.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn read() {
    let input = include_str!("parser/valid/IO/read/read.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn echoChar() {
    let input = include_str!("parser/valid/IO/read/echoChar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn echoInt() {
    let input = include_str!("parser/valid/IO/read/echoInt.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn echoPuncChar() {
    let input = include_str!("parser/valid/IO/read/echoPuncChar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn echoBigNegInt() {
    let input = include_str!("parser/valid/IO/read/echoBigNegInt.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printBool() {
    let input = include_str!("parser/valid/IO/print/printBool.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn print() {
    let input = include_str!("parser/valid/IO/print/print.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn println() {
    let input = include_str!("parser/valid/IO/print/println.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printEscChar() {
    let input = include_str!("parser/valid/IO/print/printEscChar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printChar() {
    let input = include_str!("parser/valid/IO/print/printChar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn multipleStringsAssignment() {
    let input = include_str!("parser/valid/IO/print/multipleStringsAssignment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printCharArray() {
    let input = include_str!("parser/valid/IO/print/printCharArray.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printCharAsString() {
    let input = include_str!("parser/valid/IO/print/printCharAsString.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn print_backspace() {
    let input = include_str!("parser/valid/IO/print/print-backspace.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn print_carridge_return() {
    let input = include_str!("parser/valid/IO/print/print-carridge-return.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn hashInProgram() {
    let input = include_str!("parser/valid/IO/print/hashInProgram.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printInt() {
    let input = include_str!("parser/valid/IO/print/printInt.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn fibonacciFullIt() {
    let input = include_str!("parser/valid/while/fibonacciFullIt.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn min() {
    let input = include_str!("parser/valid/while/min.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn max() {
    let input = include_str!("parser/valid/while/max.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn whileCount() {
    let input = include_str!("parser/valid/while/whileCount.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn loopCharCondition() {
    let input = include_str!("parser/valid/while/loopCharCondition.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn loopIntCondition() {
    let input = include_str!("parser/valid/while/loopIntCondition.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn whileBoolFlip() {
    let input = include_str!("parser/valid/while/whileBoolFlip.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn fibonacciIterative() {
    let input = include_str!("parser/valid/while/fibonacciIterative.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn whileBasic() {
    let input = include_str!("parser/valid/while/whileBasic.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn rmStyleAdd() {
    let input = include_str!("parser/valid/while/rmStyleAdd.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn whileFalse() {
    let input = include_str!("parser/valid/while/whileFalse.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn rmStyleAddIO() {
    let input = include_str!("parser/valid/while/rmStyleAddIO.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn sequentialCount() {
    let input = include_str!("parser/valid/expressions/sequentialCount.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn andExpr() {
    let input = include_str!("parser/valid/expressions/andExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn equalsExpr() {
    let input = include_str!("parser/valid/expressions/equalsExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn charComparisonExpr() {
    let input = include_str!("parser/valid/expressions/charComparisonExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intExpr1() {
    let input = include_str!("parser/valid/expressions/intExpr1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn andOverOrExpr() {
    let input = include_str!("parser/valid/expressions/andOverOrExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn minusPlusExpr() {
    let input = include_str!("parser/valid/expressions/minusPlusExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negBothDiv() {
    let input = include_str!("parser/valid/expressions/negBothDiv.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn plusMinusExpr() {
    let input = include_str!("parser/valid/expressions/plusMinusExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn multNoWhitespaceExpr() {
    let input = include_str!("parser/valid/expressions/multNoWhitespaceExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn equalsOverBool() {
    let input = include_str!("parser/valid/expressions/equalsOverBool.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn equalsOverOr() {
    let input = include_str!("parser/valid/expressions/equalsOverOr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negExpr() {
    let input = include_str!("parser/valid/expressions/negExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn notequalsExpr() {
    let input = include_str!("parser/valid/expressions/notequalsExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn longExpr() {
    let input = include_str!("parser/valid/expressions/longExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negDivisorDiv() {
    let input = include_str!("parser/valid/expressions/negDivisorDiv.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn lessCharExpr() {
    let input = include_str!("parser/valid/expressions/lessCharExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn multExpr() {
    let input = include_str!("parser/valid/expressions/multExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn minusExpr() {
    let input = include_str!("parser/valid/expressions/minusExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn longSplitExpr() {
    let input = include_str!("parser/valid/expressions/longSplitExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn minusNoWhitespaceExpr() {
    let input = include_str!("parser/valid/expressions/minusNoWhitespaceExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn longExpr3() {
    let input = include_str!("parser/valid/expressions/longExpr3.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negBothMod() {
    let input = include_str!("parser/valid/expressions/negBothMod.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn plusNoWhitespaceExpr() {
    let input = include_str!("parser/valid/expressions/plusNoWhitespaceExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn stringEqualsExpr() {
    let input = include_str!("parser/valid/expressions/stringEqualsExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn modExpr() {
    let input = include_str!("parser/valid/expressions/modExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn ordAndchrExpr() {
    let input = include_str!("parser/valid/expressions/ordAndchrExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negDividendDiv() {
    let input = include_str!("parser/valid/expressions/negDividendDiv.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn notExpr() {
    let input = include_str!("parser/valid/expressions/notExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn boolExpr1() {
    let input = include_str!("parser/valid/expressions/boolExpr1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn lessEqExpr() {
    let input = include_str!("parser/valid/expressions/lessEqExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intCalc() {
    let input = include_str!("parser/valid/expressions/intCalc.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn plusPlusExpr() {
    let input = include_str!("parser/valid/expressions/plusPlusExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn longExpr2() {
    let input = include_str!("parser/valid/expressions/longExpr2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn equalsOverAnd() {
    let input = include_str!("parser/valid/expressions/equalsOverAnd.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn orExpr() {
    let input = include_str!("parser/valid/expressions/orExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negDividendMod() {
    let input = include_str!("parser/valid/expressions/negDividendMod.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn plusExpr() {
    let input = include_str!("parser/valid/expressions/plusExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn greaterEqExpr() {
    let input = include_str!("parser/valid/expressions/greaterEqExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn negDivisorMod() {
    let input = include_str!("parser/valid/expressions/negDivisorMod.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn longSplitExpr2() {
    let input = include_str!("parser/valid/expressions/longSplitExpr2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn minusMinusExpr() {
    let input = include_str!("parser/valid/expressions/minusMinusExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn greaterExpr() {
    let input = include_str!("parser/valid/expressions/greaterExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn lessExpr() {
    let input = include_str!("parser/valid/expressions/lessExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn divExpr() {
    let input = include_str!("parser/valid/expressions/divExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn boolCalc() {
    let input = include_str!("parser/valid/expressions/boolCalc.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn basicSeq() {
    let input = include_str!("parser/valid/sequence/basicSeq.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intAssignment() {
    let input = include_str!("parser/valid/sequence/intAssignment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn stringAssignment() {
    let input = include_str!("parser/valid/sequence/stringAssignment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn basicSeq2() {
    let input = include_str!("parser/valid/sequence/basicSeq2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn boolAssignment() {
    let input = include_str!("parser/valid/sequence/boolAssignment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn charAssignment() {
    let input = include_str!("parser/valid/sequence/charAssignment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn exitSimple() {
    let input = include_str!("parser/valid/sequence/exitSimple.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn intLeadingZeros() {
    let input = include_str!("parser/valid/sequence/intLeadingZeros.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn if2() {
    let input = include_str!("parser/valid/if/if2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn if5() {
    let input = include_str!("parser/valid/if/if5.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn whitespace() {
    let input = include_str!("parser/valid/if/whitespace.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn if4() {
    let input = include_str!("parser/valid/if/if4.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn if6() {
    let input = include_str!("parser/valid/if/if6.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn ifFalse() {
    let input = include_str!("parser/valid/if/ifFalse.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn if1() {
    let input = include_str!("parser/valid/if/if1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn if3() {
    let input = include_str!("parser/valid/if/if3.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn ifTrue() {
    let input = include_str!("parser/valid/if/ifTrue.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn ifBasic() {
    let input = include_str!("parser/valid/if/ifBasic.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn binarySortTree() {
    let input = include_str!("parser/valid/advanced/binarySortTree.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn hashTable() {
    let input = include_str!("parser/valid/advanced/hashTable.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn ticTacToe() {
    let input = include_str!("parser/valid/advanced/ticTacToe.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn null() {
    let input = include_str!("parser/valid/pairs/null.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn createPair() {
    let input = include_str!("parser/valid/pairs/createPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printPairOfNulls() {
    let input = include_str!("parser/valid/pairs/printPairOfNulls.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn createRefPair() {
    let input = include_str!("parser/valid/pairs/createRefPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn readPair() {
    let input = include_str!("parser/valid/pairs/readPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn free() {
    let input = include_str!("parser/valid/pairs/free.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printPair() {
    let input = include_str!("parser/valid/pairs/printPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printNullPair() {
    let input = include_str!("parser/valid/pairs/printNullPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn checkRefPair() {
    let input = include_str!("parser/valid/pairs/checkRefPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn writeSnd() {
    let input = include_str!("parser/valid/pairs/writeSnd.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn createPair02() {
    let input = include_str!("parser/valid/pairs/createPair02.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn createPair03() {
    let input = include_str!("parser/valid/pairs/createPair03.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn writeFst() {
    let input = include_str!("parser/valid/pairs/writeFst.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn printNull() {
    let input = include_str!("parser/valid/pairs/printNull.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn nestedPair() {
    let input = include_str!("parser/valid/pairs/nestedPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn linkedList() {
    let input = include_str!("parser/valid/pairs/linkedList.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn exitBasic2() {
    let input = include_str!("parser/valid/basic/exit/exitBasic2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn exitBasic() {
    let input = include_str!("parser/valid/basic/exit/exitBasic.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn exit_1() {
    let input = include_str!("parser/valid/basic/exit/exit-1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn exitWrap() {
    let input = include_str!("parser/valid/basic/exit/exitWrap.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn skip() {
    let input = include_str!("parser/valid/basic/skip/skip.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn comment() {
    let input = include_str!("parser/valid/basic/skip/comment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
fn commentInLine() {
    let input = include_str!("parser/valid/basic/skip/commentInLine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn funcExpr2() {
    let input = include_str!("parser/invalid/syntaxErr/function/funcExpr2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionMissingCall() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionMissingCall.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionEndingNotReturn() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionEndingNotReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionMissingParam() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionMissingParam.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionReturnInLoop() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionReturnInLoop.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badlyPlaced() {
    let input = include_str!("parser/invalid/syntaxErr/function/badlyPlaced.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badlyNamed() {
    let input = include_str!("parser/invalid/syntaxErr/function/badlyNamed.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn funcExpr() {
    let input = include_str!("parser/invalid/syntaxErr/function/funcExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionMissingType() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionMissingType.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionMissingPType() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionMissingPType.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn thisIsNotC() {
    let input = include_str!("parser/invalid/syntaxErr/function/thisIsNotC.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionScopeDef() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionScopeDef.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionLateDefine() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionLateDefine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn noBodyAfterFuncs() {
    let input = include_str!("parser/invalid/syntaxErr/function/noBodyAfterFuncs.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionNoReturn() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionNoReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn mutualRecursionNoReturn() {
    let input = include_str!("parser/invalid/syntaxErr/function/mutualRecursionNoReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionConditionalNoReturn() {
    let input = include_str!("parser/invalid/syntaxErr/function/functionConditionalNoReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badintAssignments() {
    let input = include_str!("parser/invalid/syntaxErr/variables/badintAssignments.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn bigIntAssignment() {
    let input = include_str!("parser/invalid/syntaxErr/variables/bigIntAssignment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn varNoName() {
    let input = include_str!("parser/invalid/syntaxErr/variables/varNoName.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badintAssignments2() {
    let input = include_str!("parser/invalid/syntaxErr/variables/badintAssignments2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badintAssignments1() {
    let input = include_str!("parser/invalid/syntaxErr/variables/badintAssignments1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn arrayExpr() {
    let input = include_str!("parser/invalid/syntaxErr/array/arrayExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn printlnCharArry() {
    let input = include_str!("parser/invalid/syntaxErr/print/printlnCharArry.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn donoErr() {
    let input = include_str!("parser/invalid/syntaxErr/while/donoErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn dooErr() {
    let input = include_str!("parser/invalid/syntaxErr/while/dooErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn whileNodo() {
    let input = include_str!("parser/invalid/syntaxErr/while/whileNodo.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn whileNodone() {
    let input = include_str!("parser/invalid/syntaxErr/while/whileNodone.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn whilErr() {
    let input = include_str!("parser/invalid/syntaxErr/while/whilErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn missingOperand2() {
    let input = include_str!("parser/invalid/syntaxErr/expressions/missingOperand2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn printlnConcat() {
    let input = include_str!("parser/invalid/syntaxErr/expressions/printlnConcat.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn missingOperand1() {
    let input = include_str!("parser/invalid/syntaxErr/expressions/missingOperand1.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn emptySeq() {
    let input = include_str!("parser/invalid/syntaxErr/sequence/emptySeq.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn endSeq() {
    let input = include_str!("parser/invalid/syntaxErr/sequence/endSeq.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn extraSeq() {
    let input = include_str!("parser/invalid/syntaxErr/sequence/extraSeq.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn doubleSeq() {
    let input = include_str!("parser/invalid/syntaxErr/sequence/doubleSeq.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn missingSeq() {
    let input = include_str!("parser/invalid/syntaxErr/sequence/missingSeq.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn ifiErr() {
    let input = include_str!("parser/invalid/syntaxErr/if/ifiErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn ifNothen() {
    let input = include_str!("parser/invalid/syntaxErr/if/ifNothen.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn ifNoelse() {
    let input = include_str!("parser/invalid/syntaxErr/if/ifNoelse.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn ifNofi() {
    let input = include_str!("parser/invalid/syntaxErr/if/ifNofi.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badLookup02() {
    let input = include_str!("parser/invalid/syntaxErr/pairs/badLookup02.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badLookup01() {
    let input = include_str!("parser/invalid/syntaxErr/pairs/badLookup01.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn unescapedChar() {
    let input = include_str!("parser/invalid/syntaxErr/basic/unescapedChar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badEscape() {
    let input = include_str!("parser/invalid/syntaxErr/basic/badEscape.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badComment2() {
    let input = include_str!("parser/invalid/syntaxErr/basic/badComment2.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn multipleBegins() {
    let input = include_str!("parser/invalid/syntaxErr/basic/multipleBegins.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badComment() {
    let input = include_str!("parser/invalid/syntaxErr/basic/badComment.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn beginNoend() {
    let input = include_str!("parser/invalid/syntaxErr/basic/beginNoend.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn bgnErr() {
    let input = include_str!("parser/invalid/syntaxErr/basic/bgnErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn noBody() {
    let input = include_str!("parser/invalid/syntaxErr/basic/noBody.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn skpErr() {
    let input = include_str!("parser/invalid/syntaxErr/basic/skpErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn funcMess() {
    let input = include_str!("parser/invalid/semanticErr/multiple/funcMess.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn ifAndWhileErrs() {
    let input = include_str!("parser/invalid/semanticErr/multiple/ifAndWhileErrs.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn multiTypeErrs() {
    let input = include_str!("parser/invalid/semanticErr/multiple/multiTypeErrs.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn multiCaseSensitivity() {
    let input = include_str!("parser/invalid/semanticErr/multiple/multiCaseSensitivity.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn messyExpr() {
    let input = include_str!("parser/invalid/semanticErr/multiple/messyExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badScopeRedefine() {
    let input = include_str!("parser/invalid/semanticErr/scope/badScopeRedefine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn readTypeErr01() {
    let input = include_str!("parser/invalid/semanticErr/read/readTypeErr01.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionOverArgs() {
    let input = include_str!("parser/invalid/semanticErr/function/functionOverArgs.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionBadParam() {
    let input = include_str!("parser/invalid/semanticErr/function/functionBadParam.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionBadReturn() {
    let input = include_str!("parser/invalid/semanticErr/function/functionBadReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn funcVarAccess() {
    let input = include_str!("parser/invalid/semanticErr/function/funcVarAccess.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionSwapArgs() {
    let input = include_str!("parser/invalid/semanticErr/function/functionSwapArgs.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionBadCall() {
    let input = include_str!("parser/invalid/semanticErr/function/functionBadCall.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionUnderArgs() {
    let input = include_str!("parser/invalid/semanticErr/function/functionUnderArgs.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionBadArgUse() {
    let input = include_str!("parser/invalid/semanticErr/function/functionBadArgUse.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionAssign() {
    let input = include_str!("parser/invalid/semanticErr/function/functionAssign.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn functionRedefine() {
    let input = include_str!("parser/invalid/semanticErr/function/functionRedefine.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn undeclaredVar() {
    let input = include_str!("parser/invalid/semanticErr/variables/undeclaredVar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr08() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr08.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr01() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr01.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr10() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr10.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn undeclaredVarAccess() {
    let input = include_str!("parser/invalid/semanticErr/variables/undeclaredVarAccess.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr07() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr07.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr02() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr02.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr06() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr06.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr03() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr03.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr04() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr04.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr05() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr05.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr12() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr12.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn doubleDeclare() {
    let input = include_str!("parser/invalid/semanticErr/variables/doubleDeclare.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn caseMatters() {
    let input = include_str!("parser/invalid/semanticErr/variables/caseMatters.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn undeclaredScopeVar() {
    let input = include_str!("parser/invalid/semanticErr/variables/undeclaredScopeVar.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr11() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr11.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn basicTypeErr09() {
    let input = include_str!("parser/invalid/semanticErr/variables/basicTypeErr09.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn printTypeErr01() {
    let input = include_str!("parser/invalid/semanticErr/print/printTypeErr01.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn badCharExit() {
    let input = include_str!("parser/invalid/semanticErr/exit/badCharExit.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn globalReturn() {
    let input = include_str!("parser/invalid/semanticErr/exit/globalReturn.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn exitNonInt() {
    let input = include_str!("parser/invalid/semanticErr/exit/exitNonInt.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn readTypeErr() {
    let input = include_str!("parser/invalid/semanticErr/IO/readTypeErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn falsErr() {
    let input = include_str!("parser/invalid/semanticErr/while/falsErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn truErr() {
    let input = include_str!("parser/invalid/semanticErr/while/truErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn whileIntCondition() {
    let input = include_str!("parser/invalid/semanticErr/while/whileIntCondition.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn exprTypeErr() {
    let input = include_str!("parser/invalid/semanticErr/expressions/exprTypeErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn boolOpTypeErr() {
    let input = include_str!("parser/invalid/semanticErr/expressions/boolOpTypeErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn intOpTypeErr() {
    let input = include_str!("parser/invalid/semanticErr/expressions/intOpTypeErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn moreArrExpr() {
    let input = include_str!("parser/invalid/semanticErr/expressions/moreArrExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn lessPairExpr() {
    let input = include_str!("parser/invalid/semanticErr/expressions/lessPairExpr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn stringElemErr() {
    let input = include_str!("parser/invalid/semanticErr/expressions/stringElemErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn mixedOpTypeErr() {
    let input = include_str!("parser/invalid/semanticErr/expressions/mixedOpTypeErr.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn ifIntCondition() {
    let input = include_str!("parser/invalid/semanticErr/if/ifIntCondition.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn sndNull() {
    let input = include_str!("parser/invalid/semanticErr/pairs/sndNull.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn fstNull() {
    let input = include_str!("parser/invalid/semanticErr/pairs/fstNull.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

#[test]
#[should_panic]
fn freeNonPair() {
    let input = include_str!("parser/invalid/semanticErr/pairs/freeNonPair.wacc");
    let res = analyse(input);
    if let Err(v) = &res {
        for e in v {
            println!("{}", e)
        }
    };
    assert!(res.is_ok());
}

