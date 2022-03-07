#![allow(non_snake_case)]
#![cfg(test)]

use super::analyse;

#[test]
fn scopeRedefine() {
    let input = include_str!("valid/scope/scopeRedefine.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn scopeBasic() {
    let input = include_str!("valid/scope/scopeBasic.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn ifNested1() {
    let input = include_str!("valid/scope/ifNested1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn scopeSimpleRedefine() {
    let input = include_str!("valid/scope/scopeSimpleRedefine.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn scopeWhileNested() {
    let input = include_str!("valid/scope/scopeWhileNested.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn scopeIfRedefine() {
    let input = include_str!("valid/scope/scopeIfRedefine.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn ifNested2() {
    let input = include_str!("valid/scope/ifNested2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn scopeWhileRedefine() {
    let input = include_str!("valid/scope/scopeWhileRedefine.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intsAndKeywords() {
    let input = include_str!("valid/scope/intsAndKeywords.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printAllTypes() {
    let input = include_str!("valid/scope/printAllTypes.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn scopeVars() {
    let input = include_str!("valid/scope/scopeVars.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn indentationNotImportant() {
    let input = include_str!("valid/scope/indentationNotImportant.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn scope() {
    let input = include_str!("valid/scope/scope.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow2() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intWayOverflow() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intWayOverflow.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intmultOverflow() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intmultOverflow.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intJustOverflow() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intJustOverflow.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intnegateOverflow.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow4() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intnegateOverflow3() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intUnderflow() {
    let input = include_str!("valid/runtimeErr/integerOverflow/intUnderflow.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn divZero() {
    let input = include_str!("valid/runtimeErr/divideByZero/divZero.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn divideByZero() {
    let input = include_str!("valid/runtimeErr/divideByZero/divideByZero.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn modByZero() {
    let input = include_str!("valid/runtimeErr/divideByZero/modByZero.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn readNull2() {
    let input = include_str!("valid/runtimeErr/nullDereference/readNull2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn useNull1() {
    let input = include_str!("valid/runtimeErr/nullDereference/useNull1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn readNull1() {
    let input = include_str!("valid/runtimeErr/nullDereference/readNull1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn useNull2() {
    let input = include_str!("valid/runtimeErr/nullDereference/useNull2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn setNull2() {
    let input = include_str!("valid/runtimeErr/nullDereference/setNull2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn freeNull() {
    let input = include_str!("valid/runtimeErr/nullDereference/freeNull.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn setNull1() {
    let input = include_str!("valid/runtimeErr/nullDereference/setNull1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayOutOfBounds() {
    let input = include_str!("valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayOutOfBoundsWrite() {
    let input = include_str!("valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayNegBounds() {
    let input = include_str!("valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn incFunction() {
    let input = include_str!("valid/function/simple_functions/incFunction.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionUpdateParameter() {
    let input = include_str!("valid/function/simple_functions/functionUpdateParameter.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionSimpleLoop() {
    let input = include_str!("valid/function/simple_functions/functionSimpleLoop.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionDoubleReturn() {
    let input = include_str!("valid/function/simple_functions/functionDoubleReturn.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn sameArgName2() {
    let input = include_str!("valid/function/simple_functions/sameArgName2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionMultiReturns() {
    let input = include_str!("valid/function/simple_functions/functionMultiReturns.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn sameNameAsVar() {
    let input = include_str!("valid/function/simple_functions/sameNameAsVar.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionIfReturns() {
    let input = include_str!("valid/function/simple_functions/functionIfReturns.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn sameArgName() {
    let input = include_str!("valid/function/simple_functions/sameArgName.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionDeclaration() {
    let input = include_str!("valid/function/simple_functions/functionDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionManyArguments() {
    let input = include_str!("valid/function/simple_functions/functionManyArguments.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negFunction() {
    let input = include_str!("valid/function/simple_functions/negFunction.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn asciiTable() {
    let input = include_str!("valid/function/simple_functions/asciiTable.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionReturnPair() {
    let input = include_str!("valid/function/simple_functions/functionReturnPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionSimple() {
    let input = include_str!("valid/function/simple_functions/functionSimple.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn fibonacciFullRec() {
    let input = include_str!("valid/function/nested_functions/fibonacciFullRec.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printInputTriangle() {
    let input = include_str!("valid/function/nested_functions/printInputTriangle.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn functionConditionalReturn() {
    let input = include_str!("valid/function/nested_functions/functionConditionalReturn.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printTriangle() {
    let input = include_str!("valid/function/nested_functions/printTriangle.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn mutualRecursion() {
    let input = include_str!("valid/function/nested_functions/mutualRecursion.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn fixedPointRealArithmetic() {
    let input = include_str!("valid/function/nested_functions/fixedPointRealArithmetic.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn simpleRecursion() {
    let input = include_str!("valid/function/nested_functions/simpleRecursion.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn fibonacciRecursive() {
    let input = include_str!("valid/function/nested_functions/fibonacciRecursive.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn _VarNames() {
    let input = include_str!("valid/variables/_VarNames.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn puncCharDeclaration() {
    let input = include_str!("valid/variables/puncCharDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn longVarNames() {
    let input = include_str!("valid/variables/longVarNames.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn boolDeclaration() {
    let input = include_str!("valid/variables/boolDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn charDeclaration2() {
    let input = include_str!("valid/variables/charDeclaration2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn zeroIntDeclaration() {
    let input = include_str!("valid/variables/zeroIntDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn emptyStringDeclaration() {
    let input = include_str!("valid/variables/emptyStringDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negIntDeclaration() {
    let input = include_str!("valid/variables/negIntDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intDeclaration() {
    let input = include_str!("valid/variables/intDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn manyVariables() {
    let input = include_str!("valid/variables/manyVariables.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn capCharDeclaration() {
    let input = include_str!("valid/variables/capCharDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn charDeclaration() {
    let input = include_str!("valid/variables/charDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn boolDeclaration2() {
    let input = include_str!("valid/variables/boolDeclaration2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn stringDeclaration() {
    let input = include_str!("valid/variables/stringDeclaration.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayLookup() {
    let input = include_str!("valid/array/arrayLookup.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayBasic() {
    let input = include_str!("valid/array/arrayBasic.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayEmpty() {
    let input = include_str!("valid/array/arrayEmpty.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayLength() {
    let input = include_str!("valid/array/arrayLength.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayNested() {
    let input = include_str!("valid/array/arrayNested.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn modifyString() {
    let input = include_str!("valid/array/modifyString.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arrayPrint() {
    let input = include_str!("valid/array/arrayPrint.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn arraySimple() {
    let input = include_str!("valid/array/arraySimple.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printRef() {
    let input = include_str!("valid/array/printRef.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn array() {
    let input = include_str!("valid/array/array.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn IOLoop() {
    let input = include_str!("valid/IO/IOLoop.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn IOSequence() {
    let input = include_str!("valid/IO/IOSequence.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn echoNegInt() {
    let input = include_str!("valid/IO/read/echoNegInt.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn echoBigInt() {
    let input = include_str!("valid/IO/read/echoBigInt.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn read() {
    let input = include_str!("valid/IO/read/read.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn echoChar() {
    let input = include_str!("valid/IO/read/echoChar.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn echoInt() {
    let input = include_str!("valid/IO/read/echoInt.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn echoPuncChar() {
    let input = include_str!("valid/IO/read/echoPuncChar.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn echoBigNegInt() {
    let input = include_str!("valid/IO/read/echoBigNegInt.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printBool() {
    let input = include_str!("valid/IO/print/printBool.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn print() {
    let input = include_str!("valid/IO/print/print.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn println() {
    let input = include_str!("valid/IO/print/println.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printEscChar() {
    let input = include_str!("valid/IO/print/printEscChar.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printChar() {
    let input = include_str!("valid/IO/print/printChar.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn multipleStringsAssignment() {
    let input = include_str!("valid/IO/print/multipleStringsAssignment.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printCharArray() {
    let input = include_str!("valid/IO/print/printCharArray.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printCharAsString() {
    let input = include_str!("valid/IO/print/printCharAsString.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn print_backspace() {
    let input = include_str!("valid/IO/print/print-backspace.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn print_carridge_return() {
    let input = include_str!("valid/IO/print/print-carridge-return.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn hashInProgram() {
    let input = include_str!("valid/IO/print/hashInProgram.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printInt() {
    let input = include_str!("valid/IO/print/printInt.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn fibonacciFullIt() {
    let input = include_str!("valid/while/fibonacciFullIt.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn min() {
    let input = include_str!("valid/while/min.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn max() {
    let input = include_str!("valid/while/max.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn whileCount() {
    let input = include_str!("valid/while/whileCount.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn loopCharCondition() {
    let input = include_str!("valid/while/loopCharCondition.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn loopIntCondition() {
    let input = include_str!("valid/while/loopIntCondition.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn whileBoolFlip() {
    let input = include_str!("valid/while/whileBoolFlip.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn fibonacciIterative() {
    let input = include_str!("valid/while/fibonacciIterative.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn whileBasic() {
    let input = include_str!("valid/while/whileBasic.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn rmStyleAdd() {
    let input = include_str!("valid/while/rmStyleAdd.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn whileFalse() {
    let input = include_str!("valid/while/whileFalse.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn rmStyleAddIO() {
    let input = include_str!("valid/while/rmStyleAddIO.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn sequentialCount() {
    let input = include_str!("valid/expressions/sequentialCount.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn andExpr() {
    let input = include_str!("valid/expressions/andExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn equalsExpr() {
    let input = include_str!("valid/expressions/equalsExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn charComparisonExpr() {
    let input = include_str!("valid/expressions/charComparisonExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intExpr1() {
    let input = include_str!("valid/expressions/intExpr1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn andOverOrExpr() {
    let input = include_str!("valid/expressions/andOverOrExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn minusPlusExpr() {
    let input = include_str!("valid/expressions/minusPlusExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negBothDiv() {
    let input = include_str!("valid/expressions/negBothDiv.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn plusMinusExpr() {
    let input = include_str!("valid/expressions/plusMinusExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn multNoWhitespaceExpr() {
    let input = include_str!("valid/expressions/multNoWhitespaceExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn equalsOverBool() {
    let input = include_str!("valid/expressions/equalsOverBool.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn equalsOverOr() {
    let input = include_str!("valid/expressions/equalsOverOr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negExpr() {
    let input = include_str!("valid/expressions/negExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn notequalsExpr() {
    let input = include_str!("valid/expressions/notequalsExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn longExpr() {
    let input = include_str!("valid/expressions/longExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negDivisorDiv() {
    let input = include_str!("valid/expressions/negDivisorDiv.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn lessCharExpr() {
    let input = include_str!("valid/expressions/lessCharExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn multExpr() {
    let input = include_str!("valid/expressions/multExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn minusExpr() {
    let input = include_str!("valid/expressions/minusExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn longSplitExpr() {
    let input = include_str!("valid/expressions/longSplitExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn minusNoWhitespaceExpr() {
    let input = include_str!("valid/expressions/minusNoWhitespaceExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn longExpr3() {
    let input = include_str!("valid/expressions/longExpr3.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negBothMod() {
    let input = include_str!("valid/expressions/negBothMod.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn plusNoWhitespaceExpr() {
    let input = include_str!("valid/expressions/plusNoWhitespaceExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn stringEqualsExpr() {
    let input = include_str!("valid/expressions/stringEqualsExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn modExpr() {
    let input = include_str!("valid/expressions/modExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn ordAndchrExpr() {
    let input = include_str!("valid/expressions/ordAndchrExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negDividendDiv() {
    let input = include_str!("valid/expressions/negDividendDiv.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn notExpr() {
    let input = include_str!("valid/expressions/notExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn boolExpr1() {
    let input = include_str!("valid/expressions/boolExpr1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn lessEqExpr() {
    let input = include_str!("valid/expressions/lessEqExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intCalc() {
    let input = include_str!("valid/expressions/intCalc.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn plusPlusExpr() {
    let input = include_str!("valid/expressions/plusPlusExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn longExpr2() {
    let input = include_str!("valid/expressions/longExpr2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn equalsOverAnd() {
    let input = include_str!("valid/expressions/equalsOverAnd.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn orExpr() {
    let input = include_str!("valid/expressions/orExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negDividendMod() {
    let input = include_str!("valid/expressions/negDividendMod.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn plusExpr() {
    let input = include_str!("valid/expressions/plusExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn greaterEqExpr() {
    let input = include_str!("valid/expressions/greaterEqExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn negDivisorMod() {
    let input = include_str!("valid/expressions/negDivisorMod.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn longSplitExpr2() {
    let input = include_str!("valid/expressions/longSplitExpr2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn minusMinusExpr() {
    let input = include_str!("valid/expressions/minusMinusExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn greaterExpr() {
    let input = include_str!("valid/expressions/greaterExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn lessExpr() {
    let input = include_str!("valid/expressions/lessExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn divExpr() {
    let input = include_str!("valid/expressions/divExpr.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn boolCalc() {
    let input = include_str!("valid/expressions/boolCalc.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn basicSeq() {
    let input = include_str!("valid/sequence/basicSeq.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intAssignment() {
    let input = include_str!("valid/sequence/intAssignment.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn stringAssignment() {
    let input = include_str!("valid/sequence/stringAssignment.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn basicSeq2() {
    let input = include_str!("valid/sequence/basicSeq2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn boolAssignment() {
    let input = include_str!("valid/sequence/boolAssignment.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn charAssignment() {
    let input = include_str!("valid/sequence/charAssignment.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn exitSimple() {
    let input = include_str!("valid/sequence/exitSimple.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn intLeadingZeros() {
    let input = include_str!("valid/sequence/intLeadingZeros.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn if2() {
    let input = include_str!("valid/if/if2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn if5() {
    let input = include_str!("valid/if/if5.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn whitespace() {
    let input = include_str!("valid/if/whitespace.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn if4() {
    let input = include_str!("valid/if/if4.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn if6() {
    let input = include_str!("valid/if/if6.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn ifFalse() {
    let input = include_str!("valid/if/ifFalse.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn if1() {
    let input = include_str!("valid/if/if1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn if3() {
    let input = include_str!("valid/if/if3.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn ifTrue() {
    let input = include_str!("valid/if/ifTrue.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn ifBasic() {
    let input = include_str!("valid/if/ifBasic.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn binarySortTree() {
    let input = include_str!("valid/advanced/binarySortTree.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn hashTable() {
    let input = include_str!("valid/advanced/hashTable.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn ticTacToe() {
    let input = include_str!("valid/advanced/ticTacToe.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn null() {
    let input = include_str!("valid/pairs/null.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn createPair() {
    let input = include_str!("valid/pairs/createPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printPairOfNulls() {
    let input = include_str!("valid/pairs/printPairOfNulls.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn createRefPair() {
    let input = include_str!("valid/pairs/createRefPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn readPair() {
    let input = include_str!("valid/pairs/readPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn free() {
    let input = include_str!("valid/pairs/free.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printPair() {
    let input = include_str!("valid/pairs/printPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printNullPair() {
    let input = include_str!("valid/pairs/printNullPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn checkRefPair() {
    let input = include_str!("valid/pairs/checkRefPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn writeSnd() {
    let input = include_str!("valid/pairs/writeSnd.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn createPair02() {
    let input = include_str!("valid/pairs/createPair02.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn createPair03() {
    let input = include_str!("valid/pairs/createPair03.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn writeFst() {
    let input = include_str!("valid/pairs/writeFst.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn printNull() {
    let input = include_str!("valid/pairs/printNull.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn nestedPair() {
    let input = include_str!("valid/pairs/nestedPair.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn linkedList() {
    let input = include_str!("valid/pairs/linkedList.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn exitBasic2() {
    let input = include_str!("valid/basic/exit/exitBasic2.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn exitBasic() {
    let input = include_str!("valid/basic/exit/exitBasic.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn exit_1() {
    let input = include_str!("valid/basic/exit/exit-1.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn exitWrap() {
    let input = include_str!("valid/basic/exit/exitWrap.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn skip() {
    let input = include_str!("valid/basic/skip/skip.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn comment() {
    let input = include_str!("valid/basic/skip/comment.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn commentInLine() {
    let input = include_str!("valid/basic/skip/commentInLine.wacc");
    let res = analyse(input);
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[test]
fn funcExpr2() {
    let input = include_str!("invalid/syntaxErr/function/funcExpr2.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/funcExpr2.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionMissingCall() {
    let input = include_str!("invalid/syntaxErr/function/functionMissingCall.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionMissingCall.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionEndingNotReturn() {
    let input = include_str!("invalid/syntaxErr/function/functionEndingNotReturn.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionEndingNotReturn.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionMissingParam() {
    let input = include_str!("invalid/syntaxErr/function/functionMissingParam.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionMissingParam.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionReturnInLoop() {
    let input = include_str!("invalid/syntaxErr/function/functionReturnInLoop.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionReturnInLoop.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badlyPlaced() {
    let input = include_str!("invalid/syntaxErr/function/badlyPlaced.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/badlyPlaced.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badlyNamed() {
    let input = include_str!("invalid/syntaxErr/function/badlyNamed.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/badlyNamed.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn funcExpr() {
    let input = include_str!("invalid/syntaxErr/function/funcExpr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/funcExpr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionMissingType() {
    let input = include_str!("invalid/syntaxErr/function/functionMissingType.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionMissingType.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionMissingPType() {
    let input = include_str!("invalid/syntaxErr/function/functionMissingPType.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionMissingPType.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn thisIsNotC() {
    let input = include_str!("invalid/syntaxErr/function/thisIsNotC.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/thisIsNotC.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionScopeDef() {
    let input = include_str!("invalid/syntaxErr/function/functionScopeDef.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionScopeDef.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionLateDefine() {
    let input = include_str!("invalid/syntaxErr/function/functionLateDefine.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionLateDefine.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn noBodyAfterFuncs() {
    let input = include_str!("invalid/syntaxErr/function/noBodyAfterFuncs.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/noBodyAfterFuncs.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionNoReturn() {
    let input = include_str!("invalid/syntaxErr/function/functionNoReturn.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionNoReturn.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn mutualRecursionNoReturn() {
    let input = include_str!("invalid/syntaxErr/function/mutualRecursionNoReturn.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/mutualRecursionNoReturn.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionConditionalNoReturn() {
    let input = include_str!("invalid/syntaxErr/function/functionConditionalNoReturn.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/function/functionConditionalNoReturn.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badintAssignments() {
    let input = include_str!("invalid/syntaxErr/variables/badintAssignments.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/variables/badintAssignments.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn bigIntAssignment() {
    let input = include_str!("invalid/syntaxErr/variables/bigIntAssignment.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/variables/bigIntAssignment.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn varNoName() {
    let input = include_str!("invalid/syntaxErr/variables/varNoName.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/variables/varNoName.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badintAssignments2() {
    let input = include_str!("invalid/syntaxErr/variables/badintAssignments2.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/variables/badintAssignments2.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badintAssignments1() {
    let input = include_str!("invalid/syntaxErr/variables/badintAssignments1.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/variables/badintAssignments1.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn arrayExpr() {
    let input = include_str!("invalid/syntaxErr/array/arrayExpr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/array/arrayExpr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn printlnCharArry() {
    let input = include_str!("invalid/syntaxErr/print/printlnCharArry.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/print/printlnCharArry.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn donoErr() {
    let input = include_str!("invalid/syntaxErr/while/donoErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/while/donoErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn dooErr() {
    let input = include_str!("invalid/syntaxErr/while/dooErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/while/dooErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn whileNodo() {
    let input = include_str!("invalid/syntaxErr/while/whileNodo.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/while/whileNodo.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn whileNodone() {
    let input = include_str!("invalid/syntaxErr/while/whileNodone.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/while/whileNodone.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn whilErr() {
    let input = include_str!("invalid/syntaxErr/while/whilErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/while/whilErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn missingOperand2() {
    let input = include_str!("invalid/syntaxErr/expressions/missingOperand2.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/expressions/missingOperand2.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn printlnConcat() {
    let input = include_str!("invalid/syntaxErr/expressions/printlnConcat.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/expressions/printlnConcat.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn missingOperand1() {
    let input = include_str!("invalid/syntaxErr/expressions/missingOperand1.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/expressions/missingOperand1.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn emptySeq() {
    let input = include_str!("invalid/syntaxErr/sequence/emptySeq.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/sequence/emptySeq.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn endSeq() {
    let input = include_str!("invalid/syntaxErr/sequence/endSeq.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/sequence/endSeq.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn extraSeq() {
    let input = include_str!("invalid/syntaxErr/sequence/extraSeq.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/sequence/extraSeq.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn doubleSeq() {
    let input = include_str!("invalid/syntaxErr/sequence/doubleSeq.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/sequence/doubleSeq.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn missingSeq() {
    let input = include_str!("invalid/syntaxErr/sequence/missingSeq.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/sequence/missingSeq.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn ifiErr() {
    let input = include_str!("invalid/syntaxErr/if/ifiErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/if/ifiErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn ifNothen() {
    let input = include_str!("invalid/syntaxErr/if/ifNothen.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/if/ifNothen.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn ifNoelse() {
    let input = include_str!("invalid/syntaxErr/if/ifNoelse.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/if/ifNoelse.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn ifNofi() {
    let input = include_str!("invalid/syntaxErr/if/ifNofi.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/if/ifNofi.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badLookup02() {
    let input = include_str!("invalid/syntaxErr/pairs/badLookup02.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/pairs/badLookup02.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badLookup01() {
    let input = include_str!("invalid/syntaxErr/pairs/badLookup01.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/pairs/badLookup01.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn unescapedChar() {
    let input = include_str!("invalid/syntaxErr/basic/unescapedChar.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/unescapedChar.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badEscape() {
    let input = include_str!("invalid/syntaxErr/basic/badEscape.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/badEscape.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badComment2() {
    let input = include_str!("invalid/syntaxErr/basic/badComment2.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/badComment2.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn multipleBegins() {
    let input = include_str!("invalid/syntaxErr/basic/multipleBegins.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/multipleBegins.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badComment() {
    let input = include_str!("invalid/syntaxErr/basic/badComment.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/badComment.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn beginNoend() {
    let input = include_str!("invalid/syntaxErr/basic/beginNoend.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/beginNoend.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn bgnErr() {
    let input = include_str!("invalid/syntaxErr/basic/bgnErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/bgnErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn noBody() {
    let input = include_str!("invalid/syntaxErr/basic/noBody.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/noBody.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn skpErr() {
    let input = include_str!("invalid/syntaxErr/basic/skpErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/syntaxErr/basic/skpErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn funcMess() {
    let input = include_str!("invalid/semanticErr/multiple/funcMess.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/multiple/funcMess.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn ifAndWhileErrs() {
    let input = include_str!("invalid/semanticErr/multiple/ifAndWhileErrs.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/multiple/ifAndWhileErrs.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn multiTypeErrs() {
    let input = include_str!("invalid/semanticErr/multiple/multiTypeErrs.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/multiple/multiTypeErrs.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn multiCaseSensitivity() {
    let input = include_str!("invalid/semanticErr/multiple/multiCaseSensitivity.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/multiple/multiCaseSensitivity.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn messyExpr() {
    let input = include_str!("invalid/semanticErr/multiple/messyExpr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/multiple/messyExpr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badScopeRedefine() {
    let input = include_str!("invalid/semanticErr/scope/badScopeRedefine.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/scope/badScopeRedefine.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn readTypeErr01() {
    let input = include_str!("invalid/semanticErr/read/readTypeErr01.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/read/readTypeErr01.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionOverArgs() {
    let input = include_str!("invalid/semanticErr/function/functionOverArgs.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionOverArgs.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionBadParam() {
    let input = include_str!("invalid/semanticErr/function/functionBadParam.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionBadParam.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionBadReturn() {
    let input = include_str!("invalid/semanticErr/function/functionBadReturn.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionBadReturn.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn funcVarAccess() {
    let input = include_str!("invalid/semanticErr/function/funcVarAccess.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/funcVarAccess.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionSwapArgs() {
    let input = include_str!("invalid/semanticErr/function/functionSwapArgs.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionSwapArgs.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionBadCall() {
    let input = include_str!("invalid/semanticErr/function/functionBadCall.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionBadCall.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionUnderArgs() {
    let input = include_str!("invalid/semanticErr/function/functionUnderArgs.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionUnderArgs.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionBadArgUse() {
    let input = include_str!("invalid/semanticErr/function/functionBadArgUse.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionBadArgUse.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionAssign() {
    let input = include_str!("invalid/semanticErr/function/functionAssign.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionAssign.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn functionRedefine() {
    let input = include_str!("invalid/semanticErr/function/functionRedefine.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/function/functionRedefine.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn undeclaredVar() {
    let input = include_str!("invalid/semanticErr/variables/undeclaredVar.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/undeclaredVar.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr08() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr08.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr08.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr01() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr01.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr01.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr10() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr10.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr10.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn undeclaredVarAccess() {
    let input = include_str!("invalid/semanticErr/variables/undeclaredVarAccess.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/undeclaredVarAccess.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr07() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr07.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr07.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr02() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr02.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr02.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr06() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr06.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr06.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr03() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr03.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr03.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr04() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr04.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr04.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr05() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr05.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr05.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr12() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr12.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr12.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn doubleDeclare() {
    let input = include_str!("invalid/semanticErr/variables/doubleDeclare.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/doubleDeclare.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn caseMatters() {
    let input = include_str!("invalid/semanticErr/variables/caseMatters.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/caseMatters.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn undeclaredScopeVar() {
    let input = include_str!("invalid/semanticErr/variables/undeclaredScopeVar.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/undeclaredScopeVar.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr11() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr11.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr11.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn basicTypeErr09() {
    let input = include_str!("invalid/semanticErr/variables/basicTypeErr09.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/variables/basicTypeErr09.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn printTypeErr01() {
    let input = include_str!("invalid/semanticErr/print/printTypeErr01.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/print/printTypeErr01.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn badCharExit() {
    let input = include_str!("invalid/semanticErr/exit/badCharExit.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/exit/badCharExit.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn globalReturn() {
    let input = include_str!("invalid/semanticErr/exit/globalReturn.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/exit/globalReturn.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn exitNonInt() {
    let input = include_str!("invalid/semanticErr/exit/exitNonInt.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/exit/exitNonInt.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn readTypeErr() {
    let input = include_str!("invalid/semanticErr/IO/readTypeErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/IO/readTypeErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn falsErr() {
    let input = include_str!("invalid/semanticErr/while/falsErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/while/falsErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn truErr() {
    let input = include_str!("invalid/semanticErr/while/truErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/while/truErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn whileIntCondition() {
    let input = include_str!("invalid/semanticErr/while/whileIntCondition.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/while/whileIntCondition.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn exprTypeErr() {
    let input = include_str!("invalid/semanticErr/expressions/exprTypeErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/expressions/exprTypeErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn boolOpTypeErr() {
    let input = include_str!("invalid/semanticErr/expressions/boolOpTypeErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/expressions/boolOpTypeErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn intOpTypeErr() {
    let input = include_str!("invalid/semanticErr/expressions/intOpTypeErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/expressions/intOpTypeErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn moreArrExpr() {
    let input = include_str!("invalid/semanticErr/expressions/moreArrExpr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/expressions/moreArrExpr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn lessPairExpr() {
    let input = include_str!("invalid/semanticErr/expressions/lessPairExpr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/expressions/lessPairExpr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn stringElemErr() {
    let input = include_str!("invalid/semanticErr/expressions/stringElemErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/expressions/stringElemErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn mixedOpTypeErr() {
    let input = include_str!("invalid/semanticErr/expressions/mixedOpTypeErr.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/expressions/mixedOpTypeErr.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn ifIntCondition() {
    let input = include_str!("invalid/semanticErr/if/ifIntCondition.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/if/ifIntCondition.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn sndNull() {
    let input = include_str!("invalid/semanticErr/pairs/sndNull.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/pairs/sndNull.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn fstNull() {
    let input = include_str!("invalid/semanticErr/pairs/fstNull.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/pairs/fstNull.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}

#[test]
fn freeNonPair() {
    let input = include_str!("invalid/semanticErr/pairs/freeNonPair.wacc");
    let res = analyse(input).map_err(|mut e| {e.add_input_file(input, "invalid/semanticErr/pairs/freeNonPair.wacc".to_string()); e});
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_err());
}
