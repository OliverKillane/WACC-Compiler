#![allow(non_snake_case)]
#![cfg(test)]

use super::analyse;
use crate::gather_modules;
use rstest::rstest;
use std::path::Path;

#[rstest]
#[case("valid/voidCalls/voidBasic.wacc")]
#[case("valid/voidCalls/voidCallInFunction.wacc")]
#[case("valid/scope/scopeRedefine.wacc")]
#[case("valid/scope/scopeBasic.wacc")]
#[case("valid/scope/ifNested1.wacc")]
#[case("valid/scope/scopeSimpleRedefine.wacc")]
#[case("valid/scope/scopeWhileNested.wacc")]
#[case("valid/scope/scopeIfRedefine.wacc")]
#[case("valid/scope/ifNested2.wacc")]
#[case("valid/scope/scopeWhileRedefine.wacc")]
#[case("valid/scope/intsAndKeywords.wacc")]
#[case("valid/scope/printAllTypes.wacc")]
#[case("valid/scope/scopeVars.wacc")]
#[case("valid/scope/indentationNotImportant.wacc")]
#[case("valid/scope/scope.wacc")]
#[case("valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc")]
#[case("valid/runtimeErr/integerOverflow/intWayOverflow.wacc")]
#[case("valid/runtimeErr/integerOverflow/intmultOverflow.wacc")]
#[case("valid/runtimeErr/integerOverflow/intJustOverflow.wacc")]
#[case("valid/runtimeErr/integerOverflow/intnegateOverflow.wacc")]
#[case("valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc")]
#[case("valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc")]
#[case("valid/runtimeErr/integerOverflow/intUnderflow.wacc")]
#[case("valid/runtimeErr/divideByZero/divZero.wacc")]
#[case("valid/runtimeErr/divideByZero/divideByZero.wacc")]
#[case("valid/runtimeErr/divideByZero/modByZero.wacc")]
#[case("valid/runtimeErr/nullDereference/readNull2.wacc")]
#[case("valid/runtimeErr/nullDereference/useNull1.wacc")]
#[case("valid/runtimeErr/nullDereference/readNull1.wacc")]
#[case("valid/runtimeErr/nullDereference/useNull2.wacc")]
#[case("valid/runtimeErr/nullDereference/setNull2.wacc")]
#[case("valid/runtimeErr/nullDereference/freeNull.wacc")]
#[case("valid/runtimeErr/nullDereference/setNull1.wacc")]
#[case("valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc")]
#[case("valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc")]
#[case("valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc")]
#[case("valid/function/simple_functions/incFunction.wacc")]
#[case("valid/function/simple_functions/functionUpdateParameter.wacc")]
#[case("valid/function/simple_functions/functionSimpleLoop.wacc")]
#[case("valid/function/simple_functions/functionDoubleReturn.wacc")]
#[case("valid/function/simple_functions/sameArgName2.wacc")]
#[case("valid/function/simple_functions/functionMultiReturns.wacc")]
#[case("valid/function/simple_functions/sameNameAsVar.wacc")]
#[case("valid/function/simple_functions/functionIfReturns.wacc")]
#[case("valid/function/simple_functions/sameArgName.wacc")]
#[case("valid/function/simple_functions/functionDeclaration.wacc")]
#[case("valid/function/simple_functions/functionManyArguments.wacc")]
#[case("valid/function/simple_functions/negFunction.wacc")]
#[case("valid/function/simple_functions/asciiTable.wacc")]
#[case("valid/function/simple_functions/functionReturnPair.wacc")]
#[case("valid/function/simple_functions/functionSimple.wacc")]
#[case("valid/function/nested_functions/fibonacciFullRec.wacc")]
#[case("valid/function/nested_functions/printInputTriangle.wacc")]
#[case("valid/function/nested_functions/functionConditionalReturn.wacc")]
#[case("valid/function/nested_functions/printTriangle.wacc")]
#[case("valid/function/nested_functions/mutualRecursion.wacc")]
#[case("valid/function/nested_functions/fixedPointRealArithmetic.wacc")]
#[case("valid/function/nested_functions/simpleRecursion.wacc")]
#[case("valid/function/nested_functions/fibonacciRecursive.wacc")]
#[case("valid/variables/_VarNames.wacc")]
#[case("valid/variables/puncCharDeclaration.wacc")]
#[case("valid/variables/longVarNames.wacc")]
#[case("valid/variables/boolDeclaration.wacc")]
#[case("valid/variables/charDeclaration2.wacc")]
#[case("valid/variables/zeroIntDeclaration.wacc")]
#[case("valid/variables/emptyStringDeclaration.wacc")]
#[case("valid/variables/negIntDeclaration.wacc")]
#[case("valid/variables/intDeclaration.wacc")]
#[case("valid/variables/manyVariables.wacc")]
#[case("valid/variables/capCharDeclaration.wacc")]
#[case("valid/variables/charDeclaration.wacc")]
#[case("valid/variables/boolDeclaration2.wacc")]
#[case("valid/variables/stringDeclaration.wacc")]
#[case("valid/array/arrayLookup.wacc")]
#[case("valid/array/arrayBasic.wacc")]
#[case("valid/array/arrayEmpty.wacc")]
#[case("valid/array/arrayLength.wacc")]
#[case("valid/array/arrayNested.wacc")]
#[case("valid/array/modifyString.wacc")]
#[case("valid/array/arrayPrint.wacc")]
#[case("valid/array/arraySimple.wacc")]
#[case("valid/array/printRef.wacc")]
#[case("valid/array/array.wacc")]
#[case("valid/IO/IOLoop.wacc")]
#[case("valid/IO/IOSequence.wacc")]
#[case("valid/IO/read/echoNegInt.wacc")]
#[case("valid/IO/read/echoBigInt.wacc")]
#[case("valid/IO/read/read.wacc")]
#[case("valid/IO/read/echoChar.wacc")]
#[case("valid/IO/read/echoInt.wacc")]
#[case("valid/IO/read/echoPuncChar.wacc")]
#[case("valid/IO/read/echoBigNegInt.wacc")]
#[case("valid/IO/print/printBool.wacc")]
#[case("valid/IO/print/print.wacc")]
#[case("valid/IO/print/println.wacc")]
#[case("valid/IO/print/printEscChar.wacc")]
#[case("valid/IO/print/printChar.wacc")]
#[case("valid/IO/print/multipleStringsAssignment.wacc")]
#[case("valid/IO/print/printCharArray.wacc")]
#[case("valid/IO/print/printCharAsString.wacc")]
#[case("valid/IO/print/print-backspace.wacc")]
#[case("valid/IO/print/print-carridge-return.wacc")]
#[case("valid/IO/print/hashInProgram.wacc")]
#[case("valid/IO/print/printInt.wacc")]
#[case("valid/while/fibonacciFullIt.wacc")]
#[case("valid/while/min.wacc")]
#[case("valid/while/max.wacc")]
#[case("valid/while/whileCount.wacc")]
#[case("valid/while/loopCharCondition.wacc")]
#[case("valid/while/loopIntCondition.wacc")]
#[case("valid/while/whileBoolFlip.wacc")]
#[case("valid/while/fibonacciIterative.wacc")]
#[case("valid/while/whileBasic.wacc")]
#[case("valid/while/rmStyleAdd.wacc")]
#[case("valid/while/whileFalse.wacc")]
#[case("valid/while/rmStyleAddIO.wacc")]
#[case("valid/expressions/sequentialCount.wacc")]
#[case("valid/expressions/andExpr.wacc")]
#[case("valid/expressions/equalsExpr.wacc")]
#[case("valid/expressions/charComparisonExpr.wacc")]
#[case("valid/expressions/intExpr1.wacc")]
#[case("valid/expressions/andOverOrExpr.wacc")]
#[case("valid/expressions/minusPlusExpr.wacc")]
#[case("valid/expressions/negBothDiv.wacc")]
#[case("valid/expressions/plusMinusExpr.wacc")]
#[case("valid/expressions/multNoWhitespaceExpr.wacc")]
#[case("valid/expressions/equalsOverBool.wacc")]
#[case("valid/expressions/equalsOverOr.wacc")]
#[case("valid/expressions/negExpr.wacc")]
#[case("valid/expressions/notequalsExpr.wacc")]
#[case("valid/expressions/longExpr.wacc")]
#[case("valid/expressions/negDivisorDiv.wacc")]
#[case("valid/expressions/lessCharExpr.wacc")]
#[case("valid/expressions/multExpr.wacc")]
#[case("valid/expressions/minusExpr.wacc")]
#[case("valid/expressions/longSplitExpr.wacc")]
#[case("valid/expressions/minusNoWhitespaceExpr.wacc")]
#[case("valid/expressions/longExpr3.wacc")]
#[case("valid/expressions/negBothMod.wacc")]
#[case("valid/expressions/plusNoWhitespaceExpr.wacc")]
#[case("valid/expressions/stringEqualsExpr.wacc")]
#[case("valid/expressions/modExpr.wacc")]
#[case("valid/expressions/ordAndchrExpr.wacc")]
#[case("valid/expressions/negDividendDiv.wacc")]
#[case("valid/expressions/notExpr.wacc")]
#[case("valid/expressions/boolExpr1.wacc")]
#[case("valid/expressions/lessEqExpr.wacc")]
#[case("valid/expressions/intCalc.wacc")]
#[case("valid/expressions/plusPlusExpr.wacc")]
#[case("valid/expressions/longExpr2.wacc")]
#[case("valid/expressions/equalsOverAnd.wacc")]
#[case("valid/expressions/orExpr.wacc")]
#[case("valid/expressions/negDividendMod.wacc")]
#[case("valid/expressions/plusExpr.wacc")]
#[case("valid/expressions/greaterEqExpr.wacc")]
#[case("valid/expressions/negDivisorMod.wacc")]
#[case("valid/expressions/longSplitExpr2.wacc")]
#[case("valid/expressions/minusMinusExpr.wacc")]
#[case("valid/expressions/greaterExpr.wacc")]
#[case("valid/expressions/lessExpr.wacc")]
#[case("valid/expressions/divExpr.wacc")]
#[case("valid/expressions/boolCalc.wacc")]
#[case("valid/sequence/basicSeq.wacc")]
#[case("valid/sequence/intAssignment.wacc")]
#[case("valid/sequence/stringAssignment.wacc")]
#[case("valid/sequence/basicSeq2.wacc")]
#[case("valid/sequence/boolAssignment.wacc")]
#[case("valid/sequence/charAssignment.wacc")]
#[case("valid/sequence/exitSimple.wacc")]
#[case("valid/sequence/intLeadingZeros.wacc")]
#[case("valid/if/if2.wacc")]
#[case("valid/if/if5.wacc")]
#[case("valid/if/whitespace.wacc")]
#[case("valid/if/if4.wacc")]
#[case("valid/if/if6.wacc")]
#[case("valid/if/ifFalse.wacc")]
#[case("valid/if/if1.wacc")]
#[case("valid/if/if3.wacc")]
#[case("valid/if/ifTrue.wacc")]
#[case("valid/if/ifBasic.wacc")]
#[case("valid/advanced/binarySortTree.wacc")]
#[case("valid/advanced/hashTable.wacc")]
#[case("valid/advanced/ticTacToe.wacc")]
#[case("valid/pairs/null.wacc")]
#[case("valid/pairs/createPair.wacc")]
#[case("valid/pairs/printPairOfNulls.wacc")]
#[case("valid/pairs/createRefPair.wacc")]
#[case("valid/pairs/readPair.wacc")]
#[case("valid/pairs/free.wacc")]
#[case("valid/pairs/printPair.wacc")]
#[case("valid/pairs/printNullPair.wacc")]
#[case("valid/pairs/checkRefPair.wacc")]
#[case("valid/pairs/writeSnd.wacc")]
#[case("valid/pairs/createPair02.wacc")]
#[case("valid/pairs/createPair03.wacc")]
#[case("valid/pairs/writeFst.wacc")]
#[case("valid/pairs/printNull.wacc")]
#[case("valid/pairs/nestedPair.wacc")]
#[case("valid/pairs/linkedList.wacc")]
#[case("valid/basic/exit/exitBasic2.wacc")]
#[case("valid/basic/exit/exitBasic.wacc")]
#[case("valid/basic/exit/exit-1.wacc")]
#[case("valid/basic/exit/exitWrap.wacc")]
#[case("valid/basic/skip/skip.wacc")]
#[case("valid/basic/skip/comment.wacc")]
#[case("valid/basic/skip/commentInLine.wacc")]
#[case("valid/modules/main_files/ticTacToe.wacc")]
#[case("valid/modules/main_files/hashTable.wacc")]
#[case("valid/modules/main_files/binarySortTree.wacc")]
fn semantic_anal_pass(#[case] file_name: &str) {
    let mut absolute_prefix = Path::new(file!()).to_owned();
    absolute_prefix.pop();
    let (main_input, modules) =
        gather_modules(&absolute_prefix.join(Path::new(file_name))).unwrap();
    let res = analyse(&main_input, modules.iter().collect()).map_err(|mut e| {
        e.add_input_file(
            main_input.contents(),
            main_input.filepath.display().to_string(),
        );
        for module in &modules {
            e.add_input_file(module.contents(), module.filepath.display().to_string());
        }
        e
    });
    if let Err(e) = &res {
        println!("{}", e);
    }
    assert!(res.is_ok());
}

#[rstest]
#[case("invalid/syntaxErr/function/funcExpr2.wacc")]
#[case("invalid/syntaxErr/function/functionMissingCall.wacc")]
#[case("invalid/syntaxErr/function/functionEndingNotReturn.wacc")]
#[case("invalid/syntaxErr/function/functionMissingParam.wacc")]
#[case("invalid/syntaxErr/function/functionReturnInLoop.wacc")]
#[case("invalid/syntaxErr/function/badlyPlaced.wacc")]
#[case("invalid/syntaxErr/function/badlyNamed.wacc")]
#[case("invalid/syntaxErr/function/funcExpr.wacc")]
#[case("invalid/syntaxErr/function/functionMissingType.wacc")]
#[case("invalid/syntaxErr/function/functionMissingPType.wacc")]
#[case("invalid/syntaxErr/function/thisIsNotC.wacc")]
#[case("invalid/syntaxErr/function/functionScopeDef.wacc")]
#[case("invalid/syntaxErr/function/functionLateDefine.wacc")]
#[case("invalid/syntaxErr/function/noBodyAfterFuncs.wacc")]
#[case("invalid/syntaxErr/function/functionNoReturn.wacc")]
#[case("invalid/syntaxErr/function/mutualRecursionNoReturn.wacc")]
#[case("invalid/syntaxErr/function/functionConditionalNoReturn.wacc")]
#[case("invalid/syntaxErr/variables/badintAssignments.wacc")]
#[case("invalid/syntaxErr/variables/bigIntAssignment.wacc")]
#[case("invalid/syntaxErr/variables/varNoName.wacc")]
#[case("invalid/syntaxErr/variables/badintAssignments2.wacc")]
#[case("invalid/syntaxErr/variables/badintAssignments1.wacc")]
#[case("invalid/syntaxErr/array/arrayExpr.wacc")]
#[case("invalid/syntaxErr/print/printlnCharArry.wacc")]
#[case("invalid/syntaxErr/while/donoErr.wacc")]
#[case("invalid/syntaxErr/while/dooErr.wacc")]
#[case("invalid/syntaxErr/while/whileNodo.wacc")]
#[case("invalid/syntaxErr/while/whileNodone.wacc")]
#[case("invalid/syntaxErr/while/whilErr.wacc")]
#[case("invalid/syntaxErr/expressions/missingOperand2.wacc")]
#[case("invalid/syntaxErr/expressions/printlnConcat.wacc")]
#[case("invalid/syntaxErr/expressions/missingOperand1.wacc")]
#[case("invalid/syntaxErr/sequence/emptySeq.wacc")]
#[case("invalid/syntaxErr/sequence/endSeq.wacc")]
#[case("invalid/syntaxErr/sequence/extraSeq.wacc")]
#[case("invalid/syntaxErr/sequence/doubleSeq.wacc")]
#[case("invalid/syntaxErr/sequence/missingSeq.wacc")]
#[case("invalid/syntaxErr/if/ifiErr.wacc")]
#[case("invalid/syntaxErr/if/ifNothen.wacc")]
#[case("invalid/syntaxErr/if/ifNoelse.wacc")]
#[case("invalid/syntaxErr/if/ifNofi.wacc")]
#[case("invalid/syntaxErr/pairs/badLookup02.wacc")]
#[case("invalid/syntaxErr/pairs/badLookup01.wacc")]
#[case("invalid/syntaxErr/basic/unescapedChar.wacc")]
#[case("invalid/syntaxErr/basic/badEscape.wacc")]
#[case("invalid/syntaxErr/basic/badComment2.wacc")]
#[case("invalid/syntaxErr/basic/multipleBegins.wacc")]
#[case("invalid/syntaxErr/basic/badComment.wacc")]
#[case("invalid/syntaxErr/basic/beginNoend.wacc")]
#[case("invalid/syntaxErr/basic/bgnErr.wacc")]
#[case("invalid/syntaxErr/basic/noBody.wacc")]
#[case("invalid/syntaxErr/basic/skpErr.wacc")]
#[case("invalid/syntaxErr/modules/no_semicolon.wacc")]
#[case("invalid/semanticErr/multiple/funcMess.wacc")]
#[case("invalid/semanticErr/multiple/ifAndWhileErrs.wacc")]
#[case("invalid/semanticErr/multiple/multiTypeErrs.wacc")]
#[case("invalid/semanticErr/multiple/multiCaseSensitivity.wacc")]
#[case("invalid/semanticErr/multiple/messyExpr.wacc")]
#[case("invalid/semanticErr/scope/badScopeRedefine.wacc")]
#[case("invalid/semanticErr/read/readTypeErr01.wacc")]
#[case("invalid/semanticErr/function/functionOverArgs.wacc")]
#[case("invalid/semanticErr/function/functionBadParam.wacc")]
#[case("invalid/semanticErr/function/functionBadReturn.wacc")]
#[case("invalid/semanticErr/function/funcVarAccess.wacc")]
#[case("invalid/semanticErr/function/functionSwapArgs.wacc")]
#[case("invalid/semanticErr/function/functionBadCall.wacc")]
#[case("invalid/semanticErr/function/functionUnderArgs.wacc")]
#[case("invalid/semanticErr/function/functionBadArgUse.wacc")]
#[case("invalid/semanticErr/function/functionAssign.wacc")]
#[case("invalid/semanticErr/function/functionRedefine.wacc")]
#[case("invalid/semanticErr/variables/undeclaredVar.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr08.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr01.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr10.wacc")]
#[case("invalid/semanticErr/variables/undeclaredVarAccess.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr07.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr02.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr06.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr03.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr04.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr05.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr12.wacc")]
#[case("invalid/semanticErr/variables/doubleDeclare.wacc")]
#[case("invalid/semanticErr/variables/caseMatters.wacc")]
#[case("invalid/semanticErr/variables/undeclaredScopeVar.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr11.wacc")]
#[case("invalid/semanticErr/variables/basicTypeErr09.wacc")]
#[case("invalid/semanticErr/print/printTypeErr01.wacc")]
#[case("invalid/semanticErr/exit/badCharExit.wacc")]
#[case("invalid/semanticErr/exit/globalReturn.wacc")]
#[case("invalid/semanticErr/exit/exitNonInt.wacc")]
#[case("invalid/semanticErr/IO/readTypeErr.wacc")]
#[case("invalid/semanticErr/while/falsErr.wacc")]
#[case("invalid/semanticErr/while/truErr.wacc")]
#[case("invalid/semanticErr/while/whileIntCondition.wacc")]
#[case("invalid/semanticErr/expressions/exprTypeErr.wacc")]
#[case("invalid/semanticErr/expressions/boolOpTypeErr.wacc")]
#[case("invalid/semanticErr/expressions/intOpTypeErr.wacc")]
#[case("invalid/semanticErr/expressions/moreArrExpr.wacc")]
#[case("invalid/semanticErr/expressions/lessPairExpr.wacc")]
#[case("invalid/semanticErr/expressions/stringElemErr.wacc")]
#[case("invalid/semanticErr/expressions/mixedOpTypeErr.wacc")]
#[case("invalid/semanticErr/if/ifIntCondition.wacc")]
#[case("invalid/semanticErr/pairs/sndNull.wacc")]
#[case("invalid/semanticErr/pairs/fstNull.wacc")]
#[case("invalid/semanticErr/pairs/freeNonPair.wacc")]
fn semantic_anal_fail(#[case] file_name: &str) {
    let mut absolute_prefix = Path::new(file!()).to_owned();
    absolute_prefix.pop();
    let (main_input, modules) =
        gather_modules(&absolute_prefix.join(Path::new(file_name))).unwrap();
    let res = analyse(&main_input, modules.iter().collect()).map_err(|mut e| {
        e.add_input_file(
            main_input.contents(),
            main_input.filepath.display().to_string(),
        );
        for module in &modules {
            e.add_input_file(module.contents(), module.filepath.display().to_string());
        }
        e
    });
    if let Ok(p) = &res {
        println!("{:#?}", p);
    }
    assert!(res.is_err());
}

#[rstest]
#[case("invalid/syntaxErr/modules/empty_path.wacc")]
#[case("invalid/syntaxErr/modules/illegal_path.wacc")]
fn module_gather_fail(#[case] file_name: &str) {
    let mut absolute_prefix = Path::new(file!()).to_owned();
    absolute_prefix.pop();
    assert!(gather_modules(&absolute_prefix.join(Path::new(file_name))).is_err());
}
