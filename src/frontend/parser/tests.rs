#![allow(non_snake_case)]

use super::super::modules::{gather_modules, InputFile};
use super::*;
use rstest::rstest;

#[rstest]
#[case("../tests/valid/scope/scopeRedefine.wacc")]
#[case("../tests/valid/scope/scopeBasic.wacc")]
#[case("../tests/valid/scope/ifNested1.wacc")]
#[case("../tests/valid/scope/scopeSimpleRedefine.wacc")]
#[case("../tests/valid/scope/scopeWhileNested.wacc")]
#[case("../tests/valid/scope/scopeIfRedefine.wacc")]
#[case("../tests/valid/scope/ifNested2.wacc")]
#[case("../tests/valid/scope/scopeWhileRedefine.wacc")]
#[case("../tests/valid/scope/intsAndKeywords.wacc")]
#[case("../tests/valid/scope/printAllTypes.wacc")]
#[case("../tests/valid/scope/scopeVars.wacc")]
#[case("../tests/valid/scope/indentationNotImportant.wacc")]
#[case("../tests/valid/scope/scope.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intWayOverflow.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intmultOverflow.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intJustOverflow.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intnegateOverflow.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc")]
#[case("../tests/valid/runtimeErr/integerOverflow/intUnderflow.wacc")]
#[case("../tests/valid/runtimeErr/divideByZero/divZero.wacc")]
#[case("../tests/valid/runtimeErr/divideByZero/divideByZero.wacc")]
#[case("../tests/valid/runtimeErr/divideByZero/modByZero.wacc")]
#[case("../tests/valid/runtimeErr/nullDereference/readNull2.wacc")]
#[case("../tests/valid/runtimeErr/nullDereference/useNull1.wacc")]
#[case("../tests/valid/runtimeErr/nullDereference/readNull1.wacc")]
#[case("../tests/valid/runtimeErr/nullDereference/useNull2.wacc")]
#[case("../tests/valid/runtimeErr/nullDereference/setNull2.wacc")]
#[case("../tests/valid/runtimeErr/nullDereference/freeNull.wacc")]
#[case("../tests/valid/runtimeErr/nullDereference/setNull1.wacc")]
#[case("../tests/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc")]
#[case("../tests/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc")]
#[case("../tests/valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc")]
#[case("../tests/valid/function/simple_functions/incFunction.wacc")]
#[case("../tests/valid/function/simple_functions/functionUpdateParameter.wacc")]
#[case("../tests/valid/function/simple_functions/functionSimpleLoop.wacc")]
#[case("../tests/valid/function/simple_functions/functionDoubleReturn.wacc")]
#[case("../tests/valid/function/simple_functions/sameArgName2.wacc")]
#[case("../tests/valid/function/simple_functions/functionMultiReturns.wacc")]
#[case("../tests/valid/function/simple_functions/sameNameAsVar.wacc")]
#[case("../tests/valid/function/simple_functions/functionIfReturns.wacc")]
#[case("../tests/valid/function/simple_functions/sameArgName.wacc")]
#[case("../tests/valid/function/simple_functions/functionDeclaration.wacc")]
#[case("../tests/valid/function/simple_functions/functionManyArguments.wacc")]
#[case("../tests/valid/function/simple_functions/negFunction.wacc")]
#[case("../tests/valid/function/simple_functions/asciiTable.wacc")]
#[case("../tests/valid/function/simple_functions/functionReturnPair.wacc")]
#[case("../tests/valid/function/simple_functions/functionSimple.wacc")]
#[case("../tests/valid/function/nested_functions/fibonacciFullRec.wacc")]
#[case("../tests/valid/function/nested_functions/printInputTriangle.wacc")]
#[case("../tests/valid/function/nested_functions/functionConditionalReturn.wacc")]
#[case("../tests/valid/function/nested_functions/printTriangle.wacc")]
#[case("../tests/valid/function/nested_functions/mutualRecursion.wacc")]
#[case("../tests/valid/function/nested_functions/fixedPointRealArithmetic.wacc")]
#[case("../tests/valid/function/nested_functions/simpleRecursion.wacc")]
#[case("../tests/valid/function/nested_functions/fibonacciRecursive.wacc")]
#[case("../tests/valid/variables/_VarNames.wacc")]
#[case("../tests/valid/variables/puncCharDeclaration.wacc")]
#[case("../tests/valid/variables/longVarNames.wacc")]
#[case("../tests/valid/variables/boolDeclaration.wacc")]
#[case("../tests/valid/variables/charDeclaration2.wacc")]
#[case("../tests/valid/variables/zeroIntDeclaration.wacc")]
#[case("../tests/valid/variables/emptyStringDeclaration.wacc")]
#[case("../tests/valid/variables/negIntDeclaration.wacc")]
#[case("../tests/valid/variables/intDeclaration.wacc")]
#[case("../tests/valid/variables/manyVariables.wacc")]
#[case("../tests/valid/variables/capCharDeclaration.wacc")]
#[case("../tests/valid/variables/charDeclaration.wacc")]
#[case("../tests/valid/variables/boolDeclaration2.wacc")]
#[case("../tests/valid/variables/stringDeclaration.wacc")]
#[case("../tests/valid/array/arrayLookup.wacc")]
#[case("../tests/valid/array/arrayBasic.wacc")]
#[case("../tests/valid/array/arrayEmpty.wacc")]
#[case("../tests/valid/array/arrayLength.wacc")]
#[case("../tests/valid/array/arrayNested.wacc")]
#[case("../tests/valid/array/modifyString.wacc")]
#[case("../tests/valid/array/arrayPrint.wacc")]
#[case("../tests/valid/array/arraySimple.wacc")]
#[case("../tests/valid/array/printRef.wacc")]
#[case("../tests/valid/array/array.wacc")]
#[case("../tests/valid/IO/IOLoop.wacc")]
#[case("../tests/valid/IO/IOSequence.wacc")]
#[case("../tests/valid/IO/read/echoNegInt.wacc")]
#[case("../tests/valid/IO/read/echoBigInt.wacc")]
#[case("../tests/valid/IO/read/read.wacc")]
#[case("../tests/valid/IO/read/echoChar.wacc")]
#[case("../tests/valid/IO/read/echoInt.wacc")]
#[case("../tests/valid/IO/read/echoPuncChar.wacc")]
#[case("../tests/valid/IO/read/echoBigNegInt.wacc")]
#[case("../tests/valid/IO/print/printBool.wacc")]
#[case("../tests/valid/IO/print/print.wacc")]
#[case("../tests/valid/IO/print/println.wacc")]
#[case("../tests/valid/IO/print/printEscChar.wacc")]
#[case("../tests/valid/IO/print/printChar.wacc")]
#[case("../tests/valid/IO/print/multipleStringsAssignment.wacc")]
#[case("../tests/valid/IO/print/printCharArray.wacc")]
#[case("../tests/valid/IO/print/printCharAsString.wacc")]
#[case("../tests/valid/IO/print/print-backspace.wacc")]
#[case("../tests/valid/IO/print/print-carridge-return.wacc")]
#[case("../tests/valid/IO/print/hashInProgram.wacc")]
#[case("../tests/valid/IO/print/printInt.wacc")]
#[case("../tests/valid/while/fibonacciFullIt.wacc")]
#[case("../tests/valid/while/min.wacc")]
#[case("../tests/valid/while/max.wacc")]
#[case("../tests/valid/while/whileCount.wacc")]
#[case("../tests/valid/while/loopCharCondition.wacc")]
#[case("../tests/valid/while/loopIntCondition.wacc")]
#[case("../tests/valid/while/whileBoolFlip.wacc")]
#[case("../tests/valid/while/fibonacciIterative.wacc")]
#[case("../tests/valid/while/whileBasic.wacc")]
#[case("../tests/valid/while/rmStyleAdd.wacc")]
#[case("../tests/valid/while/whileFalse.wacc")]
#[case("../tests/valid/while/rmStyleAddIO.wacc")]
#[case("../tests/valid/expressions/sequentialCount.wacc")]
#[case("../tests/valid/expressions/andExpr.wacc")]
#[case("../tests/valid/expressions/equalsExpr.wacc")]
#[case("../tests/valid/expressions/charComparisonExpr.wacc")]
#[case("../tests/valid/expressions/intExpr1.wacc")]
#[case("../tests/valid/expressions/andOverOrExpr.wacc")]
#[case("../tests/valid/expressions/minusPlusExpr.wacc")]
#[case("../tests/valid/expressions/negBothDiv.wacc")]
#[case("../tests/valid/expressions/plusMinusExpr.wacc")]
#[case("../tests/valid/expressions/multNoWhitespaceExpr.wacc")]
#[case("../tests/valid/expressions/equalsOverBool.wacc")]
#[case("../tests/valid/expressions/equalsOverOr.wacc")]
#[case("../tests/valid/expressions/negExpr.wacc")]
#[case("../tests/valid/expressions/notequalsExpr.wacc")]
#[case("../tests/valid/expressions/longExpr.wacc")]
#[case("../tests/valid/expressions/negDivisorDiv.wacc")]
#[case("../tests/valid/expressions/lessCharExpr.wacc")]
#[case("../tests/valid/expressions/multExpr.wacc")]
#[case("../tests/valid/expressions/minusExpr.wacc")]
#[case("../tests/valid/expressions/longSplitExpr.wacc")]
#[case("../tests/valid/expressions/minusNoWhitespaceExpr.wacc")]
#[case("../tests/valid/expressions/longExpr3.wacc")]
#[case("../tests/valid/expressions/negBothMod.wacc")]
#[case("../tests/valid/expressions/plusNoWhitespaceExpr.wacc")]
#[case("../tests/valid/expressions/stringEqualsExpr.wacc")]
#[case("../tests/valid/expressions/modExpr.wacc")]
#[case("../tests/valid/expressions/ordAndchrExpr.wacc")]
#[case("../tests/valid/expressions/negDividendDiv.wacc")]
#[case("../tests/valid/expressions/notExpr.wacc")]
#[case("../tests/valid/expressions/boolExpr1.wacc")]
#[case("../tests/valid/expressions/lessEqExpr.wacc")]
#[case("../tests/valid/expressions/intCalc.wacc")]
#[case("../tests/valid/expressions/plusPlusExpr.wacc")]
#[case("../tests/valid/expressions/longExpr2.wacc")]
#[case("../tests/valid/expressions/equalsOverAnd.wacc")]
#[case("../tests/valid/expressions/orExpr.wacc")]
#[case("../tests/valid/expressions/negDividendMod.wacc")]
#[case("../tests/valid/expressions/plusExpr.wacc")]
#[case("../tests/valid/expressions/greaterEqExpr.wacc")]
#[case("../tests/valid/expressions/negDivisorMod.wacc")]
#[case("../tests/valid/expressions/longSplitExpr2.wacc")]
#[case("../tests/valid/expressions/minusMinusExpr.wacc")]
#[case("../tests/valid/expressions/greaterExpr.wacc")]
#[case("../tests/valid/expressions/lessExpr.wacc")]
#[case("../tests/valid/expressions/divExpr.wacc")]
#[case("../tests/valid/expressions/boolCalc.wacc")]
#[case("../tests/valid/sequence/basicSeq.wacc")]
#[case("../tests/valid/sequence/intAssignment.wacc")]
#[case("../tests/valid/sequence/stringAssignment.wacc")]
#[case("../tests/valid/sequence/basicSeq2.wacc")]
#[case("../tests/valid/sequence/boolAssignment.wacc")]
#[case("../tests/valid/sequence/charAssignment.wacc")]
#[case("../tests/valid/sequence/exitSimple.wacc")]
#[case("../tests/valid/sequence/intLeadingZeros.wacc")]
#[case("../tests/valid/if/if2.wacc")]
#[case("../tests/valid/if/if5.wacc")]
#[case("../tests/valid/if/whitespace.wacc")]
#[case("../tests/valid/if/if4.wacc")]
#[case("../tests/valid/if/if6.wacc")]
#[case("../tests/valid/if/ifFalse.wacc")]
#[case("../tests/valid/if/if1.wacc")]
#[case("../tests/valid/if/if3.wacc")]
#[case("../tests/valid/if/ifTrue.wacc")]
#[case("../tests/valid/if/ifBasic.wacc")]
#[case("../tests/valid/advanced/binarySortTree.wacc")]
#[case("../tests/valid/advanced/hashTable.wacc")]
#[case("../tests/valid/advanced/ticTacToe.wacc")]
#[case("../tests/valid/pairs/null.wacc")]
#[case("../tests/valid/pairs/createPair.wacc")]
#[case("../tests/valid/pairs/printPairOfNulls.wacc")]
#[case("../tests/valid/pairs/createRefPair.wacc")]
#[case("../tests/valid/pairs/readPair.wacc")]
#[case("../tests/valid/pairs/free.wacc")]
#[case("../tests/valid/pairs/printPair.wacc")]
#[case("../tests/valid/pairs/printNullPair.wacc")]
#[case("../tests/valid/pairs/checkRefPair.wacc")]
#[case("../tests/valid/pairs/writeSnd.wacc")]
#[case("../tests/valid/pairs/createPair02.wacc")]
#[case("../tests/valid/pairs/createPair03.wacc")]
#[case("../tests/valid/pairs/writeFst.wacc")]
#[case("../tests/valid/pairs/printNull.wacc")]
#[case("../tests/valid/pairs/nestedPair.wacc")]
#[case("../tests/valid/pairs/linkedList.wacc")]
#[case("../tests/valid/basic/exit/exitBasic2.wacc")]
#[case("../tests/valid/basic/exit/exitBasic.wacc")]
#[case("../tests/valid/basic/exit/exit-1.wacc")]
#[case("../tests/valid/basic/exit/exitWrap.wacc")]
#[case("../tests/valid/basic/skip/skip.wacc")]
#[case("../tests/valid/basic/skip/comment.wacc")]
#[case("../tests/valid/basic/skip/commentInLine.wacc")]
#[case("../tests/invalid/semanticErr/multiple/funcMess.wacc")]
#[case("../tests/invalid/semanticErr/multiple/ifAndWhileErrs.wacc")]
#[case("../tests/invalid/semanticErr/multiple/multiTypeErrs.wacc")]
#[case("../tests/invalid/semanticErr/multiple/multiCaseSensitivity.wacc")]
#[case("../tests/invalid/semanticErr/multiple/messyExpr.wacc")]
#[case("../tests/invalid/semanticErr/scope/badScopeRedefine.wacc")]
#[case("../tests/invalid/semanticErr/read/readTypeErr01.wacc")]
#[case("../tests/invalid/semanticErr/function/functionOverArgs.wacc")]
#[case("../tests/invalid/semanticErr/function/functionBadParam.wacc")]
#[case("../tests/invalid/semanticErr/function/functionBadReturn.wacc")]
#[case("../tests/invalid/semanticErr/function/funcVarAccess.wacc")]
#[case("../tests/invalid/semanticErr/function/functionSwapArgs.wacc")]
#[case("../tests/invalid/semanticErr/function/functionBadCall.wacc")]
#[case("../tests/invalid/semanticErr/function/functionUnderArgs.wacc")]
#[case("../tests/invalid/semanticErr/function/functionBadArgUse.wacc")]
#[case("../tests/invalid/semanticErr/function/functionAssign.wacc")]
#[case("../tests/invalid/semanticErr/function/functionRedefine.wacc")]
#[case("../tests/invalid/semanticErr/variables/undeclaredVar.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr08.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr01.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr10.wacc")]
#[case("../tests/invalid/semanticErr/variables/undeclaredVarAccess.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr07.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr02.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr06.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr03.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr04.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr05.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr12.wacc")]
#[case("../tests/invalid/semanticErr/variables/doubleDeclare.wacc")]
#[case("../tests/invalid/semanticErr/variables/caseMatters.wacc")]
#[case("../tests/invalid/semanticErr/variables/undeclaredScopeVar.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr11.wacc")]
#[case("../tests/invalid/semanticErr/variables/basicTypeErr09.wacc")]
#[case("../tests/invalid/semanticErr/print/printTypeErr01.wacc")]
#[case("../tests/invalid/semanticErr/exit/badCharExit.wacc")]
#[case("../tests/invalid/semanticErr/exit/globalReturn.wacc")]
#[case("../tests/invalid/semanticErr/exit/exitNonInt.wacc")]
#[case("../tests/invalid/semanticErr/IO/readTypeErr.wacc")]
#[case("../tests/invalid/semanticErr/while/falsErr.wacc")]
#[case("../tests/invalid/semanticErr/while/truErr.wacc")]
#[case("../tests/invalid/semanticErr/while/whileIntCondition.wacc")]
#[case("../tests/invalid/semanticErr/expressions/exprTypeErr.wacc")]
#[case("../tests/invalid/semanticErr/expressions/boolOpTypeErr.wacc")]
#[case("../tests/invalid/semanticErr/expressions/intOpTypeErr.wacc")]
#[case("../tests/invalid/semanticErr/expressions/moreArrExpr.wacc")]
#[case("../tests/invalid/semanticErr/expressions/lessPairExpr.wacc")]
#[case("../tests/invalid/semanticErr/expressions/stringElemErr.wacc")]
#[case("../tests/invalid/semanticErr/expressions/mixedOpTypeErr.wacc")]
#[case("../tests/invalid/semanticErr/if/ifIntCondition.wacc")]
#[case("../tests/invalid/semanticErr/pairs/sndNull.wacc")]
#[case("../tests/invalid/semanticErr/pairs/fstNull.wacc")]
#[case("../tests/invalid/semanticErr/pairs/freeNonPair.wacc")]
#[case("../tests/invalid/semanticErr/pairsExtended/invalidNewPairAssignment.wacc")]
#[case("../tests/invalid/semanticErr/pairsExtended/invalidPairAssignment.wacc")]
fn parse_pass(#[case] file_name: &str) {
    let mut absolute_prefix = Path::new(file!()).to_owned();
    absolute_prefix.pop();
    let (main_input, modules) =
        gather_modules(&absolute_prefix.join(Path::new(file_name))).unwrap();
    let res = parse(
        main_input.to_parse_contents(),
        modules.iter().map(InputFile::to_parse_contents).collect(),
    )
    .map_err(|mut e| {
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
    assert!(res.is_ok());
}

#[rstest]
#[case("../tests/invalid/syntaxErr/function/funcExpr2.wacc")]
#[case("../tests/invalid/syntaxErr/function/functionMissingCall.wacc")]
#[case("../tests/invalid/syntaxErr/function/functionMissingParam.wacc")]
#[case("../tests/invalid/syntaxErr/function/badlyPlaced.wacc")]
#[case("../tests/invalid/syntaxErr/function/badlyNamed.wacc")]
#[case("../tests/invalid/syntaxErr/function/funcExpr.wacc")]
#[case("../tests/invalid/syntaxErr/function/functionMissingType.wacc")]
#[case("../tests/invalid/syntaxErr/function/functionMissingPType.wacc")]
#[case("../tests/invalid/syntaxErr/function/thisIsNotC.wacc")]
#[case("../tests/invalid/syntaxErr/function/functionScopeDef.wacc")]
#[case("../tests/invalid/syntaxErr/function/functionLateDefine.wacc")]
#[case("../tests/invalid/syntaxErr/function/noBodyAfterFuncs.wacc")]
#[case("../tests/invalid/syntaxErr/variables/badintAssignments.wacc")]
#[case("../tests/invalid/syntaxErr/variables/bigIntAssignment.wacc")]
#[case("../tests/invalid/syntaxErr/variables/varNoName.wacc")]
#[case("../tests/invalid/syntaxErr/variables/badintAssignments2.wacc")]
#[case("../tests/invalid/syntaxErr/variables/badintAssignments1.wacc")]
#[case("../tests/invalid/syntaxErr/array/arrayExpr.wacc")]
#[case("../tests/invalid/syntaxErr/print/printlnCharArry.wacc")]
#[case("../tests/invalid/syntaxErr/while/donoErr.wacc")]
#[case("../tests/invalid/syntaxErr/while/dooErr.wacc")]
#[case("../tests/invalid/syntaxErr/while/whileNodo.wacc")]
#[case("../tests/invalid/syntaxErr/while/whileNodone.wacc")]
#[case("../tests/invalid/syntaxErr/while/whilErr.wacc")]
#[case("../tests/invalid/syntaxErr/expressions/missingOperand2.wacc")]
#[case("../tests/invalid/syntaxErr/expressions/printlnConcat.wacc")]
#[case("../tests/invalid/syntaxErr/expressions/missingOperand1.wacc")]
#[case("../tests/invalid/syntaxErr/sequence/emptySeq.wacc")]
#[case("../tests/invalid/syntaxErr/sequence/endSeq.wacc")]
#[case("../tests/invalid/syntaxErr/sequence/extraSeq.wacc")]
#[case("../tests/invalid/syntaxErr/sequence/doubleSeq.wacc")]
#[case("../tests/invalid/syntaxErr/sequence/missingSeq.wacc")]
#[case("../tests/invalid/syntaxErr/if/ifiErr.wacc")]
#[case("../tests/invalid/syntaxErr/if/ifNothen.wacc")]
#[case("../tests/invalid/syntaxErr/if/ifNoelse.wacc")]
#[case("../tests/invalid/syntaxErr/if/ifNofi.wacc")]
#[case("../tests/invalid/syntaxErr/pairs/badLookup02.wacc")]
#[case("../tests/invalid/syntaxErr/pairs/badLookup01.wacc")]
#[case("../tests/invalid/syntaxErr/basic/unescapedChar.wacc")]
#[case("../tests/invalid/syntaxErr/basic/badEscape.wacc")]
#[case("../tests/invalid/syntaxErr/basic/badComment2.wacc")]
#[case("../tests/invalid/syntaxErr/basic/multipleBegins.wacc")]
#[case("../tests/invalid/syntaxErr/basic/badComment.wacc")]
#[case("../tests/invalid/syntaxErr/basic/beginNoend.wacc")]
#[case("../tests/invalid/syntaxErr/basic/bgnErr.wacc")]
#[case("../tests/invalid/syntaxErr/basic/noBody.wacc")]
#[case("../tests/invalid/syntaxErr/basic/skpErr.wacc")]
#[case("../tests/invalid/syntaxErr/pairsExtended/invalidInnerPair.wacc")]
#[case("../tests/invalid/syntaxErr/pairsExtended/invalidPairArray.wacc")]
#[case("../tests/invalid/syntaxErr/pairsExtended/invalidPairNested.wacc")]
fn parse_fail(#[case] file_name: &str) {
    let mut absolute_prefix = Path::new(file!()).to_owned();
    absolute_prefix.pop();
    let (main_input, modules) =
        gather_modules(&absolute_prefix.join(Path::new(file_name))).unwrap();
    let res = parse(
        main_input.to_parse_contents(),
        modules.iter().map(InputFile::to_parse_contents).collect(),
    )
    .map_err(|mut e| {
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
