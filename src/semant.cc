//===================================================================================================================
//
// semant.cc -- this file is the implementation of the semantic checker for the pascal-cc compiler
//
// The particular checks are dependent on the environment as defined at the point we reach the semantic check in
// question.  The environment is represented by the symbol table S, which is the list of defined symbols in the
// environment at that point of the tree.
//
// TODO: need to work on union and intersection operations for types...
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----------  -------  -------  ----  ---------------------------------------------------------------------------
// 2016-01-18  Initial   v0.0    ADCL  Initial version
// 2016-01-22   #256     v0.0    ADCL  Related to phrase 6.1.6, labels must be between 0 and 9999 inclusive.
//                                     Add this semantic check.
// 2016-02-08   #262     v0.0    ADCL  In the process of changing how the type system is built, we have sevaral
//                                     changes in this file to accomodate the changes.
// 2016-02-10            v0.0    ADCL  You might notice a change in MOST of this source file.  These changes are
//                                     due to changing how the file is intended (using spaces rather than tabs).
//                                     In addition, trailing spaces have been removed.
//
//===================================================================================================================


#include "debug.hh"
#include "symtab.hh"
#include "pascal.hh"

#include <cstdarg>
#include <cstdlib>
#include <cstring>


Symbol *BOOLEAN;
Symbol *CHAR;
Symbol *INTEGER;
Symbol *REAL;
Symbol *STRING;
Symbol *POINTER;

Symbol *NIL;
Symbol *MAXINT;
Symbol *TRUE;
Symbol *FALSE;


int semantErrors = 0;


//
// -- Issue a Semantic error
//    ----------------------
void SemantError(Common *node, const char *msg, ...)
{
    va_list args;
    va_start(args, msg);
    fprintf(stderr, RED HI "Error [%s @ %d]: ", node->Get_fileName().c_str(), node->Get_lineNbr());
    vfprintf(stderr, msg, args);
    va_end(args);
    fprintf(stderr, "\n" RESET);

    if (++ semantErrors > 50) {
        fprintf(stderr, RED HI "More than 50 errors\n" RESET);
        exit(EXIT_FAILURE);
    }
}


//
// -- This is the default semantic check function -- which simply reports an error and returns.  This function
//    is used as a catch-all for unimplemeted semantic checks.  Ideally, this function will never be executed.
//    --------------------------------------------------------------------------------------------------------
void Common::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_INFO,
            RED "The semant() function has not been implemented for AST tree node type %d" RESET,
            Get_AstNodeType());
}


//====================================================================================================================
//
// -- Section 6.4.5: One of the basic rules of semantic checking pascal is the notion of Compatible Types.  These
//    are defined in section 6.4.5 of the ISO Standard.  Types T1 and T2 are compatible if ANY of the following 4
//    statements is true:
//
//    1)  T1 and T2 are the same type.
//    2)  One of the following 3 statements is true:
//        a)  T1 is a subrange of T2
//        b)  T2 is a subrange of T1
//        c)  T1 and T2 are both subranges of the same host type (which is 1 level up)
//    3)  Both T1 and T2 are set-types of compatible base-types, and:
//        a)  Either BOTH T1 and T2 are designated as packed
//        b)  Or NEITHER T1 nor T2 are designated as packed
//    4)  T1 and T2 are string types (read array of strings) with the same number of components
//
//    This function only operates on types, it does not operate on Variables.
//
//    So, this function will be used to determine compatibilty using the above rules.
//    --------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
bool AreTypesCompatible(Common *node, DefinedType *T1, DefinedType *T2)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering AreTypesCompatible()");
    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  ndoe = %p", node);
    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  T1 = %p (%s)", T1, T1?T1->GetKeyString():"");
    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  T2 = %p (%s)", T2, T2?T2->GetKeyString():"");


    //
    // -- Sanity check -- we need to be operating on types only
    //    -----------------------------------------------------
    if (!T1 || !T2) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "T1 and/or T2 are NULL");
        goto out;
    }


    //
    // -- Check condition 1...
    //    --------------------
    if (T1 == T2) {
        Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... The types are the same");
        goto exit;
    }


    //
    // -- Check condition 2...
    //    --------------------
    if (T1->kind == TYP_SUBRANGE && T1->hostType == T2) {
        Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... T1 is a subtype of T2");
        goto exit;
    }

    if (T2->kind == TYP_SUBRANGE && T2->hostType == T1) {
        Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... T2 is a subtype of T1");
        goto exit;
    }

    if (T1->kind == TYP_SUBRANGE && T2->kind == TYP_SUBRANGE && T1->hostType == T2->hostType) {
        Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... T1 and T2 are subtypes of the same host type");
        goto exit;
    }


    //
    // -- Check condition 3...
    //    --------------------
    if (node && AreTypesCompatible(NULL, T1, T2)) {
        if (T1->isPacked && T2->isPacked) {
            Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... Compatible types are packed");
            goto exit;
        }

        if (!T1->isPacked && !T2->isPacked) {
            Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... Compatible types are not packed");
            goto exit;
        }
    }


    //
    // -- Check condition 4...
    //    --------------------
    if (T1 == FindType(STRING->id) && T2 == FindType(STRING->id)) {
//		if (T1->elemCount == T2->elemCount) goto exit;
        goto exit;
    }


    //
    // -- Now, if we get here, they are not compatible types
    //    --------------------------------------------------
out:
    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  returning false");
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Exiting AreTypesCompatible()");
    return false;

exit:
    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  returning true");
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Exiting AreTypesCompatible()");
    return true;
}


//====================================================================================================================
//
// -- Check a SourceFile object
//
//    Performing a semantic check on a file consists of the following actions:
//    1.  Opening a symbol table scope
//    2.  Add all the language-defined global symbols
//    3.  If we have a ProgramHeading, we need to check it (if not, we have a module)
//    4.  Checking the code (if the code does not exist, we have an error)
//    5.  Exit the symbol table scope
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void SourceFile::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SourceFile::semant()");
    EnterScope();

    //
    // -- Add the global language types
    //    -----------------------------
    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding Global Symbols for the types");
    BOOLEAN = AddSymbol(idTable.lookup("boolean"))->SetTypedef();
    STRING = AddSymbol(idTable.lookup("string"))->SetTypedef();
    CHAR = AddSymbol(idTable.lookup("char"))->SetTypedef();
    INTEGER = AddSymbol(idTable.lookup("integer"))->SetTypedef();
    REAL = AddSymbol(idTable.lookup("real"))->SetTypedef();
    POINTER = AddSymbol(idTable.lookup("^"))->SetTypedef();

    //
    // -- Now, create defined type structures for these types
    //    ---------------------------------------------------
    DefinedType *tpBOOLEAN = AddType(BOOLEAN);
    tpBOOLEAN->kind = TYP_BASE_TYPE;
    BOOLEAN->SetTypeDefinition(tpBOOLEAN);

    DefinedType *tpPOINTER = AddType(idTable.AddUnnamedIdent());
    tpPOINTER->kind = TYP_POINTER;
    POINTER->SetTypeDefinition(tpPOINTER);

    DefinedType *tpINTEGER = AddType(INTEGER);
    tpINTEGER->kind = TYP_BASE_TYPE;
    INTEGER->SetTypeDefinition(tpINTEGER);

    DefinedType *tpREAL = AddType(REAL);
    tpREAL->kind = TYP_BASE_TYPE;
    REAL->SetTypeDefinition(tpREAL);

    DefinedType *tpCHAR = AddType(CHAR);
    tpCHAR->kind = TYP_BASE_TYPE;
    CHAR->SetTypeDefinition(tpCHAR);

    //
    // -- Add the global language constants
    //    ---------------------------------
    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding Global Symbols for language defined constants");
    MAXINT = AddSymbol(idTable.lookup("maxint"))->SetIdent()->SetInteger()->SetConst()->SetTypeDefinition(tpINTEGER);
    MAXINT->cVal = new ConstantValue;
    MAXINT->cVal->intVal = 0x7fffffff;

    NIL = AddSymbol(idTable.lookup("^"))->SetPointer()->SetConst()->SetTypeDefinition(tpPOINTER);
    NIL->cVal = new ConstantValue;
    NIL->cVal->memAddr = 0;

    FALSE = AddSymbol(idTable.lookup("false"))->SetIdent()->SetBool()->SetConst()->SetTypeDefinition(tpBOOLEAN);
    FALSE->cVal = new ConstantValue;
    FALSE->cVal->boolVal = false;

    TRUE = AddSymbol(idTable.lookup("true"))->SetIdent()->SetBool()->SetConst()->SetTypeDefinition(tpBOOLEAN);
    TRUE->cVal = new ConstantValue;
    TRUE->cVal->boolVal = true;


    //
    // -- The STRING type is actually noting more than an ARRAY of CHAR with a single index from 0 .. MAXINT
    //    --------------------------------------------------------------------------------------------------
    DefinedType *tpIndex = AddType(idTable.AddUnnamedIdent());
    tpIndex->kind = TYP_SUBRANGE;
    tpIndex->lowerBound = 0;
    tpIndex->upperBound = MAXINT->cVal->intVal;

    DefinedType *tpSTRING = AddType(STRING);
    tpSTRING->kind = TYP_ARRAY;
    tpSTRING->indexType = tpIndex;
    tpSTRING->componentType = tpCHAR;


    //
    // -- the constant value for MAXINT needs to be added to the number string table; so does 0
    //    -------------------------------------------------------------------------------------
    numTable.AddString("2147483647");
    numTable.AddString("0");


    //
    // -- Now, for the formality of checking, we Dump the type records
    //    ------------------------------------------------------------
    DumpTypeRecord(tpBOOLEAN);
    DumpTypeRecord(tpPOINTER);
    DumpTypeRecord(tpINTEGER);
    DumpTypeRecord(tpREAL);
    DumpTypeRecord(tpCHAR);
    DumpTypeRecord(tpSTRING);


    //
    // -- Now start checking the actual code
    //    ----------------------------------
    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Beginning Semantic Analysis");
    if (pgmHdg) pgmHdg->semant();

    if (code) code->semant();
    else SemantError(this, "A program must have a block of statements");


    //
    // -- Clean up
    //    --------
    ExitScope();
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving SourceFile::semant()");
}


//====================================================================================================================
//
// -- Check a ProgramHeading object
//
//    Performing a semantic check on a ProgramHeading node consists of the following:
//    1.  Add the program name to the symbol table as a function
//    2.  Check each parm name in the symbol table to see if it is duplicate
//    3.  Add each parm name to the symbol table
//    4.  Each parm has the type string and is a constant
//    -------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void ProgramHeading::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ProgramHeading::semant()");


    //
    // -- Add the program name into the symbol table
    //    ------------------------------------------
    if (CheckScope(name->Get_entry())) {
        SemantError(this, "Program name %s already defined in the symbol table", name->GetString());
    } else {
        Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding program name %s to the identifiers table", name->GetString());
        AddSymbol(name->Get_entry())->SetProgram();
    }


    //
    // -- Now, add each parameter into the symbol table and force the types
    //    -----------------------------------------------------------------
    for (IdentList *l = parms; more(l); l = next(l)) {
        if (CheckScope(l->Get_entry())) {
            SemantError(this, "Parameter name %s defined more than once", l->GetString());
        } else {
            Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding parameter name %s to the identifiers table", l->GetString());
            AddSymbol(l->Get_entry())->SetString()->SetConst();
        }
    }

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ProgramHeading::semant()");
}


//====================================================================================================================
//
// -- Check the Block object
//
//    In short, a block is a basic programming scope in pascal.  When performing semantic analysis on a block,
//    we will perform the following actions in this order:
//    1.  Create a new scope
//    2.  If exists, process the labels list; ignore if not
//    3.  If exists, process the constant definitions; ignore if not
//    4.  If exists, process the type defintiions; ignore if not
//    5.  If exists, process the variable declarations; ignore if not
//    6.  If exists, process the subroutinte definitions; ignore if not
//    7.  Finally, type check the statement list that is the code of the block
//    8.  Once all of that is complete, we will delete the scope we created
//    ----------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void Block::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering Block::semant()");

    EnterScope();

    if (labels) labels->semant();
    if (consts) consts->semant();
    if (typedefs) typedefs->semant();
    if (vardecls) vardecls->semant();
    if (subrdecls) subrdecls->semant();
    if (stmts) stmts->semant();
    else SemantError(this, "A block needs at least 1 excutable statement");

    ExitScope();

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving Block::semant()");
}


//====================================================================================================================
//
// -- Section 6.1.1: Check Label Declarations
//
//    Labels all need to be defined at the beginning of a block.  They cannot be referenced outside the block
//    so, we need to only check the current scope for any duplication.  We will add the integer strings into the
//    symbol table as LABEL types, even though they are integer literals.  Later I expect to add identifiers to
//    the labels as well.
//
//    We will handle the list of label declarations within this labels list semantic check.  This means that each
//    individual label will be taken care of in this function.  The reason for this is that the label node is also
//    used as part of a statement and as the target of the goto statement.
//
//    Therefore, the following will be done with each label in the list:
//    1.  Check the current scope for a label with the same name
//    2.  If it exists, then issue an error message
//    3.  If it does not exist, then add it to the symbol table
//    4.  Ensure that there is not a leading zero in the label
//    5.  Ensure that the label is in the range 0 - 9999 (inclusive)
//
//    ------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-01-22  ADCL   #256     v0.0    Related to phrase 6.1.6, labels must be between 0 and 9999 inclusive.
//                                        Add this semantic check.
//
//====================================================================================================================
void LabelList::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering LabelList::semant()");


    //
    // -- We are dealing with a list, so we need to iterate the list
    //    ----------------------------------------------------------
    for (LabelList *wrk = this; more(wrk); wrk = next(wrk)) {
        if (CheckScope(wrk->Get_label()->Get_entry())) {
            //
            // -- The label exists, so we issue a semantic error
            //    ----------------------------------------------
            Symbol *sym = FindSymbol(wrk->Get_label()->Get_entry());

            if (sym->kind == LABEL) {
                SemantError(this, "Label %s already defined for this scope", sym->GetKeyString());
            } else {
                SemantError(this, "Label %s is already defined as some other meaning", sym->GetKeyString());
            }
        } else {
            //
            // -- This is a new label, we we need to perform some additional checks....
            //    ---------------------------------------------------------------------
            int dig = atoi(wrk->Get_label()->Get_entry()->GetKeyString());

            if (wrk->Get_label()->Get_entry()->GetKeyString()[0] == '0' && dig != 0) {
                SemantError(this, "Label %s cannot start with a leading 0, will %d work instead?",
                        wrk->Get_label()->Get_entry()->GetKeyString(), dig);
            }

            if (dig > 9999 || dig < 0) {
                SemantError(this, "Label must be between 0 and 9999 inclusive; label %d out of range", dig);
            }


            //
            // -- Even if we have a bad label, we add it anyway so that we can reduce the unnecessary errors
            //    ------------------------------------------------------------------------------------------
            Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding symbol for label %s",
                    wrk->Get_label()->Get_entry()->GetKeyString());

            AddSymbol(wrk->Get_label()->Get_entry())->SetLabel();
        }
    }

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving LabelList::semant()");
}


//====================================================================================================================
//
// -- Check List of Constant Declarations
//
//    This simple check is there to dispatch a semantic and type check for each constant definition in the constant
//    list.
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void ConstList::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ConstList::semant()");

    for (ConstList *wrk = this; more(wrk); wrk = next(wrk)) {
        wrk->Get_constDef()->semant();
    }

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ConstList::semant()");
}


//====================================================================================================================
//
// -- Check Constant Declarations
//
//    Constants all need to be defined and EVALUATED at the beginning of a block and before they are used.
//    This means that all components of the expression that make up the constant definition must also be constants.
//    We will define a symbol for each constant and will store it's computed value in the symbol table.  The constant
//    name must be unique.
//
//    The following are the checks that will be completed with each constant declaration:
//    1.  Check the current scope for a constant name with the same name
//    2.  If it exists, issue an error message
//    3.  If it does not exist, add it to the symbol table
//    4.  Perform a type check on the constant expression that is its assigned value
//    6.  When the expression is a valid type (CHAR, BOOLEAN, INTEGER, REAL, or STRING) and is constant, evaluate
//        the expression to a final value
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//    2016-02-08  ADCL   #259     v0.0    Since I have to touch nearly every method, I will make sure we are
//                                        comparing compatible types.
//
//====================================================================================================================
void ConstAssign::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ConstAssign::semant()");

    Symbol *sym = NULL;

    if (CheckScope(id->Get_entry())) {
        sym = FindSymbol(id->Get_entry());

        if (sym->isConst) SemantError(this, "Constant %s already defined for this scope", sym->GetKeyString());
        else SemantError(this, "Constant %s is already defined as some other meaning", sym->GetKeyString());

        goto exit;
    }


    //
    // -- here begins step 4...
    //    ---------------------
    if (!val) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value missing");
        goto exit;
    }

    val->semant();

    if (!sym) {
        Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding symbol for constant %s", id->Get_entry()->GetKeyString());
        sym = AddSymbol(id->Get_entry())->SetIdent();
    }

    if (!val->Get_isConst()) {
        SemantError(this, "Constant expression does not evaluate to a constant");

        goto exit;
    }

    if (AreTypesCompatible(this, val->Get_defTyp(), BOOLEAN) ||
            AreTypesCompatible(this, val->Get_defTyp(), CHAR) ||
            AreTypesCompatible(this, val->Get_defTyp(), INTEGER) ||
            AreTypesCompatible(this, val->Get_defTyp(), REAL)) {
        sym->cVal = val->EvaluateConst();
    } else if (AreTypesCompatible(this, val->Get_defTyp(), STRING)) {
    } else if (AreTypesCompatible(this, val->Get_defTyp(), NIL)) {
    } else SemantError(this, "Constant expression does not evaluate to a base type");

    sym->typeSym = FindSymbol(val->Get_defTyp()->typeName);
    sym->isConst = val->Get_isConst();

    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "The resulting type from constant evaluation of %s is: %p (%s)",
            sym->GetKeyString(), sym->typeSym, sym->typeSym?sym->typeSym->GetKeyString():"(NULL)");


    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Assigning Constant Value to a Constant Definition");

exit:
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ConstAssign::semant()");
}


//====================================================================================================================
//
// -- Check a NIL constant
//
//    The NIL constant is a specially defined keyword with special meaning.  It is a "0" address.
//    With that knowledge we can make some very specific type checking rules for this value.
//
//
//    ___________________________
//
//    |- NIL : POINTER (Constant)
//
//    ---------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void NilLit::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NilLit::semant()");

    isConst = true;
    isPointer = true;
    defTyp = NIL->typeDefinition;

    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Allocating a constant value structure");
    cVal = new ConstantValue;
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory allocating a constant value structure");
        exit(EXIT_FAILURE);
    }

    cVal->memAddr = 0;

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NilLit::semant()");
}


//====================================================================================================================
//
// -- Check a real literal constant
//
//    A real literal is a literal constant, meaning it is not a variable and cannot change values.
//    With that knowledge we can make come very specific type checking rules for this type.
//
//    |- r is a real literal
//    ______________________
//
//    |- r : REAL (Constant)
//
//    ----------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void RealLit::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering RealLit::semant()");

    isConst = true;
    defTyp = REAL->typeDefinition;

    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Allocating a constant value structure");
    cVal = new ConstantValue;
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory allocating a constant value structure");
        exit(EXIT_FAILURE);
    }

    cVal->realVal = atof(GetString());
    Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "Converted %s to a real number %f", GetString(), cVal->realVal);

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving RealLit::semant()");
}


//====================================================================================================================
//
// -- Check an integer literal constant
//
//    An integer literal is a literal constant, meaning it is not a variable and cannot change values.
//    With that knowledge we can make come very specific type checking rules for this type.
//
//    |- i is an integer literal
//    __________________________
//
//    |- i : INTEGER (Constant)
//
//    ----------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void IntLit::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering IntLit::semant()");

    isConst = true;
    defTyp = INTEGER->typeDefinition;

    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Allocating a constant value structure");
    cVal = new ConstantValue;
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory allocating a constant value structure");
        exit(EXIT_FAILURE);
    }

    cVal->intVal = atol(GetString());
    Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "Converted %s to a integer number %ld", GetString(), cVal->intVal);

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving IntLit::semant()");
}


//====================================================================================================================
//
// -- Check a string literal constant
//
//    An string literal is a literal constant, meaning it is not a variable and cannot change values.
//    With that knowledge we can make come very specific type checking rules for this type.
//
//    |- s is an string literal
//    __________________________
//
//    |- s : STRING (Constant)
//
//    ----------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void StringLit::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering StringLit::semant()");

    isConst = true;
    defTyp = STRING->typeDefinition;

    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Allocating a constant value structure");
    cVal = new ConstantValue;
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory allocating a constant value structure");
        exit(EXIT_FAILURE);
    }

    cVal->strVal = strdup(GetString());
    Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "Established string constant %s", cVal->strVal);

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving StringLit::semant()");
}


//====================================================================================================================
//
// -- Ckeck an identifier
//
//    There are several things to check for when checking an identifier.  There are several language-defined
//    identifiers that will be present in the idTable right from the get-go, and others that will be added with
//    constant and variable and parameter and function (you get the point) declarations and definitions.
//
//    We will let the creation of the identifier handle properly decorating the type and constant of the symbol
//    as it is created.  Therefore, when we read this identifier, we should (in theory) already have a defined
//    symbol we can take the definitions from.  If not, then we will have an undefined symbol.
//
//    S |- S(id) : T
//    ______________
//
//    id : T
//
//    ---------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void Ident::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering Ident::semant()");

    Symbol *sym = FindSymbol(entry);

    if (!sym) SemantError(this, "Undefined symbol %s", entry->GetKeyString());
    else {
        SymbolKind k = sym->kind;

        switch (k) {
        case PROGRAM:
            defTyp = NULL;
            SemantError(this, "Program name '%s' cannot be used as an identifier", sym->GetKeyString());
            break;

        case LABEL:
            defTyp = NULL;
            SemantError(this, "Label '%s' cannot be used as an identifier", sym->GetKeyString());
            break;

        case PROCEDURE:
            defTyp = NULL;
            SemantError(this, "Procedure name '%s' cannot be used as an identifier", sym->GetKeyString());
            break;

        case FUNCTION:
            defTyp = NULL;
            SemantError(this, "Function name '%s' cannot be used as an identifier", sym->GetKeyString());
            break;

        case IDENT:
            Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Evaluating Identifier");

            isConst = sym->isConst;

            if (AreTypesCompatible(this, sym->typeSym, BOOLEAN)) defTyp = BOOLEAN->typeDefinition;
            else if (AreTypesCompatible(this, sym->typeSym, INTEGER)) defTyp = INTEGER->typeDefinition;
            else if (AreTypesCompatible(this, sym->typeSym, REAL)) defTyp = REAL->typeDefinition;
            else if (AreTypesCompatible(this, sym->typeSym, STRING)) defTyp = STRING->typeDefinition;
            else if (AreTypesCompatible(this, sym->typeSym, CHAR)) defTyp = CHAR->typeDefinition;
            else {
                SemantError(this, "Unknown type for identifier %s", sym->GetKeyString());
                break;
            }

            if (sym->typeSym == NULL) {
                SemantError(this, "Unknown type for identifier %s", sym->GetKeyString());
            }

            break;

        default:
            Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Unhandled kind of symbol %d in Ident::semant()", k);
            // fall through

        case UNKNOWN:
            SemantError(this, "UNKNOWN kind for name %s", sym->GetKeyString());
        }
    }

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving Ident::semant()");
}


//====================================================================================================================
//
// -- Check an identity unary expression
//
//    When a unary plus is used in an expression, it is referred to as an identity operation.  This operation
//    can only operate on numbers, that is integer and real types.
//
//    With this knowledge, we can write the type checking rules for this unary operation
//
//
//    S |- e : REAL
//    ______________
//
//    S |- +e : REAL
//
//
//    S |- e : INTEGER
//    _________________
//
//    S |- +e : INTEGER
//
//    --------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void PosExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering PosExpr::semant()");


    //
    // -- Perform a semantic check on the operand
    //    ---------------------------------------
    if (operand) operand->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Unary identity missing expression operand");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, operand->Get_defTyp(), INTEGER)
            && !AreTypesCompatible(this, operand->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type for unary identity expression operand");
    }


    //
    // -- Set up the flags for the symbol
    //    -------------------------------
    isConst = operand->Get_isConst();
    defTyp = operand->Get_defTyp();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving PosExpr::semant()");
}


//====================================================================================================================
//
// -- Check an unary negation expression
//
//    When a unary minus is used in an expression, it is negation operation.  This operation can only operate
//    on numbers, that is integer and real types.
//
//    With this knowledge, we can write the type checking rules for this unary operation
//
//
//    S |- e : REAL
//    ______________
//
//    S |- -e : REAL
//
//
//    S |- e : INTEGER
//    _________________
//
//    S |- -e : INTEGER
//
//    --------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void NegExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NegExpr::semant()");


    //
    // -- Perform a semantic check on the operand
    //    ---------------------------------------
    if (operand) operand->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Unary negation missing expression operand");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, operand->Get_defTyp(), INTEGER)
            && !AreTypesCompatible(this, operand->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type for unary negation expression operand");
    }


    //
    // -- Set up the flags for the symbol
    //    -------------------------------
    isConst = operand->Get_isConst();
    defTyp = operand->Get_defTyp();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NegExpr::semant()");
}


//====================================================================================================================
//
// -- Check an boolean not expression
//
//    Well, this is pretty simple: a not operations changes a boolean true to a boolean false, and vice-versa.
//
//
//    S |- e : BOOLEAN
//    ____________________
//
//    S |- NOT e : BOOLEAN
//
//    --------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void NotExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NotExpr::semant()");


    //
    // -- Perform a semantic check on the operand
    //    ---------------------------------------
    if (operand) operand->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Boolean not missing expression operand");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, operand->Get_defTyp(), BOOLEAN)) {
        SemantError(this, "Invalid type for boolean not expression operand");
    }


    //
    // -- Set up the flags for the symbol
    //    -------------------------------
    isConst = operand->Get_isConst();
    defTyp = operand->Get_defTyp();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NotExpr::semant()");
}


//====================================================================================================================
//
// -- Check a subtraction expression
//
//    A subtraction expression can only work on real or integer types.  Therefore, the left side of the
//    expression must evaluate to either a real or an interger type.  Furthermore, the expression is a
//    constant expression if and only if both the left and right sides of the expression evaluate to
//    constants.
//
//    At some later point, the ability to operate on types will be added.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : INTEGER
//    S |- e2 : INTEGER
//    ______________________
//
//    S |- e1 - e2 : INTEGER
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : REAL
//    ___________________
//
//    S |- e1 - e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : INTEGER
//    S |- e2 : REAL
//    ___________________
//
//    S |- e1 - e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : INTEGER
//    ___________________
//
//    S |- e1 - e2 : REAL
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void SubExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SubExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Subtraction missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Subtraction missing right side expression");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, left->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the left side of a subtraction expression");
    }


    if (!AreTypesCompatible(this, right->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the right side of a subtraction expression");
    }

    //
    // -- Now, only integer-integer computations compute to integers, otherwise real
    //    --------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        defTyp = INTEGER->typeDefinition;
    } else defTyp = REAL->typeDefinition;


    //
    // -- it is a constant if both operands are constants
    //    -----------------------------------------------
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving SubExpr::semant()");
}


//====================================================================================================================
//
// -- Check a addition expression
//
//    A addition expression can only work on real or integer types.  Therefore, the left side of the
//    expression must evaluate to either a real or an interger type.  Furthermore, the expression is a
//    constant expression if and only if both the left and right sides of the expression evaluate to
//    constants.
//
//    At some later point, the ability to operate on types will be added.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : INTEGER
//    S |- e2 : INTEGER
//    ______________________
//
//    S |- e1 + e2 : INTEGER
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : REAL
//    ___________________
//
//    S |- e1 + e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : INTEGER
//    S |- e2 : REAL
//    ___________________
//
//    S |- e1 + e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : INTEGER
//    ___________________
//
//    S |- e1 + e2 : REAL
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void AddExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering AddExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Addition missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Addition missing right side expression");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, left->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the left side of a addition expression");
    }

    if (!AreTypesCompatible(this, right->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the right side of a addition expression");
    }


    //
    // -- Now, only integer-integer computations compute to integers, otherwise real
    //    --------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        defTyp = INTEGER->typeDefinition;
    } else defTyp = REAL->typeDefinition;


    //
    // -- it is a constant if both operands are constants
    //    -----------------------------------------------
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving AddExpr::semant()");
}


//====================================================================================================================
//
// -- Check a multiplication expression
//
//    A multiplication expression can only work on real or integer types.  Therefore, the left side of the
//    expression must evaluate to either a real or an interger type.  Furthermore, the expression is a
//    constant expression if and only if both the left and right sides of the expression evaluate to
//    constants.
//
//    At some later point, the ability to operate on types will be added.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : INTEGER
//    S |- e2 : INTEGER
//    ______________________
//
//    S |- e1 * e2 : INTEGER
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : REAL
//    ___________________
//
//    S |- e1 * e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : INTEGER
//    S |- e2 : REAL
//    ___________________
//
//    S |- e1 * e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : INTEGER
//    ___________________
//
//    S |- e1 * e2 : REAL
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void MulExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering MulExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Multiplication missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Multiplication missing right side expression");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, left->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the left side of a multiplication expression");
    }

    if (!AreTypesCompatible(this, right->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the right side of a multiplication expression");
    }


    //
    // -- Now, only integer-integer computations compute to integers, otherwise real
    //    --------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        defTyp = INTEGER->typeDefinition;
    } else defTyp = REAL->typeDefinition;


    //
    // -- it is a constant if both operands are constants
    //    -----------------------------------------------
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving MulExpr::semant()");
}


//====================================================================================================================
//
// -- Check a division expression resulting in a real type
//
//    Separate from the other binary expressions, a real division will divide either real or integer numbers
//    but the result is experessed as a real number regardless of the type.
//
//    S |- e1 : (INTEGER | REAL)
//    S |- e2 : (INTEGER | REAL)
//    __________________________
//
//    S |- e1 / e2 : REAL
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void RealDivExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering RealDivExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Real division missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Real division missing right side expression");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, left->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the left side of a real division expression");
    }

    if (!AreTypesCompatible(this, right->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the right side of a real division expression");
    }


    //
    // -- Finish up the resuting type and determine if it is a constant
    //    -------------------------------------------------------------
    defTyp = REAL->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving RealDivExpr::semant()");
}


//====================================================================================================================
//
// -- Check a division expression resulting in a integer type
//
//    Separate from the other binary expressions, an integer division will divide only integer numbers
//    but the result is experessed as an integer number regardless of the result.
//
//    S |- e1 : INTEGER
//    S |- e2 : INTEGER
//    ___________________________
//
//    S |- e1 / e2 : INTEGER
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void IntDivExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering IntDivExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Integer division missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Integer division missing right side expression");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)) {
        SemantError(this, "Invalid type on the left side of a integer division expression");
    }

    if (!AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        SemantError(this, "Invalid type on the right side of a integer division expression");
    }


    //
    // -- Finish up the resuting type and determine if it is a constant
    //    -------------------------------------------------------------
    defTyp = INTEGER->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving IntDivExpr::semant()");
}


//====================================================================================================================
//
// -- Check a modulo expression
//
//    Separate from the other binary expressions, an integer division will divide either real or integer numbers
//    but the result is experessed as an integer number regardless of the type.
//
//    S |- e1 : INTEGER
//    S |- e2 : INTEGER
//    ________________________
//
//    S |- e1 mod e2 : INTEGER
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void ModExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ModExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Modulo missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Modulo missing right side expression");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)) {
        SemantError(this, "Invalid type on the left side of a modulo expression");
    }

    if (!AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        SemantError(this, "Invalid type on the right side of a modulo expression");
    }


    //
    // -- Finish up the resuting type and determine if it is a constant
    //    -------------------------------------------------------------
    defTyp = INTEGER->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ModExpr::semant()");
}


//====================================================================================================================
//
// -- Check an exponentiation expression
//
//    An exponentiation expression can only work on real or integer types.  Therefore, the left side of the
//    expression must evaluate to either a real or an interger type.  Furthermore, the expression is a
//    constant expression if and only if both the left and right sides of the expression evaluate to
//    constants.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : INTEGER
//    S |- e2 : INTEGER
//    _______________________
//
//    S |- e1 ** e2 : INTEGER
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : REAL
//    ____________________
//
//    S |- e1 ** e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : INTEGER
//    S |- e2 : REAL
//    ____________________
//
//    S |- e1 ** e2 : REAL
//
//    ---------------------------------
//
//    S |- e1 : REAL
//    S |- e2 : INTEGER
//    ____________________
//
//    S |- e1 ** e2 : REAL
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void ExpExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ExpExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Exponentiation missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Exponentiation missing right side expression");


    //
    // -- Now, that type must be compatible with an integer or real
    //    ---------------------------------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, left->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the left side of a exponentiation expression");
    }

    if (!AreTypesCompatible(this, right->Get_defTyp(), INTEGER) &&
            !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        SemantError(this, "Invalid type on the right side of a exponentiation expression");
    }


    //
    // -- Now, only integer-integer computations compute to integers, otherwise real
    //    --------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER) &&
            AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        defTyp = INTEGER->typeDefinition;
    } else defTyp = REAL->typeDefinition;


    //
    // -- it is a constant if both operands are constants
    //    -----------------------------------------------
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ExpExpr::semant()");
}


//====================================================================================================================
//
// -- Check an greater than comparison
//
//    The comparison operators are a bit funny with types.  Based on section 6.7.2.5, the left and right side
//    must be compatible types, or are set related, or one real and the other integer.  Note that boolean types
//    qualify here and that based on the definition in 6.4.2.2.b), false has a value of 0 and true has a value of 1.
//    Therefore, we can implement a comparison on boolean types as well.
//
//    Later, we will add the operations on types.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : T
//    S |- e2 : T
//    _______________________
//
//    S |- e1 > e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void GTExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering GTExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Greater than comparison missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Greater than comparison missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), right->Get_defTyp())) {
        if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
                && !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
            if (!AreTypesCompatible(this, left->Get_defTyp(), REAL)
                    && !AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
                SemantError(this, "Invalid types in a greater than comparison expression");
            }
        }
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving GTExpr::semant()");
}


//====================================================================================================================
//
// -- Check an greater than or equal to comparison
//
//    The comparison operators are a bit funny with types.  Based on section 6.7.2.5, the left and right side
//    must be compatible types, or are set related, or one real and the other integer.  Note that boolean types
//    qualify here and that based on the definition in 6.4.2.2.b), false has a value of 0 and true has a value of 1.
//    Therefore, we can implement a comparison on boolean types as well.
//
//    Later, we will add the operations on types.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : T
//    S |- e2 : T
//    _______________________
//
//    S |- e1 >= e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void GEExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering GEExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Greater than or equal comparison missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Greater than or equal comparison missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), right->Get_defTyp())) {
        if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
                && !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
            if (!AreTypesCompatible(this, left->Get_defTyp(), REAL)
                    && !AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
                SemantError(this, "Invalid types in a greater than or equal comparison expression");
            }
        }
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving GEExpr::semant()");
}


//====================================================================================================================
//
// -- Check an equal to comparison
//
//    The comparison operators are a bit funny with types.  Based on section 6.7.2.5, the left and right side
//    must be compatible types, or are set related, or one real and the other integer.  Note that boolean types
//    qualify here and that based on the definition in 6.4.2.2.b), false has a value of 0 and true has a value of 1.
//    Therefore, we can implement a comparison on boolean types as well.
//
//    Later, we will add the operations on types.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : T
//    S |- e2 : T
//    _______________________
//
//    S |- e1 = e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void EQExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering EQExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Equal comparison missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Equal comparison missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), right->Get_defTyp())) {
        if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
                && !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
            if (!AreTypesCompatible(this, left->Get_defTyp(), REAL)
                    && !AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
                SemantError(this, "Invalid types in an equal comparison expression");
            }
        }
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving EQExpr::semant()");
}


//====================================================================================================================
//
// -- Check a not equal to comparison
//
//    The comparison operators are a bit funny with types.  Based on section 6.7.2.5, the left and right side
//    must be compatible types, or are set related, or one real and the other integer.  Note that boolean types
//    qualify here and that based on the definition in 6.4.2.2.b), false has a value of 0 and true has a value of 1.
//    Therefore, we can implement a comparison on boolean types as well.
//
//    Later, we will add the operations on types.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : T
//    S |- e2 : T
//    _______________________
//
//    S |- e1 <> e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void NEExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NEExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Not Equal comparison missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Not Equal comparison missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), right->Get_defTyp())) {
        if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
                && !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
            if (!AreTypesCompatible(this, left->Get_defTyp(), REAL)
                    && !AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
                SemantError(this, "Invalid types in a not equal comparison expression");
            }
        }
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NEExpr::semant()");
}


//====================================================================================================================
//
// -- Check an less than comparison
//
//    The comparison operators are a bit funny with types.  Based on section 6.7.2.5, the left and right side
//    must be compatible types, or are set related, or one real and the other integer.  Note that boolean types
//    qualify here and that based on the definition in 6.4.2.2.b), false has a value of 0 and true has a value of 1.
//    Therefore, we can implement a comparison on boolean types as well.
//
//    Later, we will add the operations on types.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : T
//    S |- e2 : T
//    _______________________
//
//    S |- e1 < e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void LTExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering LTExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Less than comparison missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Less than comparison missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), right->Get_defTyp())) {
        if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
                && !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
            if (!AreTypesCompatible(this, left->Get_defTyp(), REAL)
                    && !AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
                SemantError(this, "Invalid types in a less than comparison expression");
            }
        }
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving LTExpr::semant()");
}


//====================================================================================================================
//
// -- Check an greater than or equal to comparison
//
//    The comparison operators are a bit funny with types.  Based on section 6.7.2.5, the left and right side
//    must be compatible types, or are set related, or one real and the other integer.  Note that boolean types
//    qualify here and that based on the definition in 6.4.2.2.b), false has a value of 0 and true has a value of 1.
//    Therefore, we can implement a comparison on boolean types as well.
//
//    Later, we will add the operations on types.
//
//    Therefore, we have several rules we need to check in order to ensure we type check this expression
//    properly.
//
//    S |- e1 : T
//    S |- e2 : T
//    _______________________
//
//    S |- e1 <= e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void LEExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering LEExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Less than or equal comparison missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Less than or equal comparison missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), right->Get_defTyp())) {
        if (!AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
                && !AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
            if (!AreTypesCompatible(this, left->Get_defTyp(), REAL)
                    && !AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
                SemantError(this, "Invalid types in a less than or equal comparison expression");
            }
        }
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving LEExpr::semant()");
}


//====================================================================================================================
//
// -- Check a boolean or expression
//
//    The boolean or operator will only work on boolean operands.  Therefore we need to check these as follows:
//
//    S |- e1 : BOOLEAN
//    S |- e2 : BOOLEAN
//    _______________________
//
//    S |- e1 OR e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void OrExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering OrExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Or expression missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Or expression missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN) &&
            !AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        SemantError(this, "Invalid types in a boolean or expression");
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving OrExpr::semant()");
}


//====================================================================================================================
//
// -- Check a boolean and expression
//
//    The boolean and operator will only work on boolean operands.  Therefore we need to check these as follows:
//
//    S |- e1 : BOOLEAN
//    S |- e2 : BOOLEAN
//    _______________________
//
//    S |- e1 AND e2 : BOOLEAN
//
//    -----------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void AndExpr::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering AndExpr::semant()");


    //
    // -- Perform semantic checks on the left and right operand
    //    -----------------------------------------------------
    if (left) left->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "And expression missing left side expression");

    if (right) right->semant();
    else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "And expression missing right side expression");


    //
    // -- Now, that type must be compatible
    //    ---------------------------------
    if (!AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN) &&
            !AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        SemantError(this, "Invalid types in a boolean and expression");
    }


    //
    // -- Now, finish up the semantic decoration
    //    --------------------------------------
    defTyp = BOOLEAN->typeDefinition;
    isConst = left->Get_isConst() && right->Get_isConst();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving AndExpr::semant()");
}


//====================================================================================================================
//
// -- Check List of new type definitions
//
//    This simple check is there to dispatch a semantic and type check for each type definition in the typedef
//    list.
//
//    The only thing that is worth mentioning is that types can be referenced in type definitions before that have
//    been defined.  Therefore we will need to make 2 passes against the list and pick up all the type names and
//    add them into the symbol table and then a second pass to actually process the semantic definitions.
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void TypeDefList::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering TypeDefList::semant()");


    //
    // -- This is the first pass to collect the names of all the types and adding them into the symbol table
    //    --------------------------------------------------------------------------------------------------
    for (TypeDefList *wrk = this; more(wrk); wrk = next(wrk)) {
        Ident *id = wrk->Get_typeDef()->Get_id();

        if (CheckScope(id->Get_entry())) {
            SemantError(this, "%s is already defined and cannot be redefined as a type", id->GetString());
        } else AddSymbol(id->Get_entry())->SetTypedef();
    }


    //
    // -- This is the second pass to acutally process the type
    //    ----------------------------------------------------
    for (TypeDefList *wrk = this; more(wrk); wrk = next(wrk)) {
        wrk->Get_typeDef()->semant();
    }


    //
    // -- Finally, we look for any forward declarations for pointers.  Only pointers can have a forward type
    //    declaration.  Therefore, it is relatively easy to find these.  To identify a forward type declaration,
    //    we will look for the following:
    //    A) type->Get_AstNodeType() will will return NODE_PointerType
    //    B) We get the FindSymbol(id)->typeDefinition->hostType will be NULL
    //    C) The actual definition type->type->Get_AstNodeType() will be a NODE_NamedType
    //
    //    Therefore, the type can be resolved with (trivialized):
    //			FindSymbol(id)->typeDefinition->hostType = FindSymbol(type->type->type)->typeDefinition
    //
    //    Of course, a lot can go wrong with that logic, so we will need to be cautious.
    //    -------------------------------------------------------------------------------------------------------
    for (TypeDefList *wrk = this; more(wrk); wrk = next(wrk)) {
        TypeDef *typeDef = wrk->Get_typeDef();
        Symbol *sym = FindSymbol(typeDef->Get_id()->Get_entry());


        // -- Check A)
        if (typeDef->Get_AstNodeType() != NODE_PointerType) continue;


        // -- Check B)
        if (!sym) {
            SemantError(this, "Forward Type Definition not complete");
            continue;
        }

        if (!sym->typeDefinition) {
            SemantError(this, "Forward Type Definition has no type record for %s", sym->GetKeyString());
            continue;
        }

        if (sym->typeDefinition->hostType) continue;


        // -- Check C)
        PointerType *pt = typeDef->GetPointerType();
        if (pt->Get_type()->Get_AstNodeType() != NODE_NamedType) {
            SemantError(this, "Forward Type Definition must have a simple named host type %s", sym->GetKeyString());
            continue;
        }


        // -- Make the connection
        NamedType *nt = pt->GetNamedType();
        sym->typeDefinition->hostType = FindSymbol(nt->Get_type()->Get_entry())->typeDefinition;
    }


    //
    // -- And now at the end of the list of types, we want to dump the entire types table.
    //    ---------------------------------------------------------------------------------
    DumpTypeTable();


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving TypeDefList::semant()");
}


//====================================================================================================================
//
// -- Check an individual Type Definition
//
//    Checking a type definition will require some thoughtful planning, since there are a lot if different kinds of
//    new types that need to be checked and evaluated.  In particular, we need to make sure that structured types
//    have offsets for all the different fields.
//
//    There will be a number of fields in the Symbol structure specific to types.
//
//    The general algotithm we will use here will be:
//    1)  Perform a semantic check on the actual definition of the type, collecting all required information as we
//        go (and return).
//    2)  Add the type (and the resulting structure) to the symbol table
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void TypeDef::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering TypeDef::semant()");

    type->semant();
    Symbol *sym = FindSymbol(id->Get_entry())->SetTypedef();
    DefinedType *tp = BuildTypeStructure();

    if (!tp) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "In TypeDef::semant(), BuildTypeStructure() returned NULL");
        goto exit;
    }


    //
    // -- Here we need to determine if we are creating an alias, (renaming a defined type).  This can be a bit of a
    //    challenge since we have no idea where we came from to get here.
    //    ---------------------------------------------------------------------------------------------------------
    if (type->Get_AstNodeType() == NODE_NamedType) {
        DefinedType *wrk = AddType();
        wrk->hostType = tp;
        wrk->kind = TYP_ALIAS;
        tp = wrk;
    }

    tp->typeName = sym->id;			// make sure we name it when we get it back; structure pointer we get back
    sym->SetTypeDefinition(tp);		// might change in the process (in particular ARRAYS)


    Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "Dumping Type Structure after all is said and done");
    DumpTypeRecord(tp);


exit:
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving TypeDef::semant()");
}


//====================================================================================================================
//
// -- Check a pointer type definition
//
//    A pointer type is a pointer to a type that either has already been defined or will be defined in this scope.
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void PointerType::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering PointerType::semant()");

    type->semant();

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving PointerType::semant()");
}


//====================================================================================================================
//
// -- Check a named type definition
//
//    A named type is already defined in the symbol table and should be visible.
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void NamedType::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NamedType::semant()");

    Symbol *sym = FindSymbol(type->Get_entry());
    if (!sym) {
        SemantError(this, "Undefined type symbol in type definition");
    } else if (sym->kind != TYPEDEF) {
        SemantError(this, "Symbol %s does not name a type in a named type reference", sym->GetKeyString());
    }

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NamedType::semant()");
}


//====================================================================================================================
//
// -- Check a subrange type
//
//    OK, so for a little more complication...  A subrange type is a type that is a limited range for an ordinal
//    type (read: integer).  An orderal type has a lower and upper bound (which are inclusive).  These bounds MUST
//    be of the same type and must be in the range of these types.  The lower bound MUST be numerically lower than
//    or equal to the upper bound.  The bounds must also sematically evaluate to a constant.
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
void SubrangeType::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SubrangeType::semant()");


    //
    // -- prepare by performing the semantic checks on the values
    //    -------------------------------------------------------
    fromVal->semant();
    toVal->semant();


    //
    // -- The first thing to do is to evaluate the 2 expressions
    //    ------------------------------------------------------
    lowerBound = fromVal->EvaluateConst();
    upperBound = toVal->EvaluateConst();


    //
    // -- make sure both are constants
    //    ----------------------------
    if (!lowerBound) SemantError(this, "Lower Bound of Subrange type must evaluate to a constant");
    if (!upperBound) SemantError(this, "Upper Bound of Subrange type must evaluate to a constant");
    if (!lowerBound || !upperBound) goto exit;


    //
    // -- Now, make sure we are dealing with an INTEGER (or like) type
    //    ------------------------------------------------------------
    if (!AreTypesCompatible(this, fromVal->Get_defTyp(), INTEGER)) {
        SemantError(this, "Lower Bound of Subrange type must evaluate to a scalar type");
        goto exit;
    }

    if (!AreTypesCompatible(this, toVal->Get_defTyp(), INTEGER)) {
        SemantError(this, "Upper Bound of Subrange type must evaluate to a scalar type");
        goto exit;
    }


    //
    // -- Finally, check that the lower bound is actually lower than the upper bound
    //    --------------------------------------------------------------------------
    if (lowerBound->intVal > upperBound->intVal) {
        SemantError(this, "The Lower Bound in Subrange cannot evaluate to greater than the upper bound");
    }


exit:
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving SubrangeType::semant()");
}


//====================================================================================================================
//
// -- Check an enumerated type
//
//    An emunerated type cannot is a list of identifiers that all have a constant value, starting from 0.  Each of
//    the identifiers cannot have been defined previously.
//    ---------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void EnumeratedType::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering EnumeratedType::semant()");

    IdentList *idList;
    long enumVal = 0;

    for (idList = idents; more(idList); idList = next(idList)) {
        Ident *id = idList->Get_ident();

        if (CheckScope(id->Get_entry())) {
            SemantError(this, "Enumerated identifier %s is already defined in this scope",
                    id->Get_entry()->GetKeyString());
        }

        ConstantValue *cv = new ConstantValue;
        if (!cv) Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory allocating a constant value in enumerated type");

        // cppcheck-suppress memsetClassFloat
        memset(cv, 0, sizeof(ConstantValue));
        cv->intVal = enumVal ++;
        id->Set_cVal(cv);

        Symbol *sym = AddSymbol(id->Get_entry());
        sym->SetIdent()->SetInteger()->SetConst();
        sym->cVal = cv;
    }


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving EnumeratedType::semant()");
}


//====================================================================================================================
//
// -- Check an Array type definition
//
//    Well, this is the first of the more complicated types to check.  The fact that we are dealing with "packed"
//    versus "unpacked" data is one challenge.  Additionally, when we have a multi-dimensional array, we have
//    several ways we can deal with them.  In fact, the following are all considered to be the same (in definition
//    and access method):
//
//    TYPE arr = ARRAY [1 .. 4] OF ARRAY [1 .. 10] OF ARRAY [0 .. 20] OF INTEGER;
//    TYPE arr = ARRAY [1 .. 4, 1 .. 10, 0 .. 20] OF INTEGER;
//    TYPE arr = ARRAY [1 .. 4] OF ARRAY [1 .. 10, 0 .. 20] OF INTEGER;
//    TYPE arr = ARRAY [1 .. 4, 1 .. 10] OF ARRAY [0 .. 20] OF INTEGER;
//
//    In fact, the first representation is considered to be "full form" and is the way we need to store the
//    representation in the symbol table; while the the other representations need to be converted into this full
//    form for the symbol table.
//
//    The index range will be an unnamed subrange type, with all the limitations of a subrange.  The component type
//    with itself be another DefinedType structure.
//
//    To complicate matters, an ARRAY OF CHAR is actually a STRING type.
//    --------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void ArrayType::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ArrayType::semant()");

    //
    // -- First we check the indices
    //    --------------------------
    for (IndexTypeList *typeList = indices; more(typeList); typeList = next(typeList)) {
        typeList->Get_type()->semant();
    }


    //
    // -- Then we check the component type
    //    --------------------------------
    type->semant();


    //
    // -- We will perform most of this checking in the BuildTypeStructure() function
    //    --------------------------------------------------------------------------
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ArrayType::semant()");
}


//====================================================================================================================
//
// -- Check a file definition
//
//    This is relatively easy to check considering that the host type of the file must be already defined.
//    --------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void FileOfType::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering FileOfType::semant()");



    //
    // -- Then we check the component type
    //    --------------------------------
    type->semant();


    //
    // -- We will perform most of this checking in the BuildTypeStructure() function
    //    --------------------------------------------------------------------------
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving FileOfType::semant()");
}


//====================================================================================================================
//
// -- Check a record type definition
//
//    Records are the most complicated structured type to check.  We need to check each field in the record AND
//    the names must be unique.
//    --------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
void RecordType::semant(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering RecordType::semant()");


    //
    // -- Check the components of the record definition
    //    ---------------------------------------------
    for (RecordSectionList *wrk = fields; more(wrk); wrk = next(wrk)) {
        RecordSection *rec = wrk->Get_recSec();
        rec->semant();
    }
    if (variant) variant->semant();


    //
    // -- At most, 1 of the 2 above can be NULL; in no cases can they both be NULL
    //    ------------------------------------------------------------------------
    if (!fields && !variant) {
        SemantError(this, "In record definition, both the field list and variant list are NULL");
    }


    //
    // -- We will perform most of this checking in the BuildTypeStructure() function
    //    --------------------------------------------------------------------------
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving RecordType::semant()");
}


