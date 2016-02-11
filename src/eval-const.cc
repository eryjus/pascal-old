//===================================================================================================================
//
// eval-const.cc -- This file is the implementation of the functions to evaluate a constant expression value
//
// This file is an extension to the semantic checks that are performed during the compile.  These functions take
// up enough lines that it makes sense to separate these and put them together in their own file.
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----------  -------  -------  ----  ---------------------------------------------------------------------------
// 2016-01-25  Initial   v0.0    ADCL  Initial version
// 2016-02-08   #262     v0.0    ADCL  In the process of changing how the type system is built, we have sevaral
//                                     changes in this file to accomodate the changes.
// 2016-02-08   #259     v0.0    ADCL  Since I have to touch nearly every method, I will make sure we are
//                                     comparing compatible types.
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
#include <cmath>


//
// -- Some functions from the semant.cc that we will leverage here in this file.
//    --------------------------------------------------------------------------
extern void SemantError(Common *node, const char *msg, ...);


//
// -- Quick function to duplicate a constant value structure
//    ------------------------------------------------------
ConstantValue *dup(ConstantValue *src)
{
    if (!src) return NULL;
    ConstantValue *tgt = new ConstantValue;
    if (!tgt) return NULL;
    memmove(tgt, src, sizeof(ConstantValue));

    return tgt;
}


//
// -- This is a default function that will be a catch-all for evaluating a constant value.  Ideally, this
//    function will never be executed.
//    ---------------------------------------------------------------------------------------------------
ConstantValue *Expr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_INFO,
            RED HI "The EvaluateConst() function has not been implemented for AST tree node type %d" RESET,
            Get_AstNodeType());

    return NULL;
}


//====================================================================================================================
//
// -- Evaluate a unary identity expression
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
ConstantValue *PosExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering PosExpr::EvaluateConst()");

    cVal = dup(operand->EvaluateConst());

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving PosExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate a unary negation expression
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//    2016-02-08  ADCL   #259     v0.0    Since I have to touch nearly every method, I will make sure we are
//                                        comparing compatible types.
//
//====================================================================================================================
ConstantValue *NegExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NegExpr::EvaluateConst()");

    cVal = dup(operand->EvaluateConst());

    if (cVal) {
        if (AreTypesCompatible(this, defTyp, INTEGER)) cVal->intVal = -cVal->intVal;
        else if (AreTypesCompatible(this, defTyp, REAL)) cVal->realVal = -cVal->realVal;
    }

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NegExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate a boolean not expression
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
ConstantValue *NotExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NotExpr::EvaluateConst()");

    cVal = dup(operand->EvaluateConst());

    if (cVal) {
        if (cVal->boolVal) cVal->boolVal = false;
        else cVal->boolVal = true;
    }

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NotExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an identifier's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
ConstantValue *Ident::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering Ident::EvaluateConst()");

    Symbol *sym = FindSymbol(entry);
    if (!sym) {
        SemantError(this, "Identifier %s in constant expression does not exist", entry->GetKeyString());
        return NULL;
    }

    if (!sym->isConst) {
        SemantError(this, "Identifier %s in constant expression is not a constant",  entry->GetKeyString());
    }

    cVal = dup(sym->cVal);

    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving Ident::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an subtraction expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *SubExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SubExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating a subtraction constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in subtraction");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant
    //    -------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->intVal = l->intVal - r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->realVal = (double)l->intVal - r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->realVal = l->realVal - (double)r->intVal;
    } else cVal->realVal = l->realVal - r->realVal;


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving SubExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an addition expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *AddExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering AddExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating an addition constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in addition");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant
    //    -------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->intVal = l->intVal + r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->realVal = (double)l->intVal + r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->realVal = l->realVal + (double)r->intVal;
    } else cVal->realVal = l->realVal + r->realVal;


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving AddExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an multiplication expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *MulExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering MulExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating an addition constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in addition");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant
    //    -------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->intVal = l->intVal * r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->realVal = (double)l->intVal * r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->realVal = l->realVal * (double)r->intVal;
    } else cVal->realVal = l->realVal * r->realVal;


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving MulExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an real number division expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *RealDivExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering RealDivExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating an real division constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in real division");
        return NULL;
    }


    //
    // -- make sure the right-side of the expression does not evaluate to a 0 constant value
    //    ----------------------------------------------------------------------------------
    if (AreTypesCompatible(this, right->Get_defTyp(), INTEGER) && r->intVal == 0) {
        SemantError(this, "Division by 0 in constant evaluation");
        cVal->realVal = 0.0;
        goto exit;
    } else if (AreTypesCompatible(this, right->Get_defTyp(), REAL) && r->realVal == 0.0) {
        SemantError(this, "Division by 0 in constant evaluation");
        cVal->realVal = 0.0;
        goto exit;
    }


    //
    // -- All sanity checks are done, evaluate the constant
    //    -------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->realVal = (double)l->intVal / (double)r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->realVal = (double)l->intVal / r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->realVal = l->realVal / (double)r->intVal;
    } else cVal->realVal = l->realVal / r->realVal;


exit:
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving RealDivExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an integer number division expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
ConstantValue *IntDivExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering IntDivExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating an integer division constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in integer division");
        return NULL;
    }


    //
    // -- make sure the right-side of the expression does not evaluate to a 0 constant value
    //    ----------------------------------------------------------------------------------
    if (r->intVal == 0) {
        SemantError(this, "Division by 0 in constant evaluation");
        cVal->intVal = 0;
        goto exit;
    }


    //
    // -- All sanity checks are done, evaluate the constant
    //    -------------------------------------------------
    cVal->intVal = l->intVal / r->intVal;


exit:
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving IntDivExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate a modulo expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
ConstantValue *ModExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ModExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating an modulo constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in modulo");
        return NULL;
    }


    //
    // -- make sure the right-side of the expression does not evaluate to a 0 constant value
    //    ----------------------------------------------------------------------------------
    if (r->intVal == 0) {
        SemantError(this, "Division by 0 in constant evaluation");
        cVal->intVal = 0;
        goto exit;
    }


    //
    // -- All sanity checks are done, evaluate the constant
    //    -------------------------------------------------
    cVal->intVal = l->intVal % r->intVal;


exit:
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ModExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an exponentiation expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *ExpExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ExpExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating an exponentiation constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in exponentiation");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant
    //    -------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->intVal = (long)pow((double)l->intVal, (double)r->intVal);
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->realVal = pow((double)l->intVal, r->realVal);
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->realVal = pow(l->realVal, (double)r->intVal);
    } else cVal->realVal = pow(l->realVal, r->realVal);


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ExpExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an greater than expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *GTExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering GTExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating a greater than comparison constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in greater than comparison");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.  But this is not as simple as it seems, since there
    //    are several types that can be utilized in this comparison.  Therefore, based on the type, we will
    //    perform one of several types of comparison.  We already know that we have comparible types from the
    //    semant() function.
    //
    //    So, there are several conditions where we can make this comparison:
    //    * BOOLEAN (or compatible) types
    //    * STRING (or compatible) types
    //    * One side is an INTEGER and the other is a REAL
    //    ------------------------------------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN)
            && AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        cVal->boolVal = (int)l->boolVal > (int)r->boolVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), STRING)
            && AreTypesCompatible(this, right->Get_defTyp(), STRING)) {
        cVal->boolVal = (strcmp(l->strVal, r->strVal) > 0);
    }  else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->intVal > r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = (double)l->intVal > r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->realVal > (double)r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = l->realVal > r->realVal;
    } else SemantError(this, "Incompatible types in Greater than comparison");


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving GTExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an greater than or equal to expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *GEExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering GEExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR,
                "Out of memory evaluating a greater than or equal comparison constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in greater than or equal comparison");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.  But this is not as simple as it seems, since there
    //    are several types that can be utilized in this comparison.  Therefore, based on the type, we will
    //    perform one of several types of comparison.  We already know that we have comparible types from the
    //    semant() function.
    //
    //    So, there are several conditions where we can make this comparison:
    //    * BOOLEAN (or compatible) types
    //    * STRING (or compatible) types
    //    * Set related types
    //    * One side is an INTEGER and the other is a REAL
    //    ------------------------------------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN)
            && AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        cVal->boolVal = (int)l->boolVal >= (int)r->boolVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), STRING)
            && AreTypesCompatible(this, right->Get_defTyp(), STRING)) {
        cVal->boolVal = (strcmp(l->strVal, r->strVal) >= 0);
    }

    // -- set related stuff should go here.... (notice the else part below)
    else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->intVal >= r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = (double)l->intVal >= r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->realVal >= (double)r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = l->realVal >= r->realVal;
    } else SemantError(this, "Incompatible types in Greater than or equal comparison");


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving GEExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an equal to expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *EQExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering EQExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR,
                "Out of memory evaluating a equal comparison constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in equal comparison");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.  But this is not as simple as it seems, since there
    //    are several types that can be utilized in this comparison.  Therefore, based on the type, we will
    //    perform one of several types of comparison.  We already know that we have comparible types from the
    //    semant() function.
    //
    //    So, there are several conditions where we can make this comparison:
    //    * BOOLEAN (or compatible) types
    //    * STRING (or compatible) types
    //    * Set related types
    //    * One side is an INTEGER and the other is a REAL
    //    ------------------------------------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN)
            && AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        cVal->boolVal = (int)l->boolVal == (int)r->boolVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), STRING)
            && AreTypesCompatible(this, right->Get_defTyp(), STRING)) {
        cVal->boolVal = (strcmp(l->strVal, r->strVal) == 0);
    }

    // -- set related stuff should go here.... (notice the else part below)
    else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->intVal == r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = (double)l->intVal == r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->realVal == (double)r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = l->realVal == r->realVal;
    } else SemantError(this, "Incompatible types in equality comparison");


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving EQExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate a not equal to expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *NEExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NEExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR,
                "Out of memory evaluating a not equal comparison constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in not equal comparison");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.  But this is not as simple as it seems, since there
    //    are several types that can be utilized in this comparison.  Therefore, based on the type, we will
    //    perform one of several types of comparison.  We already know that we have comparible types from the
    //    semant() function.
    //
    //    So, there are several conditions where we can make this comparison:
    //    * BOOLEAN (or compatible) types
    //    * STRING (or compatible) types
    //    * Set related types
    //    * One side is an INTEGER and the other is a REAL
    //    ------------------------------------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN)
            && AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        cVal->boolVal = (int)l->boolVal != (int)r->boolVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), STRING)
            && AreTypesCompatible(this, right->Get_defTyp(), STRING)) {
        cVal->boolVal = (strcmp(l->strVal, r->strVal) != 0);
    }

    // -- set related stuff should go here.... (notice the else part below)
    else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->intVal != r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = (double)l->intVal != r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->realVal != (double)r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = l->realVal != r->realVal;
    } else SemantError(this, "Incompatible types in inequality comparison");


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NEExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an less than expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *LTExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering LTExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory evaluating a less than comparison constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in less than comparison");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.  But this is not as simple as it seems, since there
    //    are several types that can be utilized in this comparison.  Therefore, based on the type, we will
    //    perform one of several types of comparison.  We already know that we have comparible types from the
    //    semant() function.
    //
    //    So, there are several conditions where we can make this comparison:
    //    * BOOLEAN (or compatible) types
    //    * STRING (or compatible) types
    //    * One side is an INTEGER and the other is a REAL
    //    ------------------------------------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN)
            && AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        cVal->boolVal = (int)l->boolVal < (int)r->boolVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), STRING)
            && AreTypesCompatible(this, right->Get_defTyp(), STRING)) {
        cVal->boolVal = (strcmp(l->strVal, r->strVal) < 0);
    }  else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->intVal < r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = (double)l->intVal > r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->realVal < (double)r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = l->realVal < r->realVal;
    } else SemantError(this, "Incompatible types in Less than comparison");


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving LTExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate an less than or equal to expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262     v0.0    Change how the type system is managed in the compiler.
//
//====================================================================================================================
ConstantValue *LEExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering LEExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR,
                "Out of memory evaluating a less than or equal comparison constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in less than or equal comparison");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.  But this is not as simple as it seems, since there
    //    are several types that can be utilized in this comparison.  Therefore, based on the type, we will
    //    perform one of several types of comparison.  We already know that we have comparible types from the
    //    semant() function.
    //
    //    So, there are several conditions where we can make this comparison:
    //    * BOOLEAN (or compatible) types
    //    * STRING (or compatible) types
    //    * Set related types
    //    * One side is an INTEGER and the other is a REAL
    //    ------------------------------------------------------------------------------------------------------
    if (AreTypesCompatible(this, left->Get_defTyp(), BOOLEAN)
            && AreTypesCompatible(this, right->Get_defTyp(), BOOLEAN)) {
        cVal->boolVal = (int)l->boolVal <= (int)r->boolVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), STRING)
            && AreTypesCompatible(this, right->Get_defTyp(), STRING)) {
        cVal->boolVal = (strcmp(l->strVal, r->strVal) <= 0);
    }

    // -- set related stuff should go here.... (notice the else part below)
    else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->intVal <= r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), INTEGER)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = (double)l->intVal <= r->realVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), INTEGER)) {
        cVal->boolVal = l->realVal <= (double)r->intVal;
    } else if (AreTypesCompatible(this, left->Get_defTyp(), REAL)
            && AreTypesCompatible(this, right->Get_defTyp(), REAL)) {
        cVal->boolVal = l->realVal <= r->realVal;
    } else SemantError(this, "Incompatible types in Less than or equal comparison");


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving LEExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate a boolean or expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
ConstantValue *OrExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering OrExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR,
                "Out of memory evaluating a boolean or expression constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in boolean or expression");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.
    //    --------------------------------------------------
    cVal->boolVal = l->boolVal || r->boolVal;


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving OrExpr::EvaluateConst()");

    return cVal;
}


//====================================================================================================================
//
// -- Evaluate a boolean and expression's constant value
//
//    ------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
ConstantValue *AndExpr::EvaluateConst(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering AndExpr::EvaluateConst()");


    //
    // -- evaluate the subtrees constant value
    //    ------------------------------------
    ConstantValue *l = left->EvaluateConst();
    ConstantValue *r = right->EvaluateConst();
    cVal = new ConstantValue;


    //
    // -- make sure we do not have a memory allocation problem
    //    ----------------------------------------------------
    if (!cVal) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR,
                "Out of memory evaluating a boolean and expression constant value");
        return NULL;
    }


    //
    // -- Make sure we got a legitimate structure pointer
    //    -----------------------------------------------
    if (!l || !r) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value structure is NULL in boolean and expression");
        return NULL;
    }


    //
    // -- All sanity checks are done, evaluate the constant.
    //    --------------------------------------------------
    cVal->boolVal = l->boolVal && r->boolVal;


    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "   returning %p", cVal);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving AndExpr::EvaluateConst()");

    return cVal;
}


