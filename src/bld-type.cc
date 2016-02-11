//===================================================================================================================
//
// bld-type.cc -- This file is used to construct the internal structures used to define a type.
//
// This file is an extension to the semantic checks that are performed during the compile.  These functions take
// up enough lines that it makes sense to separate these and put them together in their own file.
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----------  -------  -------  ----  ---------------------------------------------------------------------------
// 2016-01-29  Initial   v0.0    ADCL  Initial version
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

#include <cstring>


void SemantError(Common *node, const char *msg, ...);


//====================================================================================================================
//
// -- This function is a catch-all function to report AST node types with unimplemented functions.  Under
//    normal circumstances, this function will not be called.
//    ---------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262      v0.0   Change how the type system is managed in the compiler.
//
//====================================================================================================================
DefinedType *Type::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_INFO,
            RED HI "The BuildTypeStructure() function has not been implemented for AST tree node type %d" RESET,
            Get_AstNodeType());
    return NULL;
}


//====================================================================================================================
//
// -- Since a TypeDef AST node is a type name, and then a defined type.  Since we are interested in building
//    structures to hold the defined type, we will only process the "right" side of this equation.
//    ------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262      v0.0   Change how the type system is managed in the compiler.
//
//====================================================================================================================
DefinedType *TypeDef::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering TypeDef::BuildTypeStructure()");

    DefinedType *rv = type->BuildTypeStructure();

    if (!rv) {
        Log(DebugLog::LOG_SEMANT, LOG_ERROR, "In TypeDef::BuildTypeStructure(), BuildTypeStructure() returned NULL");
        goto exit;
    }

    if (rv->kind == TYP_UNKNOWN) rv->kind = TYP_ALIAS;

exit:
    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", rv);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving TypeDef::BuildTypeStructure()");
    return rv;
}


//====================================================================================================================
//
// -- A pointer type is nothing more than an address that holds the location of a type.  With that, we need
//    to take some time to make sure we have the symbol table and type table up to date with what we know.
//
//    Since the "operand" of the pointer might be a further type definition, we need to continue to look to
//    prepare the type table.
//    ------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262      v0.0   Change how the type system is managed in the compiler.
//
//====================================================================================================================
DefinedType *PointerType::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering PointerType::BuildTypeStructure()");

    DefinedType *tmp = type->BuildTypeStructure();
    DefinedType *rv = AddType();
    rv->kind = TYP_POINTER;
    rv->hostType = tmp;

    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", rv);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving PointerType::BuildTypeStructure()");
    return rv;
}


//====================================================================================================================
//
// -- A named type is part of the most simple type definition -- it refers to a type we already know about.
//    ------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262      v0.0   Change how the type system is managed in the compiler.
//
//====================================================================================================================
DefinedType *NamedType::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NamedType::BuildTypeStructure()");

    DefinedType *rv = NULL;
    Symbol *sym = FindSymbol(type->Get_entry());

    if (!sym) {
        SemantError(this, "Symbol %s is not found in the symbol table, when it should be",
                type->Get_entry()->GetKeyString());
        goto exit;
    }

    if (sym->kind != TYPEDEF) {
        SemantError(this, "Symbol %s is not a defined type", sym->GetKeyString());
        goto exit;
    }


    rv = sym->typeDefinition;

    Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "Dumping data for named type symbol %s", type->Get_entry()->GetKeyString());
    DumpSymbol(sym);


exit:
    Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "Dumping Type Record while leaving NamedType::BuildTypeStructure()");
    DumpTypeRecord(rv);

    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", rv);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NamedType::BuildTypeStructure()");
    return rv;
}


//====================================================================================================================
//
// -- A subrange type is a named type with just a portion of its host type as allowed values
//    --------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262      v0.0   Change how the type system is managed in the compiler.
//
//====================================================================================================================
DefinedType *SubrangeType::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SubrangeType::BuildTypeStructure()");

    DefinedType *rv = AddType();
    if (!rv) Fatal(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory defining a subrange type");

    rv->kind = TYP_SUBRANGE;
    rv->hostType = fromVal->Get_defTyp();
    rv->lowerBound = lowerBound->intVal;
    rv->upperBound = upperBound->intVal;

    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", rv);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving SubrangeType::BuildTypeStructure()");
    return rv;
}


//====================================================================================================================
//
// -- An enumerated type is a list of constants that all are treated as the same type
//    -------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262      v0.0   Change how the type system is managed in the compiler.
//
//====================================================================================================================
DefinedType *EnumeratedType::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering EnumeratedType::BuildTypeStructure()");

    DefinedType *rv = AddType();
    if (!rv) Fatal(DebugLog::LOG_SEMANT, LOG_ERROR, "Out of memory defining an enumerated type");

    rv->kind = TYP_ENUMERATION;
    rv->hostType = INTEGER->typeDefinition;
    rv->enumList = idents;

    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", rv);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving EnumeratedType::BuildTypeStructure()");
    return rv;
}


//====================================================================================================================
//
// -- Well, this is the first of the more complicated types to check.  The fact that we are dealing with "packed"
//    versus "unpacked" data is one challenge.  Additionally, when we have a multi-dimensional array, we have
//    several ways we can deal with them.  In fact, the following are all considered to be the same (in definition
//    and access method):
//
//    TYPE arr = ARRAY [1 .. 4] OF ARRAY [1 .. 10] OF ARRAY [0 .. 20] OF INTEGER;
//    TYPE arr = ARRAY [1 .. 4, 1 .. 10, 0 .. 20] OF INTEGER;
//    TYPE arr = ARRAY [1 .. 4] OF ARRAY [1 .. 10, 0 .. 20] OF INTEGER;
//    TYPE arr = ARRAY [1 .. 4, 1 .. 10] OF ARRAY [0 .. 20] OF INTEGER;
//
//    Since we delayed most of the checking until this point, we need to make sure everything is good with this type.
//    The first representation is considered to be "full form" and is the way we need to store the representation
//    in the symbol table; while the the other representations need to be converted into this full form for the
//    symbol table.
//
//    The index range will be an unnamed subrange type, with all the limitations of a subrange.  The component type
//    with itself be another DefinedType structure.
//
//    To complicate matters, an ARRAY OF CHAR is actually a STRING type.
//
//    Finally, we are handed the resulting type at the top level since it contains the
//    --------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//    2016-02-08  ADCL   #262      v0.0   Change how the type system is managed in the compiler.
//
//====================================================================================================================
DefinedType *ArrayType::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ArrayType::BuildTypeStructure()");


    //
    // -- First we build and/or use the array component type; we will need this to pass down into the index type
    //    evaluation functions
    //    ------------------------------------------------------------------------------------------------------
    DefinedType *compType = type->BuildTypeStructure();

    Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "Dumping the type of the array component", compType);
    DumpTypeRecord(compType);


    //
    // --  Now, with this information, we will go through the array indexes to build each type in the long form
    //     ----------------------------------------------------------------------------------------------------
    DefinedType *arr = indices->BuildTypeStructure();


    //
    // -- Finally, we need to add the component type to the last array in the chain
    //    -------------------------------------------------------------------------
    DefinedType *wrk = arr;
    while (wrk->componentType) wrk = wrk->componentType;

    wrk->componentType = compType;


    DumpTypeRecord(arr);

    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", arr);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ArrayType::BuildTypeStructure()");
    return arr;
}


//====================================================================================================================
//
// -- For each index type in the list, we will recurse to build the array type-of from right to left.
//
//    So, with any given iteration of this we will end up with 3 separate DefinedType instances:
//    1)  The component type of each element of the array
//    2)  The subrange controlling the index of the array
//    3)  The actual type of the array with the index and component type combination -- it is this instance
//        that will be returned
//
//    Based on how the AST is build, we get the ultimate component type as a parameter..  we need to keep track of
//    that!
//    --------------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
DefinedType *IndexTypeList::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering IndexTypeList::BuildTypeStructure()");

    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Building the unnamed index type");
    DefinedType *compType = NULL;


    //
    // -- First we need to get to the last index definition of the array type.  We will get back a component
    //    type from this function.  In particular, we will execute this block when there are 2 or more indexes
    //    in the array type, and as such the compnent types all all the other will be ARRAY [type] OF something.
    //    Note that when there is only 1 index or the right-most index, the compType will be left NULL.  This
    //    will be key in the calling function when we go to set the ultimate component type.
    //    ------------------------------------------------------------------------------------------------------
    IndexTypeList *n = next(this);
    if (n) {
        Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "This is not the last index in the list; checking next index");
        compType = n->BuildTypeStructure();
        compType->typeName = idTable.AddUnnamedIdent();
        compType->kind = TYP_ARRAY;
    }


    // -- Now, we want to look at the index that we have to deal with.  This might be the right-most index or
    //    it could be any of the others.  However, the index type will need to be processed the same way regardless.
    //    ----------------------------------------------------------------------------------------------------------
    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Building the unnamed index type");
    DefinedType *idxTyp = type->BuildTypeStructure();
    if (type->Get_AstNodeType() == NODE_NamedType) {
        DefinedType *wrk = AddType();
        wrk->hostType = idxTyp;
        wrk->kind = TYP_ALIAS;
        idxTyp = wrk;
    } else idxTyp->kind = TYP_SUBRANGE;

    idxTyp->typeName = idTable.AddUnnamedIdent();
    DumpTypeRecord(idxTyp);


    //
    // -- Finally, ragardless of what we have going in as we return, we need to return an ARRAY type.  So, we build
    //    this here.  Remember that there is a chance (and will happen once each array type) that compType is
    //    actually NULL.
    //    ---------------------------------------------------------------------------------------------------------
    Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Now, creating the type record");
    DefinedType *rv = AddType();
    rv->indexType = idxTyp;
    rv->componentType = compType;
    rv->kind = TYP_ARRAY;
    DumpTypeRecord(rv);


    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", rv);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving IndexTypeList::BuildTypeStructure()");
    return rv;
}


//====================================================================================================================
//
// -- A file type is actually a persistent storage class of a specific type.
//    ------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
DefinedType *FileOfType::BuildTypeStructure(void)
{
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering FileOfType::BuildTypeStructure()");

    DefinedType *tmp = type->BuildTypeStructure();
    DefinedType *rv = AddType();
    rv->kind = TYP_FILEOF;
    rv->hostType = tmp;

    Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", rv);
    Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving FileOfType::BuildTypeStructure()");
    return rv;
}


