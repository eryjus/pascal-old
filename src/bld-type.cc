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
//
//===================================================================================================================


#include "debug.hh"
#include "symtab.hh"
#include "pascal.hh"


//====================================================================================================================
//
// -- This function is a catch-all function to report AST node types with unimplemented functions.  Under 
//    normal circumstances, this function will not be called.
//    ---------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
DefinedType *Type::BuildTypeStructure(DefinedType *tp __attribute__((unused)))
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
//
//====================================================================================================================
DefinedType *TypeDef::BuildTypeStructure(DefinedType *tp)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering TypeDef::BuildTypeStructure()");
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  tp = %p", tp);
	
	tp = type->BuildTypeStructure(tp);
			
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", tp);
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving TypeDef::BuildTypeStructure()");
	return tp;
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
//
//====================================================================================================================
DefinedType *PointerType::BuildTypeStructure(DefinedType *tp)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering PointerType::BuildTypeStructure()");
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  tp = %p", tp);
	
	type->BuildTypeStructure(tp);
	tp->kind = TYP_POINTER;
			
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", tp);
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving PointerType::BuildTypeStructure()");
	return tp;
}


//====================================================================================================================
//
// -- A named type is part of the most simple type definition -- it refers to a type we already know about.
//    ------------------------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
DefinedType *NamedType::BuildTypeStructure(DefinedType *tp)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NamedType::BuildTypeStructure()");
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  tp = %p", tp);
	
	tp->hostType = FindSymbol(type->Get_entry());
	tp->kind = TYP_ALIAS;
			
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", tp);
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving NamedType::BuildTypeStructure()");
	return tp;
}


//====================================================================================================================
//
// -- A subrange type is a named type with just a portion of its host type as allowed values
//    --------------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
DefinedType *SubrangeType::BuildTypeStructure(DefinedType *tp)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SubrangeType::BuildTypeStructure()");
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  tp = %p", tp);
	
	tp->kind = TYP_SUBRANGE;
	tp->hostType = fromVal->Get_type();
	tp->lowerBound = lowerBound->intVal;
	tp->upperBound = upperBound->intVal;
			
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", tp);
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving SubrangeType::BuildTypeStructure()");
	return tp;
}


//====================================================================================================================
//
// -- An enumerated type is a list of constants that all are treated as the same type
//    -------------------------------------------------------------------------------
//
//      Date      Pgmr  Tracker  Version  Description
//    ----------  ----  -------  -------  --------------------------------------------------------------------------
//
//====================================================================================================================
DefinedType *EnumeratedType::BuildTypeStructure(DefinedType *tp)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering EnumeratedType::BuildTypeStructure()");
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  tp = %p", tp);
	
	tp->kind = TYP_ENUMERATION;
	tp->hostType = INTEGER;
	tp->enumList = idents;

			
	Log(DebugLog::LOG_SEMANT, LOG_PARMS, "  return value = %p", tp);
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving EnumeratedType::BuildTypeStructure()");
	return tp;
}

