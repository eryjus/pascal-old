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
//
//====================================================================================================================
bool AreTypesCompatible(Common *node, Symbol *T1, Symbol *T2)
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
		return false;
	}
	
	if (T1->kind != TYPEDEF || T2->kind != TYPEDEF) {
		Log(DebugLog::LOG_SEMANT, LOG_SEVERE, "T1 and/or T2 are not type symbols");
		return false;
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
	if (T1->typeSym == T2) {
		Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... T1 is a subtype of T2");
		 goto exit;
	}
		
	if (T2->typeSym == T1) {
		Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "... T2 is a subtype of T1");
		goto exit;
	}
		
	if (T1->typeSym && T2->typeSym && T1->typeSym == T2->typeSym) {
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
	if (T1 == FindSymbol(idTable.lookup("string")) && T2 == FindSymbol(idTable.lookup("string"))) {
		if (T1->elemCount == T2->elemCount) goto exit;
	}

	
	//
	// -- Now, if we get here, they are not compatible types
	//    --------------------------------------------------
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
//
//====================================================================================================================
void SourceFile::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SourceFile::semant()");
	EnterScope();
	
	//
	// -- Add the global language constants
	//    ---------------------------------
	Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding Global Symbols for the types");
	BOOLEAN = AddSymbol(idTable.lookup("boolean"))->SetTypedef();
	STRING = AddSymbol(idTable.lookup("string"))->SetTypedef();
	CHAR = AddSymbol(idTable.lookup("char"))->SetTypedef();
	INTEGER = AddSymbol(idTable.lookup("integer"))->SetTypedef();
	REAL = AddSymbol(idTable.lookup("real"))->SetTypedef();
	POINTER = AddSymbol(idTable.lookup("^"))->SetTypedef();
	
	Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding Global Symbols for language defined constants");
	MAXINT = AddSymbol(idTable.lookup("maxint"))->SetIdent()->SetInteger()->SetConst();
	NIL = AddSymbol(idTable.lookup("^"))->SetPointer()->SetConst();
	FALSE = AddSymbol(idTable.lookup("false"))->SetIdent()->SetBool()->SetConst();
	TRUE = AddSymbol(idTable.lookup("true"))->SetIdent()->SetBool()->SetConst();

	Log(DebugLog::LOG_SEMANT, LOG_HIDEBUG, "FALSE == %p", FALSE);


	FALSE->cVal = new ConstantValue;
	FALSE->cVal->boolVal = false;
	
	TRUE->cVal = new ConstantValue;
	TRUE->cVal->boolVal = true;
	
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
		
		return; 
	} 
	
	
	//
	// -- here begins step 4...
	//    ---------------------
	if (val) {
		val->semant();
		
		if (!sym) {
			Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding symbol for constant %s", id->Get_entry()->GetKeyString());			
			sym = AddSymbol(id->Get_entry())->SetIdent();
		}
			
		if (!val->Get_isConst()) {
			SemantError(this, "Constant expression does not evaluate to a constant");
			
			return;
		}
		
		if (val->Get_type() == BOOLEAN || val->Get_type() == CHAR || val->Get_type() == INTEGER 
				|| val->Get_type() == REAL) {
			sym->cVal = val->EvaluateConst();
		} else if (val->Get_type() == STRING || val->Get_type() == NULL) {
		} else SemantError(this, "Constant expression does not evaluate to a base type");
		
		sym->typeSym = val->Get_type();
		sym->isConst = val->Get_isConst();
		
		Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "The resulting type from constant evaluation of %s is: %p (%s)", 
				sym->GetKeyString(), sym->typeSym, sym->typeSym?sym->typeSym->GetKeyString():"(NULL)");
		
		Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Assigning Constant Value to a Constant Definition");
	} else Log(DebugLog::LOG_SEMANT, LOG_ERROR, "Constant value missing");
	
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
//
//====================================================================================================================
void NilLit::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering NilLit::semant()");
	
	isConst = true;
	isPointer = true;
	type = NULL;
	
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
//
//====================================================================================================================
void RealLit::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering RealLit::semant()");
	
	isConst = true;
	type = REAL;

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
//
//====================================================================================================================
void IntLit::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering IntLit::semant()");
	
	isConst = true;
	type = INTEGER;

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
//
//====================================================================================================================
void StringLit::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering StringLit::semant()");
	
	isConst = true;
	type = STRING;

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
			type = NULL;
			SemantError(this, "Program name '%s' cannot be used as an identifier", sym->GetKeyString());
			break;
			
		case LABEL:
			type = NULL;
			SemantError(this, "Label '%s' cannot be used as an identifier", sym->GetKeyString());
			break;
			
		case PROCEDURE:
			type = NULL;
			SemantError(this, "Procedure name '%s' cannot be used as an identifier", sym->GetKeyString());
			break;
			
		case FUNCTION:
			type = NULL;
			SemantError(this, "Function name '%s' cannot be used as an identifier", sym->GetKeyString());
			break;
			
		case IDENT:
			Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Evaluating Identifier");

			isConst = sym->isConst;
			
			if (AreTypesCompatible(this, sym->typeSym, BOOLEAN)) type = BOOLEAN;
			else if (AreTypesCompatible(this, sym->typeSym, INTEGER)) type = INTEGER;
			else if (AreTypesCompatible(this, sym->typeSym, REAL)) type = REAL;
			else if (AreTypesCompatible(this, sym->typeSym, STRING)) type = STRING;
			else if (AreTypesCompatible(this, sym->typeSym, CHAR)) type = CHAR;
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
	if (!AreTypesCompatible(this, operand->Get_type(), INTEGER) 
			&& !AreTypesCompatible(this, operand->Get_type(), REAL)) {
		SemantError(this, "Invalid type for unary identity expression operand");
	}
	
	
	//
	// -- Set up the flags for the symbol
	//    -------------------------------
	isConst = operand->Get_isConst();
	type = operand->Get_type();

	
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
	if (!AreTypesCompatible(this, operand->Get_type(), INTEGER) 
			&& !AreTypesCompatible(this, operand->Get_type(), REAL)) {
		SemantError(this, "Invalid type for unary negation expression operand");
	}
	
	
	//
	// -- Set up the flags for the symbol
	//    -------------------------------
	isConst = operand->Get_isConst();
	type = operand->Get_type();
	
		
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
	if (!AreTypesCompatible(this, operand->Get_type(), BOOLEAN)) {
		SemantError(this, "Invalid type for boolean not expression operand");
	}
	
	
	//
	// -- Set up the flags for the symbol
	//    -------------------------------
	isConst = operand->Get_isConst();
	type = operand->Get_type();
	
		
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
	if (!AreTypesCompatible(this, left->Get_type(), INTEGER) &&!AreTypesCompatible(this, left->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the left side of a subtraction expression");
	}


	if (!AreTypesCompatible(this, right->Get_type(), INTEGER) && !AreTypesCompatible(this, right->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the right side of a subtraction expression");
	}

	//
	// -- Now, only integer-integer computations compute to integers, otherwise real
	//    --------------------------------------------------------------------------
	if (AreTypesCompatible(this, left->Get_type(), INTEGER) && AreTypesCompatible(this, right->Get_type(), INTEGER)) {
		 type = INTEGER;
	} else type = REAL;
	
	
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
	if (!AreTypesCompatible(this, left->Get_type(), INTEGER) &&!AreTypesCompatible(this, left->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the left side of a addition expression");
	}
	
	if (!AreTypesCompatible(this, right->Get_type(), INTEGER) && !AreTypesCompatible(this, right->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the right side of a addition expression");
	}


	//
	// -- Now, only integer-integer computations compute to integers, otherwise real
	//    --------------------------------------------------------------------------
	if (AreTypesCompatible(this, left->Get_type(), INTEGER) && AreTypesCompatible(this, right->Get_type(), INTEGER)) {
		 type = INTEGER;
	} else type = REAL;
	
	
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
	if (!AreTypesCompatible(this, left->Get_type(), INTEGER) &&!AreTypesCompatible(this, left->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the left side of a multiplication expression");
	}
	
	if (!AreTypesCompatible(this, right->Get_type(), INTEGER) && !AreTypesCompatible(this, right->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the right side of a multiplication expression");
	}


	//
	// -- Now, only integer-integer computations compute to integers, otherwise real
	//    --------------------------------------------------------------------------
	if (AreTypesCompatible(this, left->Get_type(), INTEGER) && AreTypesCompatible(this, right->Get_type(), INTEGER)) {
		 type = INTEGER;
	} else type = REAL;
	
	
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
	if (!AreTypesCompatible(this, left->Get_type(), INTEGER) &&!AreTypesCompatible(this, left->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the left side of a real division expression");
	}
	
	if (!AreTypesCompatible(this, right->Get_type(), INTEGER) && !AreTypesCompatible(this, right->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the right side of a real division expression");
	}


	//
	// -- Finish up the resuting type and determine if it is a constant
	//    -------------------------------------------------------------
	type = REAL;
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
	if (!AreTypesCompatible(this, left->Get_type(), INTEGER)) {
		SemantError(this, "Invalid type on the left side of a integer division expression");
	}
	
	if (!AreTypesCompatible(this, right->Get_type(), INTEGER)) {
		SemantError(this, "Invalid type on the right side of a integer division expression");
	}


	//
	// -- Finish up the resuting type and determine if it is a constant
	//    -------------------------------------------------------------
	type = INTEGER;
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
	if (!AreTypesCompatible(this, left->Get_type(), INTEGER)) {
		SemantError(this, "Invalid type on the left side of a modulo expression");
	}
	
	if (!AreTypesCompatible(this, right->Get_type(), INTEGER)) {
		SemantError(this, "Invalid type on the right side of a modulo expression");
	}


	//
	// -- Finish up the resuting type and determine if it is a constant
	//    -------------------------------------------------------------
	type = INTEGER;
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
	if (!AreTypesCompatible(this, left->Get_type(), INTEGER) &&!AreTypesCompatible(this, left->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the left side of a exponentiation expression");
	}
	
	if (!AreTypesCompatible(this, right->Get_type(), INTEGER) && !AreTypesCompatible(this, right->Get_type(), REAL)) {
		SemantError(this, "Invalid type on the right side of a exponentiation expression");
	}


	//
	// -- Now, only integer-integer computations compute to integers, otherwise real
	//    --------------------------------------------------------------------------
	if (AreTypesCompatible(this, left->Get_type(), INTEGER) && AreTypesCompatible(this, right->Get_type(), INTEGER)) {
		 type = INTEGER;
	} else type = REAL;
	
	
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
	if (!AreTypesCompatible(this, left->Get_type(), right->Get_type())) {
		if (!AreTypesCompatible(this, left->Get_type(), INTEGER) 
				&& !AreTypesCompatible(this, right->Get_type(), REAL)) {
			if (!AreTypesCompatible(this, left->Get_type(), REAL) 
					&& !AreTypesCompatible(this, right->Get_type(), INTEGER)) {
				SemantError(this, "Invalid types in a greater than comparison expression");
			}
		}
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
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
	if (!AreTypesCompatible(this, left->Get_type(), right->Get_type())) {
		if (!AreTypesCompatible(this, left->Get_type(), INTEGER) 
				&& !AreTypesCompatible(this, right->Get_type(), REAL)) {
			if (!AreTypesCompatible(this, left->Get_type(), REAL) 
					&& !AreTypesCompatible(this, right->Get_type(), INTEGER)) {
				SemantError(this, "Invalid types in a greater than or equal comparison expression");
			}
		}
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
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
	if (!AreTypesCompatible(this, left->Get_type(), right->Get_type())) {
		if (!AreTypesCompatible(this, left->Get_type(), INTEGER) 
				&& !AreTypesCompatible(this, right->Get_type(), REAL)) {
			if (!AreTypesCompatible(this, left->Get_type(), REAL) 
					&& !AreTypesCompatible(this, right->Get_type(), INTEGER)) {
				SemantError(this, "Invalid types in an equal comparison expression");
			}
		}
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
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
	if (!AreTypesCompatible(this, left->Get_type(), right->Get_type())) {
		if (!AreTypesCompatible(this, left->Get_type(), INTEGER) 
				&& !AreTypesCompatible(this, right->Get_type(), REAL)) {
			if (!AreTypesCompatible(this, left->Get_type(), REAL) 
					&& !AreTypesCompatible(this, right->Get_type(), INTEGER)) {
				SemantError(this, "Invalid types in a not equal comparison expression");
			}
		}
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
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
	if (!AreTypesCompatible(this, left->Get_type(), right->Get_type())) {
		if (!AreTypesCompatible(this, left->Get_type(), INTEGER) 
				&& !AreTypesCompatible(this, right->Get_type(), REAL)) {
			if (!AreTypesCompatible(this, left->Get_type(), REAL) 
					&& !AreTypesCompatible(this, right->Get_type(), INTEGER)) {
				SemantError(this, "Invalid types in a less than comparison expression");
			}
		}
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
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
	if (!AreTypesCompatible(this, left->Get_type(), right->Get_type())) {
		if (!AreTypesCompatible(this, left->Get_type(), INTEGER) 
				&& !AreTypesCompatible(this, right->Get_type(), REAL)) {
			if (!AreTypesCompatible(this, left->Get_type(), REAL) 
					&& !AreTypesCompatible(this, right->Get_type(), INTEGER)) {
				SemantError(this, "Invalid types in a less than or equal comparison expression");
			}
		}
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
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
	if (!AreTypesCompatible(this, left->Get_type(), BOOLEAN) && 
			!AreTypesCompatible(this, right->Get_type(), BOOLEAN)) {
		SemantError(this, "Invalid types in a boolean or expression");
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
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
	if (!AreTypesCompatible(this, left->Get_type(), BOOLEAN) && 
			!AreTypesCompatible(this, right->Get_type(), BOOLEAN)) {
		SemantError(this, "Invalid types in a boolean and expression");
	}


	//
	// -- Now, finish up the semantic decoration
	//    --------------------------------------
	type = BOOLEAN;
	isConst = left->Get_isConst() && right->Get_isConst();

	
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving AndExpr::semant()");
}


