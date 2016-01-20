//===================================================================================================================
//
// semant.cc -- this file is the implementation of the semantic checker for the pascal-cc compiler
//
// The particular checks are dependent on the environment as defined at the point we reach the semantic check in
// question.  The environment is represented by the symbol table S, which is the list of defined symbols in the
// environment at that point of the tree.
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/18  Initial   v0.0    ADCL  Initial version
//
//===================================================================================================================


#include "debug.hh"
#include "pascal.hh"
#include "symtab.hh"

#include <cstdarg>
#include <cstdlib>

int semantErrors = 0;

void SemantError(const char *msg, ...)
{
	va_list args;
	va_start(args, msg);
	vfprintf(stderr, msg, args);
	va_end(args);
	
	if (++ semantErrors > 50) {
		fprintf(stderr, "More than 50 errors\n");
		exit(EXIT_FAILURE);
	} 
}


//
// -- This is the default semantic check function -- which simply reports an error and returns
//    ----------------------------------------------------------------------------------------
void Common::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_INFO, "The semant() function has not been implemented for AST tree node type %d",
			Get_AstNodeType());
}


//
// -- Check a SourceFile object
//
//    Performing a semantic check on a file consists of the following actions:
//    1. opening a symbol table scope
//    2. if we have a ProgramHeading, we need to check it (if not, we have a module)
//    3. checking the code (if the code does not exist, we have an error)
//    4. exis the symbol table scope
//    ------------------------------------------------------------------------------
void SourceFile::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering SourceFile::semant()");

	EnterScope();

	if (pgmHdg) pgmHdg->semant();
	if (code) code->semant();
	else SemantError("A program must have a block of statements");

	ExitScope();

	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving SourceFile::semant()");
}


//
// -- Check a ProgramHeading object
//
//    Performing a semantic check on a ProgramHeading node consists of the following:
//    1.  Add the program name to the symbol table as a function
//    2.  Check each parm name in the symbol table to see if it is duplicate
//    3.  Add each parm name to the symbol table
//    4.  Each parm has the type string and is a constant
//    -------------------------------------------------------------------------------
void ProgramHeading::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering ProgramHeading::semant()");


	//
	// -- Add the program name into the symbol table
	//    ------------------------------------------
	if (CheckScope(name->Get_entry())) {
		SemantError ("Program name %s already defined in the symbol table", name->GetString());
	} else {
		Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding program name %s to the identifiers table", name->GetString());
		AddSymbol(name->Get_entry())->SetProgram();
	}


	//
	// -- Now, add each parameter into the symbol table and force the types
	//    -----------------------------------------------------------------
	for (IdentList *l = parms; more(l); l = next(l)) {
		if (CheckScope(l->Get_entry())) {
			SemantError ("Parameter name %s defined more than once", l->GetString());
		} else {
			Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding parameter name %s to the identifiers table", l->GetString());
			AddSymbol(l->Get_entry())->SetString()->SetConst();
		}
	}

	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving ProgramHeading::semant()");
}


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
//    --------------------------------------------------------------------------------------------------------
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
	else SemantError("A block needs at least 1 excutable statement");
	
	ExitScope();
	
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving Block::semant()");
}


//
// -- Check Labels
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
//    ------------------------------------------------------------------------------------------------------------
void LabelList::semant(void)
{
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Entering LabelList::semant()");
	
	LabelList *wrk;
	
	for (wrk = this; more(wrk); wrk = next(wrk)) {
		if (CheckScope(wrk->Get_label()->Get_entry())) {
			Symbol *sym = FindSymbol(wrk->Get_label()->Get_entry());
			
			if (sym->type == LABEL) SemantError("Label %s already defined for this scope", sym->id->GetKeyString());
			else SemantError("Label %s id already defined as some other meaning", sym->id->GetKeyString()); 
		} else {
			Log(DebugLog::LOG_SEMANT, LOG_DEBUG, "Adding symbol for label %s", 
					wrk->Get_label()->Get_entry()->GetKeyString());
					
			AddSymbol(wrk->Get_label()->Get_entry())->SetLabel();
		}
	} 
	
	Log(DebugLog::LOG_SEMANT, LOG_ENTRY, "Leaving LabelList::semant()");
}
