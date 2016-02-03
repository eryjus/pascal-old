//===================================================================================================================
//
// symtab.cc -- implementation of the symbol table
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/18  Initial   v0.0    ADCL  Initial version
//
//===================================================================================================================

#include "symtab.hh"
#include "debug.hh"
#include "pascal.hh"

#include <cstring>


//
// -- This is the actual symbol table
//    -------------------------------
Scope *symbolTable = NULL;


//
// -- Create a new scope for the symbol table
//    ---------------------------------------
void EnterScope(void)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering EnterScope(void)");

	Scope *s = new Scope;
	if (!s) Fatal(DebugLog::LOG_SYM, LOG_SEVERE, "Out of memory in EnterScope()");

	s->syms = NULL;
	s->next = symbolTable;
	symbolTable = s;

	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Leaving EnterScope(void)");
}


//
// -- Find a symbol somewhere in the symbol table
//    -------------------------------------------
Symbol *FindSymbol(IdentEntry *entry)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering FindSymbol(IdentEntry *id)");
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  id = %p (%s)", entry, entry->GetKeyString());

	Symbol *sym = NULL;
	Scope *sc = symbolTable;
	Symbol *wrk;

	while (sc) {
		wrk = sc->syms;

		while (wrk) {
			if (wrk->id == entry) {
				sym = wrk;
				goto exit;
			}

			wrk = wrk->next;
		}

		sc = sc->next;
	}

exit:
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  return value = %p", sym);
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Leaving FindSymbol(IdentEntry *id)");

	return sym;
}


//
// -- Add a symbol to the symbol table
//    --------------------------------
Symbol *AddSymbol(IdentEntry *entry)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering AddSymbol(IdentEntry *id)");
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  id = %p (%s)", entry, entry->GetKeyString());
	
	if (entry == NULL) return NULL;

	Symbol *sym = new Symbol;
	if (!symbolTable) Fatal(DebugLog::LOG_SYM, LOG_SEVERE, "No scope defined before adding a symbol");
	if (!sym) Fatal(DebugLog::LOG_SYM, LOG_SEVERE, "Out of memory in AddSymbol()");
	
	memset(sym, 0, sizeof(Symbol));
	
	sym->id = entry;
	sym->isConst = false;
	sym->isPacked = false;
	sym->kind = UNKNOWN;
	sym->typeSym = NULL;
	sym->elemCount = 0;			// > 0 means it is an array
	sym->next = symbolTable->syms;
	symbolTable->syms = sym;

	Log(DebugLog::LOG_SYM, LOG_PARMS, "  return value = %p", sym);
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Leaving AddSymbol(IdentEntry *id)");

	return sym;
}


//
// -- Check the top scope to see if a symbol exists
//    ---------------------------------------------
bool CheckScope(IdentEntry *entry)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering CheckScope(IdentEntry *id)");
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  id = %p (%s)", entry, entry->GetKeyString());

	bool rv = false;
	Symbol *wrk;

	if (!symbolTable) goto exit;
	wrk = symbolTable->syms;

	while (wrk) {
		if (wrk->id == entry) {
			rv = true;
			goto exit;
		}

		wrk = wrk->next;
	}

exit:
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  return value = %s", rv?"true":"false");
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Leaving CheckScope(IdentEntry *id)");

	return rv;
}


//
// -- Exit a Scope, freeing all the symbols in that scope
//    ---------------------------------------------------
void ExitScope(void)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering ExitScope(void)");

	Scope *sc = symbolTable;
	if (!sc) Fatal(DebugLog::LOG_SYM, LOG_SEVERE, "No scope defined before exiting a scope");
	symbolTable = symbolTable->next;
	Symbol *wrk = sc->syms;

	while(wrk) {
		Symbol *n = wrk->next;
		delete wrk;
		wrk = n;
	}

	delete sc;

	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Leaving ExitScope(void)");
}


//
// -- Dump a symbol's data to the log
//    -------------------------------
void DumpSymbol(Symbol *sym)
{
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "**************************************************");
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "Dumping Symbol at address at %p", sym);
	
	if (!sym) return;
	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "   Symbol name..: %s", sym->GetKeyString());	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "   Symbol kind..: %d", sym->kind);	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "   Symbol const.: %s", sym->isConst?"true":"false");	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "   Symbol packed: %s", sym->isPacked?"true":"false");	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "   Symbol type..: %p", sym->typeSym);	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "**************************************************");
}


//
// -- This function is used to print a type record from the type table.  Keep in mind that a type record is 
//    actually a suite of structures.
//    -----------------------------------------------------------------------------------------------------
void DumpTypeRecord(DefinedType *tp)
{
	char *desc [] = {
		"TYP_UNKNOWN",
		"TYP_BASE_TYPE",
		"TYP_ALIAS",
		"TYP_POINTER",
		"TYP_SUBRANGE",
		"TYP_ENUMERATION",
	};
	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "===========================================================================");
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "Dumping type record located at %p", tp);
	
	if (!tp) return;
	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "  typesym : %p", tp->typeSym);
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "  name    : %s", (tp->typeSym?tp->typeSym->GetKeyString():"(null symbol)"));
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "  kind    : %d (%s)", tp->kind, desc[tp->kind]);
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "  hostType: %s", 
			(tp->hostType?tp->hostType->GetKeyString():"(null reference type)"));
	
	switch(tp->kind) {
	case TYP_BASE_TYPE:
	case TYP_POINTER:
	case TYP_ALIAS:
		break;
			
	case TYP_SUBRANGE:
		Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "  L-bound : %ld", tp->lowerBound);
		Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "  U-bound : %ld", tp->upperBound);
		break; 
		
	case TYP_ENUMERATION:
		if (!tp->enumList) break;
		for (IdentList *idList = tp->enumList; more(idList); idList = next(idList)) {
			Symbol *sym = FindSymbol(idList->Get_ident()->Get_entry());
			 
			Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "  Ident   : %s; value: %ld", sym?sym->GetKeyString():"(null symbol)", 
				sym&&sym->cVal?sym->cVal->intVal:0);
		}
		
		break;
			
	default:
		Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "DumpTypeRecord() is missing some output details!");
		break;
	}
	
	Log(DebugLog::LOG_SYM, LOG_HIDEBUG, "===========================================================================");
}


