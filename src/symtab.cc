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
Symbol *FindSymbol(Entry *id)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering FindSymbol(IdentEntry *id)");
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  id = %p (%s)", id, id->GetKeyValue().c_str());

	Symbol *sym;
	Scope *sc = symbolTable;
	Symbol *wrk;

	while (sc) {
		wrk = sc->syms;

		while (wrk) {
			if (wrk->id == id) {
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
Symbol *AddSymbol(Entry *id)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering AddSymbol(IdentEntry *id)");
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  id = %p (%s)", id, id->GetKeyValue().c_str());

	Symbol *sym = new Symbol;
	if (!symbolTable) Fatal(DebugLog::LOG_SYM, LOG_SEVERE, "No scope defined before adding a symbol");
	if (!sym) Fatal(DebugLog::LOG_SYM, LOG_SEVERE, "Out of memory in AddSymbol()");

	sym->id = id;
	sym->next = symbolTable->syms;
	sym->isConst = false;
	sym->type = UNKNOWN;
	symbolTable->syms = sym;

	Log(DebugLog::LOG_SYM, LOG_PARMS, "  return value = %p", sym);
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Leaving AddSymbol(IdentEntry *id)");

	return sym;
}


//
// -- Check the top scope to see if a symbol exists
//    ---------------------------------------------
bool CheckScope(Entry *id)
{
	Log(DebugLog::LOG_SYM, LOG_ENTRY, "Entering CheckScope(IdentEntry *id)");
	Log(DebugLog::LOG_SYM, LOG_PARMS, "  id = %p (%s)", id, id->GetKeyValue().c_str());

	bool rv = false;
	Symbol *wrk;

	if (!symbolTable) goto exit;
	wrk = symbolTable->syms;

	while (wrk) {
		if (wrk->id == id) {
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


