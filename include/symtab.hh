//===================================================================================================================
//
// symtab.hh -- classes and definitions for the symbol table
//
// This symbol table is implemented nicely as a stack of scopes, each scope having its own stack of symbols.
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/18  Initial   v0.0    ADCL  Initial version
//
//===================================================================================================================


#include "stringtab.hh"


typedef enum {
	UNKNOWN,
	PROGRAM,
	LABEL,
	PROCEDURE,
	FUNCTION,
	STRING,
	INTEGER,
	REAL,
} SymbolType;


//
// -- This is a symbol in the symbol table
//    ------------------------------------
typedef struct Symbol {
	Entry *id;
	SymbolType type;
	bool isConst;
	struct Symbol *next;

	Symbol *SetLabel() {type = LABEL; return this; }
	Symbol *SetProgram() { type = PROGRAM; return this; }
	Symbol *SetString() { type = STRING; return this; }

	Symbol *SetConst() { isConst = true; return this; }
} Symbol;


//
// -- This is a scope of symbols
//    --------------------------
typedef struct Scope {
	Symbol *syms;
	struct Scope *next;
} Scope;


//
// -- This is the symbol table -- make it globally available
//    ------------------------------------------------------
extern Scope *symbolTable;


//
// -- The following functions are the interface for the Symbol table
//    --------------------------------------------------------------
void EnterScope(void);
Symbol *FindSymbol(Entry *sym);
Symbol *AddSymbol(Entry *sym);
bool CheckScope(Entry *sym);
void ExitScope(void);




