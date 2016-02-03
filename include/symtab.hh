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


struct Symbol;
class Expr;
class IdentList;

extern Symbol *BOOLEAN;
extern Symbol *CHAR;
extern Symbol *INTEGER;
extern Symbol *REAL;
extern Symbol *STRING;


//
// -- The following functions are the interface for the Symbol table
//    --------------------------------------------------------------
void EnterScope(void);
struct Symbol *FindSymbol(IdentEntry *entry);
struct Symbol *AddSymbol(IdentEntry *entry);
bool CheckScope(IdentEntry *entry);
void ExitScope(void);

void DumpSymbol(Symbol *sym);


//
// -- These are the types we know and will be able to support
//    -------------------------------------------------------
typedef enum {
	TYP_UNKNOWN,
	TYP_BASE_TYPE,
	TYP_ALIAS,
	TYP_POINTER,
	TYP_SUBRANGE,
	TYP_ENUMERATION,
} TypeKind;


//
// -- This structure keeps track of custom types that are defined within the source file
//    ----------------------------------------------------------------------------------
typedef struct DefinedType {
	struct Symbol *typeSym;						// the name if this type; may be NULL if this is an anonymous type
	TypeKind kind;
	struct Symbol *hostType;					// the parent type when type is really a subtype (like enums or alias)
	
	//
	// -- The following will be kind-specific information we need to track
	//    ----------------------------------------------------------------
	union {		
		// -- TYP_SUBRANGE			
		struct {
			long lowerBound;					// in a subrange type, this is the lower bound of the allowed vals
			long upperBound;					// in a subrange type, this is the upper bound of the allowed vals
		};
		
		// -- TYP_ENUMERATION
		struct {
			IdentList *enumList;				// This is a list of enumerated constants
		};			
	};
} DefinedType;


//
// -- These are the kinds of symbols we will be tracking
//    --------------------------------------------------
typedef enum {
	UNKNOWN,
	PROGRAM,
	LABEL,
	PROCEDURE,
	FUNCTION,
	TYPEDEF,
	IDENT,
	LITERAL,
	MEMADDR,
} SymbolKind;


//
// -- This is a symbol in the symbol table
//    ------------------------------------
typedef struct Symbol {
	IdentEntry *id;
	SymbolKind kind;
	bool isConst;
	bool isPacked;
	Symbol *typeSym;
	int elemCount;
	DefinedType *typeDefinition;
	Symbol *next;
	
	union ConstantValue *cVal;
	
	const char *GetKeyString(void) const { return id?id->GetKeyString():"(null)"; }

	Symbol *SetProgram(void) { kind = PROGRAM; return this; }
	Symbol *SetLabel(void) { kind = LABEL; return this; }
	Symbol *SetProcedure(void) { kind = PROCEDURE; return this; }
	Symbol *SetFunction(void) { kind = FUNCTION; return this; }
	Symbol *SetTypedef(void) { kind = TYPEDEF; return this; }
	Symbol *SetIdent(void) { kind = IDENT; return this; }
	Symbol *SetLiteral(void) { kind = LITERAL; return this; }
	Symbol *SetPointer(void) { kind = MEMADDR; return this; }
	
	Symbol *SetBool(void) { typeSym = BOOLEAN; return this; }
	Symbol *SetString(void) { typeSym = STRING; return this; }
	Symbol *SetInteger(void) { typeSym = INTEGER; return this; }
	Symbol *SetReal(void) { typeSym = REAL; return this; }
	Symbol *SetChar(void) { typeSym = CHAR; return this; }
	
	Symbol *SetConst(void) { isConst = true; return this; }
	
	Symbol *SetTypeDefinition(DefinedType *t) { typeDefinition = t; return this; }
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


void DumpTypeRecord(DefinedType *tp);