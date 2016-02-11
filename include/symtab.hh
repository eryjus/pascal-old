//===================================================================================================================
//
// symtab.hh -- classes and definitions for the symbol table
//
// This symbol table is implemented nicely as a stack of scopes, each scope having its own stack of symbols.
//
// So, after a lot of consideration and false starts, I have determiend that the list of DefinedTypes needs to be
// incorporated into the scope definition where it can be maintained concurrently.  However, the difference
// between symbols and defined types is that the defined type information needs to persist beyond the scope since
// it will need to be used for emitting code.
//
// Therefore, ExitScope() will not remove the Defined types.  For that reason, persistence will be maintained
// by attaching a DefinedType into the AST strucutre as the the type.
//
// This is going to be a massive change to the work that has been completed.  The best implementation will need
// to be considered.
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/18  Initial   v0.0    ADCL  Initial version
// 2016-02-08   #262     v0.0    ADCL  In the process of changing how the type system is built, we have sevaral
//                                     changes in this file to accomodate the changes.
// 2016-02-10            v0.0    ADCL  You might notice a change in MOST of this source file.  These changes are
//                                     due to changing how the file is intended (using spaces rather than tabs).
//                                     In addition, trailing spaces have been removed.
//
//===================================================================================================================


#include "stringtab.hh"


struct Symbol;
struct DefinedType;
class Common;
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
// -- The following functions are the interface for the Types table
//    -------------------------------------------------------------
struct DefinedType *FindType(IdentEntry *entry);
struct DefinedType *AddType(IdentEntry *entry = NULL);

void DumpTypeRecord(DefinedType *tp);
void DumpTypeTable(void);


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
    TYP_FILEOF,
    TYP_RECORD,
    TYP_ARRAY,
} TypeKind;


//
// -- This structure keeps track of custom types that are defined within the source file
//    ----------------------------------------------------------------------------------
typedef struct DefinedType {
    IdentEntry *typeName;                       // the name of this type; can be used to lookup the symbol if in scope
    TypeKind kind;
    struct DefinedType *hostType;               // the parent type when type is really a subtype (like enums or alias)
    bool isPacked;

    //
    // -- The following will be kind-specific information we need to track
    //    ----------------------------------------------------------------
    union {
        // -- TYP_SUBRANGE
        struct {
            long lowerBound;                    // in a subrange type, this is the lower bound of the allowed vals
            long upperBound;                    // in a subrange type, this is the upper bound of the allowed vals
        };

        // -- TYP_ENUMERATION
        struct {
            IdentList *enumList;                // This is a list of enumerated constants
        };

        // -- TYP_ARRAY
        struct {
            struct DefinedType *indexType;      // See section 6.4.3.2....  Either or both of these symbols can be
            struct DefinedType *componentType;  // unnamed types (meaning they are not specifically defined and named)
        };
    };

    struct DefinedType *next;

    const char *GetKeyString(void) const { return typeName?typeName->GetKeyString():"(NULL)"; }
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
// -- This is the symbol table -- make it globally available; include the types table
//    -------------------------------------------------------------------------------
extern Scope *symbolTable;
extern DefinedType *allTypes;


//
// -- Finally, an inline or two to make coding simpler
//    ------------------------------------------------
extern bool AreTypesCompatible(Common *node, DefinedType *T1, DefinedType *T2);

inline DefinedType *AddType(Symbol *sym) { return AddType(sym->id); }

inline bool AreTypesCompatible(Common *node, Symbol *T1, DefinedType *T2) {
    return AreTypesCompatible(node, T1->typeDefinition, T2);
}

inline bool AreTypesCompatible(Common *node, DefinedType *T1, Symbol *T2) {
    return AreTypesCompatible(node, T1, T2->typeDefinition);
}

inline bool AreTypesCompatible(Common *node, Symbol *T1, Symbol *T2) {
    return AreTypesCompatible(node, T1->typeDefinition, T2->typeDefinition);
}



