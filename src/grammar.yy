/*=================================================================================================================*/
/*                                                                                                                 */
/* grammar.yy -- This is the bison grammar file for the pascal language.                                           */
/*                                                                                                                 */
/* --------------------------------------------------------------------------------------------------------------- */
/*                                                                                                                 */
/*    Date     Tracker  Version  Pgmr  Modification                                                                */
/* ----------  -------  -------  ----  --------------------------------------------------------------------------- */
/* 2016-01-09  Initial   v0.0    ADCL  Initial version -- leveraged from the pascal compiler, but made C++         */
/* 2016-01-19  Initial   v0.0    ADCL  Adding error checking to the scanner (which requires tokens here)           */
/*                                                                                                                 */
/*=================================================================================================================*/


%{
	#include "pascal.hh"
	#include "debug.hh"
	#include "stringtab.hh"

	#include <string>
	#include <cstring>
	#include <cstdlib>

	int yylex(void);
	int yyerror (const char *s);

	extern char *yytext;

	#define ACCEPT(m)		Log(DebugLog::LOG_YACC, LOG_ENTRY, m)
	#define YYERROR_VERBOSE

	class Expr;

	extern SourceFile *ast;
%}


/*
 * -- yystype union
 *    -------------
 */
%union {
	char *errMsg;
	int token;
	IdentEntry *id;
	NumberEntry *nbr;
	StringEntry *strVal;

	ArrayType *arrayType;
	Block *block;
	CaseConst *caseConst;
	CaseConstList *caseConstList;
	CaseElement *caseElement;
	CaseElemList *caseElemList;
	ConstAssign *constAssign;
	ConstList *constList;
	Ident *ident;
	Expr *expr;
	ExprList *exprList;
	FormalParm *formalParm;
	FormalParmList *formalParmList;
	FuncHdr *funcHdr;
	IdentList *idList;
	IndexTypeList *idxList;
	Label *label;
	LabelList *labelList;
	MemberDesignator *mbrDesignator;
	MemberDesignatorList *mbrDesignatorList;
	SubrDecl *subrDecl;
	SubrDeclList *subrDeclList;
	ProcHdr *procHdr;
	ProgramHeading *pgmHdg;
	RecordSection *recSec;
	RecordSectionList *recSecList;
	RecordType *recordType;
	SourceFile *srcFile;
	Stmt *stmt;
	StmtList *stmtList;
	StructuredType *structType;
	Type *type;
	TypeDef *typeDef;
	TypeDefList *typeDefList;
	VarAccess *varAccess;
	VarAccessList *varAccessList;
	VarDecl *var;
	VarDeclList *varDeclList;
	Variant *variant;
	VariantList *variantList;
	VariantPart *variantPart;
	VariantSelector *varSel;
}


/*
 * -- tokens used to communicate with the scanner
 *    -------------------------------------------
 */
%token					T_DOT				"dot (.)"
%token					T_COMMA				"comma (,)"
%token					T_SEMI				"semi-colon (;)"
%token					T_COLON				"colon (:)"
%token					T_LPAREN			"left paren"
%token					T_RPAREN			"right paren"
%token					T_PLUS				"plus sign (+)"
%token					T_MINUS				"minus sign (-)"
%token					T_EQUAL				"equal sign (=)"
%token					T_UPARROW			"up arrow (^)"
%token					T_STARSTAR			"star-star (**)"

%token					T_CONST				"CONST keyword"
%token					T_FORWARD			"FORWARD directive"
%token					T_FUNCTION			"FUNCTION keyword"
%token					T_LABEL				"LABEL keyword"
%token					T_OR				"OR keyword"
%token					T_PROCEDURE			"PROCEDURE keyword"
%token					T_PROGRAM			"PROGRAM keyword"
%token					T_TYPE				"TYPE keyword"
%token					T_VAR				"VAR keyword"

%token	<id>			T_ID				"identifier"
%token	<nbr>			T_INTEGER			"integer number"
%token	<strVal>		T_STRING			"string"
%token	<nbr>			T_REALNUMBER		"real number"

%token					T_AND				"AND keyword"
%token					T_ARRAY				"ARRAY keyword"
%token					T_ASSIGN			"assignment (:=)"
%token					T_BEGIN				"BEGIN keyword"
%token					T_CASE				"CASE keyword"
%token					T_DIV				"DIV keyword"
%token					T_DO				"DO keyword"
%token					T_DOTDOT			"dot-dot (..)"
%token					T_DOWNTO			"DOWNTO keyword"
%token					T_FILE				"FILE keyword"
%token					T_GOTO				"GOTO keyword"
%token					T_NIL				"NIL keyword"
%token					T_NOT				"NOT keyword"
%token					T_NOTEQUAL			"not equal (<>)"
%token					T_OF				"OF keyword"
%token					T_PACKED			"PACKED keyword"
%token					T_RECORD			"RECORD keyword"
%token					T_REPEAT			"REPEAT keyword"
%token					T_RSQUARE			"right square bracket (])"
%token					T_SET				"SET keyword"
%token					T_SLASH				"slash character (/)"
%token					T_STAR				"star character (*)"
%token					T_THEN				"THEN keyword"
%token					T_GT				"greater than comparison (>)"
%token					T_GE				"greater than or equal to comparison (>=)"
%token					T_IF				"IF keyword"
%token					T_IN				"IN keyword"
%token					T_LT				"less than comparison (<)"
%token					T_LE				"less than or equal to comparison (<=)"
%token					T_LSQUARE			"left square bracket ([)"
%token					T_MOD				"MOD keyword"
%token					T_TO				"TO keyword"
%token					T_UNTIL				"UNTIL keyword"
%token					T_WHILE				"WHILE keyword"
%token					T_WITH				"WITH keyword"
%token					T_ELSE				"ELSE keyword"
%token					T_END				"END keyword"
%token					T_FOR				"FOR keyword"
%token					T_EXTERN			"EXTERN directive"
%token					T_OTHERWISE			"OTHERWISE keyword"

%token					T_ERROR

%type	<expr>			ActualParameter CaseIndex InitialValue SetConstructor
%type	<expr>			CExponentiation CExpression CFactor CPrimary CSimpleExpression CTerm
%type	<expr>			Exponentiation Expression Factor FunctionDesignator Primary SimpleExpression Term
%type	<expr>			Constant NonString FinalValue IndexExpression
%type	<expr>			BooleanExpression UnsignedConstant UnsignedInteger UnsignedNumber UnsignedReal

%type	<stmt>			AssignmentStatement CaseStatement ClosedWithStatement OpenWithStatement OpenForStatement
%type	<stmt>			ClosedStatement GotoStatement NonLabeledClosedStatement NonLabeledOpenStatement
%type	<stmt>			OpenStatement ProcedureStatement RepeatStatement Statement ClosedForStatement
%type	<stmt>			ClosedIfStatement OpenIfStatement ClosedWhileStatement OpenWhileStatement

%type	<type>			BaseType ComponentType DomainType EnumeratedType IndexType
%type 	<type>			NewOrdinalType NewPointerType NewStructuredType NewType OrdinalType
%type	<type>			SubrangeType TypeDenoter

%type	<block>			Block FunctionBlock ProcedureBlock Module
%type	<caseConst>		CaseConstant
%type	<caseConstList>	CaseConstantList
%type	<caseElement>	CaseListElement
%type	<caseElemList>	CaseListElementList
%type	<constAssign>	ConstantDefinition
%type	<constList>		ConstantList ConstantDefinitionPart
%type   <exprList>		ActualParameterList IndexExpressionList Parms
%type	<formalParm>	FormalParameterSection FunctionalParameterSpecification ProceduralParameterSpecification
%type	<formalParm> 	ValueParameterSpecification VariableParameterSpecification
%type	<formalParmList> FormalParameterList FormalParameterSectionList
%type	<funcHdr>		FunctionHeading
%type	<ident>			ControlVariable Identifier ProcedureIdentification ResultType TagField TagType
%type 	<idList>		IdentifierList
%type	<idxList>		IndexList
%type 	<label>			Label
%type	<labelList>		LabelList LabelDeclarationPart
%type	<mbrDesignator> MemberDesignator
%type	<mbrDesignatorList>	MemberDesignatorList
%type	<pgmHdg>		ProgramHeading
%type	<procHdr>		ProcedureHeading
%type 	<recSec>		RecordSection
%type	<recSecList>	RecordSectionList
%type	<recordType>	RecordType
%type 	<srcFile>		File Program
%type	<stmtList>		CompoundStatement StatementPart StatementSequence
%type	<structType>	ArrayType FileType SetType StructuredType
%type	<subrDecl>		FunctionDeclaration ProcedureDeclaration ProcOrFuncDeclaration
%type	<subrDeclList>	ProcedureAndFunctionDeclarationPart ProcOrFuncDeclarationList
%type	<token>			AddOp MulOp RelOp Sign Directive Direction
%type	<typeDef>		TypeDefinition
%type	<typeDefList>	TypeDefinitionList TypeDefinitionPart
%type	<var>			VariableDeclaration
%type	<varAccess>		FieldDesignator IndexedVariable VariableAccess
%type	<varAccessList>	RecordVariableList
%type	<varDeclList>	VariableDeclarationList VariableDeclarationPart
%type	<variant>		Variant
%type	<variantList>	VariantList
%type	<variantPart>	VariantPart
%type	<varSel>		VariantSelector


/*
 * -- indicate the starting rule for the parser
 *    -----------------------------------------
 */
%start	File


%%

 /* ************************************************************************************************************** */

File
	/* ----------------------------------------------------------------------------------------------------------- */

	: Program
		{
			$$ = $1;
			ast = $$;
			ACCEPT("File #1");

			return 0;
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Module
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating SourceFile AST node");
			$$ = SourceFile::factory(ProgramHeading::empty(), $1);
			ast = $$;
			ACCEPT("File #2");

			return 0;
		}
	;

 /* ************************************************************************************************************** */

Program
	/* ----------------------------------------------------------------------------------------------------------- */

	: ProgramHeading T_SEMI Block T_DOT
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating SourceFile AST node");
			$$ = SourceFile::factory($1, $3);
			ACCEPT("Program #1");
		}
	;

 /* ************************************************************************************************************** */

ProgramHeading
	: T_PROGRAM Identifier
	/* ----------------------------------------------------------------------------------------------------------- */

		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating ProgramHeading AST node");
			$$ = ProgramHeading::factory($2, IdentList::empty());
			ACCEPT("ProgramHeading #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_PROGRAM Identifier T_LPAREN IdentifierList T_RPAREN
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating ProgramHeading AST node");
			$$ = ProgramHeading::factory($2, $4);
			ACCEPT("ProgramHeading #2");
		}
	;

 /* ************************************************************************************************************** */

IdentifierList
	/* ----------------------------------------------------------------------------------------------------------- */

	: IdentifierList T_COMMA Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending IdentList AST node");
			$$ = append($1, IdentList::factory($3));
			ACCEPT("IdentifierList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an IdentList AST node");
			$$ = IdentList::factory($1);
			ACCEPT("IdentifierList #2");
		}
	;

 /* ************************************************************************************************************** */

Block
	/* ----------------------------------------------------------------------------------------------------------- */

	: LabelDeclarationPart ConstantDefinitionPart TypeDefinitionPart VariableDeclarationPart
				ProcedureAndFunctionDeclarationPart StatementPart
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a Block AST node");
			$$ = Block::factory($1, $2, $3, $4, $5, $6);
			ACCEPT("Block #1");
		}
	;

 /* ************************************************************************************************************** */

Module
	/* ----------------------------------------------------------------------------------------------------------- */

	: ConstantDefinitionPart TypeDefinitionPart VariableDeclarationPart ProcedureAndFunctionDeclarationPart
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a Block AST node");
			$$ = Block::factory(LabelList::empty(), $1, $2, $3, $4, StmtList::empty());
			ACCEPT("Module #1");
		}
	;

 /* ************************************************************************************************************** */

LabelDeclarationPart
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_LABEL LabelList	T_SEMI
		{
			$$ = $2;
			ACCEPT("LabelDeclarationPart #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_LABEL error T_SEMI
		{
			/*
			 * -- This rule is set up to recover from a label definition syntax error
			 *    -------------------------------------------------------------------
			 */
			$$ = LabelList::empty();
			Log(DebugLog::LOG_YACC, LOG_ERROR, "syntax error in label declaration");
			ACCEPT("Recovering from error in the LabelDeclarationPart"); 
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| /* empty */
		{
			$$ = LabelList::empty();
			ACCEPT("LabelDeclarationPart #2");
		}
	;

 /* ************************************************************************************************************** */

LabelList
	/* ----------------------------------------------------------------------------------------------------------- */

	: LabelList T_COMMA Label
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending LabelList AST node");
			$$ = append($1, LabelList::factory($3));
			ACCEPT("LabelList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Label
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a LabelList AST node");
			$$ = LabelList::factory($1);
			ACCEPT("LabelList #2");
		}
	;

 /* ************************************************************************************************************** */

Label
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_INTEGER
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a Label AST node");
			$$ = Label::factory($1);
			ACCEPT("Label #1");
		}
	;

 /* ************************************************************************************************************** */

ConstantDefinitionPart
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_CONST ConstantList
		{
			$$ = $2;
			ACCEPT("ConstantDefinitionPart #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| /* empty */
		{
			$$ = ConstList::empty();
			ACCEPT("ConstantDefinitionPart #2");
		}
	;

 /* ************************************************************************************************************** */

ConstantList
	/* ----------------------------------------------------------------------------------------------------------- */

	: ConstantList ConstantDefinition
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending ConstList AST node");
			$$ = append($1, ConstList::factory($2));
			ACCEPT("ConstantList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ConstantDefinition
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ConstList AST node");
			$$ = ConstList::factory($1);
			ACCEPT("ConstantList #2");
		}
	;

 /* ************************************************************************************************************** */

ConstantDefinition
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier T_EQUAL CExpression T_SEMI
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ConstAssign AST node");
			$$ = ConstAssign::factory($1, $3);
			ACCEPT("ConstantDefinition #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */
	
	| Identifier T_EQUAL error T_SEMI
		{
			/*
			 * -- This rule is set up to recover from a constant expression evaluation syntax error
			 *    ---------------------------------------------------------------------------------
			 */
			$$ = ConstAssign::factory($1, Expr::empty());
			Log(DebugLog::LOG_YACC, LOG_ERROR, "syntax error in constant definition");
			ACCEPT("Recovering from error in the ConstantDefinition"); 
		}

	;

 /* ************************************************************************************************************** */

CExpression
	/* ----------------------------------------------------------------------------------------------------------- */

	: CSimpleExpression
		{
			$$ = $1;
			ACCEPT("CExpression #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CSimpleExpression RelOp CSimpleExpression
		{
			switch ($2) {
			case T_EQUAL:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an EQExpr AST node");
				$$ = EQExpr::factory($1, $3);
				break;

			case T_NOTEQUAL:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a NEExpr AST node");
				$$ = NEExpr::factory($1, $3);
				break;

			case T_LT:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an LTExpr AST node");
				$$ = LTExpr::factory($1, $3);
				break;

			case T_LE:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an LEExpr AST node");
				$$ = LEExpr::factory($1, $3);
				break;

			case T_GT:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an GTExpr AST node");
				$$ = GTExpr::factory($1, $3);
				break;

			case T_GE:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an GEExpr AST node");
				$$ = GEExpr::factory($1, $3);
				break;
			case T_IN:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an InExpr AST node");
				$$ = InExpr::factory($1, $3);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown RelOp: %d", $2);
			}

			ACCEPT("CExpression #2");
		}
	;

 /* ************************************************************************************************************** */

CSimpleExpression
	/* ----------------------------------------------------------------------------------------------------------- */

	: CTerm
		{
			$$ = $1;
			ACCEPT("CSimpleExpression #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CSimpleExpression AddOp CTerm
		{
			switch ($2) {
			case T_PLUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an AddExpr AST node");
				$$ = AddExpr::factory($1, $3);
				break;

			case T_MINUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a SubExpr AST node");
				$$ = SubExpr::factory($1, $3);
				break;

			case T_OR:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an OrExpr AST node");
				$$ = OrExpr::factory($1, $3);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown AddOp: %d", $2);
			}

			ACCEPT("CSimpleExpression #2");
		}
	;

 /* ************************************************************************************************************** */

CTerm
	/* ----------------------------------------------------------------------------------------------------------- */

	: CFactor
		{
			$$ = $1;
			ACCEPT("CTerm #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CTerm MulOp CFactor
		{
			switch ($2) {
			case T_STAR:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a MulExpr AST node");
				$$ = MulExpr::factory($1, $3);
				break;

			case T_SLASH:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a RealDivExpr AST node");
				$$ = RealDivExpr::factory($1, $3);
				break;

			case T_DIV:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an IntDivExpr AST node");
				$$ = IntDivExpr::factory($1, $3);
				break;

			case T_MOD:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a ModExpr AST node");
				$$ = ModExpr::factory($1, $3);
				break;

			case T_AND:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an AndExpr AST node");
				$$ = AndExpr::factory($1, $3);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown MulOp: %d", $2);
			}

			ACCEPT("CTerm #2");
		}
	;

 /* ************************************************************************************************************** */

CFactor
	/* ----------------------------------------------------------------------------------------------------------- */

	: Sign CFactor
		{
			switch ($1) {
			case T_PLUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a PosExpr AST node");
				$$ = PosExpr::factory($2);
				break;

			case T_MINUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a NegExpr AST node");
				$$ = NegExpr::factory($2);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown Sign: %d", $1);
			}

			ACCEPT("CFactor #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CExponentiation
		{
			$$ = $1;
			ACCEPT("CFactor #2");
		}
	;

 /* ************************************************************************************************************** */

CExponentiation
	/* ----------------------------------------------------------------------------------------------------------- */

	: CPrimary
		{
			$$ = $1;
			ACCEPT("CExponentiation #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CPrimary T_STARSTAR CExponentiation
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a ExpExpr AST node");
			$$ = ExpExpr::factory($1, $3);
			ACCEPT("CExponentiation #2");
		}
	;

 /* ************************************************************************************************************** */

CPrimary
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			$$ = $1;
			ACCEPT("CPrimary #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_LPAREN CExpression T_RPAREN
		{
			$$ = $2;
			ACCEPT("CPrimary #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| UnsignedConstant
		{
			$$ = $1;
			ACCEPT("CPrimary #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_NOT CPrimary
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a NotExpr AST node");
			$$ = NotExpr::factory($2);
			ACCEPT("CPrimary #4");
		}
	;

 /* ************************************************************************************************************** */

Constant
	/* ----------------------------------------------------------------------------------------------------------- */

	: NonString
		{
			$$ = $1;
			ACCEPT("Constant #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Sign NonString
		{
			switch ($1) {
			case T_PLUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a PosExpr AST node");
				$$ = PosExpr::factory($2);
				break;

			case T_MINUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a NegExpr AST node");
				$$ = NegExpr::factory($2);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown Sign: %d", $1);
			}

			ACCEPT("Constant #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_STRING
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an StringLit AST node");
			$$ = StringLit::factory($1);
			ACCEPT("Constant #3");
		}
	;

 /* ************************************************************************************************************** */

Sign
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_PLUS
		{
			$$ = T_PLUS;
			ACCEPT("Sign #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_MINUS
		{
			$$ = T_MINUS;
			ACCEPT("Sign #2");
		}
	;

 /* ************************************************************************************************************** */

NonString
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_INTEGER
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an IntLit AST node");
			$$ = IntLit::factory($1);
			ACCEPT("NonString #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Identifier
		{
			$$ = $1;
			ACCEPT("NonString #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_REALNUMBER
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an RealLit AST node");
			$$ = RealLit::factory($1);
			ACCEPT("NonString #3");
		}
	;

 /* ************************************************************************************************************** */

TypeDefinitionPart
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_TYPE TypeDefinitionList
		{
			$$ = $2;
			ACCEPT("TypeDefinitionPart #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| /* empty */
		{
			$$ = TypeDefList::empty();
			ACCEPT("TypeDefinitionPart #2");
		}
	;

 /* ************************************************************************************************************** */

TypeDefinitionList
	/* ----------------------------------------------------------------------------------------------------------- */

	: TypeDefinitionList TypeDefinition
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending TypeDefList AST node");
			$$ = append($1, TypeDefList::factory($2));
			ACCEPT("TypeDefinitionList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| TypeDefinition
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an TypeDefList AST node");
			$$ = TypeDefList::factory($1);
			ACCEPT("TypeDefinitionList #2");
		}
	;

 /* ************************************************************************************************************** */

TypeDefinition
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier T_EQUAL TypeDenoter T_SEMI
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an TypeDef AST node");
			$$ = TypeDef::factory($1, $3);
			ACCEPT("TypeDefinition #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Identifier T_EQUAL error T_SEMI
		{
			/*
			 * -- This rule is set up to recover from a label definition syntax error
			 *    -------------------------------------------------------------------
			 */
			$$ = TypeDef::factory($1, Type::empty());
			Log(DebugLog::LOG_YACC, LOG_ERROR, "syntax error in type definition");
			ACCEPT("Recovering from error in the TypeDefinition"); 
		}
	;

 /* ************************************************************************************************************** */

/*
 * -- For TypeDenoter, the type should be a valid type definition, already
 *    existing in the Symbol Table and in scope.  Be sure to check before
 *    returning and report any errors.
 *    --------------------------------------------------------------------
 */
TypeDenoter
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an NamedType AST node");
			$$ = NamedType::factory($1);
			ACCEPT("TypeDenoter #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| NewType
		{
			$$ = $1;
			ACCEPT("TypeDenoter #2");
		}
	;

 /* ************************************************************************************************************** */

NewType
	/* ----------------------------------------------------------------------------------------------------------- */

	: NewOrdinalType
		{
			$$ = $1;
			ACCEPT("NewType #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| NewStructuredType
		{
			$$ = $1;
			ACCEPT("NewType #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| NewPointerType
		{
			$$ = $1;
			ACCEPT("NewType #3");
		}
	;

 /* ************************************************************************************************************** */

NewOrdinalType
	/* ----------------------------------------------------------------------------------------------------------- */

	: EnumeratedType
		{
			$$ = $1;
			ACCEPT("NewOrdinalType #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| SubrangeType
		{
			$$ = $1;
			ACCEPT("NewOrdinalType #2");
		}
	;

 /* ************************************************************************************************************** */

EnumeratedType
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_LPAREN IdentifierList T_RPAREN
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an EnumeratedType AST node");
			$$ = EnumeratedType::factory($2);
			ACCEPT("EnumeratedType #1");
		}
	;

 /* ************************************************************************************************************** */

SubrangeType
	/* ----------------------------------------------------------------------------------------------------------- */

	: Constant T_DOTDOT Constant
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an SubrangeType AST node");
			$$ = SubrangeType::factory($1, $3);
			ACCEPT("SubrangeType #1");
		}
	;

 /* ************************************************************************************************************** */

NewStructuredType
	/* ----------------------------------------------------------------------------------------------------------- */

	: StructuredType
		{
			$$ = $1;
			ACCEPT("NewStructuredType #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_PACKED StructuredType
		{
			$2->Set_isPacked(true);
			$$ = $2;
			ACCEPT("NewStructuredType #2");
		}
	;

 /* ************************************************************************************************************** */

StructuredType
	/* ----------------------------------------------------------------------------------------------------------- */

	: ArrayType
		{
			$$ = $1;
			ACCEPT("StructuredType #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| RecordType
		{
			$$ = $1;
			ACCEPT("StructuredType #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| SetType
		{
			$$ = $1;
			ACCEPT("StructuredType #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| FileType
		{
			$$ = $1;
			ACCEPT("StructuredType #4");
		}
	;

 /* ************************************************************************************************************** */

ArrayType
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_ARRAY T_LSQUARE IndexList T_RSQUARE T_OF ComponentType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ArrayType AST node");
			$$ = ArrayType::factory($3, $6);
			ACCEPT("ArrayType #1");
		}
	;

 /* ************************************************************************************************************** */

IndexList
	/* ----------------------------------------------------------------------------------------------------------- */

	: IndexList T_COMMA IndexType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending an IndexTypeList AST node");
			$$ = append($1, IndexTypeList::factory($3));
			ACCEPT("IndexList #1");
		}

	/* -----------------------------------------------------------------------------------------------------------s */

	| IndexType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an IndexTypeList AST node");
			$$ = IndexTypeList::factory($1);
			ACCEPT("IndexList #2");
		}
	;

 /* ************************************************************************************************************** */

IndexType
	/* ----------------------------------------------------------------------------------------------------------- */

	: OrdinalType
		{
			$$ = $1;
			ACCEPT("IndexType #1");
		}
	;

 /* ************************************************************************************************************** */

OrdinalType
	/* ----------------------------------------------------------------------------------------------------------- */

	: NewOrdinalType
		{
			$$ = $1;
			ACCEPT("OrdinalType #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an NamedType AST node");
			$$ = NamedType::factory($1);
			ACCEPT("OrdinalType #2");
		}
	;

 /* ************************************************************************************************************** */

ComponentType
	/* ----------------------------------------------------------------------------------------------------------- */

	: TypeDenoter
		{
			$$ = $1;
			ACCEPT("ComponentType #1");
		}
	;

 /* ************************************************************************************************************** */

RecordType
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_RECORD RecordSectionList T_END
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a RecordType AST node");
			$$ = RecordType::factory($2, VariantPart::empty());
			ACCEPT("RecordType #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_RECORD RecordSectionList T_SEMI VariantPart T_END
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a RecordType AST node");
			$$ = RecordType::factory($2, $4);
			ACCEPT("RecordType #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_RECORD VariantPart T_END
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a RecordType AST node");
			$$ = RecordType::factory(RecordSectionList::empty(), $2);
			ACCEPT("RecordType #3");
		}
	;

 /* ************************************************************************************************************** */

RecordSectionList
	/* ----------------------------------------------------------------------------------------------------------- */

	: RecordSectionList T_SEMI RecordSection
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a RecordSectionList AST node");
			$$ = append($1, RecordSectionList::factory($3));
			ACCEPT("RecordSectionList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| RecordSection
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a RecordSectionList AST node");
			$$ = RecordSectionList::factory($1);
			ACCEPT("RecordSectionList #2");
		}
	;

 /* ************************************************************************************************************** */

RecordSection
	/* ----------------------------------------------------------------------------------------------------------- */

	: IdentifierList T_COLON TypeDenoter
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an RecordSection AST node");
			$$ = RecordSection::factory($1, $3);
			ACCEPT("RecordSection #1");
		}
	;

 /* ************************************************************************************************************** */

VariantPart
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_CASE VariantSelector T_OF VariantList T_SEMI
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an VariantPart AST node");
			$$ = VariantPart::factory($2, $4);
			ACCEPT("VariantPart #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_CASE VariantSelector T_OF VariantList
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an VariantPart AST node");
			$$ = VariantPart::factory($2, $4);
			ACCEPT("VariantPart #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| /* empty */
		{
			$$ = VariantPart::empty();
			ACCEPT("VariantPart #3");
		}
	;

 /* ************************************************************************************************************** */

VariantSelector
	/* ----------------------------------------------------------------------------------------------------------- */

	: TagField T_COLON TagType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a VariantSelector AST node");
			$$ = VariantSelector::factory($1, $3);
			ACCEPT("VariantSelector #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| TagType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a VariantSelector AST node");
			$$ = VariantSelector::factory(Ident::empty(), $1);
			ACCEPT("VariantSelector #2");
		}
	;

 /* ************************************************************************************************************** */

VariantList
	/* ----------------------------------------------------------------------------------------------------------- */

	: VariantList T_SEMI Variant
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a VariantList AST node");
			$$ = append($1, VariantList::factory($3));
			ACCEPT("VariantList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Variant
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a VariantList AST node");
			$$ = VariantList::factory($1);
			ACCEPT("VariantList #2");
		}
	;

 /* ************************************************************************************************************** */

Variant
	/* ----------------------------------------------------------------------------------------------------------- */

	: CaseConstantList T_COLON T_LPAREN RecordSectionList T_RPAREN
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a Variant AST node");
			$$ = Variant::factory($1, $4, VariantPart::empty());
			ACCEPT("Variant #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CaseConstantList T_COLON T_LPAREN RecordSectionList T_SEMI VariantPart T_RPAREN
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a Variant AST node");
			$$ = Variant::factory($1, $4, $6);
			ACCEPT("Variant #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CaseConstantList T_COLON T_LPAREN VariantPart T_RPAREN
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a Variant AST node");
			$$ = Variant::factory($1, RecordSectionList::empty(), $4);
			ACCEPT("Variant #3");
		}
	;

 /* ************************************************************************************************************** */

CaseConstantList
	/* ----------------------------------------------------------------------------------------------------------- */

	: CaseConstantList T_COMMA CaseConstant
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a CaseConstList AST node");
			$$ = append($1, CaseConstList::factory($3));
			ACCEPT("CaseConstantList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CaseConstant
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a CaseConstList AST node");
			$$ = CaseConstList::factory($1);
			ACCEPT("CaseConstantList #2");
		}
	;

 /* ************************************************************************************************************** */

CaseConstant
	/* ----------------------------------------------------------------------------------------------------------- */

	: Constant
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a CaseConst AST node");
			$$ = CaseConst::factory($1);
			ACCEPT("CaseConstant #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Constant T_DOTDOT Constant
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a CaseConstRange AST node");
			$$ = CaseConstRange::factory($1, $3);
			ACCEPT("CaseConstant #2");
		}
	;

 /* ************************************************************************************************************** */

TagField
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			$$ = $1;
			ACCEPT("TagField #1");
		}
	;

 /* ************************************************************************************************************** */

TagType
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			$$ = $1;
			ACCEPT("TagType #1");
		}
	;

 /* ************************************************************************************************************** */

SetType
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_SET T_OF BaseType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an SetOfType AST node");
			$$ = SetOfType::factory($3);
			ACCEPT("SetType #1");
		}
	;

 /* ************************************************************************************************************** */

BaseType
	/* ----------------------------------------------------------------------------------------------------------- */

	: OrdinalType
		{
			$$ = $1;
			ACCEPT("BaseType #1");
		}
	;

 /* ************************************************************************************************************** */

FileType
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_FILE T_OF ComponentType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an FileOfType AST node");
			$$ = FileOfType::factory($3);
			ACCEPT("FileType #1");
		}
	;

 /* ************************************************************************************************************** */

NewPointerType
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_UPARROW DomainType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an PointerType AST node");
			$$ = PointerType::factory($2);
			ACCEPT("NewPointerType #1");
		}
	;

 /* ************************************************************************************************************** */

DomainType
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an NamedType AST node");
			$$ = NamedType::factory($1);
			ACCEPT("DomainType #1");
		}
	;

 /* ************************************************************************************************************** */

VariableDeclarationPart
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_VAR VariableDeclarationList T_SEMI
		{
			$$ = $2;
			ACCEPT("VariableDeclarationPart #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| /* empty */
		{
			$$ = VarDeclList::empty();
			ACCEPT("VariableDeclarationPart #2");
		}
	;

 /* ************************************************************************************************************** */

VariableDeclarationList
	/* ----------------------------------------------------------------------------------------------------------- */

	: VariableDeclarationList T_SEMI VariableDeclaration
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a VarDeclList AST node");
			$$ = append($1, VarDeclList::factory($3));
			ACCEPT("VariableDeclarationList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| VariableDeclaration
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a VarDeclList AST node");
			$$ = VarDeclList::factory($1);
			ACCEPT("VariableDeclarationList #2");
		}
	;

 /* ************************************************************************************************************** */

VariableDeclaration
	/* ----------------------------------------------------------------------------------------------------------- */

	: IdentifierList T_COLON TypeDenoter
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an VarDecl AST node");
			$$ = VarDecl::factory($1, $3);
			ACCEPT("VariableDeclaration #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| IdentifierList T_COLON error
		{
			/*
			 * -- This rule is set up to recover from a veriable declaration syntax error
			 *    -----------------------------------------------------------------------
			 */
			$$ = VarDecl::factory($1, Type::empty());
			Log(DebugLog::LOG_YACC, LOG_ERROR, "syntax error in variable declaration");
			ACCEPT("Recovering from error in the VariableDeclaration"); 
		}
	;

 /* ************************************************************************************************************** */

ProcedureAndFunctionDeclarationPart
	/* ----------------------------------------------------------------------------------------------------------- */

	: ProcOrFuncDeclarationList
		{
			$$ = $1;
			ACCEPT("ProcedureAndFunctionDeclarationPart #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| /* empty */
		{
			$$ = SubrDeclList::empty();
			ACCEPT("ProcedureAndFunctionDeclarationPart #2");
		}
	;

 /* ************************************************************************************************************** */

ProcOrFuncDeclarationList
	/* ----------------------------------------------------------------------------------------------------------- */

	: ProcOrFuncDeclarationList T_SEMI ProcOrFuncDeclaration
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a SubrDeclList AST node");
			$$ = append ($1, SubrDeclList::factory($3));
			ACCEPT("ProcOrFuncDeclarationList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ProcOrFuncDeclaration
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a SubrDeclList AST node");
			$$ = SubrDeclList::factory($1);
			ACCEPT("ProcOrFuncDeclarationList #2");
		}
	;

 /* ************************************************************************************************************** */

ProcOrFuncDeclaration
	/* ----------------------------------------------------------------------------------------------------------- */

	: ProcedureDeclaration
		{
			$$ = $1;
			ACCEPT("ProcOrFuncDeclaration #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| FunctionDeclaration
		{
			$$ = $1;
			ACCEPT("ProcOrFuncDeclaration #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| error
		{
			/*
			 * -- This rule is set up to recover from an error in a subroutine declaration
			 *    ------------------------------------------------------------------------
			 */
			$$ = SubrDecl::empty();
			Log(DebugLog::LOG_YACC, LOG_ERROR, "syntax error in subrouting definition");
			ACCEPT("Recovering from error in the ProcOrFuncDeclaration"); 
		}

	;

 /* ************************************************************************************************************** */

ProcedureDeclaration
	/* ----------------------------------------------------------------------------------------------------------- */

	: ProcedureHeading T_SEMI Directive
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a ProcDecl AST node");
			$$ = ProcDecl::factory($1, Block::empty(), $3);
			ACCEPT("ProcedureDeclaration #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ProcedureHeading T_SEMI ProcedureBlock
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a ProcDecl AST node");
			$$ = ProcDecl::factory($1, $3, 0);
			ACCEPT("ProcedureDeclaration #2");
		}
	;

 /* ************************************************************************************************************** */

ProcedureHeading
	/* ----------------------------------------------------------------------------------------------------------- */

	: ProcedureIdentification
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a ProcHdr AST node");
			$$ = ProcHdr::factory($1, FormalParmList::empty());
			ACCEPT("ProcedureHeading #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ProcedureIdentification FormalParameterList
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a ProcHdr AST node");
			$$ = ProcHdr::factory($1, $2);
			ACCEPT("ProcedureHeading #2");
		}
	;

 /* ************************************************************************************************************** */

Directive
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_FORWARD
		{
			$$ = T_FORWARD;
			ACCEPT("Directive #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_EXTERN
		{
			$$ = T_EXTERN;
			ACCEPT("Directive #2");
		}
	;

 /* ************************************************************************************************************** */

FormalParameterList
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_LPAREN FormalParameterSectionList T_RPAREN
		{
			$$ = $2;
			ACCEPT("FormalParameterList #1");
		}
	;

 /* ************************************************************************************************************** */

FormalParameterSectionList
	/* ----------------------------------------------------------------------------------------------------------- */

	: FormalParameterSectionList T_SEMI FormalParameterSection
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a FormalParmList AST node");
			$$ = append($1, FormalParmList::factory($3));
			ACCEPT("FormalParameterSectionList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| FormalParameterSection
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a FormalParmList AST node");
			$$ = FormalParmList::factory($1);
			ACCEPT("FormalParameterSectionList #2");
		}
	;

 /* ************************************************************************************************************** */

FormalParameterSection
	/* ----------------------------------------------------------------------------------------------------------- */

	: ValueParameterSpecification
		{
			$$ = $1;
			ACCEPT("FormalParameterSection #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| VariableParameterSpecification
		{
			$$ = $1;
			ACCEPT("FormalParameterSection #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ProceduralParameterSpecification
		{
			$$ = $1;
			ACCEPT("FormalParameterSection #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| FunctionalParameterSpecification
		{
			$$ = $1;
			ACCEPT("FormalParameterSection #4");
		}
	;

 /* ************************************************************************************************************** */

ValueParameterSpecification
	/* ----------------------------------------------------------------------------------------------------------- */

	: IdentifierList T_COLON Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ValFormalParm AST node");
			$$ = ValFormalParm::factory($1, $3);
			ACCEPT("ValueParameterSpecification #1");
		}
	;

 /* ************************************************************************************************************** */

VariableParameterSpecification
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_VAR IdentifierList T_COLON Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an RefFormalParm AST node");
			$$ = RefFormalParm::factory($2, $4);
			ACCEPT("VariableParameterSpecification #1");
		}
	;

 /* ************************************************************************************************************** */

ProceduralParameterSpecification
	/* ----------------------------------------------------------------------------------------------------------- */

	: ProcedureHeading
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ProcFormalParm AST node");
			$$ = ProcFormalParm::factory($1);
			ACCEPT("ProceduralParameterSpecification #1");
		}
	;

 /* ************************************************************************************************************** */

FunctionalParameterSpecification
	/* ----------------------------------------------------------------------------------------------------------- */

	: FunctionHeading
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an FuncFormalParm AST node");
			$$ = FuncFormalParm::factory($1);
			ACCEPT("FunctionalParameterSpecification #1");
		}
	;

 /* ************************************************************************************************************** */

ProcedureIdentification
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_PROCEDURE Identifier
		{
			$$ = $2;
			ACCEPT("ProcedureIdentification #1");
		}
	;

 /* ************************************************************************************************************** */

ProcedureBlock
	/* ----------------------------------------------------------------------------------------------------------- */

	: Block
		{
			$$ = $1;
			ACCEPT("ProcedureBlock #1");
		}
	;

 /* ************************************************************************************************************** */

FunctionDeclaration
	/* ----------------------------------------------------------------------------------------------------------- */

	: FunctionHeading T_SEMI Directive
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a FuncDecl AST node");
			$$ = FuncDecl::factory($1, Block::empty(), $3);
			ACCEPT("FunctionDeclaration #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

 /******************************************************************************************************************/
 /******************************************************************************************************************/
 /****                                                                                                          ****/
 /****  So, I am not sure at this point the use for the following construct.  I have spent some time on Google  ****/
 /****  looking for some representation of this in some official language specification.  I have not yet found  ****/
 /****  anything.  I am not giving up and will keep looking, but for the moment I am disabling this rule.       ****/
 /****                                                                                                          ****/
 /******************************************************************************************************************/
 /******************************************************************************************************************/

//	| FunctionIdentification T_SEMI FunctionBlock
//		{
//			ACCEPT("FunctionDeclaration #2");
//		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| FunctionHeading T_SEMI FunctionBlock
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a FuncDecl AST node");
			$$ = FuncDecl::factory($1, $3, 0);
			ACCEPT("FunctionDeclaration #3");
		}
	;

 /* ************************************************************************************************************** */

FunctionHeading
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_FUNCTION Identifier T_COLON ResultType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a FuncHdr AST node");
			$$ = FuncHdr::factory($2, FormalParmList::empty(), $4);
			ACCEPT("FunctionHeading #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_FUNCTION Identifier FormalParameterList T_COLON ResultType
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a FuncHdr AST node");
			$$ = FuncHdr::factory($2, $3, $5);
			ACCEPT("FunctionHeading #2");
		}
	;

 /* ************************************************************************************************************** */

ResultType
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			$$ = $1;
			ACCEPT("ResultType #1");
		}
	;

 /* ************************************************************************************************************** */

 /******************************************************************************************************************/
 /******************************************************************************************************************/
 /****                                                                                                          ****/
 /****  The following rule is not needed since I disabled the additional rule above.  So, I will be disabling   ****/
 /****  this rule until I can determine what to do with the above rule to eliminate errors with bison.          ****/
 /****                                                                                                          ****/
 /******************************************************************************************************************/
 /******************************************************************************************************************/

//FunctionIdentification
	/* ----------------------------------------------------------------------------------------------------------- */

//	: T_FUNCTION Identifier
//		{
//			ACCEPT("FunctionIdentification #1");
//		}
//	;

 /* ************************************************************************************************************** */

FunctionBlock
	/* ----------------------------------------------------------------------------------------------------------- */

	: Block
		{
			$$ = $1;
			ACCEPT("FunctionBlock #1");
		}
	;

 /* ************************************************************************************************************** */

StatementPart
	/* ----------------------------------------------------------------------------------------------------------- */

	: CompoundStatement
		{
			$$ = $1;
			ACCEPT("StatementPart #1");
		}
	;

 /* ************************************************************************************************************** */

CompoundStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_BEGIN StatementSequence T_END
		{
			$$ = $2;
			ACCEPT("CompoundStatement #1");
		}
	;

 /* ************************************************************************************************************** */

StatementSequence
	/* ----------------------------------------------------------------------------------------------------------- */

	: StatementSequence T_SEMI Statement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a StmtList AST node");
			$$ = append($1, StmtList::factory($3));
			ACCEPT("StatementSequence #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Statement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a StmtList AST node");
			$$ = StmtList::factory($1);
			ACCEPT("StatementSequence #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| StatementSequence T_SEMI error
		{
			/*
			 * -- This rule is set up to recover from an error in a statement sequence
			 *    --------------------------------------------------------------------
			 */
			$$ = $1;
			Log(DebugLog::LOG_YACC, LOG_ERROR, "syntax error in a statement sequence");
			ACCEPT("Recovering from error in the StatementSequence");
		} 
	;

 /* ************************************************************************************************************** */

Statement
	/* ----------------------------------------------------------------------------------------------------------- */

	: OpenStatement
		{
			$$ = $1;
			ACCEPT("Statement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ClosedStatement
		{
			$$ = $1;
			ACCEPT("Statement #2");
		}
	;

 /* ************************************************************************************************************** */

OpenStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: Label T_COLON NonLabeledOpenStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Setting Label in Stmt AST Node");
			$3->Set_label($1);
			$$ = $3;
			ACCEPT("OpenStatement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| NonLabeledOpenStatement
		{
			$$ = $1;
			ACCEPT("OpenStatement #2");
		}
	;

 /* ************************************************************************************************************** */

ClosedStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: Label T_COLON NonLabeledClosedStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Setting Label in Stmt AST Node");
			$3->Set_label($1);
			$$ = $3;
			ACCEPT("ClosedStatement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| NonLabeledClosedStatement
		{
			$$ = $1;
			ACCEPT("ClosedStatement #2");
		}
	;

 /* ************************************************************************************************************** */

NonLabeledClosedStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: AssignmentStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ProcedureStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| GotoStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CompoundStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a CompoundStmt AST Node");
			$$ = CompoundStmt::factory($1);
			ACCEPT("NonLabeledClosedStatement #4");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CaseStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #5");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| RepeatStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #6");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ClosedWithStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #7");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ClosedIfStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #8");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ClosedWhileStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #9");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ClosedForStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledClosedStatement #10");
		}
	;

 /* ************************************************************************************************************** */

NonLabeledOpenStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: OpenWithStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledOpenStatement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| OpenIfStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledOpenStatement #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| OpenWhileStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledOpenStatement #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| OpenForStatement
		{
			$$ = $1;
			ACCEPT("NonLabeledOpenStatement #4");
		}
	;

 /* ************************************************************************************************************** */

RepeatStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_REPEAT StatementSequence T_UNTIL BooleanExpression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a RepeatStmt AST Node");
			$$ = RepeatStmt::factory($2, $4);
			ACCEPT("RepeatStatement #1");
		}
	;

 /* ************************************************************************************************************** */

OpenWhileStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_WHILE BooleanExpression T_DO OpenStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a WhileStmt AST Node");
			$$ = WhileStmt::factory($2, $4);
			ACCEPT("OpenWhileStatement #1");
		}
	;

 /* ************************************************************************************************************** */

ClosedWhileStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_WHILE BooleanExpression T_DO ClosedStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a WhileStmt AST Node");
			$$ = WhileStmt::factory($2, $4);
			ACCEPT("ClosedWhileStatement #1");
		}
	;

 /* ************************************************************************************************************** */

OpenForStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_FOR ControlVariable T_ASSIGN InitialValue Direction FinalValue T_DO OpenStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a ForStmt AST Node");
			$$ = ForStmt::factory($2, $4, $5, $6, $8);
			ACCEPT("OpenForStatement #1");
		}
	;

 /* ************************************************************************************************************** */

ClosedForStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_FOR ControlVariable T_ASSIGN InitialValue Direction FinalValue T_DO ClosedStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a ForStmt AST Node");
			$$ = ForStmt::factory($2, $4, $5, $6, $8);
			ACCEPT("ClosedForStatement #1");
		}
	;

 /* ************************************************************************************************************** */

OpenWithStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_WITH RecordVariableList T_DO OpenStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a WithStmt AST Node");
			$$ = WithStmt::factory($2, $4);
			ACCEPT("OpenWithStatement #1");
		}
	;

 /* ************************************************************************************************************** */

ClosedWithStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_WITH RecordVariableList T_DO ClosedStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a WithStmt AST Node");
			$$ = WithStmt::factory($2, $4);
			ACCEPT("ClosedWithStatement #1");
		}
	;

 /* ************************************************************************************************************** */

OpenIfStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_IF BooleanExpression T_THEN Statement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a IfStmt AST Node");
			$$ = IfStmt::factory($2, $4, Stmt::empty());
			ACCEPT("OpenIfStatement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_IF BooleanExpression T_THEN ClosedStatement T_ELSE OpenStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a IfStmt AST Node");
			$$ = IfStmt::factory($2, $4, $6);
			ACCEPT("OpenIfStatement #2");
		}
	;

 /* ************************************************************************************************************** */

ClosedIfStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_IF BooleanExpression T_THEN ClosedStatement T_ELSE ClosedStatement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a IfStmt AST Node");
			$$ = IfStmt::factory($2, $4, $6);
			ACCEPT("ClosedIfStatement #1");
		}
	;

 /* ************************************************************************************************************** */

AssignmentStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: VariableAccess T_ASSIGN Expression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a AssignmentStmt AST Node");
			$$ = AssignmentStmt::factory($1, $3);
			ACCEPT("AssignmentStatement #1");
		}
	;

 /* ************************************************************************************************************** */

VariableAccess
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a SimpleNameAccess AST Node");
			$$ = SimpleNameAccess::factory($1);
			ACCEPT("VariableAccess #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| IndexedVariable
		{
			$$ = $1;
			ACCEPT("VariableAccess #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| FieldDesignator
		{
			$$ = $1;
			ACCEPT("VariableAccess #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| VariableAccess T_UPARROW
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a PointerAccess AST Node");
			$$ = PointerAccess::factory($1);
			ACCEPT("VariableAccess #4");
		}
	;

 /* ************************************************************************************************************** */

IndexedVariable
	/* ----------------------------------------------------------------------------------------------------------- */

	: VariableAccess T_LSQUARE IndexExpressionList T_RSQUARE
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a IdxVarAccess AST Node");
			$$ = IdxVarAccess::factory($1, $3);
			ACCEPT("IndexedVariable #1");
		}
	;

 /* ************************************************************************************************************** */

IndexExpressionList
	/* ----------------------------------------------------------------------------------------------------------- */

	: IndexExpressionList T_COMMA IndexExpression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending an ExprList AST Node");
			$$ = append($1, ExprList::factory($3));
			ACCEPT("IndexExpressionList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| IndexExpression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ExprList AST Node");
			$$ = ExprList::factory($1);
			ACCEPT("IndexExpressionList #2");
		}
	;

 /* ************************************************************************************************************** */

IndexExpression
	/* ----------------------------------------------------------------------------------------------------------- */

	: Expression
		{
			$$ = $1;
			ACCEPT("IndexExpression #1");
		}
	;

 /* ************************************************************************************************************** */

FieldDesignator
	/* ----------------------------------------------------------------------------------------------------------- */

	: VariableAccess T_DOT Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an FieldAccess AST Node");
			$$ = FieldAccess::factory($1, $3);
			ACCEPT("FieldDesignator #1");
		}
	;

 /* ************************************************************************************************************** */

ProcedureStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier Parms
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ProcedureCallStmt AST Node");
			$$ = ProcedureCallStmt::factory($1, $2);
			ACCEPT("ProcedureStatement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Identifier
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ProcedureCallStmt AST Node");
			$$ = ProcedureCallStmt::factory($1, ExprList::empty());
			ACCEPT("ProcedureStatement #2");
		}
	;

 /* ************************************************************************************************************** */

Parms
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_LPAREN ActualParameterList T_RPAREN
		{
			$$ = $2;
			ACCEPT("Parms #1");
		}
	;

 /* ************************************************************************************************************** */

ActualParameterList
	/* ----------------------------------------------------------------------------------------------------------- */

	: ActualParameterList T_COMMA ActualParameter
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending an ExprList AST Node");
			$$ = append($1, ExprList::factory($3));
			ACCEPT("ActualParameterList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| ActualParameter
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ExprList AST Node");
			$$ = ExprList::factory($1);
			ACCEPT("ActualParameterList #2");
		}
	;

 /* ************************************************************************************************************** */

/*
 * this forces you to check all this to be sure that only write and
 * writeln use the 2nd and 3rd forms, you really can't do it easily in
 * the grammar, especially since write and writeln aren't reserved
 */

ActualParameter
	/* ----------------------------------------------------------------------------------------------------------- */

	: Expression
		{
			$$ = $1;
			ACCEPT("ActualParameter #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Expression T_COLON Expression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ExprExpr AST Node");
			$$ = ExprExpr::factory($1, $3);
			ACCEPT("ActualParameter #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Expression T_COLON Expression T_COLON Expression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an ExprExprExpr AST Node");
			$$ = ExprExprExpr::factory($1, $3, $5);
			ACCEPT("ActualParameter #3");
		}
	;

 /* ************************************************************************************************************** */

GotoStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_GOTO Label
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an GotoStmt AST Node");
			$$ = GotoStmt::factory($2);
			ACCEPT("GotoStatement #1");
		}
	;

 /* ************************************************************************************************************** */

CaseStatement
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_CASE CaseIndex T_OF CaseListElementList T_END
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an CaseStmt AST Node");
			$$ = CaseStmt::factory($2, $4, Stmt::empty());
			ACCEPT("CaseStatement #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_CASE CaseIndex T_OF CaseListElementList T_SEMI T_END
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an CaseStmt AST Node");
			$$ = CaseStmt::factory($2, $4, Stmt::empty());
			ACCEPT("CaseStatement #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_CASE CaseIndex T_OF CaseListElementList T_SEMI OtherwisePart Statement T_END
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an CaseStmt AST Node");
			$$ = CaseStmt::factory($2, $4, $7);
			ACCEPT("CaseStatement #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_CASE CaseIndex T_OF CaseListElementList T_SEMI OtherwisePart Statement T_SEMI T_END
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an CaseStmt AST Node");
			$$ = CaseStmt::factory($2, $4, $7);
			ACCEPT("CaseStatement #4");
		}
	;

 /* ************************************************************************************************************** */

CaseIndex
	/* ----------------------------------------------------------------------------------------------------------- */

	: Expression
		{
			$$ = $1;
			ACCEPT("CaseIndex #1");
		}
	;

 /* ************************************************************************************************************** */

CaseListElementList
	/* ----------------------------------------------------------------------------------------------------------- */

	: CaseListElementList T_SEMI CaseListElement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a CaseElemList AST Node");
			$$ = append($1, CaseElemList::factory($3));
			ACCEPT("CaseListElementList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| CaseListElement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a CaseElemList AST Node");
			$$ = CaseElemList::factory($1);
			ACCEPT("CaseListElementList #2");
		}
	;

 /* ************************************************************************************************************** */

CaseListElement
	/* ----------------------------------------------------------------------------------------------------------- */

	: CaseConstantList T_COLON Statement
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an CaseElement AST Node");
			$$ = CaseElement::factory($1, $3);
			ACCEPT("CaseListElement #1");
		}
	;

 /* ************************************************************************************************************** */

OtherwisePart
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_OTHERWISE
		{
			// -- There is no need to return a type for this rule
			ACCEPT("OtherwisePart #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_OTHERWISE T_COLON
		{
			// -- There is no need to return a type for this rule
			ACCEPT("OtherwisePart #2");
		}
	;

 /* ************************************************************************************************************** */

ControlVariable
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier
		{
			$$ = $1;
			ACCEPT("ControlVariable #1");
		}
	;

 /* ************************************************************************************************************** */

InitialValue
	/* ----------------------------------------------------------------------------------------------------------- */

	: Expression
		{
			$$ = $1;
			ACCEPT("InitialValue #1");
		}
	;

 /* ************************************************************************************************************** */

Direction
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_TO
		{
			$$ = T_TO;
			ACCEPT("Direction #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_DOWNTO
		{
			$$ = T_DOWNTO;
			ACCEPT("Direction #2");
		}
	;

 /* ************************************************************************************************************** */

FinalValue
	/* ----------------------------------------------------------------------------------------------------------- */

	: Expression
		{
			$$ = $1;
			ACCEPT("FinalValue #1");
		}
	;

 /* ************************************************************************************************************** */

RecordVariableList
	/* ----------------------------------------------------------------------------------------------------------- */

	: RecordVariableList T_COMMA VariableAccess
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a VarAccessList AST Node");
			$$ = append($1, VarAccessList::factory($3));
			ACCEPT("RecordVariableList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| VariableAccess
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a VarAccessList AST Node");
			$$ = VarAccessList::factory($1);
			ACCEPT("RecordVariableList #2");
		}
	;

 /* ************************************************************************************************************** */

BooleanExpression
	/* ----------------------------------------------------------------------------------------------------------- */

	: Expression
		{
			$$ = $1;
			ACCEPT("BooleanExpression #1");
		}
	;

 /* ************************************************************************************************************** */

Expression
	/* ----------------------------------------------------------------------------------------------------------- */

	: SimpleExpression
		{
			$$ = $1;
			ACCEPT("Expression #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| SimpleExpression RelOp SimpleExpression
		{
			switch ($2) {
			case T_EQUAL:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an EQExpr AST node");
				$$ = EQExpr::factory($1, $3);
				break;

			case T_NOTEQUAL:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a NEExpr AST node");
				$$ = NEExpr::factory($1, $3);
				break;

			case T_LT:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an LTExpr AST node");
				$$ = LTExpr::factory($1, $3);
				break;

			case T_LE:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an LEExpr AST node");
				$$ = LEExpr::factory($1, $3);
				break;

			case T_GT:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an GTExpr AST node");
				$$ = GTExpr::factory($1, $3);
				break;

			case T_GE:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an GEExpr AST node");
				$$ = GEExpr::factory($1, $3);
				break;

			case T_IN:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an InExpr AST node");
				$$ = InExpr::factory($1, $3);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown RelOp: %d", $2);
			}

			ACCEPT("Expression #2");
		}
	;

 /* ************************************************************************************************************** */

SimpleExpression
	/* ----------------------------------------------------------------------------------------------------------- */

	: Term
		{	$$ = $1;
			ACCEPT("SimpleExpression #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| SimpleExpression AddOp Term
		{
			switch ($2) {
			case T_PLUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an AddExpr AST node");
				$$ = AddExpr::factory($1, $3);
				break;

			case T_MINUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a SubExpr AST node");
				$$ = SubExpr::factory($1, $3);
				break;

			case T_OR:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an OrExpr AST node");
				$$ = OrExpr::factory($1, $3);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown AddOp: %d", $2);
			}

			ACCEPT("SimpleExpression #2");
		}
	;

 /* ************************************************************************************************************** */

Term
	/* ----------------------------------------------------------------------------------------------------------- */

	: Factor
		{
			$$ = $1;
			ACCEPT("Term #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Term MulOp Factor
		{
			switch ($2) {
			case T_STAR:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a MulExpr AST node");
				$$ = MulExpr::factory($1, $3);
				break;

			case T_SLASH:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a RealDivExpr AST node");
				$$ = RealDivExpr::factory($1, $3);
				break;

			case T_DIV:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an IntDivExpr AST node");
				$$ = IntDivExpr::factory($1, $3);
				break;

			case T_MOD:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a ModExpr AST node");
				$$ = ModExpr::factory($1, $3);
				break;

			case T_AND:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an AndExpr AST node");
				$$ = AndExpr::factory($1, $3);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown MulOp: %d", $2);
			}

			ACCEPT("Term #2");
		}
	;

 /* ************************************************************************************************************** */

Factor
	/* ----------------------------------------------------------------------------------------------------------- */

	: Sign Factor
		{
			switch ($1) {
			case T_PLUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a PosExpr AST node");
				$$ = PosExpr::factory($2);
				break;

			case T_MINUS:
				Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a NegExpr AST node");
				$$ = NegExpr::factory($2);
				break;

			default:
				Log(DebugLog::LOG_YACC, LOG_ERROR, "Internal error: trying to process an unknown Sign: %d", $1);
			}

			ACCEPT("Factor #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Exponentiation
		{
			$$ = $1;
			ACCEPT("Factor #2");
		}
	;

 /* ************************************************************************************************************** */

Exponentiation
	/* ----------------------------------------------------------------------------------------------------------- */

	: Primary
		{
			$$ = $1;
			ACCEPT("Exponentiation #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Primary T_STARSTAR Exponentiation
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a ExpExpr AST node");
			$$ = ExpExpr::factory($1, $3);
			ACCEPT("Exponentiation #2");
		}
	;

 /* ************************************************************************************************************** */

Primary
	/* ----------------------------------------------------------------------------------------------------------- */

	: VariableAccess
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a VariableAccess AST node");
			$$ = VariableAccess::factory($1);
			ACCEPT("Primary #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| UnsignedConstant
		{
			$$ = $1;
			ACCEPT("Primary #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| FunctionDesignator
		{
			$$ = $1;
			ACCEPT("Primary #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| SetConstructor
		{
			$$ = $1;
			ACCEPT("Primary #4");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_LPAREN Expression T_RPAREN
		{
			$$ = $2;
			ACCEPT("Primary #5");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_NOT Primary
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a NotExpr AST node");
			$$ = NotExpr::factory($2);
			ACCEPT("Primary #6");
		}
	;

 /* ************************************************************************************************************** */

UnsignedConstant
	/* ----------------------------------------------------------------------------------------------------------- */

	: UnsignedNumber
		{
			$$ = $1;
			ACCEPT("UnsignedConstant #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_STRING
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an StringLit AST node");
			$$ = StringLit::factory($1);
			ACCEPT("UnsignedConstant #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_NIL
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an NilLit AST node");
			$$ = NilLit::factory();
			ACCEPT("UnsignedConstant #3");
		}
	;

 /* ************************************************************************************************************** */

UnsignedNumber
	/* ----------------------------------------------------------------------------------------------------------- */

	: UnsignedInteger
		{
			$$ = $1;
			ACCEPT("UnsignedNumber #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| UnsignedReal
		{
			$$ = $1;
			ACCEPT("UnsignedNumber #2");
		}
	;

 /* ************************************************************************************************************** */

UnsignedInteger
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_INTEGER
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an IntLit AST node");
			$$ = IntLit::factory($1);
			ACCEPT("UnsignedInteger #1");
		}
	;

 /* ************************************************************************************************************** */

UnsignedReal
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_REALNUMBER
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an RealLit AST node");
			$$ = RealLit::factory($1);
			ACCEPT("UnsignedReal #1");
		}
	;

 /* ************************************************************************************************************** */

FunctionDesignator
	/* ----------------------------------------------------------------------------------------------------------- */

	: Identifier Parms
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an FuncCall AST node");
			$$ = FuncCall::factory($1, $2);
			ACCEPT("FunctionDesignator #1");
		}
	;

 /* ************************************************************************************************************** */

SetConstructor
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_LSQUARE MemberDesignatorList T_RSQUARE
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an empty SetConstructor AST node");
			$$ = SetConstructor::factory($2);
			ACCEPT("SetConstructor #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_LSQUARE T_RSQUARE
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an empty SetConstructor AST node");
			$$ = SetConstructor::empty();
			ACCEPT("SetConstructor #2");
		}
	;

 /* ************************************************************************************************************** */

MemberDesignatorList
	/* ----------------------------------------------------------------------------------------------------------- */

	: MemberDesignatorList T_COMMA MemberDesignator
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a MemberDesignatorList AST node");
			$$ = append($1, MemberDesignatorList::factory($3));
			ACCEPT("MemberDesignatorList #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| MemberDesignator
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a MemberDesignatorList AST node");
			$$ = MemberDesignatorList::factory($1);
			ACCEPT("MemberDesignatorList #2");
		}
	;

 /* ************************************************************************************************************** */

MemberDesignator
	/* ----------------------------------------------------------------------------------------------------------- */

	: MemberDesignator T_DOTDOT Expression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating and appending a MemberDesignator AST node");
			$$ = append($1, MemberDesignator::factory($3));
			ACCEPT("MemberDesignator #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| Expression
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating a MemberDesignator AST node");
			$$ = MemberDesignator::factory($1);
			ACCEPT("MemberDesignator #2");
		}
	;

 /* ************************************************************************************************************** */

AddOp
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_PLUS
		{
			$$ = T_PLUS;
			ACCEPT("AddOp #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_MINUS
		{
			$$ = T_MINUS;
			ACCEPT("AddOp #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_OR
		{
			$$ = T_OR;
			ACCEPT("AddOp #3");
		}
	;

 /* ************************************************************************************************************** */

MulOp
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_STAR
		{
			$$ = T_STAR;
			ACCEPT("MulOp #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_SLASH		/* real-result division */
		{
			$$ = T_SLASH;
			ACCEPT("MulOp #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_DIV			/* integer-result division */
		{
			$$ = T_DIV;
			ACCEPT("MulOp #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_MOD
		{
			$$ = T_MOD;
			ACCEPT("MulOp #4");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_AND
		{
			$$ = T_AND;
			ACCEPT("MulOp #5");
		}
	;

 /* ************************************************************************************************************** */

RelOp
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_EQUAL
		{
			$$ = T_EQUAL;
			ACCEPT("RelOp #1");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_NOTEQUAL
		{
			$$ = T_NOTEQUAL;
			ACCEPT("RelOp #2");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_GT
		{
			$$ = T_GT;
			ACCEPT("RelOp #3");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_GE
		{
			$$ = T_GE;
			ACCEPT("RelOp #4");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_LT
		{
			$$ = T_LT;
			ACCEPT("RelOp #5");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_LE
		{
			$$ = T_LE;
			ACCEPT("RelOp #6");
		}

	/* ----------------------------------------------------------------------------------------------------------- */

	| T_IN
		{
			$$ = T_IN;
			ACCEPT("RelOp #7");
		}
	;

 /* ************************************************************************************************************** */

Identifier
	/* ----------------------------------------------------------------------------------------------------------- */

	: T_ID
		{
			Log(DebugLog::LOG_YACC, LOG_DEBUG, "Creating an Ident AST node");
			$$ = Ident::factory($1);
			ACCEPT("Identifier #1");
		}
	;

 /* ************************************************************************************************************** */


%%

SourceFile *ast = NULL;

/*
 * == The following are variables and functions needed by other parts of the compiler
 *    ===============================================================================
 */

int pErrors = 0;
/*
 * -- Emit an error
 *    -------------
 */ 
int yyerror (const char *s)
{
	extern int yylineno;
	extern std::string srcFile;
	int rv = fprintf(stderr, "[%s: %d] %s\n", srcFile.c_str(), yylineno, s);
	
	if (++ pErrors > 50) { 
		fprintf(stderr, "More than 50 errors\n");
		exit(EXIT_FAILURE);
	}
	
	return rv;
}
