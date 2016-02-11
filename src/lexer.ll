/*=================================================================================================================*/
/*                                                                                                                 */
/* lexer.yy -- This is the flex rules file for the pascal language.                                                */
/*                                                                                                                 */
/* --------------------------------------------------------------------------------------------------------------- */
/*                                                                                                                 */
/*    Date     Tracker  Version  Pgmr  Modification                                                                */
/* ----------  -------  -------  ----  --------------------------------------------------------------------------- */
/* 2016-01-10  Initial   v0.0    ADCL  Initial version -- leveraged from the pascal compiler, but made C++         */
/* 2016-01-19  Initial   v0.0    ADCL  Add some error checking to the scanner, returning the results to the        */
/*                                     scanner and allowing the scanner to handle the error reporting.             */
/* 2016-02-10            v0.0    ADCL  You might notice a change in MOST of this source file.  These changes are   */
/*                                     due to changing how the file is intended (using spaces rather than tabs).   */
/*                                     In addition, trailing spaces have been removed.                             */
/*                                                                                                                 */
/*=================================================================================================================*/


%{
    #include "debug.hh"
    #include "stringtab.hh"
    #include "symtab.hh"
    #include "pascal.hh"
    #include "grammar.hh"

    #include <ctype.h>
    #include <string>

    int lineNum = 1;
    std::string strLiteral;
%}


/*
* -- Some start conditions for comments and strings
*    ----------------------------------------------
*/
%x              str
%x              cmt1
%x              cmt2


/*
* -- Since this scanner is case insensitive, define macros for each
*    letter, which will be used in the reserved words.
*    --------------------------------------------------------------
*/
A               [Aa]
B               [Bb]
C               [Cc]
D               [Dd]
E               [Ee]
F               [Ff]
G               [Gg]
H               [Hh]
I               [Ii]
J               [Jj]
K               [Kk]
L               [Ll]
M               [Mm]
N               [Nn]
O               [Oo]
P               [Pp]
Q               [Qq]
R               [Rr]
S               [Ss]
T               [Tt]
U               [Uu]
V               [Vv]
W               [Ww]
X               [Xx]
Y               [Yy]
Z               [Zz]
NQUOTE          [^']

letter          [a-zA-Z]
digit           [0-9]

sign            [+-]
digit_sequence  {digit}+

cmt_begin       \(\*
cmt_end         \*\)

ws              [ \t]

NULL            "\0"
NL              \n

%%

    /* The following will consume white space quietly */
    /* ---------------------------------------------- */
{ws}*

    /* The following are Special Symbols */
    /* --------------------------------- */
\*\*            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_STARSTAR");
                    return T_STARSTAR;
                }

\+              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_PLUS");
                    return T_PLUS;
                }

\-              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_MINUS");
                    return T_MINUS;
                }

\*              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_STAR");
                    return T_STAR;
                }

\/              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_SLASH");
                    return T_SLASH;
                }

\=              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_EQUAL");
                    return T_EQUAL;
                }

\<              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_LT");
                    return T_LT;
                }

\>              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_GT");
                    return T_GT;
                }

\[              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_LSQUARE");
                    return T_LSQUARE;
                }

\]              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_RSQUARE");
                    return T_RSQUARE;
                }

\.              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_DOT");
                    return T_DOT;
                }

\,              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_COMMA");
                    return T_COMMA;
                }

:               {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_COLON");
                    return T_COLON;
                }

;               {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_SEMI");
                    return T_SEMI;
                }

\^              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_UPARROW");
                    return T_UPARROW;
                }

\(              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_LPAREN");
                    return T_LPAREN;
                }

\)              {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_RPAREN");
                    return T_RPAREN;
                }

\<\>            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_NOTEQUAL");
                    return T_NOTEQUAL;
                }

\<\=            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_LE");
                    return T_LE;
                }

\>\=            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_GE");
                    return T_GE;
                }

:\=	            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_ASSIGN");
                    return T_ASSIGN;
                }

\.\.            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_DOTDOT");
                    return T_DOTDOT;
                }


    /* The following are lexical alternatives */
    /* -------------------------------------- */
@               {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning Alternative T_UPARROW");
                    return T_UPARROW;
                }

\(\.            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning Alternative T_LSQUARE");
                    return T_LSQUARE;
                }

\.\)            {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_RSQUARE");
                    return T_RSQUARE;
                }


    /* The following are Word Symbols */
    /* ------------------------------ */
{A}{N}{D}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_AND");
                                return T_AND;
                            }

{A}{R}{R}{A}{Y}             {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_ARRAY");
                                return T_ARRAY;
                            }

{B}{E}{G}{I}{N}             {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_BEGIN");
                                return T_BEGIN;
                            }

{C}{A}{S}{E}                {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_CASE");
                                return T_CASE;
                            }

{C}{O}{N}{S}{T}             {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_CONST");
                                return T_CONST;
                            }

{D}{I}{V}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_DIV");
                                return T_DIV;
                            }

{D}{O}                      {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_DO");
                                return T_DO;
                            }

{D}{O}{W}{N}{T}{O}          {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_DOWNTO");
                                return T_DOWNTO;
                            }

{E}{L}{S}{E}                {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_ELSE");
                                return T_ELSE;
                            }

{E}{N}{D}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_END");
                                return T_END;
                            }

{F}{I}{L}{E}                {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_FILE");
                                return T_FILE;
                            }

{F}{O}{R}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_FOR");
                                return T_FOR;
                            }

{F}{U}{N}{C}{T}{I}{O}{N}    {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_FUNCTION");
                                return T_FUNCTION;
                            }

{G}{O}{T}{O}                {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_GOTO");
                                return T_GOTO;
                            }

{I}{F}                      {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_IF");
                                return T_IF;
                            }

{I}{N}                      {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_IN");
                                return T_IN;
                            }

{L}{A}{B}{E}{L}             {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_LABEL");
                                return T_LABEL;
                            }

{M}{O}{D}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_MOD");
                                return T_MOD;
                            }

{N}{I}{L}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_NIL");
                                return T_NIL;
                            }

{N}{O}{T}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_NOT");
                                return T_NOT;
                            }

{O}{F}                      {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_OF");
                                return T_OF;
                            }

{O}{R}                      {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_OR");
                                return T_OR;
                            }

{P}{A}{C}{K}{E}{D}          {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_PACKED");
                                return T_PACKED;
                            }

{P}{R}{O}{C}{E}{D}{U}{R}{E} {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_PROCEDURE");
                                return T_PROCEDURE;
                            }

{P}{R}{O}{G}{R}{A}{M}       {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_PROGRAM");
                                return T_PROGRAM;
                            }

{R}{E}{C}{O}{R}{D}          {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_RECORD");
                                return T_RECORD;
                            }

{R}{E}{P}{E}{A}{T}          {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_REPEAT");
                                return T_REPEAT;
                            }

{S}{E}{T}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_SET");
                                return T_SET;
                            }

{T}{H}{E}{N}                {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_THEN");
                                return T_THEN;
                            }

{T}{O}                      {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_TO");
                                return T_TO;
                            }

{T}{Y}{P}{E}                {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_TYPE");
                                return T_TYPE;
                            }

{U}{N}{T}{I}{L}             {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_UNTIL");
                                return T_UNTIL;
                            }

{V}{A}{R}                   {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_VAR");
                                return T_VAR;
                            }

{W}{H}{I}{L}{E}             {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_WHILE");
                                return T_WHILE;
                            }

{W}{I}{T}{H}                {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_WITH");
                                return T_WITH;
                            }


    /* The following are Directives */
    /* ---------------------------- */
{F}{O}{R}{W}{A}{R}{D}       {   Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_FORWARD");
                                return T_FORWARD;
                            }


    /* The following is an identifier */
    /* ------------------------------ */
{letter}({letter}|{digit})* {   Log(DebugLog::LOG_LEX, LOG_HIDEBUG, "Scanned Identifier: %s", yytext);
                                yylval.id = idTable.AddString(yytext);
                                Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_ID");

                                return T_ID;
                            }


    /* The following are integer numbers */
    /* --------------------------------- */
{digit_sequence}            {   yylval.nbr = numTable.AddString(yytext);
                                Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_INTEGER %s", yytext);

                                return T_INTEGER;
                            }


    /* The following are real numbers */
    /* ------------------------------ */
{digit_sequence}\.{digit_sequence}({E}{sign}?{digit_sequence})?         {
                                yylval.nbr = numTable.AddString(yytext);
                                Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_REALNUMBER %s", yytext);

                                return T_REALNUMBER;
                            }

{digit_sequence}{E}{sign}?{digit_sequence}          {
                                yylval.nbr = numTable.AddString(yytext);
                                Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_REALNUMBER %s", yytext);

                                return T_REALNUMBER;
                            }


    /* The following are quoted string literals, along with the error conditions that will be trapped */
    /* ---------------------------------------------------------------------------------------------- */
\'                          {   BEGIN(str); strLiteral = ""; }

<str>\'\'                   {   strLiteral += '\''; }

<str>{NULL}                 {   yylval.errMsg = "String contains a NULL character";
                                Log(DebugLog::LOG_LEX, LOG_ERROR, yylval.errMsg);
                                BEGIN(INITIAL);

                                return T_ERROR;
                            }

<str>{NL}                   {   yylval.errMsg = "Unterminated string constant";
                                Log(DebugLog::LOG_LEX, LOG_ERROR, yylval.errMsg);
                                BEGIN(INITIAL);

                                return T_ERROR;
                            }

<str>[^']*                  {   strLiteral += *yytext; }

<str>\'                     {   yylval.strVal = strTable.AddString(strLiteral);
                                Log(DebugLog::LOG_LEX, LOG_ENTRY, "Scanner returning T_STRING %s", strLiteral.c_str());
                                BEGIN(INITIAL);

                                return T_STRING;
                            }

<str><<EOF>>                {   yylval.errMsg = "EOF detected in string constant";
                                Log(DebugLog::LOG_LEX, LOG_ERROR, yylval.errMsg);
                                BEGIN(INITIAL);

                                return T_ERROR;
                            }


    /* The following is a comment  (delimited by (* and *) ), with some error conditions to be trapped */
    /* ----------------------------------------------------------------------------------------------- */
{cmt_begin}                 {   BEGIN(cmt1);
                                Log(DebugLog::LOG_LEX, LOG_DEBUG, "Beginning comment with (* ");
                            }

<cmt1>\n                    {   Log(DebugLog::LOG_LEX, LOG_DEBUG, "matching a newline in a comment"); }

<cmt1>{cmt_end}             {   BEGIN(INITIAL);
                                Log(DebugLog::LOG_LEX, LOG_DEBUG, "Ending comment with *) ");
                            }

<cmt1>[^\n]                 {   Log(DebugLog::LOG_LEX, LOG_DEBUG, "matching a character in a comment"); }

<cmt1><<EOF>>               {   yylval.errMsg = "EOF detected in comment";
                                Log(DebugLog::LOG_LEX, LOG_ERROR, yylval.errMsg);
                                BEGIN(INITIAL);

                                return T_ERROR;
                            }


    /* The following is a comment  (delimited by { and } ), with some error conditions to be trapped */
    /* --------------------------------------------------------------------------------------------- */
\{                          {   BEGIN(cmt2);
                                Log(DebugLog::LOG_LEX, LOG_DEBUG, "Beginning comment with { ");
                            }

<cmt2>\n                    {   Log(DebugLog::LOG_LEX, LOG_DEBUG, "matching a newline in a comment"); }

<cmt2>\}                    {   BEGIN(INITIAL);
                                Log(DebugLog::LOG_LEX, LOG_DEBUG, "Ending comment with } ");
                            }

<cmt2>[^\n]                 {   Log(DebugLog::LOG_LEX, LOG_DEBUG, "matching a character in a comment"); }

<cmt2><<EOF>>               {   yylval.errMsg = "EOF detected in comment";
                                Log(DebugLog::LOG_LEX, LOG_ERROR, yylval.errMsg);
                                BEGIN(INITIAL);

                                return T_ERROR;
                            }


    /* Finally capture the newline as its own rule */
    /* ------------------------------------------- */
{NL}                        { }


    /* This is a catch-all for all invalid characters */
    /* ---------------------------------------------- */
.                           {   yylval.errMsg = "Unrecognized charaacter";
                                Log(DebugLog::LOG_LEX, LOG_ERROR, "Invalid char %c (%d)\n", *yytext, *yytext);

                                return T_ERROR;
                            }


%%

int yywrap(void) { return 1; }
