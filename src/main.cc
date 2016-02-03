//===================================================================================================================
//
// main.cc -- This is the main entry point and command line interface for the pascal-cc compiler.
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/07  Initial   v0.0    ADCL  Initial version -- leveraged from the pascal compiler, but made C++
//
//===================================================================================================================

#include "debug.hh"
#include "symtab.hh"
#include "pascal.hh"

#include <cstring>
#include <cstdlib>


//
// -- This holds the name of the source file we are compiling and is used in logging and the AST
//    ------------------------------------------------------------------------------------------
std::string srcFile = "";


//
// -- This function will print startup information about the compiler.  This will include the version number, the
//    build number, and build date.
//    -----------------------------------------------------------------------------------------------------------
static void PrintStartupData(void)
{
	extern const unsigned long BLD_NUM;
	extern const char *VERSION;
	extern const char *BLD_DATE;

	fprintf(stdout, "Pascal compiler version: v%s\n", VERSION);
	fprintf(stdout, "  ( build %lu on %s )\n\n", BLD_NUM, BLD_DATE);
}


//
// -- This function is used to establish the default compiler state
//    -------------------------------------------------------------
static void InitializeCompiler(void)
{
	PrintStartupData();
	
	idTable.AddString("boolean");
	idTable.AddString("string");
	idTable.AddString("char");
	idTable.AddString("integer");
	idTable.AddString("real");
	idTable.AddString("^");

	idTable.AddString("maxint");
	idTable.AddString("false");
	idTable.AddString("true");
}


//
// -- Parse the command line parameters
//    ---------------------------------
static void ParseCommandLine(int argc, char *argv[])
{
	extern FILE *yyin;

	int error = 0;
	int i;
	int printUsage = 0;
	int printVersion = 0;
	int quit = 0;

	for (i = 1; i < argc; i ++) {
		// -- only the last parameter should not have a "-"
		if (i == argc - 1 && *(argv[i]) != '-') break;

		if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-?") == 0) {
			printUsage = 1;
			continue;
		}

		if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--version") == 0) {
			printVersion = 1;
			continue;
		}

		if (strncmp(argv[i], "--log:", 6) == 0) {
			if (strncmp(argv[i] + 6, "hash=", 5) == 0) {
				if (*(argv[i] + 11) >= '0' && *(argv[i] + 11) <= '8') {
					dbgLog.LoggingLevel(DebugLog::LOG_HASH, (LogLevel)(*(argv[i] + 11) - '0'));
					continue;
				} else {
					error = i;
					break;
				}
			} else if (strncmp(argv[i] + 6, "sym=", 4) == 0) {
				if (*(argv[i] + 10) >= '0' && *(argv[i] + 10) <= '8') {
					dbgLog.LoggingLevel(DebugLog::LOG_SYM, (LogLevel)(*(argv[i] + 10) - '0'));
					continue;
				} else {
					error = i;
					break;
				}
			} else if (strncmp(argv[i] + 6, "lex=", 4) == 0) {
				if (*(argv[i] + 10) >= '0' && *(argv[i] + 10) <= '8') {
					dbgLog.LoggingLevel(DebugLog::LOG_LEX, (LogLevel)(*(argv[i] + 10) - '0'));
					continue;
				} else {
					error = i;
					break;
				}
			} else if (strncmp(argv[i] + 6, "semant=", 7) == 0) {
				if (*(argv[i] + 13) >= '0' && *(argv[i] + 13) <= '8') {
					dbgLog.LoggingLevel(DebugLog::LOG_SEMANT, (LogLevel)(*(argv[i] + 13) - '0'));
					continue;
				} else {
					error = i;
					break;
				}
			} else if (strncmp(argv[i] + 6, "yacc=", 5) == 0) {
				if (*(argv[i] + 11) >= '0' && *(argv[i] + 11) <= '8') {
					dbgLog.LoggingLevel(DebugLog::LOG_YACC, (LogLevel)(*(argv[i] + 11) - '0'));
					continue;
				} else {
					error = i;
					break;
				}
			}
		}

		error = i;
		break;
	}

	// -- make sure we got at least 1 parameter
	if (argc == 1) error = 1;

	// -- check for a filename at the end
	if (!error) {
		i = argc - 1;
		if (*(argv[i]) != '-') {
			yyin = fopen(argv[i], "r");
			if (!yyin) {
				fprintf(stderr, "Unable to open input file %s\n\n", argv[i]);
				exit(1);
			}
			srcFile = argv[i];
		}
	}

	if (error) {
		fprintf(stderr, "Invalid Argument: %s\n\n", argv[error]);
		printUsage = 1;
		quit = 1;
	}

	if (printVersion) {
		quit = 1;
	}

	if (printUsage) {
		printf("Usage: pas-cc [options]\n");
		printf("\n");
		printf("The options are as follows:\n");
		printf("   --help, -?               Write this text and exit\n");
		printf("   -v, --version            Write the version information and exit\n");
		printf("   --log:<log>=<level>      Write detailed system log information to stderr\n");
		printf("                            Where <log> is one of the following:\n");
		printf("                            'hash' -- Log the hash table operations\n");
		printf("                            'sym'  -- Log the symbol table operations\n");
		printf("                            'lex'  -- Log the scanner functions\n");
		printf("                            'yacc' -- Log the parser functions\n");
		printf("\n");
		printf("                            Where <level> is one of the following:\n");
		printf("                            0 -- No information will be logged at all\n");
		printf("                            1 -- Severe and unrecoverable errors will be logged\n");
		printf("                            2 -- Errors which are unrecoverable but the program can still execute\n");
		printf("                            3 -- Warnings and recoverable errors are logged\n");
		printf("                            4 -- Informational messages are logged\n");
		printf("                            5 -- Functional Entry/Exits are logged\n");
		printf("                            6 -- Parameters are logged (passed and returned)\n");
		printf("                            7 -- Debug messages are logged\n");
		printf("                            8 -- High volume debugging messages are logged (crazy amounts of data)\n");

		quit = 1;
	}

	if (quit) exit(1);
}


//
// -- This is the main entry point
//    ----------------------------
int main(int argc, char *argv[])
{
	extern int yyparse(void);
	extern SourceFile *ast;
	extern int pErrors;
	extern int semantErrors;
	
	
	InitializeCompiler();
	ParseCommandLine(argc, argv);
	dbgLog.OpenDebugFiles();
	
	printf("Parsing %s\n", srcFile.c_str());
	yyparse();
	
	if (pErrors) {
		fprintf(stderr, "%d Syntax Error%s; exiting\n", pErrors, pErrors>1?"s":"");
		dbgLog.CloseDebugFiles();
		exit(EXIT_FAILURE);
	}
	
	printf("Beginning semantic analysis\n");
	ast->semant();
	
	if (semantErrors) {
		fprintf(stderr, "%d Semantic Error%s; exiting\n", semantErrors, semantErrors>1?"s":"");
		dbgLog.CloseDebugFiles();
		exit(EXIT_FAILURE);
	}
	
	dbgLog.CloseDebugFiles();

	return EXIT_SUCCESS;
}
