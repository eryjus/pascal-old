//===================================================================================================================
//
// debug.cc -- implementation of the debugging class (system)
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/07  Initial   v0.0    ADCL  Initial version -- leveraged from the pascal compiler, but made C++
// 2016-02-10            v0.0    ADCL  You might notice a change in MOST of this source file.  These changes are
//                                     due to changing how the file is intended (using spaces rather than tabs).
//                                     In addition, trailing spaces have been removed.
//
//===================================================================================================================


#include "debug.hh"

#include <cstdarg>
#include <cstring>


extern void yyerror(const char *);
extern std::string srcFile;


//
// -- A static member that needs to be initialized at compile time
//    ------------------------------------------------------------
const std::string DebugLog::loggerNames[LOG_LAST + 1] = {
        "  Hash",
        "   Sym",
        "   Lex",
        "  Yacc",
        "Semant",
};


//
// -- This is the debugging log instance
//    ----------------------------------
DebugLog dbgLog;


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::DebugLog() -- default constructor
//-------------------------------------------------------------------------------------------------------------------
DebugLog::DebugLog(void)
{
    int l;

    for (l = LOG_FIRST; l < LOG_LAST; l ++) {
        debugFiles[l] = NULL;
        debugFileNames[l] = "";
        debugLevels[l] = LOG_SEVERE;
    }
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::~DebugLog() -- default destructor
//-------------------------------------------------------------------------------------------------------------------
DebugLog::~DebugLog(void)
{
    CloseDebugFiles();
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::OpenDebugFiles() -- Open all debug log files
//-------------------------------------------------------------------------------------------------------------------
void DebugLog::OpenDebugFiles(void)
{
    int l;

    for (l = LOG_FIRST; l <= LOG_LAST; l ++) {
        if (debugFileNames[l] == "") debugFiles[l] = fopen(debugFileNames[l].c_str(), "w");
        if (!debugFiles[l]) debugFiles[l] = stderr;
    }
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::CloseDebugFiles() -- Close all open files
//-------------------------------------------------------------------------------------------------------------------
void DebugLog::CloseDebugFiles(void)
{
    int l;

    for (l = LOG_FIRST; l < LOG_LAST; l ++) {
        if (debugFiles[l] && debugFiles[l] != stderr) fclose(debugFiles[l]);
    }
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::LoggingLevel() -- Set the looging level for the specified logger
//-------------------------------------------------------------------------------------------------------------------
void DebugLog::LoggingLevel(Logger logger, LogLevel level)
{
    if (logger < LOG_FIRST || logger > LOG_LAST) return;
    debugLevels[logger] = level;
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::LoggingLevel() -- Get the looging level for the specified logger
//-------------------------------------------------------------------------------------------------------------------
LogLevel DebugLog::LoggingLevel(Logger logger)
{
    if (logger < LOG_FIRST || logger > LOG_LAST) return LOG_NONE;
    return debugLevels[logger];
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::LoggingFile() -- Set the looging file name
//-------------------------------------------------------------------------------------------------------------------
void DebugLog::LoggingFile(Logger logger, std::string &name)
{
    if (logger < LOG_FIRST || logger > LOG_LAST) return;
    debugFileNames[logger] = name;
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::LoggingFile() -- Get the looging file name
//-------------------------------------------------------------------------------------------------------------------
std::string DebugLog::LoggingFile(Logger logger)
{
    if (logger < LOG_FIRST || logger > LOG_LAST) return NULL;
    return debugFileNames[logger];
}


//-------------------------------------------------------------------------------------------------------------------
// DebugLog::WriteLog() -- Working function to write to the log
//-------------------------------------------------------------------------------------------------------------------
void DebugLog::WriteLog(Logger log, LogLevel lvl, std::string file, int line, std::string text, ...)
{
#define MAX_BUF_LEN		2048
#define fnLen 20
    char buf[MAX_BUF_LEN];
    char fileLoc[MAX_BUF_LEN];
    va_list args;

    sprintf(fileLoc, "%s:%d", file.c_str(), line);
    std::string fLoc(fileLoc);
    if (fLoc.length() > fnLen) fLoc = fLoc.substr(fLoc.length() - fnLen);

    va_start(args, text);
    vsprintf(buf, text.c_str(), args);
    va_end(args);

    if (debugLevels[log] >= lvl) {
        fprintf(debugFiles[log], GREEN HI "%s(%d) " BLUE HI "[...%-*.*s]:" RESET " %s\n",
                loggerNames[log].c_str(), lvl, fnLen, fnLen, fLoc.c_str(), buf);
    }
}
