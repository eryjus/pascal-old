//===================================================================================================================
//
// debug.hh -- classes and definitions for debugging the compiler
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


#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <string>
#include <cstdio>
#include <cstdlib>

//
// -- The following enums will offer the following debugging levels:
//    0 == No information will be logged at all
//    1 == Severe and unrecoverable errors will be logged
//    2 == Errors which recoverable but the program can still execute
//    3 == Warnings and recoverable errors are logged
//    4 == Informational messages are logged
//    5 == Functional Entry/Exits are logged
//    6 == Parameters are logged (passed and returned)
//    7 == Debug messages are logged
//    8 == High volume debugging messages are logged (hex dumps)
//    -----------------------------------------------------------------
typedef enum {LOG_NONE = 0,
    LOG_SEVERE = 1,
    LOG_ERROR = 2,
    LOG_WARNING = 3,
    LOG_INFO = 4,
    LOG_ENTRY = 5,
    LOG_PARMS = 6,
    LOG_DEBUG = 7,
    LOG_HIDEBUG = 8,
} LogLevel;

class DebugLog {
public:
    //
    // -- These enums indicate the portions of the system that are able to be logged
    //    --------------------------------------------------------------------------
    typedef enum {
        LOG_FIRST = 0,
        LOG_HASH = 0,
        LOG_SYM = 1,
        LOG_LEX = 2,
        LOG_YACC = 3,
        LOG_SEMANT = 4,
        LOG_LAST = 4	// specially defined for array sizing
    } Logger;

private:
    //
    // -- strings that match the enums above
    //    ----------------------------------
    static const std::string loggerNames[LOG_LAST + 1];

private:
    //
    // -- these variables are used to maintain all the logs
    //    -------------------------------------------------
    std::string debugFileNames[LOG_LAST + 1];
    FILE *debugFiles[LOG_LAST + 1];
    LogLevel debugLevels[LOG_LAST + 1];

public:
    DebugLog();
    ~DebugLog();

public:
    void OpenDebugFiles(void);
    void CloseDebugFiles(void);

public:
    void LoggingLevel(Logger logger, LogLevel level);
    LogLevel LoggingLevel(Logger logger);

    void LoggingFile(Logger logger, std::string &name);
    std::string LoggingFile(Logger logger);

    void WriteLog(Logger log, LogLevel lvl, std::string file, int line, std::string text, ...);
};


//
// -- These 2 macros are used to issue data into a log
//    ------------------------------------------------
#define Log(l,s,m,...)      do {                                                            \
                                if (!m) break;                                              \
                                dbgLog.WriteLog(l, s, __FILE__, __LINE__, m, ##__VA_ARGS__);\
                            } while (0)

#define Fatal(l,s,m,...)	do {                                                            \
                                if (!m) break;                                              \
                                dbgLog.WriteLog(l, s, __FILE__, __LINE__, m, ##__VA_ARGS__);\
                                exit(EXIT_FAILURE);                                         \
                            } while (0)


//
// -- We will implement a global variable for the system logging
//    ----------------------------------------------------------
extern DebugLog dbgLog;


//
// -- ANSI escape sequences
//    ---------------------
#define ESC "\x1b["
#define RESET ESC "0m"
#define RED ESC "31m"
#define GREEN ESC "32m"
#define BLUE ESC "34m"
#define MAGENTA ESC "35m"
#define CYAN ESC "36m"
#define HI ESC "1m"


#endif
