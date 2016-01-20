//===================================================================================================================
//
// stringtab.cc -- the implementation of the functions needed for managing the string table (and some globals)
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/10  Initial   v0.0    ADCL  Initial version -- leveraged from the small-ada compiler
//
//===================================================================================================================

#include "stringtab.hh"

#include <cstdlib>
#include <utility>


//
// -- Some global variables
//    ---------------------
IdentifierTable idTable;
NumberTable numTable;
StringTable strTable;

long int Entry::_NextID = 1;


//-------------------------------------------------------------------------------------------------------------------
// Entry::strlwr() -- since we are not case sensitive, we convert all identifiers to lower case.
//-------------------------------------------------------------------------------------------------------------------
std::string Entry::strlwr(const char *s)
{
    std::string rv;

    while (*s) {
        rv += tolower(*s);
        s ++;
    }

    return rv;
}
