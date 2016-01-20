//===================================================================================================================
//
// stringtab.hh -- the compiler needs to keep track of several strings and this template will assist in this task.
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/10  Initial   v0.0    ADCL  Initial version -- leveraged from the small-ada compiler
//
//===================================================================================================================

#ifndef __STRINGTABLES_H__
#define __STRINGTABLES_H__

#include "lists.hh"

#include <string>
#include <iostream>


//
// -- The Entry class is a parent class to all the string table entry types.  It will be used to describe a
//    string of many types (string literal, numeric literal, and identifier to name the key ones)
//    -----------------------------------------------------------------------------------------------------
class Entry {
private:
    static long int _NextID;
    long int ID;
    std::string KeyValue;

public:
    std::string GetKeyValue(void) const { return KeyValue; };
    const char *GetKeyString(void) const { return KeyValue.c_str(); }

public:
    Entry(const std::string &s) : ID(_NextID ++), KeyValue(s) {};

public:
    static std::string strlwr(const std::string &s) { return strlwr(s.c_str()); };
    static std::string strlwr(const char *s);
    bool Equals(const std::string &s) { return (s == KeyValue); };
    std::string GetString(void) const { return KeyValue; }
};


//
// -- The _StringTable class is a template class for the actual table.  There will be several different tables
//    which makes management of the strings a bit more complicated, but makes emitting the code much easier to
//    deal with later.  Notice the List<T> implementation of the actual table structure.
//    --------------------------------------------------------------------------------------------------------
template <class T>
class _StringTable  {
private:
    List<T> *table;
    unsigned long _nextIndex;

public:
    _StringTable() : table(NULL), _nextIndex(1) {};

protected:
    T *add(const std::string &s) {
        T *rv = lookup(s);
        if (!rv) { rv = new T(s); table = new List<T>(rv, table); }
        return rv;
    };

public:
    T *lookup(const std::string &s) {
        List<T> *wrk = table;
        while (wrk && !wrk->elem()->Equals(s)) wrk = wrk->next();
        return (wrk?wrk->elem():NULL);
    };
};


//===================================================================================================================

//
// -- This entry will take care of the critical identifier information, but is still a basic Entry in the table.
//    ----------------------------------------------------------------------------------------------------------
class IdentEntry : public Entry {
public:
    IdentEntry(const std::string &id) : Entry(id) {};
    IdentEntry(const char *id) : Entry(id) {};
};


//
// -- This is the full identifier table
//    ---------------------------------
class IdentifierTable : public _StringTable<IdentEntry> {
public:
    IdentEntry *AddString(const char *s) { return add(Entry::strlwr(s)); }
    IdentEntry *AddString(std::string s) { return add(Entry::strlwr(s)); }
};


//===================================================================================================================

//
// -- This is an number entry into the table
//    --------------------------------------
class NumberEntry : public Entry {
public:
    NumberEntry(const std::string &num) : Entry(num) {};
    NumberEntry(const char *num) : Entry(num) {};
};


//
// -- This is the full number table
//    -----------------------------
class NumberTable : public _StringTable<NumberEntry> {
public:
    NumberEntry *AddString(const char *s) { return add(Entry::strlwr(s)); };
};


//===================================================================================================================

//
// -- This is a string literal entry into the table
//    ---------------------------------------------
class StringEntry : public Entry {
public:
    StringEntry(const std::string &s) : Entry(s) {};
    StringEntry(const char *s) : Entry(s) {};
};


//
// -- This is the full string table
//    -----------------------------
class StringTable : public _StringTable<StringEntry> {
public:
    StringEntry *AddString(const char *s) { return add(s); };
    StringEntry *AddString(std::string &s) { return add(s); };
};


//===================================================================================================================


//
// -- Declare the tables
//    ------------------
extern IdentifierTable idTable;
extern NumberTable numTable;
extern StringTable strTable;

#endif
