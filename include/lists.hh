//===================================================================================================================
//
// lists.hh -- several kinds of generic lists are needed by the comiler; this template class will help
//
// This class does not do any memory management.  It exists simply to keep track of a list of things.  It is
// implemented as a singly linked list.  Since reading this can get a little confusing, here is a quick discussion
// on the structure.
//
// List Node:
//  +-----------------+-----------------+
//  | a pointer to    | a pointer to    |
//  | the thing we    | the next node   |
//  | keeping track   | in the list.    |
//  | of in the list. |                 |
//  +-----------------+-----------------+
//
// -----------------------------------------------------------------------------------------------------------------
//
//    Date     Tracker  Version  Pgmr  Modification
// ----/--/--  -------  -------  ----  ---------------------------------------------------------------------------
// 2016/01/10  Initial   v0.0    ADCL  Initial version -- leveraged from the small-ada compiler
//
//===================================================================================================================

#ifndef __LIST_H__
#define __LIST_H__

template <class T>
class List {
private:
    T *_elem;
    List<T> *_nextList;

public:
    List(T *h, List<T> *n) : _elem(h), _nextList(n) {};

public:
    T *elem(void) const { return _elem; };
    List<T> *next(void) const { return _nextList; };
};

#endif
