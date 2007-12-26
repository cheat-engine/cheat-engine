/* FUNCTION_MATCH.H 
*/
#ifndef __FUNCTION_MATCH_H
#define __FUNCTION_MATCH_H

#include "types.h"
#include "function.h"

ostream& operator << (ostream& os, Signature& sig);
ostream& operator <<(ostream& os, Function& f);

const int MAX_OVERLOAD = 35, MAX_ARGUMENTS = 20, MAX_ERR = 512;

class MatchItem {
protected:
  int m_match;
  Type m_type;
public:
  MatchItem(int match, Type type) : m_match(match),m_type(type) {}
  int   match() { return m_match; }
  Type  type()  { return m_type; }
};

enum MatchResult { AMBIGUOUS, MATCHED, NO_MATCH_POSSIBLE };
typedef std::list<MatchItem> MatchList;

class FunctionMatch {
private:
 typedef TypeDistance MatchMatrix[MAX_OVERLOAD][MAX_ARGUMENTS];
 MatchMatrix mmatch;
 int match_idx, other_match_idx,n_sig,n_arg;
 Signature *sigs[MAX_OVERLOAD];
 Type args[MAX_ARGUMENTS];
 MatchList m_match_list;
 FunctionEntry *m_fe;
 Signature *m_sig;
 char m_err_buff[MAX_ERR];

public:
 TypeDistance list_match(TypeList& tl, Type t, bool dir);
 MatchResult user_match();
 MatchResult apply_match(MatchFunction fmatch);
 bool function_match(const FunctionEntry& fe, Signature& sig);
 char *error()  { return m_err_buff; }
 void dump_match_matrix();
 Function *matched_function();
 MatchList::iterator matches(PExprList el);
};


#endif

