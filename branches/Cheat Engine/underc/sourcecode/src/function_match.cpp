/* function_match.cpp
 * Matching actual with formal arguments and overload resolution
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include "function_match.h"
#include "expressions.h"
#include "templates.h"
#include "common.h"

#define FOR(i,n) for(i = 0; i < (n); i++)

TypeDistance
FunctionMatch::list_match(TypeList& tl, Type t, bool dir)
//----------------------------------------------------
{
 TypeDistance td, min_td=NO_MATCH;
 TypeList::iterator tli;
 int i, min_idx = -1;
 for(i = 0, tli = tl.begin(); tli != tl.end(); ++tli, ++i) {
   if (dir) td = match(*tli,t);
      else  td = match(t,*tli);
   if (td < min_td) {
      min_td = td;
      min_idx = i;
   }
 }
 if (min_td != NO_MATCH) match_idx = min_idx;
 return min_td;   
}

MatchResult
FunctionMatch:: user_match()
//----------------------
{
  TypeDistance td_from = NO_MATCH,td_to = NO_MATCH;
  int i,j;
  bool matched;
  TypeList tlist;
  Signature::iterator si;
  FOR(i,n_sig) {
   matched = true;
   si = sigs[i]->begin();
   // this avoids trivial conversions (eg copy construct.)
   // *fix 1.2.2b Don't attempt to use user conversions if we _already_ 
   // have a match for these args! 

   FOR(j,n_arg) {
     Type ft = *si++;   // formal argument type
     Type at = args[j]; // actual argument type
     // are there any constructors of ft we could use?
     if (mmatch[i][j] == NO_MATCH && ft.is_class() && ft.as_class()->get_from_type_list(tlist)) {
         if (mmatch[i][j] == NO_MATCH) {    // this avoids trivial conversions (eg copy construct.)
           td_from = list_match(tlist,at,true);
           mmatch[i][j] = td_from != NO_MATCH ? CONVERT_FROM_MATCH : NO_MATCH;
         }
     }
     // are there any user-defined conversions?
     if (mmatch[i][j] == NO_MATCH && at.is_class() && at.as_class()->get_to_type_list(tlist)) {
        td_to = list_match(tlist,ft,false);
        mmatch[i][j] = td_to != NO_MATCH ? CONVERT_TO_MATCH : NO_MATCH;
     }

     matched = matched && mmatch[i][j] != NO_MATCH;
   } // for all formal args in the function
   // Jump out if we match - not entirely right, since
   // we hafta work out if this is ambiguous!
   if (matched) {
       match_idx = i;
       return MATCHED;
   }

  } // for all candidate signatures
  // no go!
  if (td_from == NO_MATCH && td_to == NO_MATCH) return NO_MATCH_POSSIBLE;
  // one conversion path must be definitely better than the other...
  if (td_from == td_to) return AMBIGUOUS;
  // *fix 1.1.0 if we get this far then we've failed!
  return NO_MATCH_POSSIBLE;
}

MatchResult
FunctionMatch:: apply_match(MatchFunction fmatch)
//-------------------------------------------
{
   int msum_min = NO_MATCH, msum_idx = -1, msum;
   int i,j;
   int msum_sig[MAX_OVERLOAD];
   Signature::iterator si;

   FOR(i,n_sig) {
     si = sigs[i]->begin();
     msum = 0;
     FOR(j,n_arg) {
       //cout << *si << ',';
       if (mmatch[i][j]==NO_MATCH) mmatch[i][j] = fmatch(*si,args[j]);
       msum += mmatch[i][j];
       si++;
     }
     //cout << endl;
     msum_sig[i] = msum;
     if (msum < msum_min) {
       msum_min = msum;
       msum_idx = i;
     }
   } // FOR(i,n_sig)...
   if (msum_min < NO_MATCH) { // there was a minimum!!
     match_idx = msum_idx;
     // but was it unique??  
     msum_sig[msum_idx] = NO_MATCH;
     FOR(i,n_sig) 
       if (msum_sig[i] == msum_min) {
         other_match_idx = i;
         return AMBIGUOUS;
       }
     return MATCHED;
    }
    else return NO_MATCH_POSSIBLE;
}

bool 
FunctionMatch::function_match(const FunctionEntry& fe, Signature& sig)
//--------------------------------------------------------------------
{
  int i,j;
  ostrstream errs(m_err_buff,MAX_ERR);
  m_fe = (FunctionEntry *)&fe;  // cast away const...
  m_sig = &sig;
  n_arg = sig.size();  

  // If this is a template function, then it must have first attack!
  if (fe.get_template() != NULL) {
    TemplateEntry *templ = fe.get_template();
    if (templ->match(sig.type_list())) {
         match_idx = templ->match_index();
         return true;
    } else
      match_idx = -1;
      // but was it a complete failiure? There may be other functions in the overloaded set
      // Or this was a class method template (these do not require matching)
      if (templ->no_instances() == fe.size() && !templ->is_method()) {
        errs << templ->last_error() << ends;
        return false;
      }
  }

  FunctionEntry::iterator fei = fe.begin();
 
  // pick up all the signatures from the overloaded set 
  // which _in principle_ could match our argument list
  n_sig = 0;
  for(fei = fe.begin();  fei != fe.end();  ++fei) {
     if (! (*fei)->can_match(n_arg)) continue;
     FOR(j,n_arg) mmatch[n_sig][j] = NO_MATCH;
     sigs[n_sig++] = (*fei)->signature();
     // *fix 1.2.2 (Eric) Check this limit before it does any damage!
     if (n_sig >= MAX_OVERLOAD) { 
       errs << "Maximum number of function overloads is " << MAX_OVERLOAD << endl << ends;
       return false;
     }

  }
  // there was no function which could match this number of arguments!
  // *fix 1.2.0 We needed 'ends' here to terminate string (output crap on Linux!)
  if (n_sig==0) {
      errs << "Function cannot match " << n_arg << " parameters" << ends;
      return false;
  } 

  //*SJD* HACK - assuming there's only one routine for stdarg-style
  // arbitrary arglists - use the _signature_ size!
  if (fe.back()->stdarg()) n_arg = sigs[0]->size();

  Signature::iterator si = sig.begin();
  FOR(j,n_arg) args[j] = *si++;

  // n_sig might well be 1 at this point, but we go through the procedure
  // to establish whether we can make the necessary conversions.

  MatchResult mr;

  mr = apply_match(trivial_match);
  if (mr==AMBIGUOUS) goto ambiguous_match;
  if (mr==MATCHED) return true;

  mr = apply_match(promote_match);
  if (mr==AMBIGUOUS) goto ambiguous_match;
  if (mr==MATCHED) return true;

  mr = apply_match(std_match);
  if (mr==AMBIGUOUS) goto ambiguous_match;
  if (mr==MATCHED) return true;

  // we have no option left but to try out user-defined conversions

  mr = user_match();
  if (mr==AMBIGUOUS) goto ambiguous_match;
  if (mr==MATCHED) return true;

// two kinds of bad outcome here!  Let's check which parm cd not
// be converted 
 FOR(i,n_sig) 
   FOR(j,n_arg)  
     if (mmatch[i][j] == NO_MATCH) break;

 try {
  Signature::set_fun_name(m_fe->reference()->name);
 } catch(...) { Signature::set_fun_name("<unknown>"); }
  
 errs << "Could not match " << sig << ";\n" << j << " parm" << ends;
 if (Parser::debug.verbose) {
   try {
   FOR(i,n_sig)
     cerr << *sigs[i] << endl;
   } catch(...) { errs << "bad function match!"; }
 }
 return false;          
 
ambiguous_match:
  string name = m_fe->reference()->name;
  if (name == "[]") return true;  // *hack 0.9.6 operator[]
  Signature::set_fun_name(name);
  // *fix 1.2.2 (Eric) String was not terminated properly - replaced 'endl' by 'ends'
  errs << "Ambiguous match for " << sig << "\n\t"
     << *sigs[match_idx] <<  "\n\t" << *sigs[other_match_idx] << ends;
  return false;
}

void
FunctionMatch::dump_match_matrix()
{
 int i,j;
 if (!n_sig) cerr << "wrong no. of arguments\n";
 FOR(i,n_sig) {
     FOR(j,n_arg) cerr << mmatch[i][j] << ' ';
     cerr << endl;
  }
}

Function *
FunctionMatch::matched_function()
{
  if (m_fe->get_template() != NULL) {
     TemplateInstance *ti = m_fe->get_template()->match_instance();
     if (ti != NULL) return (Function *)ti->data();
     // otherwise, proceed as for normal function...
  } 
  if (match_idx > -1) {
     Signature *sig = sigs[match_idx];
     if (Parser::debug.verbose) {
        Signature::set_fun_name(m_fe->reference()->name);
        cmsg << "*sig:   " << *m_sig << endl;  
        cmsg << "*match: " << *sig   << endl;
     } 
     return m_fe->simple_match(sig);
  }
  else return NULL;
}

MatchList::iterator
FunctionMatch::matches(PExprList el)
{
 int j;
 Signature::iterator si,sia;
 m_match_list.clear();
 if (m_fe->get_template() != NULL) {
  // There are no special conversions when matching a template instance!
  // *fix 1.1.3 Except we _must_ do reference conversions!
   TemplateInstance *ti = m_fe->get_template()->match_instance();
   if (ti != NULL) {
     Signature& sign = *((Function *)ti->data())->signature();
     int sz = sign.size();
     si = sign.begin();
	 sia = m_sig->begin();
     FOR(j,sz) {
	   Type ft = *si++;      // formal arg type of templ. fun
	   Type at = *sia++;     // actual arg type passed
	   TypeDistance d = trivial_match(ft,at);
       if (d != REFERENCE_MATCH) d = STD_MATCH;
       m_match_list.push_back(MatchItem(d,ft));
     }
     return m_match_list.begin();
   } // otherwise, proceed normally...
 } 

 if (el != NULL) { // then there are extra default arguments
  ExprList::iterator eli;
  Signature::iterator si = sigs[match_idx]->begin();
  int i;
  FOR(i,n_arg) si++;
  i = n_arg;
  for(eli = el->begin();  eli != el->end();  ++eli,++si) {
   Type t2 = (*eli)->type();
   Type t1 = *si;
   TypeDistance mtype = match(t1,t2); 
   if (mtype == NO_MATCH) mtype = CONVERT_FROM_MATCH;  // _serious_ fiddle...
   mmatch[match_idx][i++] = mtype;
   n_arg++;
  }
 }
  
 // *fix 1.1.4 When passing a function pointer to a function,
 //  there's special logic involved if that function has been imported.
 // FUN_PTR_MATCH signals this, even though the match may be exact.
 int mtype;
 Type t;
 si = sigs[match_idx]->begin();
 FOR(j,n_arg) {
    mtype = mmatch[match_idx][j];
    t = *si++;
	if (t.is_pointer() && t.is_signature()) mtype = FUN_PTR_MATCH;
    m_match_list.push_back(MatchItem(mtype,t));
 }
 return m_match_list.begin();
}



