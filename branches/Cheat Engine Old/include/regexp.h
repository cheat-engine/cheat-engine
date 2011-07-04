// regexp.h
// UnderC Development Project, 2002
// These are the POSIX regular expression functions.
// (Interface to the GNU RX library)

#ifndef __REGEXP_H
#define __REGEXP_H

#ifndef __linux__
#pragma dlink rx.dll
#else
#pragma dlink libc.so.6
#endif

typedef unsigned long size_t;

// really aren't interested in all the other fields...
struct regex_t {
  long p1,p2;
  size_t re_nsub;
  char rest[300];
}; 

/* Type for byte offsets within the string.  POSIX mandates this.  */
typedef int regoff_t;

struct regmatch_t
{
  regoff_t rm_so; 		// Byte offset from string's start to substring's start.  
  regoff_t rm_eo;  		// Byte offset from string's start to substring's end.  
// *fix 1.2.2 The GLIB implementation is slightly different....
#ifndef __linux__
  regoff_t final_tag;		// data from the cut operator (only pmatch[0]) 
#endif
};

// POSIX `cflags' bits (i.e., information for `regcomp').
const int REG_EXTENDED = 1, REG_ICASE = 1 << 1, REG_NEWLINE = 1 << 2, REG_NOSUB = 1 << 3;

// POSIX 'eflags' biits (for regexec)
const int REG_NOTBOL = 1, REG_NOTEOL = 1 << 1, REG_ALLOC_REGS = 1 << 2;

#define REG_NOERROR	0		/* Success.  */
#define REG_NOMATCH	1		/* Didn't find a match (for regexec).  */

// this stuff comes directly from inst-rxposix.h
/* POSIX regcomp return error codes.  
 * (In the order listed in the standard.)  
 */
#define REG_BADPAT	2		/* Invalid pattern.  */
#define REG_ECOLLATE	3		/* Not implemented.  */
#define REG_ECTYPE	4		/* Invalid character class name.  */
#define REG_EESCAPE	5		/* Trailing backslash.  */
#define REG_ESUBREG	6		/* Invalid back reference.  */
#define REG_EBRACK	7		/* Unmatched left bracket.  */
#define REG_EPAREN	8		/* Parenthesis imbalance.  */ 
#define REG_EBRACE	9		/* Unmatched \{.  */
#define REG_BADBR	10		/* Invalid contents of \{\}.  */
#define REG_ERANGE	11		/* Invalid range end.  */
#define REG_ESPACE	12		/* Ran out of memory.  */
#define REG_BADRPT	13		/* No preceding re for repetition op.  */

/* Error codes we've added.  
 */
#define REG_EEND	14		/* Premature end.  */
#define REG_ESIZE	15		/* Compiled pattern bigger than 2^16 bytes.  */
#define REG_ERPAREN	16		/* Unmatched ) or \); not returned from regcomp.  */

extern "C" {
 int regcomp  (regex_t * preg, const char * pattern, int cflags);
 int regerror (int errcode, const regex_t *preg,
   		  char *errbuf, size_t errbuf_size);
 int regexec (const regex_t *preg, const char *str,
                 size_t nmatch, regmatch_t pmatch[], int eflags);
 int regnexec (const regex_t *preg, const char *str,int sz,
                 size_t nmatch, regmatch_t pmatch[], int eflags);
 void regfree (regex_t *preg);
}

#pragma dlink

#endif