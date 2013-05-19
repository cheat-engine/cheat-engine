/*
** $Id: ltable.c,v 2.32.1.2 2007/12/28 15:32:23 roberto Exp $
** Lua tables (hash)
** See Copyright Notice in lua.h
*/


/*
** Implementation of tables (aka arrays, objects, or hash tables).
** Tables keep its elements in two parts: an array part and a hash part.
** Non-negative integer keys are all candidates to be kept in the array
** part. The actual size of the array is the largest `n' such that at
** least half the slots between 0 and n are in use.
** Hash uses a mix of chained scatter table with Brent's variation.
** A main invariant of these tables is that, if an element is not
** in its main position (i.e. the `original' position that its hash gives
** to it), then the colliding element is in its own main position.
** Hence even when the load factor reaches 100%, performance remains good.
*/

#include <math.h>
#include <string.h>
#include <stdio.h>  
  /* debug */

#define ltable_c
#define LUA_CORE

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "ltable.h"
#include "lnum.h"


/*
** max size of array part is 2^MAXBITS
*/
#if LUAI_BITSINT > 26
#define MAXBITS		26
#else
#define MAXBITS		(LUAI_BITSINT-2)
#endif

#define MAXASIZE	(1 << MAXBITS)


#define hashpow2(t,n)      (gnode(t, lmod((n), sizenode(t))))
  
#define hashstr(t,str)  hashpow2(t, (str)->tsv.hash)
#define hashboolean(t,p)        hashpow2(t, p)

#ifdef LUA_TINT
# define hashint(t,i)    hashpow2(t,i)
#endif

/*
** for some types, it is better to avoid modulus by power of 2, as
** they tend to have many 2 factors.
*/
#define hashmod(t,n)	(gnode(t, ((n) % ((sizenode(t)-1)|1))))


#define hashpointer(t,p)	hashmod(t, IntPoint(p))


#define dummynode		(&dummynode_)

static const Node dummynode_ = {
  {{NULL}, LUA_TNIL},  /* value */
  {{{NULL}, LUA_TNIL, NULL}}  /* key */
};


/*
** number of ints of lua_Number used for hashing
*
* OS X Intel (1.5.6): 
*   'sizeof(long double)'==16 but gives "Bus error" if NUM_INTS is 4.
*   The error does not seem to happen exactly at the read, but is most
*   likely caused by random trailing bits that cause inconsistent hashes.
*   To avoid this, fix NUM_INTS to 3 (12 bytes).
*
* Linux x86 (2.6.24, gcc 4.2.4):
*   'sizeof(long double)' is 12, which works apart from LNUM_COMPLEX
*   (and LNUM_LDOUBLE) mode (if optimized >= -O2). There, bytes 11 and 12 
*   seem to have random bits and cause inconsistent hashes. The x87 FPU 
*   long double is 10 bytes (80 bits). We set NUM_INTS to 2 and don't use
*   the last 2 bytes for hashing, at all.
*
* Linux x86_64 (2.6.28, gcc 4.3.3):
*   'sizeof(long double)' is 16, but only 3 ints seem to be stable (with
*   LNUM_COMPLEX and LNUM_LDOUBLE together only 80 bits are stable).
*
* NOTE: We could simplify this by simply using always 2 ints or
*   '(sizeof(lua_Number)/sizeof(int))-1' for hashing the 'long double's
*/
#ifndef LNUM_LDOUBLE
/* Anything not LNUM_LDOUBLE */
# define NUM_INTS (sizeof(lua_Number)/sizeof(int))
#else
# if defined(__x86_64) || defined(__i386__)
/* Linux x86_64 or x86 */
#  ifdef LNUM_COMPLEX
#   define NUM_INTS 2
#  else
#   define NUM_INTS 3
#  endif
# else
/* Default (some systems will need the -1, some won't) */
#  define NUM_INTS ((sizeof(lua_Number)/sizeof(int))-1)
# endif
#endif


/*
** hash for lua_Numbers
**
** for non-complex modes, never called with 'lua_Integer' value range (s.a. 0)
*
* Note: Linux x86(_64) gcc 4.x, -O2/O3 and no '-fno-strict-aliasing' will 
*       optimize out essential stuff from here, unless 'union' is used.
*       There is no automatic way to sniff whether compiler is having strict
*       aliasing optimizations on (use '-DNO_STRICT_ALIASING' to enable the
*       potentially faster code path that expects they are not on).
*/
static Node *hashnum (const Table *t, const lua_Number v) {
  unsigned int i,sum;
#ifdef NO_STRICT_ALIASING
    /* For compilers not using "strict aliasing" optimizations (or when compiling
    * with '-fno-strict-aliasing').
    */
  const unsigned int *a= cast(const unsigned int*, &v);
  sum= a[0]; for (i = 1; i < NUM_INTS; i++) sum += a[i];
#else
  /*
  * This code is "strict aliasing" safe (to be used with gcc 4.x -O2 or above, 
  * if no '-fno-strict-aliasing' is specified). Otherwise you'll be screwed. 
  * Casting via 'char*' or 'void*' should do the same, but doesn't.
  *
  * Seems gcc enables strict aliasing even if '--std=c99' is not used.
  * There is no predefined macro to detect, whether the compiler is doing
  * strict aliasing optimizations or not. Bohoo...  --AKa 7-Apr-2009
  */
  union {
    lua_Number v;
    unsigned int a[NUM_INTS];
  } u;
  lua_assert( sizeof(u)==sizeof(v) );
  u.v= v;
  sum= u.a[0]; for (i = 1; i < NUM_INTS; i++) sum += u.a[i];
#endif
  return hashmod(t, sum);
}

/*
** returns the `main' position of an element in a table (that is, the index
** of its hash value)
**
** Floating point numbers with integer value give the hash position of the
** integer (so they use the same table position).
*/
static Node *mainposition (const Table *t, const TValue *key) {
  switch (ttype(key)) {
#ifdef LUA_TINT
    case LUA_TINT:
      return hashint(t,ivalue(key));
#endif
    case LUA_TNUMBER: {
#ifdef LUA_TINT
      lua_Integer i;
      if (tt_integer_valued(key,&i)) 
        return hashint(t, i);
# ifdef LNUM_COMPLEX
      /* Complex numbers are hashed by their scalar part. Pure imaginary values
       * with scalar 0 or -0 should give same hash.
       */
      if (nvalue_img_fast(key)!=0 && luai_numeq(nvalue_fast(key),0))
        return gnode(t, 0);  /* 0 and -0 to give same hash */
# endif
#else
      if (luai_numeq(nvalue(key),0)) return gnode(t, 0);  /* 0 and -0 to give same hash */
#endif
      return hashnum(t,nvalue_fast(key));
    }
    case LUA_TSTRING:
      return hashstr(t, rawtsvalue(key));
    case LUA_TBOOLEAN:
      return hashboolean(t, bvalue(key));
    case LUA_TLIGHTUSERDATA:
      return hashpointer(t, pvalue(key));
    default:
      return hashpointer(t, gcvalue(key));
  }
}


/*
** returns the index for `key' if `key' is an appropriate key to live in
** the array part of the table, -1 otherwise.
**
** Anything <=0 is taken as not being in the array part.
*/
static int arrayindex (const TValue *key, int max) {
  lua_Integer i;
  switch( ttype(key) ) {
#ifdef LUA_TINT
    case LUA_TINT:      i= ivalue(key); break;
#endif
    case LUA_TNUMBER:   if (tt_integer_valued(key,&i)) break;
    default:            return -1;  /* not to be used as array index */
  }
  return (i <= max) ? cast_int(i) : -1;
}


/*
** returns the index of a `key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signalled by -1.
*/
static int findindex (lua_State *L, Table *t, StkId key) {
  int i;
  if (ttisnil(key)) return -1;  /* first iteration */
  i = arrayindex(key, t->sizearray);
  if (i>0)  /* inside array part? */
    return i-1;  /* yes; that's the index (corrected to C) */
  else {
    Node *n = mainposition(t, key);
    do {  /* check whether `key' is somewhere in the chain */
      /* key may be dead already, but it is ok to use it in `next' */
      if (luaO_rawequalObj(key2tval(n), key) ||
            (ttype(gkey(n)) == LUA_TDEADKEY && iscollectable(key) &&
             gcvalue(gkey(n)) == gcvalue(key))) {
        i = cast_int(n - gnode(t, 0));  /* key index in hash table */
        /* hash elements are numbered after array ones */
        return i + t->sizearray;
      }
      else n = gnext(n);
    } while (n);
    luaG_runerror(L, "invalid key to " LUA_QL("next"));  /* key not found */
    return 0;  /* to avoid warnings */
  }
}


int luaH_next (lua_State *L, Table *t, StkId key) {
  int i = findindex(L, t, key);  /* find original element */
  for (i++; i < t->sizearray; i++) {  /* try first array part */
    if (!ttisnil(&t->array[i])) {  /* a non-nil value? */
      setivalue(key, i+1);
      setobj2s(L, key+1, &t->array[i]);
      return 1;
    }
  }
  for (i -= t->sizearray; i < sizenode(t); i++) {  /* then hash part */
    if (!ttisnil(gval(gnode(t, i)))) {  /* a non-nil value? */
      setobj2s(L, key, key2tval(gnode(t, i)));
      setobj2s(L, key+1, gval(gnode(t, i)));
      return 1;
    }
  }
  return 0;  /* no more elements */
}


/*
** {=============================================================
** Rehash
** ==============================================================
*/


static int computesizes (int nums[], int *narray) {
  int i;
  int twotoi;  /* 2^i */
  int a = 0;  /* number of elements smaller than 2^i */
  int na = 0;  /* number of elements to go to array part */
  int n = 0;  /* optimal size for array part */
  for (i = 0, twotoi = 1; twotoi/2 < *narray; i++, twotoi *= 2) {
    if (nums[i] > 0) {
      a += nums[i];
      if (a > twotoi/2) {  /* more than half elements present? */
        n = twotoi;  /* optimal size (till now) */
        na = a;  /* all elements smaller than n will go to array part */
      }
    }
    if (a == *narray) break;  /* all elements already counted */
  }
  *narray = n;
  lua_assert(*narray/2 <= na && na <= *narray);
  return na;
}


static int countint (const TValue *key, int *nums) {
  int k = arrayindex(key,MAXASIZE);
  if (k>0) {  /* appropriate array index? */
    nums[ceillog2( cast(unsigned int,k) )]++;  /* count as such */
    return 1;
  }
  else
    return 0;
}


static int numusearray (const Table *t, int *nums) {
  int lg;
  int ttlg;  /* 2^lg */
  int ause = 0;  /* summation of `nums' */
  int i = 1;  /* count to traverse all array keys */
  for (lg=0, ttlg=1; lg<=MAXBITS; lg++, ttlg*=2) {  /* for each slice */
    int lc = 0;  /* counter */
    int lim = ttlg;
    if (lim > t->sizearray) {
      lim = t->sizearray;  /* adjust upper limit */
      if (i > lim)
        break;  /* no more elements to count */
    }
    /* count elements in range (2^(lg-1), 2^lg] */
    for (; i <= lim; i++) {
      if (!ttisnil(&t->array[i-1]))
        lc++;
    }
    nums[lg] += lc;
    ause += lc;
  }
  return ause;
}


static int numusehash (const Table *t, int *nums, int *pnasize) {
  int totaluse = 0;  /* total number of elements */
  int ause = 0;  /* summation of `nums' */
  int i = sizenode(t);
  while (i--) {
    Node *n = &t->node[i];
    if (!ttisnil(gval(n))) {
      ause += countint(key2tval(n), nums);
      totaluse++;
    }
  }
  *pnasize += ause;
  return totaluse;
}


static void setarrayvector (lua_State *L, Table *t, int size) {
  int i;
  luaM_reallocvector(L, t->array, t->sizearray, size, TValue);
  for (i=t->sizearray; i<size; i++)
     setnilvalue(&t->array[i]);
  t->sizearray = size;
}


static void setnodevector (lua_State *L, Table *t, int size) {
  int lsize;
  if (size == 0) {  /* no elements to hash part? */
    t->node = cast(Node *, dummynode);  /* use common `dummynode' */
    lsize = 0;
  }
  else {
    int i;
    lsize = ceillog2(size);
    if (lsize > MAXBITS)
      luaG_runerror(L, "table overflow");
    size = twoto(lsize);
    t->node = luaM_newvector(L, size, Node);
    for (i=0; i<size; i++) {
      Node *n = gnode(t, i);
      gnext(n) = NULL;
      setnilvalue(gkey(n));
      setnilvalue(gval(n));
    }
  }
  t->lsizenode = cast_byte(lsize);
  t->lastfree = gnode(t, size);  /* all positions are free */
}


static void resize (lua_State *L, Table *t, int nasize, int nhsize) {
  int i;
  int oldasize = t->sizearray;
  int oldhsize = t->lsizenode;
  Node *nold = t->node;  /* save old hash ... */
  if (nasize > oldasize)  /* array part must grow? */
    setarrayvector(L, t, nasize);
  /* create new hash part with appropriate size */
  setnodevector(L, t, nhsize);  
  if (nasize < oldasize) {  /* array part must shrink? */
    t->sizearray = nasize;
    /* re-insert elements from vanishing slice */
    for (i=nasize; i<oldasize; i++) {
      if (!ttisnil(&t->array[i]))
        setobjt2t(L, luaH_setint(L, t, i+1), &t->array[i]);
    }
    /* shrink array */
    luaM_reallocvector(L, t->array, oldasize, nasize, TValue);
  }
  /* re-insert elements from hash part */
  for (i = twoto(oldhsize) - 1; i >= 0; i--) {
    Node *old = nold+i;
    if (!ttisnil(gval(old)))
      setobjt2t(L, luaH_set(L, t, key2tval(old)), gval(old));
  }
  if (nold != dummynode)
    luaM_freearray(L, nold, twoto(oldhsize), Node);  /* free old array */
}


void luaH_resizearray (lua_State *L, Table *t, int nasize) {
  int nsize = (t->node == dummynode) ? 0 : sizenode(t);
  resize(L, t, nasize, nsize);
}


static void rehash (lua_State *L, Table *t, const TValue *ek) {
  int nasize, na;
  int nums[MAXBITS+1];  /* nums[i] = number of keys between 2^(i-1) and 2^i */
  int i;
  int totaluse;
  for (i=0; i<=MAXBITS; i++) nums[i] = 0;  /* reset counts */
  nasize = numusearray(t, nums);  /* count keys in array part */
  totaluse = nasize;  /* all those keys are integer keys */
  totaluse += numusehash(t, nums, &nasize);  /* count keys in hash part */
  /* count extra key */
  nasize += countint(ek, nums);
  totaluse++;
  /* compute new size for array part */
  na = computesizes(nums, &nasize);
  /* resize the table to new computed sizes */
  resize(L, t, nasize, totaluse - na);
}



/*
** }=============================================================
*/


Table *luaH_new (lua_State *L, int narray, int nhash) {
  Table *t = luaM_new(L, Table);
  luaC_link(L, obj2gco(t), LUA_TTABLE);
  t->metatable = NULL;
  t->flags = cast_byte(~0);
  /* temporary values (kept only if some malloc fails) */
  t->array = NULL;
  t->sizearray = 0;
  t->lsizenode = 0;
  t->node = cast(Node *, dummynode);
  setarrayvector(L, t, narray);
  setnodevector(L, t, nhash);
  return t;
}


void luaH_free (lua_State *L, Table *t) {
  if (t->node != dummynode)
    luaM_freearray(L, t->node, sizenode(t), Node);
  luaM_freearray(L, t->array, t->sizearray, TValue);
  luaM_free(L, t);
}


static Node *getfreepos (Table *t) {
  while (t->lastfree-- > t->node) {
    if (ttisnil(gkey(t->lastfree)))
      return t->lastfree;
  }
  return NULL;  /* could not find a free place */
}



/*
** inserts a new key into a hash table; first, check whether key's main 
** position is free. If not, check whether colliding node is in its main 
** position or not: if it is not, move colliding node to an empty place and 
** put new key in its main position; otherwise (colliding node is in its main 
** position), new key goes to an empty position. 
*/
static TValue *newkey (lua_State *L, Table *t, const TValue *key) {
  Node *mp = mainposition(t, key);
  if (!ttisnil(gval(mp)) || mp == dummynode) {
    Node *othern;
    Node *n = getfreepos(t);  /* get a free place */
    if (n == NULL) {  /* cannot find a free place? */
      rehash(L, t, key);  /* grow table */
      return luaH_set(L, t, key);  /* re-insert key into grown table */
    }
    lua_assert(n != dummynode);
    othern = mainposition(t, key2tval(mp));
    if (othern != mp) {  /* is colliding node out of its main position? */
      /* yes; move colliding node into free position */
      while (gnext(othern) != mp) {
        othern = gnext(othern);  /* find previous */
        lua_assert(othern);   /* LNUM: caught if hashing is inconsistent */ 
      }
      gnext(othern) = n;  /* redo the chain with `n' in place of `mp' */
      *n = *mp;  /* copy colliding node into free pos. (mp->next also goes) */
      gnext(mp) = NULL;  /* now `mp' is free */
      setnilvalue(gval(mp));
    }
    else {  /* colliding node is in its own main position */
      /* new node will go into free position */
      gnext(n) = gnext(mp);  /* chain new position */
      gnext(mp) = n;
      mp = n;
    }
  }
  gkey(mp)->value = key->value; gkey(mp)->tt = key->tt;
  luaC_barriert(L, t, key);
  lua_assert(ttisnil(gval(mp)));
  return gval(mp);
}


/*
** search function for integers
*
* Optimize fetches to the array portion of a table (but do provide matches
* to other 32-bit integer keys as well). The 'key' parameter is 'int' (and not 
* 'lua_Integer') by purpose; values not fitting in an int will be fetched using
* the generic approach (array part won't ever have them).
*/
const TValue *luaH_getint (Table *t, int key) {
  /* (1 <= key && key <= t->sizearray) */
  if (cast(unsigned int, key-1) < cast(unsigned int, t->sizearray))
    return &t->array[key-1];
  else {
#ifdef LUA_TINT
    Node *n = hashint(t, key);
    do {  /* check whether `key' is somewhere in the chain */
      if (ttisint(gkey(n)) && (ivalue(gkey(n)) == key))
#else
    lua_Number key_d= cast_num(key);
    Node *n = hashnum(t, key_d);
    do {  /* check whether `key' is somewhere in the chain */
      if (ttisnumber(gkey(n)) && luai_numeq(nvalue(gkey(n)), key_d))
#endif
        return gval(n);  /* that's it */
      n = gnext(n);
    } while (n);
    return luaO_nilobject;
  }
}


/*
** search function for strings
*/
const TValue *luaH_getstr (Table *t, TString *key) {
  Node *n = hashstr(t, key);
  do {  /* check whether `key' is somewhere in the chain */
    if (ttisstring(gkey(n)) && rawtsvalue(gkey(n)) == key)
      return gval(n);  /* that's it */
    else n = gnext(n);
  } while (n);
  return luaO_nilobject;
}


/*
** main search function
*/
const TValue *luaH_get (Table *t, const TValue *key) {
  Node *n;
  switch (ttype(key)) {
    case LUA_TNIL: return luaO_nilobject;
    case LUA_TSTRING: return luaH_getstr(t, rawtsvalue(key));

#ifdef LUA_TINT
    case LUA_TINT: {
      int i= cast_int( ivalue(key) );
# ifdef LNUM_INT64
      if (i != ivalue(key)) break;   /* handle non-32 bit separately */
# endif
      return luaH_getint(t,i);
#endif
    }
    case LUA_TNUMBER: {
      lua_Integer j;
      if (!tt_integer_valued(key,&j)) break;
# ifdef LNUM_INT64
      if (cast_int(j) != j) break;   /* handle non-32 bit separately */
# endif
      return luaH_getint(t,cast_int(j));
    }
  }
  /* general (FP numbers, >32-bit integers, tables etc.) 
  */
  n = mainposition(t, key);
  do {  /* check whether `key' is somewhere in the chain */
    if (luaO_rawequalObj(key2tval(n), key))
      return gval(n);  /* that's it */
    else n = gnext(n);
  } while (n);
  return luaO_nilobject;
}


TValue *luaH_set (lua_State *L, Table *t, const TValue *key) {
  const TValue *p = luaH_get(t, key);
  t->flags = 0;
  if (p != luaO_nilobject)
    return cast(TValue *, p);
  else {
    if (ttisnil(key)) luaG_runerror(L, "table index is nil");
    else if (ttype(key)==LUA_TNUMBER) {
#ifdef LUA_TINT
      /* [3.0] must use the same index slot as [3] */
      lua_Integer i;
      if (tt_integer_valued(key,&i)) {
# ifdef LNUM_INT64
        if (cast_int(i) != i) {  /* does not fit in 32 bits */
          TValue k;
          setivalue(&k, i);
          return newkey(L, t, &k);
        }
# endif
        return luaH_setint(L, t, cast_int(i));
      }
#endif
      if (luai_numisnan(nvalue_fast(key)))
        luaG_runerror(L, "table index is NaN");
    }
    return newkey(L, t, key);
  }
}


TValue *luaH_setint (lua_State *L, Table *t, int key) {
  const TValue *p = luaH_getint(t, key);
  if (p != luaO_nilobject)
    return cast(TValue *, p);
  else {
    TValue k;
    setivalue(&k, key);
    return newkey(L, t, &k);
  }
}


TValue *luaH_setstr (lua_State *L, Table *t, TString *key) {
  const TValue *p = luaH_getstr(t, key);
  if (p != luaO_nilobject)
    return cast(TValue *, p);
  else {
    TValue k;
    setsvalue(L, &k, key);
    return newkey(L, t, &k);
  }
}


static int unbound_search (Table *t, unsigned int j) {
  unsigned int i = j;  /* i is zero or a present index */
  j++;
  /* find `i' and `j' such that i is present and j is not */
  while (!ttisnil(luaH_getint(t, j))) {
    i = j;
    j *= 2;
    if (j > cast(unsigned int, MAX_INT)) {  /* overflow? */
      /* table was built with bad purposes: resort to linear search */
      i = 1;
      while (!ttisnil(luaH_getint(t, i))) i++;
      return i - 1;
    }
  }
  /* now do a binary search between them */
  while (j - i > 1) {
    unsigned int m = (i+j)/2;
    if (ttisnil(luaH_getint(t, m))) j = m;
    else i = m;
  }
  return i;
}


/*
** Try to find a boundary in table `t'. A `boundary' is an integer index
** such that t[i] is non-nil and t[i+1] is nil (and 0 if t[1] is nil).
*/
int luaH_getn (Table *t) {
  unsigned int j = t->sizearray;
  if (j > 0 && ttisnil(&t->array[j - 1])) {
    /* there is a boundary in the array part: (binary) search for it */
    unsigned int i = 0;
    while (j - i > 1) {
      unsigned int m = (i+j)/2;
      if (ttisnil(&t->array[m - 1])) j = m;
      else i = m;
    }
    return i;
  }
  /* else must find a boundary in hash part */
  else if (t->node == dummynode)  /* hash part is empty? */
    return j;  /* that is easy... */
  else return unbound_search(t, j);
}



#if defined(LUA_DEBUG)

Node *luaH_mainposition (const Table *t, const TValue *key) {
  return mainposition(t, key);
}

int luaH_isdummy (Node *n) { return n == dummynode; }

#endif
