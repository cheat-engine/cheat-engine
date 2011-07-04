/* glib.h
 * Fake header for GTK+ imports
*/

#ifndef _GLIB_H
#define _GLIB_H
typedef char gchar;
typedef unsigned char guchar;
typedef int gboolean;
typedef int gint;
typedef unsigned int guint;
typedef long glong;
typedef unsigned long gulong;
typedef short gshort;
typedef unsigned short gushort;
typedef float gfloat;
typedef double gdouble;
typedef void *gpointer;
typedef void *gconstpointer;
typedef int guint32;
typedef short gint16;
typedef unsigned short guint16;
typedef char gint8;
typedef unsigned char guint8;

#define GINT_TO_POINTER(x) ((void *)x)

/* Token types */
typedef enum
{
  G_TOKEN_EOF			=   0,
  
  G_TOKEN_LEFT_PAREN		= '(',
  G_TOKEN_RIGHT_PAREN		= ')',
  G_TOKEN_LEFT_CURLY		= '{',
  G_TOKEN_RIGHT_CURLY		= '}',
  G_TOKEN_LEFT_BRACE		= '[',
  G_TOKEN_RIGHT_BRACE		= ']',
  G_TOKEN_EQUAL_SIGN		= '=',
  G_TOKEN_COMMA			= ',',
  
  G_TOKEN_NONE			= 256,
  
  G_TOKEN_ERROR,
  
  G_TOKEN_CHAR,
  G_TOKEN_BINARY,
  G_TOKEN_OCTAL,
  G_TOKEN_INT,
  G_TOKEN_HEX,
  G_TOKEN_FLOAT,
  G_TOKEN_STRING,
  
  G_TOKEN_SYMBOL,
  G_TOKEN_IDENTIFIER,
  G_TOKEN_IDENTIFIER_NULL,
  
  G_TOKEN_COMMENT_SINGLE,
  G_TOKEN_COMMENT_MULTI,
  G_TOKEN_LAST
} GTokenType;


typedef unsigned int GQuark;
typedef guint GMutex;
typedef int GScanner;
//typedef int GList;
typedef int GHashTable;
typedef int GSList;
typedef int GData;
typedef unsigned int GMemChunk;
typedef int GDestroyNotify;
typedef int GNode;
typedef void (*GCompareFunc)();
typedef void *va_list;

// stuff to go in fake GLIB header
#define G_STMT_START
#define G_STMT_END
#define g_print printf

typedef struct _GList GList;

struct _GList
{
  gpointer data;
  GList *next;
  GList *prev;
};




#endif
