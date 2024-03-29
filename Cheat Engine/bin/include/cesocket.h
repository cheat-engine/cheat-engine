#ifndef CESOCKET_H
#define CESOCKET_H

#include <stdarg.h>
#include <stddef.h>

#define SOCK_STREAM 1
#define AF_UNIX 1
#define SOL_SOCKET 1
#define SO_REUSEADDR 2

typedef unsigned int __socklen_t;

#ifndef __socklen_t_defined
typedef __socklen_t socklen_t;
# define __socklen_t_defined
#endif


#ifdef __APPLE__
typedef unsigned char sa_family_t;
struct  sockaddr_un {
    unsigned char   sun_len;        /* sockaddr len including null */
    sa_family_t     sun_family;     /* [XSI] AF_UNIX */
    char            sun_path[108];  /* [XSI] path name (gag) */
};
#else
typedef unsigned short int sa_family_t;
struct sockaddr_un
  {
    sa_family_t sun_family;
    char sun_path[108];		/* Path name.  */
  };
#endif

#ifdef __APPLE__
# define SUN_LEN(su) (sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
#else
# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path) + strlen ((ptr)->sun_path))
#endif

int socket(int domain, int type, int protocol);
int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);
int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);                  
int listen(int sockfd, int backlog);
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int close(int fd);

#endif
