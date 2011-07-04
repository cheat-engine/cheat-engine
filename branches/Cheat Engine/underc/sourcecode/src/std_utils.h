// std_utils.h
// some utility templates for working with std::list
#ifndef __STD_UTILS_H
#define __STD_UTILS_H

namespace utils {
template <class T>
bool add_unique(std::list<T>& ls, T t)
{
// useful little routine for keeping lists unique
  typename std::list<T>::iterator is;
  for(is = ls.begin(); is != ls.end(); ++is)
     if (*is == t) return false;  // already there
  ls.push_back(t);
  return true;
}

template <class T>
T list_item(std::list<T>& ls, int i)
{
  typename std::list<T>::iterator is;
  int ii = 0;
  for(is = ls.begin(); is != ls.end(); ++is,++ii)
    if (ii == i) return *is;
  return 0;
}

template <class T>
bool find(std::list<T>& ls, T t)
{
  typename std::list<T>::iterator is;
  for(is = ls.begin(); is != ls.end(); ++is)
    if (*is == t) return true;
  return false;
}

} 
#endif
