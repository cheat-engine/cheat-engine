// import1.h
// testing value-oriented imports
#ifdef __UNDERC__  
#lib import1.dll
#define EXPORT
#else
#define EXPORT __declspec(dllexport)
#endif

class EXPORT FatString {
private:
   string m_s;
   int m_ref;
public:
   FatString(char *p = "");
   FatString(const FatString& fs);
   ~FatString();
   string str();
   int size();
};

EXPORT int add(int a, int b);
EXPORT double sum(double x, double y);
EXPORT int length_of(string s);
EXPORT void combine(string s1, string s2, string& res);
EXPORT string name();
EXPORT string copy1(string s);
EXPORT string copy2(const string& s);
EXPORT int sz(FatString fs);

class EXPORT Fred {
private:
    int m_a, m_b;
public:
  Fred();
  void set(int a, int b);
  int a();
  int b();
  int sum();
};

#ifdef __UNDERC__  
#lib 
#endif

