#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main(int argc, char **argv)
{
  string name = argv[1];
  string file = name;
  string lib = "";
  if (argc == 2) file += ".h";
  else if (argv[2] == string("lib")) {
     lib = argv[3];
  }
  ofstream out(file.c_str());
  string guard_macro = "__" + name + "_H";
  out << "// UnderC Development Project, 2001\n" << endl;
  out << "#ifndef " << guard_macro << endl;
  out << "#define " << guard_macro << endl;
  if (lib != "") 
    out << "#lib " << lib << endl;
  out << endl;
  if (lib != "")
    out << "#lib" << endl;
  out << "#endif" << endl;
  return 0;
}

