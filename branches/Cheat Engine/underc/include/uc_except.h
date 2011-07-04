// uc_except.h
// Maps Win32 structured exceptions (SE) or Linux signals
// onto C++ exceptions
#ifndef _uc_except_h
#define _uc_except_h

class Exception {
private:
	char *m_what;
public:
	Exception(char *msg="unknown") : m_what(msg) {}
	char *what() { return m_what; }
	static void initialize();
};

class HardwareException: public Exception {
public:
	HardwareException(char *msg) : Exception(msg) { }
};

class RangeError: public Exception {
public:
        RangeError(char *msg="range check") : Exception(msg) {}
};

class IntOverflow: public HardwareException {
public:
	IntOverflow() : HardwareException("integer overflow") {}
};

class FloatOverflow: public HardwareException {
public:
	FloatOverflow() : HardwareException("floating point overflow") {}
};

class IntDivByZero: public HardwareException {
public:
	IntDivByZero() : HardwareException("integer division by zero") {}
};

class FloatDivByZero: public HardwareException {
public:
	FloatDivByZero() : HardwareException("floating point division by zero") {}
};

class AccessViolation: public HardwareException {
public:
	AccessViolation() : HardwareException("access violation") {}
};
#endif
