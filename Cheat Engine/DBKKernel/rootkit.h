#include "ntifs.h"
#include <windef.h>


//ZwOpenProcess
NTSYSAPI NTSTATUS NTAPI ZwOpenProcess(OUT PHANDLE ProcessHandle,  IN ACCESS_MASK DesiredAccess,   IN POBJECT_ATTRIBUTES ObjectAttributes, IN PCLIENT_ID ClientId);
NTSTATUS NewZwOpenProcess(OUT PHANDLE ProcessHandle,  IN ACCESS_MASK DesiredAccess,  IN POBJECT_ATTRIBUTES ObjectAttributes,IN PCLIENT_ID ClientId);
typedef NTSTATUS (*ZWOPENPROCESS) (PHANDLE ProcessHandle,ACCESS_MASK DesiredAccess,	POBJECT_ATTRIBUTES ObjectAttributes,PCLIENT_ID ClientId);
ZWOPENPROCESS OldZwOpenProcess;
//--------------------------------------


//ZwQuerySystemInformation
NTSTATUS NewZwQuerySystemInformation(IN SYSTEM_INFORMATION_CLASS SystemInformationClass, OUT PVOID SystemInformation, IN ULONG SystemInformationLength, OUT PULONG ReturnLength OPTIONAL);
typedef NTSTATUS (*ZWQUERYSYSTEMINFORMATION)(IN SYSTEM_INFORMATION_CLASS SystemInformationClass,OUT PVOID SystemInformation,IN ULONG SystemInformationLength,OUT PULONG ReturnLength OPTIONAL);
ZWQUERYSYSTEMINFORMATION OldZwQuerySystemInformation;
//---------------------------


//------------NtUserBuildHwndList------------
NTSTATUS NewNtUserBuildHwndList(IN HDESK hdesk, IN HWND hwndNext, IN ULONG fEnumChildren, IN DWORD idThread, IN UINT cHwndMax, OUT HWND *phwndFirst, OUT ULONG* pcHwndNeeded);
typedef NTSTATUS (*NTUSERBUILDHWNDLIST)(IN HDESK hdesk,IN HWND hwndNext, IN ULONG fEnumChildren, IN DWORD idThread, IN UINT cHwndMax, OUT HWND *phwndFirst, OUT ULONG *pcHwndNeeded);
NTUSERBUILDHWNDLIST OldNtUserBuildHwndList;
ULONG NtUserBuildHwndList_callnumber;
//--------------------------

//-----------NtUserQueryWindow-------------
UINT_PTR NewNtUserQueryWindow(IN ULONG WindowHandle,IN ULONG TypeInformation);
typedef UINT_PTR (*NTUSERQUERYWINDOW)(IN ULONG WindowHandle,IN ULONG TypeInformation);
NTUSERQUERYWINDOW OldNtUserQueryWindow;
ULONG NtUserQueryWindow_callnumber;
//-------------------------


//------NtUserFindWindowEx------
ULONG NewNtUserFindWindowEx(IN HWND hwndParent, IN HWND hwndChild, IN PUNICODE_STRING pstrClassName OPTIONAL, IN PUNICODE_STRING pstrWindowName OPTIONAL, IN DWORD dwType);
typedef ULONG (*NTUSERFINDWINDOWEX)(IN HWND hwndParent, IN HWND hwndChild, IN PUNICODE_STRING pstrClassName OPTIONAL, IN PUNICODE_STRING pstrWindowName OPTIONAL, IN DWORD dwType);
NTUSERFINDWINDOWEX OldNtUserFindWindowEx;
ULONG NtUserFindWindowEx_callnumber;
//-------------------------

//----------NtUserGetForegroundWindow-------------
ULONG NewNtUserGetForegroundWindow(VOID);
typedef ULONG (*NTUSERGETFOREGROUNDWINDOW)(VOID);
NTUSERGETFOREGROUNDWINDOW OldNtUserGetForegroundWindow;
ULONG NtUserGetForegroundWindow_callnumber;
ULONG LastForegroundWindow;
//--------------------

//---------NtUserGetDC-----------
ULONG NtUserGetDC_callnumber;
typedef HDC (*NTUSERGETDC)(IN HWND WindowHandle);
NTUSERGETDC RealNtUserGetDC;



VOID LoadImageNotifyRoutine(IN PUNICODE_STRING  FullImageName, IN HANDLE  ProcessId, IN PIMAGE_INFO  ImageInfo);
BOOLEAN MakeWritable(PVOID StartAddress,UINT_PTR size,BOOLEAN usecopyonwrite);

BOOLEAN		ImageNotifyRoutineLoaded;
BOOLEAN		ProtectOn;
BOOLEAN		DenyList;
BOOLEAN		GlobalDenyList;
char*		ModuleList;
int			ModuleListSize;
HANDLE		ProtectedProcessID;
PEPROCESS	ProtectedPEProcess;
ULONG		ProtectedCR3;
ULONG		FakeCR3;
ULONG		Size;

UINT_PTR	ActiveLinkOffset;
UINT_PTR	ProcessNameOffset;
UINT_PTR	DebugportOffset;
UINT_PTR	PIDOffset;

//------------
NTSTATUS OriginalObOpenObjectByPointer(IN PVOID Object, IN ULONG HandleAttributes, IN PACCESS_STATE PassedAccessState OPTIONAL, IN ACCESS_MASK DesiredAccess,
													  IN POBJECT_TYPE ObjectType, IN KPROCESSOR_MODE AccessMode, OUT PHANDLE Handle );
NTSTATUS NewObOpenObjectByPointer (IN PVOID Object, IN ULONG HandleAttributes, IN PACCESS_STATE PassedAccessState OPTIONAL, IN ACCESS_MASK DesiredAccess,
								   IN POBJECT_TYPE ObjectType, IN KPROCESSOR_MODE AccessMode, OUT PHANDLE Handle );

void cr3_change_callback(ULONG oldcr3, ULONG newcr3);

PEPROCESS testprotect;