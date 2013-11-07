#include "StdAfx.h"


#include "PipeServer.h"

BOOL SetPrivilege(
    HANDLE hToken,          // access token handle
    LPCTSTR lpszPrivilege,  // name of privilege to enable/disable
    BOOL bEnablePrivilege   // to enable or disable privilege
    ) 
{
    TOKEN_PRIVILEGES tp;
    LUID luid;

    if ( !LookupPrivilegeValue( 
            NULL,            // lookup privilege on local system
            lpszPrivilege,   // privilege to lookup 
            &luid ) )        // receives LUID of privilege
    {
        printf("LookupPrivilegeValue error: %u\n", GetLastError() ); 
        return FALSE; 
    }

    tp.PrivilegeCount = 1;
    tp.Privileges[0].Luid = luid;
    if (bEnablePrivilege)
        tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
    else
        tp.Privileges[0].Attributes = 0;

    // Enable the privilege or disable all privileges.

    if ( !AdjustTokenPrivileges(
           hToken, 
           FALSE, 
           &tp, 
           sizeof(TOKEN_PRIVILEGES), 
           (PTOKEN_PRIVILEGES) NULL, 
           (PDWORD) NULL) )
    { 
          printf("AdjustTokenPrivileges error: %u\n", GetLastError() ); 
          return FALSE; 
    } 

    if (GetLastError() == ERROR_NOT_ALL_ASSIGNED)

    {
          printf("The token does not have the specified privilege. \n");
          return FALSE;
    } 

    return TRUE;
}

int WINAPI wWinMain(
    _In_ HINSTANCE hInstance,
    _In_opt_ HINSTANCE hPrevInstance,
    _In_ LPWSTR lpCmdLine,
    _In_ int nShowCmd
    )
{
	CPipeServer *pw;
	HANDLE p;
	HANDLE hToken;

	
	p = OpenProcess( PROCESS_QUERY_INFORMATION, FALSE, GetCurrentProcessId() ); 
	OpenProcessToken( p, TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken ); 	 
	SetPrivilege( hToken, SE_DEBUG_NAME, TRUE );
	CoInitialize(NULL);

	
	if (lpCmdLine)
	{
		int r;
		pw=new CPipeServer(lpCmdLine);
		r=pw->Start();

		delete pw;
		return r;
	}
	else
	{
		printf("Invalid parameter");
		return 1;
	}
	return 0;
}