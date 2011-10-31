// standalonelevel1.cpp : Defines the entry point for the application.
//
// test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"



BOOL CreateMyDACL(SECURITY_ATTRIBUTES *);

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
	char tempdir[MAX_PATH];
	char Decompressor[MAX_PATH];
	char Archive[MAX_PATH];
	char SelfName[MAX_PATH];
	SECURITY_ATTRIBUTES  sa;


      
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = FALSE; 
    CreateMyDACL(&sa);

	GetModuleFileNameA(NULL, SelfName, MAX_PATH);
	PathStripPath(SelfName);

	


	char tempfolder[MAX_PATH];

#ifndef TINY
	HRSRC Decomp=FindResource(GetModuleHandle(0), "DECOMPRESSOR", RT_RCDATA);
#endif

	HRSRC Arch=FindResource(GetModuleHandle(0), "ARCHIVE", RT_RCDATA);

#ifndef TINY
	if ((Decomp==0) || (Archive==0))
	  return 0;

	int Decomp_size=SizeofResource(GetModuleHandle(0), Decomp);
#endif

	int Arch_size=SizeofResource(GetModuleHandle(0), Arch);

#ifndef TINY
	HGLOBAL Decomp_memory=LoadResource(GetModuleHandle(0), Decomp);
#endif
    HGLOBAL Arch_memory=LoadResource(GetModuleHandle(0), Arch);



	if (GetTempPathA(MAX_PATH, tempfolder)>0)
	{
		

		strcat(tempfolder,"cetrainers");		
		CreateDirectory(tempfolder, &sa); //if it works, great. If it fails, that folder probably exists or the system is such a piece of shit it won't work eitherway

		//printf("tempfolder=%s\n", tempfolder);
		if (GetTempFileNameA(tempfolder,"CET",0,tempdir)>0)
		{
			int i;
			DeleteFile(tempdir);
			//strcat(tempfolder
			//printf("tempdir=%s\n",tempdir);
			i=CreateDirectory(tempdir, &sa);
			if (i)
			{
			  HANDLE h;
			  DWORD bw;
			  strcpy(Archive, tempdir);
			  strcat(Archive, "\\");
#ifdef TINY
			  strcat(Archive, "CET_TRAINER.CETRAINER");
#else
			  strcat(Archive, "CET_Archive.dat");
#endif
			
			  h=CreateFile(Archive, GENERIC_WRITE,FILE_SHARE_READ | FILE_SHARE_WRITE, &sa, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
			  if (h)
			  {
				  WriteFile(h, Arch_memory, Arch_size, &bw, NULL);
				  CloseHandle(h);
			  }

			 
#ifdef TINY
			  //shellexecute the .cetrainer
			  i=(int)ShellExecute(NULL,"open",Archive,NULL,NULL, SW_SHOWNORMAL);
			  if (i<=32)
			  {
				if (i==SE_ERR_ASSOCINCOMPLETE)
				  MessageBoxA(0,"Your system must have Cheat Engine installed to be able to use this trainer\nwww.cheatengine.org","Launch Error",MB_OK | MB_ICONERROR);
				else
				  MessageBoxA(0,"Failure launching this trainer. Make sure Cheat Engine is properly installed on your system","Launch Error",MB_OK | MB_ICONERROR);

			  }
				  
#else
			  strcpy(Decompressor, tempdir);
			  strcat(Decompressor, "\\");
			  strcat(Decompressor, SelfName);
			
			  h=CreateFile(Decompressor, GENERIC_WRITE,FILE_SHARE_READ | FILE_SHARE_WRITE, &sa, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
			  if (h)
			  {
				  WriteFile(h, Decomp_memory, Decomp_size, &bw, NULL);
				  CloseHandle(h);
			  }

			  STARTUPINFOA StartupInfo;
			  ZeroMemory(&StartupInfo, sizeof(StartupInfo));

			  PROCESS_INFORMATION ProcessInformation;
			  ZeroMemory(&ProcessInformation, sizeof(ProcessInformation));

			  StartupInfo.cb=sizeof(StartupInfo);

			  if (CreateProcess(Decompressor, NULL, NULL, NULL, FALSE, NULL, NULL, tempdir, &StartupInfo, &ProcessInformation))
			  {
				//launch and wait till the decompressor closes
				//  printf("LIFTOFF WEEEEEEEEE!\n");
				  WaitForSingleObject(ProcessInformation.hProcess, INFINITE);
			  }
			  else
				  MessageBox(0,"Failure loading the trainer. Your tempfolder must allow execution. (Check your anti virus)\n","Trainer failure", MB_OK | MB_ICONERROR);
				//  printf("Failed to launch decompessor:%d\n", GetLastError());

			  DeleteFileA(Decompressor);
#endif

			  DeleteFileA(Archive);
			  
			  RemoveDirectoryA(tempdir);

			}

		}

	}
  return 0;
}

// CreateMyDACL.
//    Create a security descriptor that contains the DACL 
//    you want.
//    This function uses SDDL to make Deny and Allow ACEs.
//
// Parameter:
//    SECURITY_ATTRIBUTES * pSA
//    Pointer to a SECURITY_ATTRIBUTES structure. It is your
//    responsibility to properly initialize the 
//    structure and to free the structure's 
//    lpSecurityDescriptor member when you have
//    finished using it. To free the structure's 
//    lpSecurityDescriptor member, call the 
//    LocalFree function.
// 
// Return value:
//    FALSE if the address to the structure is NULL. 
//    Otherwise, this function returns the value from the
//    ConvertStringSecurityDescriptorToSecurityDescriptor 
//    function.
BOOL CreateMyDACL(SECURITY_ATTRIBUTES * pSA)
{
     // Define the SDDL for the DACL. This example sets 
     // the following access:
     //     Built-in guests are denied all access.
     //     Anonymous logon is denied all access.
     //     Authenticated users are allowed 
     //     read/write/execute access.
     //     Administrators are allowed full control.
     // Modify these values as needed to generate the proper
     // DACL for your application. 
     TCHAR * szSD = TEXT("D:")       // Discretionary ACL
        //TEXT("(D;OICI;GA;;;BG)")     // Deny access to 
       //                              // built-in guests
      //  TEXT("(D;OICI;GA;;;AN)")     // Deny access to 
     //                                // anonymous logon
   //     TEXT("(A;OICI;GA;;;AU)") // Allow 
                                     // read/write/execute 
                                     // to authenticated 
                                     // users
   //     TEXT("(A;OICI;GA;;;BA)");    // Allow full control 
                                     // to administrators
		TEXT("(A;OICI;GA;;;WD)");

    if (NULL == pSA)
        return FALSE;

     return ConvertStringSecurityDescriptorToSecurityDescriptor(
                szSD,
                SDDL_REVISION_1,
                &(pSA->lpSecurityDescriptor),
                NULL);
}