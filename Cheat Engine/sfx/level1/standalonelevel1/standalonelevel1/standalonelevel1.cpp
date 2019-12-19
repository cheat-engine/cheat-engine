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
	char Origin[MAX_PATH];
	char tempdir[MAX_PATH];
	char Decompressor[MAX_PATH];
	char Archive[1024];
	char Parameter[MAX_PATH+16];
	char SelfName[MAX_PATH];

	SECURITY_ATTRIBUTES  sa;


	
      
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = FALSE; 
    CreateMyDACL(&sa);

	GetModuleFileNameA(NULL, SelfName, MAX_PATH);
	strcpy(Origin, SelfName);

	PathRemoveFileSpec(Origin);
	PathAddBackslash(Origin);

	sprintf_s(Parameter, MAX_PATH+16, "-ORIGIN:\"%s\"", Origin);

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
#ifdef TINY
			struct stat status;
#else
		
#endif

			DeleteFile(tempdir);
			//strcat(tempfolder
			//printf("tempdir=%s\n",tempdir);
			i=CreateDirectory(tempdir, &sa);
			if (i)
			{
			  HANDLE h;
			  DWORD bw;
			  
			  char Commandline[MAX_PATH*2+16]; 
#ifdef TINY

			  char CEPath[MAX_PATH];
			  DWORD size;
#endif
		
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
			  			  
			  size=MAX_PATH;
			  HRESULT STATUS;
			  STATUS=AssocQueryString(0, ASSOCSTR_EXECUTABLE, ".cetrainer", NULL, CEPath, &size);

			 // STATUS=0x80070002;
			  if (!FAILED(STATUS))
			  {
				  int correctversion=0;
				  DWORD h=0;
				  DWORD fvis=GetFileVersionInfoSize(CEPath, &h);
				  void *data;
				  //check if it is the correct version

				  if (fvis)
				  {
					  data=malloc(fvis);
					  if (GetFileVersionInfo(CEPath, h, fvis, data))
					  {
						  VS_FIXEDFILEINFO *ffi;
						  UINT s=sizeof(ffi);
						  if (VerQueryValue(data, "\\", (LPVOID*)&ffi, &s))
						  {
							  correctversion=(ffi->dwFileVersionMS>=0x00060003);
						  }

					  }
					  free(data);
				  }	  
				  

				  if (!correctversion)
				  {
					  MessageBoxA(0,"Please update your Cheat Engine version to Cheat Engine 6.3 or later\n", "Launch Error",MB_OK | MB_ICONERROR);
				  }
				  else
				  {
					  //launch it
					  
	                  STARTUPINFOA StartupInfo;
  					  ZeroMemory(&StartupInfo, sizeof(StartupInfo));

					  PROCESS_INFORMATION ProcessInformation;
					  ZeroMemory(&ProcessInformation, sizeof(ProcessInformation));

					  StartupInfo.cb=sizeof(StartupInfo);

					  sprintf_s(Commandline, MAX_PATH*2+16,"\"%s\" \"%s\" %s",CEPath, Archive, Parameter);

					  if (CreateProcessA(CEPath, Commandline, NULL, NULL, FALSE, NULL, NULL, tempdir, &StartupInfo, &ProcessInformation))
					  {						  
						  //Because Cheat Engine deletes files with name CET_TRAINER.CETRAINER it can be used to determine when ce is finished with it			  
						  //Wait 30 seconds max for ce to delete the file
						  i=30;
						  while (i && (stat(Archive, &status) == 0))
						  {				  
							  Sleep(1000);
							  i--;
						  }
					  }
				  }				  

			  }
			  else
			  {
				  //AssocQueryString failed. This is either because 1: The file assoc is not there, so not installed, or 2: Crappy XP system

				  //fall back to shellExecute
				  //SHELLEXECUTEINFO sexi;
				 

				  if ((int)ShellExecuteA(0, "open", Archive, Parameter, NULL, SW_SHOW)>32)
				  {
		  
					  //because cheat engine deletes files with name cet_trainer.cetrainer it can be used to determine when ce is finished with it			  
					  //wait 30 seconds max for ce to delete the file (if ce takes long to even start opening the file, get a faster system)
					  i=30;
					  while (i && (stat(Archive, &status) == 0))
					  {				  
						  Sleep(1000);
						  i--;
					  }


				  }
				  else
				  {
					  
					  char errorstring[255];
					  sprintf_s(errorstring, 254,"Your system must have Cheat Engine installed to be able to use this trainer\nwww.cheatengine.org\n(%x)", STATUS);
					  MessageBoxA(0,errorstring,"Launch Error",MB_OK | MB_ICONERROR);
				  }
			  }

			  



#else
			  sprintf_s(Decompressor, MAX_PATH, "%s\\%s", tempdir, SelfName);

			  sprintf_s(Commandline, MAX_PATH*2+16,"\"%s\" %s",Decompressor, Parameter);
			  
			
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

			  if (CreateProcessA(Decompressor, Commandline, NULL, NULL, FALSE, NULL, NULL, tempdir, &StartupInfo, &ProcessInformation))
			  {

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
			else
				MessageBoxA(0,"Failure creating a temporary folder","Launch Error",MB_OK | MB_ICONERROR);


		}
		else
			MessageBoxA(0,"Failure assigning a temporary name","Launch Error",MB_OK | MB_ICONERROR);

	}
	else
		MessageBoxA(0,"Failure getting the temp folder","Launch Error",MB_OK | MB_ICONERROR);

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