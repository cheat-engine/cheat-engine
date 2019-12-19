//MyIcorDebugDataTarget

#include "stdafx.h"


#include "MyIcorDebugDataTarget.h"



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// IUnknown
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
STDMETHODIMP CMyIcorDebugDataTarget::QueryInterface(__in REFIID InterfaceId, __out PVOID* Interface)
{
	*Interface = NULL;
	
	if (IsEqualIID(InterfaceId, __uuidof(ICLRDataTarget)))
	{
		return E_NOINTERFACE;
	}

	if (IsEqualIID(InterfaceId, __uuidof(IUnknown)) || IsEqualIID(InterfaceId, __uuidof(ICorDebugDataTarget)))
	{
		*Interface = (ICorDebugDataTarget*) this;
		InterlockedIncrement(&m_ref);
		return S_OK;
	}
	else
	{
		return E_NOINTERFACE;
	}
}

STDMETHODIMP_(ULONG) CMyIcorDebugDataTarget::AddRef()
{
	return InterlockedIncrement(&m_ref);
}

STDMETHODIMP_(ULONG) CMyIcorDebugDataTarget::Release()
{
	long nCount = InterlockedDecrement(&m_ref);
	if (nCount == 0)
		delete this;

	return nCount;
}

HRESULT STDMETHODCALLTYPE CMyIcorDebugDataTarget::GetPlatform(/* [out] */ CorDebugPlatform *pTargetPlatform)
{
	if (pTargetPlatform)
	{
		BOOL Wow64=TRUE;
		

		if (_IsWow64Process)		
		{
			_IsWow64Process(processHandle, &Wow64);
			if (Wow64)			
				*pTargetPlatform=CORDB_PLATFORM_WINDOWS_X86;
			else
				*pTargetPlatform=CORDB_PLATFORM_WINDOWS_AMD64;
		}
		else
			*pTargetPlatform=CORDB_PLATFORM_WINDOWS_X86;



		

		return S_OK;

		
	}
	else
	  return E_FAIL;
}
        
HRESULT STDMETHODCALLTYPE CMyIcorDebugDataTarget::ReadVirtual(
		  /* [in] */ CORDB_ADDRESS address,
            /* [length_is][size_is][out] */ BYTE *pBuffer,
            /* [in] */ ULONG32 bytesRequested,
            /* [out] */ ULONG32 *pBytesRead)
{
	SIZE_T bytesread=0;
	ReadProcessMemory(processHandle, (void *)address, pBuffer, bytesRequested, &bytesread);

	if (pBytesRead)
		*pBytesRead=(ULONG32)bytesread;

	if (bytesread)
		return S_OK;
	else
		return E_FAIL;
}
        
HRESULT STDMETHODCALLTYPE CMyIcorDebugDataTarget::GetThreadContext( 
            /* [in] */ DWORD dwThreadID,
            /* [in] */ ULONG32 contextFlags,
            /* [in] */ ULONG32 contextSize,
            /* [size_is][out] */ BYTE *pContext)
{
	CorDebugPlatform platform;
	GetPlatform(&platform);

	HRESULT result=E_FAIL;
	HANDLE hThread=OpenThread(THREAD_ALL_ACCESS, false, dwThreadID);

	if (hThread)
	{
		if (platform==CORDB_PLATFORM_WINDOWS_X86)
		{
			WOW64_CONTEXT context;
			context.ContextFlags=contextFlags;
			
			if (Wow64GetThreadContext(hThread, &context))
			{
				ZeroMemory(pContext, contextSize);				
				CopyMemory(pContext, &context, min(contextSize, sizeof(context)));				
				result=S_OK;
			}
		}
		else
		if (platform==CORDB_PLATFORM_WINDOWS_AMD64)
		{			
			CONTEXT context;
			context.ContextFlags=contextFlags;

			
			
			if (::GetThreadContext(hThread, &context))
			{
				ZeroMemory(pContext, contextSize);				
				CopyMemory(pContext, &context, min(contextSize, sizeof(context)));				
				result=S_OK;
			}
		}

		CloseHandle(hThread);
	}

	return result;
}