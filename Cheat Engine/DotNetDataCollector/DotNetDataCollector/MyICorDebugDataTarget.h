#ifndef __CMyIcorDebugDataTarget_H__
#define __CMyIcorDebugDataTarget_H__


typedef BOOL (WINAPI *ISWOW64PROCESS)(_In_ HANDLE hProcess, _Out_ PBOOL Wow64Process);


class CMyIcorDebugDataTarget : 
    public ICorDebugDataTarget
    
{
private:
	HANDLE processHandle;
	long m_ref;

	ISWOW64PROCESS _IsWow64Process;

public:
	CMyIcorDebugDataTarget(HANDLE p)
	{
		m_ref = 1;
		processHandle=p;		
		_IsWow64Process=(ISWOW64PROCESS)GetProcAddress(GetModuleHandleA("Kernel32.dll"), "IsWow64Process");		
	}

	~CMyIcorDebugDataTarget()
	{

	}

	// IUnknown
	STDMETHOD(QueryInterface)(__in REFIID InterfaceId, __out PVOID* Interface);
	STDMETHOD_(ULONG, AddRef)();
	STDMETHOD_(ULONG, Release)();

	// ICorDebugTargetData
	    virtual HRESULT STDMETHODCALLTYPE GetPlatform(
			/* [out] */ CorDebugPlatform *pTargetPlatform);
        
        virtual HRESULT STDMETHODCALLTYPE ReadVirtual( 
            /* [in] */ CORDB_ADDRESS address,
            /* [length_is][size_is][out] */ BYTE *pBuffer,
            /* [in] */ ULONG32 bytesRequested,
            /* [out] */ ULONG32 *pBytesRead);
        
        virtual HRESULT STDMETHODCALLTYPE GetThreadContext( 
            /* [in] */ DWORD dwThreadID,
            /* [in] */ ULONG32 contextFlags,
            /* [in] */ ULONG32 contextSize,
            /* [size_is][out] */ BYTE *pContext);



};

#endif // __CCorDebugManagedCallback2_H__
