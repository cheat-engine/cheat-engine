#
# Generates platform dependent units from common code base
#
# $Id: Makefile.mak 2307 2008-01-15 22:01:28Z outchy $
#

jpp		= ..\..\devtools\jpp.exe
touch		= $(MAKEDIR)\touch.exe

Options			= -c -dJCL -dSUPPORTS_DEFAULTPARAMS -dSUPPORTS_INT64
# CommonOptions		= $(Options) -f..\common\\
VclOptions		= $(Options) -dVCL -uVisualCLX -dMSWINDOWS -uUnix -dBitmap32 -x1:..\vcl\Jcl
VClxOptions		= $(Options) -uVCL -dVisualCLX -dHAS_UNIT_TYPES -uBitmap32 -x1:..\visclx\JclQ
WinOptions		= $(Options) -dMSWINDOWS -uUNIX -uHAS_UNIT_LIBC -f..\windows\\
Win32Options		= $(Options) -uHAS_UNIT_LIBC -f..\windows\\
ContainerOptions	= $(Options) -m -ijcl.inc -f..\Common\\
UnixOptions		= $(Options) -uMSWINDOWS -dUNIX -f..\unix\\
ZlibOptions		= -uSTATIC_GZIO


release:	VCL VisualCLX Windows Unix ContainersProt Containers

VCL:    	..\vcl\JclGraphics.pas \
		..\vcl\JclGraphUtils.pas

VisualCLX:    	..\visclx\JclQGraphics.pas \
		..\visclx\JclQGraphUtils.pas

Windows:        ..\windows\JclWin32.pas \
                ..\windows\Hardlinks.pas \
                ..\windows\zlibh.pas

Unix:		..\unix\zlibh.pas

zlib:		..\windows\zlibh.pas \
		..\unix\zlibh.pas

ContainersProt:	JclAlgorithms.pas \
		JclArrayLists.pas \
		JclArraySets.pas \
		JclBinaryTrees.pas \
		JclContainerIntf.pas \
		JclHashMaps.pas \
		JclHashSets.pas \
		JclLinkedLists.pas \
		JclQueues.pas \
		JclSortedMaps.pas \
		JclStacks.pas \
                JclTrees.pas \
		JclVectors.pas

Containers:	..\Common\JclAlgorithms.pas \
		..\Common\JclArrayLists.pas \
		..\Common\JclArraySets.pas \
		..\Common\JclBinaryTrees.pas \
		..\Common\JclContainerIntf.pas \
		..\Common\JclHashMaps.pas \
		..\Common\JclHashSets.pas \
		..\Common\JclLinkedLists.pas \
		..\Common\JclQueues.pas \
		..\Common\JclSortedMaps.pas \
		..\Common\JclStacks.pas \
                ..\Common\JclTrees.pas \
		..\Common\JclVectors.pas

..\vcl\JclGraphics.pas: \
		_Graphics.pas
	$(jpp) $(VclOptions) $?

..\vcl\JclGraphUtils.pas: \
		_GraphUtils.pas
	$(jpp) $(VclOptions) $?

..\visclx\JclQGraphics.pas: \
		_Graphics.pas
	$(jpp) $(VClxOptions) $?

..\visclx\JclQGraphUtils.pas: \
		_GraphUtils.pas
	$(jpp) $(VClxOptions) $?

..\unix\JclWin32.pas: \
                JclWin32.pas
        $(jpp) -ijcl.inc $(UnixOptions) $?

..\unix\zlibh.pas: \
		zlibh.pas
        echo Unix-zlib
	$(jpp) $(UnixOptions) $(ZlibOptions) -dZLIB_DLL $?

..\windows\JclWin32.pas: \
                JclWin32.pas
        $(jpp) -ijcl.inc $(WinOptions) $?

..\windows\zlibh.pas: \
		zlibh.pas
        echo Win-zlib
	$(jpp) $(WinOptions) $(ZlibOptions) -uZLIB_DLL $?

JclAlgorithms.pas: \
		containers\JclAlgorithms.int containers\JclAlgorithms.imp
	$(touch) $@

JclArrayLists.pas: \
		containers\JclArrayLists.imp containers\JclArrayLists.int containers\JclContainerCommon.imp
	$(touch) $@

JclArraySets.pas: \
		containers\JclArraySets.imp containers\JclArraySets.int containers\JclContainerCommon.imp
	$(touch) $@

JclBinaryTrees.pas: \
		containers\JclBinaryTrees.imp containers\JclBinaryTrees.int containers\JclContainerCommon.imp
	$(touch) $@

JclContainerIntf.pas: \
		containers\JclContainerIntf.int
	$(touch) $@

JclHashMaps.pas: \
		containers\JclHashMaps.imp containers\JclHashMaps.int containers\JclContainerCommon.imp
	$(touch) $@

JclHashSets.pas: \
		containers\JclHashSets.imp containers\JclHashSets.int containers\JclContainerCommon.imp
	$(touch) $@

JclLinkedLists.pas: \
		containers\JclLinkedLists.imp containers\JclLinkedLists.int containers\JclContainerCommon.imp
	$(touch) $@

JclQueues.pas: \
		containers\JclQueues.imp containers\JclQueues.int containers\JclContainerCommon.imp
	$(touch) $@

JclSortedMaps.pas: \
		containers\JclSortedMaps.imp containers\JclSortedMaps.int containers\JclContainerCommon.imp
	$(touch) $@

JclStacks.pas: \
		containers\JclStacks.imp containers\JclStacks.int containers\JclContainerCommon.imp
	$(touch) $@

JclTrees.pas: \
		containers\JclTrees.imp containers\JclTrees.int containers\JclContainerCommon.imp
	$(touch) $@

JclVectors.pas: \
		containers\JclVectors.imp containers\JclVectors.int containers\JclContainerCommon.imp
	$(touch) $@

{.}.pas{..\common}.pas:
	$(jpp) $(ContainerOptions) $<

{.}.pas{..\windows}.pas:
	$(jpp) $(WinOptions) $<

{.}.pas{..\unix}.pas:
	$(jpp) $(UnixOptions) $<
