local emurpm = {}

local function fileExists(filename)
  local f=io.open(filename, "r")
  if (f~=nil) then
    f:close()
    return true
  else
    return fale
  end
end

if emurpm.OldOnProcessOpened~=nil then
  MainForm.OnProcessOpened=emurpm.OldOnProcessOpened
  emurpm.OldOnProcessOpened=nil
end

emurpm.hookedOnProcessOpened=false

function emurpm.OnProcessOpened(processid, processhandle, caption)
  print("emurpm.OnProcessOpened")
  if frmEmuMemory.cbAutoActivate.checked then
    --print("Doing autoactivate stuff")
    emurpm.emuSetAddress()
  end

  if emurpm.OldOnProcessOpened~=nil then
    return emurpm.OldOnProcessOpened(processid, processhandle, caption)
  end
end

--find the emurpm.frm file
local ced=getCheatEngineDir()
local possiblepaths={}
possiblepaths[1]="emurpm.frm"
possiblepaths[2]=ced.."emurpm.frm"
possiblepaths[3]=ced.."autorun\\emurpm.frm"
possiblepaths[4]="c:\\emurpm.frm"

local frmPath=nil
for i=1,4 do
  if fileExists(possiblepaths[i]) then
    frmPath=possiblepaths[i]
  end
end

if frmPath==nil then
  print("Failure finding emurpm.frm");
else
  --load the form file
  createFormFromFile(frmPath)
end



--allocate memory to store the base address of the emulated memory
autoAssemble([[
  alloc(EmuBaseAddress, 8)
  alloc(EmuSize, 8)
  registersymbol(EmuBaseAddress)
  registersymbol(EmuSize)

  EmuBaseAddress:
  dq 1000

  EmuSize:
  dq 100000
]], true)

autoAssemble([[
  alloc(EmuRPM, 512)
  alloc(EmuWPM, 512)
  alloc(EmuVQE, 512)
  label(invalidlength)
  registersymbol(EmuRPM)
  registersymbol(EmuWPM)
  registersymbol(EmuVQE)

  EmuRPM:
  [64-bit]
  sub rdx,ffffffff80000000
  add rdx,[EmuBaseAddress] //adjust the address
  [/64-bit]

  [32-bit]
  sub [esp+8],ffffffff80000000

  mov eax,[EmuBaseAddress]
  add [esp+8], eax //adjust address to read
  [/32-bit]
  jmp kernel32.ReadProcessMemory


  EmuWPM:
  [64-bit]
  sub rdx,ffffffff80000000
  add rdx,[EmuBaseAddress] //adjust the address
  [/64-bit]

  [32-bit]
  sub [esp+8],ffffffff80000000

  mov eax,[EmuBaseAddress]
  add [esp+8], eax //adjust address to read
  [/32-bit]
  jmp kernel32.WriteProcessMemory

  EmuVQE:
  //Take the base address and fill in the MBI
  [64-bit]
  //RCX=hProcess
  //RDX=lpAddress
  //R8=lpBuffer
  //R9=dwLength
  xor rax,rax

  cmp r9,#48
  jb invalidlength

  cmp rdx,[EmuSize]
  ja invalidlength //actually unreadable, but has the same effect for ce


  and rdx,fffffffffffff000
  mov [r8+0],rdx //baseaddress

  mov [r8+8],80000000 //allocationbase
  mov [r8+10],0x40 //allocation protect: page execute read write (actually a dword, but store as qword to zero the unused bytes)


  sub rdx,ffffffff80000000
  mov rax,[EmuSize]
  sub rax,rdx



  mov [r8+18],rax  //RegionSize seen from base address
  mov dword ptr [r8+20],0x1000 //state : MEM_COMMIT
  mov dword ptr [r8+24],0x40 //protection: Page execute read write
  mov dword ptr [r8+28],0x20000 //type: mem_private

  mov rax,#48 //set the return size to 48 bytes

  invalidlength:
  ret

  [/64-bit]

  [32-bit]
  push ebp
  mov ebp,esp
  //ebp+4=return address
  //ebp+8=hProcess
  //ebp+c=lpAddress
  //ebp+10=lpBuffer
  //ebp+14=dwLength
  xor eax,eax

  cmp [ebp+14],#28
  jb invalidlength

  mov ecx,[ebp+c]
  cmp ecx,[EmuSize]
  ja invalidlength //actually unreadable, but has the same effect for ce

  mov ecx,[ebp+10]

  mov eax,[ebp+c]
  and eax,fffff000
  mov [ecx+0],eax //baseaddress

  mov [ecx+4],80000000 //allocationbase
  mov [ecx+8],0x40 //allocation protect: page execute read write (actually a dword, but store as qword to zero the unused bytes)


  sub eax,80000000
  mov edx,[EmuSize]
  sub edx,eax


  mov [ecx+c],edx  //RegionSize seen from base address
  mov dword ptr [ecx+10],0x1000 //state : MEM_COMMIT
  mov dword ptr [ecx+14],0x40 //protection: Page execute read write
  mov dword ptr [ecx+18],0x20000 //type: mem_private



  mov eax,#28
  invalidlength:
  pop ebp
  ret 10
  [/32-bit]

]], true)


function emurpm.setEmuPointer()
  setAPIPointer(1, getAddress("EmuRPM", true)) --make RPM calls call emurpm
  setAPIPointer(2, getAddress("EmuWPM", true)) --make WPM calls call emuwpm
  setAPIPointer(3, getAddress("EmuVQE", true)) --make VQE calls call EmuVQE
end

function emurpm.emuSetAddress(sender) --called by the (Re)Set address button
  --first undo the api pointer change since I need to read the actual memory

  onAPIPointerChange(nil) --shouldn't be needed, but in case this ever gets changed so setAPIPointer calls it as well


  setAPIPointer(1, windows_ReadProcessMemory) --make RPM calls call emurpm
  setAPIPointer(2, windows_WriteProcessMemory)
  setAPIPointer(3, windows_VirtualQueryEx)

  writeQwordLocal("EmuBaseAddress", getAddress(frmEmuMemory.edtAddress.Text))
  writeQwordLocal("EmuSize", loadstring('return '..frmEmuMemory.edtMemsize.Text)())



  emurpm.setEmuPointer() --hook

  onAPIPointerChange(emurpm.setEmuPointer) --rehook when the hook gets lost
end

--add a menu option to configure the EmuBaseAddress

local mf=getMainForm()
local mi=createMenuItem(mf.Menu)
mi.Caption="Emulator Memory"
mf.Menu.Items.insert(mf.Menu.Items.Count-1, mi) --add it before the last entry (help)


local mi2=createMenuItem(mf.Menu)
mi2.Caption="Set Base Address"
mi2.OnClick=function()
  frmEmuMemory.showModal()
  emurpm.settings.Value["baseaddress"]=frmEmuMemory.edtAddress.Text
  emurpm.settings.Value["memorysize"]=frmEmuMemory.edtMemsize.Text
  emurpm.settings.Value["autoactivate"]=frmEmuMemory.cbAutoActivate.Checked

  if frmEmuMemory.cbAutoActivate.Checked then
    if emurpm.hookedOnProcessOpened==false then --hook it
      emurpm.OldOnProcessOpened=MainForm.OnProcessOpened
      MainForm.OnProcessOpened=emurpm.OnProcessOpened

      emurpm.hookedOnProcessOpened=true
    end
  end
end

mi.add(mi2)

emurpm.settings=getSettings("emurpm")
local baseaddress=emurpm.settings.Value["baseaddress"]
if baseaddress~='' then
  frmEmuMemory.edtAddress.Text=baseaddress
end

local memorysize=emurpm.settings.Value["memorysize"]
if memorysize~='' then
  frmEmuMemory.edtMemsize.Text=memorysize
end

local autoactivate=emurpm.settings.Value["autoactivate"]
frmEmuMemory.cbAutoActivate.Checked=autoactivate=='1'

if frmEmuMemory.cbAutoActivate.Checked then
  --print("emurpm autoactivating")
  --add openedprocesses event
  if emurpm.hookedOnProcessOpened==false then --hook it
    emurpm.OldOnProcessOpened=MainForm.OnProcessOpened

    MainForm.OnProcessOpened=emurpm.OnProcessOpened
    emurpm.hookedOnProcessOpened=true
  end
end
