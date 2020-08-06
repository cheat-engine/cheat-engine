local scripts={}

local function registerBigEndianInt16()
scripts['2 Byte Big Endian'].type=registerCustomTypeAutoAssembler([[
alloc(TypeName,256)
alloc(ByteSize,4)
alloc(ConvertRoutine,1024)
alloc(ConvertBackRoutine,1024)

TypeName:
db '2 Byte Big Endian',0

ByteSize:
dd 2

//The convert routine should hold a routine that converts the data to an integer (in eax)
//function declared as: stdcall int ConvertRoutine(unsigned char *input);
//Note: Keep in mind that this routine can be called by multiple threads at the same time.
ConvertRoutine:
//jmp dllname.functionname
[64-bit]
//or manual:
//parameters: (64-bit)
//rcx=address of input
xor eax,eax
mov ax,[rcx] //eax now contains the bytes 'input' pointed to
xchg ah,al //convert to big endian

ret
[/64-bit]

[32-bit]
//jmp dllname.functionname
//or manual:
//parameters: (32-bit)
push ebp
mov ebp,esp
//[ebp+8]=input
//example:
mov eax,[ebp+8] //place the address that contains the bytes into eax
mov ax,[eax] //place the bytes into eax so it's handled as a normal 4 byte value
and eax,ffff //cleanup
xchg ah,al //convert to big endian

pop ebp
ret 4
[/32-bit]

//The convert back routine should hold a routine that converts the given integer back to a row of bytes (e.g when the user wats to write a new value)
//function declared as: stdcall void ConvertBackRoutine(int i, unsigned char *output);
ConvertBackRoutine:
//jmp dllname.functionname
//or manual:
[64-bit]
//parameters: (64-bit)
//ecx=input
//rdx=address of output
//example:
xchg ch,cl //convert the little endian input into a big endian input
mov [rdx],cx //place the integer the 4 bytes pointed to by rdx

ret
[/64-bit]

[32-bit]
//parameters: (32-bit)
push ebp
mov ebp,esp
//[ebp+8]=input
//[ebp+c]=address of output
//example:
push eax
push ebx
mov eax,[ebp+8] //load the value into eax
mov ebx,[ebp+c] //load the address into ebx

//convert the value to big endian
xchg ah,al

mov [ebx],ax //write the value into the address
pop ebx
pop eax

pop ebp
ret 8
[/32-bit]
]])
end

local function registerBigEndianInt32()
scripts['4 Byte Big Endian'].type=registerCustomTypeAutoAssembler([[
alloc(TypeName,256)
alloc(ByteSize,4)
alloc(ConvertRoutine,1024)
alloc(ConvertBackRoutine,1024)

TypeName:
db '4 Byte Big Endian',0

ByteSize:
dd 4

//The convert routine should hold a routine that converts the data to an integer (in eax)
//function declared as: stdcall int ConvertRoutine(unsigned char *input);
//Note: Keep in mind that this routine can be called by multiple threads at the same time.
ConvertRoutine:
//jmp dllname.functionname
[64-bit]
//or manual:
//parameters: (64-bit)
//rcx=address of input
xor eax,eax
mov eax,[rcx] //eax now contains the bytes 'input' pointed to
bswap eax //convert to big endian

ret
[/64-bit]

[32-bit]
//jmp dllname.functionname
//or manual:
//parameters: (32-bit)
push ebp
mov ebp,esp
//[ebp+8]=input
//example:
mov eax,[ebp+8] //place the address that contains the bytes into eax
mov eax,[eax] //place the bytes into eax so it's handled as a normal 4 byte value

bswap eax

pop ebp
ret 4
[/32-bit]

//The convert back routine should hold a routine that converts the given integer back to a row of bytes (e.g when the user wats to write a new value)
//function declared as: stdcall void ConvertBackRoutine(int i, unsigned char *output);
ConvertBackRoutine:
//jmp dllname.functionname
//or manual:
[64-bit]
//parameters: (64-bit)
//ecx=input
//rdx=address of output
//example:
bswap ecx //convert the little endian input into a big endian input
mov [rdx],ecx //place the integer the 4 bytes pointed to by rdx

ret
[/64-bit]

[32-bit]
//parameters: (32-bit)
push ebp
mov ebp,esp
//[ebp+8]=input
//[ebp+c]=address of output
//example:
push eax
push ebx
mov eax,[ebp+8] //load the value into eax
mov ebx,[ebp+c] //load the address into ebx

//convert the value to big endian
bswap eax

mov [ebx],eax //write the value into the address
pop ebx
pop eax

pop ebp
ret 8
[/32-bit]
]])
end


local function registerBigEndianFloat()
scripts['Float Big Endian'].type=registerCustomTypeAutoAssembler([[
alloc(TypeName,256)
alloc(ByteSize,4)
alloc(ConvertRoutine,1024)
alloc(ConvertBackRoutine,1024)
alloc(UsesFloat,1)
  
  
TypeName:
db 'Float Big Endian',0
  
  
ByteSize:
dd 4

UsesFloat:
db 1
  
  
ConvertRoutine:
[64-bit]
xor eax,eax
mov eax,[rcx] //eax now contains the bytes 'input' pointed to
bswap eax //convert to big endian
ret
[/64-bit]

[32-bit]
push ebp
mov ebp,esp
mov eax,[ebp+8] //place the address that contains the bytes into eax
mov eax,[eax] //place the bytes into eax so it's handled as a normal 4 byte value
bswap eax
pop ebp
ret 4
[/32-bit]
  
  
ConvertBackRoutine:
[64-bit]
bswap ecx //convert the little endian input into a big endian input
mov [rdx],ecx //place the integer the 4 bytes pointed to by rdx
ret
[/64-bit]

[32-bit]
push ebp
mov ebp,esp
push eax
push ebx
mov eax,[ebp+8] //load the value into eax
mov ebx,[ebp+c] //load the address into ebx
bswap eax
  
mov [ebx],eax //write the value into the address
pop ebx
pop eax
pop ebp
ret 8
[/32-bit] 
]])
end


scripts['2 Byte Big Endian']={func=registerBigEndianInt16,reg='EnableBigEndianInt16'}
scripts['4 Byte Big Endian']={func=registerBigEndianInt32,reg='EnableBigEndianInt32'}
scripts['Float Big Endian']={func=registerBigEndianFloat,reg='EnableBigEndianFloat'}
local scriptnames={'2 Byte Big Endian','4 Byte Big Endian','Float Big Endian'}


--when loaded check if it should load the custom types
local i
local s=getSettings()
for i=1,#scriptnames do 
  if (s.Value[scripts[scriptnames[i]].reg]=='1') then
    scripts[scriptnames[i]].func()
  end
end

local sf=getSettingsForm()

local CustomTypesPage=nil
for i=0, sf.SettingsPageControl.PageCount-1 do
  if sf.SettingsPageControl.Page[i].Name=='CustomTypes' then --Do NOT translate this
    CustomTypesPage=s.SettingsPageControl.Page[i]
  end
end
if CustomTypesPage==nil then
  --first script to add it
  CustomTypesPage=sf.SettingsPageControl.addTab()
  CustomTypesPage.Name='CustomTypes' --again, do NOT translate this
  CustomTypesPage.Caption=translate('Extra Custom Types')

  local insertNode=sf.SettingsTreeView.Items[3]  --insert it near the unrandomizer since it'd be used as often as that setting
  local node=sf.SettingsTreeView.Items.insert(insertNode, CustomTypesPage.Caption)
  node.data=userDataToInteger(CustomTypesPage)

  local clb=createCheckListBox(CustomTypesPage);
  clb.Align='alClient'
  clb.Name='List'
end


for i=1, #scriptnames do
  CustomTypesPage.List.Items.Add(scriptnames[i])
  CustomTypesPage.List.Checked[CustomTypesPage.List.Items.Count-1]=scripts[scriptnames[i]].type~=nil
end

local oldOnClose=sf.OnClose
sf.OnClose=function(sender, closeAction)
  local result=closeAction
  if oldOnClose~=nil then
    result=oldOnClose(sender, closeAction)
  end
  
  if (result==caHide) and (sender.ModalResult==mrOK) then
    local i
    for i=0, CustomTypesPage.List.Items.Count-1 do
      local ctname=CustomTypesPage.List.Items[i]
      
      
      if scripts[ctname] then --known name
        if CustomTypesPage.List.Checked[i] then
          if scripts[ctname].type==nil then
            scripts[ctname].func()                    
          end               
          local regname=scripts[ctname].reg;
          s.Value[scripts[ctname].reg]='1' 
         -- print(s.ClassName)
        else
          
          if scripts[ctname].type then
            --print("destroy")
            scripts[ctname].type.destroy()
            scripts[ctname].type=nil
          end
          s.Value[scripts[ctname].reg]='0'
       
        end
      end
    end
  end
  return result
end


