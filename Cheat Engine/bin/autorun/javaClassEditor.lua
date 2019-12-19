if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'Java.po')
end

--Java class editor


--[[
This will show an userinterface for editing java classes and will return a list of "patch" commands
that can be used with the runtime java class edit commands

e.g:
DefineLabel(spot)
InsertBytecode(spot, command)
ModifyBytecode(spot, command)
DeleteBytecode(spot)  (could be ModifyBytecode(spot,"nop") )


The user should not have to know about exceptions and how their positions change with each insert/delete

gui:
listview:
index|byteindex|label |exception|instruction|
-----|---------|------|---------|-----------|
0    |0        |      |         |nop        |   Insert
0    |1        |l1:   |ex1:     |branch l1  |   Delete
                                                Modify

--]]

--http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html


java_bytecodes={}

--[[
paramtypes:
  s1=signed 1 byte
  s2=signed 2 byte
  s4=signed 4 byte
  u1=unsigned 1 byte
  u2=unsigned 2 byte
  u4=unsigned 4 byte


  02=null 2 bytes
  wide=special wide operand

  pad4=pad till 4 byte alignment

  [s4]=array of 4 byte signed values
  [s4s4]=array of 2 4 byte signed values
--]]

function signExtendByte(byte)
  --byte is a value between 0 and 255
  --if bit 7 is 1, it's a negative
  if bAnd(byte,0x80)~=0 then
    return byte-0x100
  else
    return byte
  end
end

function signExtendWord(word)
  if bAnd(word,0x8000)~=0 then
    return word-0x10000
  else
    return word
  end
end

function signExtendDword(dword)
  --assuming dword is in the range of 0 to 0xffffffff
  if bAnd(dword,0x80000000)~=0 then
    return dword-0x100000000
  else
    return dword
  end
end


function calculateTableSwitch(address, bytes)
  local result=nil

  if bytes[address]==0xaa then
    local padding=0

  if (address-1) % 4>0 then
    padding=4-(address-1) % 4
  end

    --local defaultbyte=byteTableToDword({bytes[8],bytes[7],bytes[6],bytes[5]})
  local low=byteTableToDword({bytes[address+1+padding+7],bytes[address+1+padding+6],bytes[address+1+padding+5],bytes[address+1+padding+4]})
  local high=byteTableToDword({bytes[address+1+padding+11],bytes[address+1+padding+10],bytes[address+1+padding+9],bytes[address+1+padding+8]})
  result=padding+4+4+4+((high-low)+1)*4 --paramsize
  end

  return result;
end

function calculateLookupSwitch(address, bytes)
  local result=nil

  if bytes[address]==0xab then
    local padding=0

  if (address-1) % 4>0 then
    padding=4-(address-1) % 4
  end

    --local defaultbyte=byteTableToDword({bytes[8],bytes[7],bytes[6],bytes[5]})
  local npairs=byteTableToDword({bytes[address+1+padding+7],bytes[address+1+padding+6],bytes[address+1+padding+5],bytes[address+1+padding+4]})
  result=padding+4+4+npairs*8 --paramsize
  end

  return result;

end

java_bytecodes[0x00]={operation="nop", description="Do nothing"}
java_bytecodes[0x01]={operation="aconst_null", description="Push null"}
java_bytecodes[0x02]={operation="iconst_m1", description="Push int -1"}
java_bytecodes[0x03]={operation="iconst_0", description="Push int 0"}
java_bytecodes[0x04]={operation="iconst_1", description="Push int 1"}
java_bytecodes[0x05]={operation="iconst_2", description="Push int 2"}
java_bytecodes[0x06]={operation="iconst_3", description="Push int 3"}
java_bytecodes[0x07]={operation="iconst_4", description="Push int 4"}
java_bytecodes[0x08]={operation="iconst_5", description="Push int 5"}
java_bytecodes[0x09]={operation="lconst_0", description="Push long 0"}
java_bytecodes[0x0a]={operation="lconst_1", description="Push long 1"}
java_bytecodes[0x0b]={operation="fconst_0", description="Push float 0.0"}
java_bytecodes[0x0c]={operation="fconst_1", description="Push float 1.0"}
java_bytecodes[0x0d]={operation="fconst_2", description="Push float 2.0"}
java_bytecodes[0x0e]={operation="dconst_0", description="Push double 0.0"}
java_bytecodes[0x0f]={operation="dconst_1", description="Push double 1.0"}
java_bytecodes[0x10]={operation="bipush", parameters={bytecount=1, {paramname="const", paramtype="s1"}}, description="Push byte"}
java_bytecodes[0x11]={operation="sipush", parameters={bytecount=2, {paramname="const", paramtype="s2"}}, description="Push short"}
java_bytecodes[0x12]={operation="ldc", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, description="Push item from run-time constant pool"}
java_bytecodes[0x13]={operation="ldc_w", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Push item from run-time constant pool(wide)"}
java_bytecodes[0x14]={operation="ldc2_w", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Push long or double from run-time constant pool(wide)"}
java_bytecodes[0x15]={operation="iload", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}},description="Load int from local variable"}
java_bytecodes[0x16]={operation="lload", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}},description="Load long from local variable"}
java_bytecodes[0x17]={operation="fload", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}},description="Load float from local variable"}
java_bytecodes[0x18]={operation="dload", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Load double from local variable"}
java_bytecodes[0x19]={operation="aload", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Load reference from local variable"}
java_bytecodes[0x1a]={operation="iload_0", description="Load int from local variable at index 0"}
java_bytecodes[0x1b]={operation="iload_1", description="Load int from local variable at index 1"}
java_bytecodes[0x1c]={operation="iload_2", description="Load int from local variable at index 2"}
java_bytecodes[0x1d]={operation="iload_3", description="Load int from local variable at index 3"}
java_bytecodes[0x1e]={operation="lload_0", description="Load long from local variable at index 0"}
java_bytecodes[0x1f]={operation="lload_1", description="Load long from local variable at index 1"}
java_bytecodes[0x20]={operation="lload_2", description="Load long from local variable at index 2"}
java_bytecodes[0x21]={operation="lload_3", description="Load long from local variable at index 3"}
java_bytecodes[0x22]={operation="fload_0", description="Load float from local variable at index 0"}
java_bytecodes[0x23]={operation="fload_1", description="Load float from local variable at index 1"}
java_bytecodes[0x24]={operation="fload_2", description="Load float from local variable at index 2"}
java_bytecodes[0x25]={operation="fload_3", description="Load float from local variable at index 3"}
java_bytecodes[0x26]={operation="dload_0", description="Load double from local variable at index 0"}
java_bytecodes[0x27]={operation="dload_1", description="Load double from local variable at index 1"}
java_bytecodes[0x28]={operation="dload_2", description="Load double from local variable at index 2"}
java_bytecodes[0x29]={operation="dload_3", description="Load double from local variable at index 3"}
java_bytecodes[0x2a]={operation="aload_0", description="Load reference from local variable at index 0"}
java_bytecodes[0x2b]={operation="aload_1", description="Load reference from local variable at index 1"}
java_bytecodes[0x2c]={operation="aload_2", description="Load reference from local variable at index 2"}
java_bytecodes[0x2d]={operation="aload_3", description="Load reference from local variable at index 3"}
java_bytecodes[0x2e]={operation="iaload", description="Load int from array"}
java_bytecodes[0x2f]={operation="laload", description="Load long from array"}
java_bytecodes[0x30]={operation="faload", description="Load float from array"}
java_bytecodes[0x31]={operation="daload", description="Load double from array"}
java_bytecodes[0x32]={operation="aaload", description="Load reference from array"}
java_bytecodes[0x33]={operation="baload", description="Load byte or boolean from array"}
java_bytecodes[0x34]={operation="caload", description="Load char from array"}
java_bytecodes[0x35]={operation="saload", description="Load short from array"}
java_bytecodes[0x36]={operation="istore", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Store float into local variable"}
java_bytecodes[0x37]={operation="lstore", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Store float into local variable"}
java_bytecodes[0x38]={operation="fstore", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Store float into local variable"}
java_bytecodes[0x39]={operation="dstore", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Store double into local variable"}
java_bytecodes[0x3a]={operation="astore", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, wideparameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Store reference into local variable"}
java_bytecodes[0x3b]={operation="istore_0", description="Store int into local variable at index 0"}
java_bytecodes[0x3c]={operation="istore_1", description="Store int into local variable at index 1"}
java_bytecodes[0x3d]={operation="istore_2", description="Store int into local variable at index 2"}
java_bytecodes[0x3e]={operation="istore_3", description="Store int into local variable at index 3"}
java_bytecodes[0x3f]={operation="lstore_0", description="Store long into local variable at index 0"}
java_bytecodes[0x40]={operation="lstore_1", description="Store long into local variable at index 1"}
java_bytecodes[0x41]={operation="lstore_2", description="Store long into local variable at index 2"}
java_bytecodes[0x42]={operation="lstore_3", description="Store long into local variable at index 3"}
java_bytecodes[0x43]={operation="fstore_0", description="Store float into local variable at index 0"}
java_bytecodes[0x44]={operation="fstore_1", description="Store float into local variable at index 1"}
java_bytecodes[0x45]={operation="fstore_2", description="Store float into local variable at index 2"}
java_bytecodes[0x46]={operation="fstore_3", description="Store float into local variable at index 3"}
java_bytecodes[0x47]={operation="dstore_0", description="Store double into local variable at index 0"}
java_bytecodes[0x48]={operation="dstore_1", description="Store double into local variable at index 1"}
java_bytecodes[0x49]={operation="dstore_2", description="Store double into local variable at index 2"}
java_bytecodes[0x4a]={operation="dstore_3", description="Store double into local variable at index 3"}
java_bytecodes[0x4b]={operation="astore_0", description="Store reference into local variable at index 0"}
java_bytecodes[0x4c]={operation="astore_1", description="Store reference into local variable at index 1"}
java_bytecodes[0x4d]={operation="astore_2", description="Store reference into local variable at index 2"}
java_bytecodes[0x4e]={operation="astore_3", description="Store reference into local variable at index 3"}
java_bytecodes[0x4f]={operation="iastore", description="Store into int array"}
java_bytecodes[0x50]={operation="lastore", description="Store into long array"}
java_bytecodes[0x51]={operation="fastore", description="Store into float array"}
java_bytecodes[0x52]={operation="dastore", description="Store into double array"}
java_bytecodes[0x53]={operation="aastore", description="Store into reference array"}
java_bytecodes[0x54]={operation="bastore", description="Store into byte or boolean array"}
java_bytecodes[0x55]={operation="castore", description="Store into char array"}
java_bytecodes[0x56]={operation="sastore", description="Store into short array"}
java_bytecodes[0x57]={operation="pop", description="Pop the top operand stack value"}
java_bytecodes[0x58]={operation="pop", description="Pop the top one or two operand stack values"}
java_bytecodes[0x59]={operation="dup", description="Duplicate the top operand stack value"}
java_bytecodes[0x5a]={operation="dup_x1", description="Duplicate the top operand stack value and insert two values down"}
java_bytecodes[0x5b]={operation="dup_x2", description="Duplicate the top operand stack value and insert three values down"}
java_bytecodes[0x5c]={operation="dup2", description="Duplicate the top one or two operand stack values"}
java_bytecodes[0x5d]={operation="dup2_x1", description="Duplicate the top one or two operand stack values and insert two or three values down"}
java_bytecodes[0x5e]={operation="dup2_x2", description="Duplicate the top one or two operand stack values and insert two, three, or four values down "}
java_bytecodes[0x5f]={operation="swap", description="Swap the two operand stack values"}
java_bytecodes[0x60]={operation="iadd", description="Add int"}
java_bytecodes[0x61]={operation="ladd", description="Add long"}
java_bytecodes[0x62]={operation="fadd", description="Add float"}
java_bytecodes[0x63]={operation="dadd", description="Add double"}
java_bytecodes[0x64]={operation="isub", description="Subtract int"}
java_bytecodes[0x65]={operation="lsub", description="Subtract long"}
java_bytecodes[0x66]={operation="fsub", description="Subtract float"}
java_bytecodes[0x67]={operation="dsub", description="Subtract double"}
java_bytecodes[0x68]={operation="imul", description="Multiply int"}
java_bytecodes[0x69]={operation="lmul", description="Multiply long"}
java_bytecodes[0x6a]={operation="fmul", description="Multiply float"}
java_bytecodes[0x6b]={operation="dmul", description="Multiply double"}
java_bytecodes[0x6c]={operation="idiv", description="Divide int"}
java_bytecodes[0x6d]={operation="ldiv", description="Divide long"}
java_bytecodes[0x6e]={operation="fdiv", description="Divide float"}
java_bytecodes[0x6f]={operation="ddiv", description="Divide double"}
java_bytecodes[0x70]={operation="irem", description="Remainder int"}
java_bytecodes[0x71]={operation="lrem", description="Remainder long"}
java_bytecodes[0x72]={operation="frem", description="Remainder float"}
java_bytecodes[0x73]={operation="drem", description="Remainder double"}
java_bytecodes[0x74]={operation="ineg", description="Negate int"}
java_bytecodes[0x75]={operation="lneg", description="Negate long"}
java_bytecodes[0x76]={operation="fneg", description="Negate float"}
java_bytecodes[0x77]={operation="dneg", description="Negate double"}
java_bytecodes[0x78]={operation="ishl", description="Shift left int"}
java_bytecodes[0x79]={operation="lshl", description="Shift left long"}
java_bytecodes[0x7a]={operation="ishr", description="Shift right int"}
java_bytecodes[0x7b]={operation="lshr", description="Aritmetic shift right long"}
java_bytecodes[0x7c]={operation="iushr", description="Logical shift right int"}
java_bytecodes[0x7d]={operation="lushr", description="Logical shift right long"}
java_bytecodes[0x7e]={operation="iand", description="Boolean AND int"}
java_bytecodes[0x7f]={operation="land", description="Boolean AND long"}
java_bytecodes[0x80]={operation="ior", description="Boolean OR int"}
java_bytecodes[0x81]={operation="lor", description="Boolean OR long"}
java_bytecodes[0x82]={operation="ixor", description="Boolean XOR int"}
java_bytecodes[0x83]={operation="lxor", description="Boolean XOR long"}
java_bytecodes[0x84]={operation="iinc", parameters={bytecount=2, {paramname="index", paramtype="u1"}, {paramname="const", paramtype="s1"}}, wideparameters={bytecount=4, {paramname="index", paramtype="u2"}, {paramname="const", paramtype="s2"}}, description="Increment local variable by constant"}
java_bytecodes[0x85]={operation="i2l", description="Convert int to long"}
java_bytecodes[0x86]={operation="i2f", description="Convert int to float"}
java_bytecodes[0x87]={operation="i2d", description="Convert int to double"}
java_bytecodes[0x88]={operation="l2i", description="Convert long to int"}
java_bytecodes[0x89]={operation="l2f", description="Convert long to float"}
java_bytecodes[0x8a]={operation="l2d", description="Convert long to double"}
java_bytecodes[0x8b]={operation="f2i", description="Convert float to int"}
java_bytecodes[0x8c]={operation="f2l", description="Convert float to long"}
java_bytecodes[0x8d]={operation="f2d", description="Convert float to double"}
java_bytecodes[0x8e]={operation="d2i", description="Convert double to int"}
java_bytecodes[0x8f]={operation="d2l", description="Convert double to long"}
java_bytecodes[0x90]={operation="d2f", description="Convert double to float"}
java_bytecodes[0x91]={operation="i2b", description="Convert int to byte"}
java_bytecodes[0x92]={operation="i2c", description="Convert int to char"}
java_bytecodes[0x93]={operation="i2s", description="Convert int to short"}
java_bytecodes[0x94]={operation="lcmp", description="Compare long"}
java_bytecodes[0x95]={operation="fcmpl", description="Compare float"}
java_bytecodes[0x96]={operation="fcmpg", description="Compare float"}
java_bytecodes[0x97]={operation="dcmpl", description="Compare double"}
java_bytecodes[0x98]={operation="dcmpg", description="Compare double"}
java_bytecodes[0x99]={operation="ifeq", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison with 0 is equal"}
java_bytecodes[0x9a]={operation="ifne", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison with 0 is not equal"}
java_bytecodes[0x9b]={operation="iflt", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison with 0 is less"}
java_bytecodes[0x9c]={operation="ifge", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison with 0 is greater or equal"}
java_bytecodes[0x9d]={operation="ifgt", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison with 0 is greater"}
java_bytecodes[0x9e]={operation="ifle", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison with 0 is less or equal"}
java_bytecodes[0x9f]={operation="if_icmpeq", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison is equal"}
java_bytecodes[0xa0]={operation="if_icmpne", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison is not equal"}
java_bytecodes[0xa1]={operation="if_icmplt", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison is less"}
java_bytecodes[0xa2]={operation="if_icmpge", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison is greater or equal"}
java_bytecodes[0xa3]={operation="if_icmpgt", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison is greater"}
java_bytecodes[0xa4]={operation="if_icmple", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if int comparison is less or equal"}
java_bytecodes[0xa5]={operation="if_acmpeq", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if reference comparison is true"}
java_bytecodes[0xa6]={operation="if_acmpne", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if reference comparison is false"}
java_bytecodes[0xa7]={operation="goto", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch always"}
java_bytecodes[0xa8]={operation="jsr", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Jump subroutine"}
java_bytecodes[0xa9]={operation="ret", parameters={bytecount=1, {paramname="index", paramtype="u1"}}, description="Return from subroutine"}
java_bytecodes[0xaa]={operation="tableswitch", parameters={bytecount=calculateTableSwitch, {paramname="padding", paramtype="pad4"}, {paramname="default", paramtype="s4"},{paramname="low", paramtype="s4"},{paramname="high", paramtype="s4"},{paramname="array",paramtype="[s4]"}}, description="Access jump table by key match and jump"}
java_bytecodes[0xab]={operation="lookupswitch", parameters={bytecount=calculateLookupSwitch, {paramname="padding", paramtype="pad4"}, {paramname="default", paramtype="s4"},{paramname="npairs", paramtype="s4"},{paramname="array",paramtype="[s4s4]"}}, description="Access jump table by key match and jump"}
java_bytecodes[0xac]={operation="ireturn", description="Return int from method"}
java_bytecodes[0xad]={operation="lreturn", description="Return long from method"}
java_bytecodes[0xae]={operation="freturn", description="Return float from method"}
java_bytecodes[0xaf]={operation="dreturn", description="Return double from method"}
java_bytecodes[0xb0]={operation="areturn", description="Return reference from method"}
java_bytecodes[0xb1]={operation="return", description="Return void from method"}
java_bytecodes[0xb2]={operation="getstatic", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Get static field from class"}
java_bytecodes[0xb3]={operation="putstatic", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Set static field in class"}
java_bytecodes[0xb4]={operation="getfield", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Fetch field from object"}
java_bytecodes[0xb5]={operation="putfield", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Set field in object"}
java_bytecodes[0xb6]={operation="invokevirtual", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Invoke instance method; dispatch based on class"}
java_bytecodes[0xb7]={operation="invokespecial", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Invoke instance method; special handling for superclass, private, and instance initialization method invocations"}
java_bytecodes[0xb8]={operation="invokestatic", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Invoke static method"}
java_bytecodes[0xb9]={operation="invokeinterface", parameters={bytecount=4, {paramname="index", paramtype="u2"}, {paramname="count", paramtype="u1"}, {paramname="null", paramtype="01"}}, description="Invoke interface method"}
java_bytecodes[0xba]={operation="invokedynamic", parameters={bytecount=4, {paramname="index", paramtype="u2"}, {paramname="null", paramtype="02"}}, description="Invoke dynamic method"}
java_bytecodes[0xbb]={operation="new", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Create new object"}
java_bytecodes[0xbc]={operation="newarray", parameters={bytecount=2, {paramname="atype", paramtype="u1"}}, description="Create new array"}
java_bytecodes[0xbd]={operation="anewarray", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Create new array of reference"}
java_bytecodes[0xbe]={operation="arraylength", description="Get length of array"}
java_bytecodes[0xbf]={operation="athrow", description="Throw exception or error"}
java_bytecodes[0xc0]={operation="checkcast", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Check wheter object is of given type"}
java_bytecodes[0xc1]={operation="instanceof", parameters={bytecount=2, {paramname="index", paramtype="u2"}}, description="Determine if object is of given type"}
java_bytecodes[0xc2]={operation="monitorenter", description="Enter monitor for object"}
java_bytecodes[0xc3]={operation="monitorexit", description="Exit monitor for object"}
java_bytecodes[0xc4]={operation="wide", parameters={bytecount=nil, {paramname="instruction", paramtype="wide"}}, description="Extend local variable index by additional bytes"}
java_bytecodes[0xc5]={operation="multianewarray", parameters={bytecount=3, {paramname="index", paramtype="u2"}, {paramname="dimensions", paramtype="u1"}}, description="Create new multidimensional array"}
java_bytecodes[0xc6]={operation="ifnull", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if reference is null"}
java_bytecodes[0xc7]={operation="ifnonnull", parameters={bytecount=2, {paramname="branchoffset", paramtype="s2"}}, description="Branch if reference is not null"}
java_bytecodes[0xc8]={operation="goto_w", parameters={bytecount=4, {paramname="branchoffset", paramtype="s4"}}, description="Branch always(wide)"}
java_bytecodes[0xc9]={operation="jsr_w", parameters={bytecount=4, {paramname="branchoffset", paramtype="s4"}}, description="Jump subroutine(wide)"}

java_operations={}


function java_buildoptobytecodetable()
  --parse through java_bytecodes and fill in java_operations
  for x,y in pairs(java_bytecodes) do
    java_operations[y.operation]=java_bytecodes[x]
    java_operations[y.operation].byte=x
  end
end

function bytecodeDisassembler(bytes)
  --note: Bytes start at index 1.  Some instructions like values on an alignment of 4 byte, so keep in mind that that is (address-1) % 4
  local i,j

  result={}
  result.labels={}
  local byteindex=1
  local index=1
  while byteindex<=#bytes do
    local startindex=byteindex
    local wide=false

    if bytes[byteindex]==0xc4 then --wide
      wide=true
      byteindex=byteindex+1
    end

    local data=java_bytecodes[bytes[byteindex]]

    result[index]={}
    result[index].data=data
    result[index].operation=data.operation
    result[index].parameter=''
    result[index].byteindex=startindex-1

    local parameters=data.parameters
    if wide then
      parameters=data.wideparameters
    end

    if parameters~=nil then
      for i=1,#parameters do
        if parameters[i].paramtype=='s1' then
          result[index].parameter=result[index].parameter..signExtendByte(bytes[byteindex+1])..' '
        elseif parameters[i].paramtype=='s2' then
          result[index].parameter=result[index].parameter..signExtendWord(byteTableToWord({bytes[byteindex+2], bytes[byteindex+1]}))..' '
        elseif parameters[i].paramtype=='s4' then
            result[index].parameter=result[index].parameter..signExtendWord(byteTableToDword({bytes[byteindex+4], bytes[byteindex+3], bytes[byteindex+2], bytes[byteindex+1]}))..' '
          elseif parameters[i].paramtype=='u1' then
          result[index].parameter=result[index].parameter..bytes[byteindex+1]..' '
        elseif parameters[i].paramtype=='u2' then
          result[index].parameter=result[index].parameter..byteTableToWord({bytes[byteindex+2], bytes[byteindex+1]})..' '
        elseif parameters[i].paramtype=='u4' then
          result[index].parameter=result[index].parameter..byteTableToDword({bytes[byteindex+4], bytes[byteindex+3], bytes[byteindex+2], bytes[byteindex+1]})..' '
        end

        if (i==1) and (parameters[i].paramname=="branchoffset") then
          local destination=tonumber(result[index].byteindex+result[index].parameter)
          local label=nil

          for j=1, #result.labels do
            --check if it's already defined
            if result.labels[j].destination==destination then
              label=result.labels[j]
              break
            end

          end

          if label==nil then
            --new label, define it
            label={}
            label.destination=destination
            label.labelname='lbl'..(#result.labels+1)
            label.origins={}
            table.insert(result.labels, label)
          end


          result[index].parameter='lbl'..#result.labels


          table.insert(label.origins, startindex)




        end

      end
    end

    if parameters==nil then
      result[index].bytesize=1
    else
      if type(parameters.bytecount)=='function' then
        result[index].bytesize=1+parameters.bytecount(byteindex, bytes)
      else
        result[index].bytesize=1+parameters.bytecount
      end
    end

    --copy the bytes
    result[index].bytes={}
    for j=startindex, startindex+result[index].bytesize-1 do
      result[index].bytes[1+(j-startindex)]=bytes[j]
    end

    byteindex=byteindex+result[index].bytesize

    index=index+1
  end

  return result
end

function singleLineBytecodeAssembler(address, instruction, labels, updatelabels)
  local result=nil
  local operation
  local parameterstring
  local userparameters={}
  local wide=false
  local i,j,s
  s=0
  i=0

  for s in string.gmatch(instruction, "%S+") do
    if i==0 then
      if s~='wide' then
        operation=s
        i=i+1
      else
        wide=true
      end
    else
      userparameters[i]=s
      i=i+1
    end
  end

  parameterstring=''
  for i=1, #userparameters do
    parameterstring=parameterstring..userparameters[i]..' '
  end


  local data=java_operations[operation]

  if data~=nil then
    if type(data.bytecount)=='function' then
      error(data.operation..translate(' is currently not implemented'))
    end

    result={}
    result.bytes={}
    result.data=data
    result.operation=data.operation
    result.byteindex=address
    result.parameters=userparams
    result.parameter=parameterstring



    local parameters
    if wide then
      parameters=data.wideparameters
      if parameters==nil then
        error(translate('wide can not be used with ')..data.operation)
      end
      table.insert(result, 0xc4)
    else
      parameters=data.parameters
    end

    table.insert(result.bytes, data.byte)

    if parameters~=nil then
      if #userparameters~=#parameters then
        error(translate('Invalid amount of parameters provided for ')..data.operation)
      end


      for i=1, #parameters do
        local bytes={}

        if (i==1) and (parameters[i].paramname=="branchoffset") then --this instruction has a label reference
          --convert the parameter to the current label
          local label=nil

          if labels~=nil then
            for j=1, #labels do
              if labels[j].labelname==result.parameters[i] then
                label=labels[j]

                result.parameters[i]=labels[j].destination-address
              end
            end
          end

          if tonumber(result.parameters[i])==nil then
            error(translate("The label ")..userparameters[i]..translate("is not yet defined"))
          end

          if (updatelabels~=nil) and (updatelabels==true) then
            if label==nil then --add it
              label={}
              label.destination=result.parameters[i]
              label.labelname='lbl'..(#labels+1)
              label.origins={}
              table.insert(labels, label)
            end

            table.insert(label.origins, address)
          end




        end



        if parameters[i].paramtype=='s1' then
          bytes={tonumber(userparameters[i]) % 256}

        elseif parameters[i].paramtype=='s2' then
          bytes=wordToByteTable(tonumber(userparameters[i]) % 65536)
        elseif parameters[i].paramtype=='s4' then
          bytes=dwordToByteTable(tonumber(userparameters[i]) % 65536)
        elseif parameters[i].paramtype=='u1' then
          bytes={tonumber(userparameters[i]) % 256}
        elseif parameters[i].paramtype=='u2' then
          bytes=wordToByteTable(tonumber(userparameters[i]) % 65536)
        elseif parameters[i].paramtype=='u4' then
          bytes=dwordToByteTable(tonumber(userparameters[i]) % 65536)
        else
          error(translate('This instruction is currently not implemented'))
        end

        for j=1, #bytes do
          table.insert(result.bytes, bytes[j])
        end
      end

    end

    result.bytesize=#result.bytes
  else
    error(translate('unknown instruction:')..operation);
  end

  return result
end



function javaclass_applyAssembleCommand(class, method, byteindex, instruction, insert)
  local codeattribute=javaclass_method_findCodeAttribute(method)

  local labels=codeattribute.code.labels
  local exceptions=codeattribute.exception_table


  --get the size of the instruction to be assembled (plus side: unlike intel with it's horrible and easily reachable 128 byte limit for small branches, java gives 32768 bytes, so less chance of an insertion causing problems)
  local newcode
  local codeattribute_codeIndex=nil


  newcode=singleLineBytecodeAssembler(byteindex, instruction, labels,false) --get the size, but don't mess with the labels

  local i,j

  local startindex=byteindex
  local offset=0

  --update the labels and exception table based on the size

  startindex=byteindex

  if insert then
    offset=newcode.bytesize
  else
    local oldcode=nil

    for i=1, #codeattribute.code do
      if codeattribute.code[i].byteindex==byteindex then
        oldcode=codeattribute.code[i]
        break
      end
    end

    if oldcode==nil then
      error(translate('You can only replace instructions on an instruction boundary'))
    end

    offset=newcode.bytesize-oldcode.bytesize
  end


  javaclass_updateOffsets(class, method, startindex, offset)

  --labels and other byteindex are updated. Now you can insert/modify the instruction

  newcode=singleLineBytecodeAssembler(byteindex, instruction, labels,true)

  local currentbyteindex=0
  for i=1, #codeattribute.code do
    if insert then
      --the byteindex has already been updated

      if currentbyteindex==byteindex then
        table.insert(codeattribute.code, i, newcode)
        break
      end
    else
      if currentbyteindex==byteindex then
        codeattribute.code[i]=newcode
        break
      end
    end

    currentbyteindex=currentbyteindex+codeattribute.code[i].bytesize
  end


end

function javaclass_updateOffsets(class, method, startindex, offset)

  print(translate("si=")..startindex..translate(" offset=")..offset)
  local i,j

  local codeattribute=javaclass_method_findCodeAttribute(method)
  local code=codeattribute.code


  local labels=codeattribute.code.labels
  local exceptions=codeattribute.exception_table

  for i=1, #labels do
    if labels[i].destination>=startindex then --the destination has been updated
      labels[i].destination=labels[i].destination+offset
    end

    for j=1, #labels[i].origins do
      local originaladdress=labels[i].origins[j]

      if originaladdress>=startindex then --this got shifted as well
        labels[i].origins[j]=originaladdress+offset
      end


      --adjust this branch instruction (note though, if originaddress was bigger than destination address, there should be no branchoffset change)
      --find the instruction in the code
      local k

      for k=1, #codeattribute.code do
        if codeattribute.code[k].byteindex==originaladdress then --check the original address (the byteindex hasn't been updated)
          local size=codeattribute.code[k].dataparameters[1].paramtype:sub(2,2) --branches only have type s2 or s4
          local newbranchoffset=labels[i].destination-labels[i].origins[j]
          local newbytes


          if size=='2' then --remember, it's big endian
            newbytes=wordToByteTable(newbranchoffset)
            codeattribute.code[k].bytes[2]=newbytes[2]
            codeattribute.code[k].bytes[3]=newbytes[1]
          elseif size=='4' then
            newbytes=dwordToByteTable(newbranchoffset)
            codeattribute.code[k].bytes[2]=newbytes[4]
            codeattribute.code[k].bytes[3]=newbytes[3]
            codeattribute.code[k].bytes[4]=newbytes[2]
            codeattribute.code[k].bytes[5]=newbytes[1]
          end
        end



      end
    end
  end

  --update tableswitch and lookupswitch commands
  for i=1,#code do
    if code[i].data.byte==0xaa then
      --tableswitch
      local defaultbyteindex=code[i].byteindex+1
      local defaultindex=2

      --find alignment padding
      local rest=defaultbyteindex%4
      if rest>0 then
        defaultindex=defaultindex+(4-rest)
        defaultbyteindex=defaultbyteindex+(4-rest)
      end

      local lowindex=defaultindex+4
      local highindex=lowindex+4
      local offsetindex=highindex+4

      local low=byteTableToDword({code[i].bytes[lowindex+3], code[i].bytes[lowindex+2], code[i].bytes[lowindex+1], code[i].bytes[lowindex]}) --big endian
      local high=byteTableToDword({code[i].bytes[highindex+3], code[i].bytes[highindex+2], code[i].bytes[highindex+1], code[i].bytes[highindex]})

      local count=high-low+1

      for j=1,count do
        local currentoffsetindex=offsetindex+(i-1)*4
        local currentoffset=byteTableToDword({code[i].bytes[currentoffsetindex+3], code[i].bytes[currentoffsetindex+2], code[i].bytes[currentoffsetindex+1], code[i].bytes[currentoffsetindex]})

        if currentoffset>=startindex then
          --update the address it points to
          currentoffset=currentoffset+offset

          local newbytes=dwordToByteTable(currentoffset)

          code.bytes[currentoffsetindex]=newbytes[4]
          code.bytes[currentoffsetindex+1]=newbytes[3]
          code.bytes[currentoffsetindex+2]=newbytes[2]
          code.bytes[currentoffsetindex+3]=newbytes[1]
        end
      end



    elseif code[i].data.byte==0xab then
      --lookupswitch
      local defaultbyteindex=code[i].byteindex+1
      local defaultindex=2

      --find alignment padding
      local rest=defaultbyteindex%4
      if rest>0 then
        defaultindex=defaultindex+(4-rest)
        defaultbyteindex=defaultbyteindex+(4-rest)
      end

      local countindex=defaultindex+4
      local count=byteTableToDword({code[i].bytes[countindex+3], code[i].bytes[countindex+2], code[i].bytes[countindex+1], code[i].bytes[countindex]})


      for j=1,count do
        local currentoffsetindex=countindex+(j-1)*8+4
        local currentoffset=byteTableToDword({code[i].bytes[currentoffsetindex+3], code[i].bytes[currentoffsetindex+2], code[i].bytes[currentoffsetindex+1], code[i].bytes[currentoffsetindex]})

        if currentoffset>=startindex then
          --update the address it points to
          currentoffset=currentoffset+offset

          local newbytes=dwordToByteTable(currentoffset)

          code.bytes[currentoffsetindex]=newbytes[4]
          code.bytes[currentoffsetindex+1]=newbytes[3]
          code.bytes[currentoffsetindex+2]=newbytes[2]
          code.bytes[currentoffsetindex+3]=newbytes[1]
        end

      end


    end
  end


  --update the exception table

  for i=1, #exceptions do
    if exceptions[i].start_pc>=startindex then
      exceptions[i].start_pc=exceptions[i].start_pc+offset
    end

    if exceptions[i].start_pc>=startindex then
      exceptions[i].end_pc=exceptions[i].end_pc+offset
    end

    if exceptions[i].start_pc>=startindex then
      exceptions[i].handler_pc=exceptions[i].handler_pc+offset
    end
  end



  --finally update the byteindexes
  for i=1, #codeattribute.code do
    if codeattribute.code[i].byteindex>=startindex then
      codeattribute.code[i].byteindex=codeattribute.code[i].byteindex+offset
    end
  end
end




function javaclasseditor_editMethod_fillInstructionsListview(lv, method)
  local i
  local codeattribute=javaclass_method_findCodeAttribute(method)
  local code=codeattribute.code
  local labels=code.labels
  local exceptions=codeattribute.exception_table
  local haslabels=false
  local hasexceptions=false

  lv.Items.clear()

  for i=1, #code do
    local j
    local item=lv.Items.add()
    local bytestring=''

    item.Caption=i-1


    for j=1, code[i].bytesize do
      bytestring=bytestring..string.format("%.2x ", code[i].bytes[j])
    end

    item.SubItems.Add(code[i].byteindex .. ':'..bytestring) --display as 0 start


    local labelname=''
    if #labels>0 then
      haslabels=true
      for j=1, #labels do
        if code[i].byteindex==labels[j].destination then
          labelname='lbl'..j
          break
        end
      end

    end


    local exceptionstr=''
    if #exceptions>0 then
      hasexceptions=true
      for j=1, #exceptions do
        if code[i].byteindex==exceptions[j].start_pc then
          exceptionstr=exceptionstr..'start'..j..' '
        end

        if code[i].byteindex==exceptions[j].end_pc then
          exceptionstr=exceptionstr..'end'..j..' '
        end

        if code[i].byteindex==exceptions[j].handler_pc then
          exceptionstr=exceptionstr..'handler'..j..' '
        end
      end
    end
    item.SubItems.Add(labelname)
    item.SubItems.Add(exceptionstr)
    item.SubItems.Add(code[i].operation..' '..code[i].parameter)
  end
end

function javaclasseditor_editMethod_insertLine(sender)
  local classMethod=getRef(sender.Tag)
  local codeattribute=javaclass_method_findCodeAttribute(classMethod.method)
  local code=codeattribute.code



  local lv=classMethod.method.editor.lvInstructions
  local linenr
  if lv.Selected==nil then
    linenr=0
  else
    linenr=lv.Selected.Index
  end

  local byteindex=code[linenr+1].byteindex


  local line=inputQuery(translate('Insert line'), translate('Input the java assembly code you wish to insert at line ')..linenr..'(byteindex '..byteindex..')','')
  if line~=nil then
    --showMessage('Assembling '..line)
    --assemble
    javaclass_applyAssembleCommand(classMethod.class, classMethod.method, byteindex, line, true)

    --show update
    javaclasseditor_editMethod_fillInstructionsListview(classMethod.method.editor.lvInstructions, classMethod.method)
  end
end

function javaclasseditor_editMethod_editLine(sender)
  local classMethod=getRef(sender.Tag)
  local codeattribute=javaclass_method_findCodeAttribute(classMethod.method)
  local code=codeattribute.code



  local lv=classMethod.method.editor.lvInstructions
  local linenr
  if lv.Selected==nil then
    return
  else
    linenr=lv.Selected.Index
  end

  local byteindex=code[linenr+1].byteindex
  local originalcode=code[linenr+1].operation..' '..code[linenr+1].parameter


  local line=inputQuery(translate('Edit line'), translate('Input the java assembly code you wish to insert at line ')..linenr..'(byteindex '..byteindex..')',originalcode)
  if line~=nil then
    javaclass_applyAssembleCommand(classMethod.class, classMethod.method, byteindex, line, false)

    --show update
    javaclasseditor_editMethod_fillInstructionsListview(classMethod.method.editor.lvInstructions, classMethod.method)
  end
end

function javaclasseditor_editMethod_defineLabel(sender)
  local classMethod=getRef(sender.Tag)
  local codeattribute=javaclass_method_findCodeAttribute(classMethod.method)
  local code=codeattribute.code
  local labels=code.labels

  local lv=classMethod.method.editor.lvInstructions
  local linenr
  if lv.Selected==nil then
    return
  else
    linenr=lv.Selected.Index
  end

  local byteindex=code[linenr+1].byteindex
  local labelname=inputQuery(translate('Define new label'), translate('Give a labelname for line ')..linenr..'(byteindex '..byteindex..')','')

  if (labelname~=nil) and (labelname~='') then
    local i
    --check if it already exists
    for i=1,#labels do
      if labels[i].labelname==labelname then
        error(translate('There is already a label with this name'))
      end
    end

    --still here, so the it wasn't in the list
    local newlabel={}
    newlabel.labelname=labelname
    newlabel.destination=byteindex
    newlabel.origins={}
    table.insert(labels, newlabel)
  end
end

function javaclasseditor_editMethod_applyUpdates(method)
  --converts the bytes of the interpreted code into the .info field of the code attribute (used by the writer)
  local i
  local codeattribute=javaclass_method_findCodeAttribute(method)
  local code=codeattribute.code

  codeattribute.max_stack=tonumber(method.editor.edtMaxStack.Text)
  codeattribute.max_locals=tonumber(method.editor.edtMaxLocals.Text)

ca=codeattribute



  local s={}  --write stream
  s.data=''
  s.index=0 --not really needed by writes but helps with debugging

  java_write_u2(s, codeattribute.max_stack)
  java_write_u2(s, codeattribute.max_locals)


  local bytesize=0
  for i=1,#code do
    bytesize=bytesize+#code[i].bytes
  end

  java_write_u4(s, bytesize) --code_length

  for i=1,#code do
    local j
    for j=1,#code[i].bytes do
      s.data=s.data..string.char(code[i].bytes[j])
    end
  end

  s.index=s.index+#code

  --exception table  (genius to allow code size to be specified by 4 bytes but not exception addresses)
  java_write_u2(s, codeattribute.exception_table_length);
  for i=1, codeattribute.exception_table_length do
    java_write_u2(s, codeattribute.exception_table[i].start_pc)
  java_write_u2(s, codeattribute.exception_table[i].end_pc)
  java_write_u2(s, codeattribute.exception_table[i].handler_pc)
  java_write_u2(s, codeattribute.exception_table[i].catch_type)
  end

  java_write_u2(s, codeattribute.attributes_count)
  java_writeAttributes(s, codeattribute.attributes, codeattribute.attributes_count)

  print(string.format(translate("old bsize=%d new bsize=%d"), codeattribute.code_length, bytesize))

  print(string.format(translate("old size=%d new size=%d"), #codeattribute.info, #s.data))


  codeattribute.info=s.data
  codeattribute.attribute_length=#codeattribute.info
end


function btnApplyChangesClick(sender)
  --apply the changes
  local classMethod=getRef(sender.Tag)
  local method=classMethod.method

  javaclasseditor_editMethod_applyUpdates(method)


  if method.editor.callbackfunction~=nil then
    method.editor.callbackfunction(classMethod.class, method.editor.callbackparam)
  end
end


function javaclasseditor_editMethod(class, method, callbackfunction, callbackparam)
  --create a gui for this method
  --note that throughout this file we call the parsed class "class".  It's not the jclass object

  if method.editor==nil then
    local ca=javaclass_method_findCodeAttribute(method)
    local classMethodRef=createRef({class=class, method=method})

    --build a gui
    method.editor={}
    method.editor.form=createForm()
    method.editor.form.onClose=nil --do not destroy on close
    method.editor.form.tag=classMethodRef --this way gui launched functions can find the base form and fetch this data if it needs it
    method.editor.form.borderstyle=bsSizeable

    method.editor.form.width=640
    method.editor.form.height=480

    method.editor.form.caption=translate("Method: ")..class.constant_pool[class.constant_pool[class.this_class].name_index].utf8.."."..class.constant_pool[method.name_index].utf8
    method.editor.form.Position=poScreenCenter


    method.editor.lblMaxStack=createLabel(method.editor.form)
    method.editor.lblMaxLocals=createLabel(method.editor.form)

    method.editor.lblMaxStack.caption=translate("Max Stack")
    method.editor.lblMaxLocals.caption=translate("Max Locals")

    method.editor.edtMaxStack=createEdit(method.editor.form)
    method.editor.edtMaxLocals=createEdit(method.editor.form)

    method.editor.edtMaxStack.Text=ca.max_stack
    method.editor.edtMaxLocals.Text=ca.max_locals


    method.editor.lblMaxStack.AnchorSideLeft.Control=method.editor.form
    method.editor.lblMaxStack.AnchorSideLeft.Side=asrLeft
    method.editor.lblMaxStack.AnchorSideTop.Control=method.editor.form
    method.editor.lblMaxStack.AnchorSideTop.Side=asrTop
    method.editor.lblMaxStack.Anchors="[akTop, akLeft]"

    method.editor.edtMaxStack.AnchorSideLeft.Control=method.editor.form
    method.editor.edtMaxStack.AnchorSideLeft.Side=asrLeft
    method.editor.edtMaxStack.AnchorSideTop.Control=method.editor.lblMaxStack
    method.editor.edtMaxStack.AnchorSideTop.Side=asrBottom
    method.editor.edtMaxStack.Anchors="[akTop, akLeft]"

    method.editor.lblMaxLocals.AnchorSideLeft.Control=method.editor.edtMaxStack
    method.editor.lblMaxLocals.AnchorSideLeft.Side=asrRight
    method.editor.lblMaxLocals.AnchorSideTop.Control=method.editor.form
    method.editor.lblMaxLocals.AnchorSideTop.Side=asrTop
    method.editor.lblMaxLocals.BorderSpacing.Left=6

    method.editor.lblMaxLocals.Anchors="[akTop, akLeft]"

    method.editor.edtMaxLocals.AnchorSideLeft.Control=method.editor.lblMaxLocals
    method.editor.edtMaxLocals.AnchorSideLeft.Side=asrLeft
    method.editor.edtMaxLocals.AnchorSideTop.Control=method.editor.edtMaxStack
    method.editor.edtMaxLocals.AnchorSideTop.Side=asrTop
    method.editor.edtMaxLocals.Anchors="[akTop, akLeft]"


    method.editor.btnApplyChanges=createButton(method.editor.form)
    method.editor.btnApplyChanges.AutoSize=true
    method.editor.btnApplyChanges.Caption=translate("Save changes")
    method.editor.btnApplyChanges.OnClick=btnApplyChangesClick
    method.editor.btnApplyChanges.Tag=classMethodRef

    method.editor.btnApplyChanges.AnchorSideLeft.Control=method.editor.edtMaxLocals
    method.editor.btnApplyChanges.AnchorSideLeft.Side=asrRight
    method.editor.btnApplyChanges.AnchorSideTop.Control=method.editor.edtMaxLocals
    method.editor.btnApplyChanges.AnchorSideTop.Side=asrTop
    method.editor.lblMaxLocals.BorderSpacing.Left=12
    method.editor.btnApplyChanges.Anchors="[akTop, akLeft]"



    method.editor.lvInstructions=createListView(method.editor.form)
    method.editor.lvInstructions.Name="lvInstructions"

    local columns=method.editor.lvInstructions.Columns
    local lcIndex=columns.add()
    local lcByteIndex=columns.add()
    local lcLabel=columns.add()
    local lcException=columns.add()
    local lcInstruction=columns.add()


    lcIndex.Caption=translate("Index")
    lcByteIndex.Caption=translate("ByteIndex")
    lcLabel.Caption=translate("Label")
    lcException.Caption=translate("Exception")
    lcInstruction.Caption=translate("Instruction")

    lcByteIndex.width=100

    lcInstruction.AutoSize=true


    method.editor.lvInstructions.AutoWidthLastColumn=true
    method.editor.lvInstructions.RowSelect=true
    method.editor.lvInstructions.HideSelection=false
    method.editor.lvInstructions.ReadOnly=true

    method.editor.lvInstructions.AnchorSideLeft.Control=method.editor.form
    method.editor.lvInstructions.AnchorSideLeft.Side=asrLeft
    method.editor.lvInstructions.AnchorSideTop.Control=method.editor.edtMaxStack
    method.editor.lvInstructions.AnchorSideTop.Side=asrBottom
    method.editor.lvInstructions.BorderSpacing.Top=6

    method.editor.lvInstructions.AnchorSideRight.Control=method.editor.form
    method.editor.lvInstructions.AnchorSideRight.Side=asrRight
    method.editor.lvInstructions.AnchorSideBottom.Control=method.editor.form
    method.editor.lvInstructions.AnchorSideBottom.Side=asrBottom

    method.editor.lvInstructions.Anchors="[akTop, akLeft, akRight, akBottom]"

    --create a popupmenu for adding/editing lines, and adding new labels
    method.editor.pmEdit=createPopupMenu(method.editor.form)
    method.editor.pmEdit.Tag=classMethodRef

    local miInsertLine=createMenuItem(method.editor.pmEdit)
    miInsertLine.caption=translate("Insert line")
    miInsertLine.onClick=javaclasseditor_editMethod_insertLine
    miInsertLine.Shortcut="Ctrl+I"
    miInsertLine.Tag=classMethodRef

    local miEditLine=createMenuItem(method.editor.pmEdit)
    miEditLine.caption=translate("Edit line")
    miEditLine.onClick=javaclasseditor_editMethod_editLine
    miEditLine.Shortcut="Ctrl+E"
    miEditLine.Tag=classMethodRef

    local miDefineLine=createMenuItem(method.editor.pmEdit)
    miDefineLine.caption=translate("Define label")
    miDefineLine.onClick=javaclasseditor_editMethod_defineLabel
    miDefineLine.Shortcut="Ctrl+L"
    miDefineLine.Tag=classMethodRef


    method.editor.lvInstructions.OnDblClick=javaclasseditor_editMethod_editLine
    method.editor.lvInstructions.Tag=classMethodRef


    method.editor.pmEdit.Items.add(miInsertLine)
    method.editor.pmEdit.Items.add(miEditLine)
    method.editor.pmEdit.Items.add(miDefineLine)

    method.editor.lvInstructions.PopupMenu=method.editor.pmEdit

    method.editor.callbackfunction=callbackfunction
    method.editor.callbackparam=callbackparam
  end

  javaclasseditor_editMethod_fillInstructionsListview(method.editor.lvInstructions, method)

  method.editor.form.show()

end




java_buildoptobytecodetable()
