--Java class editor
--[[
This will show an userinterface for editing java classes and will return a list of "patch" commands
that can be used with the runtime java class edit commands

e.g:
InsertBytecode(spot, command)
ModifyBytecode(spot, command)
DeleteBytecode(spot)  (could be ModifyBytecode(spot,"nop") )


The user should not have to know about exceptions and how their positions change with each insert/delete

--]]

--http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html


java_bytecodes={}

--[[
paramtypes:
  s1=signed 1 byte
  s2=signed 2 byte
  u1=unsigned 1 byte
  u2=unsigned 2 byte

  02=null 2 bytes

--]]

function calculateTableSwitch(address, bytes)
  local result=nil

  if bytes[1]==0xaa then
    local padding=0

	if (address-1) % 4>0 then
	  padding=4-(address-1) % 4
	end

    --local defaultbyte=byteTableToDword({bytes[8],bytes[7],bytes[6],bytes[5]})
	local low=byteTableToDword({bytes[1+padding+7],bytes[1+padding+6],bytes[1+padding+5],bytes[1+padding+4]})
	local high=byteTableToDword({bytes[1+padding+11],bytes[1+padding+10],bytes[1+padding+9],bytes[1+padding+8]})
	result=padding+4+4+4+((high-low)+1)*4 --paramsize
  end

  return result;
end

function calculateLookupSwitch(address, bytes)
  local result=nil

  if bytes[1]==0xab then
    local padding=0

	if (address-1) % 4>0 then
	  padding=4-(address-1) % 4
	end

    --local defaultbyte=byteTableToDword({bytes[8],bytes[7],bytes[6],bytes[5]})
	local npairs=byteTableToDword({bytes[1+padding+7],bytes[1+padding+6],bytes[1+padding+5],bytes[1+padding+4]})
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
java_bytecodes[0xaa]={operation="tableswitch", parameters={bytecount=calculateTableSwitch, {paramname="padding", paramtype="pad4"}, {paramname="default", paramtype="s4"},{paramname="low", paramtype="s4"},{paramname="high", paramtype="s4"},{paramname="array",paramtype="[s4s4]"}}, description="Access jump table by key match and jump"}
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
  result={}
end


java_buildoptobytecodetable()
