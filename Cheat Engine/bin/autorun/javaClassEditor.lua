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

java_bytecodes[0x01]={operation="aconst_null", description="Push null"}
java_bytecodes[0x0b]={operation="fconst_0", description="Push float 0.0"}
java_bytecodes[0x0c]={operation="fconst_1", description="Push float 1.0"}
java_bytecodes[0x0d]={operation="fconst_2", description="Push float 2.0"}
java_bytecodes[0x0e]={operation="dconst_0", description="Push double 0.0"}
java_bytecodes[0x0f]={operation="dconst_1", description="Push double 1.0"}
java_bytecodes[0x10]={operation="bipush", parameters={bytecount=1, paramname="byte"}, description="Push byte"}
java_bytecodes[0x17]={operation="fload", parameters={bytecount=1, paramname="index"}, description="Load float from local variable"}
java_bytecodes[0x18]={operation="dload", parameters={bytecount=1, paramname="index"}, description="Load double from local variable"}
java_bytecodes[0x19]={operation="aload", parameters={bytecount=1, paramname="index"}, description="Load reference from local variable"}
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
java_bytecodes[0x30]={operation="faload", description="Load float from array"}
java_bytecodes[0x31]={operation="daload", description="Load double from array"}
java_bytecodes[0x32]={operation="aaload", description="Load reference from array"}
java_bytecodes[0x33]={operation="baload", description="Load byte or boolean from array"}
java_bytecodes[0x34]={operation="caload", description="Load char from array"}
java_bytecodes[0x38]={operation="fstore", parameters={bytecount=1, paramname="index"}, description="Store float into local variable"}
java_bytecodes[0x39]={operation="dstore", parameters={bytecount=1, paramname="index"}, description="Store double into local variable"}
java_bytecodes[0x3a]={operation="astore", parameters={bytecount=1, paramname="index"}, description="Store reference into local variable"}
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
java_bytecodes[0x51]={operation="fastore", description="Store into float array"}
java_bytecodes[0x52]={operation="dastore", description="Store into double array"}
java_bytecodes[0x53]={operation="aastore", description="Store into reference array"}
java_bytecodes[0x54]={operation="bastore", description="Store into byte or boolean array"}
java_bytecodes[0x55]={operation="castore", description="Store into char array"}
java_bytecodes[0x59]={operation="dup", description="Duplicate the top operand stack value"}
java_bytecodes[0x5a]={operation="dup_x1", description="Duplicate the top operand stack value and insert two values down"}
java_bytecodes[0x5b]={operation="dup_x2", description="Duplicate the top operand stack value and insert three values down"}
java_bytecodes[0x5c]={operation="dup2", description="Duplicate the top one or two operand stack values"}
java_bytecodes[0x5d]={operation="dup2_x1", description="Duplicate the top one or two operand stack values and insert two or three values down"}
java_bytecodes[0x5e]={operation="dup2_x2", description="Duplicate the top one or two operand stack values and insert two, three, or four values down "}
java_bytecodes[0x62]={operation="fadd", description="Add float"}
java_bytecodes[0x63]={operation="dadd", description="Add double"}
java_bytecodes[0x66]={operation="fsub", description="Subtract float"}
java_bytecodes[0x67]={operation="dsub", description="Subtract double"}
java_bytecodes[0x6a]={operation="fmul", description="Multiply float"}
java_bytecodes[0x6b]={operation="dmul", description="Multiply double"}
java_bytecodes[0x6e]={operation="fdiv", description="Divide float"}
java_bytecodes[0x6f]={operation="ddiv", description="Divide double"}
java_bytecodes[0x72]={operation="frem", description="Remainder float"}
java_bytecodes[0x73]={operation="drem", description="Remainder double"}
java_bytecodes[0x76]={operation="fneg", description="Negate float"}
java_bytecodes[0x77]={operation="dneg", description="Negate double"}
java_bytecodes[0x8b]={operation="f2i", description="Convert float to int"}
java_bytecodes[0x8c]={operation="f2l", description="Convert float to long"}
java_bytecodes[0x8d]={operation="f2d", description="Convert float to double"}
java_bytecodes[0x8e]={operation="d2i", description="Convert double to int"}
java_bytecodes[0x8f]={operation="d2l", description="Convert double to long"}
java_bytecodes[0x90]={operation="d2f", description="Convert double to float"}
java_bytecodes[0x95]={operation="fcmpl", description="Compare float"}
java_bytecodes[0x96]={operation="fcmpg", description="Compare float"}
java_bytecodes[0x97]={operation="dcmpl", description="Compare double"}
java_bytecodes[0x98]={operation="dcmpg", description="Compare double"}
java_bytecodes[0xae]={operation="freturn", description="Return float from method"}
java_bytecodes[0xaf]={operation="dreturn", description="Return double from method"}
java_bytecodes[0xb0]={operation="areturn", description="Return reference from method"}
java_bytecodes[0xbd]={operation="getfield", parameters={bytecount=2, paramname="index"}, description="Fetch field from object"}
java_bytecodes[0xbd]={operation="anewarray", parameters={bytecount=2, paramname="index"}, description="Create new array of reference"}
java_bytecodes[0xbe]={operation="arraylength", description="Get length of array"}
java_bytecodes[0xbf]={operation="athrow", description="Throw exception or error"}
java_bytecodes[0xc0]={operation="checkcast", parameters={bytecount=2, paramname="index"}, description="Check wheter object is of given type"}


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
