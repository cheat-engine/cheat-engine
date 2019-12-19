if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'Java.po')
end

require([[autorun\javaClassEditor]])

--parser for .class files and java bytecode
--http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html

--constant type values
java_CONSTANT_Class=7
java_CONSTANT_Fieldref=9
java_CONSTANT_Methodref=10
java_CONSTANT_InterfaceMethodref=11
java_CONSTANT_String=8
java_CONSTANT_Integer=3
java_CONSTANT_Float=4
java_CONSTANT_Long=5
java_CONSTANT_Double=6
java_CONSTANT_NameAndType=12
java_CONSTANT_Utf8=1
java_CONSTANT_MethodHandle=15
java_CONSTANT_MethodType=16
java_CONSTANT_InvokeDynamic=18


function java_read_u4(stream)
  local b={string.byte(stream.data, stream.index,stream.index+4-1)}
  stream.index=stream.index+4

  return byteTableToDword({b[4],b[3],b[2],b[1]})
end


function java_read_u2(stream)
  local b={string.byte(stream.data, stream.index,stream.index+2-1)}
  stream.index=stream.index+2

  return byteTableToWord({b[2],b[1]})
end

function java_read_u1(stream)
  local result=string.byte(stream.data, stream.index)
  stream.index=stream.index+1

  return result
end

function java_parseConstantPool_Class(stream)
  result={}
  result.tag=java_CONSTANT_Class
  result.name_index=java_read_u2(stream)
  return result
end

function java_parseConstantPool_Fieldref(stream)
  result={}
  result.tag=java_CONSTANT_Fieldref
  result.class_index=java_read_u2(stream)
  result.name_and_type_index=java_read_u2(stream)
  return result
end

function java_parseConstantPool_Methodref(stream)
  result={}
  result.tag=java_CONSTANT_Methodref
  result.class_index=java_read_u2(stream)
  result.name_and_type_index=java_read_u2(stream)
  return result
end

function java_parseConstantPool_InterfaceMethodref(stream)
  result={}
  result.tag=java_CONSTANT_InterfaceMethodref
  result.class_index=java_read_u2(stream)
  result.name_and_type_index=java_read_u2(stream)
  return result
end


function java_parseConstantPool_String(stream)
  result={}
  result.tag=java_CONSTANT_String
  result.string_index=java_read_u2(stream)

  return result
end


function java_parseConstantPool_Integer(stream)
  result={}
  result.tag=java_CONSTANT_Integer
  result.bytes=java_read_u4(stream)
  result.value=result.bytes
  return result
end

function java_parseConstantPool_Float(stream)
  result={}
  result.tag=java_CONSTANT_Float
  result.bytes=java_read_u4(stream)

  result.value=byteTableToFloat(dwordToByteTable(result.bytes))
  return result
end


function java_parseConstantPool_Long(stream)
  result={}
  result.tag=java_CONSTANT_Long
  result.high_bytes=java_read_u4(stream)
  result.low_bytes=java_read_u4(stream)

  local hb=dwordToByteTable(result.high_bytes)
  local lb=dwordToByteTable(result.low_bytes)

  local i
  for i=1, 4 do
    lb[i+4]=hb[i]
  end

  result.value=byteTableToQword(lb)
  return result
end

function java_parseConstantPool_Double(stream)
  result={}
  result.tag=java_CONSTANT_Double
  result.high_bytes=java_read_u4(stream)
  result.low_bytes=java_read_u4(stream)

  local hb=dwordToByteTable(result.high_bytes)
  local lb=dwordToByteTable(result.low_bytes)

  local i
  for i=1, 4 do
    lb[i+4]=hb[i]
  end

  result.value=byteTableToDouble(lb)

  return result
end

function java_parseConstantPool_NameAndType(stream)
  result={}
  result.tag=java_CONSTANT_NameAndType
  result.name_index=java_read_u2(stream)
  result.descriptor_index=java_read_u2(stream)
  return result
end

function java_parseConstantPool_Utf8(stream)
  result={}
  result.tag=java_CONSTANT_Utf8
  result.length=java_read_u2(stream)
  result.bytes={string.byte(stream.data, stream.index, stream.index+result.length-1)}
  result.utf8=string.sub(stream.data, stream.index, stream.index+result.length-1)

  stream.index=stream.index+result.length

  return result
end

function java_parseConstantPool_MethodHandle(stream)
  result={}
  result.tag=java_CONSTANT_MethodHandle
  result.reference_kind=java_read_u1(stream)
  result.reference_index=java_read_u2(stream)
  return result
end

function java_parseConstantPool_MethodType(stream)
  result={}
  result.tag=java_CONSTANT_MethodType
  result.descriptor_index=java_read_u2(stream)
  return result

end

function java_parseConstantPool_InvokeDynamic(stream)
  result={}
  result.tag=java_CONSTANT_InvokeDynamic
  result.bootstrap_method_attr_index=java_read_u2(stream)
  result.name_and_type_index=java_read_u2(stream)
  return result
end






java_parseConstantPoolTag={}
java_parseConstantPoolTag[java_CONSTANT_Class]=java_parseConstantPool_Class
java_parseConstantPoolTag[java_CONSTANT_Fieldref]=java_parseConstantPool_Fieldref
java_parseConstantPoolTag[java_CONSTANT_Methodref]=java_parseConstantPool_Methodref
java_parseConstantPoolTag[java_CONSTANT_InterfaceMethodref]=java_parseConstantPool_InterfaceMethodref
java_parseConstantPoolTag[java_CONSTANT_String]=java_parseConstantPool_String
java_parseConstantPoolTag[java_CONSTANT_Integer]=java_parseConstantPool_Integer
java_parseConstantPoolTag[java_CONSTANT_Float]=java_parseConstantPool_Float
java_parseConstantPoolTag[java_CONSTANT_Long]=java_parseConstantPool_Long
java_parseConstantPoolTag[java_CONSTANT_Double]=java_parseConstantPool_Double
java_parseConstantPoolTag[java_CONSTANT_NameAndType]=java_parseConstantPool_NameAndType
java_parseConstantPoolTag[java_CONSTANT_Utf8]=java_parseConstantPool_Utf8
java_parseConstantPoolTag[java_CONSTANT_MethodHandle]=java_parseConstantPool_MethodHandle
java_parseConstantPoolTag[java_CONSTANT_MethodType]=java_parseConstantPool_MethodType
java_parseConstantPoolTag[java_CONSTANT_InvokeDynamic]=java_parseConstantPool_InvokeDynamic


function java_parseConstantPool(s, count)
  local i
  local result={}


  for i=1,count-1 do
    local tag=java_read_u1(s)

  --print(tag.." "..string.format("%x",s.index))

  if java_parseConstantPoolTag[tag]~=nil then
    result[i]=java_parseConstantPoolTag[tag](s)
  else
    error(translate("Invalid constant pool tag encountered: ")..s.index..translate(" (tag=")..tag..") (i="..i..")")
  end

  end

  return result
end

function java_parseAttribute_ConstantValue(cp, a)
  --create a local stream for parsing the info bytes of the attribute
  local s={}
  s.data=a.info
  s.index=1

  a.constantvalue_index=java_read_u2(s);
  if cp[a.constantvalue_index].tag==java_CONSTANT_String then
    a.value=cp[cp[a.constantvalue_index].string_index].utf8
  else
    a.value=cp[a.constantvalue_index].value
  end
end

function java_parseBytecode(cp, s, code_length)
  local result={}
  local bytes={string.byte(s.data, s.index, s.index+code_length-1)}

  --parse the bytes into an array of programcounter and interpreted bytecode command
  result=bytecodeDisassembler(bytes)


  s.index=s.index+code_length
  return result
end

function java_parseAttribute_Code(cp, a)
  local i;
  local s={}
  s.data=a.info
  s.index=1

  a.max_stack=java_read_u2(s)
  a.max_locals=java_read_u2(s)
  a.code_length=java_read_u4(s)
  a.code=java_parseBytecode(cp, s, a.code_length)

  a.exception_table_length=java_read_u2(s)
  a.exception_table={}
  for i=1, a.exception_table_length do
    a.exception_table[i]={}
  a.exception_table[i].start_pc=java_read_u2(s)
    a.exception_table[i].end_pc=java_read_u2(s)
  a.exception_table[i].handler_pc=java_read_u2(s)
  a.exception_table[i].catch_type=java_read_u2(s)
  end

  a.attributes_count=java_read_u2(s)
  a.attributes=java_parseAttributes(cp, s, a.attributes_count)
end

function java_parseAttribute_Exceptions(cp, a)
  local i;
  local s={}
  s.data=a.info
  s.index=1

  a.attribute_name_index=java_read_u2(s)
  a.attribute_length=java_read_u4(s)
  a.number_of_exceptions=java_read_u2(s)

  a.exception_index_table={}
  for i=1, a.number_of_exceptions do
    a.exception_index_table[i]=java_read_u2(s)
  end
end



java_parseAttribute={}
java_parseAttribute["ConstantValue"]=java_parseAttribute_ConstantValue
java_parseAttribute["Code"]=java_parseAttribute_Code
java_parseAttribute["Exceptions"]=java_parseAttribute_Exceptions



--add more yourself...

function java_parseAttributes(cp, s, count)
  local i
  local result={}
  for i=1,count do
    result[i]={}
    result[i].attribute_name_index=java_read_u2(s)
  result[i].attribute_length=java_read_u4(s)
  result[i].info=string.sub(s.data, s.index, s.index+result[i].attribute_length-1)
  s.index=s.index+result[i].attribute_length

  --fill in some extra data (not required for rebuilding)
  result[i].attribute_name=cp[result[i].attribute_name_index].utf8



  if java_parseAttribute[result[i].attribute_name]~=nil then --extra data for this attribute is available
    java_parseAttribute[result[i].attribute_name](cp, result[i])
  end


  end
  return result
end

function java_parseFields(cp, s, count)
  local i
  local result={}


  for i=1,count do
    result[i]={}
    result[i].access_flags=java_read_u2(s)
  result[i].name_index=java_read_u2(s)
  result[i].descriptor_index=java_read_u2(s)
  result[i].attributes_count=java_read_u2(s)
  result[i].attributes=java_parseAttributes(cp, s, result[i].attributes_count)
  end

  return result
end



function java_parseMethods(cp, s, count)
  local i
  local result={}


  for i=1,count do
    result[i]={}
    result[i].access_flags=java_read_u2(s)
  result[i].name_index=java_read_u2(s)
  result[i].descriptor_index=java_read_u2(s)
  result[i].attributes_count=java_read_u2(s)
  result[i].attributes=java_parseAttributes(cp, s, result[i].attributes_count)
  end


  return result
end





function java_parseClass(data)
  local result={}
  local s={}
  local i,j
  s.data=data
  s.index=1
  result.magic=java_read_u4(s)

  if (result.magic~=0xcafebabe) then
    error(translate("Not a valid classfile"))
  end

  result.minor_version=java_read_u2(s)
  result.major_version=java_read_u2(s)
  result.constant_pool_count=java_read_u2(s)
  result.constant_pool_start=s.index
  result.constant_pool=java_parseConstantPool(s, result.constant_pool_count)
  result.constant_pool_stop=s.index

  result.access_flags=java_read_u2(s)
  result.this_class=java_read_u2(s)
  result.super_class=java_read_u2(s)

  result.interfaces_count=java_read_u2(s)
  result.interfaces={}
  for i=1,result.interfaces_count do
    result.interfaces[i]=java_read_u2(s)
  end

  result.fields_count=java_read_u2(s)
  result.fields=java_parseFields(result.constant_pool, s, result.fields_count)

  result.methods_count=java_read_u2(s)
  result.methods=java_parseMethods(result.constant_pool, s, result.methods_count)


  result.attributes_count=java_read_u2(s)
  result.attributes=java_parseAttributes(result.constant_pool, s, result.attributes_count)


  return result
end

----------------------------------------Write class---------------------------------------


function java_write_u4(stream, value)
  assert(value)
  local b=dwordToByteTable(value)


  stream.data=stream.data..string.char(b[4], b[3], b[2],b[1])
  stream.index=stream.index+4
end

function java_write_u2(stream, value)
  assert(value)

  local b=wordToByteTable(value)

  stream.data=stream.data..string.char(b[2],b[1])
  stream.index=stream.index+2
end

function java_write_u1(stream, value)
  assert(value)
  stream.data=stream.data..string.char(value)
  stream.index=stream.index+1
end



function java_writeConstantPool_Class(s, cpitem)
  java_write_u2(s, cpitem.name_index)
end

function java_writeConstantPool_Fieldref(s, cpitem)
  java_write_u2(s, cpitem.class_index)
  java_write_u2(s, cpitem.name_and_type_index)
end

function java_writeConstantPool_Methodref(s, cpitem)
  java_write_u2(s, cpitem.class_index)
  java_write_u2(s, cpitem.name_and_type_index)
end

function java_writeConstantPool_InterfaceMethodref(s, cpitem)
  java_write_u2(s, cpitem.class_index)
  java_write_u2(s, cpitem.name_and_type_index)
end

function java_writeConstantPool_String(s, cpitem)
  java_write_u2(s, cpitem.string_index)
end

function java_writeConstantPool_Integer(s, cpitem)
  java_write_u4(s, cpitem.bytes)
end

function java_writeConstantPool_Float(s, cpitem)
  java_write_u4(s, cpitem.bytes)
end

function java_writeConstantPool_Long(s, cpitem)
  java_write_u4(s, cpitem.high_bytes)
  java_write_u4(s, cpitem.low_bytes)
end

function java_writeConstantPool_Double(s, cpitem)
  java_write_u4(s, cpitem.high_bytes)
  java_write_u4(s, cpitem.low_bytes)
end

function java_writeConstantPool_NameAndType(s, cpitem)
  java_write_u2(s, cpitem.name_index)
  java_write_u2(s, cpitem.descriptor_index)
end

function java_writeConstantPool_Utf8(s, cpitem)
  java_write_u2(s, cpitem.length)
  local i
  for i=1, cpitem.length do
    s.data=s.data..string.char(cpitem.bytes[i])
  end

  s.index=s.index+cpitem.length
end

function java_writeConstantPool_MethodHandle(s, cpitem)
  java_write_u1(s, cpitem.reference_kind)
  java_write_u2(s, cpitem.reference_index)
end

function java_writeConstantPool_MethodType(s, cpitem)
  java_write_u2(s, cpitem.descriptor_index)
end

function java_writeConstantPool_InvokeDynamic(s, cpitem)
  java_write_u2(s, cpitem.bootstrap_method_attr_index)
  java_write_u2(s, cpitem.name_and_type_index)
end


java_writeConstantPoolTag={}
java_writeConstantPoolTag[java_CONSTANT_Class]=java_writeConstantPool_Class
java_writeConstantPoolTag[java_CONSTANT_Fieldref]=java_writeConstantPool_Fieldref
java_writeConstantPoolTag[java_CONSTANT_Methodref]=java_writeConstantPool_Methodref
java_writeConstantPoolTag[java_CONSTANT_InterfaceMethodref]=java_writeConstantPool_InterfaceMethodref
java_writeConstantPoolTag[java_CONSTANT_String]=java_writeConstantPool_String
java_writeConstantPoolTag[java_CONSTANT_Integer]=java_writeConstantPool_Integer
java_writeConstantPoolTag[java_CONSTANT_Float]=java_writeConstantPool_Float
java_writeConstantPoolTag[java_CONSTANT_Long]=java_writeConstantPool_Long
java_writeConstantPoolTag[java_CONSTANT_Double]=java_writeConstantPool_Double
java_writeConstantPoolTag[java_CONSTANT_NameAndType]=java_writeConstantPool_NameAndType
java_writeConstantPoolTag[java_CONSTANT_Utf8]=java_writeConstantPool_Utf8
java_writeConstantPoolTag[java_CONSTANT_MethodHandle]=java_writeConstantPool_MethodHandle
java_writeConstantPoolTag[java_CONSTANT_MethodType]=java_writeConstantPool_MethodType
java_writeConstantPoolTag[java_CONSTANT_InvokeDynamic]=java_writeConstantPool_InvokeDynamic


function java_writeConstantPool(s, constant_pool, constant_pool_count)
  local i

  for i=1,constant_pool_count-1 do
    java_write_u1(s, constant_pool[i].tag)
    java_writeConstantPoolTag[constant_pool[i].tag](s, constant_pool[i])
  end
end

function java_writeAttributes(s, attributes, attributes_count)
  local i
  assert(#attributes==attributes_count)

  for i=1, attributes_count do
    java_write_u2(s, attributes[i].attribute_name_index)
    java_write_u4(s, attributes[i].attribute_length)
    s.data=s.data..attributes[i].info
    s.index=s.index+attributes[i].attribute_length
  end

end


function java_writeFields(s, fields, field_count)
  local i
  for i=1, field_count do
    java_write_u2(s, fields[i].access_flags)
    java_write_u2(s, fields[i].name_index)
    java_write_u2(s, fields[i].descriptor_index)
    java_write_u2(s, fields[i].attributes_count)

    java_writeAttributes(s, fields[i].attributes, fields[i].attributes_count)
  end

end

function java_writeMethods(s, methods, method_count)
  local i
  for i=1, method_count do
    java_write_u2(s, methods[i].access_flags)
  java_write_u2(s, methods[i].name_index)
  java_write_u2(s, methods[i].descriptor_index)
  java_write_u2(s, methods[i].attributes_count)

  java_writeAttributes(s, methods[i].attributes, methods[i].attributes_count)
  end
end





function java_writeClass(class)
  local s={}
  local i
  s.data=''
  s.index=1

  java_write_u4(s, 0xcafebabe)

  java_write_u2(s, class.minor_version)
  java_write_u2(s, class.major_version)
  java_write_u2(s, class.constant_pool_count)
  java_writeConstantPool(s, class.constant_pool, class.constant_pool_count)

  java_write_u2(s, class.access_flags)
  java_write_u2(s, class.this_class)
  java_write_u2(s, class.super_class)

  java_write_u2(s, class.interfaces_count)
  for i=1, class.interfaces_count do
    java_write_u2(s, class..interfaces[i])
  end

  java_write_u2(s, class.fields_count)
  java_writeFields(s, class.fields, class.fields_count)

  java_write_u2(s, class.methods_count)
  java_writeMethods(s, class.methods, class.methods_count)

  java_write_u2(s, class.attributes_count)
  java_writeAttributes(s, class.attributes, class.attributes_count)

  return s.data
end


----------------------------------------runtime helpers----------------------------------------

function javaclass_getMethodName(class, method)
  return class.constant_pool[method.name_index].utf8
end

function javaclass_getExceptionTable(class, method)
end

function javaclass_findMethod(class, methodname)
  --returns the method table for the requested method
  local i

  for i=1, class.methods_count do
    if javaclass_getMethodName(class, class.methods[i])==methodname then
      return class.methods[i]
    end
  end

  return nil
end

function javaclass_method_findCodeAttribute(method)
  local i
  if method.CodeAttribute==nil then
    for i=1, #method.attributes do
      if method.attributes[i].attribute_name=="Code" then
        method.CodeAttribute=method.attributes[i]
        return method.attributes[i]
      end
    end
  else
    return method.CodeAttribute
  end

  return nil
end


--teststuff

--function trace(event, line)
--  print(line)
--end


--f=io.open([[c:\Users\DB\workspace\guitest\bin\Test.class]],"rb")
--data=f:read("*all")
--f:close()

--x=java_parseClass(data)

--debug.sethook(trace, "l")

--newdata=java_writeClass(x)

--f2=io.open([[c:\Users\DB\workspace\guitest\bin\bla\Test.class]],"wb")
--f2:write(newdata)
--f2:close()

--x2=java_parseClass(newdata)
