luasymbols=registerSymbolLookupCallback(function(str)
  local c='return '..str
  local lc=loadstring(c)
  if lc then
    return lc()
  end
end, slNotSymbol) 