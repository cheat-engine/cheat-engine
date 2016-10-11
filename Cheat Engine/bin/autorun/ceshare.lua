if package.loaded.xmlSimple==nil then
  package.path=package.path..';'..getCheatEngineDir()..[[autorun\xml\?.lua]]
else
  package.loaded.xmlSimple=nil
end
xmlParser = require("xmlSimple").newParser()



ceshare={}
ceshare.version=-1
ceshare.base='http://cheatengine.org/ceshare/'

function ceshare:url_encode(str)
  if (str) then
    str = string.gsub (str, "\n", "\r\n")
    str = string.gsub (str, "([^%w %-%_%.%~])",
        function (c) return string.format ("%%%02X", string.byte(c)) end)
    str = string.gsub (str, " ", "+")
  end
  return str
end

function ceshare:getInternet()
  if self.internet==nil then
    self.internet=getInternet()
  end
  return self.internet
end

function ceshare:login(username,password)
  local i=self:getInternet()  
  local r=i.postURL(self.base..'login.php','username='..self:url_encode(username)..'&password='..self:url_encode(password))
  if (r:sub(1,1)=='<') then
    local s=xmlParser:ParseXmlText(r)
    if s then
      if s.Session then
        self.sessionID=s.Session["@ID"]
        return self.sessionID
      else
        if s.error then
          showMessage(s.error:value())
        end
      end
    else
      showMessage(r)
    end
  else
    showMessage(r);
  end
end

function ceshare:logout()
  if self.sessionID then
    local r=self:getInternet().postURL(self.base..'logout.php','sessionid='..self.sessionID)
    if (r:sub(1,1)=='<') then
      local s=xmlParser:ParseXmlText(r)
      if s then
        if s.Session then
          self.sessionID=nil
          return true
        else
          if s.error then
            showMessage(s.error:value())
          end
        end
      else
        showMessage(r)
      end
    else
      showMessage(r);
    end
  end
end

function ceshare:QueryProcessCheats(processnamemd5, headermd5)
  --print("processnamemd5="..processnamemd5.." headermd5="..headermd5)
  local result=nil
  local r=self:getInternet().postURL(self.base..'QueryProcessCheats.php','processnamemd5='..processnamemd5..'&headermd5='..headermd5)
  if (r:sub(1,1)=='<') then
    local s=xmlParser:ParseXmlText(r)
    if s then
      if s.CheatList then
        --parse the list
        local i
        if s.CheatList.CheatEntry then
          --there are results
          result={}
          for i=1, #s.CheatList.CheatEntry do
            local CheatEntry=s.CheatList.CheatEntry[i]
            if CheatEntry then
              local entry={}
              entry.ID=tonumber(CheatEntry["@ID"])
              entry.Owner=CheatEntry["@Owner"]
              entry.VersionIndependant=CheatEntry["@VersionIndependant"]==1
              entry.Public=CheatEntry["@Public"]==1
              entry.Rating=tonumber(CheatEntry["@Rating"])
              entry.RatingCount=tonumber(CheatEntry["@RatingCount"])
              entry.LastUpdate=os.date("*t", CheatEntry["@LastUpdate"])                            
              entry.FullFileHash=CheatEntry["@FullFileHash"]
              entry.SecondaryModuleName=CheatEntry["@SecondaryModuleName"]
              entry.SecondaryFullFileHash=CheatEntry["@SecondaryFullFileHash"]
              entry.CheckCode=CheatEntry["@CheckCode"]
              entry.Description=CheatEntry["@Description"]
              entry.DataType=CheatEntry["@DataType"]              
              
              table.insert(result, entry)
            end
          
          end
        end
     
      else
        if s.error then
          showMessage(s.error:value())
        end
      end                
    else
      showMessage(r);
    end    
  else
    showMessage(r);
  end
  
  return result
end

function ceshare:QueryCurrentProcess()
  local pid=getOpenedProcessID()
  if (pid) and (pid~=0) then
    local modulelist=enumModules()
    if (modulelist) and (#modulelist>0) then
      local processnamemd5=modulelist[1].Name:lower()
      local headermd5=md5memory(modulelist[1].Address,4096)
      self:QueryProcessCheats(processnamemd5, headermd5)
    end
  end
  
end