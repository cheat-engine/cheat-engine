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