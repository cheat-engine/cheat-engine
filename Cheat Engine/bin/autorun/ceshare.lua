package.path=package.path..';'..getCheatEngineDir()..[[autorun\xml\?.lua]]
xmlParser = require("xmlSimple").newParser()


ceshare={}

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
  local r=i.postURL('http://cheatengine.org/ceshare/login.php','username='..ceshare:url_encode(username)..'&password='..ceshare:url_encode(password))
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