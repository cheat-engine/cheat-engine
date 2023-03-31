require("lfs")
andtools={}

andtools.path=getAutorunPath()..[[andtools\]]  --todo: if nox is running, use the adb tool from nox instead
andtools.workpath=andtools.path..[[temp\]] --getTempFolder()
lfs.mkdir(andtools.workpath)
andtools.deviceidselectioncommand=''
--apktool d -s apk.apk -o extracted
--apktool b extracted -o apk.apk


--keytool -genkey -noprompt -v -alias alias_name -dname "L=Goobelywho" -keyalg RSA -keysize 2048 -validity 10000 -storepass password123 -keystore cekeystore -keypass password123

--jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore cekeystore my_application.apk alias_name
--zipalign -p -f -v 4 package.apk package-za.apk

function andtools.getDeviceList()
  local list={}
  s=createStringList()
  s.text=runCommand(andtools.path..'adb.exe', {'devices','-l'})
  if s.Count>0 then
    s.delete(0)
  end

  for i=0,s.count-1 do
    if string.trim(s[i])~='' then
      local e={}
      e.id=s[i]:split(' ')

      e.desc=s[i]:sub(#e.id+1,#s[i]):trim()
      list[#list+1]=e
    end
  end

  s.destroy()
  return list
end

function andtools.getAndroidPackages()
  return runCommand(andtools.path..'adb.exe', andtools.deviceidselectioncommand..'shell "pm list packages"')
end

function andtools.getPackagePath(packagename)
  local r=runCommand(andtools.path..'adb.exe', andtools.deviceidselectioncommand..'shell "pm path "'..packagename)
  local pstr,path=r:split(':')
  return path:trim()
end

function andtools.patchAPK(packagename)
  local packagepath=andtools.getPackagePath(packagename)
  local localpath=andtools.workpath..extractFileName(packagepath)

  synchronize(function()
    miAndroid.Caption='Android (Pulling APK)'
  end)

  local r,c=runCommand(andtools.path..'adb.exe', {andtools.deviceidselectioncommand, 'pull', packagepath, localpath})


  if fileExists(localpath) and r:find("pulled") then
    --got it
    synchronize(function()
      miAndroid.Caption='Android (Decoding APK)'
    end)

    local extractedPath=extractFileNameWithoutExt(localpath)

    r,c=runCommand(andtools.path..[[jre\bin\java.exe]],{'-jar', andtools.path..'apktool.jar','d','-s','-f', localpath, '-o',extractedPath})


    if (c==0) and fileExists(extractedPath..[[\AndroidManifest.xml]]) then
      synchronize(function()
        miAndroid.Caption='Android (Rebuilding APK)'
      end)


      r,c=runCommand(andtools.path..[[jre\bin\java.exe]],{'-jar', andtools.path..'apktool.jar', 'b', extractedPath, '-d', '-o', extractedPath..'_patched.apk'})  -- -d sets debuggable to true .(alternatively, I could have used ce's xml access functions)

      if fileExists(extractedPath..'_patched.apk') then
        if not fileExists(andtools.workpath..'cekey.store') then
          synchronize(function()
            miAndroid.Caption='Android (Creating signature)'
          end)

          r,c=runCommand(andtools.path..[[jre\bin\keytool.exe]], {'-genkey', '-noprompt', '-v', '-alias', 'cheater', '-dname', 'L=MotherFuckingCheater','-keyalg', 'RSA','-keysize','2048','-validity','10000','-storepass','password123', '-keystore', andtools.workpath..[[ce.keystore]],'-keypass', 'password123'})


          if not fileExists(andtools.workpath..'ce.keystore') then
            synchronize(function()
              messageDialog("Failure creating signature: "..c..'('..r..')', mtError)
            end)
            return
          end
        end

        synchronize(function()
          miAndroid.Caption='Android (Signing new APK)'
        end)
        r,c=runCommand(andtools.path..[[jre\bin\jarsigner.exe]], [[-sigalg SHA1withRSA -digestalg SHA1 -keystore "]]..andtools.workpath..[[ce.keystore" -storepass password123 "]]..extractedPath..[[_patched.apk" cheater]])

        if c~=0 then
          synchronize(function()
            messageDialog("failure signing apk: "..r,mtError)
          end)
          return
        end

        --zipalign -p -f -v 4 package.apk package-za.apk
        synchronize(function()
          miAndroid.Caption='Android (ZipAlign new APK)'
        end)

        local zaparam=[[-p -f -v 4 "%s" "%s"]]
        r,c=runCommand(andtools.path.."zipalign.exe", zaparam:format(extractedPath..'_patched.apk', extractedPath..'_patched_za.apk' ))

        if c~=0 then
          synchronize(function()
            r=messageDialog("Zipalign failed ("..r.."). Continue?",mtWarning, mbYes, mbNo)
          end)
          if r~=mrYes then return end
        end


        synchronize(function()
          miAndroid.Caption='Android (Replacing APK)'
        end)


        r,c=runCommand(andtools.path..'adb.exe',{andtools.deviceidselectioncommand, 'uninstall',packagename})
        if c~=0 then

          synchronize(function()
            r=messageDialog("Failed uninstalling "..packagename..': '..r..[[

continue installing the new package?]],mtWarning, mbYes, mbNo)
          end)
          if r~=mrYes then return end
        end

        r,c=runCommand(andtools.path..'adb.exe',{andtools.deviceidselectioncommand, 'install',extractedPath..'_patched_za.apk'})
        if c~=0 then
          synchronize(function()
            messageDialog("APK install failed: "..r,mtError)
          end)
        end

        messageDialog("APK patch successful",mtInformation)



      else
        synchronize(function()
          messageDialog("Failed finding "..extractedPath..'_patched.apk after rebuilding. Rebuild failure (code '..c..')',mtError)
        end)
      end

    else
      synchronize(function()
        messageDialog("Failed finding "..extractedPath..[[\AndroidManifest.xml after extract. Extraction failed]],mtError)
      end)
    end


  else
    synchronize(function()
      messageDialog('Failure pulling the apk from the device ('..c..' : '..r..') Try making sure the usb cable is connected properly, intact, and check that the battery is charged',mtError)
    end)

  end



end

if miAndroid then
  miAndroid.destroy()
  miAndroid=nil

  miPatchAPK=nil
end

miAndroid=createMenuItem(MainForm.Menu)
miAndroid.Caption='Android'
MainForm.Menu.Items.insert(MainForm.miHelp.MenuIndex, miAndroid)

miPatchAPK=createMenuItem(miAndroid)
miPatchAPK.Caption='Patch APK to debuggable'
miAndroid.add(miPatchAPK)

--miLaunchCEServer=createMenuItem(miAndroid)
--miLaunchCEServer.Caption='Launch ceserver'
--miAndroid.add(miLaunchCEServer)
--run-as gamepackage
--cp /sdcard/ceserver .
--chmod a+x ceserver
--./ceserver



miPatchAPK.OnClick=function(s)
  local i,r

  local devicelist=andtools.getDeviceList()
  if #devicelist==0 then
    messageDialog('There is no android device available', mtError, mbOK)
    return
  end

  if #devicelist>1 then
    s=createStringlist()
    for i=1,#devicelist do
      s.add(devicelist[i].id..'   -   '..devicelist[i].desc)
    end
    i,r=showSelectionList('APK Unlock', 'Select the android device that contains the APK you wish to unlock', s, false, 'AndroidDeviceSelectionList')

    s.destroy()

    if i==-1 then return end

    andtools.deviceidselectioncommand='-s '..devicelist[i+1].id..' '
  end

  s=createStringlist()
  s.text=andtools.getAndroidPackages()

  i,r=showSelectionList('APK Unlock', 'Select the APK to unlock', s, false, 'AndroidPackageSelectionList')
  if i~=-1 then
    x,r=s[i]:split(':')
    if messageDialog('Are you sure you wish to unlock '..r..'?  All data regarding this apk will be lost', mtWarning, mbYes,mbNo)==mrYes then
      miAndroid.Caption='Android (Working)'
      miAndroid.Enabled=false
      miPatchAPK.Enabled=false
      --miLaunchCEServer.Enabled=false
      createThread(function(t)
        t.Name='Android APK Patcher Thread'
        andtools.patchAPK(r)

        synchronize(function()
          miAndroid.Caption='Android'
          miAndroid.Enabled=true
          miPatchAPK.Enabled=true
          --miLaunchCEServer.Enabled=true
        end)
      end)
    end
  end

end


--miLaunchCEServer.OnClick=function()
  --
--end


--return andtools.getDeviceList()