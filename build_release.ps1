$cePath = "C:\projects\cheat-engine\Cheat Engine\"

$projects = @()
$projects += ,@("cecore.lpi", "Default", "", "")
$projects += ,@("cheatengine.lpi", "Release 32-Bit", "Release 64-Bit", "Release 64-Bit O4 AVX2")
$projects += ,@("allochook\allochook.lpi", "64", "32", "")
$projects += ,@("ceregreset\ceregreset.lpi", "Release", "", "")
$projects += ,@("dbk32\Kernelmodule unloader\Kernelmoduleunloader.lpi", "default", "", "")
$projects += ,@("debuggertest\debuggertest.lpi", "default", "", "")
$projects += ,@("launcher\cheatengine.lpi", "release", "", "")
$projects += ,@("luaclient\luaclient.lpi", "Release 32", "Release 64", "")
$projects += ,@("luaclient\testapp\luaclienttest.lpi", "Release", "", "")
$projects += ,@("plugin\DebugEventLog\src\DebugEventLog.lpi", "Default", "", "")
$projects += ,@("plugin\example\exampleplugin.lpi", "Default", "", "")
$projects += ,@("plugin\forcedinjection\forcedinjection.lpi", "release", "Release 64-bit", "")
$projects += ,@("sfx\level2\standalonephase2.lpi", "Release", "", "")
$projects += ,@("speedhack\speedhack.lpi", "32-bit", "64-bit", "")
$projects += ,@("speedhack\speedhacktest\speedhacktest.lpi", "default", "", "")
$projects += ,@("Tutorial\tutorial.lpi", "release 32-bit", "release 64-bit", "")
$projects += ,@("Tutorial\graphical\project1.lpi", "Release 32", "Release 64", "")
$projects += ,@("VEHDebug\vehdebug.lpi", "release 64", "release 32", "")
$projects += ,@("windowsrepair\windowsrepair.lpi", "Release", "", "")
$projects += ,@("winhook\winhook.lpi", "Release 32", "Release 64", "")
$projects += ,@("xmplayer\xmplayer.lpi", "Default", "", "")

echo "Projects to build:"
for ($x = 0; $x -lt 21; $x++)
{
	$file = $projects[$x][0]
	$path = "$cePath$file"
	echo $path
}

for ($x = 0; $x -lt 21; $x++)
{
	$file = $projects[$x][0]
	for ($y = 1; $y -lt 4; $y++)
	{
		if ($projects[$x][$y] -ne "")
		{
			$buildMode = $projects[$x][$y]
			$path = "$cePath$file"
			echo "--- BUILDING $file IN BUILD MODE: $buildMode ---"
			c:\lazarus\lazbuild $path --build-all --build-mode="$buildMode"
		}
	}
}