<p align="center">
    <a href="https://github.com/cheat-engine/cheat-engine/raw/master/Cheat%20Engine/images">
        <img src="https://github.com/cheat-engine/cheat-engine/raw/master/Cheat%20Engine/images/celogo.png" />
    </a>
</p>

<h1 align="center">Cheat Engine</h1>

Cheat Engine is a development environment focused on modding games and applications for personal use.


# Download

  * **[Latest Version](https://github.com/cheat-engine/cheat-engine/releases/latest)**

[Older versions](https://github.com/cheat-engine/cheat-engine/releases)


# Links

  * [Website](https://www.cheatengine.org)
  * [Forum](https://forum.cheatengine.org)
  * [Forum (alternate)](https://fearlessrevolution.com/index.php)
  * [Wiki](https://wiki.cheatengine.org/index.php?title=Main_Page)

## Social Media

  * [Reddit](https://reddit.com/r/cheatengine)
  * [Twitter](https://twitter.com/_cheatengine)

## Donate

  * [Patreon](https://www.patreon.com/cheatengine)
  * [PayPal](https://www.paypal.com/xclick/business=dark_byte%40hotmail.com&no_note=1&tax=0&lc=US)


## Build Instructions

  1. Download Lazarus 32bit from [here](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Lazarus%202.0.10/). Install it to the path where you have full access e.g. "D:\Lazarus"
      * if you wish to develop 64bit applications, download and install "cross-x86_64-win64" addon, use the same path e.g. "D:\Lazarus" (installer can show "folder exists" notification - click yes/ok )
      * direct links (your download will start shortly in 5 seconds):
        * [Lazarus 32bit](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Lazarus%202.0.10/lazarus-2.0.10-fpc-3.2.0-win32.exe/download)
        * [cross-x86_64-win64 addon](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Lazarus%202.0.10/lazarus-2.0.10-fpc-3.2.0-cross-x86_64-win64-win32.exe/download)
      * 64bit Windows users can do the otherwise: download and install Lazarus 64bit from [here](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%202.0.10/) (plus "cross-i386-win32" addon if you want to develop 32bit applications)
  2. Run Lazarus and click on `Project->Open Project`. Select `cheatengine.lpi` from the `Cheat Engine` folder as the project.
  3. Click on `Run->Build` or press <kbd>SHIFT+F9</kbd>.
      * you can also click on `Run->Compile many Modes` (tip: select first three compile modes)
      * If you want to run or debug from the IDE on Windows you will need to run Lazarus as administrator.

## Contributing

If you wish to contribute:
  1. Fork the repository on GitHub.
  2. Create a branch for your changes.
      * Lazarus likes to make changes to files when you open
        them or move something around, so make sure you only stage the changes that
        are required for what you are trying to accomplish.
  3. Push your branch to your personal fork.
  4. Go to the original Cheat Engine repository and create a pull request:
      * Click on `compare across forks` and select your fork as the head fork and your new
        branch.
