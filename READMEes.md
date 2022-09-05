<p align="center">
    <a href="https://github.com/cheat-engine/cheat-engine/raw/master/Cheat%20Engine/images">
        <img src="https://github.com/cheat-engine/cheat-engine/raw/master/Cheat%20Engine/images/celogo.png" />
    </a>
</p>

<h1 align="center">Cheat Engine</h1>

Cheat Engine es un entorno de desarrollo centrado en la modificación de juegos y aplicaciones para uso personal.


# Descarga

  * **[Ultima versión](https://github.com/cheat-engine/cheat-engine/releases/latest)**

[Versiones antiguas](https://github.com/cheat-engine/cheat-engine/releases)


# Enlaces

  * [Website](https://www.cheatengine.org)
  * [Forum](https://forum.cheatengine.org)
  * [Forum (alternativo)](https://fearlessrevolution.com/index.php)
  * [Wiki](https://wiki.cheatengine.org/index.php?title=Main_Page)

## Redes sociales

  * [Reddit](https://reddit.com/r/cheatengine)
  * [Twitter](https://twitter.com/_cheatengine)

## Donaciones

  * [Patreon](https://www.patreon.com/cheatengine)
  * [PayPal](https://www.paypal.com/xclick/business=dark_byte%40hotmail.com&no_note=1&tax=0&lc=US)


## Instrucciones de compilación

  1.  Descarga Lazarus 32bits desde [aquí](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Lazarus%202.0.10/). Instálelo en la ruta donde tiene acceso completo, por ejemplo, "D:\Lazarus"
      * Si desea desarrollar aplicaciones de 64 bits, descargue e instale el complemento "cross-x86_64-win64" use la misma ruta, por ejemplo, "D:\Lazarus" (el instalador puede mostrar la notificación "la carpeta existe"; haga clic en sí / ok)
      * Enlaces directos (su descarga comenzará en breve en 5 segundos):
        * [Lazarus 32bit](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Lazarus%202.0.10/lazarus-2.0.10-fpc-3.2.0-win32.exe/download)
        * [cross-x86_64-win64 addon](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Lazarus%202.0.10/lazarus-2.0.10-fpc-3.2.0-cross-x86_64-win64-win32.exe/download)
      * Los usuarios de Windows de 64 bits pueden hacer lo contrario: descargar e instalar Lazarus 64bit desde [Aquí](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%202.0.10/) (plus "cross-i386-win32" y su complemento si desea desarrollar aplicaciones de 32 bits)
  2. Ejecute Lazarus y haga clic en `Project->Open Project`. Selecciona `cheatengine.lpi` de la carpeta `Cheat Engine` como proyecto.
  3. Click en `Run->Build` o presiona <kbd>SHIFT+F9</kbd>.
      * También puede hacer clic `Run->Compile many Modes` (consejo: seleccione los primeros tres modos de compilación)
      * Si desea ejecutar o depurar desde el IDE en Windows, deberá ejecutar Lazarus como administrador.

## Contribuye al proyecto

Si deseas contribuir:
  1. Fork el repositorio en GitHub.
  2. Cree una rama para sus cambios.
      * A Lazarus le gusta hacer cambios en los archivos cuando los abre o mueve algo, así que asegúrese de realizar solo los cambios necesarios para lo que está tratando de lograr.
  3. Empuje su rama a su fork personal.
  4. Vaya al repositorio original de Cheat Engine y cree un 'Pull request'
      * Haga clic en `compare across forks` y seleccione su bifurcación como la bifurcación principal y su nueva rama.
