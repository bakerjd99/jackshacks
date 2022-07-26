![](jacksjodlogotiny.png) [`jackshacks` (Selected J-Hacks) README](https://github.com/bakerjd99/jackshacks)
===========================================================================================================

 J addon versions of selected J scripts from the 
 JACKS repository: [https://github.com/bakerjd99/jacks](https://github.com/bakerjd99/jacks)

## Setup

To install `jackshacks` enter the **indented** lines in jconsole or jqt.

```
   install 'github:bakerjd99/jackshacks'
installed: bakerjd99/jackshacks master into folder: jacks

   dir '~addons/jacks'
ipynb          <dir>     26-Jul-22 10:11:08
ipynb.ijs           4615 26-Jul-22 11:20:01
ipynb.pdf          84363 26-Jul-22 11:20:01
manifest.ijs         589 26-Jul-22 11:20:01
```

## Run `jackshacks` scripts

`jackshacks` scripts are self-contained 
[JOD generated](https://analyzethedatanotthedrivel.org/the-jod-page/) groups. 
They have no dependencies beyond simple standard J load utils that are
always present in jqt or jconsole sessions. To run
them simply load:

``` load '~addons/jacks/ipynb.ijs' ```

Each script is accompanied with a 
[jodliterate](https://analyzethedatanotthedrivel.org/2020/05/25/using-jodliterate/) generated 
PDF that  describes how to use the script. 

```~addons/jacks/ipynb.pdf ```


John Baker
July 26, 2022
