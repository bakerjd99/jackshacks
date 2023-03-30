![](jacksjodlogotiny.png) [`jackshacks` (Selected J-Hacks) README](https://github.com/bakerjd99/jackshacks)
===========================================================================================================

 J addon versions of selected J scripts from the 
 JACKS repository: [https://github.com/bakerjd99/jacks](https://github.com/bakerjd99/jacks)

## Setup

To install `jackshacks` enter the **indented** lines in `jconsole` or `jqt`.

```
   install 'github:bakerjd99/jackshacks'
installed: bakerjd99/jackshacks master into folder: jacks

   dir '~addons/jacks'
testdata       <dir>     28-Mar-23 22:45:21
brandxmp.ijs       13295 28-Mar-23 22:45:21
brandxmp.pdf      125252 28-Mar-23 22:45:21
gpxutils.ijs       17079 28-Mar-23 22:45:21
gpxutils.pdf      134365 28-Mar-23 22:45:21
ipynb.ijs           4699 28-Mar-23 22:45:21
ipynb.pdf          86966 28-Mar-23 22:45:21
manifest.ijs        1214 29-Mar-23 13:18:17
riseset.ijs        33180 29-Mar-23 13:18:17
riseset.pdf       174094 29-Mar-23 13:18:17
```

To keep `jackshacks` current simply reinstall. Any new files will be
downloaded and changes to existing files will overwrite local copies.

## Run `jackshacks` scripts

`jackshacks` scripts are self-contained 
[JOD generated](https://analyzethedatanotthedrivel.org/the-jod-page/) groups. 
They have no dependencies beyond simple standard J load utils that are
always present in `jqt` or `jconsole` sessions. To run
them simply load:

~~~ 
load '~addons/jacks/ipynb.ijs' 

load '~addons/jacks/riseset.ijs' 
~~~

Each script is accompanied with a 
[jodliterate](https://analyzethedatanotthedrivel.org/2020/05/25/using-jodliterate/) generated 
PDF that  describes how to use the script. 

~~~
~addons/jacks/ipynb.pdf
~addons/jacks/riseset.pdf
~~~


John Baker
March 29, 2023
