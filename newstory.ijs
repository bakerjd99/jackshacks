NB.*newstory s-- stub out a new story directory.
NB.
NB. This  group  fetches   template  text   from  the   (stories)
NB. dictionary and writes a  set of shell  scripts and root latex
NB. files to a directory.
NB.
NB. verbatim: typical setup steps
NB.
NB. 0) if the story is being versioned make a git repo directory
NB.
NB. 1) use (newstory) to place stub files in directory
NB.
NB. 2) make initial edits to files to tweak for story
NB.
NB. 3) rename (gitattributes) to (.gitattribute)
NB.
NB. 4) test files - create first word document following conventions
NB.
NB. 5) checkin - publish repo if sharing or working on many computers
NB.
NB. interface word(s):
NB. ------------------------------------------------------------------------------
NB.  newstory - stub out a new story root folder
NB.
NB. created: 2025feb09
NB. ------------------------------------------------------------------------------
NB. 25feb10 (gitattributes) added to stub files
NB. 25mar11 (gitignore) added - working, story, prefix order checked


coclass 'newstory'
NB.*end-header

NB. only lowercase alphabet characters
ALPHALOWER=:'abcdefghijklmnopqrstuvwxyz'

NB. characters that are blanked out in texts
BlankChars=:'()/\<>'

NB. carriage return character
CR=:13{a.

NB. number of document default sections - usually 10
DefaultSections=:10

NB. interface words (IFACEWORDSnewstory) group
IFACEWORDSnewstory=:,<'newstory'

NB. line feed character
LF=:10{a.

NB. root words (ROOTWORDSnewstory) group      
ROOTWORDSnewstory=:<;._1 ' IFACEWORDSnewstory ROOTWORDSnewstory VMDnewstory newstory wordct'

NB. round off to
RndOff=:0.0100000000000000002

NB. list of story macro text names
StoryTemplate=:<;._1 ' TEMPLATE_clean_bat TEMPLATE_clean_sh TEMPLATE_docx_bat TEMPLATE_docx_sh TEMPLATE_md_bat TEMPLATE_md_sh TEMPLATE_pdf_bat TEMPLATE_pdf_sh TEMPLATE_preamble_tex TEMPLATE_side_md_bat TEMPLATE_side_md_sh TEMPLATE_tex_bat TEMPLATE_tex_sh WORKINGTITLE_tex a0cleantex_bat a0cleantex_sh a0save_bat a0save_sh gitattributes gitignore'

NB. tab character
TAB=:a.{~9

NB. version, make count and date
VMDnewstory=:'0.3.35';7;'15 Mar 2025 15:54:32'

NB. retains string after first occurrence of (x)
afterstr=:] }.~ #@[ + 1&(i.~)@([ E. ])

NB. trims all leading and trailing white space
allwhitetrim=:] #~ [: -. [: (*./\. +. *./\) ] e. (9 10 13 32{a.)"_

NB. signal with optional message
assert=:0 0"_ $ 13!:8^:((0: e. ])`(12"_))

NB. retains string before first occurrence of (x)
beforestr=:] {.~ 1&(i.~)@([ E. ])

NB. boxes open nouns
boxopen=:<^:(L. = 0:)


changestr=:4 : 0

NB.*changestr v-- replaces substrings - see long documentation.
NB.
NB. dyad:  clReps changestr cl
NB.
NB.   NB. first character delimits replacements
NB.   '/change/becomes/me/ehh' changestr 'blah blah ...'

pairs=. 2 {."(1) _2 [\ <;._1 x      NB. change table
cnt=._1 [ lim=. # pairs
while. lim > cnt=.>:cnt do.         NB. process each change pair
  't c'=. cnt { pairs               NB. /target/change
  if. +./b=. t E. y do.             NB. next if no target
    r=. I. b                        NB. target starts
    'l q'=. #&> cnt { pairs         NB. lengths
    p=. r + 0,+/\(<:# r)$ d=. q - l NB. change starts
    s=. * d                         NB. reduce < and > to =
    if. s = _1 do.
      b=. 1 #~ # b
      b=. ((l * # r)$ 1 0 #~ q,l-q) (,r +/ i. l)} b
      y=. b # y
      if. q = 0 do. continue. end.  NB. next for deletions
    elseif. s = 1 do.
      y=. y #~ >: d r} b            NB. first target char replicated
    end.
    y=.(c $~ q *# r) (,p +/i. q)} y  NB. insert replacements
  end.
end. y                              NB. altered string
)


charsub=:4 : 0

NB.*charsub v-- single character pair replacements.
NB.
NB. dyad:  clPairs charsub cu
NB.
NB.   '-_$ ' charsub '$123 -456 -789'

'f t'=. ((#x)$0 1)<@,&a./.x
t {~ f i. y
)

NB. character table to newline delimited list
ctl=:}.@(,@(1&(,"1)@(-.@(*./\."1@(=&' '@])))) # ,@((10{a.)&(,"1)@]))

NB. boxes UTF8 names
fboxname=:([: < 8 u: >) ::]

NB. 1 if file exists 0 otherwise
fexist=:1:@(1!:4) ::0:@(fboxname&>)@boxopen

NB. make directory after expanding folder prefix: jmakedir '~WIPS/mynovel'
jmakedir=:[: 1!:5 ::0: [: < [: jpath_j_ '/' tlc ]


mdbc=:3 : 0

NB.*mdbc v-- markdown blank characters.
NB.
NB. monad:  cl =. mdbc uuIgnore
NB. dyad:  cl =. cl mdbc uuIgnore

NB. period is blanked by default
(CR,LF,TAB,'.,"#*') mdbc y
:
,' ' ,.~ ~.x,BlankChars
)


newstory=:4 : 0

NB.*newstory v-- stub out a new story root folder.
NB.
NB. dyad:  clStoryPath =. clRootDir newstory blSfileSpfxStitCnt
NB.
NB.   '~temp' newstory 'gonggong_gone';'gpgong';'gqg_';14
NB.   '~WIP' newstory 'voracious';'vpr';'vqr_';3
NB.
NB.   NB. default sections/chapters
NB.   '~temp' newstory 'anostomyte';'apyte';'aqt_'

NB. enforce basic naming conventions - check jod
sn=. 3{.y
amsg=. 'bad chars in story names'
amsg assert *./ sn *./@e.&> <'_',ALPHALOWER
amsg assert ;({. , {:)&> sn
'no jod - load and open (stories)' assert (conl 0) e.~ <'ajod'

NB. require 'general/jod' !(*)=. get MACRO_ajod_ jpath_j_ conl
'WorkingTitle StoryFile StoryPrefix StorySections'=. 4 {. y,<DefaultSections
tr=. -.(<StoryPrefix) +./@E.&> WorkingTitle;StoryFile
'story prefix found in file or title' assert tr

NB. this order helps when browsing file lists
'unhelpul order - choose better prefix and file names' assert (i.@#@] -: /:) WorkingTitle;StoryFile;StoryPrefix

NB. create directories - ignore if extant
jmakedir storypath=. (jpath_j_ '/' tlc x),StoryFile,'/'
jmakedir storypath,'zaside/'

NB. stub file texts - mostly replaced
'rc tpt'=. MACRO_ajod_ get StoryTemplate

NB. chapter/side file 
'chap side'=. <;.1 '_chap_side'

NB. bat for win, sh for unix
'ifwin ifunix'=. 1 0

NB. replacements in texts
pos=. (0 {"1 tpt) i. <'TEMPLATE_clean_bat'
tpt=. (<('/STORYPREFIX/',StoryPrefix) changestr ;(<pos;2){tpt) (<pos;2)} tpt
pos=. (0 {"1 tpt) i. <'TEMPLATE_clean_sh'
tpt=. (<('/STORYPREFIX/',StoryPrefix) changestr ;(<pos;2){tpt) (<pos;2)} tpt
pos=. (0 {"1 tpt) i. <'TEMPLATE_docx_bat'
tpt=. (<('/WORKINGTITLE/',WorkingTitle) changestr ;(<pos;2){tpt) (<pos;2)} tpt
pos=. (0 {"1 tpt) i. <'TEMPLATE_docx_sh'
tpt=. (<('/WORKINGTITLE/',WorkingTitle) changestr ;(<pos;2){tpt) (<pos;2)} tpt

NB. generate tex root file referencing sections
pos=. (0 {"1 tpt) i. <'WORKINGTITLE_tex'
tex=. ;(<pos;2){tpt
rps=. (StoryFile;StoryPrefix;chap;StorySections) workroottex tex
tpt=. (<rps) (<pos;2)} tpt

pos=. (0 {"1 tpt) i. <'TEMPLATE_md_bat'
cmds=. (StoryFile;StoryPrefix;chap;'.md';ifwin) storypandoccmds StorySections
tpt=. (<cmds) (<pos;2)} tpt
pos=. (0 {"1 tpt) i. <'TEMPLATE_md_sh'
cmds=. (StoryFile;StoryPrefix;chap;'.md';ifunix) storypandoccmds StorySections
tpt=. (<cmds) (<pos;2)} tpt

pos=. (0 {"1 tpt) i. <'TEMPLATE_side_md_bat'
cmds=. (StoryFile;StoryPrefix;side;'.md';ifwin) storypandoccmds StorySections
tpt=. (<cmds) (<pos;2)} tpt
pos=. (0 {"1 tpt) i. <'TEMPLATE_side_md_sh'
cmds=. (StoryFile;StoryPrefix;side;'.md';ifunix) storypandoccmds StorySections
tpt=. (<cmds) (<pos;2)} tpt

pos=. (0 {"1 tpt) i. <'TEMPLATE_tex_bat'
battex=. (StoryFile;StoryPrefix;chap;'.tex';ifwin) storypandoccmds StorySections
tpt=. (<battex) (<pos;2)} tpt
pos=. (0 {"1 tpt) i. <'TEMPLATE_tex_sh'
shtex=. (StoryFile;StoryPrefix;chap;'.tex';ifunix) storypandoccmds StorySections
tpt=. (<shtex) (<pos;2)} tpt

NB. sh script has pandoc commands
pos=. (0 {"1 tpt) i. <'TEMPLATE_pdf_sh'
rps=. '/STORYPREFIX/',StoryPrefix,'/WORKINGTITLE/',WorkingTitle
rps=. rps,'/STORYFILE/',StoryFile
cmds=. rps changestr ;(<pos;2){tpt  
cmds=. shtex,(2#LF),allwhitetrim 'lualatex','lualatex' afterstr cmds
tpt=. (<cmds) (<pos;2)} tpt

NB. bat script makes call
pos=. (0 {"1 tpt) i. <'TEMPLATE_pdf_bat'
tpt=. (<rps changestr ;(<pos;2){tpt) (<pos;2)} tpt

NB. replacements in save texts
rps=. '/STORYFILE/',StoryFile
pos=. (0 {"1 tpt) i. <'a0save_bat'
tpt=. (<rps changestr ;(<pos;2){tpt) (<pos;2)} tpt
pos=. (0 {"1 tpt) i. <'a0save_sh'
tpt=. (<rps changestr ;(<pos;2){tpt) (<pos;2)} tpt

NB. replacements in template names
rps=. '/TEMPLATE/',StoryFile,'/a0clean/0clean/a0save/0save'
rps=. rps,'/WORKINGTITLE/',WorkingTitle
tpt=. (rps&changestr&.> 0 {"1 tpt) (<a:;0)} tpt

NB. flip file extensions - changes only string ends
pav=. 254{a. [ shext=. ;:'sh bat tex'
rps=. ; , ('.' ,&.> shext) ,.~ (<pav,'/') ,&.>~ (<'/_') ,&.> shext
tpt=. (-.&pav&.> rps&changestr&.> (0 {"1 tpt) ,&.> pav) (<a:;0)} tpt

NB. write stub files to directory
(2 {"1 tpt) write&.> sfiles=. (<storypath) ,&.> 0{"1 tpt
'not all stub files created' assert fexist sfiles

storypath
)

NB. ordered boxed list frequency distribution - see long document
ofreqlist=:[: (([: \: [: ; 1 {  ]) { "1 ]) ~. ,: [: <"0 #/.~

NB. removes multiple blanks (char only)
rebc=:] #~ [: -. '  '&E.

NB. round (y) to nearest (x) (e.g. 1000 round 12345)
round=:[ * [: (<.) 0.5 + %~


secnumbers=:3 : 0

NB.*secnumbers v-- section numbers - count by five.
NB.
NB. monad:  blcl =. secnumbers iaN

'too many story sections <: 200' assert y <: 200
'r<0>3.0' 8!:0 ] y {. 5 * i. 200
)


storypandoccmds=:4 : 0

NB.*storypandoccmds v-- generate n story pandoc commands
NB.
NB. dyad:  blclSpfx storypandoccmds iaN
NB.
NB.   ('anostomyte';'anos_';'';'.tex';IFWIN) storypandoccmds 20 
NB.   ('anostomyte';'anos_';'_side';'.tex';IFWIN) storypandoccmds 20 

NB. story prefix and file extension
'sfile spfx side ext win'=. x
scn=. secnumbers y

NB. j profile !(*)=. jpath_j_
sdir=. 'cd ',(jpath_j_ '~WIPS/'),sfile
pfx=. <('/STORYPREFIX/',spfx) changestr 'pandoc STORYPREFIX'
ocmd=. <('/STORYPREFIX/',spfx) changestr'.docx -o STORYPREFIX'

NB. comment out commands at first
co=. ;win {'# ';'rem '

scside=. scn ,&.> <side
cmds=. ctl co ,"1 >pfx ,&.> scside ,&.> ocmd ,&.> scside ,&.> <ext
sdir,LF,LF,cmds
)

NB. terminate with character if not present: '&' tlc 'end it'
tlc=:] , [ }.~ [ = [: {: ]


wcpercen=:4 : 0

NB.*wcpercen v-- word occurrence as percent of total.
NB.
NB. dyad:  bt =. iaWcnt wcpercen btWordCnt

cnts=. y , <"0 [ RndOff round 100 * iwc % tot=. +/iwc=. ;1{y
(x;tot;RndOff round 100 * tot % x) ,. (\: ;1{cnts) {"1 cnts
)


wordct=:3 : 0

NB.*wordct v-- word count in text - considers case distinct.
NB.
NB. Run the  shell script (ggg_gonggong_md.sh) in ~/draft/stories
NB. before running this verb.
NB.
NB. monad:  bt =. wordct clText
NB.
NB.   wordct read jpath '~WIP/',StoryFile,'.md'
NB.
NB.   NB. overall blog word use freq
NB.   posts=. (1 dir '~BLOGMD/*.markdown') -. 1 dir '~BLOGMD/bm.markdown'
NB.   wordct ; ,&' '&.> read&.> posts
NB.
NB. dyad: bt =. clWords wordct clText
NB.
NB.   (bothcases 1) wordct read jpath '~WIP/',StoryFile,'.md'
NB.   (bothcases 0) wordct read jpath '~WIP/',StoryFile,'.md'

'' wordct y
:
NB. blank select characters
bp=. mdbc 0

NB. parse text and count occurrences of all case sensitive words
cnts=. ofreqlist <@allwhitetrim;._2 rebc ' '&tlc bp charsub allwhitetrim y
wcnt=. +/;1{cnts

NB. pick out particular words
if. #wrds=. ;:x do. wcnt wcpercen cnts {"1~ ({:$cnts) -.~ (0{cnts) i. ;:x else. wcnt wcpercen cnts end.
)


workroottex=:4 : 0

NB.*workroottex v-- generate root tex file from working title template.
NB.
NB. dyad:  cl =. bl workroottex clTex
NB.
NB.   NB. template from (stories) dictionary
NB.   tex=. 4 disp 'WORKINGTITLE_tex'
NB.   ('anostomypte';'anos_';'_chap';10) workroottex tex 

'StoryFile StoryPrefix ChapSuffix StorySections'=. x
scn=. secnumbers StorySections
rps=. allwhitetrim '\end{document}' beforestr 'STORYFILE_preamble.tex}' afterstr y
rps=. ('/STORYPREFIX/',StoryPrefix,'STORYSECTION',ChapSuffix) changestr rps
rps=. ;(<2#LF) ,&.>~ (<rps) {{ ('/STORYSECTION/',y) changestr x }}      &.> scn
rps=. allwhitetrim rps

NB. start and end of document
hdr=. allwhitetrim ('/STORYFILE/',StoryFile) changestr '%\begin{spacing}' beforestr y
hdr,(2#LF),rps,(2#LF),allwhitetrim '%\end{spacing}' afterstr y
)

NB. writes a list of bytes to file
write=:1!:2 ]`<@.(32&>@(3!:0))

NB.POST_newstory post processor. 

smoutput IFACE_newstory=: (0 : 0)
NB. (newstory) interface word(s): 20250315j155432
NB. ----------------------------
NB. newstory  NB. stub out a new story root folder

   NB. main draft root
   jpath '~WIPS'

   NB.  typical calls to stub out new story directory
  '~temp' newstory 'voracious';'vpr';'vqr_';3
  '~WIPS' newstory 'anostomyte';'anostomyte';'ayt_';100
)

cocurrent 'base'
coinsert  'newstory'

