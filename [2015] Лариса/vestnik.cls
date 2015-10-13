%(c) 2000-2012 Vladimir N. Vlasov

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{vestnik} [2012/06/30 v1.0]

\RequirePackage[centertags]{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsfonts}

\RequirePackage{mathtext}
\RequirePackage[cp1251]{inputenc}
\RequirePackage[T2A]{fontenc}
\RequirePackage[german,english,russian]{babel}
\RequirePackage{fancyhdr}
\RequirePackage{indentfirst}
\RequirePackage[dvips]{graphicx}
\RequirePackage{theorem}
\RequirePackage{cmap}
\RequirePackage{enumerate}

\LoadClass[a4paper,11pt,twoside]{article}

\topmargin=-20pt%
\headheight=14pt%
\footskip=0pt%% 2006-10-23
\oddsidemargin=0mm \evensidemargin=0mm%
\textheight=680pt%new: in real=650 -> reset in style.tex for normal page
\textwidth=154mm
\renewcommand{\baselinestretch}{1.21}

\newlength{\mathindent}
\setlength{\mathindent}{2.5em}

\newlength{\tmplength}

\allowdisplaybreaks

\flushbottom

\setlength{\skip\footins}{13\p@ \@plus 5\p@ \@minus 2\p@}

\newcommand{\nohyphens}{\hyphenpenalty=1000\exhyphenpenalty=1000\relax}

\renewcommand{\theequation}{\arabic{equation}}
\renewcommand{\theenumi}{\textup{\arabic{enumi})}}
\renewcommand\labelenumi{\theenumi}

\newcommand{\zeroitemsep}{\setlength{\itemsep}{0pt}}
\newcommand{\zerobible}{\setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}} %. и проч. списках

\newcommand{\hso}{\hspace{0pt}}
\newcommand{\hs}[1]{\hspace{#1pt}}
\newcommand{\zZ}{\hspace{1pt}}

\g@addto@macro\@enhook{%
  \ifx а\@entemp
    \def\@tempa{\@enLabel\asbuk}%
  \fi}
\providecommand\@enhook{}

\newif\iftranslate	%
\translatetrue		%

  \newcommand{\bibname}{Список литературы}
\renewcommand{\refname}{Список литературы} 
\newdimen\bibindent \setlength\bibindent{1.5em}

\renewcommand{\@biblabel}[1]{#1.} 

\renewenvironment{thebibliography}[1]
{%
\section*{\bibname}%
\list{\@biblabel{\@arabic\c@enumiv}}%
           {\labelwidth=0pt%
            \leftmargin=0pt \itemindent=20pt \labelsep=3pt %\itemindent=\parindent=17pt
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \setlength\labelsep{.5em}\endlist}

\newenvironment{mybibl}[1][9]{\renewcommand{\bibname}{Список литературы}\begin{thebibliography}{#1}\zerobible\frenchspacing}{\end{thebibliography}}

\newcommand{\ENG}{\selectlanguage{english}\frenchspacing}
\newcommand{\RUS}{\selectlanguage{russian}\frenchspacing}

\begingroup 
\gdef\th@plain{\normalfont\itshape
  \def\@begintheorem##1##2{%
        \item[\hskip\labelsep \theorem@headerfont ##1\ ##2.]}%!
\def\@opargbegintheorem##1##2##3{%
   \item[\hskip\labelsep \theorem@headerfont ##1\ ##2\ \normalfont{##3}.]}}
\endgroup

{\theorembodyfont{\slshape} %

\newtheorem{myteo}{Теорема}[part]%
\newtheorem{mypro}{Предложение}[part]%
\newtheorem{mystat}{Утверждение}[part]%
\newtheorem{mycol}{Следствие}[part]%
\newtheorem{mylema}{Лемма}[part]%
\newtheorem{mylem}{Лемма}[myteo]%, подчиненной теореме
\newtheorem{mysublem}{Лемма}[mylem]%
\newtheorem{myproblem}{Задача}[part]%-03

} %for \theorembodyfont{\slshape} %

{\theorembodyfont{\upshape} %

\newtheorem{mydefinition}{Определение}[part]%
\newtheorem{myremark}{Замечание}[part]%
\newtheorem{myexample}{Пример}[part]%

} %theorembodyfont{\upshape} 

\renewcommand{\themypro}{\arabic{mypro}}
\renewcommand{\themystat}{\arabic{mystat}}
\renewcommand{\themylema}{\arabic{mylema}}
\renewcommand{\themycol}{\arabic{mycol}}
\renewcommand{\themyteo}{\arabic{myteo}}
\renewcommand{\themydefinition}{\arabic{mydefinition}}
\renewcommand{\themyremark}{\arabic{myremark}}
\renewcommand{\themyexample}{\arabic{myexample}}
\renewcommand{\themylem}{\arabic{myteo}.\arabic{mylem}}
\renewcommand{\themysublem}{\arabic{mysublem}}
\renewcommand{\themyproblem}{\arabic{myproblem}}

\newcommand{\qed}{\hfill\rule{0.2\textwidth}{0pt}$\square$} %

\newcommand{\prooF}{\noindent\uppercase{Доказательство}.~ }
\newenvironment{proof}[1]{\prooF #1}{\qed\par\smallskip}
\renewcommand{\proof}{\prooF}

\newenvironment{emptyteo}[1][Теорема]%
{\par\addvspace{\theorempreskipamount}\par\noindent{\bfseries #1.}\ \slshape}%
{\par\addvspace{\theorempostskipamount}\par}

\newenvironment{emptyenv}[1][Теорема]%
{\par\addvspace{\theorempreskipamount}\par\noindent{\bfseries #1.}\ }%
{\par\addvspace{\theorempostskipamount}\par}

\newenvironment{freeenv}[1][]%!
{\par\addvspace{\theorempreskipamount}\par\noindent{#1}\ }%
{\par\addvspace{\theorempostskipamount}\par}

\global\setlength\theorempreskipamount{6pt plus 2pt minus 1pt}
\global\setlength\theorempostskipamount{4pt plus 1pt minus 0.5pt}

\newcommand{\ID}{??} 
\newcommand{\UDK}{??} 
\newcommand{\Author}{??}   
\newcommand{\Title}{?????} 
\newcommand{\FNote}[1]{\renewcommand{\thefootnote}{\fnsymbol{footnote}}\hspace{0pt}\footnote[1]{#1}}
\newcommand{\Thanks}{\FNote{??}} 
\newcommand{\AuthorToText}{\Author} 
\newcommand{\TitleToText}{\Title\Thanks} 
\newcommand{\TitleToCol}{\Title} 
\newcommand{\TitleToRef}{\Title} 
\newcommand{\TitleToCon}{\Title} 
\newcommand{\AuthorToCon}{\Author} 
\newcommand{\AuthorToCol}{\Author} 
\newcommand{\Referat}{??}    
\newcommand{\Keywords}{??} 
\newcommand{\eAuthor}{??} 
\newcommand{\eTitle}{?????} 
\newcommand{\eReferat}{??} 
\newcommand{\eKeywords}{??} 

\newcommand{\Address}
{\textbf{Адрес автора:}\par\noindent
ФАМИЛИЯ Имя Отчество,\\
Новосибирский государственный университет\\
ул.~Пирогова, 2, Новосибирск, 630090, Россия\\
e-mail: vestnikmath@nsu.ru 
} 
\newcommand{\Data}{??}    
\newcommand{\eData}{??}   

\newcommand{\FullAuthorName}{???} 
\newcommand{\JobPlace}{???} 

\newcommand{\VestnikYear}{2099}
\newcommand{\VestnikNumber}{5} 
\newcounter{VestnikVolume}
\setcounter{VestnikVolume}{99} 


\setcounter{tocdepth}{0} %

\renewcommand{\part}%
{\@startsection
{part}%
{0}%
{0mm}%
{-2\baselineskip}%
{1.5\baselineskip}%
{\centering\Large\scshape}}%

\renewcommand{\section}%
{\renewcommand{\@seccntformat}[1]{\textup{\csname thesection\endcsname.\hspace{0.75em}}}%-1 без \S
\@startsection
{section}%
{1}%
{0mm}%
{-1.3\baselineskip}%
{0.65\baselineskip}%
{\centering\normalsize\bfseries}}%

\renewcommand{\subsection}%
{\renewcommand{\@seccntformat}[1]{\textup{\csname thesubsection\endcsname.\hspace{0.75em}}}%
\@startsection
{subsection}%
{2}%
{0mm}%
{-0.8\baselineskip}%
{0.4\baselineskip}%
{\centering\normalsize\bfseries\slshape}}%
%{\centering\normalsize\bfseries\itshape}}%

\renewcommand{\subsubsection}%
{\renewcommand{\@seccntformat}[1]{\textup{\csname thesubsubsection\endcsname.\hspace{0.75em}}}%
\@startsection
{subsubsection}%
{3}%
{0mm}%
{-0.8\baselineskip}%
{0.4\baselineskip}%
{\centering\normalsize\slshape}}%

\renewcommand{\paragraph}%
{\@startsection
{paragraph}%
{4}%
{0mm}%
{-4pt plus 1pt minus 0.5pt}%
{-\fontdimen2\font
plus -\fontdimen3\font
minus -\fontdimen4\font}%
{\normalfont\normalsize\bfseries}}%

\renewcommand{\@seccntformat}[1]{\textup{\csname the#1\endcsname.\hspace{0.75em}}} %

\@addtoreset{section}{part}
\@addtoreset{equation}{part}
\@addtoreset{figure}{part}
\@addtoreset{table}{part}
\@addtoreset{footnote}{part}

\renewcommand*\l@part{\@dottedtocline{0}{0em}{2.5em}} 
\renewcommand\@tocrmarg{5em}

\renewcommand\tableofcontents{%

    \part*{\contentsname
        \@mkboth{%
           \contentsname}{\contentsname}}%
    \@starttoc{toc}%
    }

\fancypagestyle{normal}{%
\fancyhf{} %clear all header and footer fields
\chead[\slshape\normalsize \leftmark]{\slshape\normalsize \rightmark}%
\fancyhead[LE,RO]{\rm \thepage}
} %end of normal

\fancypagestyle{FirstForArticle}{%
\textheight=680pt%
\footskip=20pt
\fancyhf{} 
\lfoot{%
\raisebox{14pt}{%
\parbox[t]{0.95\textwidth}{%
\sffamily\scriptsize ISSN 1818-7943. Вестник НГУ. Серия: Математика, механика, информатика. \VestnikYear. Т.~\arabic{VestnikVolume}, вып.~\VestnikNumber. C.~\thepage--\pageref{\ID}.\\
\copyright~ \Author, \VestnikYear.
}
}
}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
}


\newcommand{\MakeFirstPage}{%
{%
\newpage%
\thispagestyle{FirstForArticle}%
}%

{%
\markboth{\AuthorToCol}{\TitleToCol}%
\addcontentsline{toc}{part}{{\it\Author},~{\rm\TitleToCon}}%
%\PuttoRef% 
%\ePuttoRef% 
\stepcounter{part}%
\parindent=0mm \nohyphens%
{\par\raggedright\ttfamily\small{}УДК~\UDK\par}%\bfseries\itshape
\vspace{\baselineskip}%
\large%
{\centering%
{\par\textsl{\AuthorToText}\par}
\vspace{\baselineskip}%
{\large\par\bfseries{\TitleToText}\par}%
\vspace{0.8\baselineskip}%
}%of centering
}%
\medskip{\par\renewcommand{\baselinestretch}{1.0}\small\Referat\par{}}\smallskip
\enlargethispage{-10pt}
\textheight=700pt%new! 
}

\newcommand{\MakeEnding}{%
\bigskip
\raggedleft{\textsl{Материал поступил в редколлегию~\Data}}
\par\noindent\raggedright{\Address}
} 

\AtBeginDocument{%
\setcounter{page}{1}
\selectlanguage{russian}
\pagestyle{normal}
\renewcommand{\subsectionmark}[1]{\markboth{\AuthorToCol}{\TitleToCol}}%
\renewcommand{\sectionmark}[1]{\markboth{\AuthorToCol}{\TitleToCol}}%
}

\newenvironment{myarticle}{\MakeFirstPage\addvspace{\baselineskip}}{\label{\ID}\MakeEnding}

\edef\citenum{%to restore funny codes
  \catcode\string`\string ` \the\catcode\string`\`
  \catcode\string`\string ' \the\catcode\string`\'
  \catcode\string`\string = \the\catcode\string`\=
  \catcode\string`\string _ \the\catcode\string`\_
  \catcode\string`\string : \the\catcode\string`\:}
\catcode\string`\` 12
\catcode`\' 12
\catcode`\= 12
\catcode`\_ 8
\catcode`\: 12

\providecommand\citeleft{[}
\providecommand\citeright{]}
\providecommand\citemid{.\penalty\@medpenalty\ }%
\providecommand\citepunct{;\penalty\@m\hskip.18emplus.1emminus.1em}%- изменен знак между ссылками; 2008-11-04 увеличено расстояние
\providecommand{\citedash}{\hbox{--}\penalty\@m}
\providecommand\citeform{}
\providecommand{\CiteMoveChars}{.,:;}
\providecommand\OverciteFont{\fontsize\sf@size\baselineskip\selectfont}

\let\oc@verbo\relax

\def\oc@movep#1{\futurelet\@tempb\@citey}

\DeclareRobustCommand\citen[1]{%
 \begingroup
  \let\@safe@activesfalse\@empty
  \@nocite{#1}%ignores spaces, writes to .aux file, returns #1 in \@no@sparg
  \@tempcntb\m@ne    %\@tempcntb tracks highest number
  \let\@h@ld\@empty  %nothing held from list yet
  \let\@citea\@empty %no punctuation preceding first
  \let\@celt\delimiter %an unexpandable, but identifiable, token
  \def\@cite@list{}%% empty list to start
  \@for \@citeb:=\@no@sparg\do{\@make@cite@list}%make a sorted list of numbers
  %After sorted citelist is made, execute it to compress citation ranges.
  \@tempcnta\m@ne    %no previous number
  \let\@celt\@compress@cite \@cite@list %output number list with compression
  \@h@ld %output anything held over
 \endgroup
 \@restore@auxhandle
 }

\def\@make@cite@list{%
 \expandafter\let \expandafter\@B@citeB
          \csname b@\@citeb\@extra@b@citeb \endcsname
 \ifx\@B@citeB\relax %undefined: output ? and warning
    \@citea {\bfseries ?}\let\@citea\citepunct \G@refundefinedtrue
    \@warning {Citation `\@citeb' on page \thepage\space undefined}%
    \oc@verbo \global\@namedef{b@\@citeb\@extra@b@citeb}{?}%
 \else %defined               % remove previous line to repeat warnings
    \ifcat _\ifnum\z@<0\@B@citeB _\else A\fi %a positive number, put in list
       \@addto@cite@list
    \else %citation is not a number, output immediately
       \@citea \citeform{\@B@citeB}\let\@citea\citepunct
 \fi\fi}

\def\@addto@cite@list{\@tempcnta\@B@citeB \relax
   \ifnum \@tempcnta>\@tempcntb %new highest, add to end (efficiently)
      \edef\@cite@list{\@cite@list \@celt{\@B@citeB}}%
      \@tempcntb\@tempcnta
   \else %arbitrary number: insert appropriately
      \edef\@cite@list{\expandafter\@sort@celt \@cite@list \@gobble @}%
   \fi}

\def\@sort@celt#1#2{\ifx \@celt #1%parameters are \@celt {num}
   \ifnum #2<\@tempcnta %number goes later in list
      \@celt{#2}%
      \expandafter\expandafter\expandafter\@sort@celt %continue
   \else %number goes here
      \@celt{\number\@tempcnta}\@celt{#2}%stop comparing
\fi\fi}

\def\@compress@cite#1{%% This is executed for each number
  \advance\@tempcnta\@ne %Now \@tempcnta is one more than the previous number
  \ifnum #1=\@tempcnta   %Number follows previous--hold on to it
     \ifx\@h@ld\@empty   %first pair of successives
        \expandafter\def\expandafter\@h@ld\expandafter{\@citea 
           \citeform{#1}}%
     \else               %compressible list of successives
        \def\@h@ld{\citedash \citeform{#1}}%
     \fi
  \else   %non-successor -- dump what's held and do this one
     \@h@ld \@citea \citeform{#1}%
     \let\@h@ld\@empty
  \fi \@tempcnta#1\let\@citea\citepunct
}

\DeclareRobustCommand{\cite}{%
  \@ifnextchar[{\@tempswatrue\@citex}{\@tempswafalse\@citex[]}}

\def\@citex[#1]#2{\@cite{\citen{#2}}{#1}}

\def\@cite#1#2{\leavevmode \cite@adjust
  \citeleft{#1\if@tempswa\@safe@activesfalse\citemid{#2}\fi
  \spacefactor\@m %punctuation in note doesn't affect outside
  }\citeright
 \@restore@auxhandle}

\def\cite@adjust{\begingroup%
  \@tempskipa\lastskip \edef\@tempa{\the\@tempskipa}\unskip
  \ifnum\lastpenalty=\z@ \penalty\@highpenalty \fi
  \ifx\@tempa\@zero@skip \spacefactor1001 \fi %if no space before, set flag
  \ifnum\spacefactor>\@m \ \else \hskip\@tempskipa \fi
  \endgroup}

\edef\@zero@skip{\the\z@skip}

\def\@citew#1{\begingroup \leavevmode
  \@if@fillglue \lastskip \relax \unskip
  \def\@tempa{\@tempcnta\spacefactor
     \/%this allows the last word to be hyphenated, and it looks better.
     \@citess{\citen{#1}}\spacefactor\@tempcnta
     \endgroup \@restore@auxhandle}%
  \oc@movep\relax}%check for following punctuation (depending on options)

\def\@citey{\let\@tempc\@tempa
   %Watch for double periods and suppress them
   \ifx\@tempb.\ifnum\spacefactor<\@bigSfactor\else
     \let\@tempb\relax \let\@tempc\oc@movep
   \fi\fi
   %Move other punctuation
   \expandafter\@citepc\CiteMoveChars\delimiter
   \@tempc}%

\def\@citepc#1{%
   \ifx\@tempb#1\@empty #1\let\@tempc\oc@movep \fi
   \ifx\delimiter#1\else \expandafter\@citepc\fi}

\def\@citess#1{\mbox{$\m@th^{\hbox{\OverciteFont{#1}}}$}}

\DeclareRobustCommand\nocite[1]{%
 \@bsphack \@nocite{#1}%
 \@for \@citeb:=\@no@sparg\do{\@ifundefined{b@\@citeb\@extra@b@citeb}%
    {\G@refundefinedtrue\@warning{Citation `\@citeb' undefined}%
    \oc@verbo \global\@namedef{b@\@citeb\@extra@b@citeb}{?}}{}}%
 \@esphack}

\def\@nocite#1{\begingroup\let\protect\string%normalize active chars
 \xdef\@no@sparg{\expandafter\@ignsp#1 \: }\endgroup%and remove ALL spaces
 \if@filesw \immediate\write\@newciteauxhandle %= \@auxout, except with multibib
    {\string\citation {\@no@sparg}}\fi
 }

\def\@ignsp#1 {\ifx\:#1\@empty\else #1\expandafter\@ignsp\fi}

\begingroup
 \catcode`F=12 \catcode`I=12\catcode`L=12
 \lowercase{\endgroup
 \def\@if@fillglue#1{%
  \begingroup \skip@#1\relax
  \expandafter\endgroup\expandafter 
  \@is@fil@ \the\skip@ \relax\@firstoftwo FIL\relax\@secondoftwo\@nil}
 \def\@is@fil@ #1FIL#2\relax#3#4\@nil{#3}
}

\let\nocitecount\relax  %in case \nocitecount was used for drftcite

\providecommand\hyper@natlinkstart[1]{}
\providecommand\hyper@natlinkend{}
\providecommand\NAT@parse{\@firstofone}

\DeclareOption{verbose}{\def\oc@verbo#1#2#3#4{}}
\DeclareOption{nospace}{\def\citepunct{,\penalty\@m}}
\DeclareOption{space}{\def\citepunct{,\penalty\@highpenalty\ }}
\DeclareOption{ref}{\def\citeleft{[Ref.\penalty\@M\ }}
\DeclareOption{nosort}{\def\@addto@cite@list
   {\edef\@cite@list{\@cite@list \@celt{\@B@citeB}}}}
\DeclareOption{sort}{}%default!
\DeclareOption{nomove}{\def\oc@movep{\@tempa}\let\@citey\oc@movep}
\DeclareOption{move}{}%default
\DeclareOption{nocompress}{%
  \def\@compress@cite#1{%% This is executed for each number
     \@h@ld \@citea \hyper@natlinkstart\citeform{#1}\hyper@natlinkend
     \let\@h@ld\@empty \let\@citea\citepunct}
}
\DeclareOption{compress}{}%default
\DeclareOption{super}{\ExecuteOptions{superscript}}
\DeclareOption{superscript}{%
  \DeclareRobustCommand{\cite}{%
    \@ifnextchar[{\@tempswatrue\@citex}{\@tempswafalse\@citew}}
}
\DeclareOption{noadjust}{\let\cite@adjust\@empty}%Don't change spaces
\DeclareOption{adjust}{}%adjust space before [ ]
\DeclareOption{biblabel}{\def\@biblabel#1{\@citess{#1}\kern-\labelsep\,}}
\ProvidesPackage{cite}[2003/11/04 \space  v 4.01]
\ProcessOptions

\ifx\@citey\oc@movep\else %we are moving punctuation; must ensure sfcodes
  \mathchardef\@bigSfactor3000
  \expandafter\def\expandafter\frenchspacing\expandafter{\frenchspacing
    \mathchardef\@bigSfactor1001
    \sfcode`\.\@bigSfactor \sfcode`\?\@bigSfactor \sfcode`\!\@bigSfactor }%
  \ifnum\sfcode`\.=\@m \frenchspacing \fi
\fi

%Compatability with chapterbib (see use of \@extra@b@citeb)
\@ifundefined{@extra@b@citeb}{\def\@extra@b@citeb{}}{}

%Compatability with multibib (see use of \@newciteauxhandle) (Yes, this is
%overly messy, but I asked for it...  I can't have multibib putting junk after 
%the cite command because it hides following punctuation, but then I have
%to restore the ordinary meaning of \@newciteauxhandle = \@auxout.)
\providecommand\@newciteauxhandle{\@auxout}
\AtBeginDocument{\@ifundefined{newcites}{\global\let\@restore@auxhandle\relax}{}}
\def\@restore@auxhandle{\def\@newciteauxhandle{\@auxout}}

\@ifundefined{G@refundefinedtrue}{\let\G@refundefinedtrue\relax}{}

\@ifundefined{@safe@activesfalse}{}{}
\@ifundefined{bbl@cite@choice}{}{\@ifundefined{org@@citex}{}%
  {\let\org@@citex\@citex}}%Prevent stomping by babel

\citenum %execute restore-catcodes

%Aliases:
\let\citenum\citen
\let\citeonline\citen

%\endinput

\DeclareMathOperator{\grad}{grad}
\DeclareMathOperator{\Div}{div}
\renewcommand{\div}{\Div}
\mathchardef\savediv="2204  %\def{\savediv}{\div}

\DeclareMathOperator{\rot}{rot}
\DeclareMathOperator{\const}{const}
\DeclareMathOperator{\sign}{sign}
