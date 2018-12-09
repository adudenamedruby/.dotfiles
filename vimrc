""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
"   .vimrc
"
"   => maintained by roux g. buciu
"
"
"   'To a hammer, everything looks like a nail. Wield VIM responsibly.'
"       (Or - don't use shit you don't need.)
"
"   => last updated(12/09/2018)
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
"   TABLE OF CONTENTS
"
"   -> General Settings                  29
"   -> VIM Quality of Life               64
"   -> Search options                   168
"   -> Statusline                       187
"   -> Colors & fonts                   324
"   -> Text, tab & indent               349
"   -> Visual mode related              374
"   -> Personal Key Mappings            385
"   -> Leader Key Mappiings             417
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use VIM settings rather than vi settings
"   - this must be first because it changes other options
set nocompatible

" Make sure that unsaved buffers that are to be put in the background are
" allowed to go in there (ie. the 'must save first' error doesn't come up)
set hidden

" Sets how many lines of history VIM has to remember
set history=200
set undolevels=200

" Let VIM try and figure out filetypes if it can
filetype indent on

" Enable syntax highlighting with current color settings
syntax enable

" Set the gui options the way I like it, uh-huh uh-huh.
set guioptions=acg

" I save all the time so let's get rid of this annoying and unecessary stuff.
set nobackup
set nowritebackup
set noswapfile

" Don't redraw while executing macros (good performance config)
set lazyredraw



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM Quality of Life
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Wildmenu, is... wild!!!!!! But also very helpful so set this up to behave nicely
set wildmenu
set wildchar=<TAB>
set wildmode=list:longest
set wildignore+=*.DS_STORE,*.jpg,*.png,*.gif

" Turn line wrapping on because who the hell wants to keep scrolling forever and ever.
set wrap

" Keep 3 lines off the edges of the screen when scrolling for more context
" while doing the scroll thing.
"set scrolloff=4

"Add line numbering, as well as relative numbers becasue no Vim should be without them
set number
set relativenumber

"Always show current position
set ruler

" Configure backspace so it acts as it should act
set backspace=eol,start,indent

" This is the timeout used while waiting for user input on a multi-keyed macro
" or while just sitting and waiting for another key to be pressed measured
" in milliseconds. Play to figure out how I like it.
set timeoutlen=500

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2

" Draws a cursorline under the cursor. This is cool in Xcode.
" But it looks neat when you invoke line&column.
"set cursorline
"set cursorcolumn

" Set up the gui cursor to look nice. Still figuring this out.
"highlight iCursor=pink         " Use this if you want a custom colour for the cursor
"set guicursor=n-v-c:block-Cursor-blinkon0,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor,r-cr:hor20-Cursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175

" Normal OS clipboard interaction
set clipboard=unnamed

" Use a status bar that is 2 rows high
set cmdheight=2

" Set up default window splitting behaviour unless I manually specify what I want
set splitright

" I use 90 space columns but VIMs regular visual line is busy and ugly. This make it such
" that, if a character appears on the 91 column, it'll be highlighted with a magenta 
" block to let me know to code cleaner and more concisely. But dont's stress about
" occasional going over. Sometimes variable names NEED to be ginormous.
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%91v', 100)

" Get rid of the goddamn bell! It's more annying than pigeons in an Italian plaza.
set vb

" Add vertical spaces to keep right and left aligned
set diffopt=filler

" Add ignorance of whitespace to diff
set diffopt+=iwhite

" Various characters are wider than normal fixed width characters, but the
" default setting of ambiwidth (single) squeezes them into normal width, which
" sucks.  Setting it to double makes it awesome.
set ambiwidth=single

" Folding is love. So let's make it behave in a civilized manner
set foldmethod=manual

" Let's avoid insane levels of folding
set foldnestmax=2

" Highlight whitespaces and tabs and extensions and....
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" ...but ignore them if in an html file or xml file.
autocmd filetype html,xml set listchars-=tab:>.

" Also, fill folds with characters
set fillchars=fold:-

" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Search Options
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set case insensitive search
set ignorecase

" Smart case search ignores case if search pattern is lowercase
" Otherwise, search is case-sensitive
set smartcase

" Hilight search terms
set hlsearch

" Should show search matches as you type
set incsearch



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Statusline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" The goal was to have a Powerline/Airline like statusline with what I want...
" ... without having a plugin. This was more annoying than it was worth, maybe.

" EASY READ LINE INFO
" LEFT SIDE => Buffer, lines in file, git branch, file [RO][Modified],  VIM MODE
" RIGHT SID => [unicode][hex], Column, % through file, filetype, encoding, file format

" Returns a string of the current branch
function GetCurrentGitStatus()
    let gitoutput = split(system('git status --porcelain -b '.shellescape(expand('%')).' 2>/dev/null'),'\n')
    if len(gitoutput) > 0
        let b:gitstatus = ' ' . strpart(get(gitoutput,0,''),3)
    else
        let b:gitstatus = ''
    endif
endfunc
autocmd BufEnter,BufWritePost * call GetCurrentGitStatus()

" Returs a string of the current mode VIM is in.
" Currently not returning Visual Block and Select Block. <C-V>/<C-S> only register once
" instead of twice which mode() apparently requires? The literals are '\<C-V>'
function! GetMode()
    if (mode() ==# 'n')
        return 'NORMAL'
    elseif (mode() ==# 'no')
        return 'NORMAL·OPERATOR PENDING '
    elseif (mode() ==# 'i')
        return 'INSERT'
    elseif (mode() ==# 'v')
        return 'VISUAL'
    elseif (mode() ==# 'V')
        return 'VISUAL·LINE'
    elseif (mode() ==# 'R')
        return 'REPLACE'
    elseif (mode() ==# 'Rv')
        return 'VISUAL·REPLACE'
    elseif (mode() ==# 's')
        return 'SELECT'
    elseif (mode() ==# 'S')
        return 'SELECT·LINE'
    elseif (mode() ==# 'c')
        return 'COMMAND'
    elseif (mode() ==# 'cv')
        return 'VIM·EX'
    elseif (mode() ==# 'ce')
        return 'EX'
    elseif (mode() ==# 'r')
        return 'PROMPT'
    elseif (mode() ==# 'rm')
        return 'MORE·AVAILABLE'
    elseif (mode() ==# 'r?')
        return 'CONFIRMATION·REQUIRED'
    elseif (mode() ==# 't')
        return 'TERMINAL·MODE'
    elseif (mode() ==# '!')
        return 'SHELL·EXECUTING'
    else
        return 'SPECIAL·MODE'
    endif
endfunction

" Automatically change the statusline color depending on mode for the NORMAL bar
function! ChangeStatuslineColor()
  if (mode() ==# 'n' || mode() ==# 'no')
    exe 'hi! StatusLine ctermfg=13 ctermbg=7'
  elseif (mode() ==# 'v' || mode() ==# 'V')
    " Visual mode
    exe 'hi! StatusLine ctermfg=54 ctermbg=7'
  elseif (mode() ==# 'i')
    " Insert mode colour:
    exe 'hi! StatusLine ctermfg=12 ctermbg=7'
  elseif (mode() ==# 'R')
    " Replace mode colour:
    exe 'hi! StatusLine ctermfg=9 ctermbg=7'
  elseif (mode() ==# 'r')
    " Prompt mode colour:
    exe 'hi! StatusLine ctermfg=21 ctermbg=7'
  elseif (mode() ==# 'c')
    " Command mode colour:
    exe 'hi! StatusLine ctermfg=22 ctermbg=7'
  else
    exe 'hi! StatusLine ctermfg=198 ctermbg=7'
  endif

  return ''
endfunction

" Automatically change the initial statusline character color depending on mode for the NORMAL bar
function! ChangeStatuslineLeftColor()
  if (mode() ==# 'n' || mode() ==# 'no')
    " normal modes
    exe 'hi! StatusLine ctermfg=15 ctermbg=13'
  elseif (mode() ==# 'v' || mode() ==# 'V')
    " Grey here for visual modes
    exe 'hi! StatusLine ctermfg=15 ctermbg=54'
  elseif (mode() ==# 'i')
    " Insert mode colour:
    exe 'hi! StatusLine ctermfg=15 ctermbg=12'
  elseif (mode() ==# 'R')
    " Replace mode colour:
    exe 'hi! StatusLine ctermfg=15 ctermbg=9'
  elseif (mode() ==# 'r')
    " Prompt mode colour:
    exe 'hi! StatusLine ctermfg=15 ctermbg=21'
  elseif (mode() ==# 'c')
    " Command mode colour:
    exe 'hi! StatusLine ctermfg=15 ctermbg=22'
  else
    exe 'hi! StatusLine ctermfg=15 ctermbg=198'
  endif

  return ''
endfunction

" Draw statusline
set laststatus=2
set statusline=
set statusline+=%1*\ %(%{&buflisted?bufnr('%'):''}:%L\ %)
set statusline+=%< " Truncate line here
set statusline+=%2*%3*\ %(%{b:gitstatus}%)\ 
set statusline+=%4*%5*\ %f\ %r%m\ %*
set statusline+=%{ChangeStatuslineLeftColor()}
set statusline+=
set statusline+=%{ChangeStatuslineColor()}
set statusline+=\ \ \ %{GetMode()}\ \ \ 
set statusline+=%=
set statusline+=%8*\ [%b][0x%B]\ %7*\ %c
set statusline+=\ %6*\%5*\ %p%%\ ☰\ \ %*
set statusline+=%4*%3*\ %y
set statusline+=\ %2*%1*\ %{&fileencoding?&fileencoding:&encoding}\ [%{&fileformat}\]%*



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" dark grey with beige text
hi User1 ctermfg=007 ctermbg=237
" Transition
hi User2 ctermfg=237 ctermbg=14
" Green with yellow background
hi User3 ctermfg=226 ctermbg=14
" Transition
hi User4 ctermfg=14 ctermbg=015
" Beige with dark text
hi User5 ctermfg=0 ctermbg=15
" Transition
hi User6 ctermfg=15 ctermbg=11
" gold with black text
hi User7 ctermfg=0 ctermbg=11
" Transition
hi User8 ctermfg=11 ctermbg=235
" Rose used for file percent
hi User9 ctermfg=9 ctermbg=235



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" Use multiple of shift width when indenting with < and >
set shiftround

" Auto indent because it's a standard these days.
set ai

" Enable smart indenting that copies based off the previous indent!
set si



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Visual mode related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Personal Key Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Easier way of pressing ESC for a Dvorak user. Because qj is great
" but I always end up recording macros! :(
inoremap <C-t>  <esc>

" Remapping the arrow keys to other useful things
map <up> <nop>
map <down> <nop>
map <left> <C-B>
map <right> <C-F>

" Swap implementations of ` and ' jump to markers. By default, ' jumps to the marked
" line, ` jumps to the marked line and column which is infinitely more useful.
nnoremap ' `
nnoremap ` '

" If there's long and wrapped lines, then j and k behave unnaturally.
" Let's take care of that unnecessary silliness.
nnoremap j gj
nnoremap k gk

" A better way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Leader Key Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" With a map leader it's possible to do extra key combinations that you define
nnoremap <space> <nop>
xnoremap <space> <nop>
let mapleader = " "

" In case I accidentally press space, double space is ESC
nnoremap <silent> <leader><space> <esc>

" Let's make saving easier on the hands
nmap <silent> <leader>s :w<CR>

" turn off search highlight
nnoremap <silent> <leader>th :nohlsearch<CR>

" Edit the vimrc file
nmap <silent> <leader>ev :e ~/.vimrc<CR>

