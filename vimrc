""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"   .vimrc
"
"   => maintained by roux g. buciu
"
"
"   'To a hammer, everything looks like a nail. Wield VIM responsibly.'
"       (Or - don't use shit you don't need.)
"
"   => last updated(09/01/2019)
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
"   TABLE OF CONTENTS
"
"   -> Plugins
"   -> General Settings
"   -> VIM Quality of Life
"   -> Search options
"   -> Statusline
"   -> Colors & fonts
"   -> Text, tab & indent
"   -> Visual mode related
"   -> Personal Key Mappings
"   -> Leader Key Mappiings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')

" Declare the list of plugins. Strive to use as little as possible. Only what is really
" needed and provides useful additions to Vim.
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'keith/swift.vim'
"Plug 'srcery-colors/srcery-vim'
" TODO Look into lightline code to see how it's different from my own statusline.
" Still undecided which way to go. :/
"Plug 'itchyny/lightline.vim'
"Plug 'tpope/vim-commentary'
"Plug 'wellle/targets.vim'
"Plug 'tpope/vim-fugitive'
"Plug 'easymotion/vim-easymotion'
"Plug 'valloric/youcompleteme'
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'

" List ends here. Plugins become visible to Vim after this call.
call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

" Turn line wrapping off becasue... well, reasons.
set nowrap

" Don't autowrap text as I'm writing. Sometimes I turn this on... and then
" forget to turn it off!
set textwidth=0

" Keep some lines off the edges of the screen when scrolling for more context
" while doing the scroll thing.
set scrolloff=4

" Add line numbering, as well as relative numbers becasue no Vim should be without them
set number
set relativenumber

" Always show current position
set ruler

" We're using Lightline/custom statusline so we'll disable the mode
set noshowmode

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
" Like RED OCTOBER!
"set cursorline
"set cursorcolumn

" Normal OS clipboard interaction
set clipboard=unnamed

" Use a status bar that is 2 rows high
set cmdheight=2

" Set up default window splitting behaviour unless I manually specify what I want
set splitright

" I use 90 space columns but VIMs regular visual line is busy and ugly. This makes it such
" that, if a character appears on the 91 column, it'll be highlighted with a magenta 
" block to let me know to code cleaner and more concisely. But dont's stress about
" occasionally going over. Sometimes variable names NEED to be ginormous.
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

" Delete trailing white space on save, useful for Python sssssssssssss
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

" Returs a string of the current mode VIM is in. It's not perfect yet, but serviceable.
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
set statusline+=%1*\ Buf:\ %(%{&buflisted?bufnr('%'):''}\ %)
set statusline+=%< " Truncate line here
set statusline+=%9*%5*\ %f\ %r%m\ %*
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
hi User9 ctermfg=237 ctermbg=007



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
set autoindent

" Enable smart indenting that copies based off the previous indent!
" This may no longer be needed as it's an old script. TODO: Investigate!
set smartindent



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Visual mode related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
"vnoremap <silent> * :call VisualSelection('f')<CR>
"vnoremap <silent> # :call VisualSelection('b')<CR>



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Personal Key Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Easier way of pressing ESC for a Dvorak user. Because qj is great
" but I always end up recording macros! :(
inoremap <C-t>  <esc>

" Temporarily remapping the arrow keys to other useful things
map <up> <nop>
map <down> <nop>
map <left> :bp<CR>
map <right> :bn<CR>

" Swap implementations of ` and ' jump to markers. By default, ' jumps to the marked
" line, ` jumps to the marked line and column which is infinitely more useful... imo.
nnoremap ' `
nnoremap ` '

" If there's long and wrapped lines, then j and k behave unnaturally.
" Let's take care of that unnecessary silliness. Keeping this setting on as
" sometimes I do like to manually wrap text for easy of reading/manipulating.
" Na'meen?
nnoremap j gj
nnoremap k gk
vmap j gj
vmap k gk


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Leader Key Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" With a map leader it's possible to do extra key combinations that you
" define. I like
nnoremap <space> <nop>
xnoremap <space> <nop>
let mapleader = " "

" Let's make saving easier on the hands
nmap <silent> <leader>s :w<CR>

" turn off search highlight
nnoremap <silent> <leader>th :nohlsearch<CR>

" Edit the vimrc file
nmap <silent> <leader>ev :e ~/.vimrc<CR>

" A better for me window management system... kinda inspired by Spacemacs!
nnoremap <leader>wh <C-W>h
nnoremap <leader>wl <C-W>l
nnoremap <leader>wj <C-W>j
nnoremap <leader>wk <C-W>k
nnoremap <leader>ws :sp<CR>
nnoremap <leader>wv :vsp<CR>
nnoremap <leader>wc :close<CR>
nnoremap <leader>wn :vne<CR>
nnoremap <leader>wo :only<CR>

" FZF activation for the important stuff
nnoremap <leader>o :Files<CR>
nnoremap <leader>bs :Buffers<CR>
