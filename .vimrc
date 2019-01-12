""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"   .vimrc
"
"   => maintained by roux g. buciu
"
"
"   'To a hammer, everything looks like a nail.'
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
"   -> Plugin Settings
"   -> General Settings
"   -> Quality of Life
"   -> Search options
"   -> Text, tab & indent
"   -> Visual mode related
"   -> Personal Key Mappings
"   -> Leader Key Mappings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Plugin download directory
call plug#begin('~/.vim/plugged')

" ACTIVE
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
Plug 'itchyny/lightline.vim'
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'

" INACTIVE
"Plug 'easymotion/vim-easymotion'
"Plug 'srcery-colors/srcery-vim'
"Plug 'junegunn/goyo.vim'
"Plug 'tpope/vim-commentary'
"Plug 'wellle/targets.vim'
"Plug 'valloric/youcompleteme'

" List ends here. Plugins become visible to Vim after this call.
call plug#end()



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""




"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Make sure that unsaved buffers that are to be put in the background are
" allowed to go in there (ie. the 'must save first' error doesn't come up)
set hidden

" Sets how many lines of history VIM remembers
set history=200
set undolevels=200

" Let VIM try and figure out filetypes if it can
filetype indent on

" Enable syntax highlighting with current color settings
syntax enable

" Set the gui options: autoselect, console popups and grey menu items if unselected
set guioptions=acg

" I save all the time so let's get rid of this stuff that adds up
set nobackup
set nowritebackup
set noswapfile

" Don't redraw while executing macros
set lazyredraw



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Quality of Life
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Wildmenu setup
set wildmenu
set wildchar=<TAB>
set wildmode=list:longest
set wildignore+=*.DS_STORE,*.jpg,*.png,*.gif

" Turn line wrapping off. It can be manually set if needed
set nowrap

" Add line numbering, as well as relative numbers becasue no Vim should be without them
set number
set relativenumber

" Configure backspace so it acts as it should act
set backspace=eol,start,indent

" This is the timeout used while waiting for user input on a multi-keyed macro
" or while just sitting and waiting for another key to be pressed, measured
" in milliseconds.
set timeoutlen=700

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set matchtime=2

" Normal OS clipboard interaction while keeping plaftorm specific options
set clipboard^=unnamed

" Use a status bar that is 2 rows high
set cmdheight=2

" Draw the statusline
set laststatus=2

" Set up default vsplit behaviour to right of the current split
set splitright

" VIMs regular visual line is busy and ugly. This makes it such that, if a character
" appears on the 91 column, it'll be highlighted with a magenta block.
highlight ColorColumn ctermbg=magenta
augroup TooLong
    autocmd!
    autocmd winEnter,BufEnter * call clearmatches() | call matchadd('ColorColumn', '\%91v', 100)
augroup END

" Get rid of the bell and relpace it with a visual bell
set visualbell

" Lightline replaces this so it's not needed
set noshowmode

" Add vertical spaces to keep right and left aligned and add ignorance of
" whitespace to diff
set diffopt=filler
set diffopt+=iwhite

" Various characters are wider than normal fixed width characters, but the
" default setting of ambiwidth (single) squeezes them into normal width, which
" sucks.  Setting it to double makes it awesome.
set ambiwidth=single

" Make folding method behave in a civilized manner
set foldmethod=manual

" Let's avoid insane levels of folding
set foldnestmax=2

" Also, fill folds with characters
set fillchars=fold:-

" Highlight whitespaces and tabs and extensions
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" ...but ignore them if in an html file or xml file.
augroup HighlightCharacters
    "Removes any autocmds from this augroup so we don't have multiple
    "instances of them
    autocmd!
    autocmd filetype html,xml set listchars-=tab:>.
augroup END

" Delete trailing white space on save, useful for Python sssssssssssss
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
augroup DeleteTrailingWhitespaces
    autocmd!
    autocmd BufWrite *.py :call DeleteTrailingWS()
    autocmd BufWrite *.coffee :call DeleteTrailingWS()
augroup End



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Search Options
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set case insensitive search as the default behaviour
set ignorecase

" Smart case search ignores case if search pattern is all lowercase.
" Otherwise, search is cAse-seNsItiVe
set smartcase

" Hilight search terms
set hlsearch

" Should show search matches as you type
set incsearch



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use spaces instead of tabs
set expandtab

" Insert blanks according to 'shiftwidth'
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" Use multiple of shift width when indenting with < and >
set shiftround

" Copy indent from current line when starting a new line
set autoindent



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Visual mode related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TODO: Toran Billups had a cool effect that, when you :s/p1/p2/, highlighted p1 and
" inserted p2 highlighted until you hit <CR>. Look into it.



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Personal Key Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Easier way of pressing ESC for a Dvorak user while in Insert mode
inoremap <C-t>  <esc>

" Temporarily remapping the arrow keys to other useful things
noremap <up> <nop>
noremap <down> <nop>
noremap <left> :bp<CR>
noremap <right> :bn<CR>

" Swap implementations of ` and ' jump to markers. By default, ' jumps to the marked
" line, ` jumps to the marked line and column which is infinitely more useful... imo.
nnoremap ' `
nnoremap ` '

" If there's long and wrapped lines, then j and k behave unnaturally. This
" mapping makes movements operate on 1 screen line in wrap mode.
function! ScreenMovement(movement)
   if &wrap
      return "g" . a:movement
   else
      return a:movement
   endif
endfunction
onoremap <silent> <expr> j ScreenMovement("j")
onoremap <silent> <expr> k ScreenMovement("k")
onoremap <silent> <expr> 0 ScreenMovement("0")
onoremap <silent> <expr> ^ ScreenMovement("^")
onoremap <silent> <expr> $ ScreenMovement("$")
nnoremap <silent> <expr> j ScreenMovement("j")
nnoremap <silent> <expr> k ScreenMovement("k")
nnoremap <silent> <expr> 0 ScreenMovement("0")
nnoremap <silent> <expr> ^ ScreenMovement("^")
nnoremap <silent> <expr> $ ScreenMovement("$")



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Leader Key Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" SPACEBAR IS MY LEADER!
nnoremap <space> <nop>
xnoremap <space> <nop>
let mapleader = " "

" Let's make saving easier on the hands
noremap <silent> <leader>s :w<CR>

" Easily turn off search highlight
nnoremap <silent> <leader>th :nohlsearch<CR>

" Edit the vimrc file
noremap <silent> <leader>vme :e ~/.vimrc<CR>

" And then source that sucker
noremap <silent> <leader>vms :source $MYVIMRC<CR>

" A better for me window management system... inspired by Spacemacs!
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
nnoremap <leader>a :Ag<CR>
nnoremap <leader>bs :Buffers<CR>
nmap <leader>H :Helptags!<CR>
