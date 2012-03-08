" References:
" * My old vim configuration
" * http://nvie.com/posts/how-i-boosted-my-vim/
" * http://stevelosh.com/blog/2010/09/coming-home-to-vim/

" General configuration
set nocompatible
let mapleader=","
let g:mapleader = ","
set hidden
set nowrap        " don't wrap lines
set tabstop=2     " a tab is four spaces
set backspace=indent,eol,start
" allow backspacing over everything in insert mode
set number        " always show line numbers
set shiftwidth=2  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch     " set show matching parenthesis
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
" case-sensitive otherwise
set autoindent             " automatic indent new lines
set smartindent            " be smart about it
set softtabstop=2          " yep, two
set shiftwidth=2           " ..
set expandtab              " expand tabs to spaces
set nosmarttab             " fuck tabs
set formatoptions+=n       " support for numbered/bullet lists
set textwidth=80           " wrap at 80 chars by default
set virtualedit=block      " allow virtual edit in visual block ..

set hlsearch      " highlight search terms
set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set title                " change the terminal's title
set visualbell           " don't beep
set noerrorbells         " don't beep
set nobackup
set noswapfile
set wildmenu " turn on wild menu
set wildmode=longest,list " filename completion
set ruler " Always show current positions along the bottom
set cmdheight=2 " the command bar is 2 high
set t_Co=256
set showmatch " show matching brackets
set mat=5 " how many tenths of a second to blink matching brackets for
set so=10 " Keep 10 lines (top/bottom) for scope
set wrap
set gdefault " global substitution by default
set formatoptions=qrn1
set mouse=a
set clipboard=unnamedplus

function! CurDir()
  let curdir = substitute(getcwd(), '/home/antonio/', "~/", "g")
  return curdir
endfunction

set statusline=\ %F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c

function! TrimWhiteSpace()
  %s/\s*$//
  ''
endfunction

filetype plugin indent on

map <space> /
map <c-space> ?
nmap <silent> ,<space> :nohlsearch<CR>
nmap <leader>w :w!<CR>
" Automatic sudo
cmap w!! w !sudo tee % >/dev/null
nnoremap j gj
nnoremap k gk
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

if has('autocmd')
  autocmd!
  autocmd bufwritepost vimrc source ~/.vimrc " autoreaload .vimrc
  autocmd filetype textile let g:TextileOS="Linux"
  autocmd filetype textile let g:TextileBrowser="/usr/bin/chromium"
  "autocmd FileWritePre * :call TrimWhiteSpace()
  "autocmd FileAppendPre * :call TrimWhiteSpace()
  "autocmd FilterWritePre * :call TrimWhiteSpace()
  "autocmd BufWritePre * :call TrimWhiteSpace()
  autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
  "autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading=1
  autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global=1
  autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
endif

" Color
if &t_Co >= 256 || has("gui_running")
  colorscheme mustang
endif

if &t_Co > 2 || has("gui_running")
  " switch syntax highlighting on, when the terminal has colors
  syntax on
endif

set pastetoggle=<F2>

" Pathogen allows to deploy plugins in ~/.vim/bundle
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

" Yankring
let g:yankring_history_file='.yankring'
nmap <leader>p :YRShow <cr>

" NERDTree
function! OpenNERDTree()
  :TMiniBufExplorer
  :NERDTreeToggle
endfunction
nmap <leader>n :NERDTreeToggle <cr>

" Tag list
nmap <leader>T :TlistToggle <cr>

" CtrlP
let g:ctrlp_map = '<leader>.'

" VimOrganizer
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org            call org#SetOrgFileType()

"Gemfile syntax
au Bufenter Gemfile set syntax=ruby

" CoffeeTags
if executable('coffeetags')
  let g:tagbar_type_coffee = {
        \ 'ctagsbin' : 'coffeetags',
        \ 'ctagsargs' : '',
        \ 'kinds' : [
        \ 'f:functions',
        \ 'o:object',
        \ ],
        \ 'sro' : ".",
        \ 'kind2scope' : {
        \ 'f' : 'object',
        \ 'o' : 'object',
        \ }
        \ }
endif
