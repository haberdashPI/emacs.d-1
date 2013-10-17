if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
  set runtimepath+=$GOROOT/misc/vim
endif
call neobundle#rc(expand('~/.vim/bundle/'))

" Plugins
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'AndrewRadev/sideways.vim'
NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'AndrewRadev/switch.vim'
NeoBundle 'danchoi/ri.vim'
NeoBundle 'MarcWeber/vim-addon-mw-utils'
NeoBundle 'ReekenX/vim-rename2'
NeoBundle 'Rykka/colorv.vim'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }
NeoBundle 'bling/vim-airline'
NeoBundle 'cakebaker/scss-syntax.vim'
NeoBundle 'chrisbra/NrrwRgn'
NeoBundle 'danro/rename.vim'
NeoBundle 'edsono/vim-matchit'
NeoBundle 'h1mesuke/unite-outline'
NeoBundle 'honza/vim-snippets'
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'jnwhiteh/vim-golang'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'kien/rainbow_parentheses.vim'
NeoBundle 'lucapette/vim-ruby-doc'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'MarcWeber/vim-addon-local-vimrc'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'mattn/gist-vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'rgarver/Kwbd.vim'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'sjl/gundo.vim'
NeoBundle 'slim-template/vim-slim'
NeoBundle 'stephenmckinney/vim-dochub'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'tomtom/tlib_vim'
NeoBundle 'tpope/vim-bundler'
NeoBundle 'tpope/vim-cucumber'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-git'
NeoBundle 'tpope/vim-haml'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'vim-scripts/ruby-matchit'
NeoBundle 'xolox/vim-misc'
NeoBundle 'xolox/vim-session'

NeoBundle 'Align'
NeoBundle 'ZoomWin'
NeoBundle 'netrw.vim'

NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'tpope/vim-vividchalk'

" General configuration
set nocompatible
set laststatus=2                         " Always show the statusline
set encoding=utf-8                       " Necessary to show Unicode glyphs
let mapleader=","
let g:mapleader =","
set hidden
set nowrap                               " don't wrap lines
set tabstop=2                            " a tab is four spaces
set backspace=indent,eol,start
set number                               " always show line numbers
set shiftwidth=2                         " number of spaces to use for autoindenting
set shiftround                           " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch                            " set show matching parenthesis
set ignorecase                           " ignore case when searching
set smartcase                            " ignore case if search pattern is all lowercase, case-sensitive otherwise
set autoindent                           " automatic indent new lines
set smartindent                          " be smart about it
set softtabstop=2                        " yep, two
set shiftwidth=2                         " ..
set expandtab                            " expand tabs to spaces
set nosmarttab                           " fuck tabs
set formatoptions+=n                     " support for numbered/bullet lists
set textwidth=80                         " wrap at 80 chars by default
set virtualedit=block                    " allow virtual edit in visual block ..

set hlsearch                             " highlight search terms
set history=1000                         " remember more commands and search history
set undolevels=1000                      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set title                                " change the terminal's title
set visualbell                           " don't beep
set noerrorbells                         " don't beep
set nobackup
set noswapfile
set wildmenu                             " turn on wild menu
set wildmode=longest,list                " filename completion
set ruler                                " Always show current positions along the bottom
set cmdheight=2                          " the command bar is 2 high
set t_Co=256
set showmatch                            " show matching brackets
set mat=5                                " how many tenths of a second to blink matching brackets for
set so=10                                " Keep 10 lines (top/bottom) for scope
set wrap
set gdefault                             " global substitution by default
set formatoptions=qrn1
set mouse=a
set clipboard=unnamed
set iskeyword+=_

filetype plugin indent on

nmap <C-j> :bn <CR>
nmap <C-k> :bp <CR>
nnoremap j gj
nnoremap k gk
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Automatic sudo
cmap w!! w !sudo tee % >/dev/null

let g:rubycomplete_rails               = 0
let g:rubycomplete_classes_in_global   = 0
let g:rubycomplete_buffer_loading      = 0
let g:rubycomplete_include_object      = 0
let g:rubycomplete_include_objectspace = 0

if has('autocmd')
  autocmd!
  autocmd bufwritepost vimrc source $HOME/.vimrc " autoreaload .vimrc
  autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
endif

" Trailing white spaces
:autocmd ColorScheme * highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
:au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
:au InsertLeave * match ExtraWhitespace /\s\+$/
:nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Color
if &t_Co >= 256 || has("gui_running")
  colorscheme mustang
endif

if &t_Co > 2 || has("gui_running")
  " switch syntax highlighting on, when the terminal has colors
  syntax on
endif

set pastetoggle=<F2>

" Coffeescript
autocmd BufEnter *.coffee set filetype=coffee
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

" Tagbar and tags
set tags=tags;/
set tags+=gems.tags
set shell=bash
if has("mac")
  let g:tagbar_ctags_bin = '/usr/local/Cellar/ctags/5.8/bin/ctags'
endif

" Ruby wizardry
" bind control-l to hashrocket
imap <C-l> =><Space>

" Toggle Words
nmap <C-t> :Switch<CR>
imap <C-t> <ESC>:Switch<CR>

" Make vim-autoclose work with vim-endwise
let g:AutoCloseExpandEnterOn = ""

map <space> /
map <c-space> ?
nnoremap <CR> :nohlsearch<CR>

function! RspecCmd()
  let l:precmd = ""
  if g:jruby == 1
    let l:precmd = "jruby --ng -S "
  endif
  if findfile(".zeus.sock") == ".zeus.sock"
    return "zeus rspec"
  else
    return l:precmd . "rspec"
  endif
endfunction

" Rspec
function! RSpecFile()
  execute("!clear && " . RspecCmd() . " " . expand("%p"))
endfunction

function! RSpecCurrent()
  execute("!clear && " . RspecCmd() . " " . expand("%p") . ":" . line("."))
endfunction

map <leader>R :call RSpecFile() <CR>
command! RSpecFile call RSpecFile()
map <leader>r :call RSpecCurrent() <CR>
command! RSpecCurrent call RSpecCurrent()

" Gist
let g:gist_detect_filetype = 1
let g:gist_clip_command = 'xclip -selection clipboard'

" Sessions
let g:session_default_to_last = "no"
let g:session_autoload = "no"
let g:session_autosave = "no"
let g:session_directory = getcwd() . '/.vim-sessions'

" Guardfile
autocmd BufEnter Guardfile set filetype=ruby

" Slim
autocmd BufEnter *.slim set filetype=slim

" Unite
let g:unite_source_history_yank_enable = 1
let g:unite_split_rule = 'botright'
let g:unite_enable_start_insert = 1
let g:unite_source_rec_max_cache_files = 10000

autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()"{{{
  " Overwrite settings.

  nmap <buffer> <ESC>      <Plug>(unite_exit)

  imap <buffer> <TAB>   <Plug>(unite_select_next_line)
  imap <buffer> <C-w>     <Plug>(unite_delete_backward_path)
  nmap <buffer> <C-l> <Plug>(unite_redraw)

endfunction"}}}

if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--noheading --nocolor'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack')
  let g:unite_source_grep_command = 'ack'
  let g:unite_source_grep_default_opts = '--no-heading --no-color -a -H'
  let g:unite_source_grep_recursive_opt = ''
endif
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
nno <leader>. :<C-u>Unite file_rec/async:! -start-insert<CR>
nno <leader>b :<C-u>Unite buffer -start-insert<CR>
nno <leader>y :<C-u>Unite history/yank -start-insert<CR>
nno <leader>o :<C-u>Unite outline -start-insert<CR>
nno <leader>f :<C-u>Unite grep:.<CR>

" vim-airline
let g:airline_powerline_fonts = 1

set undodir=~/.vim/undo " where to save undo histories
set undolevels=1000     " How many undos
set undoreload=10000    " number of lines to save for undo
set undofile            " Save undo's after file closes

nnoremap ,ri :call ri#OpenSearchPrompt(0)<cr> " horizontal split
nnoremap ,RI :call ri#OpenSearchPrompt(1)<cr> " vertical split
nnoremap ,RK :call ri#LookupNameUnderCursor()<cr> " keyword lookup

" Neocomplete
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_refresh_always = 1
let g:neocomplete#disable_auto_complete = 1
inoremap <expr><CR> pumvisible() ? neocomplete#smart_close_popup() : "\<C-g>u\<CR>"
let g:neocomplete#force_overwrite_completefunc = 1
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
if !exists('g:neocomplete#sources#omni#functions')
  let g:neocomplete#sources#omni#functions = {}
endif
if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#enable_auto_close_preview = 1
let g:neocomplete#sources#omni#input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::\w*'
" Define keyword pattern.
if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns._ = '\h\w*'
let g:neocomplete#keyword_patterns.perl = '\h\w*->\h\w*\|\h\w*::\w*'

" Hypertab
imap <expr><TAB> pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ neosnippet#expandable_or_jumpable() ?
        \ "\<Plug>(neosnippet_expand_or_jump)"
        \: neocomplete#start_manual_complete()
  function! s:check_back_space() "{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction"}}}

" Splitjoin
let g:splitjoin_split_mapping = ''
let g:splitjoin_join_mapping = ''
nmap sj :SplitjoinSplit<cr>
nmap sk :SplitjoinJoin<cr>

" Sideways
nnoremap <c-h> :SidewaysLeft<cr>
nnoremap <c-l> :SidewaysRight<cr>

" Endwise
let g:endwise_no_mappings = 1
